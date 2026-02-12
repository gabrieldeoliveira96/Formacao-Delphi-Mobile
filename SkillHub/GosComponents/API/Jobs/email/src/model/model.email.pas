unit model.email;

interface

uses
  System.Classes, controller.log, System.IniFiles, System.SysUtils,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, FS.PagSeguro;

type
  TdmEnviarEmail = class(TDataModule)
    DB: TFDConnection;
    qEmailEnvio: TFDQuery;
    qEmailEnvioID: TFDAutoIncField;
    qEmailEnvioID_EMAIL: TIntegerField;
    qEmailEnvioID_USUARIO: TIntegerField;
    qEmailEnvioDATA_ENVIO: TDateTimeField;
    qEmailEnvioENVIADO: TStringField;
    qEmail: TFDQuery;
    qEmailID: TFDAutoIncField;
    qEmailTIPO: TIntegerField;
    qEmailIDIOMA: TIntegerField;
    qEmailARQUIVO: TStringField;
    qEmailASSUNTO: TStringField;
    qEmailEnvioEMAIL: TStringField;
    qEmailCadastro: TFDQuery;
    qEmailEnvioNOME: TStringField;
    qUltimaVersao: TFDQuery;
    qUltimaVersaoID: TFDAutoIncField;
    qUltimaVersaoVERSAO: TStringField;
    qUltimaVersaoDTCRIACAO: TDateField;
    qUltimaVersaoURL: TStringField;
    qUltimaVersaoATIVO: TStringField;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function EnviarEmail: boolean;
  end;

var
  dmEnviarEmail: TdmEnviarEmail;

implementation

uses
  Vcl.Forms, controller.email, System.Generics.Collections;

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

procedure TdmEnviarEmail.DataModuleCreate(Sender: TObject);
var
  IniFile: TIniFile;
  vArqIni: string;
  LStr: TStringList;
begin

  try
    vArqIni := ExtractFilePath(Application.ExeName) + 'ambiente.ini';

    controller.log.log(vArqIni);

    if not(FileExists(vArqIni)) then
      raise Exception.Create('Arquivo não localizado: ' + vArqIni);

    IniFile := TIniFile.Create(vArqIni);
    DB.DriverName := 'MySql';
    DB.LoginPrompt := False;
    DB.ResourceOptions.KeepConnection := True;
    DB.ResourceOptions.AutoConnect := True;
    DB.ResourceOptions.AutoReconnect := True;
    DB.Params.Values['DataBase'] := IniFile.ReadString('BANCO', 'DATABASE', '');
    DB.Params.Values['User_Name'] := IniFile.ReadString('BANCO', 'USER_NAME', '');
    DB.Params.Values['Password'] := IniFile.ReadString('BANCO', 'PASSWORD', '');
    DB.Params.Values['Server'] := IniFile.ReadString('BANCO', 'SERVER', '');
    DB.Params.Values['DriverID'] := 'MySql';
    DB.Params.Values['Port'] := IniFile.ReadString('BANCO', 'PORT', '');

    DB.Connected := True;
  except
    on e: Exception do
    begin
      controller.log.log(#13'Mensagem da Classe ' + Self.ClassName + ': ' +
        e.Message);
      raise Exception.Create(#13'Mensagem da Classe ' + Self.ClassName + ': ' +
        e.Message);
    end;
  end;

end;

function TdmEnviarEmail.EnviarEmail: boolean;
var
  AReplace: TDictionary<String, string>;
  LUrlS3:string;
begin
  Result:= true;
  try
    AReplace := TDictionary<String, string>.Create;
    try
      qUltimaVersao.Open;
      LUrlS3:= qUltimaVersaoURL.AsString;
      qUltimaVersao.Close;

      AReplace.Clear;

      qEmailEnvio.open;
      qEmailEnvio.First;
      while not qEmailEnvio.Eof do
      begin
        try
          qEmail.Close;
          qEmail.ParamByName('id').AsInteger := qEmailEnvioID_EMAIL.AsInteger;
          qEmail.open;

          AReplace.Add('[%NOME%]', qEmailEnvioNOME.AsString);
          AReplace.Add('[%LINKS3%]', LUrlS3);

          controller.log.log(
            'Enviando email = ' + qEmailEnvioEMAIL.AsString +
            ' Assunto = ' + qEmailASSUNTO.AsString +
            ' Arquivo = ' +
            ExtractFilePath(Application.ExeName) + qEmailARQUIVO.AsString);

          controller.email.EnviarEmail(
            qEmailEnvioEMAIL.AsString,
            qEmailASSUNTO.AsString,
            ExtractFilePath(Application.ExeName) + qEmailARQUIVO.AsString,
            AReplace);

          qEmailEnvio.Edit;
          qEmailEnvioDATA_ENVIO.AsDateTime := now;
          qEmailEnvioENVIADO.AsString := 'S';
          qEmailEnvio.Post;

          AReplace.Clear;

          qEmailEnvio.Next;
        except
          on E:Exception do
          controller.log.Log('Erro: '+e.Message);
        end;
      end;
      qEmailEnvio.Close;

    finally
      FreeAndNil(AReplace);
    end;

  except
    on E:Exception do
    begin
      Result:= false;
      controller.log.Log('Erro: '+e.Message);
    end;
  end;
end;


end.
