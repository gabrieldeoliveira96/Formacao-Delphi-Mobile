unit model.con;

interface

uses
  System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.ConsoleUI.Wait, Data.DB, FireDAC.Comp.Client,
  System.IniFiles, FireDAC.VCLUI.Wait, controller.log;

type
  TdmCon = class(TDataModule)
    DB: TFDConnection;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses
  System.SysUtils, Vcl.Forms;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TdmCon.DataModuleCreate(Sender: TObject);
var
  IniFile: TIniFile;
  vArqIni:string;
  LStr:TStringList;
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
    DB.Params.Values['DataBase'] :=  IniFile.ReadString('BANCO', 'DATABASE', '');
    DB.Params.Values['User_Name'] := IniFile.ReadString('BANCO', 'USER_NAME', '');
    DB.Params.Values['Password'] := IniFile.ReadString('BANCO', 'PASSWORD', '');
    DB.Params.Values['Server'] := IniFile.ReadString('BANCO', 'SERVER', '');
    DB.Params.Values['DriverID'] := 'MySql';
    DB.Params.Values['Port'] := IniFile.ReadString('BANCO', 'PORT', '');

    DB.Connected:= true;

  except
    on e: Exception do
    begin
      controller.log.log(#13'Mensagem da Classe ' + Self.ClassName + ': ' +e.Message);
      raise Exception.Create(#13'Mensagem da Classe ' + Self.ClassName + ': ' +
        e.Message);
    end;
  end;

end;

end.
