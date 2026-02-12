unit model.pagamento;

interface

uses
  System.Classes, controller.log, System.IniFiles, System.SysUtils,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, FS.PagSeguro,
  controller.email;

type
  TdmPagamento = class(TDataModule)
    DB: TFDConnection;
    qPagamento: TFDQuery;
    FSPagSeguro1: TFSPagSeguro;
    qPagamentoID: TFDAutoIncField;
    qPagamentoID_USUARIO: TIntegerField;
    qPagamentoREFERENCIA: TStringField;
    qPagamentoDATA_COMPRA: TDateTimeField;
    qPagamentoSTATUS_PAGAMENTO: TIntegerField;
    qPagamentoTIPO_PAGAMENTO: TStringField;
    qPagamentoDATA_PAGAMENTO: TDateTimeField;
    procedure DataModuleCreate(Sender: TObject);
  private
    function ParseStatus(AStatus: string): integer;overload;
    function ParseStatus(AStatus:integer):string;overload;
    { Private declarations }
  public
    { Public declarations }
    procedure AnalisePagamento;
    procedure ConfiguraComponente;
  end;

var
  dmPagamento: TdmPagamento;

implementation

uses
  Vcl.Forms, System.Generics.Collections, GosComponents.ambiente;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmPagamento.DataModuleCreate(Sender: TObject);
var
  IniFile: TIniFile;
  vArqIni:string;
  LStr:TStringList;
begin
  ConfiguraComponente;

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

procedure TdmPagamento.ConfiguraComponente;
var
 LAmbiente:TAmbienteIni;
begin

  LAmbiente:= GosComponents.ambiente.ambiente;

  if LAmbiente.Ambiente = TModAmbiente.Sandbox then
  begin
    FSPagSeguro1.Ambiente:= TAmbiente.Sandbox;
    controller.log.Log('ambiente = sandbox');
  end
  else
  begin
    FSPagSeguro1.Ambiente:= TAmbiente.Producao;
    controller.log.Log('ambiente = producao');
  end;
  controller.log.Log('email = '+LAmbiente.EmailVendedor);
  controller.log.Log('token = '+LAmbiente.Token);

  FSPagSeguro1.Credencial.Email := LAmbiente.EmailVendedor;
  FSPagSeguro1.Credencial.Token := LAmbiente.Token;
end;

procedure TdmPagamento.AnalisePagamento;
var
  ItemConsultaPorData : TObjectList<TItemConsultaPorData>;
  ItemConsulta: TItemConsulta;
  I: Integer;
begin
  qPagamento.Close;
  qPagamento.Open;
  qPagamento.First;
  while not qPagamento.Eof do
  begin

    try
      ItemConsultaPorData := FSPagSeguro1.ApiStatusPagamento(qPagamentoREFERENCIA.AsString);
      try
        if ItemConsultaPorData.Count = 0 then
        begin
          qPagamento.Next;
          continue;
        end;

        for I := 0 to ItemConsultaPorData.Count -1 do
        begin
          if ItemConsultaPorData.Items[I].PaymentMethod <>
             qPagamentoTIPO_PAGAMENTO.AsString then
          begin

            qPagamento.Edit;
            qPagamentoTIPO_PAGAMENTO.AsString:= ItemConsultaPorData.Items[I].PaymentMethod;
            qPagamento.Post;

          end;

          if ParseStatus(ItemConsultaPorData.Items[I].Status) <>
            qPagamentoSTATUS_PAGAMENTO.AsInteger then
          begin
            controller.log.Log(
            'Alteração de status registrado, id = '+qPagamentoID.AsString+
            ' id_usuario = '+qPagamentoID_USUARIO.AsString +
            ' status antigo = '+ParseStatus(qPagamentoSTATUS_PAGAMENTO.AsInteger)+
            ' novo status = '+ItemConsultaPorData.Items[I].Status);

            qPagamento.Edit;
            qPagamentoSTATUS_PAGAMENTO.AsInteger:= ParseStatus(ItemConsultaPorData.Items[I].Status);
            qPagamento.Post;

          end;

          if (ParseStatus(ItemConsultaPorData.Items[I].Status) = 3) or
             (ParseStatus(ItemConsultaPorData.Items[I].Status) = 4) then
          begin
            qPagamento.Edit;
            qPagamentoDATA_PAGAMENTO.AsDateTime:= now;
            qPagamento.Post;

            controller.email.CadastrarEmail(2,qPagamentoID_USUARIO.AsInteger);

          end;
        end;
      finally
        FreeAndNil(ItemConsultaPorData);
      end;
    except
      on E:Exception do
      begin
        controller.log.Log(e.Message);
        qpagamento.next;
      end;

    end;

    qpagamento.next;
  end;
end;


function TdmPagamento.ParseStatus(AStatus:string):integer;
begin
  if AStatus = 'Aguardando pagamento (sem intenção de pagamento)' then
    exit(0);
  if AStatus = 'Aguardando pagamento' then
    exit(1);
  if AStatus = 'Em análise' then
    exit(2);
  if AStatus = 'Paga' then
    exit(3);
  if AStatus = 'Disponível' then
    exit(4);
  if AStatus = 'Em disputa' then
    exit(5);
  if AStatus = 'Devolvida' then
    exit(6);
  if AStatus = 'Cancelada' then
    exit(7);
  if AStatus = 'Debitado' then
    exit(8);
  if AStatus = 'Retenção temporária' then
    exit(9);

end;

function TdmPagamento.ParseStatus(AStatus:integer):string;
begin
  if AStatus = 0 then
    exit('Aguardando pagamento (sem intenção de pagamento)');
  if AStatus = 1 then
    exit('Aguardando pagamento');
  if AStatus = 2 then
    exit('Em análise');
  if AStatus = 3 then
    exit('Paga');
  if AStatus = 4 then
    exit('Disponível');
  if AStatus = 5 then
    exit('Em disputa');
  if AStatus = 6 then
    exit('Devolvida');
  if AStatus = 7 then
    exit('Cancelada');
  if AStatus = 8 then
    exit('Debitado');
  if AStatus = 9 then
    exit('Retenção temporária');

end;
end.
