unit model.pagamento;

interface

uses
  System.SysUtils, System.Classes, model.con, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.ConsoleUI.Wait, Data.DB,
  FireDAC.Comp.Client, FS.PagSeguro, System.JSON, model.usuario,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, DataSet.Serialize, controller.log;

type
  TdmPagamento = class(TdmCon)
    FSPagSeguro1: TFSPagSeguro;
    qVendas: TFDQuery;
    qAtualizaPagamento: TFDQuery;
    qUltimaVersao: TFDQuery;
    qUltimaVersaoID: TFDAutoIncField;
    qUltimaVersaoVERSAO: TStringField;
    qUltimaVersaoDTCRIACAO: TDateField;
    qUltimaVersaoURL: TStringField;
    qUltimaVersaoATIVO: TStringField;
    qUltimaVersaoDESCRICAO: TStringField;
    qUltimaVersaoVALOR: TFloatField;
  private
    { Private declarations }
    function Vendas(ABody: TJSONObject): TJSONObject;
    procedure ConfiguraComponente;

  public
    { Public declarations }
    function Pagamento(AIdUsuario: integer): TJSONObject;
    function AtualizaTipoPagamento(ABody:TJSONObject): TJSONObject;
  end;

var
  dmPagamento: TdmPagamento;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

uses GosComponents.ambiente;

{$R *.dfm}

function TdmPagamento.AtualizaTipoPagamento(ABody:TJSONObject): TJSONObject;
begin
  qAtualizaPagamento.Close;
  qAtualizaPagamento.SQL.Text:=
  ' update venda set STATUS_PAGAMENTO = :STATUS, '+
  ' TIPO_PAGAMENTO = :TIPO_PAGAMENTO where '+
  ' ID = :ID ';

  qAtualizaPagamento.ParamByName('ID').AsInteger:= ABody.GetValue<integer>('IdPagamento');
  qAtualizaPagamento.ParamByName('STATUS').AsString:= ABody.GetValue<string>('NovoStatus');
  qAtualizaPagamento.ParamByName('TIPO_PAGAMENTO').AsString:= ABody.GetValue<string>('NovoTipo');
  qAtualizaPagamento.ExecSQL;

  qVendas.SQL.Add(' where id = :id');
  qVendas.ParamByName('id').AsInteger:= ABody.GetValue<integer>('IdPagamento');
  qVendas.Open;
  Result:= qVendas.ToJSONObject;

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

function TdmPagamento.pagamento(AIdUsuario: integer): TJSONObject;
var
  LdmUsu: TdmUsuario;
  LUsu: TJSONObject;
  LItem: TItemPagSeguro;
  LJoVenda:TJSONObject;
  LReferencia:string;
  LUrl:string;
  LIdUltimaVersao:integer;
begin
  try
    ConfiguraComponente;

    LdmUsu := TdmUsuario.Create(self);
    try
      LUsu := LdmUsu.Get(AIdUsuario);
    finally
      FreeAndNil(LdmUsu);
    end;

    if LUsu.Count > 0 then
    begin
      FSPagSeguro1.ApiPagamento.Comprador.AreaCode := LUsu.GetValue<string>('ddd');
      FSPagSeguro1.ApiPagamento.Comprador.Phone := LUsu.GetValue<string>('telefone');
      FSPagSeguro1.ApiPagamento.Comprador.Cpf := LUsu.GetValue<string>('cpf');
      FSPagSeguro1.ApiPagamento.Comprador.Email := LUsu.GetValue<string>('email');
      FSPagSeguro1.ApiPagamento.Comprador.BornDate := LUsu.GetValue<TDate>('dtnasc');

      qUltimaVersao.Open;
      LIdUltimaVersao:= qUltimaVersaoID.AsInteger;

      LItem := TItemPagSeguro.Create;
      LItem.ItemId := LIdUltimaVersao.ToString;
      LItem.ItemDescription := qUltimaVersaoDESCRICAO.AsString;
      LItem.ItemAumont := qUltimaVersaoVALOR.AsFloat;
      LItem.ItemQuantity := 1;
      FSPagSeguro1.ApiPagamento.Items.Add(LItem);

      qUltimaVersao.Close;


      LReferencia:= FSPagSeguro1.GetNewReference;
      FSPagSeguro1.ApiPagamento.Venda.Reference := LReferencia;

      FSPagSeguro1.Parametros.MaxAge := 3600;
      FSPagSeguro1.Parametros.MaxUses := 1;

      LJoVenda:= TJSONObject.Create;
      LJoVenda.AddPair('ID_USUARIO',AIdUsuario);
      LJoVenda.AddPair('REFERENCIA',LReferencia);
      LJoVenda.AddPair('DATA_COMPRA', now);
      LJoVenda.AddPair('STATUS_PAGAMENTO','0');
      LJoVenda.AddPair('ID_VERSAO', LIdUltimaVersao);

      try
        FSPagSeguro1.Enviar(FSPagSeguro1.ApiPagamento);

        if FSPagSeguro1.Transaction <> EmptyStr then
        begin
          LUrl:= FSPagSeguro1.UrlFluxo;
          FSPagSeguro1.ApiPagamento.Items.Clear;
        end
        else
          controller.log.Log('Erro na solicitação de pagamento!');

      except
        on e: exception do
        begin
          controller.log.Log(e.message);
          raise Exception.Create('pagamento '+ e.Message);
        end;
      end;
      Result:= Vendas(LJoVenda);
      Result.AddPair('URL',LUrl);
    end;

  except
    on e: exception do
    begin
      controller.log.Log(e.message);
      raise Exception.Create('pagamento '+ e.Message);
    end;
  end;
end;

function TdmPagamento.Vendas(ABody: TJSONObject): TJSONObject;
begin
  qVendas.Open;
  qVendas.LoadFromJSON(ABody);
  qVendas.Last;
  Result := qVendas.ToJSONObject;

end;

end.
