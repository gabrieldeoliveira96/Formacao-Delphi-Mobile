unit model.componentes;

interface

uses
  System.SysUtils, System.Classes, model.con, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.ConsoleUI.Wait, Data.DB,
  FireDAC.Comp.Client, System.json, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, DataSet.Serialize;

type
  TdmComponentes = class(TdmCon)
    qComponents: TFDQuery;
    qComponentsID: TFDAutoIncField;
    qComponentsTITULO: TMemoField;
    qComponentsDTCRIACAO: TDateTimeField;
    qComponenteDescricao: TFDQuery;
    qComponenteDescricaoID: TFDAutoIncField;
    qComponenteDescricaoID_COMPONENTE: TIntegerField;
    qComponenteDescricaoDESCRICAO: TMemoField;
    qComponenteDescricaoIDIOMA: TIntegerField;
    qMidiaComponentes: TFDQuery;
    qMidiaComponentesID: TFDAutoIncField;
    qMidiaComponentesID_COMPONENTE: TIntegerField;
    qMidiaComponentesARQUIVO: TStringField;
    qMidiaComponentesTIPO: TIntegerField;
  private
    { Private declarations }
  public
    { Public declarations }
    function Get: TJSONArray;overload;
    function Get(AId:integer): TJSONObject; overload;
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ TdmComponentes }

function TdmComponentes.Get: TJSONArray;
var
 LJson:TJSONObject;
 LJsonDescricao:TJSONObject;
 LJaComponente:TJSONArray;
 LJa:TJSONArray;
begin
  qComponents.Open;
  qComponents.First;

  LJaComponente:= TJSONArray.Create;

  while not qComponents.Eof do
  begin
    LJson:= TJSONObject.Create;
    LJson.AddPair('ID',qComponentsID.AsInteger);
    LJson.AddPair('TITULO',qComponentsTITULO.AsString);
    LJson.AddPair('DTCRIACAO', qComponentsDTCRIACAO.AsString);

    qComponenteDescricao.Close;
    qComponenteDescricao.ParamByName('ID_COMPONENTE').AsInteger:= qComponentsID.AsInteger;
    qComponenteDescricao.Open;
    qComponenteDescricao.First;

    LJa:= TJSONArray.Create;

    while not qComponenteDescricao.Eof do
    begin
      LJsonDescricao:= TJSONObject.Create;
      LJsonDescricao.AddPair('TEXTO',qComponenteDescricaoDESCRICAO.AsString);
      LJsonDescricao.AddPair('IDIOMA',qComponenteDescricaoIDIOMA.AsInteger);

      LJa.Add(LJsonDescricao);
      qComponenteDescricao.Next;
    end;

    LJson.AddPair('DESCRICAO',LJa);

    qMidiaComponentes.Close;
    qMidiaComponentes.ParamByName('ID_COMPONENTE').AsInteger:= qComponentsID.AsInteger;
    qMidiaComponentes.Open;
    qMidiaComponentes.First;

    LJa:= TJSONArray.Create;

    while not qMidiaComponentes.Eof do
    begin
      LJsonDescricao:= TJSONObject.Create;
      LJsonDescricao.AddPair('ARQUIVO',qMidiaComponentesARQUIVO.AsString);
      LJsonDescricao.AddPair('TIPO',qMidiaComponentesTIPO.AsInteger);

      LJa.Add(LJsonDescricao);
      qMidiaComponentes.Next;
    end;

    LJson.AddPair('MIDIA',LJa);

    LJaComponente.Add(LJson);

    qComponents.Next;
  end;

  Result:= LJaComponente;
end;

function TdmComponentes.Get(AId: integer): TJSONObject;
begin
  qComponents.SQL.Add(' where id = :id');
  qComponents.ParamByName('id').AsInteger:= AId;
  qComponents.Open;
  Result:= qComponents.ToJSONObject;

end;

end.
