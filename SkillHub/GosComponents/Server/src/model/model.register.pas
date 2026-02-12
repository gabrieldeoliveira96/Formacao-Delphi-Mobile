unit model.register;

interface

uses
  System.SysUtils, System.Classes, model.con, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.ConsoleUI.Wait, Data.DB, FireDAC.Comp.Client, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, System.json,
  DataSetConverter4D,
  DataSetConverter4D.Impl;


type
  TdmRegister = class(TdmCon)
    qRegisters: TFDQuery;
    qRegistersID: TFDAutoIncField;
    qRegistersEMAIL: TStringField;
    qRegistersDATA_CADASTRO: TDateTimeField;
    qRegistersATIVO: TBooleanField;
  private
    { Private declarations }
  public
    function Post(ARegister: TJSONObject): TJSONObject;
    { Public declarations }
  end;

var
  dmRegister: TdmRegister;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

function TdmRegister.Post(ARegister: TJSONObject): TJSONObject;
begin

  if not qRegisters.Active then
    qRegisters.Open();

  TConverter.New.json(ARegister).ToDataSet(qRegisters);


  Result := TConverter.New.DataSet.Source(qRegisters).AsJSONObject;

end;

end.
