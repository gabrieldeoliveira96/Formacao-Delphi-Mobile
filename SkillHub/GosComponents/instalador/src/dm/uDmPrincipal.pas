unit uDmPrincipal;

interface

uses
  System.SysUtils, System.Classes, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.json;

type
  TdmCon = class(TDataModule)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
  private
    const SERVER = 'http://localhost';
    { Private declarations }
  public
    { Public declarations }
    function doConnection(AUri: string; AParameter: array of string;
      AMethod: TRESTRequestMethod; out AResult: TJSONArray; Abody: string = '';
      AbodyType: TRESTContentType = ctAPPLICATION_JSON; ATimeOut:integer = 30000): boolean; overload;

    function doConnection(AUri: string; AParameter: array of string;
      AMethod: TRESTRequestMethod; out AResult: TJSONObject; Abody: string = '';
      AbodyType: TRESTContentType = ctAPPLICATION_JSON; ATimeOut:integer = 30000): boolean; overload;
  end;

var
  dmCon: TdmCon;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}
function TdmCon.doConnection(AUri: string; AParameter: array of string;
  AMethod: TRESTRequestMethod; out AResult: TJSONObject; Abody: string = '';
  AbodyType: TRESTContentType = ctAPPLICATION_JSON; ATimeOut:integer = 30000): boolean;
var
  LUrl: string;
begin
  try
    LUrl := SERVER + '/' + AUri;

    for var i := 0 to pred(length(AParameter)) do
      LUrl := LUrl + '/' + AParameter[i];

    RESTClient1.BaseURL := LUrl;
    RESTRequest1.Method := AMethod;
    RESTRequest1.ClearBody;
    RESTRequest1.ConnectTimeout:= ATimeOut;
    RESTRequest1.Timeout:= ATimeOut;
    if Abody.Trim <> '' then
      RESTRequest1.body.Add(Abody, AbodyType);

    RESTRequest1.Execute;

    if RESTResponse1.StatusCode = 200 then
    begin
      Result := true;
      if RESTResponse1.Content.Trim <> '' then
        AResult := TJSONObject.ParseJSONValue
          (TEncoding.UTF8.GetBytes(AnsiToUtf8(RESTResponse1.Content)), 0)
          as TJSONObject;

    end
    else
    begin
      Result := false;

      if RESTResponse1.Content.Trim <> '' then
        AResult := TJSONObject.ParseJSONValue
          (TEncoding.UTF8.GetBytes(AnsiToUtf8(RESTResponse1.Content)), 0)
          as TJSONObject;

    end;

  except
    on e: exception do
    begin
      Result := false;
    end;

  end;
end;


function TdmCon.doConnection(AUri: string; AParameter: array of string;
  AMethod: TRESTRequestMethod; out AResult: TJSONArray; Abody: string = '';
  AbodyType: TRESTContentType = ctAPPLICATION_JSON; ATimeOut:integer = 30000): boolean;
var
  LUrl: string;
begin
  try
    LUrl := SERVER + '/' + AUri;

    for var i := 0 to pred(length(AParameter)) do
      LUrl := LUrl + '/' + AParameter[i];

    RESTClient1.BaseURL := LUrl;
    RESTRequest1.Method := AMethod;
    RESTRequest1.body.ClearBody;
    RESTRequest1.ConnectTimeout:= ATimeOut;
    RESTRequest1.Timeout:= ATimeOut;

    if Abody.Trim <> '' then
      RESTRequest1.body.Add(Abody, AbodyType);

    RESTRequest1.Execute;

    if RESTResponse1.StatusCode = 200 then
    begin
      Result := true;
      if RESTResponse1.Content.Trim <> '' then
        AResult := TJSONObject.ParseJSONValue
          (TEncoding.UTF8.GetBytes(AnsiToUtf8(RESTResponse1.Content)), 0)
          as TJSONArray;

    end
    else
    begin
      Result := false;

      if RESTResponse1.Content.Trim <> '' then
        AResult := TJSONObject.ParseJSONValue
          (TEncoding.UTF8.GetBytes(AnsiToUtf8(RESTResponse1.Content)), 0)
          as TJSONArray;

    end;
  except
    on e: exception do
    begin
      Result := false;
    end;

  end;
end;

end.
