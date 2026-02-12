unit Gos.CEP.Consulta;

interface

uses IdHTTP, System.JSON, System.SysUtils, System.Classes;

type
  TCep = class
    Endereco: string;
    Cidade: string;
    Bairro: string;
    Uf: String;
  end;
function ConsultarCep(ACep: string): TCep;

implementation

function ConsultarCep(ACep: string): TCep;
Var
  IdHTTP: tIdHttp;
  XMLStream: TStringStream;
  Url, LResult: String;
  LJo: TJSONObject;
  LCep: String;
begin
 Tthread.Synchronize(Tthread.CurrentThread,
    procedure
    begin
      LCep := stringreplace(ACep, '-', '', [rfReplaceAll,
        rfIgnoreCase]);
      LCep := stringreplace(LCep, ' ', '', [rfReplaceAll, rfIgnoreCase]);
      LCep := LCep.trim;
    end);

  IdHTTP := tIdHttp.Create(Nil);
  try
    IdHTTP.ReadTimeout := 30000;
    IdHTTP.Request.Accept := 'text/xml';
    IdHTTP.Request.UserAgent := 'Embarcadero RESTClient/1.0';
    IdHTTP.Request.ContentType := 'application/x-www-form-urlencoded';
    IdHTTP.HandleRedirects := true;

    XMLStream := TStringStream.Create;

    Url := 'http://cep.republicavirtual.com.br/web_cep.php?cep=' + LCep +
      '&formato=json';

    LResult := IdHTTP.Get(Url);

    LJo := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LResult),0) as TJSONObject;
    try
      Result:= TCep.Create;
      Result.Endereco:= LJo.GetValue('tipo_logradouro').Value + ' ' +
        LJo.GetValue('logradouro').Value;
      Result.Cidade:= LJo.GetValue('cidade').Value;
      Result.Bairro:= LJo.GetValue('bairro').Value;
      Result.UF:= LJo.GetValue('uf').Value;

    finally
      FreeAndNil(LJo);

    end;
  finally
    IdHTTP.disposeOf;
  end;

end;

end.
