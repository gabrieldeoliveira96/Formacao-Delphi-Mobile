unit controller.pagamento;

interface

uses Horse, model.pagamento, System.JSON, System.SysUtils, controller.log,
  Horse.JWT, controller.JWT;

procedure pagamento;
procedure Post(Req: THorseRequest; Res: THorseResponse);
procedure PostAtualizarPagamento(Req: THorseRequest; Res: THorseResponse);

implementation

procedure PostAtualizarPagamento(Req: THorseRequest; Res: THorseResponse);
var
  LDmPagamento: TdmPagamento;
  LJson: TJSONObject;
begin
  try
    LDmPagamento := TdmPagamento.create(nil);
    try
      LJson := LDmPagamento.AtualizaTipoPagamento
        (TJSONObject.ParseJSONValue(Req.Body) as TJSONObject);

      Res.Send<TJSONObject>(LJson);

    finally
      Res.Status(200);
      FreeAndNil(LDmPagamento);
    end;
  except
    on e: exception do
    begin
      controller.log.log(e.Message);
      Res.Send(e.Message).Status(400);
      FreeAndNil(LDmPagamento);
    end;

  end;
end;

procedure Post(Req: THorseRequest; Res: THorseResponse);
var
  LDmPagamento: TdmPagamento;
  LJson: TJSONObject;
begin
  try
    LDmPagamento := TdmPagamento.create(nil);
    try
      LJson := LDmPagamento.pagamento(strtoint(Req.Params.Items['iduser']));

      Res.Send<TJSONObject>(LJson);

    finally
      Res.Status(200);
      FreeAndNil(LDmPagamento);
    end;
  except
    on e: exception do
    begin
      controller.log.log(e.Message);
      Res.Send(e.Message).Status(400);
      FreeAndNil(LDmPagamento);
    end;

  end;
end;

procedure pagamento;
begin
  THorse
//    .AddCallback(HorseJWT(controller.JWT.JwtPassword))
    .Post('/pagamento/:iduser', Post);

  THorse.Post('/pagamento/atualizar', PostAtualizarPagamento);
end;


end.
