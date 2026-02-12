unit controller.componentes;

interface

uses Horse, model.componentes, Horse.JWT, controller.jwt, System.json,
System.SysUtils, controller.log;

procedure componentes;
procedure Get(Req: THorseRequest; Res: THorseResponse);
procedure GetId(Req: THorseRequest; Res: THorseResponse);

implementation

procedure Get(Req: THorseRequest; Res: THorseResponse);
var
  LdmComponentes: TdmComponentes;
begin
  try
    LdmComponentes := TdmComponentes.create(nil);
    try

      res.Send<TJSONArray>(LdmComponentes.Get);

    finally
      Res.Status(200);
      FreeAndNil(LdmComponentes);
    end;
  except
    on e: exception do
    begin
      controller.log.Log(e.Message);
      Res.Send(e.Message).Status(400);
      FreeAndNil(LdmComponentes);
    end;

  end;
end;

procedure GetId(Req: THorseRequest; Res: THorseResponse);
var
  LdmComponentes: TdmComponentes;
begin
  try
    LdmComponentes := TdmComponentes.create(nil);
    try
      res.Send<TJSONObject>(LdmComponentes.Get(Req.Params.Items['id'].ToInteger));
    finally
      Res.Status(200);
      FreeAndNil(LdmComponentes);
    end;
  except
    on e: exception do
    begin
      controller.log.Log(e.Message);
      Res.Send(e.Message).Status(400);
      FreeAndNil(LdmComponentes);
    end;

  end;
end;

procedure componentes;
begin
  THorse
    .AddCallback(HorseJWT(controller.jwt.JwtPassword))
    .Get('/componentes', Get);
  THorse
    .AddCallback(HorseJWT(controller.jwt.JwtPassword))
    .Get('/componentes/:id', GetId);
end;


end.
