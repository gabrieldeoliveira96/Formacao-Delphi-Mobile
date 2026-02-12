unit controller.register;

interface
uses Horse, System.Sysutils, system.json, model.register;

procedure register(App: THorse);
procedure Post(Req: THorseRequest; Res: THorseResponse; Next: TProc);

implementation

procedure Post(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  dmRegister: TdmRegister;
  LJo:TJSONObject;
begin

  dmRegister := TdmRegister.Create(nil);
  try

    ljo:= dmRegister.Post(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(Req.Body),0) as TJSONObject);

    Res.Send(LJo);

  finally
    Res.Status(200);
    FreeAndNil(dmRegister);
  end;
end;

procedure register(App: THorse);
begin

  App.Post('/register', Post);

end;

end.
