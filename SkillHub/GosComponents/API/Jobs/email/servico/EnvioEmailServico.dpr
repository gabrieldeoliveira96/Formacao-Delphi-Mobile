program EnvioEmailServico;

uses
  Vcl.SvcMgr,
  uPrincipal in 'uPrincipal.pas' {GosEnvioEmail: TService},
  model.email in '..\src\model\model.email.pas' {dmEnviarEmail: TDataModule},
  controller.email in '..\src\controller\controller.email.pas',
  FS.Email in '..\src\features\FS.Email.pas',
  controller.log in '..\..\..\src\controller\controller.log.pas';

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TGosEnvioEmail, GosEnvioEmail);
  Application.Run;
end.
