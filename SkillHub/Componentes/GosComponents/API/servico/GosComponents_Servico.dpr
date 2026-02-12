program GosComponents_Servico;

uses
  Vcl.SvcMgr,
  Main.Service in 'src\Main.Service.pas' {GosComponentsAPIService: TService},
  controller.jwt in '..\src\controller\controller.jwt.pas',
  controller.login in '..\src\controller\controller.login.pas',
  controller.usuario in '..\src\controller\controller.usuario.pas',
  model.con in '..\src\model\model.con.pas' {dmCon: TDataModule},
  model.login in '..\src\model\model.login.pas' {dmLogin: TDataModule},
  model.usuario in '..\src\model\model.usuario.pas' {dmUsuario: TDataModule},
  controller.log in '..\src\controller\controller.log.pas',
  controller.email in '..\Jobs\email\src\controller\controller.email.pas',
  model.email in '..\Jobs\email\src\model\model.email.pas' {dmEnviarEmail: TDataModule},
  FS.Email in '..\Jobs\email\src\features\FS.Email.pas',
  controller.componentes in '..\src\controller\controller.componentes.pas',
  controller.pagamento in '..\src\controller\controller.pagamento.pas',
  model.componentes in '..\src\model\model.componentes.pas' {dmComponentes: TDataModule},
  model.pagamento in '..\src\model\model.pagamento.pas' {dmPagamento: TDataModule},
  GosComponents.ambiente in '..\pagseguro\GosComponents.ambiente.pas';

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
  Application.CreateForm(TGosComponentsAPIService, GosComponentsAPIService);
  Application.CreateForm(TdmEnviarEmail, dmEnviarEmail);
  Application.CreateForm(TdmPagamento, dmPagamento);
  Application.Run;
end.
