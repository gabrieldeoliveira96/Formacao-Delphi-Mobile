program AnalisePagamentoServico;

uses
  Vcl.SvcMgr,
  uPrincipal in 'uPrincipal.pas' {GosAnalisePagamento: TService},
  model.pagamento in '..\src\model\model.pagamento.pas' {dmPagamento: TDataModule},
  controller.log in '..\..\..\src\controller\controller.log.pas',
  controller.email in '..\..\email\src\controller\controller.email.pas',
  FS.Email in '..\..\email\src\features\FS.Email.pas',
  model.email in '..\..\email\src\model\model.email.pas' {dmEnviarEmail: TDataModule},
  GosComponents.ambiente in '..\..\..\pagseguro\GosComponents.ambiente.pas';

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
  Application.CreateForm(TGosAnalisePagamento, GosAnalisePagamento);
  Application.Run;
end.
