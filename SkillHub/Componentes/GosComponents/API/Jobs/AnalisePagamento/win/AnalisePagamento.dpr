program AnalisePagamento;

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  model.pagamento in '..\src\model\model.pagamento.pas' {dmPagamento: TDataModule},
  controller.log in '..\..\..\src\controller\controller.log.pas',
  GosComponents.ambiente in '..\..\..\pagseguro\GosComponents.ambiente.pas',
  controller.email in '..\..\email\src\controller\controller.email.pas',
  model.email in '..\..\email\src\model\model.email.pas' {dmEnviarEmail: TDataModule},
  FS.Email in '..\..\email\src\features\FS.Email.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TdmPagamento, dmPagamento);
  Application.CreateForm(TdmEnviarEmail, dmEnviarEmail);
  Application.Run;
end.
