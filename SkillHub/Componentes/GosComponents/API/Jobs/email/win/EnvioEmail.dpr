program EnvioEmail;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form2},
  model.email in '..\src\model\model.email.pas' {dmEnviarEmail: TDataModule},
  controller.log in '..\..\..\src\controller\controller.log.pas',
  controller.email in '..\src\controller\controller.email.pas',
  FS.Email in '..\src\features\FS.Email.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TdmEnviarEmail, dmEnviarEmail);
  Application.Run;
end.
