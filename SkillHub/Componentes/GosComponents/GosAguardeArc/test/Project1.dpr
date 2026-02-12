program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uFancyDialog in '..\..\..\Teste\Mensagem Personalisada FMX\uFancyDialog.pas',
  uGosDrawerComponents in '..\..\GosDrawerComponents\src\uGosDrawerComponents.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
