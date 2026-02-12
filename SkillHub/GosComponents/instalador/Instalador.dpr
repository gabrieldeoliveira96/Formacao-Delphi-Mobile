program Instalador;











{$R *.dres}

uses
  Vcl.Forms,
  uInstalador in 'uInstalador.pas' {frmPrincipal},
  uGenerator in 'classes\uGenerator.pas',
  uDmPrincipal in 'src\dm\uDmPrincipal.pas' {dmCon: TDataModule},
  FS.Sistema in '..\..\TBDC\CommonFS\Classes\Sistema\FS.Sistema.pas',
  FS.Crypt in '..\..\TBDC\CommonFS\Classes\Sistema\FS.Crypt.pas',
  FS.Utils in '..\..\TBDC\CommonFS\Classes\Sistema\FS.Utils.pas',
  FS.Cloud in '..\..\TBDC\CommonFS\Classes\Sistema\FS.Cloud.pas',
  JclIDEUtils in 'JclIDEUtils.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
    ReportMemoryLeaksOnShutdown  := True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
