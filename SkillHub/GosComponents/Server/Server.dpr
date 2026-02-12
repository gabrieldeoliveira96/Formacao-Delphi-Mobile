program Server;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Horse,
  controller.register in 'src\controller\controller.register.pas',
  model.con in 'src\model\model.con.pas' {dmCon: TDataModule},
  model.register in 'src\model\model.register.pas' {dmCon1: TDataModule};

var
  App: THorse;

begin
  App := THorse.Create(9010);
  controller.register.register(App);
  App.Start;

end.
