program GosComponents;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Horse,
  Horse.Jhonson,
  Horse.CORS,
  System.IniFiles,
  System.SysUtils,
  vcl.Forms,
  controller.login in 'src\controller\controller.login.pas',
  model.con in 'src\model\model.con.pas' {dmCon: TDataModule},
  model.login in 'src\model\model.login.pas' {dmLogin: TDataModule},
  controller.jwt in 'src\controller\controller.jwt.pas',
  controller.usuario in 'src\controller\controller.usuario.pas',
  model.usuario in 'src\model\model.usuario.pas' {dmUsuario: TDataModule},
  controller.componentes in 'src\controller\controller.componentes.pas',
  model.componentes in 'src\model\model.componentes.pas' {dmComponentes: TDataModule},
  controller.log in 'src\controller\controller.log.pas',
  controller.pagamento in 'src\controller\controller.pagamento.pas',
  model.pagamento in 'src\model\model.pagamento.pas' {dmPagamento: TDataModule},
  model.email in 'Jobs\email\src\model\model.email.pas' {dmEnviarEmail: TDataModule},
  controller.email in 'Jobs\email\src\controller\controller.email.pas',
  FS.Email in 'Jobs\email\src\features\FS.Email.pas',
  GosComponents.ambiente in 'pagseguro\GosComponents.ambiente.pas';

begin
  HorseCORS
    .AllowedOrigin('*')
    .AllowedCredentials(true)
    .AllowedHeaders('*')
    .AllowedMethods('*')
    .ExposedHeaders('*');

  THorse.Use(CORS);
  THorse.Use(Jhonson());
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  controller.login.Login;
  controller.usuario.usuairo;
  controller.componentes.componentes;
  controller.pagamento.pagamento;

  {$IFDEF DEBUG}
  THorse.Listen(9000);
  {$ELSE}
  THorse.Listen(80);
  {$ENDIF}


end.
