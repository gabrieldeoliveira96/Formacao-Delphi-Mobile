unit Main.Service;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Horse,
  controller.login, controller.usuario, Horse.Jhonson,
  Horse.CORS, controller.componentes, controller.pagamento;

type
  TGosComponentsAPIService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  GosComponentsAPIService: TGosComponentsAPIService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  GosComponentsAPIService.Controller(CtrlCode);
end;

function TGosComponentsAPIService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TGosComponentsAPIService.ServiceCreate(Sender: TObject);
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

  Login;
  usuairo;
  componentes;
  pagamento;

end;

procedure TGosComponentsAPIService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin

  {$IFDEF DEBUG}
  THorse.Listen(9000);
  {$ELSE}
  THorse.Listen(80);
  {$ENDIF}

  Started := True;

end;

procedure TGosComponentsAPIService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  THorse.StopListen;
  Stopped := True;

end;

end.
