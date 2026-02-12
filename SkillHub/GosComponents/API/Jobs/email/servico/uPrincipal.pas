unit uPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  model.email, Vcl.ExtCtrls;

type
  TGosEnvioEmail = class(TService)
    Timer1: TTimer;
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
    FdmEmail: TdmEnviarEmail;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  GosEnvioEmail: TGosEnvioEmail;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  GosEnvioEmail.Controller(CtrlCode);
end;

function TGosEnvioEmail.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TGosEnvioEmail.ServiceCreate(Sender: TObject);
begin
  FdmEmail := TdmEnviarEmail.Create(self);

end;

procedure TGosEnvioEmail.ServiceDestroy(Sender: TObject);
begin
  FreeAndNil(FdmEmail);

end;

procedure TGosEnvioEmail.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Started:= true;
  Timer1.Enabled:= true;


end;

procedure TGosEnvioEmail.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Stopped:= true;
  Timer1.Enabled:= false;

end;

procedure TGosEnvioEmail.Timer1Timer(Sender: TObject);
begin
  FdmEmail.EnviarEmail;

end;

end.
