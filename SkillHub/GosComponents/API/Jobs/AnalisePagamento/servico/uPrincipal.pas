unit uPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  Vcl.ExtCtrls, model.pagamento;

type
  TGosAnalisePagamento = class(TService)
    Timer1: TTimer;
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
    FDmPagamento: TdmPagamento;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  GosAnalisePagamento: TGosAnalisePagamento;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  GosAnalisePagamento.Controller(CtrlCode);
end;

function TGosAnalisePagamento.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TGosAnalisePagamento.ServiceCreate(Sender: TObject);
begin
  FDmPagamento := TdmPagamento.Create(self);
  FDmPagamento.ConfiguraComponente;

end;

procedure TGosAnalisePagamento.ServiceDestroy(Sender: TObject);
begin
  FreeAndNil(FDmPagamento);

end;

procedure TGosAnalisePagamento.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Started:= true;
  Timer1.Enabled := true;

end;

procedure TGosAnalisePagamento.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Stopped:= true;
  Timer1.Enabled := false;

end;

procedure TGosAnalisePagamento.Timer1Timer(Sender: TObject);
begin
  FDmPagamento.AnalisePagamento;

end;

end.
