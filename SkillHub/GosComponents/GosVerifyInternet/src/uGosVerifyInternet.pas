unit uGosVerifyInternet;

interface

uses
  System.SysUtils, System.Classes, NetWork.State;

type
  TGosVerifyInternet = class(TComponent)
  private
    FNetworkState: TNetworkState;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect: boolean;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosVerifyInternet]);
end;

{ TGosVerifyInternet }

constructor TGosVerifyInternet.Create(AOwner: TComponent);
begin
  inherited;
  FNetworkState := TNetworkState.Create;
end;

destructor TGosVerifyInternet.Destroy;
begin
  inherited;
  FNetworkState.Free;
end;

function TGosVerifyInternet.Connect: boolean;
begin
  Result:= FNetworkState.IsConnected;
end;

end.
