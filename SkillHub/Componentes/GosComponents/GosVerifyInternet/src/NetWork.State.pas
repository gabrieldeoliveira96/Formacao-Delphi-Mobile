unit NetWork.State;

interface

type
{$IF not Defined(MSWINDOWS)}
  TCustomNetworkState = class(TObject)
    function CurrentSSID: string; virtual; abstract;
    function IsConnected: Boolean; virtual; abstract;
    function IsWifiConnected: Boolean; virtual; abstract;
    function IsMobileConnected: Boolean; virtual; abstract;
  end;

  TNetworkState = class(TCustomNetworkState)
  private
    FPlatformNetworkState: TCustomNetworkState;
  public
    constructor Create;
    destructor Destroy; override;
    function CurrentSSID: string; override;
    function IsConnected: Boolean; override;
    function IsWifiConnected: Boolean; override;
    function IsMobileConnected: Boolean; override;
  end;
{$ENDIF}

{$IF Defined(MSWINDOWS)}
  TNetworkState = class
    class function IsConnected: Boolean;
  end;
{$ENDIF}

implementation

{$IF not Defined(MSWINDOWS)}

uses
{$ENDIF}
{$IF Defined(IOS)}
  NetWork.iOS;
{$ENDIF}
{$IF Defined(ANDROID)}
NetWork.Android;
{$ENDIF}
{ TNetworkState }
{$IF not Defined(MSWINDOWS)}

constructor TNetworkState.Create;
begin
  inherited;
  FPlatformNetworkState := TPlatformNetworkState.Create;
end;

destructor TNetworkState.Destroy;
begin
  FPlatformNetworkState.Free;
  inherited;
end;

function TNetworkState.CurrentSSID: string;
begin
  Result := FPlatformNetworkState.CurrentSSID;
end;

function TNetworkState.IsConnected: Boolean;
begin
  if (not FPlatformNetworkState.IsMobileConnected) and (not FPlatformNetworkState.IsWifiConnected) then
    result := false
  else
    result := true;
//  Result := FPlatformNetworkState.IsConnected;
end;

function TNetworkState.IsMobileConnected: Boolean;
begin
  Result := FPlatformNetworkState.IsMobileConnected;
end;

function TNetworkState.IsWifiConnected: Boolean;
begin
  Result := FPlatformNetworkState.IsWifiConnected;
end;
{$ENDIF}

{$IF Defined(MSWINDOWS)}
class function TNetworkState.IsConnected: Boolean;
begin
  Result := true;
end;
{$ENDIF}

end.
