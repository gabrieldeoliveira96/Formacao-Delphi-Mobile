unit uGosFmxAni;

{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if FMX.Ani.pas was not updated and adjust the IFDEF'}
{$ENDIF}

interface

uses System.Classes,
     System.SyncObjs,
     System.Rtti,
     System.Generics.Collections,
     System.UITypes,
     {$IFDEF IOS}
     System.TypInfo,
     Macapi.ObjectiveC,
     iOSapi.Foundation,
     iOSapi.QuartzCore,
     {$ENDIF}
     {$IFDEF ANDROID}
     Androidapi.JNIBridge,
     uGosAndroidApi,
     {$ENDIF}
     FMX.Types;

type

  {~~~~~~~~~~~~~~~~~~~}
  TGosAnimation = Class;

  {~~~~~~~~~~~~~~}
  {$IFDEF ANDROID}
  TGosChoreographerThread = class(TObject)
  private type
    TChoreographerFrameCallback = class(TJavaLocal, JChoreographer_FrameCallback)
    private
      [Weak] fChoreographerThread: TGosChoreographerThread;
    public
      constructor Create(const aAniCalculations: TGosChoreographerThread);
      procedure doFrame(frameTimeNanos: Int64); cdecl;
    end;
  private
    FChoreographer: JChoreographer;
    FChoreographerFrameCallback: TChoreographerFrameCallback;
  private
    FTimerEvent: TNotifyEvent;
    FInterval: Cardinal;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    property Interval: Cardinal read FInterval write SetInterval;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnTimer: TNotifyEvent read FTimerEvent write FTimerEvent;
  end;
  {$ENDIF}

  {~~~~~~~~~~}
  {$IFDEF IOS}
  TGosDisplayLinkThread = class(TObject)
  private type

    IDisplayLinkListener = interface(NSObject)
    ['{810AD3F0-265C-4A73-9B96-74103268884A}']
      procedure displayLinkUpdated; cdecl;
    end;

    TDisplayLinkListener = class(TOCLocal)
    private
      [Weak] fDisplayLinkThread: TGosDisplayLinkThread;
    protected
      function GetObjectiveCClass: PTypeInfo; override;
    public
      constructor Create(const aDisplayLinkThread: TGosDisplayLinkThread);
      procedure displayLinkUpdated; cdecl;
    end;

  private
    fDisplayLink: CADisplayLink;
    fDisplayLinkListener: TDisplayLinkListener;
  private
    FTimerEvent: TNotifyEvent;
    FInterval: Cardinal;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    property Interval: Cardinal read FInterval write SetInterval;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnTimer: TNotifyEvent read FTimerEvent write FTimerEvent;
  end;
  {$ENDIF}


  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TGosAniThread = class({$IF defined(ANDROID)}TGosChoreographerThread{$ELSEIF defined(IOS)}TGosDisplayLinkThread{$ELSE}TTimer{$ENDIF})
  private
    FAniList: TList<TGosAnimation>;
    FTime, FDeltaTime: Double;
    FTimerService: IFMXTimerService;
    procedure OneStep;
    procedure DoSyncTimer(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddAnimation(const Ani: TGosAnimation);
    procedure RemoveAnimation(const Ani: TGosAnimation);
    Procedure WakeUpTimer;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TGosAnimation = class(TObject)
  public const
    DefaultAniFrameRate = 60;
  public class var
    AniFrameRate: Integer;
  private class var
    FAniThread: TGosAniThread;
  private
    FTag: int64;
    [Weak] FTagObject: TObject;
    FTagFloat: Double;
    fOvershoot: Single;
    FTickCount : Integer;
    FDuration: Single;
    FDelay, FDelayTime: Single;
    FTime: Single;
    FInverse: Boolean;
    FSavedInverse: Boolean;
    FLoop: Boolean;
    FPause: Boolean;
    FRunning: Boolean;
    FOnFirstFrame: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FInterpolation: TInterpolationType;
    FAnimationType: TAnimationType;
    FEnabled: Boolean;
    FAutoReverse: Boolean;
    procedure SetEnabled(const Value: Boolean);
    class procedure Uninitialize;
  protected
    function GetNormalizedTime: Single;
    procedure FirstFrame; virtual;
    procedure ProcessAnimation; virtual; abstract;
    procedure DoProcess; virtual;
    procedure DoFinish; virtual;
  public
    class Procedure WakeUpTimer;
  public
    constructor Create; Virtual;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure StopAtCurrent; virtual;
    procedure ProcessTick(time, deltaTime: Single);
    property Running: Boolean read FRunning;
    property Pause: Boolean read FPause write FPause;
    property AnimationType: TAnimationType read FAnimationType write FAnimationType default TAnimationType.In;
    property AutoReverse: Boolean read FAutoReverse write FAutoReverse default False;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Delay: Single read FDelay write FDelay;
    property Duration: Single read FDuration write FDuration nodefault;
    property Interpolation: TInterpolationType read FInterpolation write FInterpolation default TInterpolationType.Linear;
    property Inverse: Boolean read FInverse write FInverse default False;
    property NormalizedTime: Single read GetNormalizedTime;
    property Loop: Boolean read FLoop write FLoop default False;
    property CurrentTime: Single read FTime;
    property OnFirstFrame: TNotifyEvent read FOnFirstFrame write FOnFirstFrame;
    property OnProcess: TNotifyEvent read FOnProcess write FOnProcess;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property Overshoot: Single read fOvershoot write fOvershoot;
    class property AniThread: TGosAniThread read FAniThread;
    property Tag: int64 read FTag write FTag default 0;
    property TagObject: TObject read FTagObject write FTagObject;
    property TagFloat: Double read FTagFloat write FTagFloat;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TGosFloatAnimation = class(TGosAnimation)
  private
    FStartFloat: Double;
    FStopFloat: Double;
    fcurrentFloat: Double;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create; override;
    procedure Start; override;
    property StartValue: Double read FStartFloat write FStartFloat;
    property StopValue: Double read FStopFloat write FStopFloat;
    property CurrentValue: Double read fcurrentFloat;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TGosColorAnimation = class(TGosAnimation)
  private
    FStartColor: TAlphaColor;
    FStopColor: TAlphaColor;
    fcurrentColor: TAlphaColor;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create; override;
    procedure Start; override;
    property StartValue: TAlphaColor read FStartColor write FStartColor;
    property StopValue: TAlphaColor read FStopColor write FStopColor;
    property CurrentValue: TAlphaColor read fcurrentColor;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TGosCustomPropertyAnimation = class(TFmxObject)
  private
  protected
    FInstance: TObject;
    FRttiProperty: TRttiProperty;
    FPath: string;
    FPropertyName: string;
    procedure SetPropertyName(const AValue: string);
    function FindProperty: Boolean;
    procedure ParentChanged; override;
  public
    property PropertyName: string read FPropertyName write SetPropertyName;
    procedure Stop; virtual;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TGosFloatPropertyAnimation = class(TGosCustomPropertyAnimation)
  private
    FStartFromCurrent: Boolean;
    fFloatAnimation: TGosFloatAnimation;
    FOnFirstFrame: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    function getAnimationType: TAnimationType;
    function getAutoReverse: Boolean;
    function getDelay: Single;
    function getDuration: Single;
    function getEnabled: Boolean;
    function getInterpolation: TInterpolationType;
    function getInverse: Boolean;
    function getLoop: Boolean;
    function getOvershoot: Single;
    function getPause: Boolean;
    function getRunning: Boolean;
    function GetStartValue: Single;
    function GetStopValue: Single;
    function GetCurrentValue: Single;
    function OvershootStored: Boolean;
    procedure setAnimationType(const Value: TAnimationType);
    procedure setAutoReverse(const Value: Boolean);
    procedure setDelay(const Value: Single);
    procedure setDuration(const Value: Single);
    procedure SetEnabled(const Value: Boolean);
    procedure setInterpolation(const Value: TInterpolationType);
    procedure setInverse(const Value: Boolean);
    procedure setLoop(const Value: Boolean);
    procedure setOvershoot(const Value: Single);
    procedure setPause(const Value: Boolean);
    procedure SetStartValue(const Value: Single);
    procedure setStopValue(const Value: Single);
  protected
    procedure doFirstFrame(Sender: TObject);
    procedure doProcess(Sender: TObject);
    procedure doFinish(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; override;
    procedure StopAtCurrent; virtual;
    property Running: Boolean read getRunning;
    property Pause: Boolean read getPause write setPause;
    property CurrentValue: Single read GetCurrentValue;
  published
    property AnimationType: TAnimationType read getAnimationType write setAnimationType default TAnimationType.In;
    property AutoReverse: Boolean read getAutoReverse write setAutoReverse default False;
    property Enabled: Boolean read getEnabled write SetEnabled default False;
    property Delay: Single read getDelay write setDelay;
    property Duration: Single read getDuration write setDuration nodefault;
    property Interpolation: TInterpolationType read getInterpolation write setInterpolation default TInterpolationType.Linear;
    property Inverse: Boolean read getInverse write setInverse default False;
    property Loop: Boolean read getLoop write setLoop default False;
    property OnFirstFrame: TNotifyEvent read fOnFirstFrame write fOnFirstFrame;
    property OnProcess: TNotifyEvent read fOnProcess write fOnProcess;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property PropertyName;
    property StartValue: Single read GetStartValue write SetStartValue stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: Single read GetStopValue write setStopValue stored True nodefault;
    property Overshoot: Single read getOvershoot write setOvershoot Stored OvershootStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TGosColorPropertyAnimation = class(TGosCustomPropertyAnimation)
  private
    FStartFromCurrent: Boolean;
    fColorAnimation: TGosColorAnimation;
    FOnFirstFrame: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    function getAnimationType: TAnimationType;
    function getAutoReverse: Boolean;
    function getDelay: Single;
    function getDuration: Single;
    function getEnabled: Boolean;
    function getInterpolation: TInterpolationType;
    function getInverse: Boolean;
    function getLoop: Boolean;
    function getOvershoot: Single;
    function getPause: Boolean;
    function getRunning: Boolean;
    function GetStartValue: TAlphaColor;
    function GetStopValue: TAlphaColor;
    function GetCurrentValue: TAlphaColor;
    function OvershootStored: Boolean;
    procedure setAnimationType(const Value: TAnimationType);
    procedure setAutoReverse(const Value: Boolean);
    procedure setDelay(const Value: Single);
    procedure setDuration(const Value: Single);
    procedure SetEnabled(const Value: Boolean);
    procedure setInterpolation(const Value: TInterpolationType);
    procedure setInverse(const Value: Boolean);
    procedure setLoop(const Value: Boolean);
    procedure setOvershoot(const Value: Single);
    procedure setPause(const Value: Boolean);
    procedure SetStartValue(const Value: TAlphaColor);
    procedure setStopValue(const Value: TAlphaColor);
  protected
    procedure doFirstFrame(Sender: TObject);
    procedure doProcess(Sender: TObject);
    procedure doFinish(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; override;
    procedure StopAtCurrent; virtual;
    property Running: Boolean read getRunning;
    property Pause: Boolean read getPause write setPause;
    property CurrentValue: TAlphaColor read GetCurrentValue;
  published
    property AnimationType: TAnimationType read getAnimationType write setAnimationType default TAnimationType.In;
    property AutoReverse: Boolean read getAutoReverse write setAutoReverse default False;
    property Enabled: Boolean read getEnabled write SetEnabled default False;
    property Delay: Single read getDelay write setDelay;
    property Duration: Single read getDuration write setDuration nodefault;
    property Interpolation: TInterpolationType read getInterpolation write setInterpolation default TInterpolationType.Linear;
    property Inverse: Boolean read getInverse write setInverse default False;
    property Loop: Boolean read getLoop write setLoop default False;
    property OnFirstFrame: TNotifyEvent read fOnFirstFrame write fOnFirstFrame;
    property OnProcess: TNotifyEvent read fOnProcess write fOnProcess;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property PropertyName;
    property StartValue: TAlphaColor read GetStartValue write SetStartValue stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: TAlphaColor read GetStopValue write setStopValue stored True nodefault;
    property Overshoot: Single read getOvershoot write setOvershoot Stored OvershootStored;
  end;

procedure Register;

implementation

uses System.SysUtils,
     System.math,
     {$IFDEF IOS}
     Macapi.ObjCRuntime,
     {$ENDIF}
     FMX.Platform,
     FMX.Ani,
     FMX.Utils,
     uGosString,
     uGosCommon;

{$IFDEF ANDROID}

{********************************************************************************************************************}
constructor TGosChoreographerThread.TChoreographerFrameCallback.Create(const aAniCalculations: TGosChoreographerThread);
 begin
  inherited Create;
  fChoreographerThread := aAniCalculations;
end;

{******************************************************************************************}
procedure TGosChoreographerThread.TChoreographerFrameCallback.doFrame(frameTimeNanos: Int64);
begin

  {$IFDEF DEBUG}
  //ALLog('TGosChoreographerThread.TChoreographerFrameCallback.doFrame', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  if assigned(fChoreographerThread.FTimerEvent) then
    fChoreographerThread.FTimerEvent(fChoreographerThread);

  if fChoreographerThread.Enabled then
    fChoreographerThread.fChoreographer.postFrameCallback(self);

end;

{************************************************************}
constructor TGosChoreographerThread.Create(AOwner: TComponent);
begin
  inherited create;
  fChoreographer := TJChoreographer.JavaClass.getInstance;
  fChoreographerFrameCallback := TChoreographerFrameCallback.create(self);
  FTimerEvent := nil;
  Interval := 1;
  FEnabled := False;
end;

{****************************************}
destructor TGosChoreographerThread.Destroy;
begin
  GosFreeAndNil(fChoreographerFrameCallback);
  inherited;
end;

{****************************************************************}
procedure TGosChoreographerThread.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    if FEnabled then fChoreographer.postFrameCallback(fChoreographerFrameCallback)
    else fChoreographer.removeFrameCallback(fChoreographerFrameCallback);
  end;
end;

{******************************************************************}
procedure TGosChoreographerThread.SetInterval(const Value: Cardinal);
begin
  FInterval := Max(Value, 1);
end;

{$ENDIF}

{$IFDEF IOS}

{***********************************************************************************************************}
constructor TGosDisplayLinkThread.TDisplayLinkListener.Create(const aDisplayLinkThread: TGosDisplayLinkThread);
begin
  inherited Create;
  fDisplayLinkThread := aDisplayLinkThread;
end;

{*********************************************************************}
procedure TGosDisplayLinkThread.TDisplayLinkListener.displayLinkUpdated;
begin

  {$IFDEF DEBUG}
  //ALLog('TGosDisplayLinkThread.TDisplayLinkListener.displayLinkUpdated', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  if assigned(fDisplayLinkThread.FTimerEvent) then
    fDisplayLinkThread.FTimerEvent(fDisplayLinkThread);

end;

{*******************************************************************************}
function TGosDisplayLinkThread.TDisplayLinkListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IDisplayLinkListener);
end;

{**********************************************************}
constructor TGosDisplayLinkThread.Create(AOwner: TComponent);
begin
  inherited create;
  fDisplayLinkListener := TDisplayLinkListener.Create(self);
  fDisplayLink := TCADisplayLink.Wrap(TCADisplayLink.OCClass.displayLinkWithTarget(fDisplayLinkListener.GetObjectID, sel_getUid('displayLinkUpdated')));
  fDisplayLink.retain;
  fDisplayLink.addToRunLoop(TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop), NSRunLoopCommonModes); // I don't really know with is the best, NSDefaultRunLoopMode or NSRunLoopCommonModes
  fDisplayLink.setPaused(true);
  FTimerEvent := nil;
  Interval := 1;
  FEnabled := False;
end;

{**************************************}
destructor TGosDisplayLinkThread.Destroy;
begin
  fDisplayLink.invalidate; // Removes the display link from all run loop modes.
                           // Removing the display link from all run loop modes causes it to be released by the run loop. The display link also releases the target.
                           // invalidate is thread safe meaning that it can be called from a thread separate to the one in which the display link is running.
  fDisplayLink.release;
  GosFreeAndNil(fDisplayLinkListener);
  inherited;
end;

{**************************************************************}
procedure TGosDisplayLinkThread.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    if FEnabled then fDisplayLink.setPaused(False)
    else fDisplayLink.setPaused(True);
  end;
end;

{******************************************************************}
procedure TGosDisplayLinkThread.SetInterval(const Value: Cardinal);
begin
  FInterval := Max(Value, 1);
end;

{$ENDIF}

{******************************}
constructor TGosAniThread.Create;
begin
  inherited Create(nil);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    raise EUnsupportedPlatformService.Create('IFMXTimerService');
  TGosAnimation.AniFrameRate := EnsureRange(TGosAnimation.AniFrameRate, 5, 100);
  Interval := Trunc(1000 / TGosAnimation.AniFrameRate / 10) * 10;
  if (Interval <= 0) then
    Interval := 1;

  OnTimer := DoSyncTimer;
  FAniList := TList<TGosAnimation>.Create;
  FTime := FTimerService.GetTick;

  Enabled := False;
end;

{******************************}
destructor TGosAniThread.Destroy;
begin
  GosFreeAndNil(FAniList);
  FTimerService := nil;
  inherited;
end;

{***********************************************************}
procedure TGosAniThread.AddAnimation(const Ani: TGosAnimation);
begin
  if FAniList.IndexOf(Ani) < 0 then
    FAniList.Add(Ani);
  if not Enabled and (FAniList.Count > 0) then
    FTime := FTimerService.GetTick;
  Enabled := FAniList.Count > 0;
end;

{**************************************************************}
procedure TGosAniThread.RemoveAnimation(const Ani: TGosAnimation);
begin
  FAniList.Remove(Ani);
  Enabled := FAniList.Count > 0;
end;

{*********************************}
Procedure TGosAniThread.WakeUpTimer;
begin
  if not enabled then exit;
  if FTimerService.GetTick - FTime > 0.04 then begin // normally DoSyncTimer must be called every 0.016 seconds.
                                                     // but in heavy situation, especially on ios, the CADisplay link
                                                     // could never fire. I saw it on iphone 5 playing webRTC + filter
                                                     // the GPU was so busy that the CADisplay link never fire.
                                                     // so if it's was not called for more than 0.04 seconds (0.016*2 + 0.016/2)
                                                     // then call it again
    {$IFDEF DEBUG}
    GosLog('TGosAniThread.WakeUpTimer', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TGosLogType.warn);
    {$ENDIF}
    DoSyncTimer(nil);
  end;
end;

{**************************************************}
procedure TGosAniThread.DoSyncTimer(Sender: TObject);
begin
  OneStep;
  if TGosAnimation.AniFrameRate < 5 then
    TGosAnimation.AniFrameRate := 5;
  Interval := Trunc(1000 / TGosAnimation.AniFrameRate / 10) * 10;
  if (Interval <= 0) then Interval := 1;
end;

{*****************************}
procedure TGosAniThread.OneStep;
var
  I: Integer;
  NewTime: Double;
  [unsafe] Ani: TGosAnimation;
begin
  NewTime := FTimerService.GetTick;
  FDeltaTime := NewTime - FTime;
  FTime := NewTime;
  if FDeltaTime <= 0 then
    Exit;
  if FAniList.Count > 0 then
  begin
    I := FAniList.Count - 1;
    while I >= 0 do
    begin
      Ani := FAniList[I];
      if Ani.FRunning then
      begin
        Ani.ProcessTick(FTime, FDeltaTime);
      end;
      Dec(I);
      if I >= FAniList.Count then
        I := FAniList.Count - 1;
    end;
  end;
end;

{******************************}
constructor TGosAnimation.Create;
begin
  inherited;
  FEnabled := False;
  Duration := 0.2;
  fTag := 0;
  FTagObject := nil;
  FTagFloat := 0.0;
  fOvershoot := 0.0;
end;

{******************************}
destructor TGosAnimation.Destroy;
begin
  if AniThread <> nil then
    TGosAniThread(AniThread).FAniList.Remove(Self);
  inherited;
end;

{********************************}
procedure TGosAnimation.FirstFrame;
begin
  if assigned(fOnFirstFrame) then
    fOnFirstFrame(Self);
end;

{******************************************************}
procedure TGosAnimation.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then
      Start
    else
      Stop;
  end;
end;

{**********************************************}
function TGosAnimation.GetNormalizedTime: Single;
begin
  Result := 0;
  if (FDuration > 0) and (FDelayTime <= 0) then
  begin
    case FInterpolation of
      TInterpolationType.Linear:
        Result := InterpolateLinear(FTime, 0, 1, FDuration);
      TInterpolationType.Quadratic:
        Result := InterpolateQuad(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Cubic:
        Result := InterpolateCubic(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Quartic:
        Result := InterpolateQuart(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Quintic:
        Result := InterpolateQuint(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Sinusoidal:
        Result := InterpolateSine(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Exponential:
        Result := InterpolateExpo(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Circular:
        Result := InterpolateCirc(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Elastic:
        Result := InterpolateElastic(FTime, 0, 1, FDuration, 0, 0, FAnimationType);
      TInterpolationType.Back:
        Result := InterpolateBack(FTime, 0, 1, FDuration, fOvershoot, FAnimationType);
      TInterpolationType.Bounce:
        Result := InterpolateBounce(FTime, 0, 1, FDuration, FAnimationType);
    end;
  end;
end;

{*******************************}
procedure TGosAnimation.DoProcess;
begin
  if fEnabled and Assigned(FOnProcess) then // << i set that if enabled is false then the FOnProcess will not run
    FOnProcess(Self);
end;

{******************************}
procedure TGosAnimation.DoFinish;
begin
  if fEnabled and Assigned(FOnFinish) then // << i set that if enabled is false then the FOnFinish will not run
    FOnFinish(Self);
end;

{**********************************************************}
procedure TGosAnimation.ProcessTick(time, deltaTime: Single);
begin
  inherited;

  if (not FRunning) or FPause then
    Exit;

  if (FDelay > 0) and (FDelayTime <> 0) then
  begin
    if FDelayTime > 0 then
    begin
      FDelayTime := FDelayTime - deltaTime;
      if FDelayTime <= 0 then
      begin
        FDelayTime := 0;
        if FInverse then
          FTime := FDuration
        else
          FTime := 0;
        FirstFrame;
        ProcessAnimation;
        DoProcess;
      end;
    end;
    Exit;
  end;

  if FInverse then
    FTime := FTime - deltaTime
  else
    FTime := FTime + deltaTime;
  if FTime >= FDuration then
  begin
    FTime := FDuration;
    if FLoop then
    begin
      if FAutoReverse then
      begin
        FInverse := True;
        FTime := FDuration;
      end
      else
        FTime := 0;
    end
    else
      if FAutoReverse and (FTickCount = 0) then
      begin
        Inc(FTickCount);
        FInverse := True;
        FTime := FDuration;
      end
      else
        FRunning := False;
  end
  else if FTime <= 0 then
  begin
    FTime := 0;
    if FLoop then
    begin
      if FAutoReverse then
      begin
        FInverse := False;
        FTime := 0;
      end
      else
        FTime := FDuration;
    end
    else
      if FAutoReverse and (FTickCount = 0) then
      begin
        Inc(FTickCount);
        FInverse := False;
        FTime := 0;
      end
      else
        FRunning := False;
  end;

  ProcessAnimation;
  DoProcess;

  if not FRunning then
  begin
    if AutoReverse then
      FInverse := FSavedInverse;
    if AniThread <> nil then
      TGosAniThread(AniThread).RemoveAnimation(Self);
    DoFinish;
  end;
end;

{***************************}
procedure TGosAnimation.Start;
var
  SaveDuration: Single;
begin
  if not FLoop then
    FTickCount := 0;
  if AutoReverse then
  begin
    if Running then
      FInverse := FSavedInverse
    else
      FSavedInverse := FInverse;
  end;
  if (Abs(FDuration) < 0.001) then
  begin
    { immediate animation }
    SaveDuration := FDuration;
    try
      FDelayTime := 0;
      FDuration := 1;
      if FInverse then
        FTime := 0
      else
        FTime := FDuration;
      FRunning := True;
      ProcessAnimation;
      DoProcess;
      FRunning := False;
      FTime := 0;
      DoFinish;
    finally
      FDuration := SaveDuration;
    end;
  end
  else
  begin
    FDelayTime := FDelay;
    FRunning := True;
    if FInverse then
      FTime := FDuration
    else
      FTime := 0;
    if FDelay = 0 then
    begin
      FirstFrame;
      ProcessAnimation;
      DoProcess;
    end;

    if AniThread = nil then
      FAniThread := TGosAniThread.Create;

    AniThread.AddAnimation(Self);
    if not AniThread.Enabled then
      Stop
    else
      FEnabled := True;
  end;
end;

{**************************}
procedure TGosAnimation.Stop;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    TGosAniThread(AniThread).RemoveAnimation(Self);

  if AutoReverse then
    FInverse := FSavedInverse;

  if FInverse then
    FTime := 0
  else
    FTime := FDuration;
  ProcessAnimation;
  DoProcess;
  FRunning := False;
  DoFinish;
end;

{***********************************}
procedure TGosAnimation.StopAtCurrent;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    TGosAniThread(AniThread).RemoveAnimation(Self);

  if AutoReverse then
    FInverse := FSavedInverse;

  if FInverse then
    FTime := 0
  else
    FTime := FDuration;
  FRunning := False;
  FEnabled := False;
  DoFinish;
end;

{****************************************}
class procedure TGosAnimation.Uninitialize;
begin
  GosFreeAndNil(FAniThread);
end;

{***************************************}
class Procedure TGosAnimation.WakeUpTimer;
begin
  if AniThread <> nil then FAniThread.WakeUpTimer
end;

{***********************************}
constructor TGosFloatAnimation.Create;
begin
  inherited;
  Duration := 0.2;
  FStartFloat := 0;
  FStopFloat := 0;
  fCurrentFloat := 0;
end;

{********************************}
procedure TGosFloatAnimation.Start;
begin
  fCurrentFloat := FStartFloat;
  inherited Start;
end;

{*******************************************}
procedure TGosFloatAnimation.ProcessAnimation;
begin
  fCurrentFloat := FStartFloat + (FStopFloat - FStartFloat) * NormalizedTime;
end;

{***********************************}
constructor TGosColorAnimation.Create;
begin
  inherited;
  Duration := 0.2;
  FStartColor := $FFFFFFFF;
  FStopColor := $FFFFFFFF;
  fCurrentColor := $FFFFFFFF;
end;

{********************************}
procedure TGosColorAnimation.Start;
begin
  fCurrentColor := FStartColor;
  inherited Start;
end;

{*******************************************}
procedure TGosColorAnimation.ProcessAnimation;
begin
  fCurrentColor := InterpolateColor(FStartColor, FStopColor, NormalizedTime);
end;

{********************************************************}
function TGosCustomPropertyAnimation.FindProperty: Boolean;
var
  Persistent: string;
  Comp: TFmxObject;
  I: Integer;
  T: TRttiType;
  P: TRttiProperty;
  Properties: TList<TRttiProperty>;
begin
  Result := False;

  //This is to permit to put a TALxxxPropertyAnimation on a Form
  //but use it only via it's onprocess event
  if FPropertyName = '' then begin
    FInstance := nil;
    FRttiProperty := nil;
    FPath := '';
    exit(true);
  end;

  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while FPath.Contains('.') do
      begin
        Persistent := GetToken(FPath, '.');
        T := SharedContext.GetType(FInstance.ClassInfo);
        if T <> nil then
        begin
          P := T.GetProperty(Persistent);
          if (P <> nil) and (P.PropertyType.IsInstance) then
            FInstance := P.GetValue(FInstance).AsObject
          else
          if Parent <> nil then
          begin
            for I := 0 to Parent.ChildrenCount - 1 do
              if CompareText(Parent.Children[I].Name, Persistent) = 0 then
              begin
                Comp := Parent.Children[I];
                T := SharedContext.GetType(Comp.ClassInfo);
                if T <> nil then
                begin
                  P := T.GetProperty(FPath);
                  if P <> nil then
                  begin
                    FInstance := Comp;
                    Break;
                  end;
                end;
              end;
          end;
        end;
      end;
      if FInstance <> nil then
      begin

        if not ClonePropertiesCache.TryGetValue(FInstance.ClassName, Properties) then
        begin
          Properties := TList<TRttiProperty>.Create;
          ClonePropertiesCache.Add(FInstance.ClassName, Properties);
        end;

        for P in Properties do
          if P.Name = FPath then
          begin
            FRttiProperty := P;
            Break;
          end;

        if FRttiProperty = nil then
        begin
          T := SharedContext.GetType(FInstance.ClassInfo);
          FRttiProperty := T.GetProperty(FPath);
          if FRttiProperty <> nil then
            Properties.Add(FRttiProperty);
        end;
        Result := FRttiProperty <> nil;
      end;
    end
    else
      Result := True;
  end;
end;

{*************************************************}
procedure TGosCustomPropertyAnimation.ParentChanged;
begin
  inherited;
  FInstance := nil;
end;

{*************************************************************************}
procedure TGosCustomPropertyAnimation.SetPropertyName(const AValue: string);
begin
  if not SameText(AValue, PropertyName) then
    FInstance := nil;
  FPropertyName := AValue;
end;

{****************************************}
procedure TGosCustomPropertyAnimation.Stop;
begin
  FInstance := nil;
end;

{***************************************************************}
constructor TGosFloatPropertyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartFromCurrent := False;
  fFloatAnimation := TGosFloatAnimation.Create;
  fFloatAnimation.OnFirstFrame := DoFirstFrame;
  fFloatAnimation.OnProcess := DoProcess;
  fFloatAnimation.OnFinish := DoFinish;
  FOnFirstFrame := nil;
  FOnProcess := nil;
  FOnFinish := nil;
end;

{*******************************************}
destructor TGosFloatPropertyAnimation.Destroy;
begin
  fFloatAnimation.Enabled := False;
  GosFreeAndNil(fFloatAnimation);
  inherited;
end;

{****************************************************************}
procedure TGosFloatPropertyAnimation.doFirstFrame(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if (FInstance <> nil) and StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        StartValue := P.GetValue(FInstance).AsExtended;
    end;
  end;
  if assigned(fOnFirstFrame) then
    fOnFirstFrame(self);
end;

{*************************************************************}
procedure TGosFloatPropertyAnimation.doProcess(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        P.SetValue(FInstance, fFloatAnimation.CurrentValue);
    end;
  end;
  if assigned(fOnProcess) then
    fOnProcess(self);
end;

{************************************************************}
procedure TGosFloatPropertyAnimation.doFinish(Sender: TObject);
begin
  if assigned(fOnFinish) then
    fOnFinish(self);
end;

{******************************************************************}
function TGosFloatPropertyAnimation.getAnimationType: TAnimationType;
begin
  result := fFloatAnimation.AnimationType;
end;

{*********************************************************}
function TGosFloatPropertyAnimation.getAutoReverse: Boolean;
begin
  result := fFloatAnimation.AutoReverse;
end;

{**************************************************}
function TGosFloatPropertyAnimation.getDelay: Single;
begin
  result := fFloatAnimation.Delay;
end;

{*****************************************************}
function TGosFloatPropertyAnimation.getDuration: Single;
begin
  result := fFloatAnimation.Duration;
end;

{*****************************************************}
function TGosFloatPropertyAnimation.getEnabled: Boolean;
begin
  result := fFloatAnimation.Enabled;
end;

{**********************************************************************}
function TGosFloatPropertyAnimation.getInterpolation: TInterpolationType;
begin
  result := fFloatAnimation.Interpolation;
end;

{*****************************************************}
function TGosFloatPropertyAnimation.getInverse: Boolean;
begin
  result := fFloatAnimation.Inverse;
end;

{**************************************************}
function TGosFloatPropertyAnimation.getLoop: Boolean;
begin
  result := fFloatAnimation.Loop;
end;

{******************************************************}
function TGosFloatPropertyAnimation.getOvershoot: Single;
begin
  result := fFloatAnimation.Overshoot;
end;

{***************************************************}
function TGosFloatPropertyAnimation.getPause: Boolean;
begin
  result := fFloatAnimation.Pause;
end;

{*****************************************************}
function TGosFloatPropertyAnimation.getRunning: Boolean;
begin
  result := fFloatAnimation.Running;
end;

{*******************************************************}
function TGosFloatPropertyAnimation.GetStartValue: Single;
begin
  result := fFloatAnimation.StartValue;
end;

{******************************************************}
function TGosFloatPropertyAnimation.GetStopValue: Single;
begin
  result := fFloatAnimation.StopValue;
end;

{*********************************************************}
function TGosFloatPropertyAnimation.GetCurrentValue: Single;
begin
  result := fFloatAnimation.CurrentValue;
end;

{**********************************************************}
function TGosFloatPropertyAnimation.OvershootStored: Boolean;
begin
  result := fFloatAnimation.Overshoot <> 0;
end;

{********************************************************************************}
procedure TGosFloatPropertyAnimation.setAnimationType(const Value: TAnimationType);
begin
  fFloatAnimation.AnimationType := Value;
end;

{***********************************************************************}
procedure TGosFloatPropertyAnimation.setAutoReverse(const Value: Boolean);
begin
  fFloatAnimation.AutoReverse := Value;
end;

{****************************************************************}
procedure TGosFloatPropertyAnimation.setDelay(const Value: Single);
begin
  fFloatAnimation.Delay := Value;
end;

{*******************************************************************}
procedure TGosFloatPropertyAnimation.setDuration(const Value: Single);
begin
  fFloatAnimation.Duration := Value;
end;

{*******************************************************************}
procedure TGosFloatPropertyAnimation.SetEnabled(const Value: Boolean);
begin
  if (fFloatAnimation.Enabled <> Value) and
     ((not value) or
      (FindProperty)) then fFloatAnimation.Enabled := Value;
end;

{************************************************************************************}
procedure TGosFloatPropertyAnimation.setInterpolation(const Value: TInterpolationType);
begin
  fFloatAnimation.Interpolation := Value;
end;

{*******************************************************************}
procedure TGosFloatPropertyAnimation.setInverse(const Value: Boolean);
begin
  fFloatAnimation.Inverse := Value;
end;

{****************************************************************}
procedure TGosFloatPropertyAnimation.setLoop(const Value: Boolean);
begin
  fFloatAnimation.Loop := Value;
end;

{********************************************************************}
procedure TGosFloatPropertyAnimation.setOvershoot(const Value: Single);
begin
  fFloatAnimation.Overshoot := Value;
end;

{*****************************************************************}
procedure TGosFloatPropertyAnimation.setPause(const Value: Boolean);
begin
  fFloatAnimation.Pause := Value;
end;

{*********************************************************************}
procedure TGosFloatPropertyAnimation.SetStartValue(const Value: Single);
begin
  fFloatAnimation.StartValue := Value;
end;

{********************************************************************}
procedure TGosFloatPropertyAnimation.setStopValue(const Value: Single);
begin
  fFloatAnimation.StopValue := Value;
end;

{****************************************}
procedure TGosFloatPropertyAnimation.Start;
begin
  if FindProperty then
    fFloatAnimation.Start;
end;

{***************************************}
procedure TGosFloatPropertyAnimation.Stop;
begin
  fFloatAnimation.Stop;
  inherited;
end;

{************************************************}
procedure TGosFloatPropertyAnimation.StopAtCurrent;
begin
  fFloatAnimation.StopAtCurrent;
end;

{***************************************************************}
constructor TGosColorPropertyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartFromCurrent := False;
  fColorAnimation := TGosColorAnimation.Create;
  fColorAnimation.OnFirstFrame := DoFirstFrame;
  fColorAnimation.OnProcess := DoProcess;
  fColorAnimation.OnFinish := DoFinish;
  FOnFirstFrame := nil;
  FOnProcess := nil;
  FOnFinish := nil;
end;

{*******************************************}
destructor TGosColorPropertyAnimation.Destroy;
begin
  fColorAnimation.Enabled := False;
  GosFreeAndNil(fColorAnimation);
  inherited;
end;

{****************************************************************}
procedure TGosColorPropertyAnimation.doFirstFrame(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if (FInstance <> nil) and StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsOrdinal) then
        StartValue := TAlphaColor(P.GetValue(FInstance).AsOrdinal);
    end;
  end;
  if assigned(fOnFirstFrame) then
    fOnFirstFrame(self);
end;

{*************************************************************}
procedure TGosColorPropertyAnimation.doProcess(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsOrdinal) then
        P.SetValue(FInstance, fColorAnimation.CurrentValue);
    end;
  end;
  if assigned(fOnProcess) then
    fOnProcess(self);
end;

{************************************************************}
procedure TGosColorPropertyAnimation.doFinish(Sender: TObject);
begin
  if assigned(fOnFinish) then
    fOnFinish(self);
end;

{******************************************************************}
function TGosColorPropertyAnimation.getAnimationType: TAnimationType;
begin
  result := fColorAnimation.AnimationType;
end;

{*********************************************************}
function TGosColorPropertyAnimation.getAutoReverse: Boolean;
begin
  result := fColorAnimation.AutoReverse;
end;

{**************************************************}
function TGosColorPropertyAnimation.getDelay: Single;
begin
  result := fColorAnimation.Delay;
end;

{*****************************************************}
function TGosColorPropertyAnimation.getDuration: Single;
begin
  result := fColorAnimation.Duration;
end;

{*****************************************************}
function TGosColorPropertyAnimation.getEnabled: Boolean;
begin
  result := fColorAnimation.Enabled;
end;

{**********************************************************************}
function TGosColorPropertyAnimation.getInterpolation: TInterpolationType;
begin
  result := fColorAnimation.Interpolation;
end;

{*****************************************************}
function TGosColorPropertyAnimation.getInverse: Boolean;
begin
  result := fColorAnimation.Inverse;
end;

{**************************************************}
function TGosColorPropertyAnimation.getLoop: Boolean;
begin
  result := fColorAnimation.Loop;
end;

{******************************************************}
function TGosColorPropertyAnimation.getOvershoot: Single;
begin
  result := fColorAnimation.Overshoot;
end;

{***************************************************}
function TGosColorPropertyAnimation.getPause: Boolean;
begin
  result := fColorAnimation.Pause;
end;

{*****************************************************}
function TGosColorPropertyAnimation.getRunning: Boolean;
begin
  result := fColorAnimation.Running;
end;

{************************************************************}
function TGosColorPropertyAnimation.GetStartValue: TAlphaColor;
begin
  result := fColorAnimation.StartValue;
end;

{***********************************************************}
function TGosColorPropertyAnimation.GetStopValue: TAlphaColor;
begin
  result := fColorAnimation.StopValue;
end;

{**************************************************************}
function TGosColorPropertyAnimation.GetCurrentValue: TAlphaColor;
begin
  result := fColorAnimation.CurrentValue;
end;

{**********************************************************}
function TGosColorPropertyAnimation.OvershootStored: Boolean;
begin
  result := fColorAnimation.Overshoot <> 0;
end;

{********************************************************************************}
procedure TGosColorPropertyAnimation.setAnimationType(const Value: TAnimationType);
begin
  fColorAnimation.AnimationType := Value;
end;

{***********************************************************************}
procedure TGosColorPropertyAnimation.setAutoReverse(const Value: Boolean);
begin
  fColorAnimation.AutoReverse := Value;
end;

{****************************************************************}
procedure TGosColorPropertyAnimation.setDelay(const Value: Single);
begin
  fColorAnimation.Delay := Value;
end;

{*******************************************************************}
procedure TGosColorPropertyAnimation.setDuration(const Value: Single);
begin
  fColorAnimation.Duration := Value;
end;

{*******************************************************************}
procedure TGosColorPropertyAnimation.SetEnabled(const Value: Boolean);
begin
  if (fColorAnimation.Enabled <> Value) and
     ((not value) or
      (FindProperty)) then fColorAnimation.Enabled := Value;
end;

{************************************************************************************}
procedure TGosColorPropertyAnimation.setInterpolation(const Value: TInterpolationType);
begin
  fColorAnimation.Interpolation := Value;
end;

{*******************************************************************}
procedure TGosColorPropertyAnimation.setInverse(const Value: Boolean);
begin
  fColorAnimation.Inverse := Value;
end;

{****************************************************************}
procedure TGosColorPropertyAnimation.setLoop(const Value: Boolean);
begin
  fColorAnimation.Loop := Value;
end;

{********************************************************************}
procedure TGosColorPropertyAnimation.setOvershoot(const Value: Single);
begin
  fColorAnimation.Overshoot := Value;
end;

{*****************************************************************}
procedure TGosColorPropertyAnimation.setPause(const Value: Boolean);
begin
  fColorAnimation.Pause := Value;
end;

{**************************************************************************}
procedure TGosColorPropertyAnimation.SetStartValue(const Value: TAlphaColor);
begin
  fColorAnimation.StartValue := Value;
end;

{*************************************************************************}
procedure TGosColorPropertyAnimation.setStopValue(const Value: TAlphaColor);
begin
  fColorAnimation.StopValue := Value;
end;

{****************************************}
procedure TGosColorPropertyAnimation.Start;
begin
  if FindProperty then
    fColorAnimation.Start;
end;

{***************************************}
procedure TGosColorPropertyAnimation.Stop;
begin
  fColorAnimation.Stop;
  inherited;
end;

{************************************************}
procedure TGosColorPropertyAnimation.StopAtCurrent;
begin
  fColorAnimation.StopAtCurrent;
end;

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosFloatPropertyAnimation]);
  RegisterComponents('GosComponent', [TGosColorPropertyAnimation]);
end;

{************}
initialization
  RegisterFmxClasses([TGosFloatPropertyAnimation]);
  RegisterFmxClasses([TGosColorPropertyAnimation]);
  TGosAnimation.AniFrameRate := TGosAnimation.DefaultAniFrameRate;

{**********}
finalization
  TGosAnimation.Uninitialize;

end.
