unit uGosToast_v2;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.StdCtrls,
  FMX.Layouts, System.UITypes, FMX.Forms, FMX.Objects, FMX.Graphics,
  FMX.Ani
{$IF Defined(ANDROID)}
  , FMX.Helpers.Android,
   Androidapi.Helpers
{$ENDIF}  ;

type
  TType = (Sucess, Erro, Info, Waring);
  [ComponentPlatforms($FFFF)]
  TGosToast_v2 = class(TFmxObject)
  private
    FOwner: TComponent;
    FMsg: TLabel;
    FLayout: TLayout;
    FCorDefault: TAlphaColor;
    FCor: TAlphaColor;
    FRec: TRectangle;
    FDuration: single;
    FFloatAnimatin: TFloatAnimation;
    FTypeToast: TType;
    FCorWaring: TAlphaColor;
    FCorInfo: TAlphaColor;
    FCorError: TAlphaColor;
    FCorSucess: TAlphaColor;
    FMensagem: string;
    procedure FloatAnimationFinish(Sender: TObject);
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Show;
  published
    property Duration:single read FDuration write FDuration;
    property TypeToast:TType read FTypeToast write FTypeToast;
    property CorSucess:TAlphaColor read FCorSucess write FCorSucess;
    property CorError:TAlphaColor read FCorError write FCorError;
    property CorInfo:TAlphaColor read FCorInfo write FCorInfo;
    property CorWaring:TAlphaColor read FCorWaring write FCorWaring;
    property Mensagem:string read FMensagem write FMensagem;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosToast_v2]);
end;

constructor TGosToast_v2.Create(AOwner: TComponent);
begin
  inherited;
  Duration:= 3;
  FOwner := AOwner;

  FCorWaring:= TAlphaColors.Yellow;
  FCorInfo:= TAlphaColors.Aqua;
  FCorError:= TAlphaColors.Red;
  FCorSucess:= TAlphaColors.Green;

end;

procedure TGosToast_v2.Show;
var
 LCor:TAlphaColor;
begin

  case FTypeToast of
    Sucess:
      LCor := FCorSucess;
    Erro:
      LCor := FCorError;
    Info:
      LCor := FCorInfo;
    Waring:
      LCor := FCorWaring;
  end;

  FLayout := TLayout.Create(nil);
  FLayout.Position.Y := -TForm(FOwner).Height;
  FLayout.Parent := TForm(FOwner);
  FLayout.Height := 60;

  FLayout.Width := TForm(FOwner).Width;
  FLayout.Position.X := trunc(TForm(FOwner).Width / 2) - trunc(FLayout.Width / 2);

  FRec := TRectangle.Create(nil);
  FRec.Parent := FLayout;
  FRec.Align := TAlignLayout.Client;
  FRec.Stroke.Kind := TBrushKind.None;
  FRec.Fill.Color := LCor;

  FMsg := TLabel.Create(nil);
  FMsg.Parent := FRec;
  FMsg.Align := TAlignLayout.Client;
  FMsg.StyledSettings := [];
  FMsg.Margins.Left := 10;
  FMsg.Margins.Right := 10;
  FMsg.TextSettings.Font.Style := [TFontStyle.fsBold];
  FMsg.TextSettings.Trimming := TTextTrimming.None;
  FMsg.TextSettings.VertAlign := TTextAlign.Center;
  FMsg.TextSettings.FontColor := TAlphaColors.White;
  FMsg.TextSettings.HorzAlign := TTextAlign.Center;
  FMsg.TextSettings.Font.Family := 'Calibri';
  FMsg.TextSettings.Font.Size := 13;
  FMsg.AutoSize := true;
  FMsg.Text := FMensagem;
  FMsg.RecalcSize;
  // if oMsg.Text.length > 55 then
  // oLayout.Height := oLayout.Height * round(oMsg.Text.length / 55);

  //FLayout.Height := FMsg.Height + 32;
//  FLayout.AnimateFloat('Position.Y', 0, 1.5, TAnimationType.InOut,
//    TInterpolationType.Linear);

  FFloatAnimatin:= TFloatAnimation.Create(FOwner);
  FFloatAnimatin.Parent := FLayout;
  FFloatAnimatin.Inverse:= false;
  FFloatAnimatin.StartValue := -FLayout.Height;
  FFloatAnimatin.StopValue := 0;
  FFloatAnimatin.Duration := 1;
  FFloatAnimatin.Loop := false;
  FFloatAnimatin.PropertyName := 'Position.Y';
  FFloatAnimatin.AnimationType := TAnimationType.Out;
  FFloatAnimatin.Interpolation := TInterpolationType.Back;
  FFloatAnimatin.OnFinish:= FloatAnimationFinish;
  FFloatAnimatin.Start;

  {$IF Defined(IOS)}
    FCorDefault:= TForm(FOwner).SystemStatusBar.BackgroundColor;
    TForm(FOwner).SystemStatusBar.BackgroundColor := FCor;
    TForm(FOwner).SystemStatusBar.Visibility := TFormSystemStatusBar.TVisibilityMode.Visible;
  {$ENDIF}

  {$IF Defined(ANDROID)}
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        FCorDefault:=  TAndroidHelper.Activity.getWindow.getStatusBarColor;
        TAndroidHelper.Activity.getWindow.setStatusBarColor(FCor);
      end);
  {$ENDIF}

end;

procedure TGosToast_v2.FloatAnimationFinish(Sender: TObject);
begin
  if FFloatAnimatin.Inverse then
  begin
    {$IF Defined(IOS)}
      TForm(FOwner).SystemStatusBar.BackgroundColor := FCorDefault;
      TForm(FOwner).SystemStatusBar.Visibility := TFormSystemStatusBar.TVisibilityMode.Visible;
    {$ENDIF}
    {$IF Defined(ANDROID)}
      CallInUIThreadAndWaitFinishing(
        procedure
        begin
          TAndroidHelper.Activity.getWindow.setStatusBarColor(FCorDefault);
        end);
    {$ENDIF}

    if Assigned(FLayout) then
      FreeAndNil(FLayout);
    exit;
  end;

  TThread.CreateAnonymousThread(
  procedure
  begin
    sleep(round(FDuration*1000));
    TThread.Synchronize(nil,
    procedure
    begin
      FFloatAnimatin.Inverse:= true;
      FFloatAnimatin.Start;
    end);
  end).Start;
end;


end.
