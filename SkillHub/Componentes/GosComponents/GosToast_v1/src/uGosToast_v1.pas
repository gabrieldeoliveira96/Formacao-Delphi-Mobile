unit uGosToast_v1;

interface

uses
  System.SysUtils, System.Classes, FMX.Objects, FMX.StdCtrls, FMX.Layouts,
  FMX.Forms, FMX.Types, FMX.Graphics, System.UITypes, FMX.Ani;

type
  [ComponentPlatforms($FFFF)]
  TGosToast_v1 = class(TFmxObject)
  private
    FRec: TRectangle;
    FMsg: TLabel;
    FLayout: TLayout;
    FOpacity: single;
    FMenssage: string;
    FDuration: single;
    FFloatAnimatin: TFloatAnimation;
    FOpacityRectangle: single;
    FCorRectangle: TAlphaColor;
    FOwner:TComponent;
    procedure FloatAnimationFinish(Sender: TObject);
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Show;
  published
    property Menssage:string read FMenssage write FMenssage;
    property Duration:single read FDuration write FDuration;
    property OpacityRectangle:single read FOpacityRectangle write FOpacityRectangle;
    property CorRectangle: TAlphaColor read FCorRectangle write FCorRectangle;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosToast_v1]);
end;

{ TGosToast_v1 }

constructor TGosToast_v1.Create(AOwner: TComponent);
begin
  inherited;
  Duration:= 3;
  OpacityRectangle:= 0.5;
  CorRectangle:= TAlphaColors.Black;
  FOwner:= AOwner;
end;

procedure TGosToast_v1.Show;
begin
  FLayout := TLayout.Create(FOwner);
  FLayout.Parent := TForm(FOwner);
  FLayout.Width := TForm(FOwner).Width - 60;
  FLayout.Position.X := trunc(TForm(FOwner).Width / 2) - trunc(FLayout.Width / 2);
  FLayout.Position.Y := TForm(FOwner).Height - FLayout.Height - 100;

  FRec := TRectangle.Create(FOwner);
  FRec.Parent := FLayout;
  FRec.Align := TAlignLayout.Client;
  FRec.Stroke.Kind := TBrushKind.None;
  FRec.Opacity := OpacityRectangle;
  FRec.Fill.Color := CorRectangle;

  FMsg := TLabel.Create(FOwner);
  FMsg.Parent := FLayout;

  FLayout.Height := FMsg.Height + 10;

  FMsg.BringToFront;
  FMsg.Align := TAlignLayout.Top;
  FMsg.AutoSize:= true;
  FMsg.StyledSettings := [];
  FMsg.Margins.Left := 10;
  FMsg.Margins.Right := 10;
  FMsg.TextSettings.Font.Style := [TFontStyle.fsBold];
  FMsg.TextSettings.Trimming := TTextTrimming.None;
  FMsg.TextSettings.VertAlign := TTextAlign.Center;
  FMsg.TextSettings.FontColor := TAlphaColors.White;
  FMsg.TextSettings.HorzAlign := TTextAlign.Center;
  FMsg.TextSettings.Font.Family := 'Calibri';
  FMsg.TextSettings.Font.Size := 11;
  FMsg.RecalcSize;

  FLayout.Height := FMsg.Height + 10;

  FRec.XRadius := FRec.Height/2;
  FRec.YRadius := FRec.Height/2;

  FFloatAnimatin:= TFloatAnimation.Create(FOwner);
  FFloatAnimatin.Parent := FLayout;
  FFloatAnimatin.Inverse:= false;
  FFloatAnimatin.StartValue := 0;
  FFloatAnimatin.StopValue := 1;
  FFloatAnimatin.Duration := 0.2;
  FFloatAnimatin.Loop := false;
  FFloatAnimatin.PropertyName := 'Opacity';
  FFloatAnimatin.AnimationType := TAnimationType.InOut;
  FFloatAnimatin.Interpolation := TInterpolationType.Linear;
  FFloatAnimatin.OnFinish:= FloatAnimationFinish;


  FMsg.Text := FMenssage;
  FRec.Opacity := FOpacityRectangle;
  FFloatAnimatin.Inverse:= false;

  FMsg.Align := TAlignLayout.Top;
  FMsg.RecalcSize;
  FMsg.Align := TAlignLayout.None;

  FLayout.Height := FMsg.Height + 10;

  FRec.XRadius := FRec.Height/2;
  FRec.YRadius := FRec.Height/2;

  FMsg.Position.Y:= 5;
  FRec.Fill.Color := FCorRectangle;

  FLayout.Visible:= true;
  FFloatAnimatin.Start;

end;

procedure TGosToast_v1.FloatAnimationFinish(Sender: TObject);
begin
  if FFloatAnimatin.Inverse then
  begin
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
