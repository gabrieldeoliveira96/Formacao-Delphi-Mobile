unit uGosDrawerComponents;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Objects,
  FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  System.Math.Vectors, FMX.Ani, FMX.Layouts, System.UITypes;

type
  [ComponentPlatforms($FFFF)]
  TGosDrawerComponents = class(TRectangle)
  private
    FRecBack: TRectangle;
    FRecFront: TRectangle;
    FAniBack:TColorAnimation;
    FAni:TFloatAnimation;
    FHeigth:single;
    FCorBackGround: TAlphaColor;
    FDuration: single;
    FPosX:single;
    FPosY:single;
    FWidth:Single;
    FAlign:TAlignLayout;
    FEnableClickHide: boolean;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FHeightContainer: single;
    procedure ClickBackground(Sender: TObject);
    procedure ColorAnimationFinish(Sender: TObject);
    procedure SetHeightContainer(const Value: single);
    function GetHeightContainer: single;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Show;
    procedure Hide;
  published
    property Duration:single read FDuration write FDuration;
    property EnableClickHide: boolean read FEnableClickHide write FEnableClickHide;
    property OnClose:TNotifyEvent read FOnClose write FOnClose;
    property OnShow:TNotifyEvent read FOnShow write FOnShow;
    property HeightContainer: single read GetHeightContainer write SetHeightContainer;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosDrawerComponents]);
end;

{ TGosDrawerComponents }

constructor TGosDrawerComponents.Create(AOwner: TComponent);
begin
  inherited;
  self.Height:= 200;
  self.Width:= 200;
  self.Stroke.Kind:= TBrushKind.None;
  self.Fill.Color:= TAlphaColors.White;

  FDuration:= 0.7;
  FCorBackGround:= TAlphaColors.White;
  FEnableClickHide:= true;
  HeightContainer:= 200;

end;

function TGosDrawerComponents.GetHeightContainer: single;
begin
  if Assigned(FRecFront) then
    Result:= FRecFront.Height;
end;

procedure TGosDrawerComponents.ClickBackground(Sender: TObject);
begin
  if FEnableClickHide then
    Hide;
end;

procedure TGosDrawerComponents.ColorAnimationFinish(Sender: TObject);
begin

  if FAniBack.Inverse then
  begin
    self.Position.X:= FPosX;
    self.Position.Y:= FPosY;
    self.Width:= FWidth;
    self.Align:= FAlign;
    self.Height:= FHeigth;
  end;
end;

procedure TGosDrawerComponents.Hide;
begin

  self.Height:= FHeigth;
  FAni.Inverse:= true;
  FAni.Start;

  FAniBack.Inverse:= true;
  FAniBack.Start;
  if Assigned(FOnClose) then
    FOnClose(self);

end;

procedure TGosDrawerComponents.SetHeightContainer(const Value: single);
begin
  if Assigned(FRecFront) then
    FRecFront.Height:= Value;
end;

procedure TGosDrawerComponents.Show;
begin
  FPosX:= self.Position.X;
  FPosY:= self.Position.Y;
  FWidth:= self.Width;
  FAlign:= self.Align;

  if not self.Visible then
    self.Visible:= true;

  if self.Height > 0 then
    FHeigth:= self.Height;

  self.Align:= TAlignLayout.Contents;
  self.Fill.Kind:= TBrushKind.None;

  // Cria rect de fundo transparente...
  if not Assigned(FRecBack) then
    FRecBack := TRectangle.Create(self);
  with FRecBack do
  begin
    Align:= TAlignLayout.Contents;
    Fill.Kind := TBrushKind.Solid;
    Stroke.Kind:= TBrushKind.None;
    Visible := true;
    HitTest := true;
    Locked:= true;
    Fill.Color:= $64000000;
    OnClick:= ClickBackground;
  end;
  self.AddObject(FRecBack);

  //Rectangle container
  if not Assigned(FRecFront) then
    FRecFront := TRectangle.Create(self);
  with FRecFront do
  begin
    Align:= TAlignLayout.Bottom;
    Fill.Kind := TBrushKind.Solid;
    fill.Color:= TAlphaColors.White;
    Stroke.Kind:= TBrushKind.None;
    Visible := true;
    Locked:= true;
    HitTest := false;
    Corners:= [TCorner.TopLeft, TCorner.TopRight];
    Height:= FHeigth;
    BringToFront;
    XRadius:= 10;
    YRadius:= 10;
  end;
  FRecBack.AddObject(FRecFront);

  //mudando o parent dos componentes
  for var i := Pred(self.ChildrenCount) downto 0 do
    if self.Children[i] <> FRecBack then
      self.Children[i].Parent:= FRecFront;

  // Cria animacao de fade do fundo...
  if not Assigned(FAniBack) then
    FAniBack := TColorAnimation.Create(FRecBack);
  FAniBack.Inverse:= false;
  FAniBack.PropertyName:= 'Fill.Color';
  FAniBack.StartValue := TAlphaColors.Null;
  FAniBack.StopValue := $64000000;
  FAniBack.Duration := 0.4;
  FAniBack.OnFinish:= ColorAnimationFinish;
  FRecBack.AddObject(FAniBack);

  // animação subida do container
  if not Assigned(FAni) then
    FAni := TFloatAnimation.Create(self);
  FAni.Inverse:= false;
  FAni.PropertyName := 'Position.Y';
  FAni.StartValue := self.Height;
  {$IFDEF MSWINDOWS}
  FAni.StopValue := self.Height - FHeigth ;
  {$ENDIF}
  {$IF Defined(ANDROID) OR Defined(IOS)}
  FAni.StopValue := self.Height - FHeigth + 37;
  {$ENDIF}

  FAni.AnimationType := TAnimationType.InOut;
  FAni.Duration := 0.7;
  FAni.Interpolation := TInterpolationType.Back;
  FRecFront.AddObject(FAni);

  FAniBack.Start;
  FAni.Start;
  if Assigned(FOnShow) then
    FOnShow(self);

end;

end.



