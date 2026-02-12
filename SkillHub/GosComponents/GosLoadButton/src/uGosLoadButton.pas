unit uGosLoadButton;

interface

uses
  System.SysUtils, System.Classes, FMX.Controls,
  uGosStandard, FMX.StdCtrls, FMX.Effects, FMX.Filter.Effects,
  FMX.Types;

type

  [ComponentPlatforms($FFFF)]
  TGosLoadButton = class(TGosButtonView)
  private
    FText: string;
  protected
    FAni: TAniIndicator;
    [week]
    FShadow: TShadowEffect;
    [week]
    FRbg: TFillRGBEffect;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start;
    procedure Stop;
    procedure Realign; overload;
    procedure RecalcSize;overload;
  published
    property Text: string read FText write FText;
    property LoadColor: TFillRGBEffect read FRbg;
    property Shadow: TShadowEffect read FShadow;
  end;

//procedure Register;

implementation

uses
  System.UITypes;

//procedure Register;
//begin
//  RegisterComponents('GosComponent', [TGosLoadButton]);
//end;

{ TGosLoadButton }

constructor TGosLoadButton.Create(AOwner: TComponent);
begin
  inherited;
  //FText := 'GosLoadButton';

  self.Height := 22;
  self.Width := 80;
  //self.Text := Text;
  self.Background.XRadius := self.Height / 2;
  self.Background.YRadius := self.Height / 2;
  self.Background.ItemDefault.Color := $FF06B3CD;

  self.TextSettings.Color.Default := TAlphaColors.White;

  // LButton.OnClick := Show;
  // self.AddObject(LButton);

  FAni := TAniIndicator.Create(self);
  FAni.Align := TAlignLayout.HorzCenter;
  FAni.Enabled := false;
  FAni.Visible := false;

  FRbg := TFillRGBEffect.Create(self);
  FRbg.Color := TAlphaColors.White;

  FAni.AddObject(FRbg);

  self.AddObject(FAni);

  FShadow := TShadowEffect.Create(self);
  FShadow.Direction := 45;
  FShadow.Distance := 3;
  FShadow.Opacity := 0.6;
  FShadow.ShadowColor := TAlphaColors.Silver;
  FShadow.Softness := 0.3;

  self.AddObject(FShadow);

end;

procedure TGosLoadButton.Realign;
begin
  self.Background.XRadius := self.Height / 2;
  self.Background.YRadius := self.Height / 2;
end;

procedure TGosLoadButton.RecalcSize;
begin
  self.Background.XRadius := self.Height / 2;
  self.Background.YRadius := self.Height / 2;

end;

procedure TGosLoadButton.Start;
begin
  FText := self.Text;
  self.Text := '';
  FAni.Enabled := true;
  FAni.Visible := true;
end;

procedure TGosLoadButton.Stop;
begin
  FAni.Enabled := false;
  FAni.Visible := false;
  self.Text := FText;
end;

end.
