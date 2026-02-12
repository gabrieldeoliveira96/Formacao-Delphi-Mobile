unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, uGosBase, uGosStandard, FMX.Ani,
  FMX.Effects, FMX.Filter.Effects;

type
  TForm1 = class(TForm)
    GosButtonView1: TGosButtonView;
    FloatAnimation1: TFloatAnimation;
    AniIndicator1: TAniIndicator;
    Button1: TButton;
    ShadowEffect1: TShadowEffect;
    FillRGBEffect1: TFillRGBEffect;
    procedure FormCreate(Sender: TObject);
  private
    LButton: TGosButtonView;
    LAni: TAniIndicator;
    LShadow:TShadowEffect;
    LRbg:TFillRGBEffect;
    procedure Show(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin

  LButton := TGosButtonView.Create(self);
  LButton.Height := 22;
  LButton.Width := 80;
  LButton.Text := 'button teste';
  LButton.Background.XRadius := LButton.Height / 2;
  LButton.Background.YRadius := LButton.Height / 2;
  LButton.Background.ItemDefault.Color := $FF06B3CD;

  LButton.TextSettings.Color.Default:= TAlphaColors.White;

  LButton.OnClick := Show;

  self.AddObject(LButton);

  LAni := TAniIndicator.Create(self);
  LAni.Align := TAlignLayout.HorzCenter;
  LAni.Enabled := false;
  LAni.Visible := false;

  LRbg:= TFillRGBEffect.Create(self);
  LRbg.Color:= TAlphaColors.White;


  LAni.AddObject(LRbg);


  LButton.AddObject(LAni);


  LShadow:= TShadowEffect.Create(self);
  LShadow.Direction:= 45;
  LShadow.Distance:= 3;
  LShadow.Opacity:= 0.6;
  LShadow.ShadowColor:= TAlphaColors.Silver;
  LShadow.Softness:= 0.3;


  LButton.AddObject(LShadow);

end;

procedure TForm1.Show(Sender: TObject);
begin
  // show

  LButton.Text := '';
  LAni.Enabled := true;
  LAni.Visible := true;

end;

end.
