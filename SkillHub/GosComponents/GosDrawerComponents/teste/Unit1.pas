unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Ani;

type
  TForm1 = class(TForm)
    Button1: TButton;
    recBack: TRectangle;
    Layout1: TLayout;
    recFront: TRectangle;
    FloatAnimation1: TFloatAnimation;
    aniColor: TColorAnimation;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure recBackClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
 LHeigth:single;
begin
  LHeigth:= recFront.Height;

  Layout1.Align:= TAlignLayout.Contents;
  recBack.Align:= TAlignLayout.Contents;

  recFront.Align:= TAlignLayout.MostBottom;

  aniColor.Inverse:= false;
  aniColor.Start;

  FloatAnimation1.Inverse:= false;
  FloatAnimation1.StartValue:= 0;
  FloatAnimation1.StopValue:= LHeigth;

  FloatAnimation1.Start;

end;

procedure TForm1.recBackClick(Sender: TObject);
begin
  aniColor.Inverse:= true;
  aniColor.Start;

  FloatAnimation1.Inverse:= true;
  FloatAnimation1.Start;

end;

end.
