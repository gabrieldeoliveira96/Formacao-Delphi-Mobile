unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  uGosDrawerButtons, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    GosDrawerButtons1: TGosDrawerButtons;
    procedure Button1Click(Sender: TObject);
    procedure GosDrawerButtons1Items1Click(Sender: TObject);
    procedure GosDrawerButtons1Items0Click(Sender: TObject);
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
begin
  GosDrawerButtons1.Show;
end;


procedure TForm1.GosDrawerButtons1Items0Click(Sender: TObject);
begin
  ShowMessage('whats');

end;

procedure TForm1.GosDrawerButtons1Items1Click(Sender: TObject);
begin
  ShowMessage('telegram');

end;

end.
