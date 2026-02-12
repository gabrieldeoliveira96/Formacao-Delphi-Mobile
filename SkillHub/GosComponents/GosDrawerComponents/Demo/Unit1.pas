unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, uGosDrawerComponents;

type
  TForm1 = class(TForm)
    Button1: TButton;
    GosDrawerComponents1: TGosDrawerComponents;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  GosDrawerComponents1.Show;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  GosDrawerComponents1.Hide;

end;

end.
