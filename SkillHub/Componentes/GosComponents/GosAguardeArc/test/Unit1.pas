unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, System.Math,
  uGosDrawerComponents, uFancyDialog;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Arc1: TArc;
    Label1: TLabel;
    Label2: TLabel;
    GosDrawerComponents1: TGosDrawerComponents;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Fmsg:TFancyDialog;
    const Tot = 440;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
 LThread:TThread;
begin
  GosDrawerComponents1.Show;


  TThread.CreateAnonymousThread(
  procedure
    var r:double;
  begin

    for var i := 1 to Tot do
    begin
      r:= i*360;
      r:= r/100;
      r:= r * 100;
      r:= r / Tot;

      TThread.Synchronize(nil,
      procedure
      begin

        Arc1.EndAngle:= r;
        label2.Text:= i.ToString;

        r:= i *100;
        r:= r / tot;

        Label1.Text:= RoundTo(r, -2).ToString+ ' %';
      end);

      sleep(50);

    end;

    TThread.Synchronize(nil,
    procedure
    begin
      GosDrawerComponents1.Hide;

      Fmsg.Show(TIconDialog.Success,'finalizou','');

    end);

  end).Start;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Fmsg:= TFancyDialog.Create(self);
end;

end.
