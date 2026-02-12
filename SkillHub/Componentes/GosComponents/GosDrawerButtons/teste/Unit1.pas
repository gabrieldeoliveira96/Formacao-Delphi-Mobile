unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, uGosDrawerButtons;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
 LButton:TGosDrawerButtons;
    procedure Click(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Click(Sender: TObject);
begin
  ShowMessage('teste');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LButton:= TGosDrawerButtons.Create(self);
  with LButton.Items.Add do
  begin
    itemText:= 'whats';
    OnClick:= Click;
  end;
  self.AddObject(LButton);

end;

procedure TForm1.Button1Click(Sender: TObject);
begin


  LButton.Show;

end;

end.
