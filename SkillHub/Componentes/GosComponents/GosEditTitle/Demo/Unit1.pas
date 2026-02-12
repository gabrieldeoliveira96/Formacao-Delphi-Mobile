unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  uGosObjects, uGosEditTitle, FMX.Controls.Presentation, FMX.Edit,
  FMX.TabControl, FMX.StdCtrls, FMX.Layouts, System.Actions, FMX.ActnList;

type
  TForm1 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Rectangle1: TRectangle;
    SpeedButton1: TSpeedButton;
    Layout1: TLayout;
    Label1: TLabel;
    Layout2: TLayout;
    Label2: TLabel;
    SpeedButton2: TSpeedButton;
    GosEditTitle7: TGosEditTitle;
    GosEditTitle1: TGosEditTitle;
    GosEditTitle2: TGosEditTitle;
    GosEditTitle3: TGosEditTitle;
    GosEditTitle4: TGosEditTitle;
    GosEditTitle5: TGosEditTitle;
    GosEditTitle6: TGosEditTitle;
    GosEditTitle8: TGosEditTitle;
    GosEditTitle9: TGosEditTitle;
    GosEditTitle10: TGosEditTitle;
    GosEditTitle11: TGosEditTitle;
    GosEditTitle12: TGosEditTitle;
    GosEditTitle13: TGosEditTitle;
    GosEditTitle14: TGosEditTitle;
    GosEditTitle15: TGosEditTitle;
    GosEditTitle16: TGosEditTitle;
    ActionList1: TActionList;
    ctamain: TChangeTabAction;
    ctaDark: TChangeTabAction;
    Layout3: TLayout;
    Layout4: TLayout;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  GosEditTitle1.Text:= '123';
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TabControl1.ActiveTab:= TabItem1;
  TabControl1.TabPosition:= TTabPosition.None;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  ctaDark.Execute;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  ctamain.Execute;
end;

end.
