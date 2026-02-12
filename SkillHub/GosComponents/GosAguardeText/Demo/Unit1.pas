unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, uGosAguardeText,
  FMX.Ani, FMX.Objects;

type
  TForm1 = class(TForm)
    Button1: TButton;
    GosAguardeText1: TGosAguardeText;
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
  GosAguardeText1.Mensagem:= 'teste';
  GosAguardeText1.Show;

  TThread.CreateAnonymousThread(
  procedure
  begin
    sleep(3000);

    TThread.Synchronize(nil,
    procedure
    begin
      GosAguardeText1.ChangeText('123');
    end);

    sleep(3000);

    TThread.Synchronize(nil,
    procedure
    begin

      GosAguardeText1.Stop;
    end);

  end).Start;

end;

end.
