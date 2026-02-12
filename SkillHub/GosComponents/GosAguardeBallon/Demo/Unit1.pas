unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, uGosAguardeBallon;

type
  TForm1 = class(TForm)
    Button1: TButton;
    GosAguardeBallon1: TGosAguardeBallon;
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
  GosAguardeBallon1.Show;

  TThread.CreateAnonymousThread(
  procedure
  begin

    sleep(3000);

    {Coloque aqui o deve ser feito em 2° plano}

    TThread.Synchronize(nil,
    procedure
    begin
      GosAguardeBallon1.Stop;
    end);


  end).Start;
end;

end.
