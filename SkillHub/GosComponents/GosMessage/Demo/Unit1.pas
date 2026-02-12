unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  uGosMessage, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    GosMessage1: TGosMessage;
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
  GosMessage1.Show(TIconDialog.Success,'Teste','Message','Sim',
  procedure
  begin
    ShowMessage('sim');
  end,
  'Não',
  procedure
  begin
    ShowMessage('Não');
  end);
end;

end.
