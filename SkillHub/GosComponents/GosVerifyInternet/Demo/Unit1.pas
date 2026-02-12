unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  uGosVerifyInternet, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    GosVerifyInternet1: TGosVerifyInternet;
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

{necessário ativa a permission de Acess Network State}
procedure TForm1.Button1Click(Sender: TObject);
begin
  if GosVerifyInternet1.Connect then
    ShowMessage('Conectado')
  else
    ShowMessage('Não Conectado');
end;

end.
