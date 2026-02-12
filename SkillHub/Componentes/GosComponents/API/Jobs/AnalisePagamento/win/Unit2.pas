unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  model.pagamento, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Timer1: TTimer;
    Label1: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FDmPagamento: TdmPagamento;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  FDmPagamento := TdmPagamento.Create(self);
  FDmPagamento.ConfiguraComponente;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDmPagamento);
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  FDmPagamento.AnalisePagamento;
end;

end.
