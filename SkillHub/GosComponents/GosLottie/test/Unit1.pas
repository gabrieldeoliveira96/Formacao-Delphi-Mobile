unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Skia.FMX,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, System.IOUtils,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Rectangle1: TRectangle;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  FLottie: TSkLottieAnimation;
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
  if not Assigned(FLottie) then
  begin
    FLottie := TSkLottieAnimation.Create(Self);
    FLottie.Align := TAlignLayout.Client;
    FLottie.Parent := Rectangle1;
  end;

  FLottie.LoadFromFile(TPath.Combine(TPath.GetDocumentsPath, '404.json'));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
 LStream:TStringStream;
begin
  if not Assigned(FLottie) then
  begin
    FLottie := TSkLottieAnimation.Create(Self);
    FLottie.Align := TAlignLayout.Client;
    FLottie.Parent := Rectangle1;
  end;


  LStream:= TStringStream.Create(Memo1.Lines.Text);

  FLottie.LoadFromStream(LStream);

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF ANDROID}
  {TODO: fazer}
  {
    caso for android ou ios, o arquivo de complemento do skia deve ir junto,
    eu estou pensando em jogar os arquivos do skia ( para android e ios ) dentro da pasta de dcu dos componentes
    dessa forma quando o usuário fizer o create do componente, o próprio comopnente fará uma cópia do arquivo
    pra dentro da pasta do android.

  }

  {$ENDIF}
end;

end.
