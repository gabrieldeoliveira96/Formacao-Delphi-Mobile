unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Layouts, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects,
  uGosCombobox, uGosObjects, uGosEditTitle, FMX.Edit, fmx.Consts, IdHTTP;

type
  TForm2 = class(TForm)
    SpeedButton1: TSpeedButton;
    Rectangle1: TRectangle;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Rectangle2: TRectangle;
    ComboBox1: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    Rectangle3: TRectangle;
    Label6: TLabel;
    Button1: TButton;
    Button2: TButton;
    GosCombobox1: TGosCombobox;
    Button3: TButton;
    GosCombobox2: TGosCombobox;
    Edit1: TEdit;
    Edit2: TEdit;
    Lang1: TLang;
    Button4: TButton;
    procedure LabelEnter(Sender: TObject);
    procedure Rectangle3Click(Sender: TObject);
    procedure GosCombobox1ItemClick(Sender: TObject);
    procedure GosCombobox1ItemTap(Sender: TObject; const Point: TPointF);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    function googleTranslate(source, langpair: string;
      var resultString: string): string;
    function URLEncode(const S: RawByteString): RawByteString;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button2Click(Sender: TObject);
begin
  ShowMessage('text '+GosCombobox1.Selected);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  ShowMessage('order item '+GosCombobox1.ItemIndex.ToString);
end;

function TForm2.URLEncode(const S: RawByteString): RawByteString;
  const
    NoConversion = ['A'..'Z', 'a'..'z', '*', '@', '.', '_', '-', '/', ':', '=', '?'];
  var
    i, idx, len: Integer;

  function DigitToHex(Digit: Integer): AnsiChar;
  begin
    case Digit of
      0..9: Result := AnsiChar(Chr(Digit + Ord('0')));
      10..15: Result := AnsiChar(Chr(Digit - 10 + Ord('A')));
    else
      Result := '0';
    end;
  end; // DigitToHex

begin
  len := 0;
  for i := 1 to Length(S) do
    if S[i] in NoConversion then
      len := len + 1
    else
      len := len + 3;
  SetLength(Result, len);
  idx := 1;
  for i := 1 to Length(S) do
    if S[i] in NoConversion then
    begin
      Result[idx] := S[i];
      idx := idx + 1;
    end
    else
    begin
      Result[idx] := '%';
      Result[idx + 1] := DigitToHex(Ord(S[i]) div 16);
      Result[idx + 2] := DigitToHex(Ord(S[i]) mod 16);
      idx := idx + 3;
    end;
end; // URLEncode

// source - the string to be translated
// langpair - the string that defines the source and target language in special format,
//     i.e. "en|ru". The list of available languages and their abbreviations
//     you may find in Translation API description
// resultString - the translation
// result - the error message if any. Empty result means that
//     the function has been executed successfully
function TForm2.googleTranslate(source : string; langpair : string;
	var resultString : string) : string;
var
  url, s, status : String;
  utfs : UTF8String;
  http : TidHttp;

begin
  result := '';

  http := TidHttp.Create;

  try
    utfs := UTF8String(source);
    utfs := URLEncode(utfs);
    url := 'http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=' +
		String(utfs) + '&langpair=' + langpair;

    http.Request.Referer := 'http://oursite.com';
    http.Request.UserAgent := 'Our Application';
    s := http.Get(url);

    status := Copy(s, pos('"responseStatus":', s)+18, length(s));
    status := Copy(status, 0, pos('}', status)-1);

    if (status = '200') then begin //status is OK
      s := Copy(s, pos('"translatedText":', s)+18, length(s));
      resultString := Copy(s, 0, pos('"}, "responseDetails"', s)-1);
    end
    else begin //an error occurred
      s := Copy(s, pos('"responseDetails":', s)+20, length(s));
      resultString := '';
      result := Copy(s, 0, pos('", "responseStatus"', s)-1);
    end;

  finally
    http.Free;
  end;
end;

procedure TForm2.Button4Click(Sender: TObject);
var
 res, strValue : string;
begin

  googleTranslate('Hello world!', 'en|uk', strValue);
  ShowMessage(strValue);
end;

procedure TForm2.GosCombobox1ItemClick(Sender: TObject);
begin
  ShowMessage('GosCombobox1ItemClick');
  ShowMessage('text '+GosCombobox1.Selected);
  ShowMessage('order item '+GosCombobox1.ItemIndex.ToString);
end;

procedure TForm2.GosCombobox1ItemTap(Sender: TObject; const Point: TPointF);
begin
  ShowMessage('GosCombobox1ItemTap');

end;

procedure TForm2.LabelEnter(Sender: TObject);
var
 LLabel:TLabel;
begin

  LLabel:= TLabel(Sender);
  Rectangle2.Position.Y:= LLabel.Position.Y;

end;

procedure TForm2.Rectangle3Click(Sender: TObject);
begin
  Rectangle1.Visible:= not Rectangle1.Visible;

end;

end.
