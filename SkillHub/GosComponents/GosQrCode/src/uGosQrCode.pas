unit uGosQrCode;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Objects,
  uQrCode, System.UITypes, System.Math, System.UIConsts, FMX.Graphics;

type
  [ComponentPlatforms($FFFF)]
  TGosQrCode = class(TImage)
  private
    FMenssage: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent);override;
    procedure Show;

  published
    property Menssage:string read FMenssage write FMenssage;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosQrCode]);
end;

{ TGosQrCode }

constructor TGosQrCode.Create(AOwner: TComponent);
begin
  inherited;

  self.Height:= 100;
  self.Width:= 100;
  self.Bitmap:= nil;
end;

procedure TGosQrCode.Show;
const
  downsizeQuality: Integer = 2;
  // bigger value, better quality, slower rendering
var
  QRCode: TDelphiZXingQRCode;
  Row, Column: Integer;
  pixelColor: TAlphaColor;
  vBitMapData: TBitmapData;
  pixelCount, y, x: Integer;
  columnPixel, rowPixel: Integer;
  function GetPixelCount(AWidth, AHeight: Single): Integer;
  begin
    if QRCode.Rows > 0 then
      result := Trunc(Min(AWidth, AHeight)) div QRCode.Rows
    else
      result := 0;
  end;

begin

  QRCode := TDelphiZXingQRCode.Create;
  try
    QRCode.Data := FMenssage; // 2 caracteres em branco para android e ios.
    QRCode.Encoding := TQRCodeEncoding(qrAuto);
    QRCode.QuietZone := 4;
    QRCode.ErrorLevels := ecH;
    pixelCount := GetPixelCount(self.Width, self.Height);
    case self.WrapMode of
      TImageWrapMode.Original, TImageWrapMode.Tile, TImageWrapMode.Center:
        begin
          if pixelCount > 0 then
            self.Bitmap.SetSize(QRCode.Columns * pixelCount, QRCode.Rows * pixelCount);
        end;
      TImageWrapMode.Fit:
        begin
          if pixelCount > 0 then
          begin
            self.Bitmap.SetSize(QRCode.Columns * pixelCount * downsizeQuality,
              QRCode.Rows * pixelCount * downsizeQuality);
            pixelCount := pixelCount * downsizeQuality;
          end;
        end;
      TImageWrapMode.Stretch:
        raise Exception.Create('Not a good idea to stretch the QR Code');
    end;
    if self.Bitmap.Canvas.BeginScene then
    begin
      try
{$IF not Defined(MSWINDOWS)}
        self.Bitmap.Canvas.Clear(TAlphaColors.White);
{$ENDIF}
        if pixelCount > 0 then
        begin
          if self.Bitmap.Map(TMapAccess.Write, vBitMapData) then
          begin
            try
              for Row := 0 to QRCode.Rows - 1 do
              begin
                for Column := 0 to QRCode.Columns - 1 do
                begin
                  if (QRCode.IsBlack[Row, Column]) then
                    pixelColor := TAlphaColors.Black
                    // StringToAlphaColor(global.CorPadraoApp)
                  else
                    pixelColor := TAlphaColors.White;

                  columnPixel := Column * pixelCount;
                  rowPixel := Row * pixelCount;
                  for x := 0 to pixelCount - 1 do
                    for y := 0 to pixelCount - 1 do
                      vBitMapData.SetPixel(columnPixel + x, rowPixel + y, pixelColor);
                end;
              end;
            finally
              self.Bitmap.Unmap(vBitMapData);
            end;
          end;
        end;
      finally
        self.Bitmap.Canvas.EndScene;
      end;
    end;
  finally
    QRCode.Free;
  end;
end;
end.
