unit uGosComponents;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  uGosObjects, uGosEdit, uGosStandard, uGosAguardeBallon,
  uGosAguardeText, uGosDrawerButtons, uGosDrawerComponents,
  uGosEditTitle, uGosMessage, uGosQrCode, uGosToast_v1, uGosToast_v2,
  uGosLoadButton;

type
  [ComponentPlatforms($FFFF)]
  TGosComponents = class
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GosComponent',
  [TGosImage,
  TGosRectangle,
  TGosCircle,
  TGosLine,
  TGosText,
  TGosEditView,
  TGosTextView,
  TGosButtonView,
  TGosAguardeBallon,
  TGosAguardeText,
  TGosDrawerButtons,
  TGosDrawerComponents,
  TGosEditTitle,
  TGosMessage,
  TGosQrCode,
  TGosToast_v1,
  TGosToast_v2,
  TGosLoadButton]);
end;

end.
