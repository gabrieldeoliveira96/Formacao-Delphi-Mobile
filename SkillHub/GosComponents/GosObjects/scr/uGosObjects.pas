unit uGosObjects;


{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if FMX.Objects.pas was not updated and adjust the IFDEF'}
{$ENDIF}

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE _MACOS}
{$IFEND}

interface

uses System.Classes,
     System.Types,
     System.UITypes, // [DCC Hint] ALFmxObjects.pas(1418): H2443 Inline function 'TAlphaColorCGFloat.Create' has not been expanded because unit 'System.UITypes' is not specified in USES list
     System.Rtti,
     {$IFDEF DEBUG}
     System.Diagnostics,
     {$ENDIF}
     {$IF defined(ANDROID)}
     system.Messaging,
     FMX.TextLayout.GPU,
     FMX.types3D,
     {$ENDIF}
     {$IF defined(IOS)}
     system.Messaging,
     FMX.TextLayout.GPU,
     FMX.types3D,
     {$ENDIF}
     {$IF DEFINED(MSWindows) or DEFINED(_MACOS)}
     FMX.effects,
     {$ENDIF}
     FMX.controls,
     FMX.types,
     FMX.textlayout,
     FMX.graphics,
     FMX.objects,
     uGosGraphics,
     uGosFmxCommon,
     uGosEdit;

type

  {~~~~~~~~~~~~~~~~~~}
  //TALImageWrapMode
  TGosImageWrapMode = (
      //Display the image with its original dimensions:
      //* The image is placed in the upper-left corner of the rectangle of the control.
      //* If the image is larger than the control's rectangle, then only the upper-left part of the image,
      //  which fits in the rectangle of the control, is shown. The image is not resized.
      Original,

      //Best fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image
      //  (keeping image proportions – the ratio between the width and height) to fit the whole image in the rectangle
      //  of the control. That is, either the width of the resized image is equal to the width of the control's rectangle
      //  or the height of the resized image is equal to the height of the rectangle of the control. The whole image
      //  should be displayed. The image is displayed centered in the rectangle of the control.
      // * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
      //  the rectangle of the control. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
      Fit,

      //Stretch the image to fill the entire rectangle of the control.
      Stretch,

      //Tile (multiply) the image to cover the entire rectangle of the control:
      //* If the image is larger than the rectangle of the control, then only the
      //  upper-left part of the image, which fits in the rectangle of the control, is shown. The image is not resized.
      //* If the image (original size) is smaller than the rectangle of the control, then the multiple images are tiled
      //  (placed one next to another) to fill the entire rectangle of the control. The images are placed beginning from
      //  the upper-left corner of the rectangle of the control.
      Tile,

      //Center the image to the rectangle of the control:
      //* The image is always displayed at its original size (regardless whether the rectangle of the control is larger or smaller than the image size).
      Center,

      //Fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image (keeping image proportions--the ratio between the width and height)
      //  to fit the whole image in the rectangle of the control. That is, either the width of the resized image is equal to the width of the control's rectangle or the height of the
      //  resized image is equal to the height of the control's rectangle. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
      //* If the original image is smaller than the rectangle of the control, then the image is not resized. The image is displayed centered in the rectangle of the control.
      Place,

      //Best fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image
      //  (keeping image proportions – the ratio between the width and height) to fit the height or the width of the image in the rectangle
      //  of the control and crop the extra part of the image. That is, the width of the resized image is equal to the width of the control's rectangle
      //  AND the height of the resized image is equal to the height of the rectangle of the control.
      // * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
      //  the rectangle of the control. Whole the image should be displayed.
      FitAndCrop
  );

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //Under delphi, we have multi-res bitmap for Timage or Tglyph. this mean that we can gave several bitmap for several screen scale.
  //Exemple with a screen scale of 1 i will gave a bitmap of 100x100, for the screen scale of 1.5 i will gave a bitmap of 150*150, etc..
  //so taking care that most screen scale are 1, 1.5, 2, 3, and 4 we must provide 4 pictures. In 99.9999 % of the case the developer will
  //simply do a normal resize of the image (under photoshop or similar app) in the 5 of fewer screen scale (seriously is their any developer
  //who will gave radically different image between scale 1 and scale n ?)
  //But the resize algo to resize picture is quite powerful and often negligible. So if we gave only one bitmap (at the most biggest scale, 4)
  //it's must be good/powerfull and it's will reduce also the size of the app.
  //also from smartphone to tablet i notice that to keep a good ratio i must increase all the font size, and image by 15%. So using multires
  //bitmap and if i want to avoid any resize (the purpose of multires bitmap as i understand) i must have 10 bitmaps per image !!
  //so all of this to say that multi-res bitmap is a fundamentally wrong concept
  [ComponentPlatforms($FFFF)]
  TGosImage = class(TControl)
  private
    fExifOrientationInfo: TGosExifOrientationInfo;
    fRotateAccordingToExifOrientation: Boolean;
    fFileName: String;
    fResourceName: String;
    FWrapMode: TGosImageWrapMode;
    FScreenScale: single;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure SetWrapMode(const Value: TGosImageWrapMode);
    procedure setFileName(const Value: String);
    procedure setResourceName(const Value: String);
  protected
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property Hint;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property RotateAccordingToExifOrientation: Boolean read fRotateAccordingToExifOrientation write fRotateAccordingToExifOrientation default false; // << under Android, work only when using filename
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    property FileName: String read fFileName write setFileName;
    property ResourceName: String read fResourceName write setResourceName;
    property WrapMode: TGosImageWrapMode read FWrapMode write SetWrapMode default TGosImageWrapMode.Fit;
    property ParentShowHint;
    property ShowHint;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Mouse events}
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
    property OnResized;
    {$ENDIF}
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TGosRectangle = class(TRectangle)
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    fShadow: TGosShadow;
    {$IF DEFINED(MSWindows) or DEFINED(_MACOS)}
    fShadowEffect: TshadowEffect;
    {$ENDIF}
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure SetdoubleBuffered(const Value: Boolean);
    procedure SetShadow(const Value: TGosShadow);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
    property shadow: TGosShadow read fshadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TGosCircle = class(TCircle)
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    fShadow: TGosShadow;
    {$IF DEFINED(MSWindows) or DEFINED(_MACOS)}
    fShadowEffect: TshadowEffect;
    {$ENDIF}
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure SetdoubleBuffered(const Value: Boolean);
    procedure SetShadow(const Value: TGosShadow);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
    property shadow: TGosShadow read fshadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TGosLine = class(TLine)
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
  end;

  {~~~~~~~~~~~~~~}
  TGosText = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TGosDoubleBufferedTextLayout = class(TTextLayout)
  private
    FScreenScale: single;
    [weak] fTextControl: TGosText;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    //-----
    fBufHorizontalAlign: TTextAlign;
    fBufVerticalAlign: TTextAlign;
    fBuffontColor: TAlphaColor;
    fBuffontFamily: TFontName;
    fBuffontStyle: TFontStyles;
    fBuffontSize: Single;
    fBufWordWrap: Boolean;
    fBufAutosize: Boolean;
    fBufTrimming: TTextTrimming;
    fBufSize: TsizeF;
    fBufText: string;
    fBufTextBreaked: Boolean;
    fBufAllTextDrawed: Boolean;
    //-----
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
  protected
    procedure DoRenderLayout; override;
    procedure DoDrawLayout(const ACanvas: TCanvas); override;
    function GetTextHeight: Single; override;
    function GetTextWidth: Single; override;
    function GetTextRect: TRectF; override;
    function DoPositionAtPoint(const APoint: TPointF): Integer; override;
    function DoRegionForRange(const ARange: TTextRange): TRegion; override;
  public
    constructor Create(const ACanvas: TCanvas; const aTextControl: TGosText); reintroduce;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
    procedure ConvertToPath(const APath: TPathData); override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // Note: we can use this class in for exemple Tlabel
  //       by overriding the Tlabel default Style but i
  //       do not recomend to use Tlabel, because it's simply
  //       a painless class that is in the top of the TText !
  //       this class use also it's own TTextLayoutNG to calculate
  //       the size, making everythink duplicate for .. nothing in fact !
  //       but i made some test and it's work with tlabel (but check carefully
  //       that you define well the properties of the TGosText in the
  //       style to not have MakeBufBitmap called several times (applystyle
  //       don't call beginupdate/endupdate (crazy!!), so everytime a property of the
  //       TGosText is updated, MakeBufBitmap is call again)
  [ComponentPlatforms($FFFF)]
  TGosText = class(TControl)
  private
    fRestoreLayoutUpdateAfterLoaded: boolean;
    FAutoTranslate: Boolean;
    FAutoConvertFontFamily: boolean;
    FTextSettings: TTextSettings;
    FLayout: TTextLayout;
    FAutoSize: Boolean;
    fMaxWidth: Single;
    fMaxHeight: Single;
    FYRadius: Single;
    FXRadius: Single;
    FCorners: TCorners;
    FSides: TSides;
    FFill: TBrush;
    FStroke: TStrokeBrush;
    fLineSpacing: single;
    fTextIsHtml: boolean;
    fMustCallResized: Boolean;
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TStrokeBrush);
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function GetBufBitmap: TTexture;
    {$ELSE}
    function GetBufBitmap: Tbitmap;
    {$ENDIF}
    function GetdoubleBuffered: Boolean;
    procedure SetdoubleBuffered(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetHorzTextAlign(const Value: TTextAlign);
    procedure SetVertTextAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure OnFontChanged(Sender: TObject);
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    function GetColor: TAlphaColor;
    function GetFont: TFont;
    function GetHorzTextAlign: TTextAlign;
    function GetTrimming: TTextTrimming;
    function GetVertTextAlign: TTextAlign;
    function GetWordWrap: Boolean;
    function GetText: string;
    procedure SetMaxWidth(const Value: Single);
    procedure SetMaxHeight(const Value: Single);
    function IsMaxWidthStored: Boolean;
    function IsMaxHeightStored: Boolean;
  protected
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read GetBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read GetBufBitmap;
    {$ENDIF}
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure SetParent(const Value: TFmxObject); override;
    procedure FontChanged; virtual;
    function SupportsPaintStage(const Stage: TPaintStage): Boolean; override;
    function GetTextSettingsClass: TTextSettingsClass; virtual;
    procedure Paint; override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure DoRealign; override;
    procedure AdjustSize;
    procedure Resize; override;
    {$IF CompilerVersion >= 32} // tokyo
    procedure DoResized; override;
    {$ENDIF}
    procedure Loaded; override;
    property Layout: TTextLayout read FLayout;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetNewScene(AScene: IScene); override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
    procedure BeginUpdate; override; // this is neccessary because the MakeBufBitmap is not only call during the paint,
    procedure EndUpdate; override;   // but also when any property changed because need to retrieve the dimension
    function TextBreaked: Boolean;
    property Font: TFont read GetFont write SetFont;
    property Color: TAlphaColor read GetColor write SetColor;
    property HorzTextAlign: TTextAlign read GetHorzTextAlign write SetHorzTextAlign;
    property Trimming: TTextTrimming read GetTrimming write SetTrimming;
    property VertTextAlign: TTextAlign read GetVertTextAlign write SetVertTextAlign;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
  published
    property Align;
    property Anchors;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property Text: string read GetText write SetText;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property Visible default True;
    property Width;
    property MaxWidth: single read fMaxWidth write SetMaxWidth stored IsMaxWidthStored;       // these properties are usefull when used
    property MaxHeight: single read fMaxHeight write SetMaxHeight stored IsMaxHeightStored;      // with autosize
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Mouse events}
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
    property OnResized;
    {$ENDIF}
    property doubleBuffered: Boolean read GetdoubleBuffered write setdoubleBuffered default true;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true;
    property AutoConvertFontFamily: Boolean read FAutoConvertFontFamily write fAutoConvertFontFamily default true;
    property Fill: TBrush read FFill write SetFill;
    property Stroke: TStrokeBrush read FStroke write SetStroke;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property LineSpacing: single read fLineSpacing write fLineSpacing;
    property TextIsHtml: boolean read fTextIsHtml write fTextIsHtml default false;
    property TouchTargetExpansion;
  end;

procedure ALLockTexts(const aParentControl: Tcontrol);
procedure ALUnLockTexts(const aParentControl: Tcontrol);

{$IFDEF debug}
var
  AlDebugImageMakeBufBitmapCount: integer;
  AlDebugRectangleMakeBufBitmapCount: integer;
  AlDebugCircleMakeBufBitmapCount: integer;
  AlDebugLineMakeBufBitmapCount: integer;
  AlDebugTextMakeBufBitmapCount: integer;
  AlDebugTextInheritedDoRenderLayoutCount: integer;
  AlDebugTextInheritedDoDrawLayoutCount: integer;

  AlDebugImageMakeBufBitmapStopWatch: TstopWatch;
  AlDebugRectangleMakeBufBitmapStopWatch: TstopWatch;
  AlDebugCircleMakeBufBitmapStopWatch: TstopWatch;
  AlDebugLineMakeBufBitmapStopWatch: TstopWatch;
  AlDebugTextMakeBufBitmapStopWatch: TstopWatch;
{$endif}

procedure Register;

implementation

uses system.SysUtils,
     system.Math,
     system.Math.Vectors,
     fmx.consts,
     fmx.platform,
     {$IFDEF ALDPK}
     system.ioutils,
     ToolsAPI,
     {$ENDIF}
     {$IF defined(ANDROID)}
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNIBridge,
     Androidapi.Bitmap,
     FMX.Canvas.GPU,
     {$ENDIF}
     {$IF defined(IOS)}
     iOSapi.CocoaTypes,
     iOSapi.CoreGraphics,
     iOSapi.UIKit,
     FMX.Canvas.GPU,
     FMX.Surfaces,
     uGosFmxTypes3D,
     {$ENDIF}
     uGosCommon;

{**********************************************}
constructor TGosImage.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  inherited Create(AOwner);
  fExifOrientationInfo := TGosExifOrientationInfo.UNDEFINED;
  fRotateAccordingToExifOrientation := False;
  fFileName := '';
  fResourceName := '';
  FWrapMode := TGosImageWrapMode.Fit;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fBufBitmap := nil;
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
  SetAcceptsControls(False);
end;

{**************************}
destructor TGosImage.Destroy;
begin
  clearBufBitmap;
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
  {$ENDIF}
  inherited;
end;

{********************************}
procedure TGosImage.clearBufBitmap;
begin
  GosFreeAndNil(fBufBitmap);
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TGosImage.MakeBufBitmap: TTexture;
{$ELSE}
function TGosImage.MakeBufBitmap: Tbitmap;
{$ENDIF}

{$IFDEF ALDPK}
var aFileName: String;
{$ENDIF}

begin

  if (Scene = nil) or
     //--- don't do bufbitmap is size=0
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     //--- don't do bufbitmap if fFileName or fResourceName is empty
     ((fFileName = '') and (fResourceName = ''))
  then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;

  {$IFDEF debug}
  GosLog('TGosImage.MakeBufBitmap', 'Name: ' + Name, TGosLogType.verbose);
  inc(AlDebugImageMakeBufBitmapCount);
  AlDebugImageMakeBufBitmapStopWatch.Start;
  try
  {$endif}

    {$IFDEF ALDPK}
    if fresourceName = '' then aFileName := fFileName
    else begin
      aFileName := extractFilePath(getActiveProject.fileName) + 'resources\' + fResourceName; // by default all the resources files must be located in the sub-folder /resources/ of the project
      if not TFile.Exists(aFileName) then aFileName := aFileName + '.png';
    end;
    if not TFile.Exists(aFileName) then aFileName := '';
    {$ENDIF}

    if (fRotateAccordingToExifOrientation) and
       (fResourceName = '') and
       (fFileName <> '') then fExifOrientationInfo := GosGetExifOrientationInfo(ffilename)
    else fExifOrientationInfo := TGosExifOrientationInfo.UNDEFINED;

    if fExifOrientationInfo in [TGosExifOrientationInfo.TRANSPOSE,
                                TGosExifOrientationInfo.ROTATE_90,
                                TGosExifOrientationInfo.TRANSVERSE,
                                TGosExifOrientationInfo.ROTATE_270] then fBufBitmapRect := GosAlignDimensionToPixelRound(TRectF.Create(0, 0, Height, Width), FScreenScale) // to have the pixel aligned width and height
    else fBufBitmapRect := GosAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
                                                                                  // TalExifOrientationInfo.FLIP_HORIZONTAL:;
                                                                                  // TalExifOrientationInfo.FLIP_VERTICAL:;
                                                                                  // TalExifOrientationInfo.NORMAL:;
                                                                                  // TalExifOrientationInfo.ROTATE_180:;
                                                                                  // TalExifOrientationInfo.UNDEFINED:;

    case FWrapMode of

      //Display the image with its original dimensions:
      //* The image is placed in the upper-left corner of the rectangle of the control.
      //* If the image is larger than the control's rectangle, then only the upper-left part of the image,
      //  which fits in the rectangle of the control, is shown. The image is not resized.
      TGosImageWrapMode.Original:
        begin
          Result := nil; // todo
        end;

      //Best fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image
      //  (keeping image proportions – the ratio between the width and height) to fit the whole image in the rectangle
      //  of the control. That is, either the width of the resized image is equal to the width of the control's rectangle
      //  or the height of the resized image is equal to the height of the rectangle of the control. The whole image
      //  should be displayed. The image is displayed centered in the rectangle of the control.
      // * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
      //  the rectangle of the control. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
      TGosImageWrapMode.Fit:
        begin
          {$IFDEF ALDPK}
          if aFileName <> '' then fBufBitmap := ALLoadFitIntoFileImageV3(aFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := nil;
          {$ELSE}
          if fResourceName <> '' then fBufBitmap := GosLoadFitIntoResourceImageV3(fResourceName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := GosLoadFitIntoFileImageV3(fFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale);
          {$ENDIF}
          result := fBufBitmap;
        end;

      //Stretch the image to fill the entire rectangle of the control.
      TGosImageWrapMode.Stretch:
        begin
          {$IFDEF ALDPK}
          if aFileName <> '' then fBufBitmap := ALLoadStretchFileImageV3(aFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := nil;
          {$ELSE}
          if fResourceName <> '' then fBufBitmap := GosLoadStretchResourceImageV3(fResourceName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := GosLoadStretchFileImageV3(fFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale);
          {$ENDIF}
          result := fBufBitmap;
        end;

      //Tile (multiply) the image to cover the entire rectangle of the control:
      //* If the image is larger than the rectangle of the control, then only the
      //  upper-left part of the image, which fits in the rectangle of the control, is shown. The image is not resized.
      //* If the image (original size) is smaller than the rectangle of the control, then the multiple images are tiled
      //  (placed one next to another) to fill the entire rectangle of the control. The images are placed beginning from
      //  the upper-left corner of the rectangle of the control.
      TGosImageWrapMode.Tile:
        begin
          Result := nil; // todo
        end;

      //Center the image to the rectangle of the control:
      //* The image is always displayed at its original size (regardless whether the rectangle of the control is larger or smaller than the image size).
      TGosImageWrapMode.Center:
        begin
          Result := nil; // todo
        end;

      //Fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image (keeping image proportions--the ratio between the width and height)
      //  to fit the whole image in the rectangle of the control. That is, either the width of the resized image is equal to the width of the control's rectangle or the height of the
      //  resized image is equal to the height of the control's rectangle. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
      //* If the original image is smaller than the rectangle of the control, then the image is not resized. The image is displayed centered in the rectangle of the control.
      TGosImageWrapMode.Place:
        begin
          Result := nil; // todo
        end;

      //Best fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image
      //  (keeping image proportions – the ratio between the width and height) to fit the height or the width of the image in the rectangle
      //  of the control and crop the extra part of the image. That is, the width of the resized image is equal to the width of the control's rectangle
      //  AND the height of the resized image is equal to the height of the rectangle of the control.
      // * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
      //  the rectangle of the control. Whole the image should be displayed.
      TGosImageWrapMode.FitAndCrop:
        begin
          {$IFDEF ALDPK}
          if aFileName <> '' then fBufBitmap := ALLoadFitIntoAndCropFileImageV3(aFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := nil;
          {$ELSE}
          if fResourceName <> '' then fBufBitmap := GosLoadFitIntoAndCropResourceImageV3(fResourceName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := GosLoadFitIntoAndCropFileImageV3(fFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale);
          {$ENDIF}
          result := fBufBitmap;
        end;

      //to hide a stupid warning else
      else Result := nil;

    end;

    if result <> nil then fBufBitmapRect := TrectF.Create(0,0, result.Width/FScreenScale, result.Height/FScreenScale).
                                              CenterAt(LocalRect);

  {$IFDEF debug}
  finally
    AlDebugImageMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{***********************}
procedure TGosImage.Paint;
var aMatrix: Tmatrix;
    aMatrixRotationCenter: TpointF;
    R: TRectF;
begin

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  case fExifOrientationInfo of
    TGosExifOrientationInfo.FLIP_HORIZONTAL: begin
                                              aMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
                                              aMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
                                              aMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-aMatrixRotationCenter.X,-aMatrixRotationCenter.Y);
                                              aMatrix := aMatrix * TMatrix.CreateScaling(-1, 1); // matrix.setScale(-1, 1);
                                              aMatrix := aMatrix * TMatrix.CreateTranslation(aMatrixRotationCenter.X,aMatrixRotationCenter.Y);
                                              Canvas.SetMatrix(aMatrix);
                                            end;
    TGosExifOrientationInfo.FLIP_VERTICAL: begin
                                            aMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
                                            aMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
                                            aMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-aMatrixRotationCenter.X,-aMatrixRotationCenter.Y);
                                            aMatrix := aMatrix * TMatrix.CreateScaling(1, -1); // matrix.setRotate(180); matrix.setScale(-1, 1);
                                            aMatrix := aMatrix * TMatrix.CreateTranslation(aMatrixRotationCenter.X,aMatrixRotationCenter.Y);
                                            Canvas.SetMatrix(aMatrix);
                                          end;
    TGosExifOrientationInfo.NORMAL:;
    TGosExifOrientationInfo.ROTATE_180: begin
                                         aMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
                                         aMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
                                         aMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-aMatrixRotationCenter.X,-aMatrixRotationCenter.Y);
                                         aMatrix := aMatrix * TMatrix.CreateRotation(DegToRad(180)); // matrix.setRotate(180);
                                         aMatrix := aMatrix * TMatrix.CreateTranslation(aMatrixRotationCenter.X,aMatrixRotationCenter.Y);
                                         Canvas.SetMatrix(aMatrix);
                                       end;
    TGosExifOrientationInfo.ROTATE_270: begin
                                         aMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
                                         aMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
                                         aMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-aMatrixRotationCenter.X,-aMatrixRotationCenter.Y);
                                         aMatrix := aMatrix * TMatrix.CreateRotation(DegToRad(-90)); // matrix.setRotate(-90);
                                         aMatrix := aMatrix * TMatrix.CreateTranslation(aMatrixRotationCenter.X,aMatrixRotationCenter.Y);
                                         Canvas.SetMatrix(aMatrix);
                                       end;
    TGosExifOrientationInfo.ROTATE_90: begin
                                        aMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
                                        aMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
                                        aMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-aMatrixRotationCenter.X,-aMatrixRotationCenter.Y);
                                        aMatrix := aMatrix * TMatrix.CreateRotation(DegToRad(90)); // matrix.setRotate(90);
                                        aMatrix := aMatrix * TMatrix.CreateTranslation(aMatrixRotationCenter.X,aMatrixRotationCenter.Y);
                                        Canvas.SetMatrix(aMatrix);
                                      end;
    TGosExifOrientationInfo.TRANSPOSE: begin
                                        aMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
                                        aMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
                                        aMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-aMatrixRotationCenter.X,-aMatrixRotationCenter.Y);
                                        aMatrix := aMatrix * TMatrix.CreateRotation(DegToRad(90)); // matrix.setRotate(90);
                                        aMatrix := aMatrix * TMatrix.CreateScaling(-1, 1); // matrix.setScale(-1, 1);
                                        aMatrix := aMatrix * TMatrix.CreateTranslation(aMatrixRotationCenter.X,aMatrixRotationCenter.Y);
                                        Canvas.SetMatrix(aMatrix);
                                      end;
    TGosExifOrientationInfo.TRANSVERSE: begin
                                         aMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
                                         aMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
                                         aMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-aMatrixRotationCenter.X,-aMatrixRotationCenter.Y);
                                         aMatrix := aMatrix * TMatrix.CreateRotation(DegToRad(-90)); // matrix.setRotate(-90);
                                         aMatrix := aMatrix * TMatrix.CreateScaling(-1, 1); // matrix.setScale(-1, 1);
                                         aMatrix := aMatrix * TMatrix.CreateTranslation(aMatrixRotationCenter.X,aMatrixRotationCenter.Y);
                                         Canvas.SetMatrix(aMatrix);
                                       end;
    TGosExifOrientationInfo.UNDEFINED:;
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       GosPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TGosImage.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TGosImage.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************************************}
procedure TGosImage.SetWrapMode(const Value: TGosImageWrapMode);
begin
  if FWrapMode <> Value then begin
    clearBufBitmap;
    FWrapMode := Value;
    Repaint;
  end;
end;

{**************************************************}
procedure TGosImage.setFileName(const Value: String);
begin
  if FFileName <> Value then begin
    clearBufBitmap;
    FFileName := Value;
    Repaint;
  end;
end;

{******************************************************}
procedure TGosImage.setResourceName(const Value: String);
begin
  if FResourceName <> Value then begin
    clearBufBitmap;
    FResourceName := Value;
    Repaint;
  end;
end;

{**************************************************}
constructor TGosRectangle.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  inherited Create(AOwner);
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
  fShadow := TGosShadow.Create;
  fShadow.OnChanged := ShadowChanged;
  {$IF DEFINED(MSWindows) or DEFINED(_MACOS)}
  fShadowEffect := nil;
  {$ENDIF}
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
end;

{******************************}
destructor TGosRectangle.Destroy;
begin
  clearBufBitmap;
  GosFreeAndNil(fShadow);
  {$IF DEFINED(MSWindows) or DEFINED(_MACOS)}
  GosFreeAndNil(fShadowEffect);
  {$ENDIF}
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
  {$ENDIF}
  inherited;
end;

{************************************}
procedure TGosRectangle.clearBufBitmap;
begin
  GosFreeAndNil(fBufBitmap);
end;

{**************************************************}
procedure TGosRectangle.FillChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{****************************************************}
procedure TGosRectangle.StrokeChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{****************************************************}
procedure TGosRectangle.ShadowChanged(Sender: TObject);
begin
  clearBufBitmap;
  {$IF DEFINED(MSWindows) or DEFINED(_MACOS)}
  if shadow.enabled then begin
    if not assigned(fShadowEffect) then begin
      fShadowEffect := TshadowEffect.Create(self);
      fShadowEffect.Parent := self;
      fShadowEffect.SetSubComponent(true);
      fShadowEffect.stored := False;
    end;
    fShadowEffect.ShadowColor := shadow.ShadowColor;
    fShadowEffect.distance := 0; // Specifies the distance between the shadow and the visual object to which TShadowEffect is applied.
                                 // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
    fShadowEffect.Direction := 0;  // Specifies the direction (in degrees) of the shadow.
                                   // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
    fShadowEffect.Opacity := 1; // Opacity is a System.Single value that takes values in the range from 0 through 1.
                                // we use the opacity of the color instead
    fShadowEffect.softness := fShadow.blur / 24; // Specifies the amount of blur applied to the shadow.
                                                 // Softness is a System.Single value that takes values in the range from 0 through 9.
                                                 // i calculate approximatly that 0.5 = around 12 for blur
  end
  else GosFreeAndNil(fShadowEffect);
  {$ENDIF}
  if FUpdating = 0 then Repaint;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TGosRectangle.MakeBufBitmap: TTexture;
{$ELSE}
function TGosRectangle.MakeBufBitmap: Tbitmap;
{$ENDIF}

var aSaveStrokeThickness: single;
    aSaveShadowOffsetX: single;
    aSaveShadowOffsetY: single;
    aSaveShadowBlur: single;
    aRect: TRectf;
    {$IF defined(ANDROID)}
    aBitmap: Jbitmap;
    aCanvas: Jcanvas;
    {$ELSEIF defined(IOS)}
    aBitmapSurface: TbitmapSurface;
    aColorSpace: CGColorSpaceRef;
    aContext: CGContextRef;
    {$ENDIF}

begin

  if (csDesigning in ComponentState) or
     (not fdoubleBuffered) or
     (Scene = nil) or
     //--- don't do bufbitmap is size=0
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     //--- don't do bufbitmap if only fill with solid color
     (((Stroke.Kind = TBrushKind.None) or
       (sides = []))
      and
      ((SameValue(xRadius, 0, TEpsilon.position)) or
       (SameValue(yRadius, 0, TEpsilon.position)) or
       (corners=[]))
      and
      (not fShadow.enabled)
      and
      (Fill.Kind in [TBrushKind.None, TBrushKind.Solid]))
  then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;

  {$IFDEF debug}
  GosLog('TGosRectangle.MakeBufBitmap', 'Name: ' + Name, TGosLogType.verbose);
  inc(AlDebugRectangleMakeBufBitmapCount);
  AlDebugRectangleMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  //init fBufBitmapRect / aRect
  fBufBitmapRect := GosAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));
  if Shadow.enabled then begin
    fBufBitmapRect.Inflate(Shadow.blur, Shadow.blur); // add the extra space needed to draw the shadow
    fBufBitmapRect := GosAlignDimensionToPixelRound(fBufBitmapRect, FScreenScale); // to have the pixel aligned width and height
    aRect.Offset(Shadow.blur * FScreenScale, Shadow.blur * FScreenScale);
  end;

  //translate Stroke.Thickness from virtual to real pixel
  Stroke.OnChanged := Nil;
  aSaveStrokeThickness := Stroke.Thickness;
  Stroke.Thickness := Stroke.Thickness * fScreenScale;
  //-----
  Shadow.OnChanged := nil;
  aSaveShadowOffsetX := Shadow.OffsetX;
  aSaveShadowOffsetY := Shadow.OffsetY;
  aSaveShadowBlur := Shadow.Blur;
  Shadow.OffsetX := Shadow.OffsetX * fScreenScale;
  Shadow.OffsetY := Shadow.OffsetY * fScreenScale;
  Shadow.Blur := Shadow.Blur * fScreenScale;
  try

    {$IFDEF ANDROID}

    //create the drawing surface
    GosCreateDrawingSurface(aBitmap, // Var aBitmap: Jbitmap;
                           aCanvas, // var aCanvas: Jcanvas;
                           round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
                           round(fBufBitmapRect.height * FScreenScale));// const h: integer)
    try

       GosPaintRectangle(aCanvas, // const aBitmap: Jbitmap;
                        aRect, // const Rect: TrectF;
                        Fill, // const Fill: TBrush;
                        Stroke, // const Stroke: TStrokeBrush;
                        Shadow, // const Shadow: TALShadow
                        Sides, // const Sides: TSides;
                        Corners, // const Corners: TCorners;
                        XRadius * fScreenScale, // const XRadius: Single = 0;
                        YRadius * fScreenScale); // const YRadius: Single = 0);

      fBufBitmap := GosJBitmaptoTexture(aBitmap);

    finally
      GosFreeDrawingSurface(aBitmap, aCanvas);
    end;

    {$ELSEIF DEFINED(IOS)}

    //create the drawing surface
    GosCreateDrawingSurface(aBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                           aContext, //    Var aContext: CGContextRef;
                           aColorSpace, // Var aColorSpace: CGColorSpaceRef;
                           round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
                           round(fBufBitmapRect.height * FScreenScale));// const h: integer)
    try

       GosPaintRectangle(aContext, // const aContext: CGContextRef;
                        aColorSpace, // const aColorSpace: CGColorSpaceRef;
                        aBitmapSurface.Height, // const aGridHeight: Single;
                        aRect, // const Rect: TrectF;
                        Fill, // const Fill: TBrush;
                        Stroke, // const Stroke: TStrokeBrush;
                        Shadow, // const Shadow: TALShadow
                        Sides, // const Sides: TSides;
                        Corners, // const Corners: TCorners;
                        XRadius * fScreenScale, // const XRadius: Single = 0;
                        YRadius * fScreenScale); // const YRadius: Single = 0);

      fBufBitmap := GosBitmapSurfacetoTexture(aBitmapSurface);

    finally
      GosFreeDrawingSurface(aBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                           aContext, // Var aContext: CGContextRef;
                           aColorSpace); // Var aColorSpace: CGColorSpaceRef;
    end;

    {$ENDIF}

  finally
    Stroke.Thickness := aSaveStrokeThickness;
    Stroke.OnChanged := StrokeChanged;
    //-----
    Shadow.OffsetX := aSaveShadowOffsetX;
    Shadow.OffsetY := aSaveShadowOffsetY;
    Shadow.Blur := aSaveShadowBlur;
    Shadow.OnChanged := ShadowChanged;
  end;

  //set the result
  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugRectangleMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{***************************}
procedure TGosRectangle.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       GosPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

end;

{*************************************************************}
procedure TGosRectangle.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then clearbufBitmap;
  end;
end;

{*******************************************************}
procedure TGosRectangle.SetShadow(const Value: TGosShadow);
begin
  FShadow.Assign(Value);
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TGosRectangle.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TGosRectangle.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{***********************************************}
constructor TGosCircle.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
  fShadow := TGosShadow.Create;
  fShadow.OnChanged := ShadowChanged;
  {$IF DEFINED(MSWindows) or DEFINED(_MACOS)}
  fShadowEffect := nil;
  {$ENDIF}
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
end;

{***************************}
destructor TGosCircle.Destroy;
begin
  clearBufBitmap;
  GosFreeAndNil(fShadow);
  {$IF DEFINED(MSWindows) or DEFINED(_MACOS)}
  GosFreeAndNil(fShadowEffect);
  {$ENDIF}
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
  {$ENDIF}
  inherited;
end;

{*********************************}
procedure TGosCircle.clearBufBitmap;
begin
  GosFreeAndNil(fBufBitmap);
end;

{***********************************************}
procedure TGosCircle.FillChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{*************************************************}
procedure TGosCircle.StrokeChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{*************************************************}
procedure TGosCircle.ShadowChanged(Sender: TObject);
begin
  clearBufBitmap;
  {$IF DEFINED(MSWindows) or DEFINED(_MACOS)}
  if shadow.enabled then begin
    if not assigned(fShadowEffect) then begin
      fShadowEffect := TshadowEffect.Create(self);
      fShadowEffect.Parent := self;
      fShadowEffect.SetSubComponent(true);
      fShadowEffect.stored := False;
    end;
    fShadowEffect.ShadowColor := shadow.ShadowColor;
    fShadowEffect.distance := 0; // Specifies the distance between the shadow and the visual object to which TShadowEffect is applied.
                                 // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
    fShadowEffect.Direction := 0;  // Specifies the direction (in degrees) of the shadow.
                                   // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
    fShadowEffect.Opacity := 1; // Opacity is a System.Single value that takes values in the range from 0 through 1.
                                // we use the opacity of the color instead
    fShadowEffect.softness := fShadow.blur / 24; // Specifies the amount of blur applied to the shadow.
                                                 // Softness is a System.Single value that takes values in the range from 0 through 9.
                                                 // i calculate approximatly that 0.5 = around 12 for blur
  end
  else GosFreeAndNil(fShadowEffect);
  {$ENDIF}
  if FUpdating = 0 then Repaint;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TGosCircle.MakeBufBitmap: TTexture;
{$ELSE}
function TGosCircle.MakeBufBitmap: Tbitmap;
{$ENDIF}

var aSaveStrokeThickness: single;
    aSaveShadowOffsetX: single;
    aSaveShadowOffsetY: single;
    aSaveShadowBlur: single;
    aRect: TRectf;
    {$IF defined(ANDROID)}
    aBitmap: Jbitmap;
    aCanvas: Jcanvas;
    {$ELSEIF defined(IOS)}
    aBitmapSurface: TbitmapSurface;
    aColorSpace: CGColorSpaceRef;
    aContext: CGContextRef;
    {$ENDIF}

begin

  if (csDesigning in ComponentState) or
     (not fdoubleBuffered) or
     (Scene = nil) or
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;

  {$IFDEF debug}
  GosLog('TGosCircle.MakeBufBitmap', 'Name: ' + Name, TGosLogType.verbose);
  inc(AlDebugCircleMakeBufBitmapCount);
  AlDebugCircleMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  //init fBufBitmapRect / aRect
  fBufBitmapRect := GosAlignDimensionToPixelRound(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), FScreenScale); // to have the pixel aligned width and height
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));
  if Shadow.enabled then begin
    fBufBitmapRect.Inflate(Shadow.blur, Shadow.blur); // add the extra space needed to draw the shadow
    fBufBitmapRect := GosAlignDimensionToPixelRound(fBufBitmapRect, FScreenScale); // to have the pixel aligned width and height
    aRect.Offset(Shadow.blur * FScreenScale, Shadow.blur * FScreenScale);
  end;

  //translate Stroke.Thickness from virtual to real pixel
  Stroke.OnChanged := Nil;
  aSaveStrokeThickness := Stroke.Thickness;
  Stroke.Thickness := Stroke.Thickness * fScreenScale;
  //-----
  Shadow.OnChanged := nil;
  aSaveShadowOffsetX := Shadow.OffsetX;
  aSaveShadowOffsetY := Shadow.OffsetY;
  aSaveShadowBlur := Shadow.Blur;
  Shadow.OffsetX := Shadow.OffsetX * fScreenScale;
  Shadow.OffsetY := Shadow.OffsetY * fScreenScale;
  Shadow.Blur := Shadow.Blur * fScreenScale;
  try

    {$IFDEF ANDROID}

    //create the drawing surface
    GosCreateDrawingSurface(aBitmap, // Var aBitmap: Jbitmap;
                           aCanvas, // var aCanvas: Jcanvas;
                           round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
                           round(fBufBitmapRect.Height * FScreenScale));// const h: integer)
    try

      GosPaintCircle(aCanvas, // const aBitmap: Jbitmap;
                    aRect, // const Rect: TrectF;
                    Fill, // const Fill: TBrush;
                    Stroke, // const Stroke: TStrokeBrush;
                    Shadow); // const Shadow: TGosShadow

      fBufBitmap := GosJBitmaptoTexture(aBitmap);

    finally
      GosFreeDrawingSurface(aBitmap, aCanvas);
    end;

    {$ELSEIF DEFINED(IOS)}

     //create the drawing surface
    GosCreateDrawingSurface(aBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                           aContext, //    Var aContext: CGContextRef;
                           aColorSpace, // Var aColorSpace: CGColorSpaceRef;
                           round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
                           round(fBufBitmapRect.Height * FScreenScale));// const h: integer)
    try

      GosPaintCircle(aContext, // const aContext: CGContextRef;
                    aColorSpace, // const aColorSpace: CGColorSpaceRef;
                    aBitmapSurface.Height, // const aGridHeight: Single;
                    aRect, // const Rect: TrectF;
                    Fill, // const Fill: TBrush;
                    Stroke, // const Stroke: TStrokeBrush;
                    Shadow); // const Shadow: TGosShadow

      fBufBitmap := GosBitmapSurfacetoTexture(aBitmapSurface);

    finally
      GosFreeDrawingSurface(aBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                           aContext, // Var aContext: CGContextRef;
                           aColorSpace); // Var aColorSpace: CGColorSpaceRef;
    end;

    {$ENDIF}

  finally
    Stroke.Thickness := aSaveStrokeThickness;
    Stroke.OnChanged := StrokeChanged;
    //-----
    Shadow.OffsetX := aSaveShadowOffsetX;
    Shadow.OffsetY := aSaveShadowOffsetY;
    Shadow.Blur := aSaveShadowBlur;
    Shadow.OnChanged := ShadowChanged;
  end;

  //set the result
  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugCircleMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{************************}
procedure TGosCircle.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       GosPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

end;

{**********************************************************}
procedure TGosCircle.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then clearbufBitmap;
  end;
end;

{****************************************************}
procedure TGosCircle.SetShadow(const Value: TGosShadow);
begin
  FShadow.Assign(Value);
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TGosCircle.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TGosCircle.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{*********************************************}
constructor TGosLine.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
end;

{*************************}
destructor TGosLine.Destroy;
begin
  clearBufBitmap;
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
  {$ENDIF}
  inherited;
end;

{*******************************}
procedure TGosLine.clearBufBitmap;
begin
  GosFreeAndNil(fBufBitmap);
end;

{*********************************************}
procedure TGosLine.FillChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{***********************************************}
procedure TGosLine.StrokeChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TGosLine.MakeBufBitmap: TTexture;
{$ELSE}
function TGosLine.MakeBufBitmap: Tbitmap;
{$ENDIF}

{$IF defined(IOS)}
const aDefaultInputRange: array[0..1] of CGFloat = (0, 1);
{$ENDIF}

{$IF defined(ANDROID)}
var aBitmap: Jbitmap;
    aCanvas: Jcanvas;
    aPaint: JPaint;
    aRect: TRectf;
    aStrokeWidth: Single;
{$ELSEIF defined(IOS)}
var aBitmapSurface: TbitmapSurface;
    aColorSpace: CGColorSpaceRef;
    aContext: CGContextRef;
    aAlphaColor: TAlphaColorCGFloat;
    aRect: TRectf;
    aStrokeWidth: Single;
{$ENDIF}

begin

  if (csDesigning in ComponentState) or
     (not fdoubleBuffered) or
     (Scene = nil) or
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     (Stroke.Kind = TBrushKind.None) or
     (SameValue(Stroke.Thickness, 0, TEpsilon.position)) then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;

  {$IFDEF debug}
  GosLog('TGosLine.MakeBufBitmap', 'Name: ' + Name, TGosLogType.verbose);
  inc(AlDebugLineMakeBufBitmapCount);
  AlDebugLineMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  {$IFDEF ANDROID}

  //init aStrokeWidth
  if (LineLocation = TLineLocation.InnerWithin) then aStrokeWidth := Min(Stroke.Thickness, Min(Width, Height))
  else aStrokeWidth := Stroke.Thickness;

  //init fBufBitmapRect / aRect
  case lineType of
    TLineType.Diagonal: fBufBitmapRect := GosAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
    TLineType.Top: begin
                     fBufBitmapRect := GosAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, aStrokeWidth), FScreenScale); // to have the pixel aligned width and height
                     if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, -aStrokeWidth/2);
                   end;
    TLineType.Left: begin
                      fBufBitmapRect := GosAlignDimensionToPixelRound(TrectF.Create(0, 0, aStrokeWidth, height), FScreenScale); // to have the pixel aligned width and height
                      if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(-aStrokeWidth/2, 0);
                    end;
    TLineType.Bottom: begin
                        fBufBitmapRect := GosAlignDimensionToPixelRound(TrectF.Create(0, height - aStrokeWidth, Width, height), FScreenScale); // to have the pixel aligned width and height
                        if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, aStrokeWidth/2);
                      end;
    TLineType.Right: begin
                       fBufBitmapRect := GosAlignDimensionToPixelRound(TrectF.Create(width - aStrokeWidth, 0, width, height), FScreenScale); // to have the pixel aligned width and height
                       if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(aStrokeWidth/2, 0);
                     end;
  end;
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

  //create the drawing surface
  GosCreateDrawingSurface(aBitmap, // Var aBitmap: Jbitmap;
                         aCanvas, // var aCanvas: Jcanvas;
                         round(aRect.Width), // const w: integer;
                         round(aRect.Height));// const h: integer)
  try

    //create the canvas and the paint
    aPaint := TJPaint.JavaClass.init;
    aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    //stroke the circle
    if Stroke.Kind <> TBrushKind.None then begin

      //init aPaint
      aPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
      aPaint.setStrokeWidth(aStrokeWidth * FScreenScale);

      //stroke with solid color
      if Stroke.Kind = TBrushKind.Solid then begin
        aPaint.setColor(integer(Stroke.Color));
        case lineType of
          TLineType.Diagonal: aCanvas.drawLine(aRect.left {startX},
                                               aRect.top {startY},
                                               aRect.right {stopX},
                                               aRect.Bottom {stopY},
                                               apaint);
          TLineType.Top,
          TLineType.Bottom: aCanvas.drawLine(aRect.left {startX},
                                             (aRect.bottom - aRect.top) / 2 {startY},
                                             aRect.right {stopX},
                                             (aRect.bottom - aRect.top) / 2 {stopY},
                                             apaint);
          TLineType.Left,
          TLineType.Right: aCanvas.drawLine((aRect.right - aRect.left) / 2 {startX},
                                            aRect.top {startY},
                                            (aRect.right - aRect.left) / 2 {stopX},
                                            aRect.bottom {stopY},
                                            apaint);
        end;
      end;

    end;

    //free the paint and the canvas
    aPaint := nil;

    //convert aBitmap to TALTexture
    fBufBitmap := GosJBitmaptoTexture(aBitmap);

  finally
    GosFreeDrawingSurface(aBitmap, aCanvas);
  end;

  {$ELSEIF DEFINED(IOS)}

  //init aStrokeWidth
  if (LineLocation = TLineLocation.InnerWithin) then aStrokeWidth := Min(Stroke.Thickness, Min(Width, Height))
  else aStrokeWidth := Stroke.Thickness;

  //init fBufBitmapRect / aRect
  case lineType of
    TLineType.Diagonal: fBufBitmapRect := GosAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
    TLineType.Top: begin
                     fBufBitmapRect := GosAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, aStrokeWidth), FScreenScale); // to have the pixel aligned width and height
                     if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, -aStrokeWidth/2);
                   end;
    TLineType.Left: begin
                      fBufBitmapRect := GosAlignDimensionToPixelRound(TrectF.Create(0, 0, aStrokeWidth, height), FScreenScale); // to have the pixel aligned width and height
                      if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(-aStrokeWidth/2, 0);
                    end;
    TLineType.Bottom: begin
                        fBufBitmapRect := GosAlignDimensionToPixelRound(TrectF.Create(0, height - aStrokeWidth, Width, height), FScreenScale); // to have the pixel aligned width and height
                        if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, aStrokeWidth/2);
                      end;
    TLineType.Right: begin
                       fBufBitmapRect := GosAlignDimensionToPixelRound(TrectF.Create(width - aStrokeWidth, 0, width, height), FScreenScale); // to have the pixel aligned width and height
                       if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(aStrokeWidth/2, 0);
                     end;
  end;
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

  //create the drawing surface
  GosCreateDrawingSurface(aBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                         aContext, //    Var aContext: CGContextRef;
                         aColorSpace, // Var aColorSpace: CGColorSpaceRef;
                         round(aRect.Width), // const w: integer;
                         round(aRect.Height));// const h: integer)
  try

    //stroke the circle
    if Stroke.Kind <> TBrushKind.None then begin

      //stroke with solid color
      if Stroke.Kind = TBrushKind.Solid then begin
        aAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
        CGContextSetRGBStrokeColor(aContext, aAlphaColor.R, aAlphaColor.G, aAlphaColor.B, aAlphaColor.A);
        CGContextSetLineWidth(aContext, Stroke.Thickness * FScreenScale);
        case lineType of
          TLineType.Diagonal: begin
                                CGContextBeginPath(acontext);
                                CGContextMoveToPoint(acontext, aRect.left, aBitmapSurface.height - aRect.top);
                                CGContextAddLineToPoint(acontext, aRect.right, aBitmapSurface.height - aRect.Bottom);
                              end;
          TLineType.Top,
          TLineType.Bottom: begin
                              CGContextBeginPath(acontext);
                              CGContextMoveToPoint(acontext, aRect.left, aBitmapSurface.height - ((aRect.bottom - aRect.top) / 2));
                              CGContextAddLineToPoint(acontext, aRect.right, aBitmapSurface.height - ((aRect.bottom - aRect.top) / 2));
                            end;
          TLineType.Left,
          TLineType.Right: begin
                             CGContextBeginPath(acontext);
                             CGContextMoveToPoint(acontext, (aRect.right - aRect.left) / 2, aBitmapSurface.height - aRect.top);
                             CGContextAddLineToPoint(acontext, (aRect.right - aRect.left) / 2, aBitmapSurface.height - aRect.Bottom);
                           end;
        end;
        CGContextStrokePath(acontext);
      end;

    end;

    //convert the aBitmapSurface to texture
    fBufBitmap := GosBitmapSurfacetoTexture(aBitmapSurface);

  finally
    GosFreeDrawingSurface(aBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                         aContext, // Var aContext: CGContextRef;
                         aColorSpace); // Var aColorSpace: CGColorSpaceRef;
  end;

  {$ENDIF}

  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugLineMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{**********************}
procedure TGosLine.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       GosPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

end;

{********************************************************}
procedure TGosLine.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then clearbufBitmap;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TGosLine.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TGosLine.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{**************************************************************************************************}
constructor TGosDoubleBufferedTextLayout.Create(const ACanvas: TCanvas; const aTextControl: TGosText);
var aScreenSrv: IFMXScreenService;
begin
  inherited Create(ACanvas);
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fBufBitmap := nil;
  fTextControl := aTextControl;
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
end;

{*********************************************}
destructor TGosDoubleBufferedTextLayout.Destroy;
begin
  clearBufBitmap;
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
  {$ENDIF}
  inherited;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TGosDoubleBufferedTextLayout.MakeBufBitmap: TTexture;
{$ELSE}
function TGosDoubleBufferedTextLayout.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aOptions: TGosDrawMultiLineTextOptions;
begin

  if (fTextControl.Scene = nil) or // << fTextControl.Scene = nil mean mostly the fTextControl (or his parent) is not yet assigned to any form
     (fTextControl.text.IsEmpty) then begin
    clearBufBitmap;
    exit(nil);
  end;

  // we need to use the value of the fTextControl and not of the current TTextLayout
  // because TText update some value on each call to adjustsize with different value that will
  // be used in paint
  if (fBufBitmap <> nil) and
     (fBufHorizontalAlign = fTextControl.TextSettings.HorzAlign) and  // TText.adjustsize always use TTextAlign.Leading
     (fBufVerticalAlign = fTextControl.TextSettings.VertAlign) and // TText.adjustsize always use TTextAlign.Leading
     (fBufFontColor = fTextControl.TextSettings.FontColor) and
     (sametext(fBuffontFamily, fTextControl.TextSettings.Font.Family)) and
     (fBuffontStyle = fTextControl.TextSettings.Font.Style) and
     (SameValue(fBuffontSize, fTextControl.TextSettings.Font.Size, TEpsilon.FontSize)) and
     (fBufWordWrap = fTextControl.TextSettings.WordWrap) and
     (fBufAutosize = fTextControl.AutoSize) and
     (fBufTrimming = fTextControl.TextSettings.Trimming) and
     (
      (
       (not fTextControl.AutoSize) and // if not autosize then the dimensions returned by this function will depend of MaxSize.X / MaxSize.Y
       (SameValue(fBufSize.cx, MaxSize.X, TEpsilon.position)) and
       (SameValue(fBufSize.cy, MaxSize.Y, TEpsilon.position))
      )
      OR
      (
       (fTextControl.AutoSize) and
       (
        (SameValue(fBufSize.cx, MaxSize.x, TEpsilon.position)) or // if we already calculate the buf for maxsize.x
        (SameValue(fbufBitmapRect.width, MaxSize.x, TEpsilon.position)) or // if fbufBitmapRect.width = MaxSize.x we can not do anything better ;)
        ((not fBufTextBreaked) and
         (CompareValue(fbufBitmapRect.width, MaxSize.x, TEpsilon.position) <= 0)) // if fbufBitmapRect.width <= MaxSize.x and text wasn't breaked we can't do anything better
       ) and
       (
        (SameValue(fBufSize.cy, MaxSize.y, TEpsilon.position)) or // if we already calculate the buf for maxsize.y
        ((fBufAllTextDrawed) and
         (CompareValue(fbufBitmapRect.height, MaxSize.y, TEpsilon.position) <= 0)) // if fbufBitmapRect.height <= MaxSize.y and all text was drawed then we can't do anything better
       )
      )
     ) and
     (fBufText = fTextControl.Text) then exit(fBufBitmap);

  clearBufBitmap;
  fBufHorizontalAlign := fTextControl.TextSettings.HorzAlign;
  fBufVerticalAlign := fTextControl.TextSettings.VertAlign;
  fBufFontColor := fTextControl.TextSettings.FontColor;
  fBuffontFamily := fTextControl.TextSettings.Font.Family;
  fBuffontStyle := fTextControl.TextSettings.Font.Style;
  fBuffontSize := fTextControl.TextSettings.Font.Size;
  fBufWordWrap := fTextControl.TextSettings.WordWrap;
  fBufAutosize := fTextControl.AutoSize;
  fBufTrimming := fTextControl.TextSettings.Trimming;
  fBufSize := MaxSize;
  fBufText := fTextControl.Text;

  {$IFDEF debug}
  GosLog('TGosDoubleBufferedTextLayout.MakeBufBitmap', 'Name: ' + fTextControl.Name +
                                                     'text:' + fBufText +
                                                     ' - MaxSize: '+floattostr(fBufSize.cX)+'x'+floattostr(fBufSize.cY), TGosLogType.verbose);
  inc(AlDebugTextMakeBufBitmapCount);
  AlDebugTextMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  //init fBufBitmapRect
  fBufBitmapRect := TRectF.Create(0, 0, fBufSize.cX * FScreenScale, fBufSize.cY * FScreenScale);

  //create aOptions
  aOptions := TGosDrawMultiLineTextOptions.Create;
  Try

    //init aOptions
    aOptions.FontName := fBuffontFamily;
    aOptions.FontSize := fBuffontSize * FScreenScale;
    aOptions.FontStyle := fBuffontStyle;
    aOptions.FontColor := fBufFontColor;
    //-----
    //aOptions.EllipsisText: String; // default = '…';
    //aOptions.EllipsisFontStyle: TFontStyles; // default = [];
    //aOptions.EllipsisFontColor: TalphaColor; // default = TAlphaColorRec.Null;
    //-----
    if (not fBufAutosize) and
       ((fTextControl.Fill.Kind <> TbrushKind.None) or
        (fTextControl.stroke.Kind <> TbrushKind.None)) then aOptions.AutoSize := false
    else aOptions.AutoSize := True;
    aOptions.WordWrap := fBufWordWrap;
    //aOptions.MaxLines: integer; // default = 0;
    aOptions.LineSpacing := fTextControl.LineSpacing * FScreenScale;
    aOptions.Trimming := fBufTrimming;
    //aOptions.FirstLineIndent: TpointF; // default = Tpointf.create(0,0);
    //aOptions.FailIfTextBreaked: boolean; // default = false
    //-----
    aOptions.HTextAlign := fBufHorizontalAlign;
    aOptions.VTextAlign := fBufVerticalAlign;
    //-----
    aOptions.Fill.assign(fTextControl.Fill);
    aOptions.Stroke.assign(fTextControl.Stroke);
    aOptions.Stroke.Thickness := aOptions.Stroke.Thickness * FScreenScale;
    aOptions.Sides := fTextControl.Sides;
    aOptions.XRadius := fTextControl.XRadius * FScreenScale;
    aOptions.YRadius := fTextControl.YRadius * FScreenScale;
    aOptions.Corners := fTextControl.Corners;
    aOptions.Padding := fTextControl.padding.Rect;
    aOptions.Padding.Top := aOptions.Padding.Top * FScreenScale;
    aOptions.Padding.right := aOptions.Padding.right * FScreenScale;
    aOptions.Padding.left := aOptions.Padding.left * FScreenScale;
    aOptions.Padding.bottom := aOptions.Padding.bottom * FScreenScale;
    //-----
    aOptions.TextIsHtml := fTextControl.TextIsHtml;

    //build fBufBitmap
    fBufBitmap := GosDrawMultiLineText(fBufText, // const aText: String; // support only basic html tag like <b>...</b>, <i>...</i>, <font color="#ffffff">...</font> and <span id="xxx">...</span>
                                      fBufBitmapRect, // var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                                      fBufTextBreaked,
                                      fBufAllTextDrawed,
                                      aOptions);
    {$IFDEF debug}
    GosLog('TGosDoubleBufferedTextLayout.MakeBufBitmap.ALDrawMultiLineText', 'Name: ' + fTextControl.Name +
                                                                           'text:' + fBufText +
                                                                           ' - fBufBitmapRect: '+floattostr(fBufBitmapRect.width)+'x'+floattostr(fBufBitmapRect.height) +
                                                                           ' - fBufTextBreaked: '+ BoolToStr(fBufTextBreaked, true) +
                                                                           ' - fBufAllTextDrawed: '+ BoolToStr(fBufAllTextDrawed, true), TGosLogType.verbose);
    {$endif}

    //align fbufBitmapRect
    if aOptions.AutoSize and (not fBufAutosize) then begin
      case aOptions.HTextAlign of
        TTextAlign.Center: begin
                             fbufBitmapRect.Offset(((fBufSize.cx * FScreenScale) - fbufBitmapRect.width) / 2, 0);
                           end;
        TTextAlign.Trailing: begin
                               fbufBitmapRect.Offset((fBufSize.cx * FScreenScale) - fbufBitmapRect.width, 0);
                             end;
      end;
      case aOptions.VTextAlign of
        TTextAlign.Center: begin
                             fbufBitmapRect.Offset(0, ((fBufSize.cy * FScreenScale) - fbufBitmapRect.Height) / 2);
                           end;
        TTextAlign.Trailing: begin
                               fbufBitmapRect.Offset(0, (fBufSize.cy * FScreenScale) - fbufBitmapRect.Height);
                             end;
      end;
    end;
    if fBufAutosize then fBufBitmapRect.Offset(-fBufBitmapRect.left, -fBufBitmapRect.top);

    //convert fbufBitmapRect do virtual pixel
    fbufBitmapRect.Top := fbufBitmapRect.Top / FScreenScale;
    fbufBitmapRect.right := fbufBitmapRect.right / FScreenScale;
    fbufBitmapRect.left := fbufBitmapRect.left / FScreenScale;
    fbufBitmapRect.bottom := fbufBitmapRect.bottom / FScreenScale;

  finally
    GosFreeAndNil(aOptions);
  end;

  //update the result
  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugTextMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{***************************************************}
procedure TGosDoubleBufferedTextLayout.clearBufBitmap;
begin
  GosFreeAndNil(fBufBitmap);
end;

{***************************************************}
procedure TGosDoubleBufferedTextLayout.DoRenderLayout;
begin
  MakeBufBitmap; // recreate the fBufBitmap
end;

{*************************************************************************}
procedure TGosDoubleBufferedTextLayout.DoDrawLayout(const ACanvas: TCanvas);
var aDestRect: TrectF;
    ADesignatedArea: TrectF;
    aLocation: TPointF;
begin

  MakeBufBitmap;
  if fBufBitmap = nil then exit;

  aDestRect := fBufBitmapRect;
  if fBufAutosize then begin
    ADesignatedArea := FTextControl.localrect;
    case FTextControl.HorzTextAlign of
      TTextAlign.Center: aLocation.X := (ADesignatedArea.Left + ADesignatedArea.Right - aDestRect.Width) / 2;
      TTextAlign.Leading: aLocation.X := ADesignatedArea.Left;
      TTextAlign.Trailing: aLocation.X := ADesignatedArea.Right - aDestRect.Width;
    end;
    case FTextControl.VertTextAlign of
      TTextAlign.Center: aLocation.Y := (ADesignatedArea.Top + ADesignatedArea.Bottom - aDestRect.Height) / 2;
      TTextAlign.Leading: aLocation.Y := ADesignatedArea.Top;
      TTextAlign.Trailing: aLocation.Y := ADesignatedArea.Bottom - aDestRect.Height;
    end;
    aDestRect.SetLocation(aLocation);
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(ACanvas).DrawTexture(ACanvas.AlignToPixel(aDestRect), // ATexRect (destRec)
                                        TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                        GosPrepareColor(TCustomCanvasGpu.ModulateColor, Opacity), // https://quality.embarcadero.com/browse/RSP-15432
                                        fBufBitmap);


  {$ELSE}

  aCanvas.DrawBitmap(fBufBitmap,
                     TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                     aCanvas.AlignToPixel(aDestRect), {DestRect}
                     Opacity, {opacity}
                     true{highSpeed});

  {$ENDIF}

end;

{*******************************************************}
function TGosDoubleBufferedTextLayout.GetTextRect: TRectF;
begin
  if fBufBitmap = nil then result := TrectF.Create(0,0,0,0)
  else result := fBufBitmapRect;
end;

{*********************************************************}
function TGosDoubleBufferedTextLayout.GetTextHeight: Single;
begin
  result := 0;
end;

{********************************************************}
function TGosDoubleBufferedTextLayout.GetTextWidth: Single;
begin
  result := 0;
end;

{*************************************************************************************}
function TGosDoubleBufferedTextLayout.DoPositionAtPoint(const APoint: TPointF): Integer;
begin
  result := 0;
end;

{***************************************************************************************}
function TGosDoubleBufferedTextLayout.DoRegionForRange(const ARange: TTextRange): TRegion;
begin
  setlength(result, 0);
end;

{**************************************************************************}
procedure TGosDoubleBufferedTextLayout.ConvertToPath(const APath: TPathData);
begin
  //do nothing - virtual
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TGosDoubleBufferedTextLayout.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TGosDoubleBufferedTextLayout.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{**}
type
  TALTextTextSettings = class(TTextSettings)
  public
    constructor Create(const AOwner: TPersistent); override;
  published
    property Font;
    property FontColor;
    property Trimming default TTextTrimming.Character;
    property WordWrap default false;
    property HorzAlign default TTextAlign.Leading;
    property VertAlign default TTextAlign.Center;
  end;

{****************************************************************}
constructor TALTextTextSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  Trimming := TTextTrimming.Character;
  WordWrap := false;
  HorzAlign := TTextAlign.Leading;
  VertAlign := TTextAlign.Center;
end;

{*********************************************}
constructor TGosText.Create(AOwner: TComponent);
var LClass: TTextSettingsClass;
begin
  inherited;
  //-----
  FFill := TBrush.Create(TBrushKind.none, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FStroke := TStrokeBrush.Create(TBrushKind.none, $FF000000);
  FStroke.OnChanged := StrokeChanged;
  FCorners := AllCorners;
  FXRadius := 0;
  FYRadius := 0;
  FSides := AllSides;
  fLineSpacing := 0;
  fTextIsHtml := False;
  fMustCallResized := False;
  //-----
  HitTest := False;
  //-----
  FAutoConvertFontFamily := True;
  FAutoTranslate := true;
  FAutoSize := False;
  fMaxWidth := 65535;
  fMaxHeight := 65535;
  //-----
  LClass := GetTextSettingsClass;
  if LClass = nil then LClass := TALTextTextSettings;
  //-----
  FLayout := TGosDoubleBufferedTextLayout.Create(nil, self);
  //-----
  //i use this way to know that the compoment
  //will load it's properties from the dfm
  if (aOwner <> nil) and
     (csloading in aOwner.ComponentState) then begin
    fRestoreLayoutUpdateAfterLoaded := True;
    Layout.BeginUpdate;
  end
  else fRestoreLayoutUpdateAfterLoaded := False;
  //-----
  FTextSettings := LClass.Create(Self);
  FTextSettings.OnChanged := OnFontChanged;
  FTextSettings.BeginUpdate;
  try
    FTextSettings.IsAdjustChanged := True;
  finally
    FTextSettings.EndUpdate; // << this will not call adjustsize because at this time
                             // << fautosize = false. This will only update the textsettings
                             // << of the flayout, and if we are in csloading, this will not even
                             // << call any DoRenderLayout (else yes it's will call DoRenderLayout
                             // << but with empty text)
  end;
end;

{*************************}
destructor TGosText.Destroy;
begin
  GosFreeAndNil(FTextSettings);
  GosFreeAndNil(FLayout);
  GosFreeAndNil(FStroke);
  GosFreeAndNil(FFill);
  inherited;
end;

{***********************}
procedure TGosText.Loaded;
begin
  //-----
  if (AutoTranslate) and
     (Text <> '') and
     (not (csDesigning in ComponentState)) then
      Text := GosTranslate(Text);
  //-----
  if (AutoConvertFontFamily) and
     (TextSettings.Font.Family <> '') and
     (not (csDesigning in ComponentState)) then
      TextSettings.Font.Family := GosConvertFontFamily(TextSettings.Font.Family, TextSettings.Font.Style);
  //-----
  inherited;
  //-----
  if fRestoreLayoutUpdateAfterLoaded then begin
    if (FAutoSize) and
       (Text <> '') then begin

      //Originally, if WordWrap then the algo take in account the current width of the TGosText
      //to calculate the autosized width (mean the size can be lower than maxwidth if the current width
      //is already lower than maxwidth). problem with that is if we for exemple change the text (or font
      //dimension) of an already calculated TGosText, then it's the old width (that correspond to the
      //previous text/font) that will be taken in account. finally the good way is to alway use the
      //maxwidth if we desir a max width and don't rely on the current width

      //if WordWrap then Layout.MaxSize := TPointF.Create(Min(Width, maxWidth), maxHeight)
      //else Layout.MaxSize := TPointF.Create(maxWidth, MaxHeight);
      if (WordWrap) and
         (Align in [TAlignLayout.Client,
                    TAlignLayout.Contents,
                    TAlignLayout.Top,
                    TAlignLayout.Bottom,
                    TAlignLayout.MostTop,
                    TAlignLayout.MostBottom,
                    TAlignLayout.VertCenter]) then Layout.MaxSize := TPointF.Create(Width, maxHeight)
      else Layout.MaxSize := TPointF.Create(maxWidth, MaxHeight);

    end
    else Layout.MaxSize := TPointF.Create(width, height);  // << this is important because else when the component is loaded then
                                                           // << we will call DoRenderLayout that will use the original maxsise (ie: 65535, 65535)
                                                           // << and then after when we will paint the control, we will again call DoRenderLayout
                                                           // << but this time with maxsize = aTextControl.size and off course if wordwrap we will
                                                           // << need to redo the bufbitmap
    Layout.endUpdate;
    AdjustSize;
  end;
  fRestoreLayoutUpdateAfterLoaded := False;
end;

{*********************************************}
procedure TGosText.FillChanged(Sender: TObject);
begin
  clearBufBitmap;
  if FUpdating = 0 then Repaint;
end;

{***********************************************}
procedure TGosText.StrokeChanged(Sender: TObject);
begin
  clearBufBitmap;
  if FUpdating = 0 then Repaint;
end;

{*********************************************}
procedure TGosText.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

{*****************************************************}
procedure TGosText.SetStroke(const Value: TStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{****************************************}
function TGosText.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

{**************************************}
function TGosText.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

{**************************************************}
procedure TGosText.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then begin
    FCorners := Value;
    Repaint;
  end;
end;

{************************************************}
procedure TGosText.SetXRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Min(Value, Min(Width / 2, Height / 2))
  else NewValue := Value;
  if not SameValue(FXRadius, NewValue, TEpsilon.Vector) then begin
    FXRadius := NewValue;
    Repaint;
  end;
end;

{************************************************}
procedure TGosText.SetYRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Min(Value, Min(Width / 2, Height / 2))
  else NewValue := Value;
  if not SameValue(FYRadius, NewValue, TEpsilon.Vector) then begin
    FYRadius := NewValue;
    Repaint;
  end;
end;

{**********************************************}
procedure TGosText.SetSides(const Value: TSides);
begin
  if FSides <> Value then begin
    FSides := Value;
    Repaint;
  end;
end;

{***********************************************}
procedure TGosText.OnFontChanged(Sender: TObject);
begin
  FontChanged;
end;

{*******************************}
function TGosText.GetData: TValue;
begin
  Result := Text;
end;

{*********************************************}
procedure TGosText.SetData(const Value: TValue);
begin
  Text := Value.ToString;
end;

{****************************}
procedure TGosText.FontChanged;
begin
  FLayout.BeginUpdate;
  try
    FLayout.WordWrap := WordWrap;
    FLayout.HorizontalAlign := HorzTextAlign;
    FLayout.VerticalAlign := VertTextAlign;
    FLayout.Color := Color;
    FLayout.Font := Font;
    FLayout.Opacity := AbsoluteOpacity;
    FLayout.Trimming := Trimming;
  finally
    FLayout.EndUpdate;
  end;
  //-----
  if FTextSettings.IsAdjustChanged then AdjustSize;
  Repaint;
end;

{**************************}
procedure TGosText.DoRealign;
begin
  //in original delphi source code it's was
  //inherited;
  //AdjustSize;
  //but i think it's must be the oposite !
  //https://quality.embarcadero.com/browse/RSP-15761
  AdjustSize;
  inherited;
end;

{**********************************************}
function TGosText.GetTextSettings: TTextSettings;
begin
  Result := FTextSettings;
end;

{********************************************************}
function TGosText.GetTextSettingsClass: TTextSettingsClass;
begin
  Result := nil;
end;

{************************************************************}
procedure TGosText.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*********************************************************************}
function TGosText.SupportsPaintStage(const Stage: TPaintStage): Boolean;
begin
  Result := Stage in [TPaintStage.All, TPaintStage.Text];
end;

{**********************}
procedure TGosText.Paint;
begin
  FLayout.BeginUpdate;
  try
    FLayout.LayoutCanvas := Self.Canvas;
    FLayout.TopLeft := LocalRect.TopLeft;
    FLayout.Opacity := AbsoluteOpacity;
    FLayout.MaxSize := PointF(LocalRect.Width, LocalRect.Height);
  finally
    FLayout.EndUpdate;
  end;
  FLayout.RenderLayout(Canvas);
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{*************************************}
function TGosText.GetColor: TAlphaColor;
begin
  Result := FTextSettings.FontColor;
end;

{***************************************************}
procedure TGosText.SetColor(const Value: TAlphaColor);
begin
  FTextSettings.FontColor := Value;
end;

{************************************}
function TGosText.GetWordWrap: Boolean;
begin
  Result := FTextSettings.WordWrap;
end;

{**************************************************}
procedure TGosText.SetWordWrap(const Value: Boolean);
begin
  FTextSettings.WordWrap := Value;
end;

{******************************}
function TGosText.GetFont: TFont;
begin
  Result := FTextSettings.Font;
end;

{********************************************}
procedure TGosText.SetFont(const Value: TFont);
begin
  FTextSettings.Font := Value;
end;

{********************************************}
function TGosText.GetHorzTextAlign: TTextAlign;
begin
  Result := FTextSettings.HorzAlign;
end;

{**********************************************************}
procedure TGosText.SetHorzTextAlign(const Value: TTextAlign);
begin
  FTextSettings.HorzAlign := Value;
end;

{********************************************}
function TGosText.GetVertTextAlign: TTextAlign;
begin
  Result := FTextSettings.VertAlign;
end;

{**********************************************************}
procedure TGosText.SetVertTextAlign(const Value: TTextAlign);
begin
  FTextSettings.VertAlign := Value;
end;

{******************************************}
function TGosText.GetTrimming: TTextTrimming;
begin
  Result := FTextSettings.Trimming;
end;

{********************************************************}
procedure TGosText.SetTrimming(const Value: TTextTrimming);
begin
  FTextSettings.Trimming := Value;
end;

{**************************************************}
procedure TGosText.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustSize;
  end;
end;

{***********************}
procedure TGosText.Resize;
begin
  inherited;
  AdjustSize;
end;

{*************************}
{$IF CompilerVersion >= 32} // tokyo
procedure TGosText.DoResized;
begin
  if not FDisableAlign then inherited DoResized
  else fMustCallResized := True;
end;
{$endif}

{***************************}
procedure TGosText.AdjustSize;
var R: TRectF;
    AlignRoot: IAlignRoot;
    LHorzAlign: TTextAlign;
    LVertAlign: TTextAlign;
    LOpacity: Single;
begin
  if (not FDisableAlign) and
     (not (csLoading in ComponentState)) and
     (not (csDestroying in ComponentState)) and
     (not isupdating) and
     (FAutoSize) and
     (Text <> '') and
     (scene <> nil) then begin

    fMustCallResized := False;
    FDisableAlign := True;  // i don't understand why in the original delphi source code they do like this - but i feel lazzy to fully study why so i leave it
                            // this mean that we can't add aligned control inside the TGosText because when we will update the size of the TGosText via adjustsize
                            // then we will not realign all the childs
                            // NOTE: as this fucking FDisableAlign piss me off because no way to resize the control inside the OnResize event (for exemple by changing the
                            //       font size, I introduce fMustCallResized to call DoResized AFTER we release the FDisableAlign (because stupid to call resized when
                            //       FDisableAlign := True
    try

      LHorzAlign := FLayout.HorizontalAlign;
      LVertAlign := FLayout.VerticalAlign;
      LOpacity := FLayout.Opacity;
      try

        //Originally, if WordWrap then the algo take in account the current width of the TGosText
        //to calculate the autosized width (mean the size can be lower than maxwidth if the current width
        //is already lower than maxwidth). problem with that is if we for exemple change the text (or font
        //dimension) of an already calculated TGosText, then it's the old width (that correspond to the
        //previous text/font) that will be taken in account. finally the good way is to alway use the
        //maxwidth if we desir a max width and don't rely on the current width

        //if WordWrap then R := TRectF.Create(0, 0, Min(Width, maxWidth), maxHeight)
        //else R := TRectF.Create(0, 0, maxWidth, MaxHeight);
        if (WordWrap) and
           (Align in [TAlignLayout.Client,
                      TAlignLayout.Contents,
                      TAlignLayout.Top,
                      TAlignLayout.Bottom,
                      TAlignLayout.MostTop,
                      TAlignLayout.MostBottom,
                      TAlignLayout.VertCenter]) then R := TRectF.Create(0, 0, Width, maxHeight)
        else R := TRectF.Create(0, 0, maxWidth, MaxHeight);

        FLayout.BeginUpdate;
        try
          FLayout.TopLeft := R.TopLeft;
          FLayout.MaxSize := PointF(R.Width, R.Height);
          FLayout.Opacity := AbsoluteOpacity;
          FLayout.HorizontalAlign := TTextAlign.Leading;
          FLayout.VerticalAlign := TTextAlign.Leading;
        finally
          FLayout.EndUpdate;
        end;

        R := FLayout.TextRect;

      finally
        FLayout.BeginUpdate;
        try
          FLayout.Opacity := LOpacity;
          FLayout.HorizontalAlign := LHorzAlign;
          FLayout.VerticalAlign := LVertAlign;
        finally
          FLayout.EndUpdate;
        end;
      end;

      //this to take care of the align constraint of the ftextLayout
      if Align in [TAlignLayout.Client,
                   TAlignLayout.Contents,
                   TAlignLayout.Top,
                   TAlignLayout.Bottom,
                   TAlignLayout.MostTop,
                   TAlignLayout.MostBottom,
                   TAlignLayout.VertCenter] then begin
        r.Left := 0;
        r.Width := Width;
      end;
      if Align in [TAlignLayout.Client,
                   TAlignLayout.Contents,
                   TAlignLayout.Left,
                   TAlignLayout.Right,
                   TAlignLayout.MostLeft,
                   TAlignLayout.MostRight,
                   TAlignLayout.HorzCenter] then begin
        r.Top := 0;
        r.height := height;
      end;

      //SetBounds(Position.X, Position.Y, R.Width + R.Left * 2 + FTextSettings.Font.Size / 3, R.Height + R.Top * 2);
      SetBounds(Position.X, Position.Y, R.Width + R.Left * 2, R.Height + R.Top * 2);
      if Supports(Parent, IAlignRoot, AlignRoot) then AlignRoot.Realign;

    finally
      FDisableAlign := False;
    end;

    {$IF CompilerVersion >= 32} // tokyo
    if fMustCallResized then DoResized;
    {$ENDIF}

  end;
end;

{*******************************}
function TGosText.GetText: string;
begin
  Result := FLayout.Text;
end;

{*********************************************}
procedure TGosText.SetText(const Value: string);
begin
  if Text <> Value then begin
    FLayout.LayoutCanvas := Canvas;
    FLayout.Text := Value;
    AdjustSize;
    Repaint;
  end;
end;

{***************************************************}
procedure TGosText.SetParent(const Value: TFmxObject);
begin
  if FParent <> Value then begin
    inherited;
    // stupidly when we do setparent the
    // FisUpdating will be set to the value of the parent BUT without any
    // notifications nor any call to beginupdate :(
    // but when the parent will call endupdate then our EndUpdate will be also called
    if (Value <> nil) and
       (Value <> self) and
       (Value is TControl) then begin
      if TGosControlAccessPrivate(Value).FUpdating > TGosTextLayoutAccessPrivate(Layout).fupdating then begin
        while TGosControlAccessPrivate(Value).FUpdating > TGosTextLayoutAccessPrivate(Layout).fupdating do
          Layout.BeginUpdate;
      end
      else if TGosControlAccessPrivate(Value).FUpdating < TGosTextLayoutAccessPrivate(Layout).fupdating then begin
        while (TGosControlAccessPrivate(Value).FUpdating < TGosTextLayoutAccessPrivate(Layout).fupdating) and
              (TGosControlAccessPrivate(Value).FUpdating > 0) do
          Layout.EndUpdate;
      end;
    end;
  end;
end;

{********************************************}
procedure TGosText.SetNewScene(AScene: IScene);
var aParentControl: Tcontrol;
begin
  inherited SetNewScene(AScene);

  aParentControl := parentControl;
  while aParentControl <> nil do begin
    if aParentControl.IsUpdating then exit
    else aParentControl := aParentControl.parentControl;
  end;

  AdjustSize; // << because before scene was maybe nil so adjustsize returned 0
end;

{*******************************}
procedure TGosText.clearBufBitmap;
begin
  if not doubleBuffered then exit;
  TGosDoubleBufferedTextLayout(Layout).clearBufBitmap;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TGosText.MakeBufBitmap: TTexture;
{$ELSE}
function TGosText.MakeBufBitmap: Tbitmap;
{$ENDIF}
begin
  if not doubleBuffered then exit(nil);
  FLayout.BeginUpdate;
  try
    FLayout.LayoutCanvas := Self.Canvas;  // useless
    FLayout.TopLeft := LocalRect.TopLeft; // useless
    FLayout.Opacity := AbsoluteOpacity;  // useless
    FLayout.MaxSize := PointF(LocalRect.Width, LocalRect.Height);
  finally
    FLayout.EndUpdate;
  end;
  result := TGosDoubleBufferedTextLayout(Layout).MakeBufBitmap;
end;

{******************************************}
function TGosText.GetdoubleBuffered: Boolean;
begin
  result := Layout is TGosDoubleBufferedTextLayout;
end;

{********************************************************}
procedure TGosText.SetdoubleBuffered(const Value: Boolean);
var aText: String;
begin
  if value <> doubleBuffered  then begin
    aText := fLayout.Text;
    GosFreeAndNil(fLayout);
    if value then fLayout := TGosDoubleBufferedTextLayout.Create(nil, self)
    else fLayout := TTextLayoutManager.DefaultTextLayout.Create;
    if fRestoreLayoutUpdateAfterLoaded then Layout.BeginUpdate;
    FontChanged;
    FLayout.Text := aText;
    AdjustSize;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TGosText.GetBufBitmap: TTexture;
{$ELSE}
function TGosText.GetBufBitmap: Tbitmap;
{$ENDIF}
begin
  if not doubleBuffered then exit(nil);
  result := TGosDoubleBufferedTextLayout(Layout).fBufBitmap;
end;

{****************************}
procedure TGosText.BeginUpdate;
begin
  inherited;
  Layout.BeginUpdate;
end;

{**************************}
procedure TGosText.EndUpdate;
begin
  if (FAutoSize) and
     (Text <> '') then begin

    //Originally, if WordWrap then the algo take in account the current width of the TGosText
    //to calculate the autosized width (mean the size can be lower than maxwidth if the current width
    //is already lower than maxwidth). problem with that is if we for exemple change the text (or font
    //dimension) of an already calculated TGosText, then it's the old width (that correspond to the
    //previous text/font) that will be taken in account. finally the good way is to alway use the
    //maxwidth if we desir a max width and don't rely on the current width

    //if WordWrap then Layout.MaxSize := TPointF.Create(Min(Width, maxWidth), maxHeight)
    //else Layout.MaxSize := TPointF.Create(maxWidth, MaxHeight);
    if (WordWrap) and
       (Align in [TAlignLayout.Client,
                  TAlignLayout.Contents,
                  TAlignLayout.Top,
                  TAlignLayout.Bottom,
                  TAlignLayout.MostTop,
                  TAlignLayout.MostBottom,
                  TAlignLayout.VertCenter]) then Layout.MaxSize := TPointF.Create(Width, maxHeight)
    else Layout.MaxSize := TPointF.Create(maxWidth, MaxHeight);

  end
  else Layout.MaxSize := TPointF.Create(width, height);  // << this is important because else when the component is loaded then
                                                         // << we will call DoRenderLayout that will use the original maxsise (ie: 65535, 65535)
                                                         // << and then after when we will paint the control, we will again call DoRenderLayout
                                                         // << but this time with maxsize = aTextControl.size and off course if wordwrap we will
                                                         // << need to redo the bufbitmap
  Layout.EndUpdate;
  inherited; // will call dorealign that will call AdjustSize
end;

{************************************}
function TGosText.TextBreaked: Boolean;
begin
  if not doubleBuffered then exit(false);
  result := (TGosDoubleBufferedTextLayout(fLayout).fbufBitmap <> nil) and
            (TGosDoubleBufferedTextLayout(fLayout).fBufTextBreaked);
end;

{*************************************************}
procedure TGosText.SetMaxWidth(const Value: Single);
begin
  if compareValue(fMaxWidth, Value, Tepsilon.position) <> 0 then begin
    fMaxWidth := Value;
    AdjustSize;
  end;
end;

{**************************************************}
procedure TGosText.SetMaxHeight(const Value: Single);
begin
  if compareValue(fMaxHeight, Value, Tepsilon.position) <> 0 then begin
    fMaxHeight := Value;
    AdjustSize;
  end;
end;

{*****************************************}
function TGosText.IsMaxWidthStored: Boolean;
begin
  result := compareValue(fMaxWidth, 65535, Tepsilon.position) <> 0;
end;

{******************************************}
function TGosText.IsMaxHeightStored: Boolean;
begin
  result := compareValue(fMaxHeight, 65535, Tepsilon.position) <> 0;
end;

{**************************************************}
//unfortunatly the way the beginupdate/endupdate and
//realign work is not very efficient for TGosText.
//because when we do endupdate then we will first
//call endupdate to the most far away childreen, and
//go up like :
//  acontrol1
//    acontrol2
//      aalText1
//then doing acontrol1.endupdate then we will do in this order :
//      aalText1.endupdate => realign and then adjustsize
//    acontrol2.endupdate => realign and then maybe again TalText1.adjustsize
//  acontrol1.endupdate => realign and then maybe again TalText1.adjustsize
//this is a problem because we will calculate several time the bufbitmap
//to mitigate this we can do
//  ALLockTexts(acontrol1);
//  acontrol1.endupdate;
//  ALUnLockTexts(acontrol1);
procedure ALLockTexts(const aParentControl: Tcontrol);
var I: Integer;
begin
  if aParentControl is TGosText then begin
    aParentControl.BeginUpdate;
    exit;
  end;
  for I := 0 to aParentControl.Controls.Count - 1 do
    ALLockTexts(aParentControl.Controls[i]);
end;

{******************************************************}
procedure ALUnLockTexts(const aParentControl: Tcontrol);
var I: Integer;
begin
  if aParentControl is TGosText then begin
    aParentControl.EndUpdate;
    exit;
  end;
  for I := 0 to aParentControl.Controls.Count - 1 do
    ALUnLockTexts(aParentControl.Controls[i]);
end;

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosImage, TGosRectangle, TGosCircle, TGosLine, TGosText, TGosEditView]);
end;

initialization
  RegisterFmxClasses([TGosImage, TGosRectangle, TGosCircle, TGosLine, TGosText]);
  {$IFDEF debug}
  AlDebugImageMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugRectangleMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugCircleMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugLineMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugTextMakeBufBitmapStopWatch := TstopWatch.Create;
  {$endif}

end.
