unit uGosCommon;

interface

uses {$IFDEF IOS}
     iOSapi.Foundation,
     {$ENDIF}
     {$IFDEF MSWINDOWS}
     Winapi.Windows,
     {$ENDIF}
     system.sysutils,
     system.types;

{$IF CompilerVersion < 29} {Delphi XE8}
  {$IF defined(CPUX64)} // The CPU supports the x86-64 instruction set, and is in a 64-bit environment. *New* in XE2/x64
    {$DEFINE CPU64BITS} // The CPU is in a 64-bit environment, such as DCC64.EXE. *New* in XE8
  {$ENDIF}
  {$IF defined(CPUX86)} // 	The CPU supports the x86-64 instruction set, and is in a 64-bit environment. *New* in XE2/x64
    {$DEFINE CPU32BITS} // The CPU is in a 32-bit environment, such as DCC32.EXE. *New* in XE8
  {$ENDIF}
{$ENDIF}

{$IF CompilerVersion <= 25} // xe4
type
  {$SCOPEDENUMS ON}
  THorzRectAlign = (Center, Left, Right);
  TVertRectAlign = (Center, Top, Bottom);
  {$SCOPEDENUMS OFF}
{$IFEND}

type

  TGosPointDType = array [0..1] of Double;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if System.Types.TPointf still having the same implementation and adjust the IFDEF'}
  {$IFEND}
  PGosPointD = ^TGosPointD;
  TGosPointD = record
    class function Create(const AX, AY: Double): TGosPointD; overload; static; inline;
    class function Create(const APoint: TPoint): TGosPointD; overload; static; inline;
    class function Create(const APoint: TpointF): TGosPointD; overload; static; inline;

    class operator Add(const APoint1, APoint2: TGosPointD): TGosPointD;
    class operator Subtract(const APoint1, APoint2: TGosPointD): TGosPointD;
    class operator Equal(const APoint1, APoint2: TGosPointD): Boolean;
    class operator NotEqual(const APoint1, APoint2: TGosPointD): Boolean;
    class operator Implicit(const APoint: TPoint): TGosPointD;
    class operator Negative(const APoint: TGosPointD): TGosPointD;
    class operator Multiply(const APoint1, APoint2: TGosPointD): TGosPointD;
    class operator Multiply(const APoint: TGosPointD; const AFactor: Double): TGosPointD;
    class operator Multiply(const AFactor: Double; const APoint: TGosPointD): TGosPointD;
    class operator Divide(const APoint: TGosPointD; const AFactor: Double): TGosPointD;

    class function PointInCircle(const Point, Center: TGosPointD; const Radius: Integer): Boolean; static; inline;
    /// <summary> Zero point having values of (0, 0). </summary>
    class function Zero: TGosPointD; inline; static;

    function Distance(const APoint: TGosPointD): Double;
    // 3D cross-product with Z = 0
    function CrossProduct(const APoint: TGosPointD): Double;
    function DotProduct(const APoint: TGosPointD): Double; inline;

    procedure Offset(const APoint: TGosPointD); overload; inline;
    procedure Offset(const ADeltaX, ADeltaY: Double); overload; inline;
    procedure Offset(const APoint: TPoint); overload; inline;

    procedure SetLocation(const X, Y: Double); overload; deprecated 'Use ":=" assignment instead';
    procedure SetLocation(const P: TGosPointD); overload; deprecated 'Use ":=" assignment instead';
    procedure SetLocation(const P: TPoint); overload; deprecated 'Use ":=" assignment instead';
    function Subtract(const Point: TGosPointD): TGosPointD; overload; deprecated 'Use TGosPointD.Offset instead';
    function Subtract(const Point: TPoint): TGosPointD; overload; deprecated 'Use TGosPointD.Offset instead';
    function Add(const Point: TGosPointD): TGosPointD; overload; deprecated 'Use TGosPointD.Offset instead';
    function Add(const Point: TPoint): TGosPointD; overload; deprecated 'Use TGosPointD.Offset instead';
    function Scale(const AFactor: Double): TGosPointD; deprecated;
    function EqualsTo(const Point: TGosPointD; const Epsilon: Double = 0): Boolean;

    function IsZero: Boolean;
    function Ceiling: TPoint;
    function Truncate: TPoint;
    function Round: TPoint;
    /// <summary> Rounds the current point to the specified scale value
    /// <param name="AScale"> The scale of scene </param>
    /// <param name="APlaceBetweenPixels"> If <c>True</c> (by default) the resulting point moves to half scale </param>
    /// </summary>
    /// <returns> The current point after transformation </returns>
    function SnapToPixel(const AScale: Double; const APlaceBetweenPixels: Boolean = True): TGosPointD;

    function Normalize: TGosPointD;
    function Length: Double;
    function Rotate(const AAngle: Double): TGosPointD;
    function Reflect(const APoint: TGosPointD): TGosPointD; inline;
    function MidPoint(const APoint: TGosPointD): TGosPointD; inline;
    function AngleCosine(const APoint: TGosPointD): Double;
    function Angle(const APoint: TGosPointD): Double;

    function Abs: Double;

    case Integer of
      0: (V: TGosPointDType;);
      1: (X: Double;
          Y: Double;);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if System.Types.TSizef still having the same implementation and adjust the IFDEF'}
  {$IFEND}
  PGosSizeD = ^TGosSizeD;
  TGosSizeD = record
    cx: Double;
    cy: Double;
  public
    constructor Create(P: TGosSizeD); overload;
    constructor Create(const X, Y: Double); overload;
    // operator overloads
    class operator Equal(const Lhs, Rhs: TGosSizeD): Boolean;
    class operator NotEqual(const Lhs, Rhs: TGosSizeD): Boolean;
    class operator Add(const Lhs, Rhs: TGosSizeD): TGosSizeD;
    class operator Subtract(const Lhs, Rhs: TGosSizeD): TGosSizeD;

    class operator Implicit(const Size: TGosSizeD): TGosPointD;
    class operator Implicit(const Point: TGosPointD): TGosSizeD;
    class operator Implicit(const Size: TSize): TGosSizeD;

    function Ceiling: TSize;
    function Truncate: TSize;
    function Round: TSize;

    // metods
    function Add(const Point: TGosSizeD): TGosSizeD;
    function Subtract(const Point: TGosSizeD): TGosSizeD;
    function Distance(const P2: TGosSizeD): Double;
    function IsZero: Boolean;
    /// <summary>Returns size with swapped width and height</summary>
    function SwapDimensions: TGosSizeD;
    // properties
    property Width: Double read cx write cx;
    property Height: Double read cy write cy;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if System.Types.TRectf still having the same implementation and adjust the IFDEF'}
  {$IFEND}
  PGosRectD = ^TGosRectD;
  TGosRectD = record
  private
    function GetWidth: Double;
    procedure SetWidth(const Value: Double);
    function GetHeight: Double;
    procedure SetHeight(const Value: Double);
    function GetSize: TGosSizeD;
    procedure SetSize(const Value: TGosSizeD);
    function GetLocation: TGosPointD;
  public
    constructor Create(const Origin: TGosPointD); overload;                               // empty rect at given origin
    constructor Create(const Origin: TGosPointD; const Width, Height: Double); overload; // at TPoint of origin with width and height
    constructor Create(const Left, Top, Right, Bottom: Double); overload;              // at x, y with width and height
    constructor Create(const P1, P2: TGosPointD; Normalize: Boolean = False); overload;  // with corners specified by p1 and p2
    constructor Create(const R: TGosRectD; Normalize: Boolean = False); overload;
    constructor Create(const R: TRect; Normalize: Boolean = False); overload;

    // operator overloads
    class operator Equal(const Lhs, Rhs: TGosRectD): Boolean;
    class operator NotEqual(const Lhs, Rhs: TGosRectD): Boolean;
    class operator Implicit(const Source: TRect): TGosRectD;
    {$IFNDEF ALDPK} // << else i receive Unsupported language feature: 'operator explicit'
    class operator Explicit(const Source: TGosRectD): TRect;
    {$ENDIF}

    // union of two rectangles
    class operator Add(const Lhs, Rhs: TGosRectD): TGosRectD;

    // intersection of two rectangles
    class operator Multiply(const Lhs, Rhs: TGosRectD): TGosRectD;

    class function Empty: TGosRectD; inline; static;

    { This method is to be deprecated. It stretches current rectangle into designated area similarly to FitInto,
      but only when current rectangle is bigger than the area; otherwise, it only centers it. }
    function Fit(const BoundsRect: TGosRectD): Double; // deprecated 'Please consider using FitInto instead.';

    { Stretches current rectangle into the designated area, preserving aspect ratio. Note that unlike Fit, when designated
      area is bigger than current rectangle, the last one will be stretched to fill designated area (while Fit would only
      center it). }
    function FitInto(const ADesignatedArea: TGosRectD; out Ratio: Double): TGosRectD; overload;
    function FitInto(const ADesignatedArea: TGosRectD): TGosRectD; overload;

    /// <summary> Places the rectangle at center of designated area without scaling </summary>
    function CenterAt(const ADesignatedArea: TGosRectD): TGosRectD;

    /// <summary> This method places the rectangle inside the designated area. If the rectangle is greater
    /// than the designated area then the source rectangle is scaled with aspect ratio.
    /// </summary>
    /// <param name="ADesignatedArea"> The place in which the current rectangle will be placed </param>
    /// <param name="AHorzAlign"> The horizontal arrangement, if the width of the rectangle is smaller than the width
    /// of the designated area. The <b>Center</b> by default </param>
    /// <param name="AVertAlign"> The vertical arrangement, if the height of the rectangle is smaller than the height
    /// of the designated area. The <b>Center</b> by default </param>
    /// <returns> The current rectangle after transformation </returns>
    function PlaceInto(const ADesignatedArea: TGosRectD; const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;
      const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TGosRectD;

    /// <summary> Rounds the location and size of the current rectangle to the specified value
    /// <param name="AScale"> The scale of scene </param>
    /// <param name="APlaceBetweenPixels"> If <c>True</c> (by default) the resulting rectangle moves to half scale </param>
    /// </summary>
    /// <returns> The current rectangle after transformation </returns>
    function SnapToPixel(const AScale: Double; const APlaceBetweenPixels: Boolean = True): TGosRectD;

    //makes sure TopLeft is above and to the left of BottomRight
    procedure NormalizeRect;

    //returns true if left = right or top = bottom
    function IsEmpty: Boolean;

    //returns true if the point is inside the rect
    function Contains(const Pt: TGosPointD): Boolean; overload;
    function Contains(const Pt: TPointf): Boolean; overload;

    // returns true if the rect encloses R completely
    function Contains(const R: TGosRectD): Boolean; overload;

    // returns true if any part of the rect covers R
    function IntersectsWith(const R: TGosRectD): Boolean;

    // computes an intersection of R1 and R2
    class function Intersect(const R1: TGosRectD; const R2: TGosRectD): TGosRectD; overload; static;

    // replaces current rectangle with its intersection with R
    procedure Intersect(const R: TGosRectD); overload;

    // computes a union of R1 and R2
    class function Union(const R1: TGosRectD; const R2: TGosRectD): TGosRectD; overload; static;

    // replaces current rectangle with its union with R
    procedure Union(const R: TGosRectD); overload;

    // creates a minimal rectangle that contains all points from array Points
    class function Union(const Points: Array of TGosPointD): TGosRectD; overload; static;

    // offsets the rectangle origin relative to current position
    procedure Offset(const DX, DY: Double); overload;
    procedure Offset(const Point: TGosPointD); overload;

    // sets new origin
    procedure SetLocation(const X, Y: Double); overload;
    procedure SetLocation(const Point: TGosPointD); overload;

    // inflate by DX and DY
    procedure Inflate(const DX, DY: Double); overload;

    // inflate in all directions
    procedure Inflate(const DL, DT, DR, DB: Double); overload;

    //returns the center point of the rectangle;
    function CenterPoint: TGosPointD;

    function Ceiling: TRect;
    function Truncate: TRect;
    function Round: TRect;

    function EqualsTo(const R: TGosRectD; const Epsilon: Double = 0): Boolean;

    {
    function SplitRect(SplitType: TSplitRectType; Size: Integer): TRect; overload;
    function SplitRect(SplitType: TSplitRectType; Percent: Double): TRect; overload;
    }

    // changing the width is always relative to Left;
    property Width: Double read GetWidth write SetWidth;
    // changing the Height is always relative to Top
    property Height: Double read GetHeight write SetHeight;

    property Size: TGosSizeD read GetSize write SetSize;

    property Location: TGosPointD read GetLocation write SetLocation;

  case Integer of
    0: (Left, Top, Right, Bottom: Double);
    1: (TopLeft, BottomRight: TGosPointD);
  end;

{~~~~~~~~~~~~~~~~~~~~~~~~}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if functions below implemented in System.Types still having the same implementation and adjust the IFDEF'}
{$IFEND}
function GosRectWidth(const Rect: TRect): Integer; inline; overload;
function GosRectWidth(const Rect: TRectF): Single; inline; overload;
function GosRectWidth(const Rect: TGosRectD): Double; inline; overload;
function GosRectHeight(const Rect: TRect): Integer; inline; overload;
function GosRectHeight(const Rect: TRectF): Single; inline; overload;
function GosRectHeight(const Rect: TGosRectD): Double; inline; overload;
function GosOffsetRect(var R: TRect; DX, DY: Integer): Boolean; inline; overload;
function GosOffsetRect(var R: TRectf; DX, DY: single): Boolean; inline; overload;
function GosOffsetRect(var R: TGosRectD; DX, DY: double): Boolean; overload;
function GosRectCenter(var R: TRect; const Bounds: TRect): TRect; inline; overload;
function GosRectCenter(var R: TRectf; const Bounds: TRectf): TRectf; inline; overload;
function GosRectCenter(var R: TGosRectD; const Bounds: TGosRectD): TGosRectD; overload;
function GosIntersectRect(out Rect: TGosRectD; const R1, R2: TGosRectD): Boolean;
function GosUnionRect(out Rect: TGosRectD; const R1, R2: TGosRectD): Boolean;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function GosRectFitInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF; out Ratio: Single): TRectF; overload;
function GosRectFitInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF): TRectF; overload;
function GosRectFitInto(const R: TRectf; const Bounds: TRectF; out Ratio: Single): TRectF; overload;
function GosRectFitInto(const R: TRectf; const Bounds: TRectF): TRectF; overload;
function GosRectPlaceInto(const R: TRectf;
                         const Bounds: TRectf;
                         const CenterAt: TpointF;
                         out Ratio: Single): TRectF; overload;
function GosRectPlaceInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF): TRectF; overload;
function GosRectPlaceInto(const R: TRectf;
                         const Bounds: TRectF;
                         const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;
                         const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF; overload;

type

  {$IFNDEF NEXTGEN}
  EGosException = class(Exception)
  public
    constructor Create(const Msg: AnsiString);
    constructor CreateFmt(const Msg: ansistring; const Args: array of const);
  end;
  {$ENDIF}
  EGosExceptionU = class(Exception);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
var GosCallStackCustomLogsMaxCount: integer = 50;
procedure GosAddCallStackCustomLogU(Const aLog: String);
function GosGetCallStackCustomLogsU(Const aAppendTimeStamp: boolean = True): String;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Type TGosLogType = (VERBOSE, DEBUG, INFO, WARN, ERROR, ASSERT);
procedure GosLog(Const Tag: String; Const msg: String; const _type: TGosLogType = TGosLogType.INFO);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
type TGosCustomDelayedFreeObjectProc = procedure(var aObject: Tobject) of object;
var GosCustomDelayedFreeObjectProc: TGosCustomDelayedFreeObjectProc;
{$IFDEF DEBUG}
var GosFreeAndNilRefCountWarn: boolean;
threadvar GosCurThreadFreeAndNilNORefCountWarn: boolean;
type TGosFreeAndNilCanRefCountWarnProc = function(const aObject: Tobject): boolean of object;
var GosFreeAndNilCanRefCountWarnProc: TGosFreeAndNilCanRefCountWarnProc;
{$ENDIF}
Procedure GosFreeAndNil(var Obj; const adelayed: boolean = false); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
Procedure GosFreeAndNil(var Obj; const adelayed: boolean; const aRefCountWarn: Boolean); overload; {$IFNDEF DEBUG}inline;{$ENDIF}

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Function GosBoolToInt(Value:Boolean):Integer;
Function GosIntToBool(Value:integer):boolean;
Function GosMediumPos(LTotal, LBorder, LObject : integer):Integer;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function  GosIfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer; overload; inline;
function  GosIfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64 = 0): Int64; overload; inline;
function  GosIfThen(AValue: Boolean; const ATrue: UInt64; const AFalse: UInt64 = 0): UInt64; overload; inline;
function  GosIfThen(AValue: Boolean; const ATrue: Single; const AFalse: Single = 0): Single; overload; inline;
function  GosIfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0): Double; overload; inline;
function  GosIfThen(AValue: Boolean; const ATrue: Extended; const AFalse: Extended = 0): Extended; overload; inline;
{$IFNDEF NEXTGEN}
function  GosIfThen(AValue: Boolean; const ATrue: AnsiString; AFalse: AnsiString = ''): AnsiString; overload; inline;
{$ENDIF}
function  GosIfThenU(AValue: Boolean; const ATrue: String; AFalse: String = ''): String; overload; inline;

{$IFDEF MSWINDOWS}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if EnumDynamicTimeZoneInformation/SystemTimeToTzSpecificLocalTimeEx/TzSpecificLocalTimeToSystemTimeEx are still not declared in Winapi.Windows and adjust the IFDEF'}
{$ENDIF}
{$WARNINGS OFF}
function EnumDynamicTimeZoneInformation(dwIndex: DWORD; lpTimeZoneInformation: PDynamicTimeZoneInformation): DWORD; stdcall; external advapi32 delayed;
function SystemTimeToTzSpecificLocalTimeEx(lpTimeZoneInformation: PDynamicTimeZoneInformation; var lpUniversalTime, lpLocalTime: TSystemTime): BOOL; stdcall; external Kernel32 delayed;
function TzSpecificLocalTimeToSystemTimeEx(lpTimeZoneInformation: PDynamicTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall; external Kernel32 delayed;
{$WARNINGS ON}
Function GosGetDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
Function GosGetDynamicTimeZoneInformation(const aTimeZoneKeyName: String): TDynamicTimeZoneInformation;
{$ENDIF}
function GosLocalDateTimeToUTC(Const aLocalDateTime: TDateTime): TdateTime; overload;
function GosUTCDateTimeToLocal(Const aUTCDateTime: TDateTime): TdateTime; overload;
{$IFDEF MSWINDOWS}
Function GosLocalDateTimeToUTC(const aTimeZoneKeyName: String; aLocalDateTime: TdateTime): TdateTime; overload;
Function GosLocalDateTimeToUTC(const aTimeZoneInformation: TDynamicTimeZoneInformation; aLocalDateTime: TdateTime): TdateTime; overload;
Function GosUTCDateTimeToLocal(const aTimeZoneKeyName: String; aUTCDateTime: TdateTime): TdateTime; overload;
Function GosUTCDateTimeToLocal(const aTimeZoneInformation: TDynamicTimeZoneInformation; aUTCDateTime: TdateTime): TdateTime; overload;
{$ENDIF}
{$IFDEF IOS}
function GosNSDateToUTCDateTime(const ADateTime: NSDate): TDateTime;
{$ENDIF}
function GosUTCNow: TDateTime;
function GosUnixMsToDateTime(const aValue: Int64): TDateTime;
function GosDateTimeToUnixMs(const aValue: TDateTime): Int64;
Function GosInc(var x: integer; Count: integer): Integer;
var GosMove: procedure (const Source; var Dest; Count: NativeInt);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
const GosMAXUInt64: UInt64 = 18446744073709551615;
      GosMAXInt64: Int64 = 9223372036854775807;
      GosMAXUInt: cardinal = 4294967295;
      GosMAXInt: int32 = 2147483647;
      GosNullDate = 0; // There are no TDateTime values from –1 through 0
                      // dt := -0.5;
                      // writeln(formatFloat('0.0', dt));                    => -0.5
                      // writeln(DateTimeToStr(dt));                         => 1899/12/30 12:00:00.000
                      //
                      // dt := encodedatetime(1899,12,30,12,00,00,000);
                      // writeln(formatFloat('0.0', dt));                    => 0.5
                      // writeln(DateTimeToStr(dt));                         => 1899/12/30 12:00:00.000
                      //
                      // also -0.5 have the advantage to be in the form
                      // m*2^e (-1*2^-1) with mean we don't need to use
                      // samevalue to compare
                      // https://stackoverflow.com/questions/41779801/single-double-and-precision
                      //
                      // but finally -0.5 have a big drawback, if you convert it to string and back
                      // to datetime then you will obtain 0.5 ! same if you convert it to unix and
                      // back to datetime :( so i decide that 0 if more suitable than -0.5

{$IFNDEF NEXTGEN}

//
// Taken from https://github.com/synopse/mORMot.git
// https://synopse.info
// http://mormot.net
//

{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if https://github.com/synopse/mORMot.git SynCommons.pas was not updated from references\mORMot\SynCommons.pas and adjust the IFDEF'}
{$ENDIF}

type
  /// the potential features, retrieved from an Intel CPU
  // - see https://en.wikipedia.org/wiki/CPUID#EAX.3D1:_Processor_Info_and_Feature_Bits
  // - is defined on all platforms, since an ARM desktop could browse Intel logs
  TGosIntelCpuFeature =
   ( { CPUID 1 in EDX }
   cfFPU, cfVME, cfDE, cfPSE, cfTSC, cfMSR, cfPAE, cfMCE,
   cfCX8, cfAPIC, cf_d10, cfSEP, cfMTRR, cfPGE, cfMCA, cfCMOV,
   cfPAT, cfPSE36, cfPSN, cfCLFSH, cf_d20, cfDS, cfACPI, cfMMX,
   cfFXSR, cfSSE, cfSSE2, cfSS, cfHTT, cfTM, cfIA64, cfPBE,
   { CPUID 1 in ECX }
   cfSSE3, cfCLMUL, cfDS64, cfMON, cfDSCPL, cfVMX, cfSMX, cfEST,
   cfTM2, cfSSSE3, cfCID, cfSDBG, cfFMA, cfCX16, cfXTPR, cfPDCM,
   cf_c16, cfPCID, cfDCA, cfSSE41, cfSSE42, cfX2A, cfMOVBE, cfPOPCNT,
   cfTSC2, cfAESNI, cfXS, cfOSXS, cfAVX, cfF16C, cfRAND, cfHYP,
   { extended features CPUID 7 in EBX, ECX, DL }
   cfFSGS, cf_b01, cfSGX, cfBMI1, cfHLE, cfAVX2, cf_b06, cfSMEP,
   cfBMI2, cfERMS, cfINVPCID, cfRTM, cfPQM, cf_b13, cfMPX, cfPQE,
   cfAVX512F, cfAVX512DQ, cfRDSEED, cfADX, cfSMAP, cfAVX512IFMA, cfPCOMMIT, cfCLFLUSH,
   cfCLWB, cfIPT, cfAVX512PF, cfAVX512ER, cfAVX512CD, cfSHA, cfAVX512BW, cfAVX512VL,
   cfPREFW1, cfAVX512VBMI, cfUMIP, cfPKU, cfOSPKE, cf_c05, cfAVX512VBMI2, cf_c07,
   cfGFNI, cfVAES, cfVCLMUL, cfAVX512NNI, cfAVX512BITALG, cf_c13, cfAVX512VPC, cf_c15,
   cf_cc16, cf_c17, cf_c18, cf_c19, cf_c20, cf_c21, cfRDPID, cf_c23,
   cf_c24, cf_c25, cf_c26, cf_c27, cf_c28, cf_c29, cfSGXLC, cf_c31,
   cf_d0, cf_d1, cfAVX512NNIW, cfAVX512MAS, cf_d4, cf_d5, cf_d6, cf_d7);

  /// all features, as retrieved from an Intel CPU
  TGosIntelCpuFeatures = set of TGosIntelCpuFeature;

var
  /// the available CPU features, as recognized at program startup
  GosCpuFeatures: TGosIntelCpuFeatures;

procedure GosInitCpuFeatures;

{$ENDIF}

implementation

uses system.Classes,
     system.math,
     system.generics.collections,
     {$IF defined(ANDROID)}
     Androidapi.JNI.JavaTypes,
     Androidapi.Helpers,
     uGosAndroidApi,
     {$ENDIF}
     {$IF defined(IOS)}
     Macapi.Helpers,
     {$ENDIF}
     system.DateUtils,
     uGosString;

type
  _TGosCallStackCustomLogU = record
    TimeStamp: TDateTime;
    log: String;
  end;

var
  _GosCallStackCustomLogsU: TList<_TGosCallStackCustomLogU>;
  _GosCallStackCustomLogsUCurrentIndex: integer = -1;

{***********************************************}
function GosRectWidth(const Rect: TRect): Integer;
begin
  Result := RectWidth(Rect);
end;

{***********************************************}
function GosRectWidth(const Rect: TRectF): Single;
begin
  Result := RectWidth(Rect);
end;

{*************************************************}
function GosRectWidth(const Rect: TGosRectD): Double;
begin
  Result := Rect.Right - Rect.Left;
end;

{************************************************}
function GosRectHeight(const Rect: TRect): Integer;
begin
  Result := RectHeight(Rect);
end;

{************************************************}
function GosRectHeight(const Rect: TRectF): Single;
begin
  Result := RectHeight(Rect);
end;

{**************************************************}
function GosRectHeight(const Rect: TGosRectD): Double;
begin
  Result := Rect.Bottom - Rect.Top;
end;

{**************************************************************}
function GosRectCenter(var R: TRect; const Bounds: TRect): TRect;
begin
  result := RectCenter(R, Bounds);
end;

{*****************************************************************}
function GosRectCenter(var R: TRectf; const Bounds: TRectf): TRectf;
begin
  result := RectCenter(R, Bounds);
end;

{***********************************************************************}
function GosRectCenter(var R: TGosRectD; const Bounds: TGosRectD): TGosRectD;
begin
  GosOffsetRect(R, -R.Left, -R.Top);
  GosOffsetRect(R, (GosRectWidth(Bounds)/2 - GosRectWidth(R)/2), (GosRectHeight(Bounds)/2 - GosRectHeight(R)/2));
  GosOffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;

{************************************************************}
function GosOffsetRect(var R: TRect; DX, DY: Integer): Boolean;
begin
  result := OffsetRect(R, DX, DY);
end;

{************************************************************}
function GosOffsetRect(var R: TRectf; DX, DY: single): Boolean;
begin
  result := system.types.OffsetRect(R, DX, DY);
end;

{**************************************************************}
function GosOffsetRect(var R: TGosRectD; DX, DY: double): Boolean;
begin
  if @R <> nil then // Test to increase compatiblity with Windows
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
    Result := True;
  end
  else
    Result := False;
end;

{****************************************************************************}
function GosIntersectRect(out Rect: TGosRectD; const R1, R2: TGosRectD): Boolean;
var
  tmpRect: TGosRectD;
begin
  tmpRect := R1;
  if R2.Left > R1.Left then tmpRect.Left := R2.Left;
  if R2.Top > R1.Top then tmpRect.Top := R2.Top;
  if R2.Right < R1.Right then tmpRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then tmpRect.Bottom := R2.Bottom;
  Result := not tmpRect.IsEmpty;
  if not Result then
  begin
    tmpRect.Top := 0.0;
    tmpRect.Bottom := 0.0;
    tmpRect.Left := 0.0;
    tmpRect.Right := 0.0;
  end;
  Rect := tmpRect;
end;

{************************************************************************}
function GosUnionRect(out Rect: TGosRectD; const R1, R2: TGosRectD): Boolean;
var
  tmpRect: TGosRectD;
begin
  tmpRect := R1;
  if not R2.IsEmpty then
  begin
    if R2.Left < R1.Left then tmpRect.Left := R2.Left;
    if R2.Top < R1.Top then tmpRect.Top := R2.Top;
    if R2.Right > R1.Right then tmpRect.Right := R2.Right;
    if R2.Bottom > R1.Bottom then tmpRect.Bottom := R2.Bottom;
  end;
  Result := not tmpRect.IsEmpty;
  if not Result then
  begin
    tmpRect.Top :=0.0;
    tmpRect.Bottom := 0.0;
    tmpRect.Left := 0.0;
    tmpRect.Right := 0.0;
  end;
  Rect := tmpRect;
end;

{***********************************************************************************************}
//Resizes the current rectangle, preserving the current rectangle proportions, to best fit in the
//bounds rectangle, and returns the scaled rectangle centered in bounds.
//FitInto implements the following functionality:
// * If any of the current rectangle dimensions is greater than the corresponding dimension of the bounds
//   rectangle, then FitInto scales down the current rectangle to fit into bounds. The scaled rectangle is centered in
//   the bounds rectangle at CenterAt and the obtained scaled and centered rectangle is returned.
// * If both width and height of the current rectangle dimensions is smaller than the corresponding dimensions of the
//   bounds rectangle, then FitInto stretches the current rectangle to best fit into bounds. The stretched
//   rectangle is centered in the bounds rectangle at CenterAt and the obtained stretched and centered rectangle is returned.
// * If any of the bounds dimensions is zero then FitInto returns the current rectangle and sets Ratio equals to 1.
//Ratio is the implemented scaling ratio.
//CenterAt is where we need to center the result in the bounds (ex: center the result on a face instead of the middle of the bounds)
//if center contain negative value then it's indicate percentage
function GosRectFitInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF; out Ratio: Single): TRectF;
begin

  if (Bounds.Width <= 0) or (Bounds.Height <= 0) then
  begin
    Ratio := 1;
    Exit(R);
  end;

  if (R.Width / Bounds.Width) > (R.Height / Bounds.Height) then
    Ratio := R.Width / Bounds.Width
  else
    Ratio := R.Height / Bounds.Height;

  if Ratio = 0 then
    Exit(R)
  else
  begin

    Result := TRectF.Create(0, 0, R.Width / Ratio, R.Height / Ratio);

    system.types.OffsetRect(Result, -Result.Left, -Result.Top);
    if (CenterAt.X < 0) or (CenterAt.y < 0) then system.types.OffsetRect(Result, max(0, (((Bounds.Width) / 100) * -CenterAt.x) - (Result.Width / 2)), max(0, (((Bounds.Height) / 100) * -CenterAt.Y) - (Result.height / 2)))
    else system.types.OffsetRect(Result, max(0, CenterAt.x - (Result.Width / 2)), max(0, CenterAt.y - (Result.height / 2)));
    system.types.OffsetRect(Result, -max(0, Result.Right - Bounds.Width), -max(0, Result.bottom - Bounds.height));
    system.types.OffsetRect(Result, Bounds.Left, Bounds.Top);

  end;

end;

{*********************************************************************************************}
function GosRectFitInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF): TRectF;
var
  Ratio: Single;
begin
  Result := GosRectFitInto(R, Bounds, CenterAt, Ratio);
end;

{******************************************************************************************************************}
//this is the same as TRectf.fitInto but it is here for old delphi version (like xe4) with don't have it implemented
function GosRectFitInto(const R: TRectf; const Bounds: TRectF; out Ratio: Single): TRectF;
begin
  if (Bounds.Width <= 0) or (Bounds.Height <= 0) then
  begin
    Ratio := 1;
    Exit(R);
  end;

  if (R.Width / Bounds.Width) > (R.Height / Bounds.Height) then
    Ratio := R.Width / Bounds.Width
  else
    Ratio := R.Height / Bounds.Height;

  if Ratio = 0 then
    Exit(R)
  else
  begin
    Result := TRectF.Create(0, 0, R.Width / Ratio, R.Height / Ratio);
    RectCenter(Result, Bounds);
  end;
end;

{******************************************************************************************************************}
//this is the same as TRectf.fitInto but it is here for old delphi version (like xe4) with don't have it implemented
function GosRectFitInto(const R: TRectf; const Bounds: TRectF): TRectF;
var
  Ratio: Single;
begin
  Result := GosRectFitInto(R, Bounds, Ratio);
end;

{**************************************************************************************************************}
//If any dimension of the current rectangle is greater than the corresponding dimension of the Bounds rectangle,
//then the current rectangle is scaled down to best fit the Bounds rectangle. The obtained rectangle is aligned in Bounds.
//PlaceInto implements the following behavior:
// * If the width or height of the current rectangle is greater than the corresponding dimension of Bounds.
//   Then PlaceInto scales down the current rectangle (preserving the current rectangle proportions – the ratio between the width
//   and height) to fit in the Bounds rectangle and centers the scaled rectangle in Bounds at CenterAt.
// * Otherwise, PlaceInto just center the current rectangle in the Bounds rectangle according to CenterAt
// * PlaceInto returns the current rectangle if any of the Bounds dimensions is zero.
function GosRectPlaceInto(const R: TRectf;
                         const Bounds: TRectf;
                         const CenterAt: TpointF; // << this is used only when we need to fit the result
                         out Ratio: Single): TRectF;
begin

  if (R.Width > Bounds.Width) or (R.Height > Bounds.Height) then Result := GosRectFitInto(R, Bounds, CenterAt, Ratio)
  else begin

    Result := R;
    system.types.OffsetRect(Result, -Result.Left, -Result.Top);
    if (CenterAt.X < 0) or (CenterAt.y < 0) then system.types.OffsetRect(Result, max(0, (((Bounds.Width) / 100) * -CenterAt.x) - (Result.Width / 2)), max(0, (((Bounds.Height) / 100) * -CenterAt.Y) - (Result.height / 2)))
    else system.types.OffsetRect(Result, max(0, CenterAt.x - (Result.Width / 2)), max(0, CenterAt.y - (Result.height / 2)));
    system.types.OffsetRect(Result, -max(0, Result.Right - Bounds.Width), -max(0, Result.bottom - Bounds.height));
    system.types.OffsetRect(Result, Bounds.Left, Bounds.Top);

  end;

end;

{***********************************************************************************************}
function GosRectPlaceInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF): TRectF;
var
  Ratio: Single;
begin
  Result := GosRectPlaceInto(R, Bounds, CenterAt, Ratio);
end;

{********************************************************************************************************************}
//this is the same as TRectf.PlaceInto but it is here for old delphi version (like xe4) with don't have it implemented
function GosRectPlaceInto(const R: TRectf;
                         const Bounds: TRectF;
                         const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;
                         const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF;
var
  LLocation: TPointF;
begin
  Result := R;
  if (R.Width > Bounds.Width) or (R.Height > Bounds.Height) then
    Result := GosRectFitInto(Result, Bounds);
 case AHorzAlign of
   THorzRectAlign.Center: LLocation.X := (Bounds.Left + Bounds.Right - Result.Width) / 2;
   THorzRectAlign.Left: LLocation.X := Bounds.Left;
   THorzRectAlign.Right: LLocation.X := Bounds.Right - Result.Width;
 end;
 case AVertAlign of
   TVertRectAlign.Center: LLocation.Y := (Bounds.Top + Bounds.Bottom - Result.Height) / 2;
   TVertRectAlign.Top: LLocation.Y := Bounds.Top;
   TVertRectAlign.Bottom: LLocation.Y := Bounds.Bottom - Result.Height;
 end;
 Result.SetLocation(LLocation);
end;

{***************************************************************}
class function TGosPointD.Create(const AX, AY: Double): TGosPointD;
begin
  Result.X := AX;
  Result.Y := AY;
end;

{***************************************************************}
class function TGosPointD.Create(const APoint: TPoint): TGosPointD;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

{****************************************************************}
class function TGosPointD.Create(const APoint: TPointF): TGosPointD;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

{*************************************************************************}
class operator TGosPointD.Add(const APoint1, APoint2: TGosPointD): TGosPointD;
begin
  Result.X := APoint1.X + APoint2.X;
  Result.Y := APoint1.Y + APoint2.Y;
end;

{******************************************************************************}
class operator TGosPointD.Subtract(const APoint1, APoint2: TGosPointD): TGosPointD;
begin
  Result.X := APoint1.X - APoint2.X;
  Result.Y := APoint1.Y - APoint2.Y;
end;

{*************************************************************************}
class operator TGosPointD.Equal(const APoint1, APoint2: TGosPointD): Boolean;
begin
  Result := SameValue(APoint1.X, APoint2.X) and SameValue(APoint1.Y, APoint2.Y);
end;

{**********************************************************************************}
function TGosPointD.EqualsTo(const Point: TGosPointD; const Epsilon: Double): Boolean;
begin
  Result := SameValue(X, Point.X, Epsilon) and SameValue(Y, Point.Y, Epsilon);
end;

{****************************************************************************}
class operator TGosPointD.NotEqual(const APoint1, APoint2: TGosPointD): Boolean;
begin
  Result := not (APoint1 = APoint2);
end;

{*****************************************************************}
class operator TGosPointD.Implicit(const APoint: TPoint): TGosPointD;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

{********************************************************************}
class operator TGosPointD.Negative(const APoint: TGosPointD): TGosPointD;
begin
  Result.X := - APoint.X;
  Result.Y := - APoint.Y;
end;

{******************************************************************************}
class operator TGosPointD.Multiply(const APoint1, APoint2: TGosPointD): TGosPointD;
begin
  Result.X := APoint1.X * APoint2.X;
  Result.Y := APoint1.Y * APoint2.Y;
end;

{*******************************************************************************************}
class operator TGosPointD.Multiply(const APoint: TGosPointD; const AFactor: Double): TGosPointD;
begin
  Result.X := APoint.X * AFactor;
  Result.Y := APoint.Y * AFactor;
end;

{*******************************************************************************************}
class operator TGosPointD.Multiply(const AFactor: Double; const APoint: TGosPointD): TGosPointD;
begin
  Result.X := AFactor * APoint.X;
  Result.Y := AFactor * APoint.Y;
end;

{*****************************************************************************************}
class operator TGosPointD.Divide(const APoint: TGosPointD; const AFactor: Double): TGosPointD;
var
  InvFactor: Double;
begin
  if AFactor <> 0 then
  begin
    InvFactor := 1 / AFactor;

    Result.X := APoint.X * InvFactor;
    Result.Y := APoint.Y * InvFactor;
  end else
    Result := APoint;
end;

{********************************************************}
function TGosPointD.Add(const Point: TGosPointD): TGosPointD;
begin
  Result.X := Self.X + Point.X;
  Result.Y := Self.Y + Point.Y;
end;

{*****************************************************}
function TGosPointD.Add(const Point: TPoint): TGosPointD;
begin
  Result.X := Self.X + Point.X;
  Result.Y := Self.Y + Point.Y;
end;

{***********************************************************}
function TGosPointD.Distance(const APoint: TGosPointD): Double;
begin
  Result := (APoint - Self).Length;
end;

{***************************************************************}
function TGosPointD.CrossProduct(const APoint: TGosPointD): Double;
begin
  Result := Self.X * APoint.Y - Self.Y * APoint.X;
end;

{*************************************************************}
function TGosPointD.DotProduct(const APoint: TGosPointD): Double;
begin
  Result := (Self.X * APoint.X) + (Self.Y * APoint.Y);
end;

{**************************************************}
procedure TGosPointD.Offset(const APoint: TGosPointD);
begin
  Self := Self + APoint;
end;

{*********************************************************}
procedure TGosPointD.Offset(const ADeltaX, ADeltaY: Double);
begin
  Self.Offset(TGosPointD.Create(ADeltaX, ADeltaY));
end;

{***********************************************}
procedure TGosPointD.Offset(const APoint: TPoint);
begin
  Self.Offset(TGosPointD.Create(APoint));
end;

{*****************************************************************************************************}
class function TGosPointD.PointInCircle(const Point, Center: TGosPointD; const Radius: Integer): Boolean;
begin
  Result := Point.Distance(Center) <= Radius;
end;

{***************************************}
class function TGosPointD.Zero: TGosPointD;
begin
  Result.X := 0;
  Result.Y := 0;
end;

{*********************************}
function TGosPointD.IsZero: Boolean;
begin
  Result := SameValue(X, 0.0) and SameValue(Y, 0.0);
end;

{*********************************}
function TGosPointD.Ceiling: TPoint;
begin
  Result.X := Ceil(X);
  Result.Y := Ceil(Y);
end;

{**********************************}
function TGosPointD.Truncate: TPoint;
begin
  Result.X := Trunc(X);
  Result.Y := Trunc(Y);
end;

{*******************************}
function TGosPointD.Round: TPoint;
begin
  Result.X := System.Round(X);
  Result.Y := System.Round(Y);
end;

{**************************************************************************************************}
function TGosPointD.SnapToPixel(const AScale: Double; const APlaceBetweenPixels: Boolean): TGosPointD;
var
  LScale: Double;
begin
  if AScale <= 0 then
    LScale := 1
  else
    LScale := AScale;
  Result.X := System.Round(Self.X * LScale) / LScale;
  Result.Y := System.Round(Self.Y * LScale) / LScale;
  if APlaceBetweenPixels then
  begin
    LScale := LScale / 2;
    Result.Offset(LScale, LScale);
  end;
end;

{*********************************************************}
function TGosPointD.Scale(const AFactor: Double): TGosPointD;
begin
  Result := Self * AFactor;
end;

{**************************************}
function TGosPointD.Normalize: TGosPointD;
var
  Len: Double;
begin
  Len := Sqrt(Sqr(X) + Sqr(Y));

  if (Len <> 0.0) then
  begin
    Result.X := X / Len;
    Result.Y := Y / Len;
  end
  else
    Result := Self;
end;

{********************************}
function TGosPointD.Length: Double;
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
end;

{***********************************************}
procedure TGosPointD.SetLocation(const P: TPoint);
begin
  Self.X := P.X;
  Self.Y := P.Y;
end;

{**************************************************}
procedure TGosPointD.SetLocation(const P: TGosPointD);
begin
  Self := P;
end;

{**************************************************}
procedure TGosPointD.SetLocation(const X, Y: Double);
begin
  Self.X := X;
  Self.Y := Y;
end;

{*************************************************************}
function TGosPointD.Subtract(const Point: TGosPointD): TGosPointD;
begin
  Result.X := Self.X - Point.X;
  Result.Y := Self.Y - Point.Y;
end;

{**********************************************************}
function TGosPointD.Subtract(const Point: TPoint): TGosPointD;
begin
  Result.X := Self.X - Point.X;
  Result.Y := Self.Y - Point.Y;
end;

{****************************************************************}
procedure SinCosDouble(const Theta: Double; var Sin, Cos: Double);
var
{$IF SizeOf(Extended) > SizeOf(Double)}
  S, C: Extended;
{$ELSE}
  S, C: Double;
{$ENDIF}
begin
  System.SineCosine(Theta, S, C);
  Sin := S;
  Cos := C;
end;

{*********************************************************}
function TGosPointD.Rotate(const AAngle: Double): TGosPointD;
var
  Sine, Cosine: Double;
begin
  SinCosDouble(AAngle, Sine, Cosine);
  Result.X := X * Cosine - Y * Sine;
  Result.Y := X * Sine + Y * Cosine;
end;

{*************************************************************}
function TGosPointD.Reflect(const APoint: TGosPointD): TGosPointD;
begin
  Result := Self + APoint * (-2 * Self.DotProduct(APoint));
end;

{**************************************************************}
function TGosPointD.MidPoint(const APoint: TGosPointD): TGosPointD;
begin
  Result.X := (Self.X + APoint.X) / 2;
  Result.Y := (Self.Y + APoint.Y) / 2;
end;

{********************************************************}
function TGosPointD.Angle(const APoint: TGosPointD): Double;
begin
  Result := Arctan2(Self.Y - APoint.Y, Self.X - APoint.X);
end;

{*****************************}
function TGosPointD.Abs: Double;
begin
  Result := Sqrt(Sqr(self.X) + Sqr(self.Y));
end;

{**************************************************************}
function TGosPointD.AngleCosine(const APoint: TGosPointD): Double;
begin
  Result := Self.Length * APoint.Length;

  if system.Abs(Result) > Epsilon then
    Result := Self.DotProduct(APoint) / Result
  else
    Result := Self.DotProduct(APoint) / Epsilon;

  Result := Max(Min(Result, 1), -1);
end;

{*****************************************************************}
constructor TGosRectD.Create(const R: TGosRectD; Normalize: Boolean);
begin
  Self := R;
  if Normalize then NormalizeRect;
end;

{**************************************************************}
constructor TGosRectD.Create(const R: TRect; Normalize: Boolean);
begin
  Self.Left := R.Left;
  Self.Top  := R.Top;
  Self.Right := R.Right;
  Self.Bottom := R.Bottom;
  if Normalize then NormalizeRect;
end;

{***************************************************}
constructor TGosRectD.Create(const Origin: TGosPointD);
begin
  TopLeft := Origin;
  BottomRight := Origin;
end;

{******************************************************************}
constructor TGosRectD.Create(const Left, Top, Right, Bottom: Double);
begin
  Self.Left := Left; Self.Top := Top;
  Self.Right := Right; Self.Bottom := Bottom;
end;

{***********************************************************************}
constructor TGosRectD.Create(const P1, P2: TGosPointD; Normalize: Boolean);
begin
  Self.TopLeft := P1;
  Self.BottomRight := P2;
  if Normalize then NormalizeRect;
end;

{********************************************************************************}
constructor TGosRectD.Create(const Origin: TGosPointD; const Width, Height: Double);
begin
  Self.TopLeft := Origin;
  Self.Width := Width;
  Self.Height := Height;
end;

{***************************************************************}
class operator TGosRectD.Equal(const Lhs, Rhs: TGosRectD): Boolean;
begin
  Result := (Lhs.TopLeft = Rhs.TopLeft) and
            (Lhs.BottomRight = Rhs.BottomRight);
end;

{****************************************************************************}
function TGosRectD.EqualsTo(const R: TGosRectD; const Epsilon: Double): Boolean;
begin
  Result := TopLeft.EqualsTo(R.TopLeft, Epsilon) and BottomRight.EqualsTo(R.BottomRight, Epsilon);
end;

{**************************************************************************************}
function TGosRectD.FitInto(const ADesignatedArea: TGosRectD; out Ratio: Double): TGosRectD;
begin
  if (ADesignatedArea.Width <= 0) or (ADesignatedArea.Height <= 0) then
  begin
    Ratio := 1;
    Exit(Self);
  end;

  if (Self.Width / ADesignatedArea.Width) > (Self.Height / ADesignatedArea.Height) then
    Ratio := Self.Width / ADesignatedArea.Width
  else
    Ratio := Self.Height / ADesignatedArea.Height;

  if Ratio = 0 then
    Exit(Self)
  else
  begin
    Result := TGosRectD.Create(0, 0, Self.Width / Ratio, Self.Height / Ratio);
    GosRectCenter(Result, ADesignatedArea);
  end;
end;

{*******************************************************************}
function TGosRectD.FitInto(const ADesignatedArea: TGosRectD): TGosRectD;
var
  Ratio: Double;
begin
  Result := FitInto(ADesignatedArea, Ratio);
end;

{********************************************************************}
function TGosRectD.CenterAt(const ADesignatedArea: TGosRectD): TGosRectD;
begin
  Result := Self;
  GosRectCenter(Result, ADesignatedArea);
end;

{********************************************************************************************}
function TGosRectD.PlaceInto(const ADesignatedArea: TGosRectD; const AHorzAlign: THorzRectAlign;
  const AVertAlign: TVertRectAlign): TGosRectD;
var
  LLocation: TGosPointD;
begin
  Result := Self;
  if (Self.Width > ADesignatedArea.Width) or (Self.Height > ADesignatedArea.Height) then
    Result := Result.FitInto(ADesignatedArea);
 case AHorzAlign of
   THorzRectAlign.Center: LLocation.X := (ADesignatedArea.Left + ADesignatedArea.Right - Result.Width) / 2;
   THorzRectAlign.Left: LLocation.X := ADesignatedArea.Left;
   THorzRectAlign.Right: LLocation.X := ADesignatedArea.Right - Result.Width;
 end;
 case AVertAlign of
   TVertRectAlign.Center: LLocation.Y := (ADesignatedArea.Top + ADesignatedArea.Bottom - Result.Height) / 2;
   TVertRectAlign.Top: LLocation.Y := ADesignatedArea.Top;
   TVertRectAlign.Bottom: LLocation.Y := ADesignatedArea.Bottom - Result.Height;
 end;
 Result.SetLocation(LLocation);
end;

{************************************************************************************************}
function TGosRectD.SnapToPixel(const AScale: Double; const APlaceBetweenPixels: Boolean): TGosRectD;
var
  LScale, HalfPixel: Double;
begin
  if AScale <= 0 then
    LScale := 1
  else
    LScale := AScale;
  Result.Left := System.Trunc(Self.Left * LScale) / LScale;
  Result.Top := System.Trunc(Self.Top * LScale) / LScale;
  Result.Width := System.Round(Self.Width * LScale) / LScale;
  Result.Height := System.Round(Self.Height * LScale) / LScale;
  if APlaceBetweenPixels then
  begin
    HalfPixel := 1 / (2 * LScale);
    Result.Offset(HalfPixel, HalfPixel);
  end;
end;

{********************************************************}
function TGosRectD.Fit(const BoundsRect: TGosRectD): Double;
var
  Ratio: Double;
begin
  Result := 1;
  if (BoundsRect.Width <= 0) or (BoundsRect.Height <= 0) then
    Exit;

  if (Self.Width / BoundsRect.Width) > (Self.Height / BoundsRect.Height) then
    Ratio := Self.Width / BoundsRect.Width
  else
    Ratio := Self.Height / BoundsRect.Height;

  if Ratio < 1 then
    Self := TGosRectD.Create(0, 0, Self.Width, Self.Height)
  else
    Self := TGosRectD.Create(0, 0, Self.Width / Ratio, Self.Height / Ratio);

  Result := Ratio;
  GosRectCenter(Self, BoundsRect);
end;

{******************************************************************}
class operator TGosRectD.NotEqual(const Lhs, Rhs: TGosRectD): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

{**************************************************************}
class operator TGosRectD.Implicit(const Source: TRect): TGosRectD;
begin
  Result := TGosRectD.Create(Source);
end;

{*************}
{$IFNDEF ALDPK}
class operator TGosRectD.Explicit(const Source: TGosRectD): TRect;
begin
  Result := Source.Round;
end;
{$ENDIF}

{**************************************************************}
class operator TGosRectD.Add(const Lhs, Rhs: TGosRectD): TGosRectD;
begin
  Result := TGosRectD.Union(Lhs, Rhs);
end;

{*******************************************************************}
class operator TGosRectD.Multiply(const Lhs, Rhs: TGosRectD): TGosRectD;
begin
  Result := TGosRectD.Intersect(Lhs, Rhs);
end;

{***************************************}
function TGosRectD.CenterPoint: TGosPointD;
begin
  Result.X := (Right - Left)/2.0 + Left;
  Result.Y := (Bottom - Top)/2.0 + Top;
end;

{*****************************************************}
function TGosRectD.Contains(const R: TGosRectD): Boolean;
begin
  Result := (Self.Left <= R.Left)
        and (Self.Right >= R.Right)
        and (Self.Top <= R.Top)
        and (Self.Bottom >= R.Bottom);
end;

{*******************************************************************}
function PtInRect(const Rect: TGosRectD; const P: TGosPointD): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

{*******************************************************}
function TGosRectD.Contains(const Pt: TGosPointD): Boolean;
begin
  Result := (Pt.X >= Self.Left)
        and (Pt.X < Self.Right)
        and (Pt.Y >= Self.Top)
        and (Pt.Y < Self.Bottom);
end;

{*****************************************************}
function TGosRectD.Contains(const Pt: TPointf): Boolean;
begin
  Result := (Pt.X >= Self.Left)
        and (Pt.X < Self.Right)
        and (Pt.Y >= Self.Top)
        and (Pt.Y < Self.Bottom);
end;

{**************************************}
class function TGosRectD.Empty: TGosRectD;
begin
  Result := TGosRectD.Create(0,0,0,0);
end;

{**********************************}
function TGosRectD.GetHeight: Double;
begin
  Result := Self.Bottom - Self.Top;
end;

{************************************************}
procedure TGosRectD.SetHeight(const Value: Double);
begin
  Self.Bottom := Self.Top + Value;
end;

{*********************************}
function TGosRectD.GetWidth: Double;
begin
  Result := Self.Right - Self.Left;
end;

{***********************************************}
procedure TGosRectD.SetWidth(const Value: Double);
begin
  Self.Right := Self.Left + Value;
end;

{**********************************}
function TGosRectD.GetSize: TGosSizeD;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

{************************************************}
procedure TGosRectD.SetSize(const Value: TGosSizeD);
begin
  Width := Value.cx;
  Height := Value.cy;
end;

{***********************************************}
procedure TGosRectD.Inflate(const DX, DY: Double);
begin
  TopLeft.Offset(-DX, -DY);
  BottomRight.Offset(DX, DY);
end;

{*******************************************************}
procedure TGosRectD.Inflate(const DL, DT, DR, DB: Double);
begin
  TopLeft.Offset(-DL, -DT);
  BottomRight.Offset(DR, DB);
end;

{************************************************}
procedure TGosRectD.Offset(const Point: TGosPointD);
begin
  TopLeft.Offset(Point);
  BottomRight.Offset(Point);
end;

{**********************************************}
procedure TGosRectD.Offset(const DX, DY: Double);
begin
  TopLeft.Offset(DX, DY);
  BottomRight.Offset(DX, DY);
end;

{***************************************}
function TGosRectD.GetLocation: TGosPointD;
begin
  Result := TopLeft;
end;

{*****************************************************}
procedure TGosRectD.SetLocation(const Point: TGosPointD);
begin
  Offset(Point.X - Left, Point.Y - Top);
end;

{*************************************************}
procedure TGosRectD.SetLocation(const X, Y: Double);
begin
  Offset(X - Left, Y - Top);
end;

{***********************************************************}
function TGosRectD.IntersectsWith(const R: TGosRectD): Boolean;
begin
  Result := (Self.Left < R.Right)
        and (Self.Right > R.Left)
        and (Self.Top < R.Bottom)
        and (Self.Bottom > R.Top);
end;

{*********************************}
function TGosRectD.IsEmpty: Boolean;
begin
  Result := (Right <= Left) or (Bottom <= Top);
end;

{*******************************}
procedure TGosRectD.NormalizeRect;
var
  temp: Double;
begin
  if Top > Bottom then
  begin
    temp := Top;
    Top := Bottom;
    Bottom := temp;
  end;
  if Left > Right then
  begin
    temp := Left;
    Left := Right;
    Right := temp;
  end
end;

{*******************************}
function TGosRectD.Ceiling: TRect;
begin
  Result.TopLeft := TopLeft.Ceiling;
  Result.BottomRight := BottomRight.Ceiling;
end;

{********************************}
function TGosRectD.Truncate: TRect;
begin
  Result.TopLeft := TopLeft.Truncate;
  Result.BottomRight := BottomRight.Truncate;
end;

{*****************************}
function TGosRectD.Round: TRect;
begin
  Result.TopLeft := TopLeft.Round;
  Result.BottomRight := BottomRight.Round;
end;

{******************************************************************}
class function TGosRectD.Intersect(const R1, R2: TGosRectD): TGosRectD;
begin
  GosIntersectRect(Result, R1, R2);
end;

{**********************************************}
procedure TGosRectD.Intersect(const R: TGosRectD);
begin
  Self := Intersect(Self, R);
end;

{**************************************************************}
class function TGosRectD.Union(const R1, R2: TGosRectD): TGosRectD;
begin
  GosUnionRect(Result, R1, R2);
end;

{******************************************}
procedure TGosRectD.Union(const R: TGosRectD);
begin
  Self := TGosRectD.Union(Self, R);
end;

{************************************************************************}
class function TGosRectD.Union(const Points: Array of TGosPointD): TGosRectD;
var
  I: Integer;
  TLCorner, BRCorner: TGosPointD;
begin
  if Length(Points) > 0 then
  begin
    TLCorner := Points[Low(Points)];
    BRCorner := Points[Low(Points)];

    if Length(Points) > 1 then
    begin
      for I := Low(Points) + 1 to High(Points) do
      begin
        if Points[I].X < TLCorner.X then TLCorner.X := Points[I].X;
        if Points[I].X > BRCorner.X then BRCorner.X := Points[I].X;
        if Points[I].Y < TLCorner.Y then TLCorner.Y := Points[I].Y;
        if Points[I].Y > BRCorner.Y then BRCorner.Y := Points[I].Y;
      end;
    end;

    Result := TGosRectD.Create(TLCorner, BRCorner);
  end
  else begin
    Result := TGosRectD.Empty;
  end;
end;

{*****************************************************}
function TGosSizeD.Add(const Point: TGosSizeD): TGosSizeD;
begin
  Result.cx := cx + Point.cx;
  Result.cy := cy + Point.cy;
end;

{**************************************************************}
class operator TGosSizeD.Add(const Lhs, Rhs: TGosSizeD): TGosSizeD;
begin
  Result.cx := Lhs.cx + Rhs.cx;
  Result.cy := Lhs.cy + Rhs.cy;
end;

{**********************************************}
constructor TGosSizeD.Create(const X, Y: Double);
begin
  cx := X;
  cy := Y;
end;

{***************************************}
constructor TGosSizeD.Create(P: TGosSizeD);
begin
  cx := P.cx;
  cy := P.cy;
end;

{*****************************************************}
function TGosSizeD.Distance(const P2: TGosSizeD): Double;
begin
  Result := Sqrt(Sqr(Self.cx - P2.cx) + Sqr(Self.cy - P2.cy));
end;

{*****************************************************************}
class operator TGosSizeD.Implicit(const Point: TGosPointD): TGosSizeD;
begin
  Result.cx := Point.X;
  Result.cy := Point.Y;
end;

{****************************************************************}
class operator TGosSizeD.Implicit(const Size: TGosSizeD): TGosPointD;
begin
  Result.X := Size.cx;
  Result.Y := Size.cy;
end;

{********************************}
function TGosSizeD.IsZero: Boolean;
begin
  Result := SameValue(cx, 0.0) and SameValue(cy, 0.0);
end;

{***************************************************************}
class operator TGosSizeD.Equal(const Lhs, Rhs: TGosSizeD): Boolean;
begin
  Result := SameValue(Lhs.cx, Rhs.cx) and SameValue(Lhs.cy, Rhs.cy);
end;

{******************************************************************}
class operator TGosSizeD.NotEqual(const Lhs, Rhs: TGosSizeD): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

{**********************************************************}
function TGosSizeD.Subtract(const Point: TGosSizeD): TGosSizeD;
begin
  Result.cx := cx - Point.cx;
  Result.cy := cy - Point.cy;
end;

{*****************************************}
function TGosSizeD.SwapDimensions: TGosSizeD;
begin
  Result := TGosSizeD.Create(Height, Width);
end;

{*******************************************************************}
class operator TGosSizeD.Subtract(const Lhs, Rhs: TGosSizeD): TGosSizeD;
begin
  Result.cx := Lhs.cx - Rhs.cx;
  Result.cy := Lhs.cy - Rhs.cy;
end;

{*******************************}
function TGosSizeD.Ceiling: TSize;
begin
  Result.cx := Ceil(cx);
  Result.cy := Ceil(cy);
end;

{*****************************}
function TGosSizeD.Round: TSize;
begin
  Result.cx := Trunc(cx + 0.5);
  Result.cy := Trunc(cy + 0.5);
end;

{********************************}
function TGosSizeD.Truncate: TSize;
begin
  Result.cx := Trunc(cx);
  Result.cy := Trunc(cy);
end;

{************************************************************}
class operator TGosSizeD.Implicit(const Size: TSize): TGosSizeD;
begin
  Result.cx := Size.cx;
  Result.cy := Size.cy;
end;

{$IFNDEF NEXTGEN}

{*****************************************************}
constructor EGosException.Create(const Msg: AnsiString);
begin
  inherited create(String(Msg));
end;

{************************************************************************************}
constructor EGosException.CreateFmt(const Msg: ansistring; const Args: array of const);
begin
  inherited CreateFmt(String(Msg), Args);
end;

{$ENDIF !NEXTGEN}

{*****************************************************}
procedure GosAddCallStackCustomLogU(Const aLog: String);
var LCallStackCustomLogU: _TGosCallStackCustomLogU;
begin
  LCallStackCustomLogU.TimeStamp := Now;
  LCallStackCustomLogU.log := aLog;
  Tmonitor.enter(_GosCallStackCustomLogsU);
  Try
    _GosCallStackCustomLogsUCurrentIndex := (_GosCallStackCustomLogsUCurrentIndex + 1) mod GosCallStackCustomLogsMaxCount;
    if _GosCallStackCustomLogsUCurrentIndex <= _GosCallStackCustomLogsU.Count - 1 then
      _GosCallStackCustomLogsU[_GosCallStackCustomLogsUCurrentIndex] := LCallStackCustomLogU
    else
      _GosCallStackCustomLogsUCurrentIndex := _GosCallStackCustomLogsU.Add(LCallStackCustomLogU);
  Finally
    Tmonitor.exit(_GosCallStackCustomLogsU);
  End;
end;

{*********************************************************************************}
function GosGetCallStackCustomLogsU(Const aAppendTimeStamp: boolean = True): String;
Var i: integer;
begin
  Result := '';
  if aAppendTimeStamp then begin
    for i := _GosCallStackCustomLogsUCurrentIndex downto 0 do
      Result := Result + ALFormatDateTimeU('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z''', TTimeZone.Local.ToUniversalTime(_GosCallStackCustomLogsU[i].TimeStamp), ALDefaultFormatSettingsU) + ': ' + _GosCallStackCustomLogsU[i].log + #13#10;
    for i := _GosCallStackCustomLogsU.Count - 1 downto _GosCallStackCustomLogsUCurrentIndex + 1 do
      Result := Result + ALFormatDateTimeU('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z''', TTimeZone.Local.ToUniversalTime(_GosCallStackCustomLogsU[i].TimeStamp), ALDefaultFormatSettingsU) + ': ' + _GosCallStackCustomLogsU[i].log + #13#10;
  end
  else begin
    for i := _GosCallStackCustomLogsUCurrentIndex downto 0 do
      Result := Result + _GosCallStackCustomLogsU[i].log + #13#10;
    for i := _GosCallStackCustomLogsU.Count - 1 downto _GosCallStackCustomLogsUCurrentIndex + 1 do
      Result := Result + _GosCallStackCustomLogsU[i].log + #13#10;
  end;
  Result := ALTrimU(Result);
end;

{***********************************************************************************************}
procedure GosLog(Const Tag: String; Const msg: String; const _type: TGosLogType = TGosLogType.INFO);
{$IF defined(IOS) or defined(MSWINDOWS)}
var aMsg: String;
{$ENDIF}
begin
  {$IF defined(ANDROID)}
  case _type of
    TGosLogType.VERBOSE: TJLog.JavaClass.v(StringToJString(Tag), StringToJString(msg));
    TGosLogType.DEBUG: TJLog.JavaClass.d(StringToJString(Tag), StringToJString(msg));
    TGosLogType.INFO: TJLog.JavaClass.i(StringToJString(Tag), StringToJString(msg));
    TGosLogType.WARN: TJLog.JavaClass.w(StringToJString(Tag), StringToJString(msg));
    TGosLogType.ERROR: TJLog.JavaClass.e(StringToJString(Tag), StringToJString(msg));
    TGosLogType.ASSERT: TJLog.JavaClass.wtf(StringToJString(Tag), StringToJString(msg)); // << wtf for What a Terrible Failure but everyone know that it's for what the fuck !
  end;
  {$ELSEIF defined(IOS)}
  // https://forums.developer.apple.com/thread/4685
  //if _type <> TGosLogType.VERBOSE  then begin // because log on ios slow down the app so skip verbosity
    if msg <> '' then aMsg := ' => ' + msg
    else aMsg := '';
    case _type of
      TGosLogType.VERBOSE: NSLog(StringToID('[V] ' + Tag + aMsg));
      TGosLogType.DEBUG:   NSLog(StringToID('[D][V] ' + Tag + aMsg));
      TGosLogType.INFO:    NSLog(StringToID('[I][D][V] ' + Tag + aMsg));
      TGosLogType.WARN:    NSLog(StringToID('[W][I][D][V] ' + Tag + aMsg));
      TGosLogType.ERROR:   NSLog(StringToID('[E][W][I][D][V] ' + Tag + aMsg));
      TGosLogType.ASSERT:  NSLog(StringToID('[A][E][W][I][D][V] ' + Tag + aMsg));
    end;
  //end;
  {$ELSEIF defined(MSWINDOWS)}
  if _type <> TGosLogType.VERBOSE  then begin // because log on windows slow down the app so skip verbosity
    if msg <> '' then aMsg := ' => ' + stringReplace(msg, '%', '%%', [rfReplaceALL]) // https://quality.embarcadero.com/browse/RSP-15942
    else aMsg := '';
    case _type of
      TGosLogType.VERBOSE: OutputDebugString(pointer('[V] ' + Tag + aMsg + ' |'));
      TGosLogType.DEBUG:   OutputDebugString(pointer('[D][V] ' + Tag + aMsg + ' |'));
      TGosLogType.INFO:    OutputDebugString(pointer('[I][D][V] ' + Tag + aMsg + ' |'));
      TGosLogType.WARN:    OutputDebugString(pointer('[W][I][D][V] ' + Tag + aMsg + ' |'));
      TGosLogType.ERROR:   OutputDebugString(pointer('[E][W][I][D][V] ' + Tag + aMsg + ' |'));
      TGosLogType.ASSERT:  OutputDebugString(pointer('[A][E][W][I][D][V] ' + Tag + aMsg + ' |'));
    end;
  end;
  {$IFEND}
end;

{******************************************}
Function GosBoolToInt(Value:Boolean):Integer;
Begin
  If Value then result := 1
  else result := 0;
end;

{******************************************}
Function GosIntToBool(Value:integer):boolean;
begin
  result := Value <> 0;
end;

{***************************************************************}
Function GosMediumPos(LTotal, LBorder, LObject : integer):Integer;
Begin
  result := (LTotal - (LBorder*2) - LObject) div 2 + LBorder;
End;

{$IFNDEF NEXTGEN}

{***********************************************************************************************}
function GosIfThen(AValue: Boolean; const ATrue: AnsiString; AFalse: AnsiString = ''): AnsiString;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{$ENDIF !NEXTGEN}

{************************************************************************************}
function GosIfThenU(AValue: Boolean; const ATrue: String; AFalse: String = ''): String;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{*******************************************************************************************}
function GosIfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{*************************************************************************************}
function GosIfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64 = 0): Int64;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{****************************************************************************************}
function GosIfThen(AValue: Boolean; const ATrue: UInt64; const AFalse: UInt64 = 0): UInt64;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{****************************************************************************************}
function GosIfThen(AValue: Boolean; const ATrue: Single; const AFalse: Single = 0): Single;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{****************************************************************************************}
function GosIfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0): Double;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{**********************************************************************************************}
function GosIfThen(AValue: Boolean; const ATrue: Extended; const AFalse: Extended = 0): Extended;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{****************}
{$IFDEF MSWINDOWS}
Function GosGetDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
var LResult: Dword;
    LdynamicTimezone:TDynamicTimeZoneInformation;
    I: integer;
begin
  setlength(result, 200);
  for i := 0 to maxint do begin
    Lresult := EnumDynamicTimeZoneInformation(I, @LdynamicTimezone);
    if (Lresult = ERROR_SUCCESS) then begin
      if i >= length(result) then Setlength(Result, length(result) + 100);
      Result[i] := LdynamicTimezone;
    end
    else if (Lresult = ERROR_NO_MORE_ITEMS) then begin
      setlength(result, i);
      break
    end
    else raiseLastOsError
  end;
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Function GosGetDynamicTimeZoneInformation(const aTimeZoneKeyName: String): TDynamicTimeZoneInformation;
var LDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
    I: integer;
begin
  LDynamicTimeZoneInformations := GosGetDynamicTimeZoneInformations;
  for I := low(LDynamicTimeZoneInformations) to High(LDynamicTimeZoneInformations) do
    if ALSameTextU(LDynamicTimeZoneInformations[i].TimeZoneKeyName, aTimeZoneKeyName) then begin
      result := LDynamicTimeZoneInformations[i];
      Exit;
    end;
  raise Exception.Create('Unknown TimeZoneKeyName');
end;
{$ENDIF}

{************************************************************************}
function GosLocalDateTimeToUTC(Const aLocalDateTime: TDateTime): TdateTime;
begin
  result := TTimeZone.Local.ToUniversalTime(aLocalDateTime);
end;

{****************}
{$IFDEF MSWINDOWS}
Function GosLocalDateTimeToUTC(const aTimeZoneKeyName: String; aLocalDateTime: TdateTime): TdateTime; overload;
var LDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
    LSystemTime, LLocalTime: TSystemTime;
    I: integer;
begin
  LDynamicTimeZoneInformations := GosGetDynamicTimeZoneInformations;
  for I := low(LDynamicTimeZoneInformations) to High(LDynamicTimeZoneInformations) do begin
    if ALSameTextU(LDynamicTimeZoneInformations[i].TimeZoneKeyName, aTimeZoneKeyName) then begin
      DecodeDateTime(
        aLocalDateTime,
        LLocalTime.wYear,
        LLocalTime.wMonth,
        LLocalTime.wDay,
        LLocalTime.wHour,
        LLocalTime.wMinute,
        LLocalTime.wSecond,
        LLocalTime.wMilliseconds);
      LLocalTime.wDayOfWeek := DayOfWeek(aLocalDateTime) - 1;
      if TzSpecificLocalTimeToSystemTimeEx(@LDynamicTimeZoneInformations[i], LLocalTime, LSystemTime) then begin
        Result := EncodeDateTime(
                    LSystemTime.wYear,
                    LSystemTime.wMonth,
                    LSystemTime.wDay,
                    LSystemTime.wHour,
                    LSystemTime.wMinute,
                    LSystemTime.wSecond,
                    LSystemTime.wMilliseconds);
        exit;
      end
      else
        RaiseLastOsError;
    end;
  end;
  raise Exception.Create('Unknown TimeZoneKeyName');
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Function GosLocalDateTimeToUTC(const aTimeZoneInformation: TDynamicTimeZoneInformation; aLocalDateTime: TdateTime): TdateTime; overload;
var LSystemTime, LLocalTime: TSystemTime;
begin
  DecodeDateTime(
    aLocalDateTime,
    LLocalTime.wYear,
    LLocalTime.wMonth,
    LLocalTime.wDay,
    LLocalTime.wHour,
    LLocalTime.wMinute,
    LLocalTime.wSecond,
    LLocalTime.wMilliseconds);
  LLocalTime.wDayOfWeek := DayOfWeek(aLocalDateTime) - 1;
  if TzSpecificLocalTimeToSystemTimeEx(@aTimeZoneInformation, LLocalTime, LSystemTime) then
    Result := EncodeDateTime(
                LSystemTime.wYear,
                LSystemTime.wMonth,
                LSystemTime.wDay,
                LSystemTime.wHour,
                LSystemTime.wMinute,
                LSystemTime.wSecond,
                LSystemTime.wMilliseconds)
  else begin
    result := 0; // to hide a warning
    RaiseLastOsError;
  end;
end;
{$ENDIF}

{**********************************************************************}
function GosUTCDateTimeToLocal(Const aUTCDateTime: TDateTime): TdateTime;
begin
  result := TTimeZone.Local.ToLocalTime(aUTCDateTime);
end;

{****************}
{$IFDEF MSWINDOWS}
Function GosUTCDateTimeToLocal(const aTimeZoneKeyName: String; aUTCDateTime: TdateTime): TdateTime; overload;
var LDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
    LSystemTime, LLocalTime: TSystemTime;
    I: integer;
begin
  LDynamicTimeZoneInformations := GosGetDynamicTimeZoneInformations;
  for I := low(LDynamicTimeZoneInformations) to High(LDynamicTimeZoneInformations) do begin
    if ALSameTextU(LDynamicTimeZoneInformations[i].TimeZoneKeyName, aTimeZoneKeyName) then begin
      DecodeDateTime(
        aUTCDateTime,
        LSystemTime.wYear,
        LSystemTime.wMonth,
        LSystemTime.wDay,
        LSystemTime.wHour,
        LSystemTime.wMinute,
        LSystemTime.wSecond,
        LSystemTime.wMilliseconds);
      LLocalTime.wDayOfWeek := DayOfWeek(aUTCDateTime) - 1;
      if SystemTimeToTzSpecificLocalTime(@LDynamicTimeZoneInformations[i], LSystemTime, LLocalTime) then begin
        Result := EncodeDateTime(
                    LLocalTime.wYear,
                    LLocalTime.wMonth,
                    LLocalTime.wDay,
                    LLocalTime.wHour,
                    LLocalTime.wMinute,
                    LLocalTime.wSecond,
                    LLocalTime.wMilliseconds);
        exit;
      end
      else
        RaiseLastOsError;
    end;
  end;
  raise Exception.Create('Unknown TimeZoneKeyName');
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Function GosUTCDateTimeToLocal(const aTimeZoneInformation: TDynamicTimeZoneInformation; aUTCDateTime: TdateTime): TdateTime; overload;
var LSystemTime, LLocalTime: TSystemTime;
begin
  DecodeDateTime(
    aUTCDateTime,
    LSystemTime.wYear,
    LSystemTime.wMonth,
    LSystemTime.wDay,
    LSystemTime.wHour,
    LSystemTime.wMinute,
    LSystemTime.wSecond,
    LSystemTime.wMilliseconds);
  LLocalTime.wDayOfWeek := DayOfWeek(aUTCDateTime) - 1;
  if SystemTimeToTzSpecificLocalTime(@aTimeZoneInformation, LSystemTime, LLocalTime) then
    Result := EncodeDateTime(
                LLocalTime.wYear,
                LLocalTime.wMonth,
                LLocalTime.wDay,
                LLocalTime.wHour,
                LLocalTime.wMinute,
                LLocalTime.wSecond,
                LLocalTime.wMilliseconds)
  else begin
    result := 0; // to hide a warning
    RaiseLastOsError;
  end;
end;
{$ENDIF}

{**********}
{$IFDEF IOS}
function GosNSDateToUTCDateTime(const ADateTime: NSDate): TDateTime;
begin
  if ADateTime <> nil then
    Result := ADateTime.TimeIntervalSince1970 / SecsPerDay + EncodeDate(1970, 1, 1)
  else
    Result := 0.0;
end;
{$ENDIF}

{*************************}
{The same like Now but used
 UTC-time not local time.}
function GosUTCNow: TDateTime;
begin
  result := TTimeZone.Local.ToUniversalTime(NOW);
end;

{******************************************************}
Function GosInc(var x: integer; Count: integer): Integer;
begin
  inc(X, count);
  result := X;
end;

{*******************************************************}
{Accepts number of milliseconds in the parameter aValue,
 provides 1000 times more precise value of TDateTime}
function GosUnixMsToDateTime(const aValue: Int64): TDateTime;
begin
  Result := IncMilliSecond(UnixDateDelta, aValue);
end;

{********************************************************}
{Returns UNIX-time as the count of milliseconds since the
 UNIX epoch. Can be very useful for the purposes of
 special precision.}
function GosDateTimeToUnixMs(const aValue: TDateTime): Int64;
begin
  result := MilliSecondsBetween(UnixDateDelta, aValue);
  if aValue < UnixDateDelta then result := -result;
end;

{***************************************************************}
Procedure GosFreeAndNil(var Obj; const adelayed: boolean = false);
var Temp: TObject;
begin
  Temp := TObject(Obj);
  if temp = nil then exit;
  TObject(Obj) := nil;
  if adelayed and assigned(GosCustomDelayedFreeObjectProc) then GosCustomDelayedFreeObjectProc(Temp)
  else begin
    {$IF defined(AUTOREFCOUNT)}
    if AtomicCmpExchange(temp.refcount{Target}, 0{NewValue}, 0{Compareand}) = 1 then begin // it's seam it's not an atomic operation (http://stackoverflow.com/questions/39987850/is-reading-writing-an-integer-4-bytes-atomic-on-ios-android-like-on-win32-win6)
      temp.Free;
      temp := nil;
    end
    else begin
      Temp.DisposeOf; // TComponent Free Notification mechanism notifies registered components that particular
                      // component instance is being freed. Notified components can handle that notification inside
                      // virtual Notification method and make sure that they clear all references they may hold on
                      // component being destroyed.
                      //
                      // Free Notification mechanism is being triggered in TComponent destructor and without DisposeOf
                      // and direct execution of destructor, two components could hold strong references to each
                      // other keeping themselves alive during whole application lifetime.
      {$IF defined(DEBUG)}
      if ALFreeAndNilRefCountWarn and
        (not ALCurThreadFreeAndNilNORefCountWarn) and
        ((not assigned(ALFreeAndNilCanRefCountWarnProc)) or
         (ALFreeAndNilCanRefCountWarnProc(Temp))) then begin
        if (Temp.RefCount - 1) and (not $40000000{Temp.objDisposedFlag}) <> 0 then
          ALLog('ALFreeAndNil', Temp.ClassName + ' | Refcount is not null (' + Inttostr((Temp.RefCount - 1) and (not $40000000{Temp.objDisposedFlag})) + ')', TGosLogType.warn);
      end;
      {$ENDIF}
      temp := nil;
    end;
    {$ELSE}
    temp.Free;
    temp := nil;
    {$IFEND}
  end;
end;

{*************************************************************************************}
Procedure GosFreeAndNil(var Obj; const adelayed: boolean; const aRefCountWarn: Boolean);
{$IFDEF DEBUG}
var aOldCurThreadFreeAndNilNoRefCountWarn: boolean;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aOldCurThreadFreeAndNilNoRefCountWarn := GosCurThreadFreeAndNilNORefCountWarn;
  GosCurThreadFreeAndNilNORefCountWarn := not aRefCountWarn;
  try
  {$ENDIF}

    GosFreeAndNil(Obj, adelayed);

  {$IFDEF DEBUG}
  finally
    GosCurThreadFreeAndNilNORefCountWarn := aOldCurThreadFreeAndNilNORefCountWarn;
  end;
  {$ENDIF}
end;

{$IFNDEF NEXTGEN}

//
// Taken from https://github.com/synopse/mORMot.git
// https://synopse.info
// http://mormot.net
//

{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if https://github.com/synopse/mORMot.git SynCommons.pas was not updated from references\mORMot\SynCommons.pas and adjust the IFDEF'}
{$ENDIF}

{**}
type
  TRegisters = record
    eax,ebx,ecx,edx: cardinal;
  end;

{*************************************************************}
procedure GetCPUID(Param: Cardinal; var Registers: TRegisters);
{$IF defined(CPU64BITS)}
{$IF CompilerVersion <= 33} // rio
asm .noframe // ecx=param, rdx=Registers (Linux: edi,rsi)
        mov     eax, Param
        mov     r9, Registers
        mov     r10, rbx // preserve rbx
        xor     ebx, ebx
        xor     ecx, ecx
        xor     edx, edx
        cpuid
        mov     TRegisters(r9).&eax, eax
        mov     TRegisters(r9).&ebx, ebx
        mov     TRegisters(r9).&ecx, ecx
        mov     TRegisters(r9).&edx, edx
        mov     rbx, r10
end;
{$endif}
{$endif}
{$IF CompilerVersion <= 33} // rio
asm
        push    esi
        push    edi
        mov     esi, edx
        mov     edi, eax
        pushfd
        pop     eax
        mov     edx, eax
        xor     eax, $200000
        push    eax
        popfd
        pushfd
        pop     eax
        xor     eax, edx
        jz      @nocpuid
        push    ebx
        mov     eax, edi
        xor     ecx, ecx
        cpuid
        mov     TRegisters(esi).&eax, eax
        mov     TRegisters(esi).&ebx, ebx
        mov     TRegisters(esi).&ecx, ecx
        mov     TRegisters(esi).&edx, edx
        pop     ebx
@nocpuid:
        pop     edi
        pop     esi
end;
{$else}
begin
end;
{$endif}


{******************************************************}
function crc32cBy4SSE42(crc, value: cardinal): cardinal;
{$IF CompilerVersion <= 33} // rio
{$IF defined(CPU64BITS)}
asm .noframe
        mov     eax, crc
        crc32   eax, value
end;
{$endif}
{$endif}
{$IF CompilerVersion <= 33} // rio
asm // eax=crc, edx=value
        {$ifdef UNICODE}
        crc32   eax, edx
        {$else}
        db      $F2, $0F, $38, $F1, $C2
        {$endif}
end;
{$else}
begin
end;
{$endif}

{**************************}
function RdRand32: cardinal;
{$IF defined(CPU64BITS)}
{$IF CompilerVersion <= 33} // rio
asm .noframe
end;
{$endif}
{$endif}
{$IF CompilerVersion <= 33} // rio
asm
  // rdrand eax: same opcodes for x86 and x64
  db $0f, $c7, $f0
  // returns in eax, ignore carry flag (eax=0 won't hurt)
end;
{$else}
begin
end;
{$endif}

{*********************************************}
function IsXmmYmmOSEnabled: boolean; assembler;
{$IF CompilerVersion <= 33} // rio
asm // see https://software.intel.com/en-us/blogs/2011/04/14/is-avx-enabled
        xor     ecx, ecx  // specify control register XCR0 = XFEATURE_ENABLED_MASK
        db  $0f, $01, $d0 // XGETBV reads XCR0 into EDX:EAX
        and     eax, 6    // check OS has enabled both XMM (bit 1) and YMM (bit 2)
        cmp     al, 6
        sete    al
end;
{$else}
begin
end;
{$endif}

{**************************}
procedure GosInitCpuFeatures;
var regs: TRegisters;
    c: cardinal;
begin
  {$R-} // this code require range check error OFF
  // retrieve CPUID raw flags
  regs.edx := 0;
  regs.ecx := 0;
  GetCPUID(1,regs);
  PIntegerArray(@GosCpuFeatures)^[0] := regs.edx;
  PIntegerArray(@GosCpuFeatures)^[1] := regs.ecx;
  GetCPUID(7,regs);
  PIntegerArray(@GosCpuFeatures)^[2] := regs.ebx;
  PIntegerArray(@GosCpuFeatures)^[3] := regs.ecx;
  PByte(@PIntegerArray(@GosCpuFeatures)^[4])^ := regs.edx;
  if not(cfOSXS in GosCpuFeatures) or not IsXmmYmmOSEnabled then
    GosCpuFeatures := GosCpuFeatures-[cfAVX,cfAVX2,cfFMA];
  // validate accuracy of most used HW opcodes
  if cfRAND in GosCpuFeatures then
    try
      c := RdRand32;
      if RdRand32=c then // most probably a RDRAND bug, e.g. on AMD Rizen 3000
        exclude(GosCpuFeatures,cfRAND);
    except // may trigger an illegal instruction exception on some Ivy Bridge
      exclude(GosCpuFeatures,cfRAND);
    end;
  if cfSSE42 in GosCpuFeatures then
    try
      if crc32cBy4SSE42(0,1)<>3712330424 then
        raise EGosException.Create('Invalid crc32cBy4SSE42');
    except // disable now on illegal instruction or incorrect result
      exclude(GosCpuFeatures,cfSSE42);
    end;
  {$R+} // enable back the {$R+}
end;

{$ENDIF}

initialization

  {$IFNDEF NEXTGEN}
  GosInitCpuFeatures;
  {$ENDIF}

  GosCustomDelayedFreeObjectProc := nil;
  {$IFDEF DEBUG}
  GosFreeAndNilRefCountWarn := False;
  GosFreeAndNilCanRefCountWarnProc := nil;
  {$ENDIF}

  _GosCallStackCustomLogsU := TList<_TGosCallStackCustomLogU>.Create;
  GosMove := system.Move;


Finalization

  GosFreeAndNil(_GosCallStackCustomLogsU);

end.
