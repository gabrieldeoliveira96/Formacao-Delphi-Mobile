{*************************************************************
product:      uGosQuickSortList
Description:  TALIntegerList or TALDoubleList that work exactly
              like TstringList but with integer or Double.
**************************************************************}

unit uGosQuickSortList;

{Exemple of a QuickSort Algorithm :

procedure _SampleQuickSort(aArray: TArrayOfxxx; L, R: Integer);

  Function _Compare(I1,I2: xxx): Integer;
  Begin
    if I1 < I2 then result := -1
    else if I1 > I2 then result := 1
    else result := 0;
  end;

var aItem: xxx;
    I, J, P: Integer;

begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while _Compare(aArray[I], aArray[P]) < 0 do Inc(I);
      while _Compare(aArray[J], aArray[P]) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then begin
          aItem := aArray[i];
          aArray[i] := aArray[j];
          aArray[j] := aItem;
        end;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then _SampleQuickSort(L, J);
    L := I;
  until I >= R;
end;

}

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Uses System.Classes;

Type

  {----------------------------------------------------------------------------------}
  TGosQuickSortListCompare = function(List: TObject; Index1, Index2: Integer): Integer;

  {-----------------------------------------}
  TGosQuickSortPointerList = array of Pointer;

  {-----------------------------------}
  TGosBaseQuickSortList = class(TObject)
  private
    FList: TGosQuickSortPointerList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    procedure SetSorted(Value: Boolean);
    procedure QuickSort(L, R: Integer; SCompare: TGosQuickSortListCompare);
  protected
    function  Get(Index: Integer): Pointer;
    procedure Grow;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function  CompareItems(const Index1, Index2: Integer): Integer; virtual;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure InsertItem(Index: Integer; Item: Pointer);
    procedure Insert(Index: Integer; Item: Pointer);
    property  List: TGosQuickSortPointerList read FList;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: NativeInt); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
    procedure Exchange(Index1, Index2: Integer);
    function  Expand: TGosBaseQuickSortList;
    procedure CustomSort(Compare: TGosQuickSortListCompare); virtual;
    procedure Sort; virtual;
    property  Sorted: Boolean read FSorted write SetSorted;
    property  Capacity: Integer read FCapacity write SetCapacity;
    property  Count: Integer read FCount write SetCount;
    property  Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  {---------------------------------------}
  PGosIntegerListItem = ^TGosIntegerListItem;
  TGosIntegerListItem = record
    FInteger: integer;
    FObject: TObject;
  end;

  {------------------------------------------}
  TGosIntegerList = class(TGosBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function  GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; const Item: Integer);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: integer; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function  IndexOf(Item: Integer): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: integer): Integer;
    Function  AddObject(const Item: integer; AObject: TObject): Integer;
    function  TryAdd(const Item: integer): boolean;
    Function  TryAddObject(const Item: integer; AObject: TObject): boolean;
    function  Find(item: Integer; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: integer);
    procedure InsertObject(Index: Integer; const item: integer; AObject: TObject);
    property  Items[Index: Integer]: Integer read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
    property  OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    function  Push(Item: Integer): Integer;
    function  Pop: Integer; inline;
    function  Peek: Integer; inline;
    function  ToArray: TArray<Integer>;
  end;

  {-----------------------------------------}
  PGosCardinalListItem = ^TGosCardinalListItem;
  TGosCardinalListItem = record
    FCardinal: Cardinal;
    FObject: TObject;
  end;

  {-------------------------------------------}
  TGosCardinalList = class(TGosBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function  GetItem(Index: Integer): Cardinal;
    procedure SetItem(Index: Integer; const Item: Cardinal);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: Cardinal; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function  IndexOf(Item: Cardinal): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: Cardinal): Integer;
    Function  AddObject(const Item: Cardinal; AObject: TObject): Integer;
    function  TryAdd(const Item: Cardinal): boolean;
    Function  TryAddObject(const Item: Cardinal; AObject: TObject): boolean;
    function  Find(item: Cardinal; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: Cardinal);
    procedure InsertObject(Index: Integer; const item: Cardinal; AObject: TObject);
    property  Items[Index: Integer]: Cardinal read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
    property  OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    function  Push(Item: Cardinal): Cardinal;
    function  Pop: Cardinal; inline;
    function  Peek: Cardinal; inline;
    function  ToArray: TArray<Cardinal>;
  end;

  {-----------------------------------}
  PGosInt64ListItem = ^TGosInt64ListItem;
  TGosInt64ListItem = record
    FInt64: Int64;
    FObject: TObject;
  end;

  {----------------------------------------}
  TGosInt64List = class(TGosBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function  GetItem(Index: Integer): Int64;
    procedure SetItem(Index: Integer; const Item: Int64);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: Int64; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function  IndexOf(Item: Int64): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: Int64): Integer;
    Function  AddObject(const Item: Int64; AObject: TObject): Integer;
    function  TryAdd(const Item: Int64): boolean;
    Function  TryAddObject(const Item: Int64; AObject: TObject): boolean;
    function  Find(item: Int64; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: Int64);
    procedure InsertObject(Index: Integer; const item: Int64; AObject: TObject);
    property  Items[Index: Integer]: Int64 read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
    property  OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    function  Push(Item: Int64): Int64;
    function  Pop: Int64; inline;
    function  Peek: Int64; inline;
    function  ToArray: TArray<Int64>;
  end;

  {-------------------------------------------}
  PGosNativeIntListItem = ^TGosNativeIntListItem;
  TGosNativeIntListItem = record
    FNativeInt: NativeInt;
    FObject: TObject;
  end;

  {--------------------------------------------}
  TGosNativeIntList = class(TGosBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function  GetItem(Index: Integer): NativeInt;
    procedure SetItem(Index: Integer; const Item: NativeInt);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: NativeInt; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function  IndexOf(Item: NativeInt): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: NativeInt): Integer;
    Function  AddObject(const Item: NativeInt; AObject: TObject): Integer;
    function  TryAdd(const Item: NativeInt): boolean;
    Function  TryAddObject(const Item: NativeInt; AObject: TObject): boolean;
    function  Find(item: NativeInt; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: NativeInt);
    procedure InsertObject(Index: Integer; const item: NativeInt; AObject: TObject);
    property  Items[Index: Integer]: NativeInt read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
    property  OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    function  Push(Item: NativeInt): NativeInt;
    function  Pop: NativeInt; inline;
    function  Peek: NativeInt; inline;
    function  ToArray: TArray<NativeInt>;
  end;

  {-------------------------------------}
  PGosDoubleListItem = ^TGosDoubleListItem;
  TGosDoubleListItem = record
    FDouble: Double;
    FObject: TObject;
  end;

  {-----------------------------------------}
  TGosDoubleList = class(TGosBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function  GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Item: Double);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: Double; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function  IndexOf(Item: Double): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: Double): Integer;
    Function  AddObject(const Item: Double; AObject: TObject): Integer;
    function  TryAdd(const Item: Double): boolean;
    Function  TryAddObject(const Item: Double; AObject: TObject): boolean;
    function  Find(item: Double; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: Double);
    procedure InsertObject(Index: Integer; const item: Double; AObject: TObject);
    property  Items[Index: Integer]: Double read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
    property  OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    function  Push(Item: Double): Double;
    function  Pop: Double; inline;
    function  Peek: Double; inline;
    function  ToArray: TArray<Double>;
  end;

resourcestring
  SGosDuplicateItem = 'List does not allow duplicates';
  SGosListCapacityError = 'List capacity out of bounds (%d)';
  SGosListCountError = 'List count out of bounds (%d)';
  SGosListIndexError = 'List index out of bounds (%d)';
  SGosSortedListError = 'Operation not allowed on sorted list';

implementation

uses System.Math,
     uGosCommon,
     uGosString;

{***********************************************************************************}
function AlBaseQuickSortListCompare(List: TObject; Index1, Index2: Integer): Integer;
Begin
  result := TGosBaseQuickSortList(List).CompareItems(Index1, Index2);
end;

{**************************************}
constructor TGosBaseQuickSortList.Create;
begin
  SetLength(FList,0);
  FCount:= 0;
  FCapacity:= 0;
  FSorted := False;
  FDuplicates := dupIgnore;
end;

{**************************************}
destructor TGosBaseQuickSortList.Destroy;
begin
  Clear;
  inherited;
end;

{***********************************}
procedure TGosBaseQuickSortList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{***********************************************************************}
procedure TGosBaseQuickSortList.InsertItem(Index: Integer; Item: Pointer);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    GosMove(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  Pointer(FList[Index]) := nil;
  FList[Index] := Item;
  Inc(FCount);
  if (Item <> nil) then
    Notify(Item, lnAdded);
end;

{********************************************************************}
procedure TGosBaseQuickSortList.ExchangeItems(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

{****************************************************}
procedure TGosBaseQuickSortList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SGosListIndexError, Index);
  Temp := FList[Index];
  Dec(FCount);
  if Index < FCount then begin
    FList[Index] := nil;
    GosMove(FList[Index + 1],
           FList[Index],
          (FCount - Index) * SizeOf(Pointer));
    Pointer(FList[FCount]) := nil;
  end;
  if (Temp <> nil) then
    Notify(Temp, lnDeleted);
end;

{*****************************************************************************}
class procedure TGosBaseQuickSortList.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

{******************************************************************************}
class procedure TGosBaseQuickSortList.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]);
end;

{***************************************************************}
procedure TGosBaseQuickSortList.Exchange(Index1, Index2: Integer);
begin
  {Warning:	Do not call Exchange on a sorted list except to swap two identical
   items with different associated objects. Exchange does not check whether
   the list is sorted, and can destroy the sort order of a sorted list.}
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SGosListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SGosListIndexError, Index2);
  ExchangeItems(Index1, Index2);
end;

{*******************************************************************}
procedure TGosBaseQuickSortList.Insert(Index: Integer; Item: Pointer);
begin
  if Sorted then Error(@SGosSortedListError, 0);
  if (Index < 0) or (Index > FCount) then
    Error(@SGosListIndexError, Index);
  InsertItem(Index, Item);
end;

{****************************************************************}
procedure TGosBaseQuickSortList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if Sorted then
    Error(@SGosSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then
    Error(@SGosListIndexError, Index);
  if Item <> FList[Index] then
  begin
    Temp := FList[Index];
    FList[Index] := Item;
    if Temp <> nil then
      Notify(Temp, lnDeleted);
    if Item <> nil then
      Notify(Item, lnAdded);
  end;
end;

{*********************************************************}
function TGosBaseQuickSortList.Get(Index: Integer): Pointer;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SGosListIndexError, Index);
  Result := FList[Index];
end;

{*********************************************************}
function TGosBaseQuickSortList.Expand: TGosBaseQuickSortList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

{**********************************}
procedure TGosBaseQuickSortList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{***************************************************************}
procedure TGosBaseQuickSortList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SGosListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{*********************************************************}
procedure TGosBaseQuickSortList.SetCount(NewCount: Integer);
var
  I: Integer;
  Temp: Pointer;
begin
  if NewCount < 0 then
    Error(@SGosListCountError, NewCount);
  if NewCount <> FCount then
  begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if NewCount > FCount then
      FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
    else
    for I := FCount - 1 downto NewCount do
    begin
      Dec(FCount);
      Temp := List[I];
      if Temp <> nil then
        Notify(Temp, lnDeleted);
    end;
    FCount := NewCount;
  end;
end;

{*****************************************************************************}
procedure TGosBaseQuickSortList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

{*******************************************************}
procedure TGosBaseQuickSortList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{*****************************************************************************************}
procedure TGosBaseQuickSortList.QuickSort(L, R: Integer; SCompare: TGosQuickSortListCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then
          ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

{**************************************************************************}
procedure TGosBaseQuickSortList.CustomSort(Compare: TGosQuickSortListCompare);
begin
  if not Sorted and (FCount > 1) then
    QuickSort(0, FCount - 1, Compare);
end;

{**********************************}
procedure TGosBaseQuickSortList.Sort;
begin
  CustomSort(AlBaseQuickSortListCompare);
end;

{*********************************************************************************}
function TGosBaseQuickSortList.CompareItems(const Index1, Index2: Integer): Integer;
begin
  Result := 0;
end;

{********************************************************}
function TGosIntegerList.Add(const Item: integer): Integer;
begin
  Result := AddObject(Item, nil);
end;

{********************************************************************************}
function TGosIntegerList.AddObject(const Item: integer; AObject: TObject): Integer;
begin
  if not Sorted then Result := FCount
  else if Find(Item, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SGosDuplicateItem, 0);
    end;
  InsertItem(Result, Item, AObject);
end;

{***********************************************************}
function TGosIntegerList.TryAdd(const Item: integer): Boolean;
begin
  Result := TryAddObject(Item, nil);
end;

{***********************************************************************************}
function TGosIntegerList.TryAddObject(const Item: integer; AObject: TObject): Boolean;
var aIndex: integer;
begin
  if not Sorted then aIndex := FCount
  else if Find(Item, aIndex) then
    case Duplicates of
      dupIgnore,
      dupError: Exit(False);
    end;
  InsertItem(aIndex, Item, AObject);
  result := true;
end;

{*****************************************************************************************}
procedure TGosIntegerList.InsertItem(Index: Integer; const item: integer; AObject: TObject);
Var aPALIntegerListItem: PGosIntegerListItem;
begin
  New(aPALIntegerListItem);
  aPALIntegerListItem^.FInteger := item;
  aPALIntegerListItem^.FObject := AObject;
  try
    inherited InsertItem(index,aPALIntegerListItem);
  except
    aPALIntegerListItem^.FObject := nil;
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{***************************************************************************}
function TGosIntegerList.CompareItems(const Index1, Index2: integer): Integer;
begin
  result := CompareValue(PGosIntegerListItem(Get(Index1))^.FInteger, PGosIntegerListItem(Get(Index2))^.FInteger);
end;

{***********************************************************************}
function TGosIntegerList.Find(item: Integer; var Index: Integer): Boolean;
var L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := GetItem(I) - item;
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{*******************************************************}
function TGosIntegerList.GetItem(Index: Integer): Integer;
begin
  Result := PGosIntegerListItem(Get(index))^.FInteger
end;

{********************************}
constructor TGosIntegerList.Create;
begin
  inherited Create;
  FOwnsObject := false;
end;

{******************************************************}
constructor TGosIntegerList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{******************************************************}
function TGosIntegerList.IndexOf(Item: Integer): Integer;
begin
  if not Sorted then Begin
    Result := 0;
    while (Result < FCount) and (GetItem(result) <> Item) do Inc(Result);
    if Result = FCount then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{*******************************************************************}
procedure TGosIntegerList.Insert(Index: Integer; const Item: Integer);
begin
  InsertObject(Index, Item, nil);
end;

{*******************************************************************************************}
procedure TGosIntegerList.InsertObject(Index: Integer; const item: integer; AObject: TObject);
Var aPALIntegerListItem: PGosIntegerListItem;
begin
  New(aPALIntegerListItem);
  aPALIntegerListItem^.FInteger := item;
  aPALIntegerListItem^.FObject := AObject;
  try
    inherited insert(index,aPALIntegerListItem);
  except
    aPALIntegerListItem^.FObject := nil;
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{***********************************************************************}
procedure TGosIntegerList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then Gosfreeandnil(PGosIntegerListItem(Ptr).FObject)
    else PGosIntegerListItem(Ptr).FObject := nil; // to decrease the refcount in ARC
    dispose(ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{********************************************************************}
procedure TGosIntegerList.SetItem(Index: Integer; const Item: Integer);
Var aPALIntegerListItem: PGosIntegerListItem;
begin
  New(aPALIntegerListItem);
  aPALIntegerListItem^.FInteger := item;
  aPALIntegerListItem^.FObject := nil;
  Try
    Put(Index, aPALIntegerListItem);
  except
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{*********************************************************}
function TGosIntegerList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SGosListIndexError, Index);
  Result :=  PGosIntegerListItem(Get(index))^.FObject;
end;

{***************************************************************}
function TGosIntegerList.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{*******************************************************************}
procedure TGosIntegerList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SGosListIndexError, Index);
  PGosIntegerListItem(Get(index))^.FObject := AObject;
end;

{***************************************************}
function TGosIntegerList.Push(Item: integer): integer;
begin
  Add(Item);
  result := Item;
end;

{***********************************}
function TGosIntegerList.Pop: integer;
begin
  Result := Peek;
  Delete(Count-1);
end;

{************************************}
function TGosIntegerList.Peek: integer;
begin
  Result := GetItem(Count-1);
end;

{***********************************************}
function TGosIntegerList.ToArray: TArray<Integer>;
var i: integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[i] := GetItem(i);
end;

{**********************************************************}
function TGosCardinalList.Add(const Item: Cardinal): Integer;
begin
  Result := AddObject(Item, nil);
end;

{**********************************************************************************}
function TGosCardinalList.AddObject(const Item: Cardinal; AObject: TObject): Integer;
begin
  if not Sorted then Result := FCount
  else if Find(Item, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SGosDuplicateItem, 0);
    end;
  InsertItem(Result, Item, AObject);
end;

{*************************************************************}
function TGosCardinalList.TryAdd(const Item: Cardinal): Boolean;
begin
  Result := TryAddObject(Item, nil);
end;

{*************************************************************************************}
function TGosCardinalList.TryAddObject(const Item: Cardinal; AObject: TObject): Boolean;
var aIndex: integer;
begin
  if not Sorted then aIndex := FCount
  else if Find(Item, aIndex) then
    case Duplicates of
      dupIgnore,
      dupError: Exit(False);
    end;
  InsertItem(aIndex, Item, AObject);
  result := true;
end;

{*******************************************************************************************}
procedure TGosCardinalList.InsertItem(Index: Integer; const item: Cardinal; AObject: TObject);
Var aPALCardinalListItem: PGosCardinalListItem;
begin
  New(aPALCardinalListItem);
  aPALCardinalListItem^.FCardinal := item;
  aPALCardinalListItem^.FObject := AObject;
  try
    inherited InsertItem(index,aPALCardinalListItem);
  except
    aPALCardinalListItem^.FObject := nil;
    Dispose(aPALCardinalListItem);
    raise;
  end;
end;

{****************************************************************************}
function TGosCardinalList.CompareItems(const Index1, Index2: integer): Integer;
var aCard1: Cardinal;
    aCard2: Cardinal;
begin
  aCard1 := PGosCardinalListItem(Get(Index1))^.FCardinal;
  aCard2 := PGosCardinalListItem(Get(Index2))^.FCardinal;
  if aCard1 < aCard2 then result := -1
  else if aCard1 > aCard2 then result := 1
  else result := 0;
end;

{*************************************************************************}
function TGosCardinalList.Find(item: Cardinal; var Index: Integer): Boolean;
var L, H, I, C: Integer;

  {---------------------------------------------------------}
  Function _CompareCardinal(D1,D2: Cardinal): Integer; inline
  Begin
    if D1 < D2 then result := -1
    else if D1 > D2 then result := 1
    else result := 0;
  end;

begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := _CompareCardinal(GetItem(I),item);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{*********************************************************}
function TGosCardinalList.GetItem(Index: Integer): Cardinal;
begin
  Result := PGosCardinalListItem(Get(index))^.FCardinal
end;

{*********************************}
constructor TGosCardinalList.Create;
begin
  inherited Create;
  FOwnsObject := false;
end;

{*******************************************************}
constructor TGosCardinalList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{********************************************************}
function TGosCardinalList.IndexOf(Item: Cardinal): Integer;
begin
  if not Sorted then Begin
    Result := 0;
    while (Result < FCount) and (GetItem(result) <> Item) do Inc(Result);
    if Result = FCount then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{*********************************************************************}
procedure TGosCardinalList.Insert(Index: Integer; const Item: Cardinal);
begin
  InsertObject(Index, Item, nil);
end;

{*********************************************************************************************}
procedure TGosCardinalList.InsertObject(Index: Integer; const item: Cardinal; AObject: TObject);
Var aPALCardinalListItem: PGosCardinalListItem;
begin
  New(aPALCardinalListItem);
  aPALCardinalListItem^.FCardinal := item;
  aPALCardinalListItem^.FObject := AObject;
  try
    inherited insert(index,aPALCardinalListItem);
  except
    aPALCardinalListItem^.FObject := nil;
    Dispose(aPALCardinalListItem);
    raise;
  end;
end;

{************************************************************************}
procedure TGosCardinalList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then Gosfreeandnil(PGosCardinalListItem(Ptr).FObject)
    else PGosCardinalListItem(Ptr).FObject := nil; // to decrease the refcount in ARC
    dispose(ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{**********************************************************************}
procedure TGosCardinalList.SetItem(Index: Integer; const Item: Cardinal);
Var aPALCardinalListItem: PGosCardinalListItem;
begin
  New(aPALCardinalListItem);
  aPALCardinalListItem^.FCardinal := item;
  aPALCardinalListItem^.FObject := nil;
  Try
    Put(Index, aPALCardinalListItem);
  except
    Dispose(aPALCardinalListItem);
    raise;
  end;
end;

{**********************************************************}
function TGosCardinalList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SGosListIndexError, Index);
  Result :=  PGosCardinalListItem(Get(index))^.FObject;
end;

{****************************************************************}
function TGosCardinalList.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{********************************************************************}
procedure TGosCardinalList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SGosListIndexError, Index);
  PGosCardinalListItem(Get(index))^.FObject := AObject;
end;

{******************************************************}
function TGosCardinalList.Push(Item: cardinal): cardinal;
begin
  Add(Item);
  result := Item;
end;

{*************************************}
function TGosCardinalList.Pop: cardinal;
begin
  Result := Peek;
  Delete(Count-1);
end;

{**************************************}
function TGosCardinalList.Peek: cardinal;
begin
  Result := GetItem(Count-1);
end;

{*************************************************}
function TGosCardinalList.ToArray: TArray<cardinal>;
var i: integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[i] := GetItem(i);
end;

{****************************************************}
function TGosInt64List.Add(const Item: Int64): Integer;
begin
  Result := AddObject(Item, nil);
end;

{****************************************************************************}
function TGosInt64List.AddObject(const Item: Int64; AObject: TObject): Integer;
begin
  if not Sorted then Result := FCount
  else if Find(Item, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SGosDuplicateItem, 0);
    end;
  InsertItem(Result, Item, AObject);
end;

{*******************************************************}
function TGosInt64List.TryAdd(const Item: Int64): Boolean;
begin
  Result := TryAddObject(Item, nil);
end;

{*******************************************************************************}
function TGosInt64List.TryAddObject(const Item: Int64; AObject: TObject): Boolean;
var aIndex: integer;
begin
  if not Sorted then aIndex := FCount
  else if Find(Item, aIndex) then
    case Duplicates of
      dupIgnore,
      dupError: Exit(False);
    end;
  InsertItem(aIndex, Item, AObject);
  result := true;
end;

{*************************************************************************************}
procedure TGosInt64List.InsertItem(Index: Integer; const item: Int64; AObject: TObject);
Var aPALInt64ListItem: PGosInt64ListItem;
begin
  New(aPALInt64ListItem);
  aPALInt64ListItem^.FInt64 := item;
  aPALInt64ListItem^.FObject := AObject;
  try
    inherited InsertItem(index,aPALInt64ListItem);
  except
    aPALInt64ListItem^.FObject := nil;
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{*************************************************************************}
function TGosInt64List.CompareItems(const Index1, Index2: integer): Integer;
begin
  result := compareValue(PGosInt64ListItem(Get(Index1))^.FInt64, PGosInt64ListItem(Get(Index2))^.FInt64);
end;

{*******************************************************************}
function TGosInt64List.Find(item: Int64; var Index: Integer): Boolean;
var L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := CompareValue(GetItem(I),item);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{***************************************************}
function TGosInt64List.GetItem(Index: Integer): Int64;
begin
  Result := PGosInt64ListItem(Get(index))^.FInt64
end;

{******************************}
constructor TGosInt64List.Create;
begin
  inherited Create;
  FOwnsObject := false;
end;

{****************************************************}
constructor TGosInt64List.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{**************************************************}
function TGosInt64List.IndexOf(Item: Int64): Integer;
begin
  if not Sorted then Begin
    Result := 0;
    while (Result < FCount) and (GetItem(result) <> Item) do Inc(Result);
    if Result = FCount then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{***************************************************************}
procedure TGosInt64List.Insert(Index: Integer; const Item: Int64);
begin
  InsertObject(Index, Item, nil);
end;

{***************************************************************************************}
procedure TGosInt64List.InsertObject(Index: Integer; const item: Int64; AObject: TObject);
Var aPALInt64ListItem: PGosInt64ListItem;
begin
  New(aPALInt64ListItem);
  aPALInt64ListItem^.FInt64 := item;
  aPALInt64ListItem^.FObject := AObject;
  try
    inherited insert(index,aPALInt64ListItem);
  except
    aPALInt64ListItem^.FObject := nil;
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{*********************************************************************}
procedure TGosInt64List.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then Gosfreeandnil(PGosInt64ListItem(Ptr).FObject)
    else PGosInt64ListItem(Ptr).FObject := nil; // to decrease the refcount in ARC
    dispose(ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{****************************************************************}
procedure TGosInt64List.SetItem(Index: Integer; const Item: Int64);
Var aPALInt64ListItem: PGosInt64ListItem;
begin
  New(aPALInt64ListItem);
  aPALInt64ListItem^.FInt64 := item;
  aPALInt64ListItem^.FObject := nil;
  Try
    Put(Index, aPALInt64ListItem);
  except
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{*******************************************************}
function TGosInt64List.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SGosListIndexError, Index);
  Result :=  PGosInt64ListItem(Get(index))^.FObject;
end;

{*************************************************************}
function TGosInt64List.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{*****************************************************************}
procedure TGosInt64List.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SGosListIndexError, Index);
  PGosInt64ListItem(Get(index))^.FObject := AObject;
end;

{**********************************************}
function TGosInt64List.Push(Item: int64): int64;
begin
  Add(Item);
  result := Item;
end;

{*******************************}
function TGosInt64List.Pop: int64;
begin
  Result := Peek;
  Delete(Count-1);
end;

{********************************}
function TGosInt64List.Peek: int64;
begin
  Result := GetItem(Count-1);
end;

{*******************************************}
function TGosInt64List.ToArray: TArray<int64>;
var i: integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[i] := GetItem(i);
end;

{************************************************************}
function TGosNativeIntList.Add(const Item: NativeInt): Integer;
begin
  Result := AddObject(Item, nil);
end;

{************************************************************************************}
function TGosNativeIntList.AddObject(const Item: NativeInt; AObject: TObject): Integer;
begin
  if not Sorted then Result := FCount
  else if Find(Item, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SGosDuplicateItem, 0);
    end;
  InsertItem(Result, Item, AObject);
end;

{***************************************************************}
function TGosNativeIntList.TryAdd(const Item: NativeInt): Boolean;
begin
  Result := TryAddObject(Item, nil);
end;

{***************************************************************************************}
function TGosNativeIntList.TryAddObject(const Item: NativeInt; AObject: TObject): Boolean;
var aIndex: integer;
begin
  if not Sorted then aIndex := FCount
  else if Find(Item, aIndex) then
    case Duplicates of
      dupIgnore,
      dupError: Exit(False);
    end;
  InsertItem(aIndex, Item, AObject);
  result := true;
end;

{*********************************************************************************************}
procedure TGosNativeIntList.InsertItem(Index: Integer; const item: NativeInt; AObject: TObject);
Var aPALNativeIntListItem: PGosNativeIntListItem;
begin
  New(aPALNativeIntListItem);
  aPALNativeIntListItem^.FNativeInt := item;
  aPALNativeIntListItem^.FObject := AObject;
  try
    inherited InsertItem(index,aPALNativeIntListItem);
  except
    aPALNativeIntListItem^.FObject := nil;
    Dispose(aPALNativeIntListItem);
    raise;
  end;
end;

{*****************************************************************************}
function TGosNativeIntList.CompareItems(const Index1, Index2: integer): Integer;
var aNativeInt1: Cardinal;
    aNativeInt2: Cardinal;
begin
  aNativeInt1 := PGosNativeIntListItem(Get(Index1))^.FNativeInt;
  aNativeInt2 := PGosNativeIntListItem(Get(Index2))^.FNativeInt;
  if aNativeInt1 < aNativeInt2 then result := -1
  else if aNativeInt1 > aNativeInt2 then result := 1
  else result := 0;
end;

{***************************************************************************}
function TGosNativeIntList.Find(item: NativeInt; var Index: Integer): Boolean;
var L, H, I, C: Integer;

  {------------------------------------------------------------}
  Function _CompareNativeInt(D1,D2: NativeInt): Integer; inline;
  Begin
    if D1 < D2 then result := -1
    else if D1 > D2 then result := 1
    else result := 0;
  end;

begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := _CompareNativeInt(GetItem(I),item);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{***********************************************************}
function TGosNativeIntList.GetItem(Index: Integer): NativeInt;
begin
  Result := PGosNativeIntListItem(Get(index))^.FNativeInt
end;

{**********************************}
constructor TGosNativeIntList.Create;
begin
  inherited Create;
  FOwnsObject := false;
end;

{********************************************************}
constructor TGosNativeIntList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{**********************************************************}
function TGosNativeIntList.IndexOf(Item: NativeInt): Integer;
begin
  if not Sorted then Begin
    Result := 0;
    while (Result < FCount) and (GetItem(result) <> Item) do Inc(Result);
    if Result = FCount then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{***********************************************************************}
procedure TGosNativeIntList.Insert(Index: Integer; const Item: NativeInt);
begin
  InsertObject(Index, Item, nil);
end;

{***********************************************************************************************}
procedure TGosNativeIntList.InsertObject(Index: Integer; const item: NativeInt; AObject: TObject);
Var aPALNativeIntListItem: PGosNativeIntListItem;
begin
  New(aPALNativeIntListItem);
  aPALNativeIntListItem^.FNativeInt := item;
  aPALNativeIntListItem^.FObject := AObject;
  try
    inherited insert(index,aPALNativeIntListItem);
  except
    aPALNativeIntListItem^.FObject := nil;
    Dispose(aPALNativeIntListItem);
    raise;
  end;
end;

{*************************************************************************}
procedure TGosNativeIntList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then Gosfreeandnil(PGosNativeIntListItem(Ptr).FObject)
    else PGosNativeIntListItem(Ptr).FObject := nil; // to decrease the refcount in ARC
    dispose(ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{************************************************************************}
procedure TGosNativeIntList.SetItem(Index: Integer; const Item: NativeInt);
Var aPALNativeIntListItem: PGosNativeIntListItem;
begin
  New(aPALNativeIntListItem);
  aPALNativeIntListItem^.FNativeInt := item;
  aPALNativeIntListItem^.FObject := nil;
  Try
    Put(Index, aPALNativeIntListItem);
  except
    Dispose(aPALNativeIntListItem);
    raise;
  end;
end;

{***********************************************************}
function TGosNativeIntList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SGosListIndexError, Index);
  Result :=  PGosNativeIntListItem(Get(index))^.FObject;
end;

{*****************************************************************}
function TGosNativeIntList.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{*********************************************************************}
procedure TGosNativeIntList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SGosListIndexError, Index);
  PGosNativeIntListItem(Get(index))^.FObject := AObject;
end;

{*********************************************************}
function TGosNativeIntList.Push(Item: NativeInt): NativeInt;
begin
  Add(Item);
  result := Item;
end;

{***************************************}
function TGosNativeIntList.Pop: NativeInt;
begin
  Result := Peek;
  Delete(Count-1);
end;

{****************************************}
function TGosNativeIntList.Peek: NativeInt;
begin
  Result := GetItem(Count-1);
end;

{***************************************************}
function TGosNativeIntList.ToArray: TArray<NativeInt>;
var i: integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[i] := GetItem(i);
end;

{******************************************************}
function TGosDoubleList.Add(const Item: Double): Integer;
begin
  Result := AddObject(Item, nil);
end;

{******************************************************************************}
function TGosDoubleList.AddObject(const Item: Double; AObject: TObject): Integer;
begin
  if not Sorted then Result := FCount
  else if Find(Item, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SGosDuplicateItem, 0);
    end;
  InsertItem(Result, Item, AObject);
end;

{*********************************************************}
function TGosDoubleList.TryAdd(const Item: Double): Boolean;
begin
  Result := TryAddObject(Item, nil);
end;

{*********************************************************************************}
function TGosDoubleList.TryAddObject(const Item: Double; AObject: TObject): Boolean;
var aIndex: integer;
begin
  if not Sorted then aIndex := FCount
  else if Find(Item, aIndex) then
    case Duplicates of
      dupIgnore,
      dupError: Exit(False);
    end;
  InsertItem(aIndex, Item, AObject);
  result := true;
end;

{***************************************************************************************}
procedure TGosDoubleList.InsertItem(Index: Integer; const item: Double; AObject: TObject);
Var aPALDoubleListItem: PGosDoubleListItem;
begin
  New(aPALDoubleListItem);
  aPALDoubleListItem^.FDouble := item;
  aPALDoubleListItem^.FObject := AObject;
  try
    inherited InsertItem(index,aPALDoubleListItem);
  except
    aPALDoubleListItem^.FObject := nil;
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{**************************************************************************}
function TGosDoubleList.CompareItems(const Index1, Index2: integer): Integer;
begin
  result := compareValue(PGosDoubleListItem(Get(Index1))^.FDouble, PGosDoubleListItem(Get(Index2))^.FDouble);
end;

{*********************************************************************}
function TGosDoubleList.Find(item: Double; var Index: Integer): Boolean;
var L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := compareValue(GetItem(I),item);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{*****************************************************}
function TGosDoubleList.GetItem(Index: Integer): Double;
begin
  Result := PGosDoubleListItem(Get(index))^.FDouble
end;

{*******************************}
constructor TGosDoubleList.Create;
begin
  inherited Create;
  FOwnsObject := false;
end;

{*****************************************************}
constructor TGosDoubleList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{****************************************************}
function TGosDoubleList.IndexOf(Item: Double): Integer;
begin
  if not Sorted then Begin
    Result := 0;
    while (Result < FCount) and (GetItem(result) <> Item) do Inc(Result);
    if sameValue(Result, FCount) then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{*****************************************************************}
procedure TGosDoubleList.Insert(Index: Integer; const Item: Double);
begin
  InsertObject(Index, Item, nil);
end;

{*****************************************************************************************}
procedure TGosDoubleList.InsertObject(Index: Integer; const item: Double; AObject: TObject);
Var aPALDoubleListItem: PGosDoubleListItem;
begin
  New(aPALDoubleListItem);
  aPALDoubleListItem^.FDouble := item;
  aPALDoubleListItem^.FObject := AObject;
  try
    inherited insert(index,aPALDoubleListItem);
  except
    aPALDoubleListItem^.FObject := nil;
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{**********************************************************************}
procedure TGosDoubleList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then Gosfreeandnil(PGosDoubleListItem(Ptr).FObject)
    else PGosDoubleListItem(Ptr).FObject := nil; // to decrease the refcount in ARC
    dispose(ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{******************************************************************}
procedure TGosDoubleList.SetItem(Index: Integer; const Item: Double);
Var aPALDoubleListItem: PGosDoubleListItem;
begin
  New(aPALDoubleListItem);
  aPALDoubleListItem^.FDouble := item;
  aPALDoubleListItem^.FObject := nil;
  Try
    Put(Index, aPALDoubleListItem);
  except
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{********************************************************}
function TGosDoubleList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SGosListIndexError, Index);
  Result :=  PGosDoubleListItem(Get(index))^.FObject;
end;

{**************************************************************}
function TGosDoubleList.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{******************************************************************}
procedure TGosDoubleList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SGosListIndexError, Index);
  PGosDoubleListItem(Get(index))^.FObject := AObject;
end;

{************************************************}
function TGosDoubleList.Push(Item: Double): Double;
begin
  Add(Item);
  result := Item;
end;

{*********************************}
function TGosDoubleList.Pop: Double;
begin
  Result := Peek;
  Delete(Count-1);
end;

{**********************************}
function TGosDoubleList.Peek: Double;
begin
  Result := GetItem(Count-1);
end;

{*********************************************}
function TGosDoubleList.ToArray: TArray<Double>;
var i: integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[i] := GetItem(i);
end;

end.
