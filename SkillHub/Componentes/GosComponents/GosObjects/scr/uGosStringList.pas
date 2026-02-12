{*************************************************************
Description:  TALStringList
              TALStringList Work the same as Delphi TstringList except that it's
              allow to search a name=value using a quicksort algorithm when the
              list is sorted. Also TALStringList use a locale independant
              algorithme (based on the 8-bit ordinal value of each character)
              instead of the AnsiCompareText and AnsiCompareStr used by the
              Delphi TstringList. at the end the sort in TALStringList is up to
              10x more faster than in Delphi TstringList. Also TALStringList is
              not an unicode TstringList but an 100% Ansi StringList

              TALNVStringList
              TALNVStringList (NV for NameValue) is same as TALStringList (use
              also a quicksort algorithme) except that here optimisation is
              oriented for name/value list instead of string list.

              TALAVLStringList
              TALAVLStringList is same as TALStringList except that it's use
              internally a self-balancing binary Tree instead of a quicksort
              algorithm

              TALHashedStringList
              TALHashedStringList is same as TALStringList except that it's use
              an internal hash table instead of a quicksort algorithm. By using
              TALHashedStringList instead of TALStringList, you can improve
              performance when the list contains a large number of strings
              (else if you list don't contain a lot of strings the performance
              is lower than TALStringList because of the cost to calculate the
              hash)
**************************************************************}

unit uGosStringList;

interface

Uses System.Classes,
     System.Sysutils,
     {$IFNDEF NEXTGEN}
     System.Contnrs,
     {$ENDIF}
     System.Generics.Defaults,
     System.Generics.Collections,
     System.Math,
     {$IFNDEF NEXTGEN}
     uGosAvlBinaryTRee,
     {$ENDIF}
     uGosQuickSortList;

{$IFNDEF NEXTGEN}

Type

  {-----------------}
  TGosStrings = class;

  {--------------------------}
  TGosStringsEnumerator = class
  private
    FIndex: Integer;
    FStrings: TGosStrings;
  public
    constructor Create(AStrings: TGosStrings);
    function GetCurrent: AnsiString;
    function MoveNext: Boolean; inline;
    property Current: AnsiString read GetCurrent;
  end;

  {-----------------------------}
  TGosStrings = class(TPersistent)
  private
    //[deleted from Tstrings] FEncoding: TEncoding;
    //[deleted from Tstrings] FDefaultEncoding: TEncoding;
    //[deleted from Tstrings] FWriteBOM: Boolean;
    //[deleted from Tstrings] procedure SetDefaultEncoding(const Value: TEncoding);
    //[deleted from Tstrings] procedure SetStringsAdapter(const Value: IStringsAdapter);
    //[deleted from Tstrings] FAdapter: IStringsAdapter;
    //[deleted from Tstrings] procedure ReadData(Reader: TReader);
    //[deleted from Tstrings] procedure WriteData(Writer: TWriter);
    FDelimiter: AnsiChar;
    FLineBreak: AnsiString;
    FQuoteChar: AnsiChar;
    FNameValueSeparator: AnsiChar;
    FStrictDelimiter: Boolean;
    FUpdateCount: Integer;
    FProtectedSave: Boolean;
    function GetCommaText: AnsiString;
    function GetDelimitedText: AnsiString;
    procedure SetCommaText(const Value: AnsiString);
    procedure SetDelimitedText(const Value: AnsiString);
  protected
    //[deleted from Tstrings] procedure SetEncoding(const Value: TEncoding);
    //[deleted from Tstrings] procedure DefineProperties(Filer: TFiler); override;
    function GetName(Index: Integer): AnsiString; virtual;
    function GetStrictName(Index: Integer): AnsiString; virtual; // [added from Tstrings]
    function GetValue(const Name: AnsiString): AnsiString; virtual;
    procedure SetValue(const Name, Value: AnsiString); virtual;
    function GetValueFromIndex(Index: Integer): AnsiString; virtual;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString); virtual;
    procedure SetPersistentValue(const Name, Value: AnsiString); virtual; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString); virtual; // [added from Tstrings]
    procedure Error(const Msg: String; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: AnsiString): AnsiString;
    function Get(Index: Integer): AnsiString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: AnsiString; virtual;
    procedure Put(Index: Integer; const S: AnsiString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: AnsiString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: AnsiString): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
  public
    //[deleted from Tstrings] procedure LoadFromFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    //[deleted from Tstrings] procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    //[deleted from Tstrings] procedure SaveToFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    //[deleted from Tstrings] procedure SaveToStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    //[deleted from Tstrings] property DefaultEncoding: TEncoding read FDefaultEncoding write SetDefaultEncoding;
    //[deleted from Tstrings] property Encoding: TEncoding read FEncoding;
    //[deleted from Tstrings] property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
    //[deleted from Tstrings] property StringsAdapter: IStringsAdapter read FAdapter write SetStringsAdapter;
    //[deleted from Tstrings] destructor Destroy; override;
    constructor Create; virtual;
    function Add(const S: AnsiString): Integer; virtual;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; virtual;
    function AddNameValue(const Name, Value: AnsiString): Integer; virtual; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer; virtual; // [added from Tstrings]
    procedure Append(const S: AnsiString);
    procedure AddStrings(Strings: TGosStrings); overload; virtual;
    procedure AddStrings(const Strings: TArray<AnsiString>); overload;
    procedure AddStrings(const Strings: TArray<AnsiString>; const Objects: TArray<TObject>); overload;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TGosStrings): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TGosStringsEnumerator; inline;
    function GetText: PAnsiChar; virtual;
    function IndexOf(const S: AnsiString): Integer; virtual;
    function IndexOfName(const Name: AnsiString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: AnsiString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); virtual;
    procedure InsertNameValue(Index: Integer; const Name, Value: AnsiString); virtual; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject); virtual; // [added from Tstrings]
    procedure LoadFromFile(const FileName: AnsiString); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: AnsiString); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(Text: PAnsiChar); virtual;
    function ToStringArray: TArray<AnsiString>;
    function ToObjectArray: TArray<TObject>;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: AnsiString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: AnsiChar read fDelimiter write fDelimiter;
    property DelimitedText: AnsiString read GetDelimitedText write SetDelimitedText;
    property LineBreak: AnsiString read fLineBreak write fLineBreak;
    property Names[Index: Integer]: AnsiString read GetName;
    property StrictNames[Index: Integer]: AnsiString read GetStrictName; // [added from Tstrings]
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: AnsiChar read fQuoteChar write fQuoteChar;
    property Values[const Name: AnsiString]: AnsiString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: AnsiString read GetValueFromIndex write SetValueFromIndex;
    property PersistentValues[const Name: AnsiString]: AnsiString read GetValue write SetPersistentValue; // [added from Tstrings]
    property PersistentValueFromIndex[Index: Integer]: AnsiString read GetValueFromIndex write SetPersistentValueFromIndex; // [added from Tstrings]
    property NameValueSeparator: AnsiChar read fNameValueSeparator write fNameValueSeparator;
    property StrictDelimiter: Boolean read fStrictDelimiter write fStrictDelimiter;
    property Strings[Index: Integer]: AnsiString read Get write Put; default;
    property Text: AnsiString read GetTextStr write SetTextStr;
    property ProtectedSave: Boolean read fProtectedSave write fProtectedSave;
  end;

  {--------------------}
  TGosStringList = class;

  {-----------------------------}
  PGosStringItem = ^TGosStringItem;
  TGosStringItem = record
    FString: AnsiString;
    FObject: TObject;
  end;

  {-----------------------------------------}
  TGosStringItemList = array of TGosStringItem;
  TGosStringListSortCompare = reference to function(List: TGosStringList; Index1, Index2: Integer): Integer;

  {-------------------------------}
  TGosStringList = class(TGosStrings)
  private
    FList: TGosStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    FNameValueOptimization: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TGosStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: AnsiString): Integer; override;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean); virtual; //[added from TStringList]
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: AnsiString; var Index: Integer): Boolean; virtual;
    function FindName(const S: AnsiString; var Index: Integer): Boolean; // [added from TStringList]
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TGosStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    property NameValueOptimization: Boolean read FNameValueOptimization write FNameValueOptimization;
  end;

  {----------------------}
  TGosNVStringList = class;

  {---------------------------------}
  PGosNVStringItem = ^TGosNVStringItem;
  TGosNVStringItem = record
    FName: AnsiString;
    FValue: AnsiString;
    FNVS: Boolean;
    FObject: TObject;
  end;

  {---------------------------------------------}
  TGosNVStringItemList = array of TGosNVStringItem;
  TGosNVStringListSortCompare = reference to function(List: TGosNVStringList; Index1, Index2: Integer): Integer;

  {---------------------------------}
  TGosNVStringList = class(TGosStrings)
  private
    FList: TGosNVStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TGosNVStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    Function ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean; //[added from TStringList]
  protected
    function GetName(Index: Integer): AnsiString; override;
    function GetStrictName(Index: Integer): AnsiString; override; // [added from Tstrings]
    function GetValue(const Name: AnsiString): AnsiString; override;
    procedure SetValue(const Name, Value: AnsiString); override;
    function GetValueFromIndex(Index: Integer): AnsiString; override;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString); override;
    procedure SetPersistentValue(const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString); override; // [added from Tstrings]
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetTextStr: AnsiString; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: AnsiString): Integer; override;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); overload; virtual;
    procedure InsertItem(Index: Integer; const Name: AnsiString; WithNvS: boolean; AObject: TObject); overload; virtual; //[added from TStringList]
    procedure InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject); overload; virtual; //[added from TStringList]
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean); virtual; //[added from TStringList]
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    function AddNameValue(const Name, Value: AnsiString): Integer; override; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer; override; // [added from Tstrings]
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: AnsiString; var Index: Integer): Boolean; virtual;
    function FindName(const Name: AnsiString; var Index: Integer): Boolean; overload; // [added from TStringList]
    function FindName(const Name: AnsiString; WithNvS: boolean; var Index: Integer): Boolean; overload; // [added from TStringList]
    function FindNameValue(const Name, Value: AnsiString; var Index: Integer): Boolean;  // [added from TStringList]
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure InsertNameValue(Index: Integer; const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject); override; // [added from Tstrings]
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TGosNVStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

  {-----------------------}
  TGosAVLStringList = class;
  TGosAVLStringListSortCompare = function(List: TGosAVLStringList; Index1, Index2: Integer): Integer;

  {-------------------------------------------------------------------}
  TGosAVLStringListBinaryTreeNode = class(TGosStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Val: AnsiString;  // Value
    Obj: Tobject; // Object
    Idx: integer; // Index in the NodeList
    Nvs: Boolean; // if NameValueSeparator was present
  end;

  {-------------------------------}
  //when duplicate is set to ignore
  //it's take care only about the "name" part
  //and ignore the value part. exemple
  //if name1=value2 is in the list and
  //we add name1=value3 then if ignore duplicates
  //no error will be raise and the add command
  //will be silently ignored
  TGosAVLStringList = class(TGosStrings)
  private
    FNodeList: TObjectList;
    FAVLBinTree: TGosStringKeyAVLBinaryTree;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure QuickSort(L, R: Integer; SCompare: TGosAVLStringListSortCompare);
    procedure SetCaseSensitive(const Value: Boolean);
    function GetCaseSensitive: Boolean;
    Function ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
    procedure SetDuplicates(const Value: TDuplicates);
  protected
    function GetName(Index: Integer): AnsiString; override;
    function GetStrictName(Index: Integer): AnsiString; override; // [added from Tstrings]
    function GetValue(const Name: AnsiString): AnsiString; override;
    procedure SetValue(const Name, Value: AnsiString); override;
    function GetValueFromIndex(Index: Integer): AnsiString; override;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString); override;
    procedure SetPersistentValue(const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString); override; // [added from Tstrings]
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetTextStr: AnsiString; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject); overload; virtual;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); overload; virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean); virtual; //[added from TStringList]
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    function AddNameValue(const Name, Value: AnsiString): Integer; override; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer; override; // [added from Tstrings]
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure InsertNameValue(Index: Integer; const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject); override; // [added from Tstrings]
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure CustomSort(Compare: TGosAVLStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

  {--------------------------}
  TGosHashedStringList = class;
  TGosHashedStringListSortCompare = function(List: TGosHashedStringList; Index1, Index2: Integer): Integer;

  {-------------------------------------------------}
  TGosHashedStringListDictionaryNode = class(Tobject)
  Private
  Protected
  Public
    ID: AnsiString; // Name
    Val: AnsiString;  // Value
    Obj: Tobject; // Object
    Idx: integer; // Index in the NodeList
    Nvs: Boolean; // if NameValueSeparator was present
  end;

  {----------------------------------}
  //when duplicate is set to ignore
  //it's take care only about the "name" part
  //and ignore the value part. exemple
  //if name1=value2 is in the list and
  //we add name1=value3 then if ignore duplicates
  //no error will be raise and the add command
  //will be silently ignored
  TGosHashedStringList = class(TGosStrings)
  private
    FNodeList: TObjectList<TGosHashedStringListDictionaryNode>;
    FDictionary: TObjectDictionary<ansiString, TGosHashedStringListDictionaryNode>;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    FCaseSensitive: boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure QuickSort(L, R: Integer; SCompare: TGosHashedStringListSortCompare);
    procedure SetCaseSensitive(const Value: Boolean);
    function GetCaseSensitive: Boolean;
    Function ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
    procedure SetDuplicates(const Value: TDuplicates);
    function CreateDictionary(ACapacity: integer; aCaseSensitive: boolean): TObjectDictionary<ansiString, TGosHashedStringListDictionaryNode>;
  protected
    function GetName(Index: Integer): AnsiString; override;
    function GetStrictName(Index: Integer): AnsiString; override; // [added from Tstrings]
    function GetValue(const Name: AnsiString): AnsiString; override;
    procedure SetValue(const Name, Value: AnsiString); override;
    function GetValueFromIndex(Index: Integer): AnsiString; override;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString); override;
    procedure SetPersistentValue(const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString); override; // [added from Tstrings]
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetTextStr: AnsiString; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject); overload; virtual;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); overload; virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean; ACapacity: Integer); virtual; //[added from TStringList]
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    constructor Create(ACapacity: Integer); reintroduce; overload; //[added from Tstrings]
    constructor Create(OwnsObjects: Boolean; ACapacity: Integer); reintroduce; overload; //[added from Tstrings]
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    function AddNameValue(const Name, Value: AnsiString): Integer; override; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer; override; // [added from Tstrings]
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure InsertNameValue(Index: Integer; const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject); override; // [added from Tstrings]
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure CustomSort(Compare: TGosHashedStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

{$ENDIF}

type

  {------------------}
  TALStringsU = class;

  {---------------------------}
  TALStringsEnumeratorU = class
  private
    FIndex: Integer;
    FStrings: TALStringsU;
  public
    constructor Create(AStrings: TALStringsU);
    function GetCurrent: String;
    function MoveNext: Boolean; inline;
    property Current: String read GetCurrent;
  end;

  {------------------------}
  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if System.classes.TStrings didn''t change and adjust the IFDEF'}
  {$IFEND}
  TALStringsU = class(TPersistent)
  private
    //[deleted from Tstrings] procedure SetStringsAdapter(const Value: IStringsAdapter);
    //[deleted from Tstrings] FAdapter: IStringsAdapter;
    //[deleted from Tstrings] procedure ReadData(Reader: TReader);
    //[deleted from Tstrings] procedure WriteData(Writer: TWriter);
    FEncoding: TEncoding;
    FDefaultEncoding: TEncoding;
    FDelimiter: Char;
    FLineBreak: String;
    FQuoteChar: Char;
    FNameValueSeparator: Char;
    FStrictDelimiter: Boolean;
    FUpdateCount: Integer;
    FWriteBOM: Boolean;
    FProtectedSave: Boolean;
    function GetCommaText: String;
    function GetDelimitedText: String;
    procedure SetCommaText(const Value: String);
    procedure SetDelimitedText(const Value: String);
    procedure SetDefaultEncoding(const Value: TEncoding);
  protected
    //[deleted from Tstrings] procedure DefineProperties(Filer: TFiler); override;
    function GetName(Index: Integer): String; virtual;
    function GetStrictName(Index: Integer): String; virtual; // [added from Tstrings]
    function GetValue(const Name: String): String; virtual;
    procedure SetValue(const Name, Value: String); virtual;
    function GetValueFromIndex(Index: Integer): String; virtual;
    procedure SetValueFromIndex(Index: Integer; const Value: String); virtual;
    procedure SetPersistentValue(const Name, Value: String); virtual; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: String); virtual; // [added from Tstrings]
    procedure Error(const Msg: String; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: String): String;
    function Get(Index: Integer): String; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: String; virtual;
    procedure Put(Index: Integer; const S: String); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetEncoding(const Value: TEncoding); virtual;
    procedure SetTextStr(const Value: String); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: String): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
  public
    //[deleted from Tstrings] property StringsAdapter: IStringsAdapter read FAdapter write SetStringsAdapter;
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const S: String): Integer; virtual;
    function AddObject(const S: String; AObject: TObject): Integer; virtual;
    function AddNameValue(const Name, Value: String): Integer; virtual; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: String; AObject: TObject): Integer; virtual; // [added from Tstrings]
    procedure Append(const S: String);
    procedure AddStrings(Strings: TALStringsU); overload; virtual;
    procedure AddStrings(const Strings: TArray<String>); overload;
    procedure AddStrings(const Strings: TArray<String>; const Objects: TArray<TObject>); overload;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TALStringsU): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TALStringsEnumeratorU; inline;
    function GetText: PChar; virtual;
    function IndexOf(const S: String): Integer; virtual;
    function IndexOfName(const Name: String): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: String); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: String; AObject: TObject); virtual;
    procedure InsertNameValue(Index: Integer; const Name, Value: String); virtual; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: String; AObject: TObject); virtual; // [added from Tstrings]
    procedure LoadFromFile(const FileName: String); overload; virtual;
    procedure LoadFromFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    procedure LoadFromStream(Stream: TStream); overload; virtual;
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: String); overload; virtual;
    procedure SaveToFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    procedure SaveToStream(Stream: TStream); overload; virtual;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    procedure SetText(Text: PChar); virtual;
    function ToStringArray: TArray<String>;
    function ToObjectArray: TArray<TObject>;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: String read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property DefaultEncoding: TEncoding read FDefaultEncoding write SetDefaultEncoding;
    property Delimiter: Char read fDelimiter write fDelimiter;
    property DelimitedText: String read GetDelimitedText write SetDelimitedText;
    property Encoding: TEncoding read FEncoding;
    property LineBreak: String read fLineBreak write fLineBreak;
    property Names[Index: Integer]: String read GetName;
    property StrictNames[Index: Integer]: String read GetStrictName; // [added from Tstrings]
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: Char read fQuoteChar write fQuoteChar;
    property Values[const Name: String]: String read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: String read GetValueFromIndex write SetValueFromIndex;
    property PersistentValues[const Name: String]: String read GetValue write SetPersistentValue; // [added from Tstrings]
    property PersistentValueFromIndex[Index: Integer]: String read GetValueFromIndex write SetPersistentValueFromIndex; // [added from Tstrings]
    property NameValueSeparator: Char read fNameValueSeparator write fNameValueSeparator;
    property StrictDelimiter: Boolean read fStrictDelimiter write fStrictDelimiter;
    property Strings[Index: Integer]: String read Get write Put; default;
    property Text: String read GetTextStr write SetTextStr;
    property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
    property ProtectedSave: Boolean read fProtectedSave write fProtectedSave;
  end;

  {---------------------}
  TALStringListU = class;

  {-------------------------------}
  PALStringItemU = ^TALStringItemU;
  TALStringItemU = record
    FString: String;
    FObject: TObject;
  end;

  {-------------------------------------------}
  TALStringItemListU = array of TALStringItemU;
  TALStringListSortCompareU = reference to function(List: TALStringListU; Index1, Index2: Integer): Integer;

  {------------------------}
  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if System.classes.TStringList didn''t change and adjust the IFDEF'}
  {$IFEND}
  TALStringListU = class(TALStringsU)
  private
    FList: TALStringItemListU;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    FNameValueOptimization: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TALStringListSortCompareU);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): String; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: String): Integer; override;
    procedure InsertItem(Index: Integer; const S: String; AObject: TObject); virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean); virtual; //[added from TStringList]
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    destructor Destroy; override;
    function Add(const S: String): Integer; override;
    function AddObject(const S: String; AObject: TObject): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: String; var Index: Integer): Boolean; virtual;
    function FindName(const S: String; var Index: Integer): Boolean; // [added from TStringList]
    function IndexOf(const S: String): Integer; override;
    function IndexOfName(const Name: String): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: String); override;
    procedure InsertObject(Index: Integer; const S: String; AObject: TObject); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TALStringListSortCompareU); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    property NameValueOptimization: Boolean read FNameValueOptimization write FNameValueOptimization;
  end;

  {-----------------------}
  TALNVStringListU = class;

  {-----------------------------------}
  PALNVStringItemU = ^TALNVStringItemU;
  TALNVStringItemU = record
    FName: String;
    FValue: String;
    FNVS: Boolean;
    FObject: TObject;
  end;

  {-----------------------------------------------}
  TALNVStringItemListU = array of TALNVStringItemU;
  TALNVStringListSortCompareU = reference to function(List: TALNVStringListU; Index1, Index2: Integer): Integer;

  {-----------------------------------}
  TALNVStringListU = class(TALStringsU)
  private
    FList: TALNVStringItemListU;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TALNVStringListSortCompareU);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    Function ExtractNameValue(const S: String; var Name, Value: String): Boolean; //[added from TStringList]
  protected
    function GetName(Index: Integer): String; override;
    function GetStrictName(Index: Integer): String; override; // [added from Tstrings]
    function GetValue(const Name: String): String; override;
    procedure SetValue(const Name, Value: String); override;
    function GetValueFromIndex(Index: Integer): String; override;
    procedure SetValueFromIndex(Index: Integer; const Value: String); override;
    procedure SetPersistentValue(const Name, Value: String); override; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: String); override; // [added from Tstrings]
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): String; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetTextStr: String; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: String): Integer; override;
    procedure InsertItem(Index: Integer; const S: String; AObject: TObject); overload; virtual;
    procedure InsertItem(Index: Integer; const Name: String; WithNvS: boolean; AObject: TObject); overload; virtual; //[added from TStringList]
    procedure InsertItem(Index: Integer; const Name, Value: String; AObject: TObject); overload; virtual; //[added from TStringList]
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean); virtual; //[added from TStringList]
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    destructor Destroy; override;
    function Add(const S: String): Integer; override;
    function AddObject(const S: String; AObject: TObject): Integer; override;
    function AddNameValue(const Name, Value: String): Integer; override; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: String; AObject: TObject): Integer; override; // [added from Tstrings]
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: String; var Index: Integer): Boolean; virtual;
    function FindName(const Name: String; var Index: Integer): Boolean; overload; // [added from TStringList]
    function FindName(const Name: String; WithNvS: boolean; var Index: Integer): Boolean; overload; // [added from TStringList]
    function FindNameValue(const Name, Value: String; var Index: Integer): Boolean;  // [added from TStringList]
    function IndexOf(const S: String): Integer; override;
    function IndexOfName(const Name: String): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: String); override;
    procedure InsertObject(Index: Integer; const S: String; AObject: TObject); override;
    procedure InsertNameValue(Index: Integer; const Name, Value: String); override; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: String; AObject: TObject); override; // [added from Tstrings]
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TALNVStringListSortCompareU); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

implementation

Uses System.RTLConsts,
     system.IOUtils,
     {$IFNDEF NEXTGEN}
     System.Ansistrings,
     System.Hash,
     {$ENDIF}
     uGosString,
     uGosCommon;

{$IFNDEF NEXTGEN}

{************************************************************}
constructor TGosStringsEnumerator.Create(AStrings: TGosStrings);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

{***************************************************}
function TGosStringsEnumerator.GetCurrent: AnsiString;
begin
  Result := FStrings[FIndex];
end;

{**********************************************}
function TGosStringsEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FStrings.Count;
end;

{****************************}
constructor TGosStrings.Create;
begin
  inherited Create;
  FDelimiter := ',';
  FLineBreak := sLineBreak;
  FQuoteChar := '"';
  FNameValueSeparator := '=';
  FStrictDelimiter:= False;
  FUpdateCount:= 0;
  fProtectedSave := False;
end;

{****************************************************}
function TGosStrings.Add(const S: AnsiString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

{****************************************************************************}
function TGosStrings.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

{***********************************************************************}
function TGosStrings.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  result := add(name + NameValueSeparator + Value);
end;

{***********************************************************************************************}
function TGosStrings.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  result := addObject(name + NameValueSeparator + Value, AObject);
end;

{***********************************************}
procedure TGosStrings.Append(const S: AnsiString);
begin
  Add(S);
end;

{***************************************************}
procedure TGosStrings.AddStrings(Strings: TGosStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

{*****************************************************************}
procedure TGosStrings.AddStrings(const Strings: TArray<AnsiString>);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      Add(Strings[I]);
  finally
    EndUpdate;
  end;
end;

{*************************************************************************************************}
procedure TGosStrings.AddStrings(const Strings: TArray<AnsiString>; const Objects: TArray<TObject>);
var
  I: Integer;
begin
  if Length(Strings) <> Length(Objects) then
    raise EArgumentOutOfRangeException.CreateRes(@System.RTLConsts.sInvalidStringAndObjectArrays);
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      AddObject(Strings[I], Objects[I]);
  finally
    EndUpdate;
  end;
end;

{***********************************************}
procedure TGosStrings.Assign(Source: TPersistent);
var i: integer;
begin
  if Source is TGosStrings then
  begin
    BeginUpdate;
    try
      Clear;
      NameValueSeparator := TGosStrings(Source).NameValueSeparator;
      QuoteChar := TGosStrings(Source).QuoteChar;
      Delimiter := TGosStrings(Source).Delimiter;
      LineBreak := TGosStrings(Source).LineBreak;
      StrictDelimiter := TGosStrings(Source).StrictDelimiter;
      AddStrings(TGosStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      NameValueSeparator := AnsiChar(TStrings(Source).NameValueSeparator);
      QuoteChar := AnsiChar(TStrings(Source).QuoteChar);
      Delimiter := AnsiChar(TStrings(Source).Delimiter);
      LineBreak := AnsiString(TStrings(Source).LineBreak);
      StrictDelimiter := TStrings(Source).StrictDelimiter;
      for I := 0 to Tstrings(Source).Count - 1 do
        AddObject(Ansistring(Tstrings(Source)[I]), Tstrings(Source).Objects[I]);
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

{***********************************************}
procedure TGosStrings.AssignTo(Dest: TPersistent);
var i: integer;
begin
  if Dest is TStrings then
  begin
    Tstrings(Dest).BeginUpdate;
    try
      Tstrings(Dest).Clear;
      Tstrings(Dest).NameValueSeparator := char(NameValueSeparator);
      Tstrings(Dest).QuoteChar := char(QuoteChar);
      Tstrings(Dest).Delimiter := char(Delimiter);
      Tstrings(Dest).LineBreak := String(LineBreak);
      Tstrings(Dest).StrictDelimiter := StrictDelimiter;
      for I := 0 to Count - 1 do
        Tstrings(Dest).AddObject(string(get(I)), Objects[I]);
    finally
      Tstrings(Dest).EndUpdate;
    end;
    Exit;
  end;
  inherited AssignTo(Dest);
end;

{*******************************}
procedure TGosStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

{*****************************}
procedure TGosStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

{*******************************************************}
function TGosStrings.Equals(Strings: TGosStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

{***********************************************************}
procedure TGosStrings.Error(const Msg: String; Data: Integer);
begin
  raise EStringListError.CreateFmt(Msg, [Data]);
end;

{************************************************************}
procedure TGosStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  raise EStringListError.CreateFmt(LoadResString(Msg), [Data]);
end;

{*****************************************************}
procedure TGosStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: AnsiString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

{***************************************************************}
function TGosStrings.ExtractName(const S: AnsiString): AnsiString;
var
  P: Integer;
begin
  Result := S;
  P := ALPos(NameValueSeparator, Result);

  // change behavior from original Tstring
  // i thing that if a Tstring have an item
  //
  // item1
  //
  // then set MyTstrings.values[item1] := Value1
  // must do
  //
  // item1=Value1
  //
  // instead of what he actually do
  //
  // item1
  // item1=Value1
  //
  // also when MyTStrings contain
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4
  // item5=Value5
  //
  // then doing
  // MyTStrings.valueFromIndex[4] := 'value4'
  // must result in
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4=Value4
  // item5=Value5
  //
  // instead of the current behavior of Tstrings
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // =Value4
  // item5=Value5
  //
  //
  // Original function:
  //
  // if P <> 0 then
  //   SetLength(Result, P-1) else
  //   SetLength(Result, 0);

  if P <> 0 then SetLength(Result, P-1);
end;

{***************************************}
function TGosStrings.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

{*******************************************}
function TGosStrings.GetCommaText: AnsiString;
var
  LOldDelimiter: AnsiChar;
  LOldQuoteChar: AnsiChar;
begin
  LOldDelimiter := Delimiter;
  LOldQuoteChar := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    Delimiter := LOldDelimiter;
    QuoteChar := LOldQuoteChar;
  end;
end;

{***********************************************}
function TGosStrings.GetDelimitedText: AnsiString;
var
  S: AnsiString;
  P: PAnsiChar;
  I, Count: Integer;
  LDelimiters: TSysCharSet;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    LDelimiters := [AnsiChar(#0), AnsiChar(QuoteChar), AnsiChar(Delimiter)];
    if not StrictDelimiter then
      LDelimiters := LDelimiters + [AnsiChar(#1)..AnsiChar(' ')];
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PAnsiChar(S);
      while not (P^ in LDelimiters) do
        Inc(P);
      if (P^ <> #0) then S := ALQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

{******************************************************}
function TGosStrings.GetEnumerator: TGosStringsEnumerator;
begin
  Result := TGosStringsEnumerator.Create(Self);
end;

{******************************************************}
function TGosStrings.GetName(Index: Integer): AnsiString;
begin
  Result := ExtractName(Get(Index));
end;

{************************************************************}
function TGosStrings.GetStrictName(Index: Integer): AnsiString;
var P: Integer;
begin
  Result := Get(Index);
  P := ALPos(NameValueSeparator, Result);
  if P <> 0 then SetLength(Result, P-1)
  else SetLength(Result, 0);
end;

{*****************************************************}
function TGosStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

{*************************************}
function TGosStrings.GetText: PAnsiChar;
begin
  Result := System.Ansistrings.StrNew(PAnsiChar(GetTextStr));
end;

{*****************************************}
function TGosStrings.GetTextStr: AnsiString;
var
  I, L, Size, Count: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      GosMove(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      GosMove(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

{***************************************************************}
function TGosStrings.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := ALCopyStr(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

{********************************************************}
function TGosStrings.IndexOf(const S: AnsiString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

{***************************************************************}
function TGosStrings.IndexOfName(const Name: AnsiString): Integer;
var
  P: Integer;
  S: AnsiString;
begin

  // change behavior from original Tstring
  // i thing that if a Tstring have an item
  //
  // item1
  //
  // then set MyTstrings.values[item1] := Value1
  // must do
  //
  // item1=Value1
  //
  // instead of what he actually do
  //
  // item1
  // item1=Value1
  //
  // also when MyTStrings contain
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4
  // item5=Value5
  //
  // then doing
  // MyTStrings.valueFromIndex[4] := 'value4'
  // must result in
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4=Value4
  // item5=Value5
  //
  // instead of the current behavior of Tstrings
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // =Value4
  // item5=Value5
  //
  //
  // Original function:
  //
  // for Result := 0 to GetCount - 1 do
  // begin
  //   S := Get(Result);
  //   P := ALPos(NameValueSeparator, S);
  //   if (P <> 0) and (CompareStrings(ALCopyStr(S, 1, P - 1), Name) = 0) then Exit;
  // end;
  // Result := -1;

  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := ALPos(NameValueSeparator, S);
    if ((P <> 0) and (CompareStrings(ALCopyStr(S, 1, P - 1), Name) = 0)) or
       ((P = 0) and (CompareStrings(S, Name) = 0)) then Exit;
  end;
  Result := -1;
end;

{***********************************************************}
function TGosStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{***************************************************************************************}
procedure TGosStrings.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

{**********************************************************************************}
procedure TGosStrings.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  Insert(Index, name + NameValueSeparator + Value);
end;

{**********************************************************************************************************}
procedure TGosStrings.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  InsertObject(Index, name + NameValueSeparator + Value, AObject);
end;

{************************************************************}
procedure TGosStrings.LoadFromFile(const FileName: AnsiString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(String(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    GosFreeAndNil(Stream);
  end;
end;

{***************************************************}
procedure TGosStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.ReadBuffer(Pointer(S)^, Size);
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

{*****************************************************}
procedure TGosStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: AnsiString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      PutObject(CurIndex, nil);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************}
procedure TGosStrings.Put(Index: Integer; const S: AnsiString);
begin
end;

{***************************************************************}
procedure TGosStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

{**********************************************************}
procedure TGosStrings.SaveToFile(const FileName: AnsiString);
Var afileStream: TfileStream;
    aTmpFilename: AnsiString;
begin
  if ProtectedSave then aTmpFilename := FileName + '.~tmp'
  else aTmpFilename := FileName;
  try

    aFileStream := TfileStream.Create(String(aTmpFilename),fmCreate);
    Try
      SaveToStream(aFileStream);
    finally
      GosFreeAndNil(aFileStream);
    end;

    if aTmpFilename <> FileName then begin
      if TFile.Exists(String(FileName)) then TFile.Delete(String(FileName));
      TFile.Move(String(aTmpFilename), String(FileName));
    end;

  except
    if (aTmpFilename <> FileName) and
       (TFile.Exists(String(aTmpFilename))) then TFile.Delete(String(aTmpFilename));
    raise;
  end;
end;

{*************************************************}
procedure TGosStrings.SaveToStream(Stream: TStream);
var
  S: AnsiString;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

{*****************************************************}
procedure TGosStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

{*********************************************************}
procedure TGosStrings.SetCommaText(const Value: AnsiString);
var
  LOldDelimiter: AnsiChar;
  LOldQuoteChar: AnsiChar;
begin
  LOldDelimiter := Delimiter;
  LOldQuoteChar := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    SetDelimitedText(Value);
  finally
    Delimiter := LOldDelimiter;
    QuoteChar := LOldQuoteChar;
  end;
end;

{********************************************}
procedure TGosStrings.SetText(Text: PAnsiChar);
begin
  SetTextStr(Text);
end;

{*******************************************************}
procedure TGosStrings.SetTextStr(const Value: AnsiString);
var
  P, Start, LB: PAnsiChar;
  S: AnsiString;
  LineBreakLen: Integer;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      if ALCompareStr(LineBreak, sLineBreak) = 0 then
      begin
        // This is a lot faster than using StrPos/AnsiStrPos when
        // LineBreak is the default (#13#10)
        while P^ <> #0 do
        begin
          Start := P;
          while not (P^ in [#0, #10, #13]) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P^ = #13 then Inc(P);
          if P^ = #10 then Inc(P);
        end;
      end
      else
      begin
        LineBreakLen := Length(LineBreak);
        while P^ <> #0 do
        begin
          Start := P;
          LB := System.Ansistrings.StrPos(P, PAnsiChar(LineBreak));
          while (P^ <> #0) and (P <> LB) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P = LB then
            Inc(P, LineBreakLen);
        end;
      end;
  finally
    EndUpdate;
  end;
end;

{*****************************************************}
procedure TGosStrings.SetUpdateState(Updating: Boolean);
begin
end;

{***********************************************************}
procedure TGosStrings.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then Add(Name + NameValueSeparator + Value)
    else Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{*********************************************************************}
procedure TGosStrings.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then Add(Name + NameValueSeparator + Value)
  else Put(I, Name + NameValueSeparator + Value);
end;

{*************************************************************}
procedure TGosStrings.SetDelimitedText(const Value: AnsiString);
var
  P, P1: PAnsiChar;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Clear;
    P := PAnsiChar(Value);
    if not StrictDelimiter then
      while P^ in [#1..' '] do
        Inc(P);
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := ALExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while ((not StrictDelimiter and (P^ > ' ')) or
              (StrictDelimiter and (P^ <> #0))) and (P^ <> Delimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not StrictDelimiter then
        while P^ in [#1..' '] do
          Inc(P);

      if P^ = Delimiter then
      begin
        P1 := P;
        Inc(P1);
        if P1^ = #0 then
          Add('');
        repeat
          Inc(P);
        until not (not StrictDelimiter and (P^ in [#1..' ']));
      end;
    end;
  finally
    EndUpdate;
  end;
end;

{********************************************************************}
function TGosStrings.CompareStrings(const S1, S2: AnsiString): Integer;
begin
  Result := ALCompareText(S1, S2);
end;

{****************************************************************}
function TGosStrings.GetValueFromIndex(Index: Integer): AnsiString;
var
  SepPos: Integer;
begin
  if Index >= 0 then
  begin
    Result := Get(Index);
    SepPos := ALPos(NameValueSeparator, Result);
    if (SepPos > 0) then
      System.Delete(Result, 1, SepPos)
    else
      Result := '';
  end
  else
    Result := '';
end;

{******************************************************************************}
procedure TGosStrings.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Add(NameValueSeparator + Value)
    else Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

{****************************************************************************************}
procedure TGosStrings.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then Add(NameValueSeparator + Value)
  else Put(Index, Names[Index] + NameValueSeparator + Value);
end;

{****************************************************}
function TGosStrings.ToStringArray: TArray<AnsiString>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Strings[I];
end;

{*************************************************}
function TGosStrings.ToObjectArray: TArray<TObject>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Objects[I];
end;

{*******************************}
destructor TGosStringList.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do
      Temp[I] := FList[I].FObject;
  end;

  inherited Destroy;
  FCount := 0;
  SetCapacity(0);

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      GosFreeAndNil(Temp[I]);
end;

{*******************************************************}
function TGosStringList.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{*******************************************************************************}
function TGosStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

{**************************************************}
procedure TGosStringList.Assign(Source: TPersistent);
begin
  if Source is TGosStringList then
  begin
    Clear;
    FCaseSensitive := TGosStringList(Source).FCaseSensitive;
    FDuplicates := TGosStringList(Source).FDuplicates;
    FSorted := TGosStringList(Source).FSorted;
  end
  else if Source is TGosNVStringList then
  begin
    Clear;
    FCaseSensitive := TGosNVStringList(Source).FCaseSensitive;
    FDuplicates := TGosNVStringList(Source).FDuplicates;
    FSorted := TGosNVStringList(Source).FSorted;
  end
  else if Source is TGosHashedStringList then
  begin
    Clear;
    FCaseSensitive := TGosHashedStringList(Source).CaseSensitive;
    FDuplicates := TGosHashedStringList(Source).FDuplicates;
    FSorted := False;
  end
  else if Source is TGosAVLStringList then
  begin
    Clear;
    FCaseSensitive := TGosAVLStringList(Source).CaseSensitive;
    FDuplicates := TGosAVLStringList(Source).FDuplicates;
    FSorted := False;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    Duplicates := TStringList(Source).Duplicates;
    Sorted := TStringList(Source).Sorted;
  end;
  inherited Assign(Source);
end;

{**************************************************}
procedure TGosStringList.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := fCaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := fSorted;
  end;
  inherited AssignTo(Dest);
end;

{******************************}
procedure TGosStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{*******************************}
procedure TGosStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{****************************}
procedure TGosStringList.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if FCount <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, FCount);
      for I := 0 to FCount - 1 do
        Temp[I] := FList[I].FObject;
    end;

    FCount := 0;
    SetCapacity(0);

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        GosFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{*********************************************}
procedure TGosStringList.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    GosMove(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(TGosStringItem));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FString)^ := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    GosFreeAndNil(Obj);
  Changed;
end;

{*************************************************************}
function  TGosStringList.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  result := FList[Index].FObject;
  FList[Index].FObject := nil;
  Changed;
end;

{********************************************************}
procedure TGosStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{*************************************************************}
procedure TGosStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PGosStringItem;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];
  Temp := Pointer(Item1^.FString);
  Pointer(Item1^.FString) := Pointer(Item2^.FString);
  Pointer(Item2^.FString) := Temp;
  Temp := Pointer(Item1^.FObject);
  Pointer(Item1^.FObject) := Pointer(Item2^.FObject);
  Pointer(Item2^.FObject) := Temp;
end;

{****************************************************************************}
function TGosStringList.Find(const S: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{********************************************************************************}
function TGosStringList.FindName(const S: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(ExtractName(FList[I].FString), S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          if CompareStrings(ExtractName(FList[I].FString), s) = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{*****************************************************}
function TGosStringList.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FString;
end;

{******************************************}
function TGosStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

{***************************************}
function TGosStringList.GetCount: Integer;
begin
  Result := FCount;
end;

{********************************************************}
function TGosStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FObject;
end;

{***************************}
procedure TGosStringList.Grow;
{$IF CompilerVersion <= 32}{tokyo}
var
  Delta: Integer;
{$endif}
begin
  {$IF CompilerVersion <= 32}{tokyo}
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
  {$else}
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
  {$endif}
end;

{***********************************************************}
function TGosStringList.IndexOf(const S: AnsiString): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

{******************************************************************}
function TGosStringList.IndexOfName(const Name: ansistring): Integer;
begin
  if (not Sorted) or (not FNameValueOptimization) then Result := inherited IndexOfName(Name)
  else if not FindName(Name, Result) then Result := -1;
end;

{******************************************************************}
procedure TGosStringList.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{******************************************************************************************}
procedure TGosStringList.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{********************************************************}
procedure TGosStringList.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: AnsiString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      FList[CurIndex].FObject := nil;
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
procedure TGosStringList.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    GosMove(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TGosStringItem));
  Pointer(FList[Index].FString) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FString := S;
  Inc(FCount);
  Changed;
end;

{***************************************************************}
procedure TGosStringList.Put(Index: Integer; const S: AnsiString);
begin
  if not sorted then begin
    if Cardinal(Index) >= Cardinal(FCount) then
      Error(@SListIndexError, Index);
    Changing;
    FList[Index].FString := S;
    Changed;
  end
  else begin
    delete(index);
    add(s);
  end;
end;

{******************************************************************}
procedure TGosStringList.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  FList[Index].FObject := AObject;

  if Obj <> nil then
    GosFreeAndNil(Obj);

  Changed;
end;

{***********************************************************************************}
procedure TGosStringList.QuickSort(L, R: Integer; SCompare: TGosStringListSortCompare);
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

{********************************************************}
procedure TGosStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{************************************************}
procedure TGosStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{********************************************************}
procedure TGosStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{*****************************************************************************************}
function ALStringListCompareStrings(List: TGosStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList[Index1].FString,
                                List.FList[Index2].FString);
end;

{***************************}
procedure TGosStringList.Sort;
begin
  CustomSort(ALStringListCompareStrings);
end;

{********************************************************************}
procedure TGosStringList.CustomSort(Compare: TGosStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{***********************************************************************}
function TGosStringList.CompareStrings(const S1, S2: AnsiString): Integer;

  {-------------------------------------------------------------}
  function internalCompareStr(const S1, S2: AnsiString): Integer;
  var
    P1, P2: PAnsiChar;
    I: Integer;
    L1, L2: Integer;
  begin
    { Length and PChar of S1 }
    L1 := Length(S1);
    P1 := PAnsiChar(S1);

    { Length and PChar of S2 }
    L2 := Length(S2);
    P2 := PAnsiChar(S2);

    { Continue the loop until the end of one string is reached. }
    I := 0;
    while (I < L1) and (I < L2) do
    begin
      if (P1^ <> P2^) then begin
        if (P1^ = NameValueSeparator) then result := -1
        else if (P2^ = NameValueSeparator) then result := 1
        else result := Ord(P1^) - Ord(P2^);
        Exit;
      end;

      Inc(P1);
      Inc(P2);
      Inc(I);
    end;

    { If chars were not different return the difference in length }
    Result := L1 - L2;
  end;

  {--------------------------------------------------------------}
  function internalCompareText(const S1, S2: AnsiString): Integer;
  var
    P1, P2: PAnsiChar;
    I: Integer;
    C1, C2: AnsiChar;
    L1, L2: Integer;
  begin
    { Length and PChar of S1 }
    L1 := Length(S1);
    P1 := PAnsiChar(S1);

    { Length and PChar of S2 }
    L2 := Length(S2);
    P2 := PAnsiChar(S2);

    { Continue the loop until the end of one string is reached. }
    I := 0;
    while (I < L1) and (I < L2) do
    begin
      if P1^ in ['a'..'z'] then
        C1 := AnsiChar(Byte(P1^) xor $20)
      else
        C1 := P1^;

      if P2^ in ['a'..'z'] then
        C2 := AnsiChar(Byte(P2^) xor $20)
      else
        C2 := P2^;

      if (C1 <> C2) then begin
        if (C1 = NameValueSeparator) then result := -1
        else if (C2 = NameValueSeparator) then result := 1
        else result := Ord(C1) - Ord(C2);
        Exit;
      end;

      Inc(P1);
      Inc(P2);
      Inc(I);
    end;

    { If chars were not different return the difference in length }
    Result := L1 - L2;
  end;

begin

  // Orignial Delphi Code
  // the difference between TGosStringList and TStringList is that
  // TstringList use ansiCompareStr or ansiCompareText that are
  // dependant from the local. I don't like this behavior because
  // as you can read
  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd317759(v=vs.85).aspx
  // "Using CompareString incorrectly can compromise the security of your
  // application. Strings that are not compared correctly can produce
  // invalid input. For example, the function can raise security issues when
  // used for a non-linguistic comparison, because two strings that are
  // distinct in their binary representation can be linguistically equivalent"
  // so i prefere to use instead CompareStr and CompareText but only
  // a..z = A..Z will be handle when case insensitive is set.
  // other behavior must be handle in descendant classe
  //
  // also not the ansiCompareStr and ansiCompareText
  // are 10x more slower than CompareStr and CompareText
  //
  // if CaseSensitive then
  //   Result := AnsiCompareStr(S1, S2)
  // else
  //   Result := AnsiCompareText(S1, S2);

  // it's important that the order is not change because
  // of the ord(NameValueSeparator) this is need because of
  // function FindName
  //
  // EX the items
  //
  //   aaa0
  //   aaa=123
  //   aaaa
  //
  // must be ordered like
  //
  //   aaa=123   |     aaa
  //   aaa0      |     aaa0
  //   aaaa      |     aaaa
  //                   => OK, ordered work with findname
  //
  // but with just Result := ALCompareText(S1, S2)
  // it's will be ordered like
  //
  //   aaa0      |     aaa0
  //   aaa=123   |     aaa
  //   aaaa      |     aaaa
  //                   => KO, NOT ordered, break the findname
  //
  // so we need to give to the NameValueSeparator the lowest ASCII
  // number (#0). for this we must use custom CompareStr and
  // custom CompareText. the cost is that our custom implementation
  // of internalCompareStr and internalCompareText (based on the pure
  // pascal implementation of CompareStr and CompareText, will be aound
  // 50% more slower than the ASM version of CompareStr and CompareText
  // IE 50% more slower with average 50 bytes for S1 and S2, it's even
  // more slower when the number of bytes in S1 and S2 increase

  if  fNameValueOptimization then begin
    if CaseSensitive then
      Result := InternalCompareStr(S1, S2)
    else
      Result := InternalCompareText(S1, S2);
  end
  else begin
    if CaseSensitive then
      Result := AlCompareStr(S1, S2)
    else
      Result := AlCompareText(S1, S2);
  end;
end;

{*************************************************}
procedure TGosStringList.init(OwnsObjects: Boolean);
begin
  setlength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupIgnore;
  FCaseSensitive := False;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
  FNameValueOptimization := True;
end;

{*******************************}
constructor TGosStringList.Create;
begin
  inherited Create;
  init(False);
end;

{*****************************************************}
constructor TGosStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{*************************************************************}
procedure TGosStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;

{*********************************}
destructor TGosNVStringList.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do
      Temp[I] := FList[I].FObject;
  end;

  inherited Destroy;
  FCount := 0;
  SetCapacity(0);

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      GosFreeAndNil(Temp[I]);
end;

{*********************************************************}
function TGosNVStringList.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{*********************************************************************************}
function TGosNVStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
Var aName, aValue: AnsiString;
begin
  if not Sorted then begin
    Result := FCount;
    InsertItem(Result, S, AObject);
  end
  else begin
    if ExtractNameValue(S, aName, aValue) then begin
      if FindNameValue(aName, aValue, Result) then
        case Duplicates of
          dupIgnore: Exit;
          dupError: Error(@SDuplicateString, 0);
        end;
      InsertItem(Result, aName, aValue, AObject);
    end
    else begin
      if FindName(s, false{WithNvS}, Result) then
        case Duplicates of
          dupIgnore: Exit;
          dupError: Error(@SDuplicateString, 0);
        end;
      InsertItem(Result, s, False{WithNvS}, AObject);
    end;
  end;
end;

{****************************************************************************}
function TGosNVStringList.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{****************************************************************************************************}
function TGosNVStringList.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  if not Sorted then begin
    Result := FCount;
  end
  else begin
    if FindNameValue(Name, Value, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  end;
  InsertItem(Result, Name, Value, AObject);
end;

{****************************************************}
procedure TGosNVStringList.Assign(Source: TPersistent);
begin
  if Source is TGosNVStringList then
  begin
    Clear;
    FCaseSensitive := TGosNVStringList(Source).FCaseSensitive;
    FDuplicates := TGosNVStringList(Source).FDuplicates;
    FSorted := TGosNVStringList(Source).FSorted;
  end
  else if Source is TGosStringList then
  begin
    Clear;
    FCaseSensitive := TGosStringList(Source).FCaseSensitive;
    FDuplicates := TGosStringList(Source).FDuplicates;
    FSorted := TGosStringList(Source).FSorted;
  end
  else if Source is TGosHashedStringList then
  begin
    Clear;
    FCaseSensitive := TGosHashedStringList(Source).CaseSensitive;
    FDuplicates := TGosHashedStringList(Source).FDuplicates;
    FSorted := False;
  end
  else if Source is TGosAVLStringList then
  begin
    Clear;
    FCaseSensitive := TGosAVLStringList(Source).CaseSensitive;
    FDuplicates := TGosAVLStringList(Source).FDuplicates;
    FSorted := False;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    Duplicates := TStringList(Source).Duplicates;
    Sorted := TStringList(Source).Sorted;
  end;
  inherited Assign(Source);
end;

{****************************************************}
procedure TGosNVStringList.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := fCaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := fSorted;
  end;
  inherited AssignTo(Dest);
end;

{********************************}
procedure TGosNVStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{*********************************}
procedure TGosNVStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{******************************}
procedure TGosNVStringList.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if FCount <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, FCount);
      for I := 0 to FCount - 1 do
        Temp[I] := FList[I].FObject;
    end;

    FCount := 0;
    SetCapacity(0);

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        GosFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{***********************************************}
procedure TGosNVStringList.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    GosMove(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(TGosNVStringItem));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FName)^   := nil;
    PPointer(@FList[FCount].FValue)^  := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    GosFreeAndNil(Obj);
  Changed;
end;

{***************************************************************}
function  TGosNVStringList.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  result := FList[Index].FObject;
  FList[Index].FObject := nil;
  Changed;
end;

{**********************************************************}
procedure TGosNVStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{***************************************************************}
procedure TGosNVStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PGosNVStringItem;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];

  Temp := Pointer(Item1^.FName);
  Pointer(Item1^.FName) := Pointer(Item2^.FName);
  Pointer(Item2^.FName) := Temp;

  Temp := Pointer(Item1^.FValue);
  Pointer(Item1^.FValue) := Pointer(Item2^.FValue);
  Pointer(Item2^.FValue) := Temp;

  Temp := pointer(Item1^.FObject);
  pointer(Item1^.FObject) := pointer(Item2^.FObject);
  pointer(Item2^.FObject) := Temp;
end;

{******************************************************************************}
function TGosNVStringList.Find(const S: AnsiString; var Index: Integer): Boolean;
var Name, Value: ansiString;
begin
  if ExtractNameValue(S, Name, Value) then result := FindNameValue(Name, Value, Index)
  else result := FindName(Name, False{WithNvS}, Index);
end;

{*************************************************************************************}
function TGosNVStringList.FindName(const Name: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);     // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          if CompareStrings(FList[I].FName, Name) = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{*******************************************************************************************************}
function TGosNVStringList.FindName(const Name: AnsiString; WithNvS: boolean; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);     // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if (C = 0) then begin
      if (not WithNvS) and FList[I].FNVS then c := 1   // Must be ordered in this order :
                                                       // aa
                                                       // aaa
                                                       // aaa=
                                                       // aaaa
      else if (WithNvS) and (not FList[I].FNVS) then c := -1;  // Must be ordered in this order :
                                                               // aa
                                                               // aaa
                                                               // aaa=
                                                               // aaaa
    end;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          C := CompareStrings(FList[I].FName, Name);
          if (C = 0) then begin
            if (not WithNvS) and FList[I].FNVS then c := 1   // Must be ordered in this order :
                                                             // aa
                                                             // aaa
                                                             // aaa=
                                                             // aaaa
            else if (WithNvS) and (not FList[I].FNVS) then c := -1;  // Must be ordered in this order :
                                                                     // aa
                                                                     // aaa
                                                                     // aaa=
                                                                     // aaaa
          end;
          if c = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{*************************************************************************************************}
function TGosNVStringList.FindNameValue(const Name, Value: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);  // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if (C = 0) then begin
      if (not FList[I].FNVS) then c := -1 // Must be ordered in this order :
                                          // aa
                                          // aaa
                                          // aaa=
                                          // aaaa
      else C := CompareStrings(FList[I].FValue, Value);  // The return value is less than 0 if FList[I].FValue < Value, 0 if FList[I].FValue = Value, or greater than 0 if FList[I].FValue > Value.
    end;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{*******************************************************}
function TGosNVStringList.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  if FList[Index].fNvs then Result := FList[Index].FName + NameValueSeparator + FList[Index].fValue
  else Result := FList[Index].fname;
end;

{********************************************}
function TGosNVStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

{*****************************************}
function TGosNVStringList.GetCount: Integer;
begin
  Result := FCount;
end;

{**********************************************************}
function TGosNVStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FObject;
end;

{*****************************}
procedure TGosNVStringList.Grow;
{$IF CompilerVersion <= 32}{tokyo}
var
  Delta: Integer;
{$endif}
begin
  {$IF CompilerVersion <= 32}{tokyo}
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
  {$else}
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
  {$endif}
end;

{**********************************************}
function TGosNVStringList.GetTextStr: AnsiString;
var
  I, L, Size: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
  NvS: AnsiChar;
begin
  Size := 0;
  LB := LineBreak;
  NvS := nameValueSeparator;
  for I := 0 to FCount - 1 do begin
    if FList[i].fNvs then Inc(Size, Length(FList[i].fName) + 1{length(NameValueSeparator)} +  Length(FList[i].fValue) + Length(LB))
    else Inc(Size, Length(FList[i].fName) + Length(LB))
  end;
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to FCount - 1 do
  begin
    S := FList[i].fName;
    L := Length(S);
    if L <> 0 then
    begin
      GosMove(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    if FList[i].fNvs then begin
      GosMove(NvS, P^, 1);
      Inc(P, 1);
      S := FList[i].fValue;
      L := Length(S);
      if L <> 0 then
      begin
        GosMove(Pointer(S)^, P^, L);
        Inc(P, L);
      end;
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      GosMove(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

{*************************************************************}
function TGosNVStringList.IndexOf(const S: AnsiString): Integer;
Var aName, aValue: AnsiString;
begin
  if ExtractNameValue(S, aName, aValue) then begin
    if not Sorted then begin
      for Result := 0 to FCount - 1 do
        if Flist[Result].FNVS and
           (CompareStrings(Flist[Result].FName, aName) = 0) and
           (CompareStrings(Flist[Result].FValue, aValue) = 0) then Exit;
      Result := -1;
    end
    else begin
      if not FindNameValue(aName, aValue, Result) then Result := -1;
    end;
  end
  else begin
    if not Sorted then begin
      for Result := 0 to FCount - 1 do
        if (not Flist[Result].FNVS) and
           (CompareStrings(Flist[Result].FName, s) = 0) then Exit;
      Result := -1;
    end
    else begin
      if not FindName(s, false{WinthNvS}, Result) then Result := -1;
    end;
  end;
end;

{********************************************************************}
function TGosNVStringList.IndexOfName(const Name: ansistring): Integer;
begin
  if not Sorted then begin
    for Result := 0 to FCount - 1 do
      if CompareStrings(Flist[Result].FName, Name) = 0 then Exit;
    Result := -1;
  end
  else begin
    if not FindName(Name, Result) then Result := -1;
  end;
end;

{********************************************************************}
procedure TGosNVStringList.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{********************************************************************************************}
procedure TGosNVStringList.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{***************************************************************************************}
procedure TGosNVStringList.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{***************************************************************************************************************}
procedure TGosNVStringList.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, Name, Value, AObject);
end;

{**********************************************************}
procedure TGosNVStringList.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempName: AnsiString;
  TempValue: AnsiString;
  TempNvS: Boolean;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempName := Flist[curIndex].FName;
      TempNvs := Flist[curIndex].FNvs;
      if TempNvs then TempValue := Flist[curIndex].FValue;
      TempObject := Flist[curIndex].FObject;
      FList[CurIndex].FObject := nil;
      Delete(CurIndex);
      if TempNvs then InsertObject(NewIndex, TempName, TempObject)
      else InsertNameValueObject(NewIndex, TempName, TempValue, TempObject)
    finally
      EndUpdate;
    end;
  end;
end;

{******************************************************************************************}
procedure TGosNVStringList.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
var Name, Value: ansiString;
begin
  if ExtractNameValue(S, Name, Value) then InsertItem(Index, Name, Value, AObject)
  else InsertItem(Index, s, False{WithNvS}, AObject);
end;

{****************************************************************************************************}
procedure TGosNVStringList.InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    GosMove(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TGosNVStringItem));
  Pointer(FList[Index].FName) := nil;
  Pointer(FList[Index].FValue) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FName := Name;
  FList[Index].fNvs := true;
  FList[Index].FValue := Value;
  Inc(FCount);
  Changed;
end;

{***************************************************************************************************************}
procedure TGosNVStringList.InsertItem(Index: Integer; const Name: AnsiString; WithNvS: boolean; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    GosMove(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TGosNVStringItem));
  Pointer(FList[Index].FName) := nil;
  Pointer(FList[Index].FValue) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FName := Name;
  FList[Index].fNvs := WithNvS;
  FList[Index].FValue := '';
  Inc(FCount);
  Changed;
end;

{*****************************************************************}
procedure TGosNVStringList.Put(Index: Integer; const S: AnsiString);
var Name, Value: ansiString;
begin
  if not sorted then begin
    if Cardinal(Index) >= Cardinal(FCount) then
      Error(@SListIndexError, Index);
    Changing;
    if ExtractNameValue(S, Name, Value) then begin
      FList[Index].FName := Name;
      FList[Index].FNvS := True;
      FList[Index].FValue := Value;
    end
    else begin
      FList[Index].FName := S;
      FList[Index].FNvS := False;
      FList[Index].FValue := '';
    end;
    Changed;
  end
  else begin
    delete(index);
    add(s);
  end;
end;

{********************************************************************}
procedure TGosNVStringList.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  FList[Index].FObject := AObject;

  if Obj <> nil then
    GosFreeAndNil(Obj);

  Changed;
end;

{***************************************************************************************}
procedure TGosNVStringList.QuickSort(L, R: Integer; SCompare: TGosNVStringListSortCompare);
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

{**********************************************************}
procedure TGosNVStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{**************************************************}
procedure TGosNVStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{**********************************************************}
procedure TGosNVStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{*********************************************************************************************}
function ALNVStringListCompareStrings(List: TGosNVStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList[Index1].FName,
                                List.FList[Index2].FName);  // The return value is less than 0 if List.FList[Index1].FName < List.FList[Index2].FName, 0 if List.FList[Index1].FName = List.FList[Index2].FName, or greater than 0 if List.FList[Index1].FName > List.FList[Index2].FName.
  if result = 0 then begin
    if (not List.FList[Index1].fNvS) and List.FList[Index2].FNVS then result := -1  // Must be ordered in this order :
                                                                                    // aa
                                                                                    // aaa
                                                                                    // aaa=
                                                                                    // aaaa
    else if (List.FList[Index1].fNvS) and (not List.FList[Index2].FNVS) then result := 1;  // Must be ordered in this order :
                                                                                           // aa
                                                                                           // aaa
                                                                                           // aaa=
                                                                                           // aaaa
    if (result=0) then Result := List.CompareStrings(List.FList[Index1].FValue,
                                                     List.FList[Index2].FValue);  // The return value is less than 0 if List.FList[Index1].FValue < List.FList[Index2].FValue, 0 if List.FList[Index1].FValue = List.FList[Index2].FValue, or greater than 0 if List.FList[Index1].FValue > List.FList[Index2].FValue.
  end;
end;

{*****************************}
procedure TGosNVStringList.Sort;
begin
  CustomSort(ALNVStringListCompareStrings);
end;

{************************************************************************}
procedure TGosNVStringList.CustomSort(Compare: TGosNVStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{*************************************************************************}
function TGosNVStringList.CompareStrings(const S1, S2: AnsiString): Integer;
begin

  // Orignial Delphi Code
  // the difference between TGosNVStringList and TStringList is that
  // TstringList use ansiCompareStr or ansiCompareText that are
  // dependant from the local. I don't like this behavior because
  // as you can read
  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd317759(v=vs.85).aspx
  // "Using CompareString incorrectly can compromise the security of your
  // application. Strings that are not compared correctly can produce
  // invalid input. For example, the function can raise security issues when
  // used for a non-linguistic comparison, because two strings that are
  // distinct in their binary representation can be linguistically equivalent"
  // so i prefere to use instead CompareStr and CompareText but only
  // a..z = A..Z will be handle when case insensitive is set.
  // other behavior must be handle in descendant classe
  //
  // also not the ansiCompareStr and ansiCompareText
  // are 10x more slower than CompareStr and CompareText
  //
  // if CaseSensitive then
  //   Result := AnsiCompareStr(S1, S2)
  // else
  //   Result := AnsiCompareText(S1, S2);

  // it's important that the order is not change because
  // of the ord(NameValueSeparator) this is need because of
  // function FindName
  //
  // EX the items
  //
  //   aaa0
  //   aaa=123
  //   aaaa
  //
  // must be ordered like
  //
  //   aaa=123   |     aaa
  //   aaa0      |     aaa0
  //   aaaa      |     aaaa
  //                   => OK, ordered work with findname
  //
  // but with just Result := ALCompareText(S1, S2)
  // it's will be ordered like
  //
  //   aaa0      |     aaa0
  //   aaa=123   |     aaa
  //   aaaa      |     aaaa
  //                   => KO, NOT ordered, break the findname
  //

  if CaseSensitive then
    Result := ALCompareStr(S1, S2)
  else
    Result := ALCompareText(S1, S2);

end;

{***************************************************}
procedure TGosNVStringList.init(OwnsObjects: Boolean);
begin
  setlength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupIgnore;
  FCaseSensitive := False;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
end;

{*********************************}
constructor TGosNVStringList.Create;
begin
  inherited Create;
  init(False);
end;

{*******************************************************}
constructor TGosNVStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{***************************************************************}
procedure TGosNVStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;

{***************************************************************************************************}
Function TGosNVStringList.ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
Var P1: Integer;
begin
  P1 := AlPos(NameValueSeparator,S);
  if P1 > 0 then begin
    result := True;
    Name := AlCopyStr(S,1,P1-1);
    Value := AlCopyStr(S,P1+1, maxint);
  end
  else begin
    Result := False;
    Name := S;
    Value := '';
  end;
end;

{***********************************************************}
function TGosNVStringList.GetName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := Flist[Index].fName;
end;

{*****************************************************************}
function TGosNVStringList.GetStrictName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  if Flist[Index].fnvs then Result := Flist[Index].fName
  else result := ''
end;

{********************************************************************}
function TGosNVStringList.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then begin
    if Flist[i].fnvs then Result := Flist[i].fValue
    else Result := '';
  end else
    Result := '';
end;

{****************************************************************}
procedure TGosNVStringList.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      Changing;
      Flist[i].fValue := Value;
      Flist[i].fNVS := True;
      Changed;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{*********************************************************************}
function TGosNVStringList.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    if (Flist[index].fNvs) then
      result := Flist[index].fValue
    else
      Result := '';
  end
  else
    Result := '';
end;

{***********************************************************************************}
procedure TGosNVStringList.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(Count) then
        Error(@SListIndexError, Index);
      Changing;
      Flist[Index].fValue := Value;
      Flist[Index].fNVS := True;
      Changed;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{**************************************************************************}
procedure TGosNVStringList.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    Changing;
    Flist[I].fValue := Value;
    Flist[I].fNVS := True;
    Changed;
  end
end;

{*********************************************************************************************}
procedure TGosNVStringList.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    Changing;
    Flist[Index].fValue := Value;
    Flist[Index].fNVS := True;
    Changed;
  end;
end;

{**********************************}
destructor TGosAVLStringList.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, Count);
    for I := 0 to Count - 1 do
      Temp[I] := Objects[I];
  end;

  GosFreeAndNil(FAVLBinTree);
  GosFreeAndNil(FNodeList);
  inherited Destroy;

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      GosFreeAndNil(Temp[I]);
end;

{**********************************************************}
function TGosAVLStringList.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{**********************************************************************************}
function TGosAVLStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, S, AObject);
end;

{*****************************************************************************}
function TGosAVLStringList.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{*****************************************************************************************************}
function TGosAVLStringList.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, Name, Value, AObject);
end;

{*****************************************************}
procedure TGosAVLStringList.Assign(Source: TPersistent);
begin
  if Source is TGosAVLStringList then
  begin
    Clear;
    CaseSensitive := TGosAVLStringList(Source).CaseSensitive;
    FDuplicates := TGosAVLStringList(Source).FDuplicates;
  end
  else if Source is TGosStringList then
  begin
    Clear;
    CaseSensitive := TGosStringList(Source).CaseSensitive;
    FDuplicates := TGosStringList(Source).Duplicates;
  end
  else if Source is TGosNVStringList then
  begin
    Clear;
    CaseSensitive := TGosNVStringList(Source).FCaseSensitive;
    FDuplicates := TGosNVStringList(Source).FDuplicates;
  end
  else if Source is TGosHashedStringList then
  begin
    Clear;
    CaseSensitive := TGosHashedStringList(Source).CaseSensitive;
    FDuplicates := TGosHashedStringList(Source).FDuplicates;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    FDuplicates := TStringList(Source).Duplicates;
  end;
  inherited Assign(Source);
end;

{*****************************************************}
procedure TGosAVLStringList.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := CaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := False;
  end;
  inherited AssignTo(Dest);
end;

{*********************************}
procedure TGosAVLStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{**********************************}
procedure TGosAVLStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{*******************************}
procedure TGosAVLStringList.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if Count <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, Count);
      for I := 0 to Count - 1 do
        Temp[I] := Objects[I];
    end;

    FAVLBinTree.Clear;
    FnodeList.Clear;

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        GosFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{************************************************}
procedure TGosAVLStringList.Delete(Index: Integer);
var
  Obj: TObject;
  i: integer;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := Objects[Index]
  else
    Obj := nil;

  FAVLBinTree.DeleteNode(TGosAVLStringListBinaryTreeNode(FNodelist[Index]).ID);
  FNodelist.Delete(Index);
  for i := Index to FNodeList.Count - 1 do
    TGosAVLStringListBinaryTreeNode(FNodelist[i]).Idx := i;

  if Obj <> nil then
    GosFreeAndNil(Obj);
  Changed;
end;

{****************************************************************}
function  TGosAVLStringList.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Changing;
  with TGosAVLStringListBinaryTreeNode(FNodeList[Index]) do begin
    result := Obj;
    Obj := nil;
  end;
  Changed;
end;

{***********************************************************}
procedure TGosAVLStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= Count) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= Count) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{****************************************************************}
procedure TGosAVLStringList.ExchangeItems(Index1, Index2: Integer);
var Item1, Item2: TGosAVLStringListBinaryTreeNode;
begin
  Item1 := TGosAVLStringListBinaryTreeNode(FNodelist[Index1]);
  Item2 := TGosAVLStringListBinaryTreeNode(FNodelist[Index2]);
  FNodeList.Exchange(Index1,Index2);
  Item1.idx := Index2;
  Item2.idx := Index1;
end;

{********************************************************}
function TGosAVLStringList.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  with TGosAVLStringListBinaryTreeNode(FNodelist[Index]) do begin
    if Nvs then Result := ID + NameValueSeparator + Val
    else Result := ID;
  end;
end;

{******************************************}
function TGosAVLStringList.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

{***********************************************************}
function TGosAVLStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := TGosAVLStringListBinaryTreeNode(FNodelist[Index]).Obj;
end;

{***********************************************}
function TGosAVLStringList.GetTextStr: AnsiString;
var
  I, L, Size, Count: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
  NvS: AnsiChar;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  NvS := nameValueSeparator;
  for I := 0 to Count - 1 do begin
    if TGosAVLStringListBinaryTreeNode(fNodeList[i]).Nvs then Inc(Size, Length(TGosAVLStringListBinaryTreeNode(fNodeList[i]).ID) + 1{length(NameValueSeparator)} +  Length(TGosAVLStringListBinaryTreeNode(fNodeList[i]).Val) + Length(LB))
    else Inc(Size, Length(TGosAVLStringListBinaryTreeNode(fNodeList[i]).ID) + Length(LB))
  end;
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := TGosAVLStringListBinaryTreeNode(fNodeList[i]).ID;
    L := Length(S);
    if L <> 0 then
    begin
      GosMove(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    if TGosAVLStringListBinaryTreeNode(fNodeList[i]).Nvs then begin
      GosMove(NvS, P^, 1);
      Inc(P, 1);
      S := TGosAVLStringListBinaryTreeNode(fNodeList[i]).Val;
      L := Length(S);
      if L <> 0 then
      begin
        GosMove(Pointer(S)^, P^, L);
        Inc(P, L);
      end;
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      GosMove(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

{**************************************************************}
function TGosAVLStringList.IndexOf(const S: AnsiString): Integer;
Var aName, aValue: AnsiString;
    aNode: TGosAVLStringListBinaryTreeNode;
begin
  if ExtractNameValue(S, aName, aValue) then begin
    aNode := TGosAVLStringListBinaryTreeNode(FAVLBinTree.FindNode(aName));
    if (not assigned(aNode))
       or
       ((CaseSensitive) and
        (aNode.Val <> aValue))
       or
       ((not CaseSensitive) and
        (not ALSametext(aNode.Val, aValue)))
    then result := -1
    else result := aNode.idx;
  end
  else begin
    aNode := TGosAVLStringListBinaryTreeNode(FAVLBinTree.FindNode(S));
    if (not assigned(aNode)) or (aNode.Nvs) then result := -1
    else result := aNode.idx;
  end;
end;

{*********************************************************************}
function TGosAVLStringList.IndexOfName(const Name: ansistring): Integer;
Var aNode: TGosAVLStringListBinaryTreeNode;
begin
  aNode := TGosAVLStringListBinaryTreeNode(FAVLBinTree.FindNode(Name));
  if assigned(aNode) then result := aNode.Idx
  else result := -1;
end;

{*********************************************************************}
procedure TGosAVLStringList.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{*********************************************************************************************}
procedure TGosAVLStringList.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{****************************************************************************************}
procedure TGosAVLStringList.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{****************************************************************************************************************}
procedure TGosAVLStringList.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then Error(@SListIndexError, Index);
  InsertItem(Index, Name, Value, AObject);
end;

{***********************************************************}
procedure TGosAVLStringList.Move(CurIndex, NewIndex: Integer);
var i: integer;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      FNodeList.Move(CurIndex, NewIndex);
      for i := min(CurIndex, NewIndex) to FNodeList.Count - 1 do
        TGosAVLStringListBinaryTreeNode(FNodelist[i]).Idx := i;
    finally
      EndUpdate;
    end;
  end;
end;

{********************************************************************************************************}
procedure TGosAVLStringList.InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
Var aNode: TGosAVLStringListBinaryTreeNode;
    i: integer;
begin
  Changing;

  aNode := TGosAVLStringListBinaryTreeNode.Create;
  aNode.Idx := Index;
  aNode.ID := Name;
  aNode.Val := Value;
  aNode.Nvs := True;
  aNode.Obj := AObject;
  if not FAVLBinTree.AddNode(aNode) then begin
    GosFreeAndNil(aNode);
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, aNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    TGosAVLStringListBinaryTreeNode(FNodelist[i]).Idx := i;

  Changed;
end;

{*******************************************************************************************}
procedure TGosAVLStringList.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
Var aName, AValue: AnsiString;
    aNvs: Boolean;
    aNode: TGosAVLStringListBinaryTreeNode;
    i: integer;
begin
  Changing;

  aNvs := ExtractNameValue(S, aName, aValue);
  aNode := TGosAVLStringListBinaryTreeNode.Create;
  aNode.Idx := Index;
  aNode.ID := aName;
  aNode.Val := aValue;
  aNode.Nvs := aNvs;
  aNode.Obj := AObject;
  if not FAVLBinTree.AddNode(aNode) then begin
    GosFreeAndNil(aNode);
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, aNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    TGosAVLStringListBinaryTreeNode(FNodelist[i]).Idx := i;

  Changed;
end;

{******************************************************************}
procedure TGosAVLStringList.Put(Index: Integer; const S: AnsiString);
Var aNewName, aNewValue: AnsiString;
    aNewNvs: Boolean;
    aNewNode, aOldNode: TGosAVLStringListBinaryTreeNode;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Changing;

  aNewNvs := ExtractNameValue(S, aNewName, aNewValue);
  aOldNode := TGosAVLStringListBinaryTreeNode(FNodeList[index]);
  if (CaseSensitive and (aOldNode.ID <> aNewName)) or
     ((not CaseSensitive) and (not ALSametext(aOldNode.ID, aNewName))) then begin
    aNewNode := TGosAVLStringListBinaryTreeNode.Create;
    aNewNode.Idx := Index;
    aNewNode.ID := aNewName;
    aNewNode.Val := ANewValue;
    aNewNode.NVS := aNewNvs;
    aNewNode.Obj := aOldNode.Obj;
    if not FAVLBinTree.AddNode(aNewNode) then begin
      GosFreeAndNil(aNewNode);
      case Duplicates of
        dupIgnore: Exit;
        else Error(@SDuplicateString, 0);
      end;
    end;
    FNodeList[Index] := aNewNode;
    FAVLBinTree.DeleteNode(aOldNode.ID);
  end
  else begin
    aOldNode.Val := aNewValue;
    aOldNode.NVS := aNewNVS;
  end;

  Changed;
end;

{*********************************************************************}
procedure TGosAVLStringList.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := TGosAVLStringListBinaryTreeNode(FNodeList[Index]).Obj
  else
    Obj := nil;

  TGosAVLStringListBinaryTreeNode(FNodeList[Index]).Obj := AObject;

  if Obj <> nil then
    GosFreeAndNil(Obj);

  Changed;
end;

{*****************************************************************************************}
procedure TGosAVLStringList.QuickSort(L, R: Integer; SCompare: TGosAVLStringListSortCompare);
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

{***********************************************************}
procedure TGosAVLStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{**************************************************************************}
procedure TGosAVLStringList.CustomSort(Compare: TGosAVLStringListSortCompare);
begin
  if (Count > 1) then
  begin
    Changing;
    QuickSort(0, Count - 1, Compare);
    Changed;
  end;
end;

{****************************************************}
procedure TGosAVLStringList.init(OwnsObjects: Boolean);
begin
  FAVLBinTree:= TGosStringKeyAVLBinaryTree.Create;
  FAVLBinTree.CaseSensitive := False;
  FNodeList := TObjectList.Create(False);
  FDuplicates := dupError;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
end;

{**********************************}
constructor TGosAVLStringList.Create;
begin
  inherited Create;
  init(False);
end;

{********************************************************}
constructor TGosAVLStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{****************************************************************}
procedure TGosAVLStringList.SetCaseSensitive(const Value: Boolean);
begin
  FAVLBinTree.CaseSensitive := Value;
end;

{**************************************************}
function TGosAVLStringList.GetCaseSensitive: Boolean;
begin
  result := FAVLBinTree.CaseSensitive;
end;

{*****************************************************************}
procedure TGosAVLStringList.SetDuplicates(const Value: TDuplicates);
begin
  if value = dupAccept then raise exception.Create('TGosAVLStringList does not support duplicate Names');
  FDuplicates := Value;
end;

{****************************************************************************************************}
Function TGosAVLStringList.ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
Var P1: Integer;
begin
  P1 := AlPos(NameValueSeparator,S);
  if P1 > 0 then begin
    result := True;
    Name := AlCopyStr(S,1,P1-1);
    Value := AlCopyStr(S,P1+1, maxint);
  end
  else begin
    Result := False;
    Name := S;
    Value := '';
  end;
end;

{************************************************************}
function TGosAVLStringList.GetName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := TGosAVLStringListBinaryTreeNode(FNodelist[Index]).ID;
end;

{******************************************************************}
function TGosAVLStringList.GetStrictName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  if TGosAVLStringListBinaryTreeNode(FNodelist[Index]).nvs then Result := TGosAVLStringListBinaryTreeNode(FNodelist[Index]).ID
  else result := ''
end;

{*********************************************************************}
function TGosAVLStringList.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then begin
    if TGosAVLStringListBinaryTreeNode(FNodelist[i]).nvs then Result := TGosAVLStringListBinaryTreeNode(FNodelist[i]).Val
    else Result := '';
  end else
    Result := '';
end;

{*****************************************************************}
procedure TGosAVLStringList.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      Changing;
      TGosAVLStringListBinaryTreeNode(FNodeList[i]).Val := Value;
      TGosAVLStringListBinaryTreeNode(FNodeList[i]).NVS := True;
      Changed;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{**********************************************************************}
function TGosAVLStringList.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    if (TGosAVLStringListBinaryTreeNode(FNodeList[index]).Nvs) then
      result := TGosAVLStringListBinaryTreeNode(FNodeList[index]).Val
    else
      Result := '';
  end
  else
    Result := '';
end;

{************************************************************************************}
procedure TGosAVLStringList.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(Count) then
        Error(@SListIndexError, Index);
      Changing;
      TGosAVLStringListBinaryTreeNode(FNodeList[Index]).Val := Value;
      TGosAVLStringListBinaryTreeNode(FNodeList[Index]).NVS := True;
      Changed;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{***************************************************************************}
procedure TGosAVLStringList.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    Changing;
    TGosAVLStringListBinaryTreeNode(FNodeList[I]).Val := Value;
    TGosAVLStringListBinaryTreeNode(FNodeList[I]).NVS := True;
    Changed;
  end
end;

{**********************************************************************************************}
procedure TGosAVLStringList.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    Changing;
    TGosAVLStringListBinaryTreeNode(FNodeList[Index]).Val := Value;
    TGosAVLStringListBinaryTreeNode(FNodeList[Index]).NVS := True;
    Changed;
  end;
end;

{*************************************}
destructor TGosHashedStringList.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, Count);
    for I := 0 to Count - 1 do
      Temp[I] := Objects[I];
  end;

  GosFreeAndNil(FDictionary);
  GosFreeAndNil(FNodeList);
  inherited Destroy;

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      GosFreeAndNil(Temp[I]);
end;

{*************************************************************}
function TGosHashedStringList.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{*************************************************************************************}
function TGosHashedStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, S, AObject);
end;

{********************************************************************************}
function TGosHashedStringList.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{********************************************************************************************************}
function TGosHashedStringList.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, Name, Value, AObject);
end;

{********************************************************}
procedure TGosHashedStringList.Assign(Source: TPersistent);
begin
  if Source is TGosHashedStringList then
  begin
    Clear;
    CaseSensitive := TGosHashedStringList(Source).CaseSensitive;
    FDuplicates := TGosHashedStringList(Source).FDuplicates;
  end
  else if Source is TGosStringList then
  begin
    Clear;
    CaseSensitive := TGosStringList(Source).CaseSensitive;
    FDuplicates := TGosStringList(Source).Duplicates;
  end
  else if Source is TGosNVStringList then
  begin
    Clear;
    FCaseSensitive := TGosNVStringList(Source).FCaseSensitive;
    FDuplicates := TGosNVStringList(Source).FDuplicates;
  end
  else if Source is TGosAVLStringList then
  begin
    Clear;
    CaseSensitive := TGosAVLStringList(Source).CaseSensitive;
    FDuplicates := TGosAVLStringList(Source).FDuplicates;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    FDuplicates := TStringList(Source).Duplicates;
  end;
  inherited Assign(Source);
end;

{********************************************************}
procedure TGosHashedStringList.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := CaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := False;
  end;
  inherited AssignTo(Dest);
end;

{************************************}
procedure TGosHashedStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{*************************************}
procedure TGosHashedStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{**********************************}
procedure TGosHashedStringList.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if Count <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, Count);
      for I := 0 to Count - 1 do
        Temp[I] := Objects[I];
    end;

    FDictionary.Clear;
    FnodeList.Clear;

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        GosFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{***************************************************}
procedure TGosHashedStringList.Delete(Index: Integer);
var
  Obj: TObject;
  i: integer;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := Objects[Index]
  else
    Obj := nil;

  Fdictionary.Remove(FNodelist[Index].ID);
  FNodelist.Delete(Index);
  for i := Index to FNodeList.Count - 1 do
    FNodelist[i].Idx := i;

  if Obj <> nil then
    GosFreeAndNil(Obj);
  Changed;
end;

{*******************************************************************}
function  TGosHashedStringList.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Changing;
  with FNodeList[Index] do begin
    result := Obj;
    Obj := nil;
  end;
  Changed;
end;

{**************************************************************}
procedure TGosHashedStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= Count) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= Count) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{*******************************************************************}
procedure TGosHashedStringList.ExchangeItems(Index1, Index2: Integer);
var Item1, Item2: TGosHashedStringListDictionaryNode;
begin
  Item1 := FNodelist[Index1];
  Item2 := FNodelist[Index2];
  FNodeList.Exchange(Index1,Index2);
  Item1.idx := Index2;
  Item2.idx := Index1;
end;

{***********************************************************}
function TGosHashedStringList.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  with FNodelist[Index] do begin
    if Nvs then Result := ID + NameValueSeparator + Val
    else Result := ID;
  end;
end;

{*********************************************}
function TGosHashedStringList.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

{**************************************************************}
function TGosHashedStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := FNodelist[Index].Obj;
end;

{**************************************************}
function TGosHashedStringList.GetTextStr: AnsiString;
var
  I, L, Size, Count: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
  NvS: AnsiChar;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  NvS := nameValueSeparator;
  for I := 0 to Count - 1 do begin
    if fNodeList[i].Nvs then Inc(Size, Length(fNodeList[i].ID) + 1{length(NameValueSeparator)} +  Length(fNodeList[i].Val) + Length(LB))
    else Inc(Size, Length(fNodeList[i].ID) + Length(LB))
  end;
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := fNodeList[i].ID;
    L := Length(S);
    if L <> 0 then
    begin
      GosMove(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    if fNodeList[i].Nvs then begin
      GosMove(NvS, P^, 1);
      Inc(P, 1);
      S := fNodeList[i].Val;
      L := Length(S);
      if L <> 0 then
      begin
        GosMove(Pointer(S)^, P^, L);
        Inc(P, L);
      end;
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      GosMove(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

{*****************************************************************}
function TGosHashedStringList.IndexOf(const S: AnsiString): Integer;
Var aName, aValue: AnsiString;
    aNode: TGosHashedStringListDictionaryNode;
begin
  if ExtractNameValue(S, aName, aValue) then begin
    if (not fDictionary.TryGetValue(aName,aNode))
       or
       ((CaseSensitive) and
        (aNode.Val <> aValue))
       or
       ((not CaseSensitive) and
        (not ALSametext(aNode.Val, aValue)))
    then result := -1
    else result := aNode.idx;
  end
  else begin
    if (not fDictionary.TryGetValue(S,aNode)) or (aNode.Nvs) then result := -1
    else result := aNode.idx;
  end;
end;

{************************************************************************}
function TGosHashedStringList.IndexOfName(const Name: ansistring): Integer;
Var aNode: TGosHashedStringListDictionaryNode;
begin
  if fDictionary.TryGetValue(Name,aNode) then result := aNode.Idx
  else result := -1;
end;

{************************************************************************}
procedure TGosHashedStringList.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{************************************************************************************************}
procedure TGosHashedStringList.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{*******************************************************************************************}
procedure TGosHashedStringList.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{*******************************************************************************************************************}
procedure TGosHashedStringList.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then Error(@SListIndexError, Index);
  InsertItem(Index, Name, Value, AObject);
end;

{**************************************************************}
procedure TGosHashedStringList.Move(CurIndex, NewIndex: Integer);
var i: integer;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      FNodeList.Move(CurIndex, NewIndex);
      for i := min(CurIndex, NewIndex) to FNodeList.Count - 1 do
        FNodelist[i].Idx := i;
    finally
      EndUpdate;
    end;
  end;
end;

{********************************************************************************************************}
procedure TGosHashedStringList.InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
Var aNode: TGosHashedStringListDictionaryNode;
    i: integer;
begin
  Changing;

  aNode := TGosHashedStringListDictionaryNode.Create;
  aNode.Idx := Index;
  aNode.ID := Name;
  aNode.Val := Value;
  aNode.Nvs := True;
  aNode.Obj := AObject;
  {$IF CompilerVersion <= 32} // tokyo
  if not Fdictionary.ContainsKey(Name) then Fdictionary.Add(Name,aNode)
  else begin
  {$ELSE}
  if not Fdictionary.TryAdd(Name,aNode) then begin
  {$ENDIF}
    GosFreeAndNil(aNode);
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, aNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    FNodelist[i].Idx := i;

  Changed;
end;

{**********************************************************************************************}
procedure TGosHashedStringList.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
Var aName, AValue: AnsiString;
    aNvs: Boolean;
    aNode: TGosHashedStringListDictionaryNode;
    i: integer;
begin
  Changing;

  aNvs := ExtractNameValue(S, aName, aValue);
  aNode := TGosHashedStringListDictionaryNode.Create;
  aNode.Idx := Index;
  aNode.ID := aName;
  aNode.Val := aValue;
  aNode.Nvs := aNvs;
  aNode.Obj := AObject;
  {$IF CompilerVersion <= 32} // tokyo
  if not Fdictionary.ContainsKey(aName) then Fdictionary.Add(aName,aNode)
  else begin
  {$ELSE}
  if not Fdictionary.TryAdd(aName,aNode) then begin
  {$ENDIF}
    GosFreeAndNil(aNode);
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, aNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    FNodelist[i].Idx := i;

  Changed;
end;

{*********************************************************************}
procedure TGosHashedStringList.Put(Index: Integer; const S: AnsiString);
Var aNewName, aNewValue: AnsiString;
    aNewNvs: Boolean;
    aNewNode, aOldNode: TGosHashedStringListDictionaryNode;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Changing;

  aNewNvs := ExtractNameValue(S, aNewName, aNewValue);
  aOldNode := FNodeList[index];
  if (CaseSensitive and (aOldNode.ID <> aNewName)) or
     ((not CaseSensitive) and (not ALSametext(aOldNode.ID, aNewName))) then begin
    aNewNode := TGosHashedStringListDictionaryNode.Create;
    aNewNode.Idx := Index;
    aNewNode.ID := aNewName;
    aNewNode.Val := ANewValue;
    aNewNode.NVS := aNewNvs;
    aNewNode.Obj := aOldNode.Obj;
    {$IF CompilerVersion <= 32} // tokyo
    if not Fdictionary.ContainsKey(aNewName) then Fdictionary.Add(aNewName, aNewNode)
    else begin
    {$ELSE}
    if not Fdictionary.TryAdd(aNewName, aNewNode) then begin
    {$ENDIF}
      GosFreeAndNil(aNewNode);
      case Duplicates of
        dupIgnore: Exit;
        else Error(@SDuplicateString, 0);
      end;
    end;
    FNodeList[Index] := aNewNode;
    Fdictionary.Remove(aOldNode.ID);
  end
  else begin
    aOldNode.Val := aNewValue;
    aOldNode.NVS := aNewNVS;
  end;

  Changed;
end;

{************************************************************************}
procedure TGosHashedStringList.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FNodeList[Index].Obj
  else
    Obj := nil;

  FNodeList[Index].Obj := AObject;

  if Obj <> nil then
    GosFreeAndNil(Obj);

  Changed;
end;

{**************************************************************}
procedure TGosHashedStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <= FDictionary.Count then FDictionary.TrimExcess;
  FNodeList.Capacity := NewCapacity;
end;

{***********************************************************************************************}
procedure TGosHashedStringList.QuickSort(L, R: Integer; SCompare: TGosHashedStringListSortCompare);
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

{**************************************************************}
procedure TGosHashedStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{********************************************************************************}
procedure TGosHashedStringList.CustomSort(Compare: TGosHashedStringListSortCompare);
begin
  if (Count > 1) then
  begin
    Changing;
    QuickSort(0, Count - 1, Compare);
    Changed;
  end;
end;

{***********************************************************************************************************************************************************}
function TGosHashedStringList.CreateDictionary(ACapacity: integer; aCaseSensitive: boolean): TObjectDictionary<ansiString, TGosHashedStringListDictionaryNode>;
begin
  if aCaseSensitive then result := TObjectDictionary<ansiString, TGosHashedStringListDictionaryNode>.create([doOwnsValues],
                                                                                                           ACapacity,
                                                                                                           TDelegatedEqualityComparer<ansiString>.Create(
                                                                                                             function(const Left, Right: ansiString): Boolean
                                                                                                             begin
                                                                                                               Result := ALSameText(Left, Right);
                                                                                                             end,
                                                                                                             function(const Value: ansiString): Integer
                                                                                                             begin
                                                                                                               Result := THashBobJenkins.GetHashValue(PAnsiChar(Value)^, Length(Value) * SizeOf(AnsiChar));
                                                                                                             end))
  else result := TObjectDictionary<ansiString, TGosHashedStringListDictionaryNode>.create([doOwnsValues],
                                                                                         ACapacity,
                                                                                         TDelegatedEqualityComparer<ansiString>.Create(
                                                                                           function(const Left, Right: ansiString): Boolean
                                                                                           begin
                                                                                             Result := ALSameText(Left, Right);
                                                                                           end,
                                                                                           function(const Value: ansiString): Integer
                                                                                           var LLowerValue: ansiString;
                                                                                           begin
                                                                                             LLowerValue := ALLowerCase(Value);
                                                                                             Result := THashBobJenkins.GetHashValue(PAnsiChar(LLowerValue)^, Length(LLowerValue) * SizeOf(AnsiChar));
                                                                                           end));
end;

{***************************************************************************}
procedure TGosHashedStringList.init(OwnsObjects: Boolean; ACapacity: Integer);
begin
  FDictionary := CreateDictionary(ACapacity, False);
  FCaseSensitive := False;
  FNodeList := TObjectList<TGosHashedStringListDictionaryNode>.Create(False);
  FNodeList.Capacity := ACapacity;
  FDuplicates := dupError;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
end;

{*************************************}
constructor TGosHashedStringList.Create;
begin
  inherited Create;
  init(False, 0);
end;

{***********************************************************}
constructor TGosHashedStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects, 0);
end;

{*********************************************************}
constructor TGosHashedStringList.Create(ACapacity: Integer);
begin
  inherited Create;
  init(False, ACapacity);
end;

{*******************************************************************************}
constructor TGosHashedStringList.Create(OwnsObjects: Boolean; ACapacity: Integer);
begin
  inherited Create;
  init(OwnsObjects, ACapacity);
end;

type

  {************************}
  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if System.Generics.Collections.TObjectDictionary<TKey,TValue> was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALObjectDictionaryAccessPrivate<TKey,TValue> = class(TObjectDictionary<TKey,TValue>)
  private
    FOwnerships: TDictionaryOwnerships;
  end;

{*******************************************************************}
procedure TGosHashedStringList.SetCaseSensitive(const Value: Boolean);
var aTmpDictionary: TObjectDictionary<ansiString, TGosHashedStringListDictionaryNode>;
    i: integer;
begin
  if fCaseSensitive <> Value then begin
    aTmpDictionary := CreateDictionary(count, Value);
    for I := 0 to FnodeList.Count - 1 do
      aTmpDictionary.Add(FNodeList[i].ID,FNodeList[i]);
    TALObjectDictionaryAccessPrivate<ansiString, TGosHashedStringListDictionaryNode>(Fdictionary).fOwnerships := [];
    GosFreeAndNil(Fdictionary);
    FDictionary := aTmpDictionary;
  end;
end;

{*****************************************************}
function TGosHashedStringList.GetCaseSensitive: Boolean;
begin
  result := fCaseSensitive;
end;

{********************************************************************}
procedure TGosHashedStringList.SetDuplicates(const Value: TDuplicates);
begin
  if value = dupAccept then raise exception.Create('TGosHashedStringList does not support duplicate Names');
  FDuplicates := Value;
end;

{*******************************************************************************************************}
Function TGosHashedStringList.ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
Var P1: Integer;
begin
  P1 := AlPos(NameValueSeparator,S);
  if P1 > 0 then begin
    result := True;
    Name := AlCopyStr(S,1,P1-1);
    Value := AlCopyStr(S,P1+1, maxint);
  end
  else begin
    Result := False;
    Name := S;
    Value := '';
  end;
end;

{***************************************************************}
function TGosHashedStringList.GetName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := FNodelist[Index].ID;
end;

{*********************************************************************}
function TGosHashedStringList.GetStrictName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  if FNodelist[Index].nvs then Result := FNodelist[Index].ID
  else result := ''
end;

{************************************************************************}
function TGosHashedStringList.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then begin
    if FNodelist[i].nvs then Result := FNodelist[i].Val
    else Result := '';
  end else
    Result := '';
end;

{********************************************************************}
procedure TGosHashedStringList.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      Changing;
      FNodeList[i].Val := Value;
      FNodeList[i].NVS := True;
      Changed;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{*************************************************************************}
function TGosHashedStringList.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    if (FNodeList[index].Nvs) then
      result := FNodeList[index].Val
    else
      Result := '';
  end
  else
    Result := '';
end;

{***************************************************************************************}
procedure TGosHashedStringList.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(Count) then
        Error(@SListIndexError, Index);
      Changing;
      FNodeList[Index].Val := Value;
      FNodeList[Index].NVS := True;
      Changed;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{******************************************************************************}
procedure TGosHashedStringList.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    Changing;
    FNodeList[I].Val := Value;
    FNodeList[I].NVS := True;
    Changed;
  end
end;

{*************************************************************************************************}
procedure TGosHashedStringList.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    Changing;
    FNodeList[Index].Val := Value;
    FNodeList[Index].NVS := True;
    Changed;
  end;
end;

{$ENDIF}

{**************************************************************}
constructor TALStringsEnumeratorU.Create(AStrings: TALStringsU);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

{************************************************}
function TALStringsEnumeratorU.GetCurrent: String;
begin
  Result := FStrings[FIndex];
end;

{***********************************************}
function TALStringsEnumeratorU.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FStrings.Count;
end;

{*****************************}
constructor TALStringsU.Create;
begin
  inherited Create;
  FDefaultEncoding := TEncoding.UTF8;
  FEncoding := nil;
  FWriteBOM := True;
  FDelimiter := ',';
  FLineBreak := sLineBreak;
  FQuoteChar := '"';
  FNameValueSeparator := '=';
  FStrictDelimiter:= False;
  FUpdateCount:= 0;
  fProtectedSave := False;
end;

{*****************************}
destructor TALStringsU.Destroy;
begin
  if (FEncoding <> nil) and (not TEncoding.IsStandardEncoding(FEncoding)) then
    GosFreeAndNil(FEncoding);
  if (FDefaultEncoding <> nil) and (not TEncoding.IsStandardEncoding(FDefaultEncoding)) then
    GosFreeAndNil(FDefaultEncoding);
  inherited Destroy;
end;

{*************************************************}
function TALStringsU.Add(const S: String): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

{*************************************************************************}
function TALStringsU.AddObject(const S: String; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

{********************************************************************}
function TALStringsU.AddNameValue(const Name, Value: String): Integer;
begin
  result := add(name + NameValueSeparator + Value);
end;

{********************************************************************************************}
function TALStringsU.AddNameValueObject(const Name, Value: String; AObject: TObject): Integer;
begin
  result := addObject(name + NameValueSeparator + Value, AObject);
end;

{********************************************}
procedure TALStringsU.Append(const S: String);
begin
  Add(S);
end;

{*****************************************************}
procedure TALStringsU.AddStrings(Strings: TALStringsU);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

{**************************************************************}
procedure TALStringsU.AddStrings(const Strings: TArray<String>);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      Add(Strings[I]);
  finally
    EndUpdate;
  end;
end;

{**********************************************************************************************}
procedure TALStringsU.AddStrings(const Strings: TArray<String>; const Objects: TArray<TObject>);
var
  I: Integer;
begin
  if Length(Strings) <> Length(Objects) then
    raise EArgumentOutOfRangeException.CreateRes(@System.RTLConsts.sInvalidStringAndObjectArrays);
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      AddObject(Strings[I], Objects[I]);
  finally
    EndUpdate;
  end;
end;

{************************************************}
procedure TALStringsU.Assign(Source: TPersistent);
var i: integer;
begin
  if Source is TALStringsU then
  begin
    BeginUpdate;
    try
      Clear;
      // Must use property setter for DefaultEncoding
      DefaultEncoding := TALStringsU(Source).DefaultEncoding;
      // Must use internal property setter for Encoding
      SetEncoding(TALStringsU(Source).Encoding);
      NameValueSeparator := TALStringsU(Source).NameValueSeparator;
      QuoteChar := TALStringsU(Source).QuoteChar;
      Delimiter := TALStringsU(Source).Delimiter;
      LineBreak := TALStringsU(Source).LineBreak;
      StrictDelimiter := TALStringsU(Source).StrictDelimiter;
      WriteBOM := TALStringsU(Source).WriteBOM;
      AddStrings(TALStringsU(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      // Must use property setter for DefaultEncoding
      DefaultEncoding := TStrings(Source).DefaultEncoding;
      // Must use internal property setter for Encoding
      SetEncoding(TStrings(Source).Encoding);
      NameValueSeparator := TStrings(Source).NameValueSeparator;
      QuoteChar := TStrings(Source).QuoteChar;
      Delimiter := TStrings(Source).Delimiter;
      LineBreak := TStrings(Source).LineBreak;
      StrictDelimiter := TStrings(Source).StrictDelimiter;
      WriteBOM := TStrings(Source).WriteBOM;
      for I := 0 to Tstrings(Source).Count - 1 do
        AddObject(Tstrings(Source)[I], Tstrings(Source).Objects[I]);
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

{************************************************}
procedure TALStringsU.AssignTo(Dest: TPersistent);
var i: integer;
begin
  if Dest is TStrings then
  begin
    Tstrings(Dest).BeginUpdate;
    try
      Tstrings(Dest).Clear;
      Tstrings(Dest).NameValueSeparator := NameValueSeparator;
      Tstrings(Dest).QuoteChar := QuoteChar;
      Tstrings(Dest).Delimiter := Delimiter;
      Tstrings(Dest).LineBreak := LineBreak;
      Tstrings(Dest).StrictDelimiter := StrictDelimiter;
      for I := 0 to Count - 1 do
        Tstrings(Dest).AddObject(get(I), Objects[I]);
    finally
      Tstrings(Dest).EndUpdate;
    end;
    Exit;
  end;
  inherited AssignTo(Dest);
end;

{********************************}
procedure TALStringsU.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

{******************************}
procedure TALStringsU.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

{*********************************************************}
function TALStringsU.Equals(Strings: TALStringsU): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

{************************************************************}
procedure TALStringsU.Error(const Msg: String; Data: Integer);
begin
  raise EStringListError.CreateFmt(Msg, [Data]);
end;

{*************************************************************}
procedure TALStringsU.Error(Msg: PResStringRec; Data: Integer);
begin
  raise EStringListError.CreateFmt(LoadResString(Msg), [Data]);
end;

{******************************************************}
procedure TALStringsU.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: String;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

{********************************************************}
function TALStringsU.ExtractName(const S: String): String;
var
  P: Integer;
begin
  Result := S;
  P := ALPosU(NameValueSeparator, Result);

  // change behavior from original Tstring
  // i thing that if a Tstring have an item
  //
  // item1
  //
  // then set MyTstrings.values[item1] := Value1
  // must do
  //
  // item1=Value1
  //
  // instead of what he actually do
  //
  // item1
  // item1=Value1
  //
  // also when MyTStrings contain
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4
  // item5=Value5
  //
  // then doing
  // MyTStrings.valueFromIndex[4] := 'value4'
  // must result in
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4=Value4
  // item5=Value5
  //
  // instead of the current behavior of Tstrings
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // =Value4
  // item5=Value5
  //
  //
  // Original function:
  //
  // if P <> 0 then
  //   SetLength(Result, P-1) else
  //   SetLength(Result, 0);

  if P <> 0 then SetLength(Result, P-1);
end;

{****************************************}
function TALStringsU.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

{****************************************}
function TALStringsU.GetCommaText: String;
var
  LOldDelimiter: Char;
  LOldQuoteChar: Char;
begin
  LOldDelimiter := Delimiter;
  LOldQuoteChar := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    Delimiter := LOldDelimiter;
    QuoteChar := LOldQuoteChar;
  end;
end;

{**************************}
{$WARN WIDECHAR_REDUCED OFF}
function TALStringsU.GetDelimitedText: String;
var
  S: String;
  P: PChar;
  I, Count: Integer;
  LDelimiters: set of Char;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    LDelimiters := [Char(#0), Char(QuoteChar), Char(Delimiter)];
    if not StrictDelimiter then
      LDelimiters := LDelimiters + [Char(#1)..Char(' ')];
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PChar(S);
      while not (P^ in LDelimiters) do
        Inc(P);
      if (P^ <> #0) then S := ALQuotedStrU(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;
{$WARN WIDECHAR_REDUCED ON}

{********************************************************}
function TALStringsU.GetEnumerator: TALStringsEnumeratorU;
begin
  Result := TALStringsEnumeratorU.Create(Self);
end;

{***************************************************}
function TALStringsU.GetName(Index: Integer): String;
begin
  Result := ExtractName(Get(Index));
end;

{*********************************************************}
function TALStringsU.GetStrictName(Index: Integer): String;
var P: Integer;
begin
  Result := Get(Index);
  P := ALPosU(NameValueSeparator, Result);
  if P <> 0 then SetLength(Result, P-1)
  else SetLength(Result, 0);
end;

{******************************************************}
function TALStringsU.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

{**********************************}
function TALStringsU.GetText: PChar;
begin
  Result := StrNew(PChar(GetTextStr));
end;

{**************************************}
function TALStringsU.GetTextStr: String;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S, LB: String;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      GosMove(Pointer(S)^, P^, L*SizeOf(Char));
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      GosMove(Pointer(LB)^, P^, L*SizeOf(Char));
      Inc(P, L);
    end;
  end;
end;

{********************************************************}
function TALStringsU.GetValue(const Name: String): String;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := ALCopyStrU(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

{*****************************************************}
function TALStringsU.IndexOf(const S: String): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

{************************************************************}
function TALStringsU.IndexOfName(const Name: String): Integer;
var
  P: Integer;
  S: String;
begin

  // change behavior from original Tstring
  // i thing that if a Tstring have an item
  //
  // item1
  //
  // then set MyTstrings.values[item1] := Value1
  // must do
  //
  // item1=Value1
  //
  // instead of what he actually do
  //
  // item1
  // item1=Value1
  //
  // also when MyTStrings contain
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4
  // item5=Value5
  //
  // then doing
  // MyTStrings.valueFromIndex[4] := 'value4'
  // must result in
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4=Value4
  // item5=Value5
  //
  // instead of the current behavior of Tstrings
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // =Value4
  // item5=Value5
  //
  //
  // Original function:
  //
  // for Result := 0 to GetCount - 1 do
  // begin
  //   S := Get(Result);
  //   P := ALPos(NameValueSeparator, S);
  //   if (P <> 0) and (CompareStrings(ALCopyStr(S, 1, P - 1), Name) = 0) then Exit;
  // end;
  // Result := -1;

  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := ALPosU(NameValueSeparator, S);
    if ((P <> 0) and (CompareStrings(ALCopyStrU(S, 1, P - 1), Name) = 0)) or
       ((P = 0) and (CompareStrings(S, Name) = 0)) then Exit;
  end;
  Result := -1;
end;

{************************************************************}
function TALStringsU.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{************************************************************************************}
procedure TALStringsU.InsertObject(Index: Integer; const S: String; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

{*******************************************************************************}
procedure TALStringsU.InsertNameValue(Index: Integer; const Name, Value: String);
begin
  Insert(Index, name + NameValueSeparator + Value);
end;

{*******************************************************************************************************}
procedure TALStringsU.InsertNameValueObject(Index: Integer; const Name, Value: String; AObject: TObject);
begin
  InsertObject(Index, name + NameValueSeparator + Value, AObject);
end;

{*********************************************************}
procedure TALStringsU.LoadFromFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    GosFreeAndNil(Stream);
  end;
end;

{******************************************************************************}
procedure TALStringsU.LoadFromFile(const FileName: string; Encoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, Encoding);
  finally
    GosFreeAndNil(Stream);
  end;
end;

{****************************************************}
procedure TALStringsU.LoadFromStream(Stream: TStream);
begin
  LoadFromStream(Stream, nil);
end;

{*************************************************************************}
procedure TALStringsU.LoadFromStream(Stream: TStream; Encoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    {$IF CompilerVersion >= 30}{Delphi seattle}
    Stream.ReadBuffer(Buffer, 0, Size);
    {$else}
    Stream.Read(Buffer, 0, Size);
    {$ifend}
    Size := TEncoding.GetBufferEncoding(Buffer, Encoding, FDefaultEncoding);
    SetEncoding(Encoding); // Keep Encoding in case the stream is saved
    SetTextStr(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALStringsU.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: String;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      PutObject(CurIndex, nil);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

{*********************************************************}
procedure TALStringsU.Put(Index: Integer; const S: String);
begin
end;

{****************************************************************}
procedure TALStringsU.PutObject(Index: Integer; AObject: TObject);
begin
end;

{*******************************************************}
procedure TALStringsU.SaveToFile(const FileName: string);
begin
  SaveToFile(FileName, FEncoding);
end;

{****************************************************************************}
procedure TALStringsU.SaveToFile(const FileName: string; Encoding: TEncoding);
Var afileStream: TfileStream;
    aTmpFilename: String;
begin
  if ProtectedSave then aTmpFilename := FileName + '.~tmp'
  else aTmpFilename := FileName;
  try

    aFileStream := TfileStream.Create(aTmpFilename,fmCreate);
    Try
      SaveToStream(aFileStream, Encoding);
    finally
      GosFreeAndNil(aFileStream);
    end;

    if aTmpFilename <> FileName then begin
      if TFile.Exists(FileName) then TFile.Delete(FileName);
      TFile.Move(aTmpFilename, FileName);
    end;

  except
    if (aTmpFilename <> FileName) and
       (TFile.Exists(aTmpFilename)) then TFile.Delete(aTmpFilename);
    raise;
  end;
end;

{**************************************************}
procedure TALStringsU.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, FEncoding);
end;

{***********************************************************************}
procedure TALStringsU.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := FDefaultEncoding;
  Buffer := Encoding.GetBytes(GetTextStr);
  if WriteBOM then
  begin
    Preamble := Encoding.GetPreamble;
    if Length(Preamble) > 0 then
      Stream.WriteBuffer(Preamble, Length(Preamble));
  end;
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

{******************************************************}
procedure TALStringsU.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

{******************************************************}
procedure TALStringsU.SetCommaText(const Value: String);
var
  LOldDelimiter: Char;
  LOldQuoteChar: Char;
begin
  LOldDelimiter := Delimiter;
  LOldQuoteChar := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    SetDelimitedText(Value);
  finally
    Delimiter := LOldDelimiter;
    QuoteChar := LOldQuoteChar;
  end;
end;

{*****************************************}
procedure TALStringsU.SetText(Text: PChar);
begin
  SetTextStr(Text);
end;

{**************************}
{$WARN WIDECHAR_REDUCED OFF}
procedure TALStringsU.SetTextStr(const Value: String);
var
  P, Start, LB: PChar;
  S: String;
  LineBreakLen: Integer;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      if ALCompareStrU(LineBreak, sLineBreak) = 0 then
      begin
        // This is a lot faster than using StrPos/AnsiStrPos when
        // LineBreak is the default (#13#10)
        while P^ <> #0 do
        begin
          Start := P;
          while not (P^ in [#0, #10, #13]) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P^ = #13 then Inc(P);
          if P^ = #10 then Inc(P);
        end;
      end
      else
      begin
        LineBreakLen := Length(LineBreak);
        while P^ <> #0 do
        begin
          Start := P;
          LB := StrPos(P, PChar(LineBreak));
          while (P^ <> #0) and (P <> LB) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P = LB then
            Inc(P, LineBreakLen);
        end;
      end;
  finally
    EndUpdate;
  end;
end;
{$WARN WIDECHAR_REDUCED ON}

{******************************************************}
procedure TALStringsU.SetUpdateState(Updating: Boolean);
begin
end;

{********************************************************}
procedure TALStringsU.SetValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then Add(Name + NameValueSeparator + Value)
    else Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{******************************************************************}
procedure TALStringsU.SetPersistentValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then Add(Name + NameValueSeparator + Value)
  else Put(I, Name + NameValueSeparator + Value);
end;

{***************************************************************}
procedure TALStringsU.SetDefaultEncoding(const Value: TEncoding);
begin
  if (FDefaultEncoding <> nil) and (not TEncoding.IsStandardEncoding(FDefaultEncoding)) then
    GosFreeAndNil(FDefaultEncoding);
  if TEncoding.IsStandardEncoding(Value) then
    FDefaultEncoding := Value
  else if Value <> nil then
    FDefaultEncoding := Value.Clone
  else
    FDefaultEncoding := TEncoding.UTF8;
end;

{**********************************************************}
procedure TALStringsU.SetDelimitedText(const Value: String);
var
  P, P1: PChar;
  S: String;
begin
  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    if not StrictDelimiter then
      while ((P^ >= char(#1)) and (P^ <= char(' '))) do
        Inc(P);
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := ALExtractQuotedStrU(P, QuoteChar)
      else
      begin
        P1 := P;
        while ((not StrictDelimiter and (P^ > ' ')) or
              (StrictDelimiter and (P^ <> #0))) and (P^ <> Delimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not StrictDelimiter then
        while ((P^ >= char(#1)) and (P^ <= char(' '))) do
          Inc(P);

      if P^ = Delimiter then
      begin
        P1 := P;
        Inc(P1);
        if P1^ = #0 then
          Add('');
        repeat
          Inc(P);
        until not (not StrictDelimiter and ((P^ >= char(#1)) and (P^ <= char(' '))));
      end;
    end;
  finally
    EndUpdate;
  end;
end;

{********************************************************}
procedure TALStringsU.SetEncoding(const Value: TEncoding);
begin
  if (FEncoding <> nil) and (not TEncoding.IsStandardEncoding(FEncoding)) then
    GosFreeAndNil(FEncoding);
  if TEncoding.IsStandardEncoding(Value) then
    FEncoding := Value
  else if Value <> nil then
    FEncoding := Value.Clone
  else
    FEncoding := TEncoding.UTF8;
end;

{*****************************************************************}
function TALStringsU.CompareStrings(const S1, S2: String): Integer;
begin
  Result := ALCompareTextU(S1, S2);
end;

{*************************************************************}
function TALStringsU.GetValueFromIndex(Index: Integer): String;
var
  SepPos: Integer;
begin
  if Index >= 0 then
  begin
    Result := Get(Index);
    SepPos := ALPosU(NameValueSeparator, Result);
    if (SepPos > 0) then
      System.Delete(Result, 1, SepPos)
    else
      Result := '';
  end
  else
    Result := '';
end;

{***************************************************************************}
procedure TALStringsU.SetValueFromIndex(Index: Integer; const Value: String);
begin
  if Value <> '' then
  begin
    if Index < 0 then Add(NameValueSeparator + Value)
    else Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

{*************************************************************************************}
procedure TALStringsU.SetPersistentValueFromIndex(Index: Integer; const Value: String);
begin
  if Index < 0 then Add(NameValueSeparator + Value)
  else Put(Index, Names[Index] + NameValueSeparator + Value);
end;

{*************************************************}
function TALStringsU.ToStringArray: TArray<String>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Strings[I];
end;

{**************************************************}
function TALStringsU.ToObjectArray: TArray<TObject>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Objects[I];
end;

{********************************}
destructor TALStringListU.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do
      Temp[I] := FList[I].FObject;
  end;

  inherited Destroy;
  FCount := 0;
  SetCapacity(0);

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      GosFreeAndNil(Temp[I]);
end;

{****************************************************}
function TALStringListU.Add(const S: String): Integer;
begin
  Result := AddObject(S, nil);
end;

{****************************************************************************}
function TALStringListU.AddObject(const S: String; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

{***************************************************}
procedure TALStringListU.Assign(Source: TPersistent);
begin
  if Source is TALStringListU then
  begin
    Clear;
    FCaseSensitive := TALStringListU(Source).FCaseSensitive;
    FDuplicates := TALStringListU(Source).FDuplicates;
    FSorted := TALStringListU(Source).FSorted;
  end
  else if Source is TALNVStringListU then
  begin
    Clear;
    FCaseSensitive := TALNVStringListU(Source).FCaseSensitive;
    FDuplicates := TALNVStringListU(Source).FDuplicates;
    FSorted := TALNVStringListU(Source).FSorted;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    Duplicates := TStringList(Source).Duplicates;
    Sorted := TStringList(Source).Sorted;
  end;
  inherited Assign(Source);
end;

{***************************************************}
procedure TALStringListU.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := fCaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := fSorted;
  end;
  inherited AssignTo(Dest);
end;

{*******************************}
procedure TALStringListU.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{********************************}
procedure TALStringListU.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{*****************************}
procedure TALStringListU.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if FCount <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, FCount);
      for I := 0 to FCount - 1 do
        Temp[I] := FList[I].FObject;
    end;

    FCount := 0;
    SetCapacity(0);

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        GosFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{**********************************************}
procedure TALStringListU.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    GosMove(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(TALStringItemU));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FString)^ := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    GosFreeAndNil(Obj);
  Changed;
end;

{**************************************************************}
function  TALStringListU.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  result := FList[Index].FObject;
  FList[Index].FObject := nil;
  Changed;
end;

{*********************************************************}
procedure TALStringListU.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{**************************************************************}
procedure TALStringListU.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PALStringItemU;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];
  Temp := Pointer(Item1^.FString);
  Pointer(Item1^.FString) := Pointer(Item2^.FString);
  Pointer(Item2^.FString) := Temp;
  Temp := pointer(Item1^.FObject);
  pointer(Item1^.FObject) := pointer(Item2^.FObject);
  pointer(Item2^.FObject) := Temp;
end;

{*************************************************************************}
function TALStringListU.Find(const S: String; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{*****************************************************************************}
function TALStringListU.FindName(const S: String; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(ExtractName(FList[I].FString), S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          if CompareStrings(ExtractName(FList[I].FString), s) = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{**************************************************}
function TALStringListU.Get(Index: Integer): String;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FString;
end;

{*******************************************}
function TALStringListU.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

{****************************************}
function TALStringListU.GetCount: Integer;
begin
  Result := FCount;
end;

{*********************************************************}
function TALStringListU.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FObject;
end;

{****************************}
procedure TALStringListU.Grow;
{$IF CompilerVersion <= 32}{tokyo}
var
  Delta: Integer;
{$endif}
begin
  {$IF CompilerVersion <= 32}{tokyo}
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
  {$else}
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
  {$endif}
end;

{********************************************************}
function TALStringListU.IndexOf(const S: String): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

{***************************************************************}
function TALStringListU.IndexOfName(const Name: String): Integer;
begin
  if (not Sorted) or (not FNameValueOptimization) then Result := inherited IndexOfName(Name)
  else if not FindName(Name, Result) then Result := -1;
end;

{***************************************************************}
procedure TALStringListU.Insert(Index: Integer; const S: String);
begin
  InsertObject(Index, S, nil);
end;

{***************************************************************************************}
procedure TALStringListU.InsertObject(Index: Integer; const S: String; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{*********************************************************}
procedure TALStringListU.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: String;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      FList[CurIndex].FObject := nil;
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************************************}
procedure TALStringListU.InsertItem(Index: Integer; const S: String; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    GosMove(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TALStringItemU));
  Pointer(FList[Index].FString) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FString := S;
  Inc(FCount);
  Changed;
end;

{************************************************************}
procedure TALStringListU.Put(Index: Integer; const S: String);
begin
  if not sorted then begin
    if Cardinal(Index) >= Cardinal(FCount) then
      Error(@SListIndexError, Index);
    Changing;
    FList[Index].FString := S;
    Changed;
  end
  else begin
    delete(index);
    add(s);
  end;
end;

{*******************************************************************}
procedure TALStringListU.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  FList[Index].FObject := AObject;

  if Obj <> nil then
    GosFreeAndNil(Obj);

  Changed;
end;

{*************************************************************************************}
procedure TALStringListU.QuickSort(L, R: Integer; SCompare: TALStringListSortCompareU);
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

{*********************************************************}
procedure TALStringListU.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{*************************************************}
procedure TALStringListU.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{*********************************************************}
procedure TALStringListU.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{*******************************************************************************************}
function ALStringListCompareStringsU(List: TALStringListU; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList[Index1].FString,
                                List.FList[Index2].FString);
end;

{****************************}
procedure TALStringListU.Sort;
begin
  CustomSort(ALStringListCompareStringsU);
end;

{**********************************************************************}
procedure TALStringListU.CustomSort(Compare: TALStringListSortCompareU);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{********************************************************************}
function TALStringListU.CompareStrings(const S1, S2: String): Integer;

  {---------------------------------------------------------}
  function internalCompareStr(const S1, S2: String): Integer;
  var
    P1, P2: PChar;
    I: Integer;
    L1, L2: Integer;
  begin
    { Length and PChar of S1 }
    L1 := Length(S1);
    P1 := PChar(S1);

    { Length and PChar of S2 }
    L2 := Length(S2);
    P2 := PChar(S2);

    { Continue the loop until the end of one string is reached. }
    I := 0;
    while (I < L1) and (I < L2) do
    begin
      if (P1^ <> P2^) then begin
        if (P1^ = NameValueSeparator) then result := -1
        else if (P2^ = NameValueSeparator) then result := 1
        else result := Ord(P1^) - Ord(P2^);
        Exit;
      end;

      Inc(P1);
      Inc(P2);
      Inc(I);
    end;

    { If chars were not different return the difference in length }
    Result := L1 - L2;
  end;

  {----------------------------------------------------------}
  function internalCompareText(const S1, S2: String): Integer;
  var
    P1, P2: PChar;
    I: Integer;
    C1, C2: Char;
    L1, L2: Integer;
  begin
    { Length and PChar of S1 }
    L1 := Length(S1);
    P1 := PChar(S1);

    { Length and PChar of S2 }
    L2 := Length(S2);
    P2 := PChar(S2);

    { Continue the loop until the end of one string is reached. }
    I := 0;
    while (I < L1) and (I < L2) do
    begin
      if (P1^ >= char('a')) and (P1^ <= char('z')) then
        C1 := Char(Byte(P1^) xor $20)
      else
        C1 := P1^;

      if (P2^ >= char('a')) and (P2^ <= char('z')) then
        C2 := Char(Byte(P2^) xor $20)
      else
        C2 := P2^;

      if (C1 <> C2) then begin
        if (C1 = NameValueSeparator) then result := -1
        else if (C2 = NameValueSeparator) then result := 1
        else result := Ord(C1) - Ord(C2);
        Exit;
      end;

      Inc(P1);
      Inc(P2);
      Inc(I);
    end;

    { If chars were not different return the difference in length }
    Result := L1 - L2;
  end;

begin

  // Orignial Delphi Code
  // the difference between TGosStringList and TStringList is that
  // TstringList use ansiCompareStr or ansiCompareText that are
  // dependant from the local. I don't like this behavior because
  // as you can read
  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd317759(v=vs.85).aspx
  // "Using CompareString incorrectly can compromise the security of your
  // application. Strings that are not compared correctly can produce
  // invalid input. For example, the function can raise security issues when
  // used for a non-linguistic comparison, because two strings that are
  // distinct in their binary representation can be linguistically equivalent"
  // so i prefere to use instead CompareStr and CompareText but only
  // a..z = A..Z will be handle when case insensitive is set.
  // other behavior must be handle in descendant classe
  //
  // also not the ansiCompareStr and ansiCompareText
  // are 10x more slower than CompareStr and CompareText
  //
  // if CaseSensitive then
  //   Result := AnsiCompareStr(S1, S2)
  // else
  //   Result := AnsiCompareText(S1, S2);

  // it's important that the order is not change because
  // of the ord(NameValueSeparator) this is need because of
  // function FindName
  //
  // EX the items
  //
  //   aaa0
  //   aaa=123
  //   aaaa
  //
  // must be ordered like
  //
  //   aaa=123   |     aaa
  //   aaa0      |     aaa0
  //   aaaa      |     aaaa
  //                   => OK, ordered work with findname
  //
  // but with just Result := ALCompareText(S1, S2)
  // it's will be ordered like
  //
  //   aaa0      |     aaa0
  //   aaa=123   |     aaa
  //   aaaa      |     aaaa
  //                   => KO, NOT ordered, break the findname
  //
  // so we need to give to the NameValueSeparator the lowest ASCII
  // number (#0). for this we must use custom CompareStr and
  // custom CompareText. the cost is that our custom implementation
  // of internalCompareStr and internalCompareText (based on the pure
  // pascal implementation of CompareStr and CompareText, will be aound
  // 50% more slower than the ASM version of CompareStr and CompareText
  // IE 50% more slower with average 50 bytes for S1 and S2, it's even
  // more slower when the number of bytes in S1 and S2 increase

  if  fNameValueOptimization then begin
    if CaseSensitive then
      Result := InternalCompareStr(S1, S2)
    else
      Result := InternalCompareText(S1, S2);
  end
  else begin
    if CaseSensitive then
      Result := AlCompareStrU(S1, S2)
    else
      Result := AlCompareTextU(S1, S2);
  end;
end;

{**************************************************}
procedure TALStringListU.init(OwnsObjects: Boolean);
begin
  setlength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupIgnore;
  FCaseSensitive := False;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
  FNameValueOptimization := True;
end;

{********************************}
constructor TALStringListU.Create;
begin
  inherited Create;
  init(False);
end;

{******************************************************}
constructor TALStringListU.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{**************************************************************}
procedure TALStringListU.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;

{**********************************}
destructor TALNVStringListU.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do
      Temp[I] := FList[I].FObject;
  end;

  inherited Destroy;
  FCount := 0;
  SetCapacity(0);

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      GosFreeAndNil(Temp[I]);
end;

{******************************************************}
function TALNVStringListU.Add(const S: String): Integer;
begin
  Result := AddObject(S, nil);
end;

{******************************************************************************}
function TALNVStringListU.AddObject(const S: String; AObject: TObject): Integer;
Var aName, aValue: String;
begin
  if not Sorted then begin
    Result := FCount;
    InsertItem(Result, S, AObject);
  end
  else begin
    if ExtractNameValue(S, aName, aValue) then begin
      if FindNameValue(aName, aValue, Result) then
        case Duplicates of
          dupIgnore: Exit;
          dupError: Error(@SDuplicateString, 0);
        end;
      InsertItem(Result, aName, aValue, AObject);
    end
    else begin
      if FindName(s, false{WithNvS}, Result) then
        case Duplicates of
          dupIgnore: Exit;
          dupError: Error(@SDuplicateString, 0);
        end;
      InsertItem(Result, s, False{WithNvS}, AObject);
    end;
  end;
end;

{*************************************************************************}
function TALNVStringListU.AddNameValue(const Name, Value: String): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{*************************************************************************************************}
function TALNVStringListU.AddNameValueObject(const Name, Value: String; AObject: TObject): Integer;
begin
  if not Sorted then begin
    Result := FCount;
  end
  else begin
    if FindNameValue(Name, Value, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  end;
  InsertItem(Result, Name, Value, AObject);
end;

{*****************************************************}
procedure TALNVStringListU.Assign(Source: TPersistent);
begin
  if Source is TALNVStringListU then
  begin
    Clear;
    FCaseSensitive := TALNVStringListU(Source).FCaseSensitive;
    FDuplicates := TALNVStringListU(Source).FDuplicates;
    FSorted := TALNVStringListU(Source).FSorted;
  end
  else if Source is TALStringListU then
  begin
    Clear;
    FCaseSensitive := TALStringListU(Source).FCaseSensitive;
    FDuplicates := TALStringListU(Source).FDuplicates;
    FSorted := TALStringListU(Source).FSorted;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    Duplicates := TStringList(Source).Duplicates;
    Sorted := TStringList(Source).Sorted;
  end;
  inherited Assign(Source);
end;

{*****************************************************}
procedure TALNVStringListU.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := fCaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := fSorted;
  end;
  inherited AssignTo(Dest);
end;

{*********************************}
procedure TALNVStringListU.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{**********************************}
procedure TALNVStringListU.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{*******************************}
procedure TALNVStringListU.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if FCount <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, FCount);
      for I := 0 to FCount - 1 do
        Temp[I] := FList[I].FObject;
    end;

    FCount := 0;
    SetCapacity(0);

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        GosFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{************************************************}
procedure TALNVStringListU.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    GosMove(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(TALNVStringItemU));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FName)^   := nil;
    PPointer(@FList[FCount].FValue)^  := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    GosFreeAndNil(Obj);
  Changed;
end;

{****************************************************************}
function  TALNVStringListU.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  result := FList[Index].FObject;
  FList[Index].FObject := nil;
  Changed;
end;

{***********************************************************}
procedure TALNVStringListU.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{****************************************************************}
procedure TALNVStringListU.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PALNVStringItemU;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];

  Temp := Pointer(Item1^.FName);
  Pointer(Item1^.FName) := Pointer(Item2^.FName);
  Pointer(Item2^.FName) := Temp;

  Temp := Pointer(Item1^.FValue);
  Pointer(Item1^.FValue) := Pointer(Item2^.FValue);
  Pointer(Item2^.FValue) := Temp;

  Temp := pointer(Item1^.FObject);
  pointer(Item1^.FObject) := pointer(Item2^.FObject);
  pointer(Item2^.FObject) := Temp;
end;

{***************************************************************************}
function TALNVStringListU.Find(const S: String; var Index: Integer): Boolean;
var Name, Value: String;
begin
  if ExtractNameValue(S, Name, Value) then result := FindNameValue(Name, Value, Index)
  else result := FindName(Name, False{WithNvS}, Index);
end;

{**********************************************************************************}
function TALNVStringListU.FindName(const Name: String; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);     // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          if CompareStrings(FList[I].FName, Name) = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{****************************************************************************************************}
function TALNVStringListU.FindName(const Name: String; WithNvS: boolean; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);     // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if (C = 0) then begin
      if (not WithNvS) and FList[I].FNVS then c := 1   // Must be ordered in this order :
                                                       // aa
                                                       // aaa
                                                       // aaa=
                                                       // aaaa
      else if (WithNvS) and (not FList[I].FNVS) then c := -1;  // Must be ordered in this order :
                                                               // aa
                                                               // aaa
                                                               // aaa=
                                                               // aaaa
    end;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          C := CompareStrings(FList[I].FName, Name);
          if (C = 0) then begin
            if (not WithNvS) and FList[I].FNVS then c := 1   // Must be ordered in this order :
                                                             // aa
                                                             // aaa
                                                             // aaa=
                                                             // aaaa
            else if (WithNvS) and (not FList[I].FNVS) then c := -1;  // Must be ordered in this order :
                                                                     // aa
                                                                     // aaa
                                                                     // aaa=
                                                                     // aaaa
          end;
          if c = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{**********************************************************************************************}
function TALNVStringListU.FindNameValue(const Name, Value: String; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);  // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if (C = 0) then begin
      if (not FList[I].FNVS) then c := -1 // Must be ordered in this order :
                                          // aa
                                          // aaa
                                          // aaa=
                                          // aaaa
      else C := CompareStrings(FList[I].FValue, Value);  // The return value is less than 0 if FList[I].FValue < Value, 0 if FList[I].FValue = Value, or greater than 0 if FList[I].FValue > Value.
    end;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{****************************************************}
function TALNVStringListU.Get(Index: Integer): String;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  if FList[Index].fNvs then Result := FList[Index].FName + NameValueSeparator + FList[Index].fValue
  else Result := FList[Index].fname;
end;

{*********************************************}
function TALNVStringListU.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

{******************************************}
function TALNVStringListU.GetCount: Integer;
begin
  Result := FCount;
end;

{***********************************************************}
function TALNVStringListU.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FObject;
end;

{******************************}
procedure TALNVStringListU.Grow;
{$IF CompilerVersion <= 32}{tokyo}
var
  Delta: Integer;
{$endif}
begin
  {$IF CompilerVersion <= 32}{tokyo}
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
  {$else}
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
  {$endif}
end;

{*******************************************}
function TALNVStringListU.GetTextStr: String;
var
  I, L, Size: Integer;
  P: PChar;
  S, LB: String;
  NvS: Char;
begin
  Size := 0;
  LB := LineBreak;
  NvS := nameValueSeparator;
  for I := 0 to FCount - 1 do begin
    if FList[i].fNvs then Inc(Size, Length(FList[i].fName) + 1{length(NameValueSeparator)} +  Length(FList[i].fValue) + Length(LB))
    else Inc(Size, Length(FList[i].fName) + Length(LB))
  end;
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to FCount - 1 do
  begin
    S := FList[i].fName;
    L := Length(S);
    if L <> 0 then
    begin
      GosMove(Pointer(S)^, P^, L*SizeOf(Char));
      Inc(P, L);
    end;
    if FList[i].fNvs then begin
      GosMove(NvS, P^, 1*SizeOf(Char));
      Inc(P, 1);
      S := FList[i].fValue;
      L := Length(S);
      if L <> 0 then
      begin
        GosMove(Pointer(S)^, P^, L*SizeOf(Char));
        Inc(P, L);
      end;
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      GosMove(Pointer(LB)^, P^, L*SizeOf(Char));
      Inc(P, L);
    end;
  end;
end;

{**********************************************************}
function TALNVStringListU.IndexOf(const S: String): Integer;
Var aName, aValue: String;
begin
  if ExtractNameValue(S, aName, aValue) then begin
    if not Sorted then begin
      for Result := 0 to FCount - 1 do
        if Flist[Result].FNVS and
           (CompareStrings(Flist[Result].FName, aName) = 0) and
           (CompareStrings(Flist[Result].FValue, aValue) = 0) then Exit;
      Result := -1;
    end
    else begin
      if not FindNameValue(aName, aValue, Result) then Result := -1;
    end;
  end
  else begin
    if not Sorted then begin
      for Result := 0 to FCount - 1 do
        if (not Flist[Result].FNVS) and
           (CompareStrings(Flist[Result].FName, s) = 0) then Exit;
      Result := -1;
    end
    else begin
      if not FindName(s, false{WinthNvS}, Result) then Result := -1;
    end;
  end;
end;

{*****************************************************************}
function TALNVStringListU.IndexOfName(const Name: String): Integer;
begin
  if not Sorted then begin
    for Result := 0 to FCount - 1 do
      if CompareStrings(Flist[Result].FName, Name) = 0 then Exit;
    Result := -1;
  end
  else begin
    if not FindName(Name, Result) then Result := -1;
  end;
end;

{*****************************************************************}
procedure TALNVStringListU.Insert(Index: Integer; const S: String);
begin
  InsertObject(Index, S, nil);
end;

{*****************************************************************************************}
procedure TALNVStringListU.InsertObject(Index: Integer; const S: String; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{************************************************************************************}
procedure TALNVStringListU.InsertNameValue(Index: Integer; const Name, Value: String);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{************************************************************************************************************}
procedure TALNVStringListU.InsertNameValueObject(Index: Integer; const Name, Value: String; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, Name, Value, AObject);
end;

{***********************************************************}
procedure TALNVStringListU.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempName: String;
  TempValue: String;
  TempNvS: Boolean;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempName := Flist[curIndex].FName;
      TempNvs := Flist[curIndex].FNvs;
      if TempNvs then TempValue := Flist[curIndex].FValue;
      TempObject := Flist[curIndex].FObject;
      FList[CurIndex].FObject := nil;
      Delete(CurIndex);
      if TempNvs then InsertObject(NewIndex, TempName, TempObject)
      else InsertNameValueObject(NewIndex, TempName, TempValue, TempObject)
    finally
      EndUpdate;
    end;
  end;
end;

{***************************************************************************************}
procedure TALNVStringListU.InsertItem(Index: Integer; const S: String; AObject: TObject);
var Name, Value: String;
begin
  if ExtractNameValue(S, Name, Value) then InsertItem(Index, Name, Value, AObject)
  else InsertItem(Index, s, False{WithNvS}, AObject);
end;

{*************************************************************************************************}
procedure TALNVStringListU.InsertItem(Index: Integer; const Name, Value: String; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    GosMove(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TALNVStringItemU));
  Pointer(FList[Index].FName) := nil;
  Pointer(FList[Index].FValue) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FName := Name;
  FList[Index].fNvs := true;
  FList[Index].FValue := Value;
  Inc(FCount);
  Changed;
end;

{************************************************************************************************************}
procedure TALNVStringListU.InsertItem(Index: Integer; const Name: String; WithNvS: boolean; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    GosMove(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TALNVStringItemU));
  Pointer(FList[Index].FName) := nil;
  Pointer(FList[Index].FValue) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FName := Name;
  FList[Index].fNvs := WithNvS;
  FList[Index].FValue := '';
  Inc(FCount);
  Changed;
end;

{**************************************************************}
procedure TALNVStringListU.Put(Index: Integer; const S: String);
var Name, Value: String;
begin
  if not sorted then begin
    if Cardinal(Index) >= Cardinal(FCount) then
      Error(@SListIndexError, Index);
    Changing;
    if ExtractNameValue(S, Name, Value) then begin
      FList[Index].FName := Name;
      FList[Index].FNvS := True;
      FList[Index].FValue := Value;
    end
    else begin
      FList[Index].FName := S;
      FList[Index].FNvS := False;
      FList[Index].FValue := '';
    end;
    Changed;
  end
  else begin
    delete(index);
    add(s);
  end;
end;

{*********************************************************************}
procedure TALNVStringListU.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  FList[Index].FObject := AObject;

  if Obj <> nil then
    GosFreeAndNil(Obj);

  Changed;
end;

{*****************************************************************************************}
procedure TALNVStringListU.QuickSort(L, R: Integer; SCompare: TALNVStringListSortCompareU);
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

{***********************************************************}
procedure TALNVStringListU.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{***************************************************}
procedure TALNVStringListU.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{***********************************************************}
procedure TALNVStringListU.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{***********************************************************************************************}
function ALNVStringListCompareStringsU(List: TALNVStringListU; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList[Index1].FName,
                                List.FList[Index2].FName);  // The return value is less than 0 if List.FList[Index1].FName < List.FList[Index2].FName, 0 if List.FList[Index1].FName = List.FList[Index2].FName, or greater than 0 if List.FList[Index1].FName > List.FList[Index2].FName.
  if result = 0 then begin
    if (not List.FList[Index1].fNvS) and List.FList[Index2].FNVS then result := -1  // Must be ordered in this order :
                                                                                    // aa
                                                                                    // aaa
                                                                                    // aaa=
                                                                                    // aaaa
    else if (List.FList[Index1].fNvS) and (not List.FList[Index2].FNVS) then result := 1;  // Must be ordered in this order :
                                                                                           // aa
                                                                                           // aaa
                                                                                           // aaa=
                                                                                           // aaaa
    if (result=0) then Result := List.CompareStrings(List.FList[Index1].FValue,
                                                     List.FList[Index2].FValue);  // The return value is less than 0 if List.FList[Index1].FValue < List.FList[Index2].FValue, 0 if List.FList[Index1].FValue = List.FList[Index2].FValue, or greater than 0 if List.FList[Index1].FValue > List.FList[Index2].FValue.
  end;
end;

{******************************}
procedure TALNVStringListU.Sort;
begin
  CustomSort(ALNVStringListCompareStringsU);
end;

{**************************************************************************}
procedure TALNVStringListU.CustomSort(Compare: TALNVStringListSortCompareU);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{**********************************************************************}
function TALNVStringListU.CompareStrings(const S1, S2: String): Integer;
begin

  // Orignial Delphi Code
  // the difference between TGosNVStringList and TStringList is that
  // TstringList use ansiCompareStr or ansiCompareText that are
  // dependant from the local. I don't like this behavior because
  // as you can read
  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd317759(v=vs.85).aspx
  // "Using CompareString incorrectly can compromise the security of your
  // application. Strings that are not compared correctly can produce
  // invalid input. For example, the function can raise security issues when
  // used for a non-linguistic comparison, because two strings that are
  // distinct in their binary representation can be linguistically equivalent"
  // so i prefere to use instead CompareStr and CompareText but only
  // a..z = A..Z will be handle when case insensitive is set.
  // other behavior must be handle in descendant classe
  //
  // also not the ansiCompareStr and ansiCompareText
  // are 10x more slower than CompareStr and CompareText
  //
  // if CaseSensitive then
  //   Result := AnsiCompareStr(S1, S2)
  // else
  //   Result := AnsiCompareText(S1, S2);

  // it's important that the order is not change because
  // of the ord(NameValueSeparator) this is need because of
  // function FindName
  //
  // EX the items
  //
  //   aaa0
  //   aaa=123
  //   aaaa
  //
  // must be ordered like
  //
  //   aaa=123   |     aaa
  //   aaa0      |     aaa0
  //   aaaa      |     aaaa
  //                   => OK, ordered work with findname
  //
  // but with just Result := ALCompareText(S1, S2)
  // it's will be ordered like
  //
  //   aaa0      |     aaa0
  //   aaa=123   |     aaa
  //   aaaa      |     aaaa
  //                   => KO, NOT ordered, break the findname
  //

  if CaseSensitive then
    Result := ALCompareStrU(S1, S2)
  else
    Result := ALCompareTextU(S1, S2);

end;

{****************************************************}
procedure TALNVStringListU.init(OwnsObjects: Boolean);
begin
  setlength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupIgnore;
  FCaseSensitive := False;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
end;

{**********************************}
constructor TALNVStringListU.Create;
begin
  inherited Create;
  init(False);
end;

{********************************************************}
constructor TALNVStringListU.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{****************************************************************}
procedure TALNVStringListU.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;

{********************************************************************************************}
Function TALNVStringListU.ExtractNameValue(const S: String; var Name, Value: String): Boolean;
Var P1: Integer;
begin
  P1 := AlPosU(NameValueSeparator,S);
  if P1 > 0 then begin
    result := True;
    Name := AlCopyStrU(S,1,P1-1);
    Value := AlCopyStrU(S,P1+1, maxint);
  end
  else begin
    Result := False;
    Name := S;
    Value := '';
  end;
end;

{********************************************************}
function TALNVStringListU.GetName(Index: Integer): String;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := Flist[Index].fName;
end;

{**************************************************************}
function TALNVStringListU.GetStrictName(Index: Integer): String;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  if Flist[Index].fnvs then Result := Flist[Index].fName
  else result := ''
end;

{*************************************************************}
function TALNVStringListU.GetValue(const Name: String): String;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then begin
    if Flist[i].fnvs then Result := Flist[i].fValue
    else Result := '';
  end else
    Result := '';
end;

{*************************************************************}
procedure TALNVStringListU.SetValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      Changing;
      Flist[i].fValue := Value;
      Flist[i].fNVS := True;
      Changed;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{******************************************************************}
function TALNVStringListU.GetValueFromIndex(Index: Integer): String;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    if (Flist[index].fNvs) then
      result := Flist[index].fValue
    else
      Result := '';
  end
  else
    Result := '';
end;

{********************************************************************************}
procedure TALNVStringListU.SetValueFromIndex(Index: Integer; const Value: String);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(Count) then
        Error(@SListIndexError, Index);
      Changing;
      Flist[Index].fValue := Value;
      Flist[Index].fNVS := True;
      Changed;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{***********************************************************************}
procedure TALNVStringListU.SetPersistentValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    Changing;
    Flist[I].fValue := Value;
    Flist[I].fNVS := True;
    Changed;
  end
end;

{******************************************************************************************}
procedure TALNVStringListU.SetPersistentValueFromIndex(Index: Integer; const Value: String);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    Changing;
    Flist[Index].fValue := Value;
    Flist[Index].fNVS := True;
    Changed;
  end;
end;

end.