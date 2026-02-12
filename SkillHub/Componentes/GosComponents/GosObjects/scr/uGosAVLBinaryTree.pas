{*************************************************************
product:      ALAVLBinaryTree (Self-Balancing Binary Trees)
Description:  Binary trees that are self-balancing in the AVL sense
              (the depth of any left branch differs by no more than
              one from the depth of the right branch).
**************************************************************}

unit uGosAVLBinaryTree;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses system.classes;

type

  {class defintion----------------}
  TGosBaseAVLBinaryTreeNode = class;
  TGosBaseAVLBinaryTree = class;

  {iterate function--------------------------------------------------}
  TGosAVLBinaryTreeIterateFunc = procedure(aTree: TGosBaseAVLBinaryTree;
                                          aNode: TGosBaseAVLBinaryTreeNode;
                                          aExtData: Pointer;
                                          Var aContinue: Boolean);

  {TALBaseAVLBinaryTreeNode---------------}
  TGosBaseAVLBinaryTreeNode = class(Tobject)
  Private
  Protected
    ChildNodes: array[Boolean] of TGosBaseAVLBinaryTreeNode;
    Bal: -1..1;
    Procedure SaveToStream(Astream: Tstream); Virtual;
    Procedure LoadFromStream(Astream: Tstream); Virtual;
  Public
    Constructor Create; virtual;
  end;

  {TALBaseAVLBinaryTree---------------}
  TGosBaseAVLBinaryTree = class(TObject)
  private
    FHead: TGosBaseAVLBinaryTreeNode;
    FNodeCount: Integer;
  protected
    procedure FreeNodeObj(aNode: TGosBaseAVLBinaryTreeNode); virtual;
    Function  CompareNode(IdVal: pointer; ANode: TGosBaseAVLBinaryTreeNode): Integer; overload; Virtual; Abstract; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function  CompareNode(aNode1, ANode2: TGosBaseAVLBinaryTreeNode): Integer; overload; Virtual; Abstract; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function  CreateNode: TGosBaseAVLBinaryTreeNode; virtual; abstract;
    procedure InternalIterate(Action: TGosAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer); virtual;
    function  InternalAddNode(aNode: TGosBaseAVLBinaryTreeNode): Boolean; virtual;
    Function  InternalExtractNode(IdVal: Pointer): TGosBaseAVLBinaryTreeNode; virtual;
    Function  InternalDeleteNode(IdVal: Pointer): Boolean; virtual;
    Procedure InternalClear; Virtual;
    Function  InternalGetHead: TGosBaseAVLBinaryTreeNode; virtual;
    Procedure InternalSaveToStream(Astream: Tstream); Virtual;
    Procedure InternalLoadFromStream(Astream: Tstream); Virtual;
    function  InternalFindNode(idVal: pointer): TGosBaseAVLBinaryTreeNode; virtual;
    function  InternalFirst: TGosBaseAVLBinaryTreeNode; virtual; {Return the smallest-value node in the tree}
    function  InternalLast: TGosBaseAVLBinaryTreeNode; virtual; {Return the largest-value node in the tree}
    function  InternalNext(aNode: TGosBaseAVLBinaryTreeNode): TGosBaseAVLBinaryTreeNode; virtual; {Return the next node whose value is larger than aNode}
    function  InternalPrev(aNode: TGosBaseAVLBinaryTreeNode): TGosBaseAVLBinaryTreeNode; virtual; {Return the largest node whose value is smaller than aNode}
    Function  InternalGetNodeCount: integer; virtual;
  public
    Constructor Create; virtual;
    Destructor  Destroy; Override;
    procedure   Iterate(Action: TGosAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer); virtual;
    function    AddNode(aNode: TGosBaseAVLBinaryTreeNode): Boolean; virtual;
    Function    ExtractNode(IdVal: Pointer): TGosBaseAVLBinaryTreeNode; virtual;
    Function    DeleteNode(IdVal: Pointer): Boolean; virtual;
    Procedure   Clear; Virtual;
    Function    Head: TGosBaseAVLBinaryTreeNode; virtual;
    Procedure   SaveToStream(Astream: Tstream); Virtual;
    Procedure   LoadFromStream(Astream: Tstream); Virtual;
    Procedure   SaveToFile(const AFilename: AnsiString); Virtual;
    Procedure   LoadFromFile(const AFilename: AnsiString); Virtual;
    function    FindNode(idVal: pointer): TGosBaseAVLBinaryTreeNode; virtual;
    function    First: TGosBaseAVLBinaryTreeNode; virtual; {Return the smallest-value node in the tree}
    function    Last: TGosBaseAVLBinaryTreeNode; virtual; {Return the largest-value node in the tree}
    function    Next(aNode: TGosBaseAVLBinaryTreeNode): TGosBaseAVLBinaryTreeNode; virtual; {Return the next node whose value is larger than aNode}
    function    Prev(aNode: TGosBaseAVLBinaryTreeNode): TGosBaseAVLBinaryTreeNode; virtual; {Return the largest node whose value is smaller than aNode}
    Function    NodeCount: integer; virtual;
  end;

  {TALIntegerKeyAVLBinaryTreeNode--------------------------------}
  TGosIntegerKeyAVLBinaryTreeNode = class(TGosBaseAVLBinaryTreeNode)
  Private
  Protected
    Procedure SaveToStream(Astream: Tstream); override;
    Procedure LoadFromStream(Astream: Tstream); override;
  Public
    ID: Integer;
    Constructor Create; Override;
  end;

  {TALIntegerKeyAVLBinaryTree-------------------------}
  TGosIntegerKeyAVLBinaryTree = class(TGosBaseAVLBinaryTree)
  private
  protected
    Function CompareNode(IdVal: pointer; ANode: TGosBaseAVLBinaryTreeNode): Integer; override; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function CompareNode(aNode1, ANode2: TGosBaseAVLBinaryTreeNode): Integer; override; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function CreateNode: TGosBaseAVLBinaryTreeNode; override;
  public
    function AddNode(aNode: TGosIntegerKeyAVLBinaryTreeNode): Boolean; reintroduce; virtual;
    Function ExtractNode(IdVal: Integer): TGosIntegerKeyAVLBinaryTreeNode; reintroduce; virtual;
    function DeleteNode(idVal: Integer): boolean; reintroduce; virtual;
    Function Head: TGosIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function FindNode(idVal: Integer): TGosIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function First: TGosIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the smallest-value node in the tree}
    function Last: TGosIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest-value node in the tree}
    function Next(aNode: TGosIntegerKeyAVLBinaryTreeNode): TGosIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the next node whose value is larger than aNode}
    function Prev(aNode: TGosIntegerKeyAVLBinaryTreeNode): TGosIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest node whose value is smaller than aNode}
  end;

  {TALCardinalKeyAVLBinaryTreeNode--------------------------------}
  TGosCardinalKeyAVLBinaryTreeNode = class(TGosBaseAVLBinaryTreeNode)
  Private
  Protected
    Procedure SaveToStream(Astream: Tstream); override;
    Procedure LoadFromStream(Astream: Tstream); override;
  Public
    ID: Cardinal;
    Constructor Create; Override;
  end;

  {TALCardinalKeyAVLBinaryTree-------------------------}
  TGosCardinalKeyAVLBinaryTree = class(TGosBaseAVLBinaryTree)
  private
  protected
    Function CompareNode(IdVal: pointer; ANode: TGosBaseAVLBinaryTreeNode): Integer; override; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function CompareNode(aNode1, ANode2: TGosBaseAVLBinaryTreeNode): Integer; override; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function CreateNode: TGosBaseAVLBinaryTreeNode; override;
  public
    function AddNode(aNode: TGosCardinalKeyAVLBinaryTreeNode): Boolean; reintroduce; virtual;
    Function ExtractNode(IdVal: Cardinal): TGosCardinalKeyAVLBinaryTreeNode; reintroduce; virtual;
    function DeleteNode(idVal: Cardinal): boolean; reintroduce; virtual;
    Function Head: TGosCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function FindNode(idVal: Cardinal): TGosCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function First: TGosCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the smallest-value node in the tree}
    function Last: TGosCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest-value node in the tree}
    function Next(aNode: TGosCardinalKeyAVLBinaryTreeNode): TGosCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the next node whose value is larger than aNode}
    function Prev(aNode: TGosCardinalKeyAVLBinaryTreeNode): TGosCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest node whose value is smaller than aNode}
  end;

  {TALInt64KeyAVLBinaryTreeNode--------------------------------}
  TGosInt64KeyAVLBinaryTreeNode = class(TGosBaseAVLBinaryTreeNode)
  Private
  Protected
    Procedure SaveToStream(Astream: Tstream); override;
    Procedure LoadFromStream(Astream: Tstream); override;
  Public
    ID: Int64;
    Constructor Create; Override;
  end;

  {TALInt64KeyAVLBinaryTree----------------------------}
  TGosInt64KeyAVLBinaryTree = class(TGosBaseAVLBinaryTree)
  private
  protected
    Function CompareNode(IdVal: pointer; ANode: TGosBaseAVLBinaryTreeNode): Integer; override; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function CompareNode(aNode1, ANode2: TGosBaseAVLBinaryTreeNode): Integer; override; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function CreateNode: TGosBaseAVLBinaryTreeNode; override;
  public
    function AddNode(aNode: TGosInt64KeyAVLBinaryTreeNode): Boolean; reintroduce; virtual;
    Function ExtractNode(IdVal: Int64): TGosInt64KeyAVLBinaryTreeNode; reintroduce; virtual;
    function DeleteNode(idVal: Int64): boolean; reintroduce; virtual;
    Function Head: TGosInt64KeyAVLBinaryTreeNode; Reintroduce; virtual;
    function FindNode(idVal: int64): TGosInt64KeyAVLBinaryTreeNode; Reintroduce; virtual;
    function First: TGosInt64KeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the smallest-value node in the tree}
    function Last: TGosInt64KeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest-value node in the tree}
    function Next(aNode: TGosInt64KeyAVLBinaryTreeNode): TGosInt64KeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the next node whose value is larger than aNode}
    function Prev(aNode: TGosInt64KeyAVLBinaryTreeNode): TGosInt64KeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest node whose value is smaller than aNode}
  end;

  {TALStringKeyAVLBinaryTreeNode--------------------------------}
  TGosStringKeyAVLBinaryTreeNode = class(TGosBaseAVLBinaryTreeNode)
  Private
  Protected
    Procedure SaveToStream(Astream: Tstream); override;
    Procedure LoadFromStream(Astream: Tstream); override;
  Public
    ID: AnsiString;
    Constructor Create; Override;
  end;

  {TALStringKeyAVLBinaryTreeCompareKeyFunct----------------------------------------------------------}
  TGosStringKeyAVLBinaryTreeCompareKeyFunct = function (const aKey1, aKey2: AnsiString): Integer of object;

  {TALStringKeyAVLBinaryTree----------------------------}
  TGosStringKeyAVLBinaryTree = class(TGosBaseAVLBinaryTree)
  private
    FCaseSensitive: Boolean;
    FcompareKeyFunct: TGosStringKeyAVLBinaryTreeCompareKeyFunct;
    procedure SetcaseSensitive(const Value: Boolean);
  protected
    Function CompareKeyCaseSensitive(Const aKey1, aKey2: AnsiString): Integer; {compares akey1 and akey2 and returns 0 if they are equal. If akey1 is greater than akey2, returns an integer greater than 0. If akey1 is less than akey2, returns an integer less than 0.}
    Function CompareKeyCaseInSensitive(Const aKey1, aKey2: AnsiString): Integer; {compares akey1 and akey2 and returns 0 if they are equal. If akey1 is greater than akey2, returns an integer greater than 0. If akey1 is less than akey2, returns an integer less than 0.}
    Function CompareNode(IdVal: pointer; ANode: TGosBaseAVLBinaryTreeNode): Integer; override; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function CompareNode(aNode1, ANode2: TGosBaseAVLBinaryTreeNode): Integer; override; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function CreateNode: TGosBaseAVLBinaryTreeNode; override;
  public
    Constructor Create; override;
    function    AddNode(aNode: TGosStringKeyAVLBinaryTreeNode): Boolean; reintroduce; virtual;
    function    ExtractNode(const IdVal: AnsiString): TGosStringKeyAVLBinaryTreeNode; reintroduce; virtual;
    function    DeleteNode(const idVal: AnsiString): boolean; reintroduce; virtual;
    Function    Head: TGosStringKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function    FindNode(const idVal: AnsiString): TGosStringKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function    First: TGosStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the smallest-value node in the tree}
    function    Last: TGosStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest-value node in the tree}
    function    Next(aNode: TGosStringKeyAVLBinaryTreeNode): TGosStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the next node whose value is larger than aNode}
    function    Prev(aNode: TGosStringKeyAVLBinaryTreeNode): TGosStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest node whose value is smaller than aNode}
    Property    CaseSensitive: Boolean read FCaseSensitive write SetcaseSensitive default True;
  end;

implementation

uses System.Contnrs,
     System.sysUtils,
     uGosString;

{Following stack declarations are used to avoid recursion in all tree
 routines. Because the tree is AVL-balanced, a stack size of 40
 allows at least 2**32 elements in the tree without overflowing the
 stack.}

const
  cGosAVLBinaryTree_StackSize = 40;
  cGosAVLBinaryTree_LeftChild = False;
  cGosAVLBinaryTree_RightChild = True;

type
  TGosAVLBinaryTree_StackNode = record
    Node : TGosBaseAVLBinaryTreeNode;
    Comparison : Integer;
  end;
  TGosAVLBinaryTree_StackArray = array[1..cGosAVLBinaryTree_StackSize] of TGosAVLBinaryTree_StackNode;


{*************************************************}
function AlAVLBinaryTree_Sign(I: Integer): Integer;
begin
  if I < 0 then Result := -1
  else if I > 0 then Result := +1
  else Result := 0;
end;

{***********************************************************************}
procedure AlAVLBinaryTree_DelBalance(var aNode: TGosBaseAVLBinaryTreeNode;
                                     var SubTreeDec: Boolean;
                                     CmpRes: Integer);
var N1, N2: TGosBaseAVLBinaryTreeNode;
    B1, B2: Integer;
    LR: Boolean;
begin
  CmpRes := AlAVLBinaryTree_Sign(CmpRes);
  if aNode.Bal = CmpRes then aNode.Bal := 0
  else if aNode.Bal = 0 then begin
    aNode.Bal := -CmpRes;
    SubTreeDec := False;
  end
  else begin
    LR := (CmpRes < 0);
    N1 := aNode.ChildNodes[LR];
    B1 := N1.Bal;
    if (B1 = 0) or (B1 = -CmpRes) then begin
      {Single RR or LL rotation}
      aNode.ChildNodes[LR] := N1.ChildNodes[not LR];
      N1.ChildNodes[not LR] := aNode;
      if B1 = 0 then begin
        aNode.Bal := -CmpRes;
        N1.Bal := CmpRes;
        SubTreeDec := False;
      end
      else begin
        aNode.Bal := 0;
        N1.Bal := 0;
      end;
      aNode := N1;
    end
    else begin
      {Double RL or LR rotation}
      N2 := N1.ChildNodes[not LR];
      B2 := N2.Bal;
      N1.ChildNodes[not LR] := N2.ChildNodes[LR];
      N2.ChildNodes[LR] := N1;
      aNode.ChildNodes[LR] := N2.ChildNodes[not LR];
      N2.ChildNodes[not LR] := aNode;
      if B2 = -CmpRes then aNode.Bal := CmpRes
      else aNode.Bal := 0;
      if B2 = CmpRes then N1.Bal := -CmpRes
      else N1.Bal := 0;
      aNode := N2;
      N2.Bal := 0;
    end;
  end;
end;

{***********************************************************************}
procedure GosAVLBinaryTree_InsBalance(var aNode: TGosBaseAVLBinaryTreeNode;
                                     var SubTreeInc: Boolean;
                                     CmpRes: Integer);
var N1: TGosBaseAVLBinaryTreeNode;
    N2: TGosBaseAVLBinaryTreeNode;
    LR: Boolean;
begin
  CmpRes := AlAVLBinaryTree_Sign(CmpRes);
  if aNode.Bal = -CmpRes then begin
    aNode.Bal := 0;
    SubTreeInc := False;
  end
  else if aNode.Bal = 0 then aNode.Bal := CmpRes
  else begin
    LR := (CmpRes > 0);
    N1 := aNode.ChildNodes[LR];
    if N1.Bal = CmpRes then begin
      aNode.ChildNodes[LR] := N1.ChildNodes[not LR];
      N1.ChildNodes[not LR] := aNode;
      aNode.Bal := 0;
      aNode := N1;
    end
    else begin
      N2 := N1.ChildNodes[not LR];
      N1.ChildNodes[not LR] := N2.ChildNodes[LR];
      N2.ChildNodes[LR] := N1;
      aNode.ChildNodes[LR] := N2.ChildNodes[not LR];
      N2.ChildNodes[not LR] := aNode;
      if N2.Bal = CmpRes then aNode.Bal := -CmpRes
      else aNode.Bal := 0;
      if N2.Bal = -CmpRes then N1.Bal := CmpRes
      else N1.Bal := 0;
      aNode := N2;
    end;
    aNode.Bal := 0;
    SubTreeInc := False;
  end;
end;

{***************************************************************************}
procedure GosAVLBinaryTree_IterateDestroyNodeFunc(aTree: TGosBaseAVLBinaryTree;
                                                 aNode: TGosBaseAVLBinaryTreeNode;
                                                 aExtData: Pointer;
                                                 Var aContinue: Boolean);

begin
  aTree.FreeNodeObj(aNode);
  acontinue := True;
end;

{******************************************}
constructor TGosBaseAVLBinaryTreeNode.Create;
begin
 ChildNodes[cGosAVLBinaryTree_LeftChild] := nil;
 ChildNodes[cGosAVLBinaryTree_RightChild] := nil;
 Bal := 0;
end;

{******************************************************************}
procedure TGosBaseAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
 //virtual
end;

{****************************************************************}
procedure TGosBaseAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
 //virtual
end;

{**************************************}
Constructor TGosBaseAVLBinaryTree.Create;
begin
  FHead := Nil;
  FNodeCount := 0;
  randomize;
  Inherited;
end;

{**************************************}
Destructor TGosBaseAVLBinaryTree.Destroy;
begin
  InternalClear;
  Inherited;
end;

{*********************************************************************************}
procedure TGosBaseAVLBinaryTree.InternalIterate(Action: TGosAVLBinaryTreeIterateFunc;
                                               Up: Boolean;
                                               ExtData: Pointer);
var N1: TGosBaseAVLBinaryTreeNode;
    N2: TGosBaseAVLBinaryTreeNode;
    StackPos: Integer;
    Stack: TGosAVLBinaryTree_StackArray;
    Continue: Boolean;
begin
  Continue := True;
  StackPos := 0;
  N1 := Fhead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.ChildNodes[not Up];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    N2 := N1;
    N1 := N1.ChildNodes[Up];

    Action(Self, N2, ExtData, Continue);
    if not continue then Exit;
  until False;
end;


{**************************************************************************************}
function TGosBaseAVLBinaryTree.InternalAddNode(aNode: TGosBaseAVLBinaryTreeNode): Boolean;
var N1: TGosBaseAVLBinaryTreeNode;
    CmpRes: Integer;
    StackPos: Integer;
    Stack: TGosAVLBinaryTree_StackArray;
    SubTreeInc: Boolean;
begin
  {exit if node is nil}
  if not Assigned(aNode) then begin
    Result := False;
    Exit;
  end;

  {Handle first node}
  N1 := FHead;
  if not Assigned(N1) then begin
    Fhead := aNode;
    Inc(FNodeCount);
    result := True;
    Exit;
  end;

  {Find where new node should fit in tree}
  StackPos := 0;
  CmpRes := 0;
  while Assigned(N1) do begin

    {compare node}
    CmpRes := CompareNode(aNode, N1);

    {node already exist, so exit}
    if CmpRes = 0 then begin
      Result := False;
      Exit;
    end;

    {Build the stack}
    Inc(StackPos);
    with Stack[StackPos] do begin
      Node := N1;
      Comparison := CmpRes;
    end;

    {continue the loop}
    N1 := N1.ChildNodes[CmpRes > 0];

  end;

  {Insert new node}
  Stack[StackPos].Node.ChildNodes[CmpRes > 0] := aNode;
  Inc(FNodeCount);
  result := True;

  {Unwind the stack and rebalance}
  SubTreeInc := True;
  while (StackPos > 0) and SubTreeInc do begin
    if StackPos = 1 then GosAVLBinaryTree_InsBalance(Fhead, SubTreeInc, Stack[1].Comparison)
    else with Stack[StackPos-1] do
      GosAVLBinaryTree_InsBalance(Node.ChildNodes[Comparison > 0], SubTreeInc, Stack[StackPos].Comparison);
    dec(StackPos);
  end;
end;

{*******************************************}
procedure TGosBaseAVLBinaryTree.InternalClear;
begin
  InternalIterate(GosAVLBinaryTree_IterateDestroyNodeFunc,
                  True,
                  nil);
  FHead := nil;
  FNodeCount := 0;
end;

{**********************************************************************}
function TGosBaseAVLBinaryTree.InternalGetHead: TGosBaseAVLBinaryTreeNode;
begin
  Result := Fhead;
end;

{**************************************************************************}
procedure TGosBaseAVLBinaryTree.FreeNodeObj(aNode: TGosBaseAVLBinaryTreeNode);
begin
  aNode.Free;
end;

{******************************************************************************************}
function TGosBaseAVLBinaryTree.InternalExtractNode(IdVal: Pointer): TGosBaseAVLBinaryTreeNode;
var N1: TGosBaseAVLBinaryTreeNode;
    N2: TGosBaseAVLBinaryTreeNode;
    TmpNode: TGosBaseAVLBinaryTreeNode;
    CmpRes: Integer;
    Found: Boolean;
    SubTreeDec: Boolean;
    StackPos: Integer;
    StackParentPos: integer;
    Stack: TGosAVLBinaryTree_StackArray;
begin
  {exit if head is nil}
  N1 := Fhead;
  if not Assigned(N1) then begin
    result := nil;
    Exit;
  end;

  {Find node to delete and stack the nodes to reach it}
  Found := False;
  StackPos := 0;
  while not Found do begin

    {compare node}
    CmpRes := CompareNode(IdVal, N1);
    Inc(StackPos);

    {Found node}
    if CmpRes = 0 then begin
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := -1;
      end;
      Found := True;
    end

    {not found yet, continue the search}
    else begin
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := CmpRes;
      end;
      N1 := N1.ChildNodes[CmpRes > 0];

      {Node not found, then exit}
      if not Assigned(N1) then begin
        Result := nil;
        Exit;
      end;
    end;

  end;

  {save the position of the parent of the node to delete in the stack}
  StackParentPos := StackPos - 1;

  {Delete the node found}
  N2 := N1;
  if (not Assigned(N2.ChildNodes[cGosAVLBinaryTree_RightChild])) or (not Assigned(N2.ChildNodes[cGosAVLBinaryTree_LeftChild])) then begin
    {Node has at most one branch}
    Dec(StackPos);
    N1 := N2.ChildNodes[Assigned(N2.ChildNodes[cGosAVLBinaryTree_RightChild])];
    if StackPos = 0 then Fhead := N1
    else with Stack[StackPos] do
      Node.ChildNodes[Comparison > 0] := N1;
  end
  else begin
    {Node has two branches; stack nodes to reach one with no right child}
    N1 := N2.ChildNodes[cGosAVLBinaryTree_LeftChild];
    while Assigned(N1.ChildNodes[cGosAVLBinaryTree_RightChild]) do begin
      Inc(StackPos);
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := 1;
      end;
      N1 := N1.ChildNodes[cGosAVLBinaryTree_RightChild];
    end;

    {Swap the node to delete with the terminal node}
    N1.Bal := N2.Bal;
    If StackParentPos = 0 then Fhead := N1
    else with Stack[StackParentPos] do
      Node.ChildNodes[Comparison > 0] := N1;

    with Stack[StackParentPos+1] do
      Node := N1;

    tmpnode := N1.ChildNodes[cGosAVLBinaryTree_LeftChild];
    N1.ChildNodes[cGosAVLBinaryTree_RightChild] := N2.ChildNodes[cGosAVLBinaryTree_RightChild];
    N1.ChildNodes[cGosAVLBinaryTree_LeftChild] := N2.ChildNodes[cGosAVLBinaryTree_LeftChild];

    with Stack[StackPos] do
      Node.ChildNodes[Comparison > 0] := tmpnode;
  end;

  {return the deleted node}
  result := N2;
  Dec(FNodeCount);

  {Unwind the stack and rebalance}
  SubTreeDec := True;
  while (StackPos > 0) and SubTreeDec do begin
    if StackPos = 1 then AlAVLBinaryTree_DelBalance(Fhead, SubTreeDec, Stack[1].Comparison)
    else with Stack[StackPos-1] do
      AlAVLBinaryTree_DelBalance(Node.ChildNodes[Comparison > 0], SubTreeDec, Stack[StackPos].Comparison);
    dec(StackPos);
  end;
end;

{************************************************************************}
function TGosBaseAVLBinaryTree.InternalDeleteNode(IdVal: Pointer): Boolean;
var N1: TGosBaseAVLBinaryTreeNode;
begin
  N1 := InternalExtractNode(IdVal);
  if assigned(N1) then begin
    result := True;
    FreeNodeObj(N1);
  end
  else result := False;
end;

{**********************************************************************}
procedure TGosBaseAVLBinaryTree.InternalLoadFromStream(Astream: Tstream);
Var K:Boolean;
    LstNode: TObjectStack;
    aParentNode, aNode: TGosBaseAVLBinaryTreeNode;
begin
  {clear the binary tree}
  InternalClear;

  {create the TobjectStack}
  LstNode := TObjectStack.Create;
  Try

    {load the Head}
    AStream.Readbuffer(k, SizeOf(k));
    if k then begin
      FHead := CreateNode;
      AStream.ReadBuffer(FHead.Bal, SizeOf(FHead.Bal));
      FHead.LoadFromStream(aStream);
      inc(FnodeCount);
      FHead.childNodes[cGosAVLBinaryTree_RightChild] := FHead; //a flag
      FHead.childNodes[cGosAVLBinaryTree_LeftChild] := FHead;  //a flag
      {continue the loop with the leftchild and rightChild}
      LstNode.Push(FHead); //rightChild
      LstNode.Push(FHead); //leftChild
    end;

    {start the loop (if neccessary)}
    While LstNode.Count > 0 do begin

      {extract the parent node of the node where we will work on}
      aParentNode := TGosBaseAVLBinaryTreeNode(LstNode.Pop);

      {load the data}
      AStream.ReadBuffer(k, SizeOf(k));
      if k then begin

        {find the good child node where we will work on}
        If aParentNode.childNodes[cGosAVLBinaryTree_LeftChild] = aParentNode then begin
          aParentNode.childNodes[cGosAVLBinaryTree_LeftChild] := CreateNode;
          aNode := aParentNode.childNodes[cGosAVLBinaryTree_LeftChild];
        end
        else begin
          aParentNode.childNodes[cGosAVLBinaryTree_RightChild] := CreateNode;
          aNode := aParentNode.childNodes[cGosAVLBinaryTree_RightChild];
        end;

        AStream.ReadBuffer(ANode.Bal, SizeOf(ANode.Bal));
        ANode.LoadFromStream(aStream);
        inc(FnodeCount);
        aNode.childNodes[cGosAVLBinaryTree_RightChild] := aNode; //a flag
        aNode.childNodes[cGosAVLBinaryTree_LeftChild] := aNode;  //a flag
        {continue the loop with the leftchild and rightChild}
        LstNode.Push(aNode); //rightChild
        LstNode.Push(aNode); //leftChild
      end
      else begin
        {find the good child node where we will work on}
        If aParentNode.childNodes[cGosAVLBinaryTree_LeftChild] = aParentNode then aParentNode.childNodes[cGosAVLBinaryTree_LeftChild] := nil
        else aParentNode.childNodes[cGosAVLBinaryTree_RightChild] := nil;
      end

    end;

  finally
    LstNode.free;
  end;
end;

{********************************************************************}
procedure TGosBaseAVLBinaryTree.InternalSaveToStream(Astream: Tstream);
Var K:Boolean;
    LstNode: TObjectStack;
    aNode: TGosBaseAVLBinaryTreeNode;
begin
  {create the TobjectStack}
  LstNode := TObjectStack.Create;
  Try

    {push the head in the TobjectStack}
    LstNode.Push(FHead);

    {start the loop}
    While LstNode.Count > 0 do begin
      aNode := TGosBaseAVLBinaryTreeNode(LstNode.Pop);
      If assigned(aNode) then begin
        {write that the node exist}
        K := True;
        AStream.WriteBuffer(k, SizeOf(k));
        {write the balance}
        AStream.WriteBuffer(aNode.bal, SizeOf(aNode.bal));
        {write the data}
        Anode.SaveToStream(astream);
        {continue the loop with the leftchild and rightChild}
        LstNode.Push(aNode.childNodes[cGosAVLBinaryTree_RightChild]);
        LstNode.Push(aNode.childNodes[cGosAVLBinaryTree_LeftChild]);
      end
      else begin
        {write that the node doesn't exist}
        K := False;
        AStream.WriteBuffer(k, SizeOf(k));
      end;
    end;

  finally
    LstNode.free;
  end;
end;

{***********************************************************************}
procedure TGosBaseAVLBinaryTree.LoadFromFile(const AFilename: AnsiString);
var aStream: TStream;
begin
  aStream := TFileStream.Create(String(aFileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(aStream);
  finally
    aStream.Free;
  end;
end;

{*********************************************************************}
procedure TGosBaseAVLBinaryTree.SaveToFile(const AFilename: AnsiString);
var aStream: TStream;
begin
  aStream := TFileStream.Create(String(aFileName), fmCreate);
  try
    SaveToStream(aStream);
  finally
    aStream.Free;
  end;
end;

{***************************************************************************************}
function TGosBaseAVLBinaryTree.InternalFindNode(idVal: pointer): TGosBaseAVLBinaryTreeNode;
var N1: TGosBaseAVLBinaryTreeNode;
    CmpRes: Integer;
begin
  N1 := FHead;
  while Assigned(N1) do begin
    CmpRes := CompareNode(IdVal, N1);
    if CmpRes = 0 then begin
      Result := N1;
      Exit;
    end
    else N1 := N1.ChildNodes[CmpRes > 0];
  end;

  Result := nil;
end;

{********************************************************************}
function TGosBaseAVLBinaryTree.InternalFirst: TGosBaseAVLBinaryTreeNode;
begin
  if FNodeCount = 0 then Result := nil
  else begin
    Result := Fhead;
    while Assigned(Result.ChildNodes[cGosAVLBinaryTree_LeftChild]) do
      Result := Result.ChildNodes[cGosAVLBinaryTree_LeftChild];
  end;
end;

{*******************************************************************}
function TGosBaseAVLBinaryTree.InternalLast: TGosBaseAVLBinaryTreeNode;
begin
  if FNodeCount = 0 then Result := nil
  else begin
    Result := FHead;
    while Assigned(Result.ChildNodes[cGosAVLBinaryTree_RightChild]) do
      Result := Result.ChildNodes[cGosAVLBinaryTree_RightChild];
  end;
end;

{****************************************************************************************************}
function TGosBaseAVLBinaryTree.InternalNext(aNode: TGosBaseAVLBinaryTreeNode): TGosBaseAVLBinaryTreeNode;
var Found: Word;
    N1: TGosBaseAVLBinaryTreeNode;
    StackPos: Integer;
    Stack: TGosAVLBinaryTree_StackArray;
begin
  Result := nil;
  Found := 0;
  StackPos := 0;
  N1 := FHead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.ChildNodes[cGosAVLBinaryTree_LeftChild];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    if Found = 1 then begin
      Result := N1;
      Exit;
    end;
    if N1 = aNode then Inc(Found);
    N1 := N1.ChildNodes[cGosAVLBinaryTree_RightChild];
  until False;
end;

{****************************************************************************************************}
function TGosBaseAVLBinaryTree.InternalPrev(aNode: TGosBaseAVLBinaryTreeNode): TGosBaseAVLBinaryTreeNode;
var Found: Word;
    N1: TGosBaseAVLBinaryTreeNode;
    StackPos: Integer;
    Stack: TGosAVLBinaryTree_StackArray;
begin
  Result := nil;
  Found := 0;
  StackPos := 0;
  N1 := FHead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.ChildNodes[cGosAVLBinaryTree_RightChild];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    if Found = 1 then begin
      Result := N1;
      Exit;
    end;
    if N1 = aNode then
      Inc(Found);
    N1 := N1.ChildNodes[cGosAVLBinaryTree_LeftChild];
  until False;
end;

{**********************************************************}
function TGosBaseAVLBinaryTree.InternalGetNodeCount: integer;
begin
  Result := FnodeCount;
end;

{******************************************************************************}
function TGosBaseAVLBinaryTree.AddNode(aNode: TGosBaseAVLBinaryTreeNode): Boolean;
begin
  Result := InternalAddNode(aNode);
end;

{***********************************}
procedure TGosBaseAVLBinaryTree.Clear;
begin
  InternalClear;
end;

{**********************************************************************************}
function TGosBaseAVLBinaryTree.ExtractNode(IdVal: Pointer): TGosBaseAVLBinaryTreeNode;
begin
  Result := InternalExtractNode(IdVal);
end;

{****************************************************************}
function TGosBaseAVLBinaryTree.DeleteNode(IdVal: Pointer): Boolean;
begin
  Result := InternalDeleteNode(IdVal);
end;

{*******************************************************************************}
function TGosBaseAVLBinaryTree.FindNode(idVal: pointer): TGosBaseAVLBinaryTreeNode;
begin
  Result := InternalFindNode(idVal);
end;

{************************************************************}
function TGosBaseAVLBinaryTree.First: TGosBaseAVLBinaryTreeNode;
begin
  result := InternalFirst;
end;

{***********************************************************}
function TGosBaseAVLBinaryTree.Head: TGosBaseAVLBinaryTreeNode;
begin
  Result := InternalGetHead;
end;

{*********************************************************************************************************}
procedure TGosBaseAVLBinaryTree.Iterate(Action: TGosAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer);
begin
  InternalIterate(Action, Up, ExtData);
end;

{***********************************************************}
function TGosBaseAVLBinaryTree.Last: TGosBaseAVLBinaryTreeNode;
begin
  Result := InternalLast;
end;

{********************************************************************************************}
function TGosBaseAVLBinaryTree.Next(aNode: TGosBaseAVLBinaryTreeNode): TGosBaseAVLBinaryTreeNode;
begin
  Result := InternalNext(aNode);
end;

{********************************************************************************************}
function TGosBaseAVLBinaryTree.Prev(aNode: TGosBaseAVLBinaryTreeNode): TGosBaseAVLBinaryTreeNode;
begin
  Result := InternalPrev(aNode);
end;

{************************************************************}
procedure TGosBaseAVLBinaryTree.SaveToStream(Astream: Tstream);
begin
  InternalSaveToStream(Astream);
end;

{**************************************************************}
procedure TGosBaseAVLBinaryTree.LoadFromStream(Astream: Tstream);
begin
  InternalLoadFromStream(Astream);
end;

{***********************************************}
function TGosBaseAVLBinaryTree.NodeCount: integer;
begin
  Result := InternalGetNodeCount;
end;

{************************************************}
constructor TGosIntegerKeyAVLBinaryTreeNode.Create;
begin
  Inherited;
  ID := 0;
end;

{************************************************************************}
procedure TGosIntegerKeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
  AStream.ReadBuffer(ID, SizeOf(ID));
end;

{**********************************************************************}
procedure TGosIntegerKeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
  AStream.writeBuffer(ID, SizeOf(ID));
end;

{******************************************************************************************}
function TGosIntegerKeyAVLBinaryTree.AddNode(aNode: TGosIntegerKeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{********************************************************************************************************}
function TGosIntegerKeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TGosBaseAVLBinaryTreeNode): Integer;
Var aIntKey: Integer;
begin
  aIntKey := PInteger(IdVal)^;
  IF aIntKey = TGosIntegerKeyAVLBinaryTreeNode(aNode).ID then result := 0
  else IF aIntKey > TGosIntegerKeyAVLBinaryTreeNode(aNode).ID then result := 1
  else result := -1;
end;

{*************************************************************************************************}
function TGosIntegerKeyAVLBinaryTree.CompareNode(aNode1, ANode2: TGosBaseAVLBinaryTreeNode): Integer;
begin
  IF TGosIntegerKeyAVLBinaryTreeNode(aNode1).ID = TGosIntegerKeyAVLBinaryTreeNode(aNode2).ID then result := 0
  else IF TGosIntegerKeyAVLBinaryTreeNode(aNode1).ID > TGosIntegerKeyAVLBinaryTreeNode(aNode2).ID then result := 1
  else result := -1;
end;

{***********************************************************************}
function TGosIntegerKeyAVLBinaryTree.CreateNode: TGosBaseAVLBinaryTreeNode;
begin
  Result := TGosIntegerKeyAVLBinaryTreeNode.Create;
end;

{**********************************************************************************************}
function TGosIntegerKeyAVLBinaryTree.ExtractNode(IdVal: Integer): TGosIntegerKeyAVLBinaryTreeNode;
begin
  result := TGosIntegerKeyAVLBinaryTreeNode(inherited ExtractNode(@idVal));
end;

{**********************************************************************}
Function TGosIntegerKeyAVLBinaryTree.DeleteNode(idVal: Integer): boolean;
begin
  result := inherited DeleteNode(@idVal);
end;

{*******************************************************************************************}
function TGosIntegerKeyAVLBinaryTree.FindNode(idVal: Integer): TGosIntegerKeyAVLBinaryTreeNode;
begin
  result := TGosIntegerKeyAVLBinaryTreeNode(inherited FindNode(@idVal));
end;

{************************************************************************}
function TGosIntegerKeyAVLBinaryTree.First: TGosIntegerKeyAVLBinaryTreeNode;
begin
  Result := TGosIntegerKeyAVLBinaryTreeNode(inherited First);
end;

{***********************************************************************}
function TGosIntegerKeyAVLBinaryTree.Last: TGosIntegerKeyAVLBinaryTreeNode;
begin
  Result := TGosIntegerKeyAVLBinaryTreeNode(inherited Last);
end;

{**************************************************************************************************************}
function TGosIntegerKeyAVLBinaryTree.Next(aNode: TGosIntegerKeyAVLBinaryTreeNode): TGosIntegerKeyAVLBinaryTreeNode;
begin
  Result := TGosIntegerKeyAVLBinaryTreeNode(inherited Next(aNode));
end;

{**************************************************************************************************************}
function TGosIntegerKeyAVLBinaryTree.Prev(aNode: TGosIntegerKeyAVLBinaryTreeNode): TGosIntegerKeyAVLBinaryTreeNode;
begin
  Result := TGosIntegerKeyAVLBinaryTreeNode(inherited Prev(aNode));
end;

{***********************************************************************}
function TGosIntegerKeyAVLBinaryTree.Head: TGosIntegerKeyAVLBinaryTreeNode;
begin
  Result := TGosIntegerKeyAVLBinaryTreeNode(inherited Head);
end;

{*************************************************}
constructor TGosCardinalKeyAVLBinaryTreeNode.Create;
begin
  Inherited;
  ID := 0;
end;

{*************************************************************************}
procedure TGosCardinalKeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
  AStream.ReadBuffer(ID, SizeOf(ID));
end;

{***********************************************************************}
procedure TGosCardinalKeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
  AStream.writeBuffer(ID, SizeOf(ID));
end;

{********************************************************************************************}
function TGosCardinalKeyAVLBinaryTree.AddNode(aNode: TGosCardinalKeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{*********************************************************************************************************}
function TgosCardinalKeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TGosBaseAVLBinaryTreeNode): Integer;
Var aCardKey: Cardinal;
begin
  aCardKey := PCardinal(IdVal)^;
  IF aCardKey = TGosCardinalKeyAVLBinaryTreeNode(aNode).ID then result := 0
  else IF aCardKey > TGosCardinalKeyAVLBinaryTreeNode(aNode).ID then result := 1
  else result := -1;
end;

{**************************************************************************************************}
function TGosCardinalKeyAVLBinaryTree.CompareNode(aNode1, ANode2: TGosBaseAVLBinaryTreeNode): Integer;
begin
  IF TGosCardinalKeyAVLBinaryTreeNode(aNode1).ID = TGosCardinalKeyAVLBinaryTreeNode(aNode2).ID then result := 0
  else IF TGosCardinalKeyAVLBinaryTreeNode(aNode1).ID > TGosCardinalKeyAVLBinaryTreeNode(aNode2).ID then result := 1
  else result := -1;
end;

{************************************************************************}
function TGosCardinalKeyAVLBinaryTree.CreateNode: TGosBaseAVLBinaryTreeNode;
begin
  Result := TGosCardinalKeyAVLBinaryTreeNode.Create;
end;

{*************************************************************************************************}
function TGosCardinalKeyAVLBinaryTree.ExtractNode(IdVal: Cardinal): TGosCardinalKeyAVLBinaryTreeNode;
begin
  result := TGosCardinalKeyAVLBinaryTreeNode(inherited ExtractNode(@idVal));
end;

{************************************************************************}
Function TGosCardinalKeyAVLBinaryTree.DeleteNode(idVal: Cardinal): boolean;
begin
  result := inherited DeleteNode(@idVal);
end;

{**********************************************************************************************}
function TGosCardinalKeyAVLBinaryTree.FindNode(idVal: Cardinal): TGosCardinalKeyAVLBinaryTreeNode;
begin
  result := TGosCardinalKeyAVLBinaryTreeNode(inherited FindNode(@idVal));
end;

{**************************************************************************}
function TGosCardinalKeyAVLBinaryTree.First: TGosCardinalKeyAVLBinaryTreeNode;
begin
  Result := TGosCardinalKeyAVLBinaryTreeNode(inherited First);
end;

{*************************************************************************}
function TGosCardinalKeyAVLBinaryTree.Last: TGosCardinalKeyAVLBinaryTreeNode;
begin
  Result := TGosCardinalKeyAVLBinaryTreeNode(inherited Last);
end;

{*****************************************************************************************************************}
function TGosCardinalKeyAVLBinaryTree.Next(aNode: TGosCardinalKeyAVLBinaryTreeNode): TGosCardinalKeyAVLBinaryTreeNode;
begin
  Result := TGosCardinalKeyAVLBinaryTreeNode(inherited Next(aNode));
end;

{*****************************************************************************************************************}
function TGosCardinalKeyAVLBinaryTree.Prev(aNode: TGosCardinalKeyAVLBinaryTreeNode): TGosCardinalKeyAVLBinaryTreeNode;
begin
  Result := TGosCardinalKeyAVLBinaryTreeNode(inherited Prev(aNode));
end;

{*************************************************************************}
function TGosCardinalKeyAVLBinaryTree.Head: TGosCardinalKeyAVLBinaryTreeNode;
begin
  Result := TGosCardinalKeyAVLBinaryTreeNode(inherited Head);
end;

{**********************************************}
constructor TGosInt64KeyAVLBinaryTreeNode.Create;
begin
  Inherited;
  ID := 0;
end;

{**********************************************************************}
procedure TGosInt64KeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
  AStream.ReadBuffer(ID, SizeOf(ID));
end;

{********************************************************************}
procedure TGosInt64KeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
  AStream.writeBuffer(ID, SizeOf(ID));
end;

{**************************************************************************************}
function TGosInt64KeyAVLBinaryTree.AddNode(aNode: TGosInt64KeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{******************************************************************************************************}
function TGosInt64KeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TGosBaseAVLBinaryTreeNode): Integer;
Var aInt64Key: Int64;
begin
  aInt64Key := Pint64(IdVal)^;
  IF aInt64Key = TGosInt64KeyAVLBinaryTreeNode(aNode).ID then result := 0
  else IF aInt64Key > TGosInt64KeyAVLBinaryTreeNode(aNode).ID then result := 1
  else result := -1;
end;

{***********************************************************************************************}
function TGosInt64KeyAVLBinaryTree.CompareNode(aNode1, ANode2: TGosBaseAVLBinaryTreeNode): Integer;
begin
  IF TGosInt64KeyAVLBinaryTreeNode(aNode1).ID = TGosInt64KeyAVLBinaryTreeNode(aNode2).ID then result := 0
  else IF TGosInt64KeyAVLBinaryTreeNode(aNode1).ID > TGosInt64KeyAVLBinaryTreeNode(aNode2).ID then result := 1
  else result := -1;
end;

{*********************************************************************}
function TGosInt64KeyAVLBinaryTree.CreateNode: TGosBaseAVLBinaryTreeNode;
begin
  Result := TGosInt64KeyAVLBinaryTreeNode.Create;
end;

{****************************************************************************************}
function TGosInt64KeyAVLBinaryTree.ExtractNode(IdVal: Int64): TGosInt64KeyAVLBinaryTreeNode;
begin
  result := TGosInt64KeyAVLBinaryTreeNode(inherited ExtractNode(@idVal));
end;

{******************************************************************}
function TGosInt64KeyAVLBinaryTree.DeleteNode(idVal: Int64): boolean;
begin
  Result := inherited DeleteNode(@idVal);
end;

{*************************************************************************************}
function TGosInt64KeyAVLBinaryTree.FindNode(idVal: Int64): TGosInt64KeyAVLBinaryTreeNode;
begin
  Result := TGosInt64KeyAVLBinaryTreeNode(inherited FindNode(@idVal));
end;

{********************************************************************}
function TGosInt64KeyAVLBinaryTree.First: TGosInt64KeyAVLBinaryTreeNode;
begin
  Result := TGosInt64KeyAVLBinaryTreeNode(inherited First);
end;

{*******************************************************************}
function TGosInt64KeyAVLBinaryTree.Last: TGosInt64KeyAVLBinaryTreeNode;
begin
  Result := TGosInt64KeyAVLBinaryTreeNode(inherited Last);
end;

{********************************************************************************************************}
function TGosInt64KeyAVLBinaryTree.Next(aNode: TGosInt64KeyAVLBinaryTreeNode): TGosInt64KeyAVLBinaryTreeNode;
begin
  Result := TGosInt64KeyAVLBinaryTreeNode(inherited Next(aNode));
end;

{********************************************************************************************************}
function TGosInt64KeyAVLBinaryTree.Prev(aNode: TGosInt64KeyAVLBinaryTreeNode): TGosInt64KeyAVLBinaryTreeNode;
begin
  Result := TGosInt64KeyAVLBinaryTreeNode(inherited Prev(aNode));
end;

{*******************************************************************}
function TGosInt64KeyAVLBinaryTree.Head: TGosInt64KeyAVLBinaryTreeNode;
begin
  Result := TGosInt64KeyAVLBinaryTreeNode(inherited Head);
end;

{***********************************************}
constructor TGosStringKeyAVLBinaryTreeNode.Create;
begin
  inherited;
  ID := '';
end;

{***********************************************************************}
procedure TGosStringKeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
Var K:integer;
begin
  AStream.ReadBuffer(k, SizeOf(k));
  SetLength(ID, k);
  if k > 0 then AStream.ReadBuffer(pointer(ID)^, k);
end;

{*********************************************************************}
procedure TGosStringKeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
Var K:integer;
begin
  K := length(ID);
  AStream.writeBuffer(k, SizeOf(k));
  if k > 0 then AStream.writeBuffer(pointer(ID)^, k);
end;

{*******************************************}
constructor TGosStringKeyAVLBinaryTree.Create;
begin
  inherited;
  FcaseSensitive := True;
  FcompareKeyFunct := CompareKeyCaseSensitive;
end;

{**********************************************************************}
function TGosStringKeyAVLBinaryTree.CreateNode: TGosBaseAVLBinaryTreeNode;
begin
  Result := TGosStringKeyAVLBinaryTreeNode.Create;
end;

{****************************************************************************************}
function TGosStringKeyAVLBinaryTree.AddNode(aNode: TGosStringKeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{*******************************************************************************************************}
function TGosStringKeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TGosBaseAVLBinaryTreeNode): Integer;
begin
  Result := FcompareKeyFunct(PAnsiString(IdVal)^,TGosStringKeyAVLBinaryTreeNode(aNode).ID);
end;

{***********************************************************************************************}
function TGosStringKeyAVLBinaryTree.CompareNode(aNode1,ANode2: TGosBaseAVLBinaryTreeNode): Integer;
begin
  Result := FcompareKeyFunct(TGosStringKeyAVLBinaryTreeNode(aNode1).ID,TGosStringKeyAVLBinaryTreeNode(aNode2).ID);
end;

{*****************************************************************************************************}
function TGosStringKeyAVLBinaryTree.ExtractNode(const IdVal: AnsiString): TGosStringKeyAVLBinaryTreeNode;
begin
  result := TGosStringKeyAVLBinaryTreeNode(inherited ExtractNode(@idVal));
end;

{******************************************************************************}
function TGosStringKeyAVLBinaryTree.DeleteNode(const idVal: AnsiString): boolean;
begin
  result := inherited DeleteNode(@idVal);
end;

{**************************************************************************************************}
function TGosStringKeyAVLBinaryTree.FindNode(const idVal: AnsiString): TGosStringKeyAVLBinaryTreeNode;
begin
  Result := TGosStringKeyAVLBinaryTreeNode(inherited FindNode(@idVal));
end;

{**********************************************************************}
function TGosStringKeyAVLBinaryTree.First: TGosStringKeyAVLBinaryTreeNode;
begin
  Result := TGosStringKeyAVLBinaryTreeNode(inherited First);
end;

{*********************************************************************}
function TGosStringKeyAVLBinaryTree.Last: TGosStringKeyAVLBinaryTreeNode;
begin
  Result := TGosStringKeyAVLBinaryTreeNode(inherited Last);
end;

{***********************************************************************************************************}
function TGosStringKeyAVLBinaryTree.Next(aNode: TGosStringKeyAVLBinaryTreeNode): TGosStringKeyAVLBinaryTreeNode;
begin
  Result := TGosStringKeyAVLBinaryTreeNode(inherited Next(aNode));
end;

{***********************************************************************************************************}
function TGosStringKeyAVLBinaryTree.Prev(aNode: TGosStringKeyAVLBinaryTreeNode): TGosStringKeyAVLBinaryTreeNode;
begin
  Result := TGosStringKeyAVLBinaryTreeNode(inherited Prev(aNode));
end;


{****************************************************************************************************}
function TGosStringKeyAVLBinaryTree.CompareKeyCaseInSensitive(const aKey1, aKey2: AnsiString): Integer;
begin
  Result := ALCompareText(aKey1,aKey2);
end;

{**************************************************************************************************}
function TGosStringKeyAVLBinaryTree.CompareKeyCaseSensitive(const aKey1, aKey2: AnsiString): Integer;
begin
  result := ALCompareStr(aKey1,aKey2);
end;

{*************************************************************************}
procedure TGosStringKeyAVLBinaryTree.SetcaseSensitive(const Value: Boolean);
begin
  If Value <> FCaseSensitive then begin
    FCaseSensitive := Value;
    If FCaseSensitive then FcompareKeyFunct := CompareKeyCaseSensitive
    else FcompareKeyFunct := CompareKeyCaseInSensitive;
  end;
end;

{*********************************************************************}
function TGosStringKeyAVLBinaryTree.Head: TGosStringKeyAVLBinaryTreeNode;
begin
  Result := TGosStringKeyAVLBinaryTreeNode(inherited head);
end;

end.
