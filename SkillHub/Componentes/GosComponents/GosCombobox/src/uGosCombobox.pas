unit uGosCombobox;

interface

uses FMX.Objects, System.Classes, FMX.Graphics, FMX.StdCtrls, FMX.Types,
System.UITypes, System.Generics.Collections, FMX.ListBox, System.Types,
FMX.Edit, FMX.Controls;

type

  [ComponentPlatforms($FFFF)]
  TGosCombobox = class(TRectangle)
  private
    const HeightItem = 22;
    const HeightCombobox = 22;
    const WidthCombobox = 100;
    var
    FListaItens:TObjectList<Tlabel>;
    FCombo:TCustomComboBox;
    FItemIndex: integer;
    FPop:TPopup;
    procedure AbrirLista(Sender: TObject); overload;
    procedure AbrirLista(Sender: TObject; const Point: TPointF); overload;
    procedure FecharLista(Sender:TObject);
    procedure LabelEnter(Sender: TObject);
    procedure Add(const AItem:string);
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
    procedure SetItemIndex(const Value: integer);
    procedure SetItemFontColor(const Value: TalphaColor);
    function GetItemFontColor: TalphaColor;
    procedure ItemOnClick(Sender:TObject);
    procedure ItemOnTap(Sender: TObject; const Point: TPointF);
    procedure SetText(const Value: string);
  protected
    FLabel:TLabel;
    FSpeedButton: TSpeedButton;
    [Week]FLista: TRectangle;
    FSelecionado: TRectangle;
    FCorLista: TalphaColor;
    FCorSelecionado: TalphaColor;
    FItemFontColor: TalphaColor;
    FOnItemClick: TNotifyEvent;
    FOnItemTap: TTapEvent;
    FText: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    function Selected:string;
  published
    property ColorSelect: TalphaColor read FCorSelecionado write FCorSelecionado;
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
    property ItemFontColor: TalphaColor read GetItemFontColor write SetItemFontColor;
    property OnItemClick: TNotifyEvent read FOnItemClick write FOnItemClick;
    property OnItemTap: TTapEvent read FOnItemTap write FOnItemTap;
    property Text: string read FText write SetText;
    property List: TRectangle read FLista;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosCombobox]);
end;

procedure TGosCombobox.Add(const AItem: string);
var
 LLabel:TLabel;
begin

  LLabel:= TLabel.Create(self);
  LLabel.Text:= AItem;
  LLabel.Align:= TAlignLayout.Bottom;
  LLabel.Height:= HeightItem;
  LLabel.OnMouseEnter:= LabelEnter;
  LLabel.HitTest:= true;
  LLabel.Locked:= true;
  LLabel.StyledSettings:= [];
  LLabel.TextSettings.FontColor:= FItemFontColor;
  LLabel.Tag:= FListaItens.Count;

  LLabel.Margins.Left:= 8;
  LLabel.OnClick:= ItemOnClick;
  LLabel.OnTap:= ItemOnTap;

  FLista.AddObject(LLabel);
  FListaItens.Add(LLabel);

  FLista.Height:= FListaItens.Count * HeightItem;
  LLabel.Align:= TAlignLayout.Top;

end;

procedure TGosCombobox.Clear;
begin
  FCombo.Items.Clear;
end;

constructor TGosCombobox.Create(AOwner: TComponent);
begin
  inherited;

  self.Height:= HeightCombobox;
  self.Width:= WidthCombobox;
  self.OnClick:= AbrirLista;
  self.OnTap:= AbrirLista;

  FCorLista:= $32E99F25;
  FCorSelecionado:= $96E99F25;
  FItemFontColor:= $FF060606;
  FItemIndex:= -1;

  FCombo:= TCustomComboBox.Create(self);
  FCombo.Stored:= false;

  //Label
  FLabel:= TLabel.Create(self);
  FLabel.Text:= Text;
  FLabel.Align:= TAlignLayout.client;
  FLabel.Locked:= true;
  FLabel.Margins.Left:= 8;
  FLabel.HitTest:= false;
  FLabel.SetSubComponent(true);
  FLabel.Stored := False;
  FLabel.StyledSettings:= [];
  FLabel.Text:= FText;

  self.AddObject(FLabel);

  //Botão
  FSpeedButton:= TSpeedButton.Create(self);
  FSpeedButton.Locked:= true;
  FSpeedButton.HitTest:= false;
  FSpeedButton.StyleLookup:= 'dropdowneditbutton';
  FSpeedButton.Width:= 20;
  FSpeedButton.Align:= TAlignLayout.Right;
  FSpeedButton.SetSubComponent(true);
  FSpeedButton.Stored := False;

  self.AddObject(FSpeedButton);

  //Caixa da Lista
  FLista:= TRectangle.Create(self);
  FLista.Name:= 'GosListaItens';
  FLista.Fill.Color:= FCorLista;
  FLista.Width:= self.Width;
  FLista.Height:= self.Height;
  FLista.Position.Y:= self.Position.Y + self.Height;
  FLista.Position.X:= self.Position.X;
  FLista.Visible:= false;
  FLista.Stored := False;
  FLista.SetSubComponent(true);

  self.AddObject(FLista);

  //Selecao
  FSelecionado:= TRectangle.Create(self);
  FSelecionado.Stroke.Kind:= TBrushKind.None;
  FSelecionado.Fill.Color:= $96E99F25;
  FSelecionado.Height:= self.Height;
  FSelecionado.Width:= FLista.Width;
  FSelecionado.Position.Y:= 0;
  FSelecionado.Position.X:= 0;
  FSelecionado.SendToBack;
  FSelecionado.Stored := False;

  FLista.AddObject(FSelecionado);

  FPop:= TPopup.Create(self);
  FPop.OnClosePopup:= FecharLista;

  FListaItens:= TObjectList<TLabel>.Create;

end;

procedure TGosCombobox.FecharLista(Sender: TObject);
begin
  FLista.Visible:= false;
end;

procedure TGosCombobox.AbrirLista(Sender: TObject; const Point: TPointF);
begin
  self.AbrirLista(sender);
end;

procedure TGosCombobox.AbrirLista(Sender: TObject);
begin
  FListaItens.Clear;

  for var s in FCombo.Items do
    self.Add(s);

  FLista.Width:= self.Width;
  FLista.Visible:= not FLista.Visible;

  FSelecionado.Width:= self.Width - (FSelecionado.Stroke.Thickness*2);
  FSelecionado.Position.Y:= 0;
  FSelecionado.Position.X:= FSelecionado.Stroke.Thickness;
  FSelecionado.Fill.Color:= FCorSelecionado;

  if not Assigned(FListaItens) then
    exit;

  if FListaItens.Count = 0 then
    exit;

  FLista.Height:= FListaItens.Count * HeightItem;

  for var LItem in FListaItens do
    LItem.TextSettings.FontColor:= FItemFontColor;

  FPop.Popup;

end;

procedure TGosCombobox.LabelEnter(Sender: TObject);
var
 LLabel:TLabel;
begin

  LLabel:= TLabel(Sender);
  FSelecionado.Position.Y:= LLabel.Position.Y;
  FSelecionado.Position.X:= FSelecionado.Stroke.Thickness;

end;

procedure TGosCombobox.SetItemIndex(const Value: integer);
begin

  if Value = -1 then
  begin
    FLabel.Text:= Text;
    FItemIndex := Value;
  end
  else
  if FCombo.Count > Value then
  begin
    FLabel.Text:= FCombo.Items[Value];
    FItemIndex := Value;
  end;

end;

procedure TGosCombobox.SetItems(const Value: TStrings);
begin
  FCombo.Items:= Value;
end;

procedure TGosCombobox.SetText(const Value: string);
begin
  FText := Value;
  FLabel.Text:= Value;
end;

function TGosCombobox.Selected: string;
begin
  if Assigned(FCombo.Selected) then
    Result:= FCombo.Selected.Text;

  if Result = '' then
    Result:= FText;

end;

procedure TGosCombobox.SetItemFontColor(const Value: TalphaColor);
begin
  FItemFontColor:= Value;
  FLabel.TextSettings.FontColor:= Value;
end;

function TGosCombobox.GetItemFontColor: TalphaColor;
begin
  Result:= FItemFontColor;
end;

function TGosCombobox.GetItems: TStrings;
begin
  Result:= FCombo.Items;
end;

procedure TGosCombobox.ItemOnClick(Sender: TObject);
begin
  FCombo.ItemIndex:= TLabel(Sender).Tag;
  FItemIndex:= TLabel(Sender).Tag;
  if Assigned(FOnItemClick) then
    FOnItemClick(Sender);
  FLabel.Text:= TLabel(Sender).text;
  FLista.Visible:= false;
  FCombo.ItemIndex:= TLabel(Sender).Tag;
  FItemIndex:= TLabel(Sender).Tag;

end;

procedure TGosCombobox.ItemOnTap(Sender: TObject; const Point: TPointF);
begin
  FCombo.ItemIndex:= TLabel(Sender).Tag;
  FItemIndex:= TLabel(Sender).Tag;
  if Assigned(FOnItemTap) then
    FOnItemTap(Sender,Point);
  FLabel.Text:= TLabel(Sender).text;
  FLista.Visible:= false;
  FCombo.ItemIndex:= TLabel(Sender).Tag;
  FItemIndex:= TLabel(Sender).Tag;
end;

end.
