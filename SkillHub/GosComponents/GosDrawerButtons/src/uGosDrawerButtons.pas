unit uGosDrawerButtons;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Layouts, FMX.Objects,
  FMX.Forms, FMX.Graphics, FMX.Ani, FMX.StdCtrls, System.UITypes,
  System.Generics.Collections, FMX.Controls, System.ImageList,
  System.Types, FMX.ImgList, System.RTLConsts;

type
  TGosDrawerButtons = class;
  TGosDrawerButtonItem = class(TCollectionItem)
  private
    FcodItem: string;
    FitemText: string;
    FOnClick: TNotifyEvent;
    FValueClick: TNotifyEvent;
    FfontTextColor: TAlphaColor;
    FfontSize: integer;
  public
    constructor Create(Collection: TCollection); override;
  published
    property codItem: string read FcodItem write FcodItem;
    property itemText: string read FitemText write FitemText;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property fontTextColor: TAlphaColor read FfontTextColor write FfontTextColor;
    property fontSize: integer read FfontSize write FfontSize;
  end;

  TGosDrawerButtonsCollection = class(TCollection)
  private
    FSegmentButtons: TGosDrawerButtons;
  protected
    function GetOwner: TPersistent; override;
    function GetItem(Index: Integer): TGosDrawerButtonItem;
    procedure SetItem(Index: Integer; Value: TGosDrawerButtonItem);
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AButtons: TGosDrawerButtons);
    function Add: TGosDrawerButtonItem;
    function Insert( Index: Integer ): TGosDrawerButtonItem;
    property Items[index: Integer]: TGosDrawerButtonItem read GetItem write SetItem; default; // default - Added by Fenistil
  end;


  [ComponentPlatforms($FFFF)]
  TGosDrawerButtons = class(TFmxObject)
  private
    rectFundo, rectMenu, rectCancelar: TRectangle;
    lyt: TLayout;
    ani : TFloatAnimation;
    lblTitulo, lblCanc, lblItem : TLabel;
    FTitleMenuText, FCancelMenuText, FTagString : string;
    lineBorder: TLine;
    FalturaItems, FTitleFontSize, FCancelFontSize, FTag: integer;
    FCancelFontColor, FMenuColor, FTitleFontColor : TAlphaColor;
    FBackgroundOpacity : Double;
    FOwner: TComponent;
    FItems: TGosDrawerButtonsCollection;
    procedure ClickBackground(Sender: TObject);
    procedure FinishFade(Sender: TObject);
    procedure UpdateButtons;
    procedure SetItems(const Value: TGosDrawerButtonsCollection);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
  published
    property TitleMenuText: string read FTitleMenuText write FTitleMenuText;
    property TitleFontSize: integer read FTitleFontSize write FTitleFontSize;
    property TitleFontColor: TAlphaColor read FTitleFontColor write FTitleFontColor;
    property CancelMenuText: string read FCancelMenuText write FCancelMenuText;
    property CancelFontSize: integer read FCancelFontSize write FCancelFontSize;
    property CancelFontColor: TAlphaColor read FCancelFontColor write FCancelFontColor;
    property BackgroundOpacity: Double read FBackgroundOpacity write FBackgroundOpacity;
    property MenuColor: TAlphaColor read FMenuColor write FMenuColor;
    property Tag: Integer read FTag write FTag;
    property TagString: String read FTagString write FTagString;
    property Items: TGosDrawerButtonsCollection read FItems write SetItems;


  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosDrawerButtons]);
end;

{ TGosDrawerButtons }

procedure TGosDrawerButtons.ClickBackground(Sender: TObject);
begin
  Hide;
end;

constructor TGosDrawerButtons.Create(AOwner: TComponent);
begin
  inherited;
  FOwner:= AOwner;
  FAlturaItems := 0;
  FTitleMenuText := 'Escolha uma opção';
  FTitleFontSize := 15;
  FTitleFontColor := $FF8B8B8B;
  FCancelMenuText := 'Cancelar';
  FCancelFontSize := 17;
  FCancelFontColor := $FF087AF7;
  FBackgroundOpacity := 0.5;
  FMenuColor := $FFFFFFFF;

  FItems:= TGosDrawerButtonsCollection.Create(self);
end;

destructor TGosDrawerButtons.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TGosDrawerButtons.FinishFade(Sender: TObject);
begin
  if rectFundo.Tag = 0 then
    rectFundo.Visible := false;
end;

procedure TGosDrawerButtons.Hide;
begin
  rectMenu.AnimateFloat('Margins.Bottom',
                        (rectMenu.Height + 100) * -1,
                        0.3,
                        TAnimationType.InOut,
                        TInterpolationType.Circular);

  rectFundo.Tag := 0;
  ani.Delay := 0.3;
  ani.Duration := 0.2;
  ani.StartValue := 0.5;
  ani.StopValue := 0;
  //ani.OnFinish := FinishFade;
  //ani.Start;
  rectFundo.Visible := false;
end;

procedure TGosDrawerButtons.SetItems(const Value: TGosDrawerButtonsCollection);
begin
  FItems.Assign(Value);
end;

procedure TGosDrawerButtons.Show;
begin
  FalturaItems:= 0;

  // Cria rect de fundo transparente...
  rectFundo := TRectangle.Create(TForm(FOwner));
  with rectFundo do
  begin
    Align := TAlignLayout.Contents;
    Fill.Kind := TBrushKind.Solid;
    Fill.Color := $FF000000;
    Opacity := 0;
    BringToFront;
    Visible := false;
    HitTest := true;
    OnClick := ClickBackground;
    Tag := 0; // Invisivel
  end;
  TForm(FOwner).AddObject(rectFundo);


  // Cria animacao de fade do fundo...
  ani := TFloatAnimation.Create(rectFundo);
  ani.PropertyName := 'Opacity';
  ani.StartValue := 0;
  ani.StopValue := 0.5;
  ani.Inverse := false;
  ani.Duration := 0.2;
  rectFundo.AddObject(ani);

  // Layout que vai conter o menu...
  lyt := TLayout.Create(TForm(FOwner));
  lyt.Align := TAlignLayout.Contents;
  lyt.BringToFront;
  TForm(FOwner).AddObject(lyt);

  // Fundo do menu...
  rectMenu := TRectangle.Create(lyt);
  with rectMenu do
  begin
    Align := TAlignLayout.Bottom;
    Fill.Kind := TBrushKind.Solid;
    Fill.Color := FMenuColor;
    Opacity := 1;
    BringToFront;
    Visible := true;
    HitTest := true;
    XRadius := 10;
    YRadius := 10;
    Margins.Left := 10;
    Margins.Right := 10;
    Margins.Bottom := -900;
    Height := 200;
    Parent := lyt;
    Stroke.Kind := TBrushKind.None;
  end;
  lyt.AddObject(rectMenu);


  // Label com pergunta
  lblTitulo := TLabel.Create(rectMenu);
  with lblTitulo do
  begin
    Text := FTitleMenuText;
    Align := TAlignLayout.Top;
    Height := 0;
    VertTextAlign := TTextAlign.Center;
    TextAlign := TTextAlign.Center;
    StyledSettings := StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor];
    Font.Size := FTitleFontSize;
    FontColor := FTitleFontColor;
    Margins.Left := 15;
    Margins.Right := 15;
  end;
  rectMenu.AddObject(lblTitulo);

  UpdateButtons;


  // Fundo do cancelar...
  rectCancelar := TRectangle.Create(rectMenu);
  with rectCancelar do
  begin
    Align := TAlignLayout.Bottom;
    Fill.Kind := TBrushKind.Solid;
    Fill.Color := FMenuColor;
    Opacity := 1;
    BringToFront;
    Visible := true;
    HitTest := true;
    XRadius := 10;
    YRadius := 10;
    Margins.Bottom := -60;
    Height := 50;
    Parent := rectMenu;
    Stroke.Kind := TBrushKind.None;
  end;
  rectMenu.AddObject(rectCancelar);


  // Label cancelar
  lblCanc := TLabel.Create(rectCancelar);
  with lblCanc do
  begin
    Text := FCancelMenuText;
    Align := TAlignLayout.Client;
    VertTextAlign := TTextAlign.Center;
    TextAlign := TTextAlign.Center;
    StyledSettings := StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor];
    Font.Size := FCancelFontSize;
    FontColor := FCancelFontColor;
    HitTest := true;
    OnClick := ClickBackground;
  end;
  rectCancelar.AddObject(lblCanc);

  // Acerta o fundo opaco...
  rectFundo.Opacity := 0;
  rectFundo.Visible := true;
  rectFundo.Tag := 1;
  ani.Delay := 0;
  ani.StartValue := 0;
  ani.StopValue := FBackgroundOpacity;
  ani.Start;

  // Acerta item cancelar...
  lblCanc.Font.Size := FCancelFontSize;
  lblCanc.Text := FCancelMenuText;
  lblCanc.FontColor := FCancelFontColor;
  rectCancelar.Fill.Color := FMenuColor;


  // Acerta titulo do menu...
  if Trim(FTitleMenuText) = '' then
      lblTitulo.Height := 0
  else
      lblTitulo.Height := 40;

  lblTitulo.Font.Size := FTitleFontSize;
  lblTitulo.Text := FTitleMenuText;


  // Acerta menu...
  rectMenu.Fill.Color := FMenuColor;
  rectMenu.Height := lblTitulo.Height + FalturaItems;
  rectMenu.Margins.Bottom := (rectMenu.Height + 100) * -1;
  rectMenu.Visible := true;
  rectMenu.AnimateFloat('Margins.Bottom', rectCancelar.Height + 20, 0.3,
                        TAnimationType.InOut,
                        TInterpolationType.Circular);
end;

procedure TGosDrawerButtons.UpdateButtons;
begin
  for var I:= 0 to FItems.Count-1 do
  begin
    if FItems[i].itemText.IsEmpty then
     exit;

    lineBorder := TLine.Create(rectMenu);
    with lineBorder do
    begin
      Stroke.Kind := TBrushKind.Solid;
      Stroke.Color := $FFCCCCCC;
      Opacity := 0.4;
      Height := 1;
      Align := TAlignLayout.Top;
    end;
    rectMenu.AddObject(lineBorder);


    lblItem := TLabel.Create(rectMenu);
    with lblItem do
    begin
      Text := FItems[i].itemText;
      Align := TAlignLayout.Top;
      Height := 55;
      VertTextAlign := TTextAlign.Center;
      TextAlign := TTextAlign.Center;
      StyledSettings := StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor];
      Font.Size := FItems[i].FfontSize;
      FontColor := FItems[i].FfontTextColor;
      HitTest := true;
      Margins.Left := 10;
      Margins.Right := 10;
      TagString := FItems[i].FcodItem;
      OnClick := FItems[i].FOnClick;
    end;
    rectMenu.AddObject(lblItem);

    FalturaItems := FalturaItems + Trunc(lblItem.Height + 1);

  end;
end;

{ TGosDrawerButtonItem }

constructor TGosDrawerButtonItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FfontTextColor:= $FF087AF7;
  FfontSize:= 17;

end;

{ TGosDrawerButtonsCollection }

function TGosDrawerButtonsCollection.GetOwner: TPersistent;
begin
  Result := FSegmentButtons;
end;

function TGosDrawerButtonsCollection.Add: TGosDrawerButtonItem;
begin
  Result := inherited Add as TGosDrawerButtonItem;
end;

constructor TGosDrawerButtonsCollection.Create(AButtons: TGosDrawerButtons);
begin
  inherited Create(TGosDrawerButtonItem);
  FSegmentButtons := AButtons;

end;

function TGosDrawerButtonsCollection.Insert(
  Index: Integer): TGosDrawerButtonItem;
begin
  Result := inherited insert( index ) as TGosDrawerButtonItem;
end;

function TGosDrawerButtonsCollection.GetItem(Index: Integer): TGosDrawerButtonItem;
begin
  Result := inherited Items[index] as TGosDrawerButtonItem;
end;

procedure TGosDrawerButtonsCollection.SetItem(Index: Integer; Value: TGosDrawerButtonItem);
begin
  inherited SetItem(index, Value);
end;

procedure TGosDrawerButtonsCollection.Update(Item: TCollectionItem);
begin
  inherited;
end;

end.
