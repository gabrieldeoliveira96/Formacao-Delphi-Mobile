unit uGosEditTitle;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.ActnList,
  System.UITypes, FMX.StdCtrls, FMX.Ani, FMX.Graphics,
  FMX.Objects, FMX.Edit, uGosEdit, uGosObjects, uGosBase;

type
  TMode = (Line, Rectangle);

  TFontSize = class(TPersistent)
  private
    FSizeTitle:single;
    FSizePrompt:single;
    FSizeText:single;
    procedure SetSizePrompt(const Value: Single);
    procedure SetSizeText(const Value: Single);
    procedure SetSizeTitle(const Value: Single);
  public
    var OnChange: TProc;
    constructor Create(ASizeDefault:Single);
  published
    property SizeTitle:Single read FSizeTitle write SetSizeTitle;
    property SizePrompt:Single read FSizePrompt write SetSizePrompt;
    property SizeText:Single read FSizeText write SetSizeText;
  end;

  [ComponentPlatforms($FFFF)]
  TGosEditTitle = class(TGosRectangle, IControl)
  private
    FBackGroundColor: TAlphaColor;
    FCorTextTitle: TAlphaColor;
    FTextPrompt: string;
    FBorderStroke: integer;
    FTextTitulo: string;
    FBorderColor: TAlphaColor;
    FMode: TMode;
    FText: string;
    FFontSize: TFontSize;
    procedure FinishFloatAnimationEvent(Sender: TObject);
    procedure FloatAnimationProgress(Sender: TObject);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetBackGroundColor(const Value: TAlphaColor);
    procedure SetTextPrompt(const Value: string);
    procedure SetTextTitulo(const Value: string);
    procedure SetBorderStroke(const Value: integer);
    procedure SetCorTextTitle(const Value: TAlphaColor);
    procedure SetMode(const Value: TMode);
    procedure SetText(const Value: string);
    procedure BuildProperty;

  protected
    FLabelPrompt:TLabel;
    FAniEnterEdit:TFloatAnimation;
    FRecLabel:TGosRectangle;
    [Week]FEditCustom:TGosEditView;
    FFoco:Boolean;
    FRec:TGosRectangle;
    FLabelTitle:TLabel;
    function EnterChildren(AObject: IControl): Boolean;override;
    function ExitChildren(AObject: IControl): Boolean;override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BorderColor:TAlphaColor read FBorderColor write SetBorderColor;
    property BackGroundColor:TAlphaColor read FBackGroundColor write SetBackGroundColor;
    property BorderStroke:integer read FBorderStroke write SetBorderStroke;

    property EditCustom:TGosEditView read FEditCustom;

    property Mode: TMode read FMode write SetMode;
    property Text: string read FText write SetText;

    property TextPrompt:string read FTextPrompt write SetTextPrompt;
    property TextTitulo:string read FTextTitulo write SetTextTitulo;
    property CorTextTitle:TAlphaColor read FCorTextTitle write SetCorTextTitle;
    property FontSize: TFontSize read FFontSize write FFontSize;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosEditTitle]);
end;

{ TGosEditTitle }

function TGosEditTitle.EnterChildren(AObject: IControl): Boolean;
begin
  if FFoco then
    exit;

  FFoco:= true;
  FRecLabel.Visible:= true;

  FRecLabel.Width:= FLabelTitle.Canvas.TextWidth(FLabelTitle.Text)+6;
  FRecLabel.Height:= FLabelTitle.Canvas.TextHeight(FLabelTitle.Text);
  FLabelPrompt.Text:= '';
  FAniEnterEdit.Inverse := false;
  FRecLabel.Parent:= TGosEditView(AObject.GetObject).Parent;
  FAniEnterEdit.Start;
end;

function TGosEditTitle.ExitChildren(AObject: IControl): Boolean;
begin
  FAniEnterEdit.Inverse:= true;
  FAniEnterEdit.Start;
end;

procedure TGosEditTitle.FinishFloatAnimationEvent(Sender:TObject);
begin

  if FAniEnterEdit.Inverse then
  begin
    FRecLabel.Visible:= false;
    if FEditCustom.Text.IsEmpty then
      FLabelPrompt.Text:= FTextPrompt;

    FFoco:= false;
  end;

end;

procedure TGosEditTitle.FloatAnimationProgress(Sender: TObject);
begin
  FLabelPrompt.Text:= '';
end;

procedure TGosEditTitle.BuildProperty;
begin
  self.Fill.Color:= FBackGroundColor;

  FRec.Stroke.Color:= BorderColor;
  FRec.Stroke.Thickness:= BorderStroke;
  FRec.Fill.Color:= BackGroundColor;

  FLabelPrompt.Text:= FTextPrompt;

  FLabelTitle.Text:= TextTitulo;
  FLabelTitle.TextSettings.FontColor:= CorTextTitle;

  FRecLabel.Fill.Color:= BackGroundColor;

  case Mode of
    Line:
    begin
      FRec.Stroke.Kind:= TBrushKind.None;
      TDrawableBorder(FEditCustom.Background).Border.Kind:= TBrushKind.Solid;
      TDrawableBorder(FEditCustom.Background).Border.Style:= TViewBorderStyle.LineBottom;
      TDrawableBorder(FEditCustom.Background).Border.Color:= TGosViewColor.Create(BorderColor);
      TDrawableBorder(FEditCustom.Background).Border.Width:= BorderStroke;
    end;
    Rectangle:
    begin
      FRec.Stroke.Kind:= TBrushKind.Solid;
      TDrawableBorder(FEditCustom.Background).Border.Kind:= TBrushKind.none;
    end;
  end;

  FLabelPrompt.TextSettings.Font.Size:= FontSize.SizePrompt;
  FLabelTitle.TextSettings.Font.Size:= FontSize.SizeTitle;
  FEditCustom.TextSettings.Font.Size:= FontSize.SizeText;

end;

procedure TGosEditTitle.SetBorderColor(const Value: TAlphaColor);
begin
  FBorderColor := Value;
  BuildProperty;
end;

procedure TGosEditTitle.SetBorderStroke(const Value: integer);
begin
  FBorderStroke := Value;
  BuildProperty;
end;

procedure TGosEditTitle.SetBackGroundColor(const Value: TAlphaColor);
begin
  FBackGroundColor := Value;
  BuildProperty;
end;

procedure TGosEditTitle.SetCorTextTitle(const Value: TAlphaColor);
begin
  FCorTextTitle := Value;
  BuildProperty;
end;

procedure TGosEditTitle.SetMode(const Value: TMode);
begin
  FMode := Value;
  BuildProperty;
end;

procedure TGosEditTitle.SetText(const Value: string);
begin
  EditCustom.Text:= Value;
  EnterChildren(EditCustom);
  FText := Value;
end;

procedure TGosEditTitle.SetTextPrompt(const Value: string);
begin
  FTextPrompt := Value;
  BuildProperty;
end;

procedure TGosEditTitle.SetTextTitulo(const Value: string);
begin
  FTextTitulo := Value;
  BuildProperty;
end;

constructor TGosEditTitle.Create(AOwner: TComponent);
begin
  inherited;
  //inicializando as variaveis
  FFoco:= false;
  FBorderColor:= $FF087AF7;
  FBackGroundColor:= $FFFFFFFF;
  FTextPrompt:= 'Digite Aqui';
  FTextTitulo:= 'Titulo';
  FBorderStroke:= 2;
  FCorTextTitle:= $FF087AF7;
  FMode:= Rectangle;

  FFontSize:= TFontSize.create(12);
  FFontSize.onChange:= BuildProperty;

  //inicializando o tamanho do container ( componente )
  self.Height:= 48;
  self.Width:= 200;
  self.XRadius:= 5;
  self.YRadius:= 5;
  self.Fill.Color:= BackGroundColor;
  self.Stroke.Kind:= TBrushKind.None;
  self.CanFocus:= true;
  self.HitTest:= false;

  //rectangle base para o componente
  FRec:= TGosRectangle.Create(self);
  FRec.Align:= TAlignLayout.Client;
  FRec.XRadius:= 5;
  FRec.YRadius:= 5;
  FRec.Stroke.Color:= BorderColor;
  FRec.Stroke.Thickness:= BorderStroke;
  FRec.Fill.Color:= BackGroundColor;
  FRec.Margins.Top:= 9;
  FRec.HitTest:= false;
  FRec.Stored := False;

  //modo de exibição da linha
  if Mode = Line then
    FRec.Stroke.Kind:= TBrushKind.None;
  if Mode = Rectangle then
    FRec.Stroke.Kind:= TBrushKind.Solid;

  FRec.SetSubComponent(true);
  FRec.HitTest:= false;

  self.AddObject(FRec);

  //componente de texto
  FEditCustom:= TGosEditView.Create(self);
  FEditCustom.Align:= TAlignLayout.Client;
  FEditCustom.Margins.Left:= 8;
  FEditCustom.Margins.Right:= 8;
  FEditCustom.Margins.Top:= 4;
  FEditCustom.Margins.Bottom:= 4;
  FEditCustom.Name:= 'GosEditView';
  FEditCustom.Text:= '';
  FEditCustom.Stored := False;

  case Mode of
    Line:
    begin
      TDrawableBorder(FEditCustom.Background).Border.Kind:= TBrushKind.Solid;
      TDrawableBorder(FEditCustom.Background).Border.Style:= TViewBorderStyle.LineBottom;
      TDrawableBorder(FEditCustom.Background).Border.Color:= TGosViewColor.Create(BorderColor);
      TDrawableBorder(FEditCustom.Background).Border.Width:= BorderStroke;
    end;
    Rectangle: TDrawableBorder(FEditCustom.Background).Border.Kind:= TBrushKind.none;
  end;

  FEditCustom.SetSubComponent(true);

  //adicionando componente de texto ao container base
  FRec.AddObject(FEditCustom);

  //texto de prompt
  FLabelPrompt:= TLabel.Create(self);
  FLabelPrompt.Align:= TAlignLayout.Client;
  FLabelPrompt.Margins.Left:= 2;
  FLabelPrompt.Text:= TextPrompt;
  FLabelPrompt.StyledSettings:= [];
  FLabelPrompt.TextSettings.FontColor:= $FF9E9E9E;
  FLabelPrompt.HitTest:= false;
  FLabelPrompt.Stored := False;

  FEditCustom.AddObject(FLabelPrompt);

///titulo

  FLabelTitle:= TLabel.Create(self);
  FLabelTitle.Align:= TAlignLayout.Client;
  FLabelTitle.Text:= TextTitulo;
  FLabelTitle.AutoSize:= true;
  FLabelTitle.StyledSettings:= [];
  FLabelTitle.TextSettings.FontColor:= CorTextTitle;
  FLabelTitle.Position.Y:= FLabelPrompt.Position.Y;
  FLabelTitle.RecalcSize;
  FLabelTitle.TextSettings.HorzAlign:= TTextAlign.Center;
  FLabelTitle.SetSubComponent(true);
  FLabelTitle.HitTest:= false;
  FLabelTitle.Stored := False;

  FRecLabel:= TGosRectangle.Create(FRec);
  FRecLabel.Align:= TAlignLayout.None;
  FRecLabel.Width:= FLabelTitle.Width;
  FRecLabel.Height:= FLabelTitle.Height;
  FRecLabel.Fill.Color:= BackGroundColor;
  FRecLabel.Stroke.Kind:= TBrushKind.None;
  FRecLabel.Position.X:= 10;
  FRecLabel.Position.Y:= -7;
  FRecLabel.HitTest:= false;
  FRecLabel.Stored := False;

  FRecLabel.AddObject(FLabelTitle);

  FAniEnterEdit:= TFloatAnimation.Create(self);
  FAniEnterEdit.PropertyName := 'Position.Y';
  FAniEnterEdit.StartValue := FLabelTitle.Position.Y;
  FAniEnterEdit.StopValue := -FLabelTitle.Height/2;
  FAniEnterEdit.Duration := 0.1;
  FAniEnterEdit.OnFinish:= FinishFloatAnimationEvent;
  FAniEnterEdit.OnProcess:= FloatAnimationProgress;
  FRecLabel.AddObject(FAniEnterEdit);

  FEditCustom.Parent.AddObject(FRecLabel);

  FRecLabel.Visible:= false;

  //iniciando classe de fontes
  FLabelPrompt.TextSettings.Font.Size:= FontSize.SizePrompt;
  FLabelTitle.TextSettings.Font.Size:= FontSize.SizeTitle;
  FEditCustom.TextSettings.Font.Size:= FontSize.SizeText;

end;

{ TFontSize }

constructor TFontSize.Create(ASizeDefault:Single);
begin
  inherited Create;
  SizeTitle:= ASizeDefault;
  SizePrompt:= ASizeDefault;
  SizeText:= ASizeDefault;
end;

procedure TFontSize.SetSizePrompt(const Value: Single);
begin
  FSizePrompt := Value;
  if Assigned(OnChange) then
   OnChange;
end;

procedure TFontSize.SetSizeText(const Value: Single);
begin
  FSizeText := Value;
  if Assigned(OnChange) then
   OnChange;
end;

procedure TFontSize.SetSizeTitle(const Value: Single);
begin
  FSizeTitle := Value;
  if Assigned(OnChange) then
   OnChange;
end;

initialization
  RegisterFmxClasses([TGosEditView]);

end.
