unit uGosAguardeText;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Layouts,
  FMX.VirtualKeyboard, FMX.Platform, FMX.Objects, FMX.StdCtrls, FMX.Ani,
  System.UITypes, FMX.Graphics;

type
  [ComponentPlatforms($FFFF)]
  TGosAguardeText = class(TLayout)
  private
    Layout: TLayout;
    Fundo: TRectangle;
    Arco: TArc;
    lblMensagem: TLabel;
    Animacao: TFloatAnimation;
    FOpacidade: single;
    FMensagem: string;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    procedure Show;
    procedure ChangeText(NewText: string);
    procedure Stop;
  published
    { Published declarations }
    property Mensagem:string read FMensagem write FMensagem;
    property OpacityBackGround:single read FOpacidade write FOpacidade;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosAguardeText]);
end;

{ TGosAguardeText }

constructor TGosAguardeText.Create(AOwner: TComponent);
begin
  inherited;
  try
    // Panel de fundo opaco...
    Fundo := TRectangle.Create(self);
    Fundo.Opacity := 0;
    Fundo.Parent := self;
    Fundo.Align := TAlignLayout.Contents;
    Fundo.Fill.Color := TAlphaColorRec.Black;
    Fundo.Fill.Kind := TBrushKind.Solid;
    Fundo.Stroke.Kind := TBrushKind.None;
    Fundo.Visible := false;

    // Layout contendo o texto e o arco...
    Layout := TLayout.Create(self);
    Layout.Opacity := 0;
    Layout.Parent := self;
    Layout.Align := TAlignLayout.Contents;
    Layout.Width := 250;
    Layout.Height := 78;
    Layout.Visible := false;

    // Arco da animacao...
    Arco := TArc.Create(self);
    Arco.Visible := true;
    Arco.Parent := Layout;
    Arco.Align := TAlignLayout.Center;
    Arco.Margins.Bottom := 55;
    Arco.Width := 25;
    Arco.Height := 25;
    Arco.EndAngle := 280;
    Arco.Stroke.Color := $FFFEFFFF;
    Arco.Stroke.Thickness := 2;
    Arco.Position.X := Round((Layout.Width - Arco.Width) / 2);
    Arco.Position.Y := 0;

    // Animacao...
    Animacao := TFloatAnimation.Create(self);
    Animacao.Parent := Arco;
    Animacao.StartValue := 0;
    Animacao.StopValue := 360;
    Animacao.Duration := 0.8;
    Animacao.Loop := true;
    Animacao.PropertyName := 'RotationAngle';
    Animacao.AnimationType := TAnimationType.InOut;
    Animacao.Interpolation := TInterpolationType.Linear;
    //Animacao.Start;

    // Label do texto...
    lblMensagem := TLabel.Create(self);
    lblMensagem.Parent := Layout;
    lblMensagem.Align := TAlignLayout.Center;
    lblMensagem.Margins.Top := 60;
    lblMensagem.Font.Size := 13;
    lblMensagem.Height := 70;
    lblMensagem.FontColor := $FFFEFFFF;
    lblMensagem.TextSettings.HorzAlign := TTextAlign.Center;
    lblMensagem.TextSettings.VertAlign := TTextAlign.Leading;
    lblMensagem.StyledSettings := [TStyledSetting.Family, TStyledSetting.Style];
    lblMensagem.VertTextAlign := TTextAlign.Leading;
    lblMensagem.Trimming := TTextTrimming.None;
    lblMensagem.TabStop := false;
    lblMensagem.SetFocus;

    FOpacidade:= 0.7;

  except
    on e: exception do
      raise Exception.Create('TLoading error: '+e.Message);
  end;
end;

destructor TGosAguardeText.Destroy;
begin

  if Assigned(Layout) then
  begin

    try
      if Assigned(lblMensagem) then
        lblMensagem.DisposeOf;

      if Assigned(Animacao) then
        Animacao.DisposeOf;

      if Assigned(Arco) then
        Arco.DisposeOf;

      if Assigned(Fundo) then
        Fundo.DisposeOf;

      if Assigned(Layout) then
        Layout.DisposeOf;

    except
    end;
  end;

  lblMensagem := nil;
  Animacao := nil;
  Arco := nil;
  Layout := nil;
  Fundo := nil;
  inherited;

end;

procedure TGosAguardeText.Show;
var
  FService: IFMXVirtualKeyboardService;
begin
  // Esconde o teclado virtual...
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService,
    IInterface(FService));
  if (FService <> nil) then
  begin
    FService.HideVirtualKeyboard;
  end;
  FService := nil;

  Fundo.Visible:= true;
  Layout.Visible:= true;
  lblMensagem.Width := self.Width - 100;

  Animacao.Start;
  lblMensagem.Text := FMensagem;

  // Exibe os controles...
  Fundo.AnimateFloat('Opacity', FOpacidade);
  Layout.AnimateFloat('Opacity', 1);
  Layout.BringToFront;

end;

procedure TGosAguardeText.Stop;
begin
  Fundo.Visible:= false;
  Layout.Visible:= false;
  Animacao.Stop;
end;

procedure TGosAguardeText.ChangeText(NewText: string);
begin
  if Assigned(Layout) then
  begin
    try
      if Assigned(lblMensagem) then
        lblMensagem.Text := NewText;
    except
    end;
  end;
end;
end.
