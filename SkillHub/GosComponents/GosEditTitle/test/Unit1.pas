unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, ALFmxObjects, ALFmxEdit, FMX.Controls.Presentation, FMX.Ani,
  UI.Base, UI.Edit;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure FloatAnimationEditExitFinish(Sender: TObject);
  private
    const BoardCor = $FF087AF7;
    const CorFundo = $FFFFFFFF;
    const TextPront = 'Digite Aqui';
    const TextTitulo = 'Titulo';
    const BoardStroke = 2;
    const CorTextTitle = $FF087AF7;
    var
     FRecContainer:TALRectangle;
     FRec:TALRectangle;
     FEdit:TEditView;
     FLabelPronpt:TLabel;

     FLabelTitle:TLabel;
     FAniEnterEdit:TFloatAnimation;
     FRecLabel:TALRectangle;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.EditEnter(Sender: TObject);
begin

  FRecLabel.Width:= FLabelTitle.Canvas.TextWidth(FLabelTitle.Text)+6;
  FRecLabel.Height:= FLabelTitle.Canvas.TextHeight(FLabelTitle.Text);
  FLabelPronpt.Visible:= false;

  FAniEnterEdit.Inverse := false;

  FRecLabel.Visible:= true;
  FAniEnterEdit.Start;

end;

procedure TForm1.EditExit(Sender: TObject);
begin
  FEdit.OnEnter:= nil;
  FAniEnterEdit.Inverse:= true;
  FAniEnterEdit.Start;
end;

procedure TForm1.FloatAnimationEditExitFinish(Sender: TObject);
begin
  if FAniEnterEdit.Inverse then
  begin
    FRecLabel.Visible:= false;
    if FEdit.Text.IsEmpty then
      FLabelPronpt.Visible:= true;

    FEdit.OnEnter:= EditEnter;
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  FRecContainer:= TALRectangle.Create(self);
  FRecContainer.Align:= TAlignLayout.Center;{*}
  FRecContainer.Height:= 48;
  FRecContainer.Width:= 200;
  FRecContainer.XRadius:= 5;
  FRecContainer.YRadius:= 5;
  FRecContainer.Stroke.Kind:= TBrushKind.None;
  FRecContainer.Fill.Color:= CorFundo;

  FRec:= TALRectangle.Create(self);
  FRec.Align:= TAlignLayout.Client;
  FRec.XRadius:= 5;
  FRec.YRadius:= 5;
  FRec.Stroke.Color:= BoardCor;
  FRec.Stroke.Thickness:= BoardStroke;
  FRec.Fill.Color:= CorFundo;
  FRec.Margins.Top:= 9;

  FRecContainer.AddObject(FRec);

  FEdit:= TEditView.Create(self);
  FEdit.Align:= TAlignLayout.Client;
  FEdit.Margins.Left:= 8;
  FEdit.Margins.Right:= 8;
  FEdit.Margins.Top:= 4;
  FEdit.Margins.Bottom:= 4;
  FEdit.OnEnter:= EditEnter;
  FEdit.OnExit:= EditExit;

  FRec.AddObject(FEdit);

  FLabelPronpt:= TLabel.Create(self);
  FLabelPronpt.Align:= TAlignLayout.Client;
  FLabelPronpt.Margins.Left:= 2;
  FLabelPronpt.Text:= TextPront;
  FLabelPronpt.StyledSettings:= [];
  FLabelPronpt.TextSettings.FontColor:= $FF9E9E9E;

  FEdit.AddObject(FLabelPronpt);


///titulo
///
///

  FLabelTitle:= TLabel.Create(self);
  FLabelTitle.Align:= TAlignLayout.Client;
  FLabelTitle.Text:= TextTitulo;
  FLabelTitle.AutoSize:= true;
  FLabelTitle.StyledSettings:= [];
  FLabelTitle.TextSettings.FontColor:= CorTextTitle;
  FLabelTitle.Position.Y:= FLabelPronpt.Position.Y;
  FLabelTitle.RecalcSize;
  FLabelTitle.TextSettings.HorzAlign:= TTextAlign.Center;

  FAniEnterEdit:= TFloatAnimation.Create(self);
  FAniEnterEdit.PropertyName := 'Position.Y';
  FAniEnterEdit.StartValue := FLabelTitle.Position.Y;
  FAniEnterEdit.StopValue := -FLabelTitle.Height/2;
  FAniEnterEdit.Duration := 0.1;
  FAniEnterEdit.OnFinish:= FloatAnimationEditExitFinish;

  FRecLabel:= TALRectangle.Create(self);
  FRecLabel.Align:= TAlignLayout.None;
  FRecLabel.Fill.Color:= TAlphaColors.White;
  FRecLabel.Stroke.Color:= TAlphaColors.White;
  FRecLabel.Position.X:= 10;
  FRecLabel.Width:= FLabelTitle.Width;
  FRecLabel.Height:= FLabelTitle.Height;

  FRecLabel.AddObject(FLabelTitle);
  FRecLabel.AddObject(FAniEnterEdit);
  FRecLabel.Visible:= false;


  FRec.AddObject(FRecLabel);


  self.AddObject(FRecContainer);



end;

end.
