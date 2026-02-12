unit uGosAguardeBallon;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Layouts,
  FMX.Forms, FMX.Ani, System.Generics.Collections, FMX.Objects,
  System.UIConsts, FMX.Graphics, System.UITypes;

type
  [ComponentPlatforms($FFFF)]
  TGosAguardeBallon = class(TLayout)
  private
    var
    FStop: Boolean;
    AC: TCircle;
    Ani: TFloatAnimation;
    FCor4: TalphaColor;
    FCor5: TalphaColor;
    FCor2: TalphaColor;
    FCor3: TalphaColor;
    FCor1: TalphaColor;
    FDelay: integer;
    procedure AddAnimation(AAni: TFloatAnimation; ACircle: TCircle);
    procedure FloatAnimationFinish(Sender: TObject);
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    { Public declarations }
  published
    { Published declarations }
    property Delay: integer read FDelay write FDelay;
    property Cor1: TalphaColor read FCor1 write FCor1;
    property Cor2: TalphaColor read FCor2 write FCor2;
    property Cor3: TalphaColor read FCor3 write FCor3;
    property Cor4: TalphaColor read FCor4 write FCor4;
    property Cor5: TalphaColor read FCor5 write FCor5;
    procedure Show;
    procedure Stop;
  end;

procedure Register;

implementation

procedure TGosAguardeBallon.AddAnimation(AAni: TFloatAnimation; ACircle: TCircle);
begin

  AAni.Parent := ACircle;
  AAni.StartValue := self.Height;
  AAni.StopValue := -ACircle.Height;
  AAni.Duration := 0.8;
  AAni.Loop := false;
  AAni.PropertyName := 'Position.Y';
  AAni.AnimationType := TAnimationType.InOut;
  AAni.Interpolation := TInterpolationType.Linear;
  AAni.OnFinish := FloatAnimationFinish;

end;

procedure TGosAguardeBallon.Show;
begin
  FStop := false;

  TThread.CreateAnonymousThread(
    procedure
    var
      i: integer;
    begin

      i := 0;
      while true do
      begin

        inc(i);
        Ani := TFloatAnimation.Create(self);
        AC := TCircle.Create(self);

        if (i mod 5) = 1 then
          AC.Fill.Color := Cor1
        else if (i mod 5) = 2 then
          AC.Fill.Color := Cor2
        else if (i mod 5) = 3 then
          AC.Fill.Color := Cor3
        else if (i mod 5) = 4 then
          AC.Fill.Color := Cor4
        else if (i mod 5) = 5 then
          AC.Fill.Color := Cor5
        else
          AC.Fill.Color := Cor1;

        TThread.Synchronize(nil,
          procedure
          begin
            if self = nil then
             abort;

            AC.Stroke.Kind := TBrushKind.None;
            AC.Position.Y := self.Height + AC.Height + 100;
          end);

        self.AddObject(AC);
        AddAnimation(Ani, AC);

        AC.Position.X := Random(round(self.Width));


        TThread.Synchronize(nil,
          procedure
          begin
            Ani.Start;
          end);

        TThread.Synchronize(nil,
          procedure
          begin
            if FStop then
              abort;
          end);
        sleep(Delay);
      end;

    end).Start;

end;

procedure TGosAguardeBallon.Stop;
begin
  FStop := true;
end;

procedure TGosAguardeBallon.FloatAnimationFinish(Sender: TObject);
begin
  if TFloatAnimation(Sender).Parent is TCircle then
    TFloatAnimation(Sender).Parent.Free;
end;

constructor TGosAguardeBallon.Create(AOwner: TComponent);
begin
  inherited;
  FDelay:= 0;
  FCor1:= $FF0050A0;
  FCor2:= $FF4DA6FF;
  FCor3:= $FF809AB4;
  FCor4:= $FF18324C;
  FCor5:= $FFBFD4EA;
end;

destructor TGosAguardeBallon.Destroy;
begin
  FStop := true;
  Ani.Free;
  Ac.Free;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('GosComponent', [TGosAguardeBallon]);
end;

end.
