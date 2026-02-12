object GosEnvioEmail: TGosEnvioEmail
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'GosEnvioEmail'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 201
  Width = 389
  PixelsPerInch = 96
  object Timer1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 40
    Top = 16
  end
end
