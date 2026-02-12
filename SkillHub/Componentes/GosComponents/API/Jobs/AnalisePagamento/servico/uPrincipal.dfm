object GosAnalisePagamento: TGosAnalisePagamento
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'GosAnalisePagamento'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 201
  Width = 389
  PixelsPerInch = 96
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 176
    Top = 88
  end
end
