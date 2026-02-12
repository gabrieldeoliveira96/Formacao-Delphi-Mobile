object dmPagamento: TdmPagamento
  OnCreate = DataModuleCreate
  Height = 251
  Width = 553
  PixelsPerInch = 96
  object DB: TFDConnection
    Params.Strings = (
      'Database=goscomponentes'
      'User_Name=gos'
      'Server=ec2-54-160-223-58.compute-1.amazonaws.com'
      'Password=oliveira.s3084'
      'DriverID=MySQL')
    ConnectedStoredUsage = [auRunTime]
    LoginPrompt = False
    Left = 176
    Top = 88
  end
  object qPagamento: TFDQuery
    Connection = DB
    SQL.Strings = (
      'select * from venda v'
      'where v.STATUS_PAGAMENTO in (0,1,2)')
    Left = 264
    Top = 112
    object qPagamentoID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object qPagamentoID_USUARIO: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'ID_USUARIO'
      Origin = 'ID_USUARIO'
    end
    object qPagamentoREFERENCIA: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'REFERENCIA'
      Origin = 'REFERENCIA'
      Size = 255
    end
    object qPagamentoDATA_COMPRA: TDateTimeField
      AutoGenerateValue = arDefault
      FieldName = 'DATA_COMPRA'
      Origin = 'DATA_COMPRA'
    end
    object qPagamentoSTATUS_PAGAMENTO: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'STATUS_PAGAMENTO'
      Origin = 'STATUS_PAGAMENTO'
    end
    object qPagamentoTIPO_PAGAMENTO: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'TIPO_PAGAMENTO'
      Origin = 'TIPO_PAGAMENTO'
      Size = 255
    end
    object qPagamentoDATA_PAGAMENTO: TDateTimeField
      AutoGenerateValue = arDefault
      FieldName = 'DATA_PAGAMENTO'
      Origin = 'DATA_PAGAMENTO'
    end
  end
  object FSPagSeguro1: TFSPagSeguro
    Ambiente = Sandbox
    ApiPagamento.Frete.ShippingType = 0
    ApiAssinaturaCobranca.Assinatura.ApprovalPeriod = WEEKLY
    ApiAssinaturaCobranca.Assinatura.ApprovalNeverExpire = False
    ApiAssinaturaCobranca.Cliente.AdressNumber = 0
    ApiBoleto.Cliente.AdressNumber = 0
    ApiBoleto.Boleto.NumberOfPayments = 0
    Parametros.MaxUses = 0
    Parametros.MaxAge = 0
    Parametros.Log.GravarLogEnvio = True
    Parametros.Log.LogEnvio = '.\log_pagseguro_envio.txt'
    Parametros.Log.GravarLogRetorno = True
    Parametros.Log.LogRetorno = '.\log_pagseguro_retorno.txt'
    Parametros.UtilizaCloudFS = False
    Parametros.TimeOut = 0
    Left = 384
    Top = 40
  end
end
