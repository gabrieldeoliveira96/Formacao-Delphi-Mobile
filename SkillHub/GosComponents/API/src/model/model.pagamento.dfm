inherited dmPagamento: TdmPagamento
  Height = 353
  Width = 620
  PixelsPerInch = 96
  inherited DB: TFDConnection
    Left = 112
    Top = 56
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
    Left = 272
    Top = 32
  end
  object qVendas: TFDQuery
    Connection = DB
    SQL.Strings = (
      'select * from venda')
    Left = 256
    Top = 104
  end
  object qAtualizaPagamento: TFDQuery
    Connection = DB
    Left = 88
    Top = 120
  end
  object qUltimaVersao: TFDQuery
    Connection = DB
    SQL.Strings = (
      'select * from componente_versoes cv'
      'where cv.ativo = '#39'S'#39
      'order by ID desc'
      'limit 1')
    Left = 376
    Top = 200
    object qUltimaVersaoID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object qUltimaVersaoVERSAO: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'VERSAO'
      Origin = 'VERSAO'
    end
    object qUltimaVersaoDTCRIACAO: TDateField
      AutoGenerateValue = arDefault
      FieldName = 'DTCRIACAO'
      Origin = 'DTCRIACAO'
    end
    object qUltimaVersaoURL: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'URL'
      Origin = 'URL'
      Size = 255
    end
    object qUltimaVersaoATIVO: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'ATIVO'
      Origin = 'ATIVO'
      FixedChar = True
      Size = 1
    end
    object qUltimaVersaoDESCRICAO: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'DESCRICAO'
      Origin = 'DESCRICAO'
      Size = 255
    end
    object qUltimaVersaoVALOR: TFloatField
      AutoGenerateValue = arDefault
      FieldName = 'VALOR'
      Origin = 'VALOR'
    end
  end
end
