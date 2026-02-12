object dmEnviarEmail: TdmEnviarEmail
  OnCreate = DataModuleCreate
  Height = 239
  Width = 490
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
  object qEmailEnvio: TFDQuery
    Connection = DB
    SQL.Strings = (
      'select e.*, u.NOME, u.EMAIL from email_envio e'
      'inner join usuario u on u.ID = e.ID_USUARIO'
      'where e.enviado = '#39'N'#39)
    Left = 280
    Top = 72
    object qEmailEnvioID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object qEmailEnvioID_EMAIL: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'ID_EMAIL'
      Origin = 'ID_EMAIL'
    end
    object qEmailEnvioID_USUARIO: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'ID_USUARIO'
      Origin = 'ID_USUARIO'
    end
    object qEmailEnvioDATA_ENVIO: TDateTimeField
      AutoGenerateValue = arDefault
      FieldName = 'DATA_ENVIO'
      Origin = 'DATA_ENVIO'
    end
    object qEmailEnvioENVIADO: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'ENVIADO'
      Origin = 'ENVIADO'
      FixedChar = True
      Size = 1
    end
    object qEmailEnvioEMAIL: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'EMAIL'
      Origin = 'EMAIL'
      ProviderFlags = []
      ReadOnly = True
      Size = 50
    end
    object qEmailEnvioNOME: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'NOME'
      Origin = 'NOME'
      ProviderFlags = []
      ReadOnly = True
      Size = 255
    end
  end
  object qEmail: TFDQuery
    Connection = DB
    SQL.Strings = (
      'select * from email'
      'where id = :id')
    Left = 392
    Top = 72
    ParamData = <
      item
        Name = 'ID'
        ParamType = ptInput
      end>
    object qEmailID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object qEmailTIPO: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'TIPO'
      Origin = 'TIPO'
    end
    object qEmailIDIOMA: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'IDIOMA'
      Origin = 'IDIOMA'
    end
    object qEmailARQUIVO: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'ARQUIVO'
      Origin = 'ARQUIVO'
      Size = 255
    end
    object qEmailASSUNTO: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'ASSUNTO'
      Origin = 'ASSUNTO'
      Size = 255
    end
  end
  object qEmailCadastro: TFDQuery
    Connection = DB
    SQL.Strings = (
      
        'insert into email_envio (ID_EMAIL, ID_USUARIO) values (:ID_EMAIL' +
        ', :ID_USUARIO)')
    Left = 276
    Top = 136
    ParamData = <
      item
        Name = 'ID_EMAIL'
        ParamType = ptInput
      end
      item
        Name = 'ID_USUARIO'
        ParamType = ptInput
      end>
  end
  object qUltimaVersao: TFDQuery
    Connection = DB
    SQL.Strings = (
      'select * from componente_versoes cv'
      'where cv.ativo = '#39'S'#39
      'order by ID desc'
      'limit 1')
    Left = 392
    Top = 144
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
  end
end
