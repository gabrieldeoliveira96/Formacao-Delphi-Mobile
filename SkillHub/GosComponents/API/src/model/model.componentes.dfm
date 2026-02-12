inherited dmComponentes: TdmComponentes
  Height = 250
  Width = 480
  PixelsPerInch = 96
  inherited DB: TFDConnection
    Connected = True
    Left = 80
    Top = 48
  end
  object qComponents: TFDQuery
    Connection = DB
    SQL.Strings = (
      'select * from componente')
    Left = 184
    Top = 48
    object qComponentsID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object qComponentsTITULO: TMemoField
      AutoGenerateValue = arDefault
      FieldName = 'TITULO'
      Origin = 'TITULO'
      BlobType = ftMemo
    end
    object qComponentsDTCRIACAO: TDateTimeField
      AutoGenerateValue = arDefault
      FieldName = 'DTCRIACAO'
      Origin = 'DTCRIACAO'
    end
  end
  object qComponenteDescricao: TFDQuery
    Connection = DB
    SQL.Strings = (
      'select * from componente_descricao cd'
      'where cd.ID_COMPONENTE = :ID_COMPONENTE')
    Left = 176
    Top = 120
    ParamData = <
      item
        Name = 'ID_COMPONENTE'
        DataType = ftString
        ParamType = ptInput
        Value = Null
      end>
    object qComponenteDescricaoID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object qComponenteDescricaoID_COMPONENTE: TIntegerField
      FieldName = 'ID_COMPONENTE'
      Origin = 'ID_COMPONENTE'
      Required = True
    end
    object qComponenteDescricaoDESCRICAO: TMemoField
      AutoGenerateValue = arDefault
      FieldName = 'DESCRICAO'
      Origin = 'DESCRICAO'
      BlobType = ftMemo
    end
    object qComponenteDescricaoIDIOMA: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'IDIOMA'
      Origin = 'IDIOMA'
    end
  end
  object qMidiaComponentes: TFDQuery
    Connection = DB
    SQL.Strings = (
      'select * from midia m'
      'where m.ID_COMPONENTE = :ID_COMPONENTE;')
    Left = 352
    Top = 128
    ParamData = <
      item
        Name = 'ID_COMPONENTE'
        DataType = ftString
        ParamType = ptInput
        Value = Null
      end>
    object qMidiaComponentesID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
    end
    object qMidiaComponentesID_COMPONENTE: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'ID_COMPONENTE'
      Origin = 'ID_COMPONENTE'
    end
    object qMidiaComponentesARQUIVO: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'ARQUIVO'
      Origin = 'ARQUIVO'
      Size = 255
    end
    object qMidiaComponentesTIPO: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'TIPO'
      Origin = 'TIPO'
    end
  end
end
