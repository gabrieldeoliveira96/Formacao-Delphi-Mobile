inherited dmRegister: TdmRegister
  inherited FDConnection1: TFDConnection
    Connected = True
  end
  object qRegisters: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from registers')
    Left = 244
    Top = 136
    object qRegistersID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
    end
    object qRegistersEMAIL: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'EMAIL'
      Origin = 'EMAIL'
      Size = 255
    end
    object qRegistersDATA_CADASTRO: TDateTimeField
      AutoGenerateValue = arDefault
      FieldName = 'DATA_CADASTRO'
      Origin = 'DATA_CADASTRO'
    end
    object qRegistersATIVO: TBooleanField
      AutoGenerateValue = arDefault
      FieldName = 'ATIVO'
      Origin = 'ATIVO'
    end
  end
end
