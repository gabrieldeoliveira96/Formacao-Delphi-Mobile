object dmCon: TdmCon
  OldCreateOrder = False
  Height = 243
  Width = 398
  object RESTClient1: TRESTClient
    BaseURL = 'http://localhost:9010/servicos/7'
    Params = <>
    Left = 176
    Top = 112
  end
  object RESTRequest1: TRESTRequest
    AssignedValues = [rvConnectTimeout, rvReadTimeout]
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    Left = 216
    Top = 160
  end
  object RESTResponse1: TRESTResponse
    Left = 272
    Top = 104
  end
end
