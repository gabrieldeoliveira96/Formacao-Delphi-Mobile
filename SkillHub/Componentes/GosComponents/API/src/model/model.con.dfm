object dmCon: TdmCon
  OnCreate = DataModuleCreate
  Height = 201
  Width = 389
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
end
