unit GosComponents.ambiente;

interface

uses System.Classes, controller.log, System.IniFiles, System.SysUtils, Vcl.Forms;

type
  TModAmbiente = (Prod, Sandbox);

  TAmbienteIni = class
    EmailVendedor: string;
    Token: string;
    Ambiente: TModAmbiente;
  end;

function ambiente: TAmbienteIni;

implementation

function ambiente: TAmbienteIni;
var
  IniFile: TIniFile;
  vArqIni: string;
  LStr: TStringList;
begin

  try
    vArqIni :=  ExtractFilePath(Application.ExeName) +  'ambiente_pagseguro.ini';
    if not(FileExists(vArqIni)) then
      raise Exception.Create('Arquivo não localizado: ' + vArqIni);

    controller.log.log('Ambiente = '+vArqIni);

    IniFile := TIniFile.Create(vArqIni);
    Result:= TAmbienteIni.create;

    Result.EmailVendedor:= IniFile.ReadString('AMBIENTE', 'EmailVendedor', '');
    Result.Token:= IniFile.ReadString('AMBIENTE', 'Token', '');
    if IniFile.ReadString('AMBIENTE', 'AMBIENTE', '') = 'Sandbox' then
      Result.ambiente:= TModAmbiente.Sandbox;
    if IniFile.ReadString('AMBIENTE', 'AMBIENTE', '') = 'Prod' then
      Result.ambiente:= TModAmbiente.Prod;

  except
    on e: Exception do
    begin
      controller.log.log(e.Message);
      raise Exception.Create(e.Message);
    end;
  end;
end;

end.
