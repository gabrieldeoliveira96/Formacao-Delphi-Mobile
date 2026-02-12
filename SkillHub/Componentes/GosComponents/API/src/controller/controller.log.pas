unit controller.log;

interface

uses System.sysUtils, vcl.forms, System.Classes;

procedure Log(AStr:string);


implementation

procedure Log(AStr:string);
var
 LArqLog:string;
 LFile:TStringList;
begin

  LArqLog := ExtractFilePath(Application.ExeName) + 'log.txt';

  LFile:= TStringList.create;
  try

    if FileExists(LArqLog) then
      LFile.loadfromfile(LArqLog);

    LFile.Add(DateTimeToStr(now) + ' - '+ AStr);

    LFile.savetofile(LArqLog);

  finally
    freeandnil(LFile);
  end;


end;

end.
