unit uGenerator;

interface

uses
  System.Classes, System.SysUtils, Vcl.Forms, FS.Utils;

type
  TTipoInstalacao = (None, UniGui, Cloud);

  TGenerator = class
  private
  public
    class procedure Dpk(TipoInstalacao: TTipoInstalacao; DirRoot: string; Version: Integer; Mobile, UniGuiActive: Boolean);
    class procedure Reg(TipoInstalacao: TTipoInstalacao; DirRoot, Version, Email: string);
  end;

implementation


{ TGenerator }

class procedure TGenerator.Dpk(TipoInstalacao: TTipoInstalacao; DirRoot: string; Version: Integer; Mobile, UniGuiActive: Boolean);
var
  File_: TextFile;
  vArq: string;
  vDirSources: string;
  vNamePackage: string;
  FilesPas: TStringList;
  vI: Integer;
begin
  vDirSources := DirRoot + 'Sources\';

  FilesPas := TUtils.GetFilesDirectory(vDirSources, '.pas');
  try
    if (TipoInstalacao = UniGui) and (not Mobile) then
    begin
      for vI := FilesPas.Count - 1 downto 0 do
      begin
        if Pos('Unim', FilesPas[vI]) > 0 then
          FilesPas.Delete(vI);
      end;
    end;

    if (TipoInstalacao = Cloud) and (not UniGuiActive) then
    begin
      for vI := FilesPas.Count - 1 downto 0 do
      begin
        if FilesPas.KeyNames[vI] = 'FS.PagSeguro.UniGui.pas' then
          FilesPas.Delete(vI);
      end;
    end;

    if FilesPas.Count = 0 then
      raise Exception.Create('Nenhum arquivo [.pas] localizado no diretório [' + vDirSources + ']');

    if TipoInstalacao = Cloud then
    begin
      vNamePackage := 'TCloudFalcon';
      vArq := DirRoot + 'Packages\' + vNamePackage + '.dpk';
    end;

    if TipoInstalacao = UniGui then
    begin
      vNamePackage := 'TUniFalcon';
      vArq := DirRoot + 'Packages\' + vNamePackage + Version.ToString + '.dpk';
    end;

    try
      AssignFile(File_, vArq);
      Rewrite(File_);

      if TipoInstalacao = Cloud then
        Writeln(File_, 'package TCloudFalcon;');
      if TipoInstalacao = UniGui then
        Writeln(File_, 'package TUniFalcon' + Version.ToString + ';');

      Writeln(File_, '');
      Writeln(File_, '{$R ' + vNamePackage + '.res}');
      Writeln(File_, '{$IFDEF IMPLICITBUILDING This IFDEF should not be used by users}');
      Writeln(File_, '{$ALIGN 8}');
      Writeln(File_, '{$ASSERTIONS ON}');
      Writeln(File_, '{$BOOLEVAL OFF}');
      Writeln(File_, '{$DEBUGINFO OFF}');
      Writeln(File_, '{$EXTENDEDSYNTAX ON}');
      Writeln(File_, '{$IMPORTEDDATA ON}');
      Writeln(File_, '{$IOCHECKS ON}');
      Writeln(File_, '{$LOCALSYMBOLS OFF}');
      Writeln(File_, '{$LONGSTRINGS ON}');
      Writeln(File_, '{$OPENSTRINGS ON}');
      Writeln(File_, '{$OPTIMIZATION ON}');
      Writeln(File_, '{$OVERFLOWCHECKS OFF}');
      Writeln(File_, '{$RANGECHECKS OFF}');
      Writeln(File_, '{$REFERENCEINFO ON}');
      Writeln(File_, '{$SAFEDIVIDE OFF}');
      Writeln(File_, '{$STACKFRAMES ON}');
      Writeln(File_, '{$TYPEDADDRESS OFF}');
      Writeln(File_, '{$VARSTRINGCHECKS ON}');
      Writeln(File_, '{$WRITEABLECONST OFF}');
      Writeln(File_, '{$MINENUMSIZE 1}');
      Writeln(File_, '{$IMAGEBASE $400000}');
      Writeln(File_, '{$DEFINE RELEASE}');
      Writeln(File_, '{$ENDIF IMPLICITBUILDING}');
      if TipoInstalacao = Cloud then
        Writeln(File_, '{$DESCRIPTION ''FS - Cloud - [https://store.falconsistemas.com.br]''}');
      if TipoInstalacao = UniGui then
        Writeln(File_, '{$DESCRIPTION ''FS - uniGUI - [https://store.falconsistemas.com.br]''}');
      Writeln(File_, '{$IMPLICITBUILD ON}');
      Writeln(File_, '');

      Writeln(File_, 'requires');
      if TipoInstalacao = Cloud then
      begin
        Writeln(File_, '  designide,');
        if UniGuiActive then
          Writeln(File_, '  uniGUI' + Version.ToString + ',');
        Writeln(File_, '  IndyCore,');
        Writeln(File_, '  IndySystem,');
        Writeln(File_, '  IndyProtocols,');
        Writeln(File_, '  xmlrtl;');
      end;
      if TipoInstalacao = UniGui then
      begin
        Writeln(File_, '  designide,');
        Writeln(File_, '  vcl,');
        Writeln(File_, '  uniGUI' + Version.ToString + ',');
        if Mobile then
          Writeln(File_, '  uniGUI' + Version.ToString + 'm,');
        Writeln(File_, '  IndyCore,');
        Writeln(File_, '  IndySystem,');
        Writeln(File_, '  IndyProtocols;');
      end;

      Writeln(File_, '');
      Writeln(File_, 'contains');

      for vI := 0 to FilesPas.Count - 1 do
      begin
        FilesPas.Sort;
        if vI = FilesPas.Count - 1 then
          Writeln(File_, '  ' + StringReplace(FilesPas.KeyNames[vI], '.pas', '', []) + ' in ''..\Sources\' + FilesPas.KeyNames[vI] + '''; ')
        else
          Writeln(File_, '  ' + StringReplace(FilesPas.KeyNames[vI], '.pas', '', []) + ' in ''..\Sources\' + FilesPas.KeyNames[vI] + ''', ');
      end;

      Writeln(File_, '');
      Writeln(File_, 'end.');
    finally
      CloseFile(File_);
    end;
  finally
    FreeAndNil(FilesPas);
  end;
end;

class procedure TGenerator.Reg(TipoInstalacao: TTipoInstalacao; DirRoot, Version, Email: string);
var
  File_: TextFile;
  vArq: string;
begin

  if TipoInstalacao = UniGui then
  begin
    vArq := DirRoot + 'Sources\UniFSRegister.pas';

    try
      AssignFile(File_, vArq);
      Rewrite(File_);

      Writeln(File_, 'unit UniFSRegister;');
      Writeln(File_, '');
      Writeln(File_, 'interface');
      Writeln(File_, '');
      Writeln(File_, 'procedure Register;');
      Writeln(File_, '');
      Writeln(File_, 'implementation');
      Writeln(File_, '');
      Writeln(File_, 'uses');
      Writeln(File_, '  System.Classes,');
      Writeln(File_, '  System.SysUtils,');
      Writeln(File_, '  Winapi.Windows,');
      Writeln(File_, '  DateUtils,');
      Writeln(File_, '  DesignIntf,');
      Writeln(File_, '  ToolsApi;');
      Writeln(File_, '');
      Writeln(File_, 'const');
      Writeln(File_, '  UniFalconPackage = ''' + Version + ''';');
      Writeln(File_, '');
      Writeln(File_, 'var');
      Writeln(File_, ' AboutBoxServices: IOTAAboutBoxServices = nil;');
      Writeln(File_, ' AboutBoxIndex: Integer = 0;');
      Writeln(File_, '');
      Writeln(File_, 'procedure RegisterAboutBox;');
      Writeln(File_, 'var');
      Writeln(File_, '  BitmapHandle: HBITMAP;');
      Writeln(File_, 'begin');
      Writeln(File_, '  Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices);');
      Writeln(File_, '  Assert(AboutBoxServices <> nil);');
      Writeln(File_, '  BitmapHandle := LoadBitmap(FindResourceHInstance(HInstance), ''FALCONSPLASH'');');
      Writeln(File_, '');
      Writeln(File_, '  AboutBoxIndex := AboutBoxServices.AddPluginInfo(');
      Writeln(File_, '    ''UniFalcon Components '' + UniFalconPackage,');
      Writeln(File_, '      Format(''UniFalcon Components for uniGui''#13#10 +');
      Writeln(File_, '             ''Copyright 2016-%d https://store.falconsistemas.com.br''#13#10 +');
      Writeln(File_, '             ''All Rights Reserved.''#13#10 +');
      Writeln(File_, '             ''suporte@falconsistemas.com.br'', [YearOf(Now)]),');
      Writeln(File_, '             BitmapHandle, False, ''Registered for ' + Email + ' ''');
      Writeln(File_, '  );');
      Writeln(File_, 'end;');
      Writeln(File_, '');
      Writeln(File_, 'procedure UnregisterAboutBox;');
      Writeln(File_, 'begin');
      Writeln(File_, '  if (AboutBoxIndex <> 0) and (AboutBoxServices <> nil) then');
      Writeln(File_, '  begin');
      Writeln(File_, '    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);');
      Writeln(File_, '    AboutBoxIndex := 0;');
      Writeln(File_, '    AboutBoxServices := nil;');
      Writeln(File_, '  end;');
      Writeln(File_, 'end;');
      Writeln(File_, '');
      Writeln(File_, 'procedure RegisterSplashScreen;');
      Writeln(File_, 'var');
      Writeln(File_, '  BitmapHandle: HBITMAP;');
      Writeln(File_, 'begin');
      Writeln(File_, '  Assert(SplashScreenServices <> nil);');
      Writeln(File_, '  BitmapHandle := LoadBitmap(FindResourceHInstance(HInstance), ''FALCONSPLASH'');');
      Writeln(File_, '');
      Writeln(File_, '  SplashScreenServices.AddPluginBitmap(''UniFalcon Components '' + UniFalconPackage,');
      Writeln(File_, '    BitmapHandle, False, ''Registered for ' + Email + ' '');');
      Writeln(File_, 'end;');
      Writeln(File_, '');
      Writeln(File_, 'procedure Register;');
      Writeln(File_, 'begin');
      Writeln(File_, '  ForceDemandLoadState(dlDisable);');
      Writeln(File_, 'end;');
      Writeln(File_, '');
      Writeln(File_, 'initialization');
      Writeln(File_, '  RegisterSplashScreen;');
      Writeln(File_, '  RegisterAboutBox;');
      Writeln(File_, '');
      Writeln(File_, 'finalization');
      Writeln(File_, '  UnregisterAboutBox;');
      Writeln(File_, '');
      Writeln(File_, 'end.');
    finally
      CloseFile(File_);
    end;

  end;
end;

end.

