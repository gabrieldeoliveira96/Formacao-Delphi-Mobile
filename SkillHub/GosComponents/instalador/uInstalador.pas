unit uInstalador;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  System.Win.Registry,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.StrUtils,
  System.IniFiles,
  System.TypInfo,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.CheckLst,
  Vcl.FileCtrl,
  Vcl.Imaging.pngimage,
  JvWizard,
  JvExControls,
  JvWizardRouteMapNodes,
  JclIDEUtils,
  JclCompilerUtils,
  uGenerator,
  JvExStdCtrls,
  JvCombobox,
  JvListComb,
  JvComponentBase,
  JvCreateProcess, uDmPrincipal, System.json,
  AbUnzper;

type
  TfrmPrincipal = class(TForm)
    Wizard: TJvWizard;
    pnlTop: TPanel;
    lblDesc: TLabel;
    lblSite: TLabel;
    imgLogo: TImage;
    wzPageWelcome: TJvWizardInteriorPage;
    wzPageConfiguration: TJvWizardInteriorPage;
    WizardMapNodes: TJvWizardRouteMapNodes;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    lbl7: TLabel;
    chkDelphiVersion: TCheckListBox;
    lbl10: TLabel;
    lbl11: TLabel;
    cmbPlatform: TComboBox;
    cmbDelphiVersion: TComboBox;
    wzPageInstalation: TJvWizardInteriorPage;
    wzPageFinalization: TJvWizardInteriorPage;
    ckbRemoverArquivosAntigos: TCheckBox;
    lbl12: TLabel;
    mmoLog: TMemo;
    btnInstalar: TButton;
    lbl13: TLabel;
    lbl14: TLabel;
    lbl15: TLabel;
    lbl16: TLabel;
    lbl17: TLabel;
    lbl18: TLabel;
    lbl19: TLabel;
    lbl20: TLabel;
    pnlInfoCompilador: TPanel;
    lbInfo: TListBox;
    btnVisualizarLog: TButton;
    grpRegisterUnigui: TGroupBox;
    lbluniGUI: TLabel;
    lblUniGuiWeb: TLabel;
    lblUniGuiMobile: TLabel;
    imgUniGuiWebYes: TImage;
    imgUniGuiMobileYes: TImage;
    imgUniGuiWebNo: TImage;
    imgUniGuiMobileNo: TImage;
    wzPageLogin: TJvWizardWelcomePage;
    edtLogin: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtPassword: TEdit;
    btnConnect: TButton;
    chkRemember: TCheckBox;
    lblStatus: TLabel;
    lblVersion: TLabel;
    JvCreateProcess: TJvCreateProcess;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lblSiteClick(Sender: TObject);
    procedure wzPageWelcomeNextButtonClick(Sender: TObject; var Stop: Boolean);
    procedure wzPageConfigurationNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure btnInstalarClick(Sender: TObject);
    procedure wzPageInstalationNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure WizardCancelButtonClick(Sender: TObject);
    procedure WizardFinishButtonClick(Sender: TObject);
    procedure cmbDelphiVersionChange(Sender: TObject);
    procedure wzPageInstalationEnterPage(Sender: TObject;
      const FromPage: TJvWizardCustomPage);
    procedure btnVisualizarLogClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  protected
    procedure RemoverPacoteDiretorio(NomePacote: string; BDSPlatform: TJclBDSPlatform);

    function GetValReg(Chave: string): string;
    procedure SetValReg(Chave, Valor: string);
  private
    { Private declarations }
    FRadToolInstalls: TJclBorRADToolInstallations;
    FPlatform: TJclBDSPlatform;

    FCountErros: Integer;
    FInstacaoConcluida: Boolean;

    vVersionInstaller: string;
    vVersion: Integer;
    vLogGenerator: string;
    vEmailRegister: string;

//    procedure BeforeExecute(Sender: TJclBorlandCommandLineTool);
//    procedure OutputCallLine(const Text: string);

    procedure MostraDadosVersao;
    procedure RemoverComponentesAntigos;
    procedure Logar(const AString: String);

    procedure Instalar;
    function ClearDirectory(aDirectory: String): Boolean;
  public
    { Public declarations }
  end;

const
  KEY_REG_FALCON = '00A2CF584DWR5DF84';
  URL_STORE = 'https://store.falconsistemas.com.br/';

var
  frmPrincipal: TfrmPrincipal;

implementation

{$R *.dfm}

uses FS.Utils, FS.Sistema, FS.Cloud, FS.Crypt, REST.Types;


procedure TfrmPrincipal.btnVisualizarLogClick(Sender: TObject);
begin
  if vLogGenerator = EmptyStr then
    Exit;
  ShellExecute(Handle, 'open', PWideChar(vLogGenerator), '', '', 1);
end;

procedure TfrmPrincipal.btnConnectClick(Sender: TObject);
var
  dmCon:TdmCon;
  LJson:TJSONObject;
  AResult: TJSONObject;
begin
  try
    if (edtLogin.Text = EmptyStr) and (edtPassword.Text = EmptyStr) then
      Exit;

    lblStatus.Visible := True;
    lblStatus.Caption := 'Connecting ...';
    Application.ProcessMessages;

    if not TSistema.GetConexaoInternet then
      raise Exception.Create('No internet connection!');

    LJson:= TJSONObject.Create;
    dmCon := TdmCon.Create(Self);
    try
      vEmailRegister := edtLogin.Text;

      LJson.AddPair('email',edtLogin.Text);
      LJson.AddPair('password',edtPassword.Text);
      LJson.AddPair('serial',TSistema.GetSerialMotheboard);
      LJson.AddPair('ip',TSistema.GetIP);

      {TODO: REgister}
//      if not dmCon.doConnection(URL_STORE,[],TRESTRequestMethod.rmPOST, AResult, LJson.ToJSON) then
//      begin
//        lblStatus.Caption := 'E-mail or password invalid!';
//        Exit
//      end;
//
      if chkRemember.Checked then
      begin
        SetValReg('checked','true');
        SetValReg('email',edtLogin.Text);
        SetValReg('password',edtPassword.Text);
      end
      else
      begin
        SetValReg('checked','false');
        SetValReg('email','');
        SetValReg('password','');
      end;
      SetValReg('language','ptBR');

      Wizard.SelectNextPage;
    finally
      FreeAndNil(LJson);
      FreeAndNil(dmCon);
    end;
  except
    on e: exception do
      lblStatus.Caption := e.Message;
  end;
end;

procedure TfrmPrincipal.btnInstalarClick(Sender: TObject);
begin
  Instalar;
end;

procedure TfrmPrincipal.cmbDelphiVersionChange(Sender: TObject);
begin
  vVersion := cmbDelphiVersion.ItemIndex;
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FRadToolInstalls.Free;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  iFor: Integer;
begin
  if GetValReg('checked') = 'true' then
  begin
    chkRemember.Checked := True;
    edtLogin.Text := GetValReg('email');
    edtPassword.Text := GetValReg('password');
  end;

  vVersionInstaller := TSistema.GetVersao;
  vVersion    := -1;

  lblVersion.Caption := vVersionInstaller;

  FRadToolInstalls := TJclBorRADToolInstallations.Create;

  // popular o combobox de versões do delphi instaladas na máquina
  for iFor := 0 to FRadToolInstalls.Count - 1 do
  begin

    if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd3' then
      cmbDelphiVersion.Items.Add('Delphi 3')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd4' then
      cmbDelphiVersion.Items.Add('Delphi 4')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd5' then
      cmbDelphiVersion.Items.Add('Delphi 5')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd6' then
      cmbDelphiVersion.Items.Add('Delphi 6')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd7' then
      cmbDelphiVersion.Items.Add('Delphi 7')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd9' then
      cmbDelphiVersion.Items.Add('Delphi 2005')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd10' then
      cmbDelphiVersion.Items.Add('Delphi 2006')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd11' then
      cmbDelphiVersion.Items.Add('Delphi 2007')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd12' then
      cmbDelphiVersion.Items.Add('Delphi 2009')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd14' then
      cmbDelphiVersion.Items.Add('Delphi 2010')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd15' then
      cmbDelphiVersion.Items.Add('Delphi XE')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd16' then
      cmbDelphiVersion.Items.Add('Delphi XE2')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd17' then
      cmbDelphiVersion.Items.Add('Delphi XE3')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd18' then
      cmbDelphiVersion.Items.Add('Delphi XE4')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd19' then
      cmbDelphiVersion.Items.Add('Delphi XE5')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd20' then
      cmbDelphiVersion.Items.Add('Delphi XE6')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd21' then
      cmbDelphiVersion.Items.Add('Delphi XE7')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd22' then
      cmbDelphiVersion.Items.Add('Delphi XE8')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd23' then
      cmbDelphiVersion.Items.Add('Delphi 10 Seattle')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd24' then
      cmbDelphiVersion.Items.Add('Delphi 10.1 Berlin')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd25' then
      cmbDelphiVersion.Items.Add('Delphi 10.2 Tokyo')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd26' then
      cmbDelphiVersion.Items.Add('Delphi 10.3 Rio')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd27' then
      cmbDelphiVersion.Items.Add('Delphi 10.4 Sydney')
    else if FRadToolInstalls.Installations[iFor].VersionNumberStr = 'd28' then
      cmbDelphiVersion.Items.Add('Delphi 11.0 Alenxandria');

    // -- Evento disparado antes de iniciar a execução do processo. ( compilador )
    //FRadToolInstalls.Installations[iFor].DCC32.OnBeforeExecute := BeforeExecute;
    // -- Evento para saidas de mensagens.
    //FRadToolInstalls.Installations[iFor].OutputCallback := OutputCallLine;
  end;

  chkDelphiVersion.Items.Text := cmbDelphiVersion.Items.Text;

  if cmbDelphiVersion.Items.Count > 0 then
  begin
    cmbDelphiVersion.ItemIndex := 0;
    vVersion := 0;
  end;
end;

function TfrmPrincipal.GetValReg(Chave: string): string;
var
  Registro : TRegistry;
begin
  Registro := TRegistry.Create;
  try
    Registro.RootKey:=HKEY_CURRENT_USER;

    if Registro.OpenKey(KEY_REG_FALCON, True) then
    begin
      if Chave = 'checked' then
        Result := TCrypt.Crypt(TEncription.Decrypt, Registro.ReadString('checked'));
      if Chave = 'email' then
        Result := TCrypt.Crypt(TEncription.Decrypt, Registro.ReadString('email'));
      if Chave = 'password' then
        Result := TCrypt.Crypt(TEncription.Decrypt, Registro.ReadString('password'));
      if Chave = 'language' then
        Result := TCrypt.Crypt(TEncription.Decrypt, Registro.ReadString('language'));
    end;

    Registro.CloseKey;
  finally
    FreeAndNil(Registro);
  end;
end;

procedure TfrmPrincipal.Instalar;
var
  bRunOnly: Boolean;
  LDirPath:string;
  LPath:string;
  LPathLog:string;
  LFullNameBpl:string;
  LFullNameDir:string;
  LFullNameDcu:string;
  LPathZip:string;
  LAbZip: TAbUnZipper;
begin
  FCountErros := 0;
  FInstacaoConcluida := False;
  try

    for var n:= 0 to chkDelphiVersion.Count -1 do
    begin
      // só instala as versão marcadas para instalar.
      if chkDelphiVersion.Checked[n] then
      begin
        LDirPath:= FRadToolInstalls.Installations[n].GetCommonProjectsDir;
        LPath:= LDirPath + '\bpl\GosComponents';
        LPathLog:= LPath+ '\log';

        mmoLog.Clear;

        // seleciona a versão no combobox.
        cmbDelphiVersion.ItemIndex := n;
        cmbDelphiVersionChange(cmbDelphiVersion);

        // define dados da plataforna selecionada
        vVersion := cmbDelphiVersion.ItemIndex;

        //criando pasta de log caso não exista
        if not DirectoryExists(LPathLog) then
          CreateDir(LPathLog);

        vLogGenerator:= LPathLog+'\'+FormatDateTime('ddmmyyyyhhmmss',now)+'.txt';

        // mostra dados da versão na tela a ser instaladas
        MostraDadosVersao();

        btnInstalar.Enabled := False;
        wzPageInstalation.EnableButton(bkNext, False);
        wzPageInstalation.EnableButton(bkBack, False);
        wzPageInstalation.EnableButton(TJvWizardButtonKind(bkCancel), False);
        try
          var LCabecalho := TStringBuilder.Create;
          try
            LCabecalho.Append('Versão do delphi: ' + cmbDelphiVersion.Text + ' (' + IntToStr(vVersion)+ ')' + sLineBreak);
            LCabecalho.Append('Plataforma: ' + cmbPlatform.Text + '(' + IntToStr(Integer(FPlatform)) + ')' + sLineBreak);
            LCabecalho.Append('Instalador: ' + TSistema.GetVersao + sLineBreak);
            LCabecalho.Append(StringOfChar('=', 76));

            mmoLog.Clear;
            Logar(LCabecalho.ToString);
          finally
            FreeAndNil(LCabecalho);
          end;

          if ckbRemoverArquivosAntigos.Checked then
          begin
            Logar('Removendo instalações antigas...');
            RemoverComponentesAntigos;
          end;

          if not DirectoryExists(LPath) then
            CreateDir(LPath)
          else
          if ckbRemoverArquivosAntigos.Checked then
          begin
            Logar('Removendo Dir Antigo');
            ClearDirectory(LPath);
          end;

          LPathZip:= LPath+'\bin.zip';

          TResourceStream.Create(HInstance,'bin',RT_RCDATA).SaveToFile(LPathZip);

          LFullNameBpl:= LPath+'\bpl';
          if not DirectoryExists(LFullNameBpl) then
             CreateDir(LFullNameBpl);

          LFullNameDcu:= LPath+'\dcu';
          if not DirectoryExists(LFullNameDcu) then
             CreateDir(LFullNameDcu);

          LAbZip := TAbUnZipper.Create(nil);
          LAbZip.OpenArchive(LPathZip);

          for var i := 0 to PRed(LAbZip.Count) do
          begin
            if LAbZip.Items[i].IsDirectory then
            begin
              if not DirectoryExists(LPath +'\'+ LAbZip.Items[i].FileName) then
                CreateDir(LPath +'\'+ LAbZip.Items[i].FileName);
            end
            else
            LAbZip.ExtractAt(i, LPath +'\'+ LAbZip.Items[i].FileName);
          end;

          LAbZip.CloseArchive;

          LFullNameBpl:= LFullNameBpl+'\GosComponents.bpl';

          // *************************************************************************
          // Adiciona os paths dos fontes na versão do delphi selecionada
          // *************************************************************************
          with FRadToolInstalls.Installations[vVersion] do
          begin

            Logar('Adicionando library paths...');

            AddToLibrarySearchPath(LFullNameDcu, bpWin32);
            AddToLibrarySearchPath(LFullNameDcu, bpWin64);
            AddToLibrarySearchPath(LFullNameDcu, bpAndroid32);
            AddToLibrarySearchPath(LFullNameDcu, bpAndroid64);
            AddToLibrarySearchPath(LFullNameDcu, bpiOSDevice64);

            Logar('Adicionando DCU paths...');

            AddToLibrarySearchPath(LFullNameDcu+'\Win32', bpWin32);
            AddToLibrarySearchPath(LFullNameDcu+'\Win64', bpWin64);
            AddToLibrarySearchPath(LFullNameDcu+'\Android', bpAndroid32);
            AddToLibrarySearchPath(LFullNameDcu+'\Android64', bpAndroid64);
            AddToLibrarySearchPath(LFullNameDcu+'\iOSDevice64', bpiOSDevice64);

          end;

          GetDPKFileInfo(LFullNameBpl, bRunOnly);
          if not bRunOnly then
          begin
            if FRadToolInstalls.Installations[vVersion].RegisterPackage(LFullNameBpl,'GosComponents') then
              Logar('Pacote GosComponents instalado com sucesso.')
            else
            begin
              Inc(FCountErros);
              Logar('Ocorreu um erro ao instalar o pacote GosComponents.');
              Break;
            end;
          end;

          if DeleteFile(LPathZip) then
            Logar('Apagando arquivo zip')
          else
            Logar('Erro: Apagando arquivo zip');

          if FCountErros > 0 then
          begin
            if Application.MessageBox(
                PWideChar(
                  'Ocorreram erros durante o processo de instalação, '+sLineBreak+
                  'para maiores informações verifique o arquivo de log gerado.'+sLineBreak+sLineBreak+
                  'Deseja visualizar o arquivo de log gerado?'+sLineBreak+
                  '---'+sLineBreak+
                  'Errors occurred during the installation process, '+sLineBreak+
                  'for more information check the generated log file.'+sLineBreak+sLineBreak+
                  'Do you want to view the generated log file?'
                ),
                'Instalação',
                MB_ICONQUESTION + MB_YESNO
              ) = ID_YES then
            begin
              btnVisualizarLog.Click;
              Break
            end;
          end;

          // *************************************************************************
          // Não instalar outros requisitos se ocorreu erro anteriormente
          // *************************************************************************
          if FCountErros <= 0 then
          begin
            Logar('');
            Logar('Instalação efetuada com sucesso.');
          end;
        finally
          btnInstalar.Enabled := True;
          wzPageInstalation.EnableButton(bkBack, True);
          wzPageInstalation.EnableButton(bkNext, FCountErros = 0);
          wzPageInstalation.EnableButton(TJvWizardButtonKind(bkCancel), True);
        end;
      end;
    end;

  finally
    if FCountErros <= 0 then
    begin
      ShowMessage(
        'Instalação efetuada com sucesso!'+sLineBreak+
        '---'+sLineBreak+
        'Installation successful!');
      FInstacaoConcluida := True;
      wzPageInstalation.Wizard.SelectNextPage;
    end;
  end;

end;

procedure TfrmPrincipal.Label1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(lblSite.Caption), '', '', 1);
end;

procedure TfrmPrincipal.lblSiteClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(TLabel(Sender).Caption), '', '', 1);
end;

procedure TfrmPrincipal.Logar(const AString: String);
begin
  mmoLog.Lines.Add(AString);
  TUtils.WriteFile(vLogGenerator, AString);
  Application.ProcessMessages;
end;

procedure TfrmPrincipal.MostraDadosVersao;
begin
  // Mostrar ao usuário as informações de compilação
  lbInfo.Clear;
  with lbInfo.Items do
  begin
    Clear;
    Add(cmbDelphiVersion.Text + ' ' + cmbPlatform.Text);
    Application.ProcessMessages;
  end;
end;

procedure TfrmPrincipal.RemoverComponentesAntigos;
begin
  RemoverPacoteDiretorio('GosComponents', bpWin32);
  RemoverPacoteDiretorio('GosComponents', bpWin64);
  RemoverPacoteDiretorio('GosComponents', bpAndroid32);
  RemoverPacoteDiretorio('GosComponents', bpAndroid64);
  RemoverPacoteDiretorio('GosComponents', bpiOSDevice64);
end;

procedure TfrmPrincipal.RemoverPacoteDiretorio(NomePacote: string; BDSPlatform: TJclBDSPlatform);
var
  ListaPaths: TStringList;
  I: Integer;
begin
  NomePacote := UpperCase(NomePacote);
  ListaPaths := TStringList.Create;
  try
    ListaPaths.StrictDelimiter := True;
    ListaPaths.Delimiter := ';';
    with FRadToolInstalls.Installations[vVersion] do
    begin
      // Remover do search path
      ListaPaths.Clear;
      ListaPaths.DelimitedText := RawLibrarySearchPath[BDSPlatform];
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if (Pos(NomePacote, UpperCase(ListaPaths[I])) > 0) then
          ListaPaths.Delete(I);
      end;
      RawLibrarySearchPath[BDSPlatform] := ListaPaths.DelimitedText;
      // Remover do browse path
      ListaPaths.Clear;
      ListaPaths.DelimitedText := RawLibraryBrowsingPath[BDSPlatform];
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if (Pos(NomePacote, UpperCase(ListaPaths[I])) > 0) then
          ListaPaths.Delete(I);
      end;
      RawLibraryBrowsingPath[BDSPlatform] := ListaPaths.DelimitedText;
      // Remover do Debug DCU path
      ListaPaths.Clear;
      ListaPaths.DelimitedText := RawDebugDCUPath[BDSPlatform];
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if (Pos(NomePacote, UpperCase(ListaPaths[I])) > 0) then
          ListaPaths.Delete(I);
      end;
      RawDebugDCUPath[BDSPlatform] := ListaPaths.DelimitedText;
      // Remover pacotes antigos
      for I := IdePackages.Count - 1 downto 0 do
      begin
        if (Pos(NomePacote, UpperCase(IdePackages.PackageFileNames[I])) > 0) then
          IdePackages.RemovePackage(IdePackages.PackageFileNames[I]);
      end;
    end;
  finally
    ListaPaths.Free;
  end;
end;

procedure TfrmPrincipal.SetValReg(Chave, Valor: string);
var
  Registro : TRegistry;
begin
  Registro := TRegistry.Create;
  try
    Registro.RootKey:=HKEY_CURRENT_USER;

    if Registro.OpenKey(KEY_REG_FALCON, True) then
    begin
      if Chave = 'email' then
        Registro.WriteString('email', TCrypt.Crypt(TEncription.Encrypt, Valor));
      if Chave = 'password' then
        Registro.WriteString('password', TCrypt.Crypt(TEncription.Encrypt, Valor));
      if Chave = 'checked' then
        Registro.WriteString('checked', TCrypt.Crypt(TEncription.Encrypt, Valor));
      if Chave = 'language' then
        Registro.WriteString('language', TCrypt.Crypt(TEncription.Encrypt, Valor));
    end;

    Registro.CloseKey;
  finally
    FreeAndNil(Registro);
  end;
end;

procedure TfrmPrincipal.WizardCancelButtonClick(Sender: TObject);
begin
  if Application.MessageBox(
      'Deseja realmente cancelar a instalação? '#13+
      '---'#13+
      'Do you really want to cancel the installation?',
      'Close', MB_ICONQUESTION + MB_YESNO) = ID_YES then
    Self.Close;
end;

procedure TfrmPrincipal.WizardFinishButtonClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmPrincipal.wzPageConfigurationNextButtonClick(Sender: TObject;
  var Stop: Boolean);
var
  iFor: Integer;
  bChk: Boolean;
begin
  bChk := False;
  for iFor := 0 to chkDelphiVersion.Count -1 do
  begin
     if chkDelphiVersion.Checked[iFor] then
        bChk := True;
  end;

  if not bChk then
  begin
    Stop := True;
    chkDelphiVersion.SetFocus;
    Application.MessageBox(
      'Para continuar escolha a versão do Delphi que deseja instalar.'#13+
      '---'#13+
      'To continue choose the version of Delphi that you want to install.',
      'Erro.', MB_OK + MB_ICONERROR);
  end;

  // Prevenir plataforma em branco
  if Trim(cmbPlatform.Text) = '' then
  begin
    Stop := True;
    cmbPlatform.SetFocus;
    Application.MessageBox(
      'Plataforma de compilação não informada.'#13+
      '---'#13+
      'Platform of compilation not informed.',
      'Erro.', MB_OK + MB_ICONERROR);
  end;

end;

procedure TfrmPrincipal.wzPageInstalationEnterPage(Sender: TObject;
  const FromPage: TJvWizardCustomPage);
var
  iFor: Integer;
begin
  // para 64 bit somente compilar
  if FPlatform = bpWin32 then // Win32
    btnInstalar.Caption := 'Instalar'
  else // win64
    btnInstalar.Caption := 'Compilar';

  lbInfo.Clear;
  for iFor := 0 to chkDelphiVersion.Count -1 do
  begin
     // Só pega os dados da 1a versão selecionada, para mostrar na tela qual vai iniciar
     if chkDelphiVersion.Checked[iFor] then
     begin
        lbInfo.Items.Add('Instalar : ' + chkDelphiVersion.Items[ifor] + ' ' + cmbPlatform.Text);
     end;
  end;
end;

procedure TfrmPrincipal.wzPageInstalationNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  if not FInstacaoConcluida then
  begin
    Stop := True;
    Instalar;
  end;
end;

procedure TfrmPrincipal.wzPageWelcomeNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  {$IFDEF RELEASE}
  if TSistema.GetProcessoAberto('BDS.EXE') then
  begin
    Stop := True;
    Application.MessageBox(
      'Feche a IDE do Delphi antes de continuar.'#13+
      '---'#13+
      'Close the IDE of Delphi beforing of continue.',
      PWideChar(Application.Title), MB_ICONERROR + MB_OK);
  end;
  {$ENDIF}
end;

function TfrmPrincipal.ClearDirectory(aDirectory : String) : Boolean;
var
  SR: TSearchRec;
  I: integer;
begin
  I := FindFirst(aDirectory + '*.*', faAnyFile, SR);
  while I = 0 do
  begin
    if (SR.Attr and faDirectory) <> faDirectory then
    begin
      if not DeleteFile(PChar(aDirectory + SR.Name)) then
      begin
        Result := False;
        Exit;
      end;
    end;

    I := FindNext(SR);
  end;

  Result := True;
end;


//procedure TfrmPrincipal.BeforeExecute(Sender: TJclBorlandCommandLineTool);
//begin
//  // limpar os parâmetros do compilador
//  Sender.Options.Clear;
//
//  // não utilizar o dcc32.cfg
//  if FRadToolInstalls.Installations[vVersion].SupportsNoConfig then
//    Sender.Options.Add('--no-config');
//
//  // -B = Build all units
//  Sender.Options.Add('-B');
//  // O+ = Optimization
//  Sender.Options.Add('-$O-');
//  // W- = Generate stack frames
//  Sender.Options.Add('-$W+');
//  // Y+ = Symbol reference info
//  Sender.Options.Add('-$Y-');
//  // -M = Make modified units
//  Sender.Options.Add('-M');
//  // -Q = Quiet compile
//  Sender.Options.Add('-Q');
//  // não mostrar hints
//  Sender.Options.Add('-H-');
//  // não mostrar warnings
//  Sender.Options.Add('-W-');
//  // -D<syms> = Define conditionals
//  Sender.Options.Add('-DRELEASE');
//  // -U<paths> = Unit directories
//  Sender.AddPathOption('U', FRadToolInstalls.Installations[vVersion].LibFolderName[FPlatform]);
//  Sender.AddPathOption('U', FRadToolInstalls.Installations[vVersion].LibrarySearchPath[FPlatform]);
//  Sender.AddPathOption('U', vDirLibrary);
//  // -I<paths> = Include directories
//  Sender.AddPathOption('I', FRadToolInstalls.Installations[vVersion].LibrarySearchPath[FPlatform]);
//  // -R<paths> = Resource directories
//  Sender.AddPathOption('R', FRadToolInstalls.Installations[vVersion].LibrarySearchPath[FPlatform]);
//  // -N0<path> = unit .dcu output directory
//  Sender.AddPathOption('N0', vDirLibrary);
//  Sender.AddPathOption('LE', vDirLibrary);
//  Sender.AddPathOption('LN', vDirLibrary);
//
//  with FRadToolInstalls.Installations[vVersion] do
//  begin
//    if MatchText(VersionNumberStr, ['d16','d17','d18','d19','d20','d21','d22','d23','d24','d25','d26','d27']) then
//    begin
//      Sender.Options.Add('-NSSystem;Xml;Data;Datasnap;Web;Soap;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Winapi;System.Win;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell');
//    end;
//  end;
//end;
//
//procedure TfrmPrincipal.OutputCallLine(const Text: string);
//begin
//  TUtils.WriteFile(vLogGenerator, Text);
//end;

end.
