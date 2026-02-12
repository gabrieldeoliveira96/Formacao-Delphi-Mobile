unit FS.Email;

interface

uses
  System.Classes, System.Generics.Collections, System.SyncObjs;

type
  TConfigEmail = class
  private
    FSmtp: string;
    FSSL: Boolean;
    FTLS: Boolean;
    FPorta: Integer;
    FAutenticacao: Boolean;
    FEmail: string;
    FReadTime: Integer;
    FSenha: string;
    FUsuario: string;
    FProtocolo: string;
  public
    property Smtp: string read FSmtp write FSmtp;
    property Porta: Integer read FPorta write FPorta;
    property Autenticacao: Boolean read FAutenticacao write FAutenticacao;
    property SSL: Boolean read FSSL write FSSL;
    property TLS: Boolean read FTLS write FTLS;
    property Email: string read FEmail write FEmail;
    property Usuario: string read FUsuario write FUsuario;
    property Senha: string read FSenha write FSenha;
    property ReadTime: Integer read FReadTime write FReadTime;
    property Protocolo: string read FProtocolo write FProtocolo;
  end;

  TAnexoEmail = class
  private
    FNomeAnexo: string;
    FAnexo: string;
    FAnexoStream: TStream;
  public
    property NomeAnexo: string read FNomeAnexo write FNomeAnexo;
    property Anexo: string read FAnexo write FAnexo;
    property AnexoStream: TStream read FAnexoStream write FAnexoStream;

    constructor Create;
    destructor Destroy; override;
  end;

  TDadosEmail = class
  private
    FID: Integer;
    FDeNome: string;
    FPara: string;
    FCCPara: string;
    FCCOPara: string;
    FAssunto: string;
    FCorpo: string;
    FAnexo : TObjectList<TAnexoEmail>;
  public
    property ID: Integer read FID write FID;
    property DeNome: string read FDeNome write FDeNome;
    property Para: string read FPara write FPara;
    property CCPara: string read FCCPara write FCCPara;
    property CCOPara: string read FCCOPara write FCCOPara;
    property Assunto: string read FAssunto write FAssunto;
    property Corpo: string read FCorpo write FCorpo;
    property Anexo: TObjectList<TAnexoEmail> read FAnexo write FAnexo;

    constructor Create;
    destructor Destroy; override;
  end;

  TEmail = class
  private
    FConfigEmail: TConfigEmail;
    FDadosEmail: TDadosEmail;
  public
    constructor Cretae(Configuracao: TConfigEmail);
    destructor Destroy; override;

    function Envia(DadosEmail: TDadosEmail): Boolean;

    class procedure GravaLogErro(ID: Integer; Msg : string);
  end;

implementation

uses
  IdSMTP, IdMessage, IdText, IdSSLOpenSSL, IdAttachmentFile, System.SysUtils,
  IdExplicitTLSClientServerBase, System.IniFiles, Vcl.Forms, IdAttachmentMemory;

{ TDadosEmail }

constructor TDadosEmail.Create;
begin
  FAnexo := TObjectList<TAnexoEmail>.Create();
end;

destructor TDadosEmail.Destroy;
begin
  if Assigned(FAnexo) then
    FreeAndNil(FAnexo);
  inherited;
end;

{ TEmail }

constructor TEmail.Cretae(Configuracao: TConfigEmail);
begin
  FConfigEmail := Configuracao;
end;

destructor TEmail.Destroy;
begin

  inherited;
end;

function TEmail.Envia(DadosEmail: TDadosEmail): Boolean;
var
  IdSMTP: TIdSMTP;
  IdMessage: TIdMessage;
  //IdAttachmentMemory : TIdAttachmentMemory;
  IdSSLv: TIdSSLIOHandlerSocketOpenSSL;
  LTextPart: TIdText;
  I : Integer;
begin
  Result  := False;
  FDadosEmail := DadosEmail;

  try
    if FConfigEmail.Protocolo = 'SMTP' then
    begin
      {:: Cria a Estrutura da Mensagem ::}
      IdMessage := TIdMessage.Create(Nil);

      with IdMessage do
      begin
        Clear;
        IsEncoded := True;
        Encoding := meMIME; // meDefault;
        ConvertPreamble := True;
        //Priority := mpHighest; // Low=baixo / High=alto / mpHighest=urgente
        CharSet := 'utf-8';
        Date := Now;
        Subject := FDadosEmail.Assunto;

        { :: Define o Remetente e Destinatário :: }
        From.Address := FConfigEmail.Email; // ou '[EMAIL PROTECTED]'
        From.Name := FDadosEmail.DeNome;
        // ReplyTo.EMailAddresses    := Email;
        ReplyTo.EMailAddresses := Trim(FDadosEmail.Para);
        Recipients.EMailAddresses := Trim(FDadosEmail.Para);
        CCList.EMailAddresses := Trim(FDadosEmail.CCPara);
        BccList.EMailAddresses := Trim(FDadosEmail.CCOPara);
      end;

      {:: Cria o Corpo da Mensagem ::}
      LTextPart := TIdText.Create(IdMessage.MessageParts);

      with LTextPart do
      begin
        LTextPart.Body.Text := FDadosEmail.Corpo;
        LTextPart.ContentType := 'text/html';
      end;

      { :: Adiciona Anexo :: }
      if Assigned(FDadosEmail.Anexo) then
        if FDadosEmail.Anexo.Count > 0 then
        begin
          for I := 0 to FDadosEmail.Anexo.Count - 1 do
          begin
            if FDadosEmail.Anexo[I].Anexo <> EmptyStr then
            begin
              TIdAttachmentFile.Create(IdMessage.MessageParts,
                TFileName(FDadosEmail.Anexo[I].Anexo));
            end;
            {if (Assigned(FDadosEmail.Anexo[I].AnexoStream)) and (FDadosEmail.Anexo[I].AnexoStream.Size > 0) then
            begin
              IdAttachmentMemory := TIdAttachmentMemory.Create
                (IdMessage.MessageParts, FDadosEmail.Anexo[I].AnexoStream);
              IdAttachmentMemory.FileName := FDadosEmail.Anexo[I].NomeAnexo;
            end;}
          end;
        end;

      {:: Cria a Conexão com o Servidor de e-mail ::}
      IdSMTP := TIdSMTP.Create(Nil);

      with IdSMTP do
      begin
        if FConfigEmail.Autenticacao then
          AuthType  := satDefault
        else
          AuthType  := satNone;

        if FConfigEmail.SSL then
        begin
          IdSSLv := TIdSSLIOHandlerSocketOpenSSL.Create();

          IdSSLv.SSLOptions.Method := sslvSSLv23;
          IdSSLv.SSLOptions.Mode := sslmClient;
          IOHandler := IdSSLv;
          if FConfigEmail.TLS then
            UseTLS := utUseExplicitTLS;
          if Not FConfigEmail.TLS then
            UseTLS := utUseImplicitTLS; // Yahoo, Hotmail, Gmail...
        end
        else
        begin
          IOHandler := nil;
          UseTLS := utNoTLSSupport;
        end;

        ReadTimeout := FConfigEmail.ReadTime * 1000; // Leitura da Conexão em segundos!  1000 = 1 segundo
        Host := Trim(FConfigEmail.Smtp); // 'smtp.dominio.com.br';
        Port := FConfigEmail.Porta; // padrão seria 25 ou autenticada 465;
        UserName := Trim(FConfigEmail.Usuario); // 'usuario';
        Password := Trim(FConfigEmail.Senha); // 'senha';
      end;

      with IdSMTP do
      begin
        {:: Conecta e Autentica ::}
        try
          Connect;
          Authenticate;
        except
          on E: Exception do
          begin
            GravaLogErro(FDadosEmail.ID, FConfigEmail.Usuario + ' | ' + e.ClassName + ' | ' + 'Connect ' + e.Message);
            Disconnect();
          end;
        end;

        {:: Envia E-mail ::}
        Try
          Send(IdMessage);
          Disconnect();
          Result := True;
        Except
          on e: exception do
          begin
            Result := False;
            GravaLogErro(FDadosEmail.ID, FConfigEmail.Usuario + ' | ' + e.ClassName + ' | ' + 'Send ' + e.Message);
            Disconnect();
          end;
        end;
      end;

    end;

  finally
    FreeAndNil(LTextPart);
    FreeAndNil(IdSMTP);
    FreeAndNil(IdSSLv);
    FreeAndNil(IdMessage);
  end;
end;

class procedure TEmail.GravaLogErro(ID: Integer; Msg: string);
var
  Arq : TIniFile;
  Dir : string;

begin
//  FS.CriticalSection.CriticalSection.Enter;
  try
    Dir := ExtractFileDir(Application.ExeName)+'\log\';

    if not(DirectoryExists(Dir)) then
      CreateDir(Dir);

    Arq := TIniFile.Create(Dir+'log.ini');
    try
      Arq.WriteString('::' + FormatDateTime('dd/mm/yyyy hh:mm:ss:zzz', Now) + '::',
        '->' + ID.ToString() + '-> ', '' + Msg + '');
    finally
      FreeAndNil(Arq);
    end;
  finally
//    FS.CriticalSection.CriticalSection.Release;
  end;
end;

{ TAnexoEmail }

constructor TAnexoEmail.Create;
begin
  //FAnexoStream := TStream.Create;
end;

destructor TAnexoEmail.Destroy;
begin
  //FreeAndNil(FAnexoStream);
  inherited;
end;

end.
