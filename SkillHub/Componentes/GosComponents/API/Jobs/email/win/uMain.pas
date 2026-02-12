unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase,
  IdMessageClient, IdSMTPBase, IdSMTP, Vcl.ComCtrls, IdMessage,
  IdSSLOpenSSL, IdText, IdAttachmentFile, FS.Email, Vcl.ExtCtrls,
  model.email;

type
  TForm2 = class(TForm)
    Timer1: TTimer;
    Label1: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FdmEmail:TdmEnviarEmail;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  FdmEmail:= TdmEnviarEmail.Create(self);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FdmEmail);
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  FdmEmail.EnviarEmail;
end;


(*

procedure TForm2.Button2Click(Sender: TObject);
var
  LEmail:TEmail;
  LConfigEmail: TConfigEmail;
  LDadosEmail: TDadosEmail;

  LStringList:TStringList;
begin

  LConfigEmail:= TConfigEmail.Create;
  LConfigEmail.Smtp:= 'smtp.gmail.com';
  LConfigEmail.Porta:= 587;
  LConfigEmail.Autenticacao:= true;
  LConfigEmail.TLS:= true;
  LConfigEmail.SSL:= true;
  LConfigEmail.Email:= 'goscomponents@gmail.com';
  LConfigEmail.Usuario:= 'goscomponents@gmail.com';
  LConfigEmail.Senha:= 'rzqsyzlllsgidycu';
  LConfigEmail.ReadTime:= 10;
  LConfigEmail.Protocolo:= 'SMTP';

  LStringList:= TStringList.Create;
  LStringList.LoadFromFile('modelos\modelo_html_gabriel.html');


  LDadosEmail:= TDadosEmail.Create;
  LDadosEmail.ID:= 1;
  LDadosEmail.DeNome:= 'GosComponents';
//  LDadosEmail.Para:= 'gabriel.o.s@hotmail.com';
  LDadosEmail.Para:= 'gabriel.oliveira@tbdc.com.br';
  LDadosEmail.Assunto:= 'Teste email';
  LDadosEmail.Corpo:= LStringList.Text;



  LEmail:= TEmail.Cretae(LConfigEmail);

  LEmail.Envia(LDadosEmail);

end;

procedure TForm2.Enviar;
var
  // variáveis e objetos necessários para o envio
  IdSSLIOHandlerSocket: TIdSSLIOHandlerSocketOpenSSL;
  IdSMTP: TIdSMTP;
  IdMessage: TIdMessage;
  IdText: TIdText;
  sAnexo: string;

begin

  // instanciação dos objetos
  IdSSLIOHandlerSocket := TIdSSLIOHandlerSocketOpenSSL.Create(Self);
  IdSMTP := TIdSMTP.Create(Self);
  IdMessage := TIdMessage.Create(Self);

  try
    // Configuração do protocolo SSL (TIdSSLIOHandlerSocketOpenSSL)
    IdSSLIOHandlerSocket.SSLOptions.Method := sslvSSLv23;
    IdSSLIOHandlerSocket.SSLOptions.Mode := sslmClient;
//    IdSSLIOHandlerSocket.Host:= 'smtp.gmail.com';
//    IdSSLIOHandlerSocket.Port:= 587;


    // Configuração do servidor SMTP (TIdSMTP)
    IdSMTP.IOHandler := IdSSLIOHandlerSocket;
    IdSMTP.UseTLS := utUseExplicitTLS;

    IdSMTP.AuthType := satDefault;
    IdSMTP.Port := 587;
    IdSMTP.Host := 'smtp.gmail.com';
    IdSMTP.Username := 'goscomponents@gmail.com';
    IdSMTP.Password := 'rzqsyzlllsgidycu';


    // Configuração da mensagem (TIdMessage)
    IdMessage.From.Address := 'goscomponents@gmail.com';
    IdMessage.From.Name := 'goscomponents';
    IdMessage.ReplyTo.EMailAddresses := IdMessage.From.Address;
    IdMessage.Recipients.Add;
    IdMessage.Recipients.Items[0].Address:= 'gabriel.o.s@hotmail.com';
//    IdMessage.Recipients.Add.Text := 'destinatario2@email.com'; // opcional
//    IdMessage.Recipients.Add.Text := 'destinatario3@email.com'; // opcional
    IdMessage.Subject := 'Boas vindas';
    IdMessage.Encoding := meMIME;

    // Configuração do corpo do email (TIdText)
    IdText := TIdText.Create(IdMessage.MessageParts);
    IdText.Body.Add('Corpo do e-mail de boas vindas');
    IdText.ContentType := 'text/plain; charset=iso-8859-1';

    // Opcional - Anexo da mensagem (TIdAttachmentFile)
//    sAnexo := 'C:\Anexo.pdf';
//    if FileExists(sAnexo) then
//    begin
//      TIdAttachmentFile.Create(IdMessage.MessageParts, sAnexo);
//    end;

    // Conexão e autenticação
    try

//      IdSMTP.ReadTimeout := 100000;
//      IdSMTP.ConnectTimeout := 100000;
      IdSMTP.Connect;

      IdSMTP.Authenticate;
    except
      on E: Exception do
      begin
        MessageDlg('Erro na conexão ou autenticação: ' + E.Message, mtWarning,
          [mbOK], 0);
        Exit;
      end;
    end;

    // Envio da mensagem
    try
      IdSMTP.Send(IdMessage);
      MessageDlg('Mensagem enviada com sucesso!', mtInformation, [mbOK], 0);
    except
      On E: Exception do
      begin
        MessageDlg('Erro ao enviar a mensagem: ' + E.Message, mtWarning,
          [mbOK], 0);
      end;
    end;
  finally
    // desconecta do servidor
    IdSMTP.Disconnect;
    // liberação da DLL
    UnLoadOpenSSLLibrary;
    // liberação dos objetos da memória
    FreeAndNil(IdMessage);
    FreeAndNil(IdSSLIOHandlerSocket);
    FreeAndNil(IdSMTP);
  end;
end;
*)


end.
