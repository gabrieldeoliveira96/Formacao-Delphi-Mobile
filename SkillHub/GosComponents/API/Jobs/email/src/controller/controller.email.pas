unit controller.email;

interface

uses Fs.Email, System.Classes, model.email, System.SysUtils,
  System.Generics.Collections;

procedure EnviarEmail(AEmailDestino, AAssuntoEmail, APathArquivo:string;
  AReplace:TDictionary<String, string>);
procedure CadastrarEmail(AIdEmail, AIdUsuario:integer);

implementation

procedure EnviarEmail(AEmailDestino, AAssuntoEmail, APathArquivo:string;
  AReplace:TDictionary<String, string>);
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
  LStringList.LoadFromFile(APathArquivo);

  for var i := 0 to Pred(LStringList.Count) do
  begin
    for var LKey in AReplace.Keys do
    begin
      if (not LKey.IsEmpty) and LStringList[i].Contains(LKey) then
        LStringList[i]:= LStringList[i].Replace(LKey,AReplace.Items[LKey],[rfReplaceAll,rfIgnoreCase]);
    end;
  end;

  LDadosEmail:= TDadosEmail.Create;
  LDadosEmail.ID:= 1;
  LDadosEmail.DeNome:= 'GosComponents';
  LDadosEmail.Para:= AEmailDestino;
  LDadosEmail.Assunto:= AAssuntoEmail;
  LDadosEmail.Corpo:= LStringList.Text;

  LEmail:= TEmail.Cretae(LConfigEmail);

  LEmail.Envia(LDadosEmail);

end;

procedure CadastrarEmail(AIdEmail, AIdUsuario:integer);
var
 LDmEmail:TdmEnviarEmail;

begin
  LDmEmail:= TdmEnviarEmail.Create(nil);
  try
    LDmEmail.qEmailCadastro.Close;
    LDmEmail.qEmailCadastro.ParamByName('ID_EMAIL').AsInteger:= AIdEmail;
    LDmEmail.qEmailCadastro.ParamByName('ID_USUARIO').AsInteger:= AIdUsuario;
    LDmEmail.qEmailCadastro.ExecSQL;
  finally
    FreeAndNil(LDmEmail);
  end;


end;



(*
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
  LDadosEmail.Para:= 'gabriel.oliveira@tbdc.com.br';
  LDadosEmail.Assunto:= 'Teste email';
  LDadosEmail.Corpo:= LStringList.Text;

  LEmail:= TEmail.Cretae(LConfigEmail);

  LEmail.Envia(LDadosEmail);
*)

end.
