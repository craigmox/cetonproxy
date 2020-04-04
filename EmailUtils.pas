unit EmailUtils;

interface

uses
  System.Classes,
  System.SysUtils,

  IdSMTP,
  IdEmailAddress,
  IdMessage,
  IdMessageBuilder,
  IdSSLOpenSSL,
  IdSSLOpenSSLHeaders,
  IdExplicitTLSClientServerBase;

{$SCOPEDENUMS ON}

const
  cEmailDefaultPort = 25;

type
  TEmailTLSOption = (
    None,
    NoTLSSupport,
    UseImplicitTLS,
    UseRequireTLS,
    UseExplicitTLS
  );
  TEmailSSLVersion = (
    None,
    SSLv2,
    SSLv23,
    SSLv3,
    TLSv1,
    TLSv1_1,
    TLSv1_2
  );

  TEmailServerSettings = class(TPersistent)
  private
    fServerPort: Integer;
    fServerAddress: String;
    fSSLVersion: TEmailSSLVersion;
    fPassword: String;
    fUsername: String;
    fTLSOption: TEmailTLSOption;
  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property ServerAddress: String read fServerAddress write fServerAddress;
    property ServerPort: Integer read fServerPort write fServerPort;
    property TLSOption: TEmailTLSOption read fTLSOption write fTLSOption;
    property SSLVersion: TEmailSSLVersion read fSSLVersion write fSSLVersion;
    property Username: String read fUsername write fUsername;
    property Password: String read fPassword write fPassword;
  end;

  TEmail = class(TPersistent)
  private
    fBody: String;
    fSubject: String;
    fRecipients: String;
    fSender: String;
  public
    property Sender: String read fSender write fSender;
    property Recipients: String read fRecipients write fRecipients;
    property Subject: String read fSubject write fSubject;
    property Body: String read fBody write fBody;
  end;

  TEmailUtils = class abstract
  public
    class procedure Send(const aSettings: TEmailServerSettings; const aEmail: TEmail); static;
  end;

implementation

{ TEmailServerSettings }

procedure TEmailServerSettings.AssignTo(Dest: TPersistent);
begin
  if Dest is TEmailServerSettings then
  begin
    TEmailServerSettings(Dest).fServerAddress := fServerAddress;
    TEmailServerSettings(Dest).fServerPort := fServerPort;
    TEmailServerSettings(Dest).fTLSOption := fTLSOption;
    TEmailServerSettings(Dest).fSSLVersion := fSSLVersion;
    TEmailServerSettings(Dest).fUsername := fUsername;
    TEmailServerSettings(Dest).fPassword := fPassword;
  end
  else
    inherited;
end;

procedure TEmailServerSettings.Assign(Source: TPersistent);
begin
  if not Assigned(Source) then
  begin
    fServerPort := cEmailDefaultPort;
  end
  else
    inherited;
end;


constructor TEmailServerSettings.Create;
begin
  Assign(nil);
end;

{ TEmailUtils }

class procedure TEmailUtils.Send(const aSettings: TEmailServerSettings;
  const aEmail: TEmail);
var
  lSMTP: TIdSMTP;
  lMessage: TIdMessage;
begin
  lSMTP := TIdSMTP.Create(nil);
  try
    lSMTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create;
    case aSettings.TLSOption of
      TEmailTLSOption.NoTLSSupport: lSMTP.UseTLS := utNoTLSSupport;
      TEmailTLSOption.UseImplicitTLS: lSMTP.UseTLS := utUseImplicitTLS;
      TEmailTLSOption.UseRequireTLS: lSMTP.UseTLS := utUseRequireTLS;
      TEmailTLSOption.UseExplicitTLS: lSMTP.UseTLS := utUseExplicitTLS;
    end;
    case aSettings.SSLVersion of
      TEmailSSLVersion.SSLv2: TIdSSLIOHandlerSocketOpenSSL(lSMTP.IOHandler).SSLOptions.Method := sslvSSLv2;
      TEmailSSLVersion.SSLv23: TIdSSLIOHandlerSocketOpenSSL(lSMTP.IOHandler).SSLOptions.Method := sslvSSLv23;
      TEmailSSLVersion.SSLv3: TIdSSLIOHandlerSocketOpenSSL(lSMTP.IOHandler).SSLOptions.Method := sslvSSLv3;
      TEmailSSLVersion.TLSv1: TIdSSLIOHandlerSocketOpenSSL(lSMTP.IOHandler).SSLOptions.Method := sslvTLSv1;
      TEmailSSLVersion.TLSv1_1: TIdSSLIOHandlerSocketOpenSSL(lSMTP.IOHandler).SSLOptions.Method := sslvTLSv1_1;
      TEmailSSLVersion.TLSv1_2: TIdSSLIOHandlerSocketOpenSSL(lSMTP.IOHandler).SSLOptions.Method := sslvTLSv1_2;
    end;

    lSMTP.Host := aSettings.ServerAddress;
    lSMTP.Port := aSettings.ServerPort;

    if aSettings.Username <> '' then
    begin
      lSMTP.AuthType := satDefault;
      lSMTP.Username := aSettings.Username;
      lSMTP.Password := aSettings.Password;
    end
    else
      lSMTP.AuthType := satNone;

    lMessage := TIdMessage.Create(nil);
    try
      lMessage.From.Text := aEmail.Sender;
      lMessage.ReplyTo.EMailAddresses := aEmail.Sender;

      lMessage.Recipients.EMailAddresses := aEmail.Recipients;

      lMessage.Subject := aEmail.Subject;

      lMessage.Body.Text := aEmail.Body;

      lMessage.Date := Now;

      lSMTP.Connect;
      lSMTP.Send(lMessage);
    finally
      lMessage.Free;
    end;
  finally
    lSMTP.Free;
  end;

end;

end.
