unit ProxyServer;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.ActiveX,
  FMX.Types,

  IdHTTPWebBrokerBridge,
  IdBaseComponent,
  IdScheduler,
  IdSchedulerOfThread,
  IdSchedulerOfThreadDefault,
  IdThread,
  IdContext,
  IdUDPServer,
  IdSocketHandle,
  IdGlobal,

  ProxyService,
  HDHR;

type
  TIdCOMThread = class(TIdThreadWithTask)
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
  end;

  TProxyServerModule = class(TDataModule)
    IdScheduler: TIdSchedulerOfThreadDefault;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fServer: TIdHTTPWebBrokerBridge;
    fDiscoveryServer: TIdUDPServer;

    procedure DiscoveryUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);

    procedure ServerException(AContext: TIdContext; AException: Exception);

    function GetActive: Boolean;
  public
    { Public declarations }

    procedure StartServer;
    procedure StopServer;

    property Active: Boolean read GetActive;
  end;

var
  ProxyServerModule: TProxyServerModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TIdCOMThread }

procedure TIdCOMThread.AfterExecute;
begin
  inherited;

  CoUninitialize;
end;

procedure TIdCOMThread.BeforeExecute;
begin
  Coinitialize(nil);

  inherited;
end;

{ TProxyServerModule }

procedure TProxyServerModule.DataModuleCreate(Sender: TObject);
begin
  IdScheduler.ThreadClass := TIdCOMThread;

  fServer := TIdHTTPWebBrokerBridge.Create(Self);
  fServer.Scheduler := IdScheduler;
  fServer.OnException := ServerException;

  fDiscoveryServer := TIdUDPServer.Create(Self);
  fDiscoveryServer.DefaultPort := HDHR_DISCOVERY_PORT;
  fDiscoveryServer.ThreadedEvent := False;
  fDiscoveryServer.OnUDPRead := DiscoveryUDPRead;
end;

procedure TProxyServerModule.DataModuleDestroy(Sender: TObject);
begin
  //
end;

function TProxyServerModule.GetActive: Boolean;
begin
  Result := fServer.Active;
end;

procedure TProxyServerModule.StartServer;
begin
  if not fServer.Active then
  begin
    fServer.Bindings.Clear;
    with fServer.Bindings.Add do
    begin
      Port := 80;
    end;
    with fServer.Bindings.Add do
    begin
      Port := HDHR_HTTP_PORT;
    end;
    fServer.Active := True;

    fDiscoveryServer.Active := True;
  end;
end;

procedure TProxyServerModule.StopServer;
begin
  fServer.Active := False;
  fServer.Bindings.Clear;

  fDiscoveryServer.Active := False;
  fDiscoveryServer.Bindings.Clear;
end;

procedure TProxyServerModule.ServerException(AContext: TIdContext;
  AException: Exception);
begin
  // Do nothing
end;

procedure TProxyServerModule.DiscoveryUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  lHex: AnsiString;
begin
  if Length(AData) > 0 then
  begin
    SetLength(lHex, Length(AData)*2);
    BinToHex(AData, PAnsiChar(lHex), Length(lHex));
    Log.D('Received discovery data: %s', [lHex]);
  end;
end;

end.
