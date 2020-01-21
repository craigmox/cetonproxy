unit ProxyServerModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.ActiveX,
  FMX.Types,
  FMX.Dialogs,

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

  ProxyServiceModuleUnit,

  Ceton,
  HDHR;

type
  TIdCOMThread = class(TIdThreadWithTask)
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
  end;

  TProxyServerModule = class(TDataModule, IServiceConfigEvents)
    IdScheduler: TIdSchedulerOfThreadDefault;
    ConfigTimer: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ConfigTimerTimer(Sender: TObject);
  private
    { Private declarations }
    fConfigManager: IServiceConfigManager;
    fClient: TCetonClient;

    fServer: TIdHTTPWebBrokerBridge;
    fDiscoveryServer: TIdUDPServer;

    fRestartServers: Boolean;

    procedure DiscoveryUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);

    procedure ServerException(AContext: TIdContext; AException: Exception);

    function GetActive: Boolean;

    property ConfigManager: IServiceConfigManager read fConfigManager;
    property Client: TCetonClient read fClient;
  protected
    // IServiceConfigEvents
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
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

  fConfigManager := ProxyServiceModule.ConfigManager;
  fConfigManager.AddListener(Self);

  fClient := ProxyServiceModule.Client;

  fServer := TIdHTTPWebBrokerBridge.Create(Self);
  fServer.Scheduler := IdScheduler;
  fServer.OnException := ServerException;

  fDiscoveryServer := TIdUDPServer.Create(Self);
  fDiscoveryServer.ThreadedEvent := False;
  fDiscoveryServer.OnUDPRead := DiscoveryUDPRead;
end;

procedure TProxyServerModule.DataModuleDestroy(Sender: TObject);
begin
  fConfigManager.RemoveListener(Self);
end;

function TProxyServerModule.GetActive: Boolean;
begin
  Result := fServer.Active;
end;

procedure TProxyServerModule.StartServer;
var
  lConfig: TServiceConfig;
  lListenIP: String;
  lHTTPPort: Integer;
begin
  if not fServer.Active then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lListenIP := lConfig.Ceton.ListenIP;
      lHTTPPort := lConfig.HTTPPort;
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    try
      fServer.Bindings.Clear;
      // Needed by NextPVR to find lineup.xml
      with fServer.Bindings.Add do
      begin
        IP := lListenIP;
        Port := 80;
      end;
      with fServer.Bindings.Add do
      begin
        IP := lListenIP;
        Port := lHTTPPort;
      end;
      fServer.Active := True;
    except
      Log.d('Unable to bind HTTP server listening port');
    end;

    try
      fDiscoveryServer.Bindings.Clear;
      with fDiscoveryServer.Bindings.Add do
      begin
        IP := lListenIP;
        Port := HDHR_DISCOVERY_PORT;
      end;
      fDiscoveryServer.Active := True;
    except
      Log.d('Unable to bind discovery listening port');
    end;
  end;
end;

procedure TProxyServerModule.StopServer;
begin
  try
    fServer.Active := False;
    fServer.Bindings.Clear;
  except
    // Ignore
  end;

  try
    fDiscoveryServer.Active := False;
    fDiscoveryServer.Bindings.Clear;
  except
    // Ignore
  end;
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
  lPacket: TPacket;
  lDiscovery: TDiscoveryData;
  lConfig: TServiceConfig;
  lBytes: TBytes;
  lListenIP: String;
begin
  if Length(AData) > 0 then
  begin
    SetLength(lHex, Length(AData)*2);
    BinToHex(AData, PAnsiChar(lHex), Length(AData));
    Log.D('Received control data: %s', [lHex]);

    if TPacket.TryFromBytes(TBytes(AData), lPacket) then
    begin
      if lPacket.IsValid then
      begin
        case lPacket._Type of
          HDHOMERUN_TYPE_DISCOVER_REQ: begin
            lDiscovery := lPacket.ToDiscovery;
            Log.D('Received discovery request: Device type: %d, Device ID: %s', [lDiscovery.DeviceType, IntToHex(lDiscovery.DeviceID, 8)]);

            if (lDiscovery.DeviceType = HDHOMERUN_DEVICE_TYPE_TUNER) and (lDiscovery.DeviceID = HDHOMERUN_DEVICE_ID_WILDCARD) then
            begin
              lListenIP := Client.ListenIP;

              ConfigManager.LockConfig(lConfig);
              try
                lDiscovery.DeviceID := lConfig.DeviceID;
                lDiscovery.TunerCount := 6;
                lDiscovery.BaseURL := AnsiString(Format('http://%s:%d', [lListenIP, lConfig.HTTPPort]));
                lDiscovery.LineupURL := AnsiString(Format('%s/lineup.json', [lDiscovery.BaseURL]));
              finally
                ConfigManager.UnlockConfig(lConfig);
              end;

              lBytes := TPacket.FromDiscovery(False, lDiscovery).ToBytes;
              SetLength(lHex, Length(lBytes)*2);
              BinToHex(lBytes, PAnsiChar(lHex), Length(lBytes));
              Log.D('Sending discovery response: Device ID: %s, Base URL: %s, %s', [IntToHex(lDiscovery.DeviceID, 8), lDiscovery.BaseURL, lHex]);

              AThread.Server.SendBuffer(ABinding.PeerIP, ABinding.PeerPort, TIdBytes(lBytes));
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TProxyServerModule.Changed(const aSender: TObject;
  const aSections: TServiceConfigSections);
begin
  TThread.ForceQueue(nil,
    procedure()
    begin
      if TServiceConfigSection.Other in aSections then
      begin
        fRestartServers := True;
      end;
    end);
end;

procedure TProxyServerModule.ConfigTimerTimer(Sender: TObject);
begin
  if fRestartServers then
  begin
    fRestartServers := False;

    StopServer;
    StartServer;
  end;
end;

end.
