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
  IdIPMCastServer,
  IdIPMCastClient,
  IdTCPServer,
  IdSocketHandle,
  IdGlobal,

  ProxyServiceModuleUnit,

  Ceton,
  HDHR,
  LogUtils,
  SocketUtils;

const
  SSDP_PORT = 1900;

type
  TIdCOMThread = class(TIdThreadWithTask)
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
  end;

  TProxyServerModule = class(TDataModule, IServiceConfigEvents)
    IdScheduler: TIdSchedulerOfThreadDefault;
    ServiceTimer: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ServiceTimerTimer(Sender: TObject);
  private
    { Private declarations }
    fConfigManager: IServiceConfigManager;
    fClient: TCetonClient;

    fServer: TIdHTTPWebBrokerBridge;
    fDiscoveryServer: TIdUDPServer;
//    fSSDPServer: TIdIPMCastServer;
    fSSDPClient: TIdIPMCastClient;
//    fControlServer: TIdTCPServer;

    fRestartServers: Boolean;

    procedure DiscoveryUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);

    procedure SSDPClientRead(Sender: TObject; const AData: TIdBytes; ABinding: TIdSocketHandle);

    procedure ControlTCPConnect(aContext: TIdContext);
    procedure ControlTCPExecute(aContext: TIdContext);

    procedure ServerException(AContext: TIdContext; AException: Exception);

    function TryCreateSSDPAlivePacket(const aRequestHost: String; out aPacket: String): Boolean;

    function GetActive: Boolean;

    property ConfigManager: IServiceConfigManager read fConfigManager;
    property Client: TCetonClient read fClient;
  protected
    // IServiceConfigEvents
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
    procedure Log(const aMessage: String);
  public
    { Public declarations }

    procedure StartServer;
    procedure StopServer;

    function TryGetAddress(const aRequestLocalIP: String; out aAddress: String): Boolean;

    property Active: Boolean read GetActive;

    property DiscoveryServer: TIdUDPServer read fDiscoveryServer;
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
  fDiscoveryServer.ThreadedEvent := True;
  fDiscoveryServer.OnUDPRead := DiscoveryUDPRead;

//  fSSDPServer := TIdIPMCastServer.Create(Self);
//  fSSDPServer.MulticastGroup := '239.255.255.250';

  fSSDPClient := TIdIPMCastClient.Create(Self);
  fSSDPClient.MulticastGroup := '239.255.255.250';
  fSSDPClient.OnIPMCastRead := SSDPClientRead;
  fSSDPClient.ThreadedEvent := True;

//  fControlServer := TIdTCPServer.Create(Self);
//  fControlServer.OnConnect := ControlTCPConnect;
//  fControlServer.OnExecute := ControlTCPExecute;
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
  lLocalIPInfo: TLocalIPInfo;
begin
  if not fServer.Active then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lListenIP := lConfig.ListenIP;
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
      TLogger.Log('Unable to bind HTTP server listening port');
    end;

    try
      fDiscoveryServer.Bindings.Clear;

      if lListenIP = '' then
      begin
        // If listening on all IPs, have to explicitly create a binding for each local IP
        // so that we can properly read which IP that UDP packets come in on
        for lLocalIPInfo in TSocketUtils.GetLocalIPs do
          if lLocalIPInfo.IPVersion = 4 then
            with fDiscoveryServer.Bindings.Add do
            begin
              IP := lLocalIPInfo.IP;
              Port := HDHR_DISCOVERY_PORT;
            end;
      end
      else
      begin
        with fDiscoveryServer.Bindings.Add do
        begin
          IP := lListenIP;
          Port := HDHR_DISCOVERY_PORT;
        end;
      end;
      fDiscoveryServer.Active := True;
    except
      TLogger.Log('Unable to bind discovery listening port');
    end;

{    try
      fControlServer.Bindings.Clear;
      with fControlServer.Bindings.Add do
      begin
        IP := lListenIP;
        Port := HDHR_DISCOVERY_PORT;
      end;
      fControlServer.Active := True;
    except
      TLogger.Log('Unable to bind control listening port');
    end;}

    try
      fSSDPClient.Bindings.Clear;
      with fSSDPClient.Bindings.Add do
      begin
        IP := lListenIP;
        Port := SSDP_PORT;
      end;
      fSSDPClient.ReuseSocket := rsTrue;
      fSSDPClient.Active := True;
    except
      TLogger.Log('Unable to bind SSDP listening port');
    end;

{    try
      fSSDPServer.BoundIP := lListenIP;
      fSSDPServer.BoundPort := SSDP_PORT;
      fSSDPServer.Active := True;
    except
      TLogger.Log('Unable to bind SSDP listening port');
    end;}
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

{  try
    fControlServer.Active := False;
    fControlServer.Bindings.Clear;
  except
    // Ignore
  end;}

  try
    fSSDPClient.Active := False;
    fSSDPClient.Bindings.Clear;
  except
    // Ignore
  end;

{  try
    fSSDPServer.Active := False;
  except
    // Ignore
  end;}
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
  lAddress: String;
  lDeviceID: UInt32;
  lTunerCount: Integer;
begin
  if Length(AData) > 0 then
  begin
    SetLength(lHex, Length(AData)*2);
    BinToHex(AData, PAnsiChar(lHex), Length(AData));
    TLogger.LogFmt('Received control data from %s on %s: %s', [ABinding.PeerIP, ABinding.IP, lHex]);

    if TPacket.TryFromBytes(TBytes(AData), lPacket) then
    begin
      if lPacket.IsValid then
      begin
        case lPacket._Type of
          HDHOMERUN_TYPE_DISCOVER_REQ: begin
            lDiscovery := lPacket.ToDiscovery;
            TLogger.LogFmt('Received discovery request: Device type: %d, Device ID: %s', [lDiscovery.DeviceType, IntToHex(lDiscovery.DeviceID, 8)]);

            ConfigManager.LockConfig(lConfig);
            try
              lDeviceID := lConfig.DeviceID;
            finally
              ConfigManager.UnlockConfig(lConfig);
            end;

            if ((lDiscovery.DeviceType = HDHOMERUN_DEVICE_TYPE_TUNER) or (lDiscovery.DeviceType = HDHOMERUN_DEVICE_TYPE_WILDCARD)) and ((lDiscovery.DeviceID = HDHOMERUN_DEVICE_ID_WILDCARD) or (lDiscovery.DeviceID = lDeviceID)) then
            begin
              if TryGetAddress(ABinding.IP, lAddress) then
              begin
                lTunerCount := Client.TunerCount;

                ConfigManager.LockConfig(lConfig);
                try
                  lDiscovery.DeviceID := lConfig.DeviceID;
                  lDiscovery.TunerCount := lTunerCount;
                  lDiscovery.DeviceAuthStr := 'abcdefg';
                  lDiscovery.BaseURL := AnsiString(Format('http://%s:%d', [lAddress, lConfig.ExternalHTTPPort]));
                  lDiscovery.LineupURL := AnsiString(Format('%s/lineup.json', [lDiscovery.BaseURL]));
                finally
                  ConfigManager.UnlockConfig(lConfig);
                end;

                lBytes := TPacket.FromDiscovery(False, lDiscovery).ToBytes;
                SetLength(lHex, Length(lBytes)*2);
                BinToHex(lBytes, PAnsiChar(lHex), Length(lBytes));
                TLogger.LogFmt('Sending discovery response: Device ID: %s, Base URL: %s, %s', [IntToHex(lDiscovery.DeviceID, 8), lDiscovery.BaseURL, lHex]);

                AThread.Server.SendBuffer(ABinding.PeerIP, ABinding.PeerPort, TIdBytes(lBytes));
              end;
            end;
          end;
          HDHOMERUN_TYPE_DISCOVER_RPY: begin
            lDiscovery := lPacket.ToDiscovery;
            TLogger.LogFmt('Received discovery reply: Device type: %d, Device ID: %s', [lDiscovery.DeviceType, IntToHex(lDiscovery.DeviceID, 8)]);
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

procedure TProxyServerModule.ServiceTimerTimer(Sender: TObject);
begin
  if fRestartServers then
  begin
    fRestartServers := False;

    StopServer;
    StartServer;
  end;

//  fSSDPServer.Send(CreateSSDPAlivePacket);
end;

function TProxyServerModule.TryGetAddress(const aRequestLocalIP: String; out aAddress: String): Boolean;
var
  lConfig: TServiceConfig;
  lAddresses: TLocalIPInfoArray;
  lModel: TCetonModel;
begin
  ConfigManager.LockConfig(lConfig);
  try
    aAddress := lConfig.ExternalAddress;
    if aAddress <> '' then
      Exit(True);

    aAddress := lConfig.ListenIP;
    if aAddress <> '' then
      Exit(True);
  finally
    ConfigManager.UnlockConfig(lConfig);
  end;

  // Normally we would respond to UPnP or discovery requests with the
  // local IP that we received the request on, because we know for sure
  // the remote host is able to reach us on that.

  // However, if the request is coming from an app on the same computer
  // as this app, we may get multiple requests from them on different local
  // IPs.  To avoid confusing the app, we must respond with the same
  // BaseURL to all of those requests.  e.g.  NextPVR gets confused if it receives
  // different discovery responses with the same DeviceID, but different
  // BaseURLs.

  // We can detect if the request is coming from the same computer as this
  // app by checking if the PeerIP is found in the local address list.

  lAddresses := TSocketUtils.GetLocalIPs;

  if (lAddresses.IndexOf(ARequestLocalIP) > -1) then
  begin
    if ProxyServiceModule.Client.TunerCount = 0 then
      Exit(False);

    lModel := ProxyServiceModule.Client.Model;
    if lModel <> TCetonModel.Ethernet then
    begin
      // If it's a USB/PCI device, exclude the local ip that was created by
      // the Ceton device
      lAddresses := lAddresses.Remove(ProxyServiceModule.Client.ListenIP);
    end;

    aAddress := lAddresses.LowestMetric(4).IP;
    Result := not SameText(aRequestLocalIP, aAddress);
  end
  else
  begin
    aAddress := aRequestLocalIP;
    Result := aAddress <> '';
  end;
end;

procedure TProxyServerModule.ControlTCPConnect(aContext: TIdContext);
begin
  TLogger.Log('Control connect');
end;

procedure TProxyServerModule.ControlTCPExecute(aContext: TIdContext);
begin
  TLogger.Log('Control execute');
end;

//  Received SSDP data from 192.168.1.89: M-SEARCH * HTTP/1.1
//  MX: 5
//  ST: upnp:rootdevice
//  MAN: "ssdp:discover"
//  User-Agent: Linux/3.0.13 UPnP/1.0 LGE_DLNA_SDK/1.6.0 [TV][LG]55LA7400-UD/05.09.11 DLNADOC/1.50
//  DLNADeviceName.lge.com: %5bTV%5d%5bLG%5d55LA7400-UD
//  Connection: close
//  Host: 239.255.255.250:1900

procedure TProxyServerModule.SSDPClientRead(Sender: TObject;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  lData: UTF8String;
  lPacket: String;
begin
  if Length(AData) > 0 then
  begin
    SetLength(lData, Length(AData));
    Move(AData[0], lData[Low(lData)], Length(AData));

    // TODO: Make more robust
    if String(lData).StartsWith('M-SEARCH', True) then
    begin
      if String(lData).Contains('ST: upnp:rootdevice') then
      begin
        TLogger.LogFmt('Received M-SEARCH for rootdevice from %s on %s', [ABinding.PeerIP, ABinding.IP]);

        if TryCreateSSDPAlivePacket(ABinding.IP, lPacket) then
          fDiscoveryServer.Send(ABinding.PeerIP, ABinding.PeerPort, lPacket);
      end;
    end;
  end;
end;

//  Received SSDP data from 192.168.1.43: NOTIFY * HTTP/1.1
//  Host: 239.255.255.250:1900
//  NT: upnp:rootdevice
//  NTS: ssdp:alive
//  Server: HDHomeRun/1.0 UPnP/1.0
//  Location: http://192.168.1.43:80/dri/device.xml
//  Cache-Control: max-age=1800
//  USN: uuid:473366D2-A765-3D61-B466-XXXXX::upnp:rootdevice

function TProxyServerModule.TryCreateSSDPAlivePacket(const aRequestHost: String; out aPacket: String): Boolean;
const
  cSSDPAlive =
    'NOTIFY * HTTP/1.1'#13#10+
    'Host: 239.255.255.250:1900'#13#10+
    'NT: upnp:rootdevice'#13#10+
    'NTS: ssdp:alive'#13#10+
    'Server: HDHomeRun/1.0 UPnP/1.0'#13#10+
    'Location: http://%s:%d/device.xml'#13#10+
    'Cache-Control: max-age=1800'#13#10+
    'USN: uuid:%s::upnp:rootdevice'#13#10#13#10;
var
  lAddress: String;
  lPort: Integer;
  lDeviceUUID: String;
  lConfig: TServiceConfig;
begin
  if TryGetAddress(ARequestHost, lAddress) then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lPort := lConfig.ExternalHTTPPort;
      lDeviceUUID := lConfig.DeviceUUID;
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    aPacket := Format(cSSDPAlive, [lAddress, lPort, lDeviceUUID]);
    Result := True;
  end
  else
    Result := False;
end;

procedure TProxyServerModule.Log(const aMessage: String);
begin
  // Nothing
end;

end.
