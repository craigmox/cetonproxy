unit ProxyServerModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.DateUtils,
  Winapi.ActiveX,
  FMX.Types,
  FMX.Dialogs,

  REST.Client,
  REST.Types,

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
  IdUDPClient,

  ProxyServiceModuleUnit,

  Ceton,
  HDHR,
  LogUtils,
  SocketUtils;

const
  SSDP_MULTICAST_GROUP = '239.255.255.250';
  SSDP_PORT = 1900;

  cSSDPAliveIntervalSec = 1800;

type
  TIdCOMThread = class(TIdThreadWithTask)
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
  end;

  TProxyServerModule = class(TDataModule, IServiceConfigEvents)
    IdScheduler: TIdSchedulerOfThreadDefault;
    RestartServersTimer: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure RestartServersTimerTimer(Sender: TObject);
  private
    { Private declarations }
    fConfigManager: IServiceConfigManager;
    fClient: TCetonClient;

    fServer: TIdHTTPWebBrokerBridge;
    fDiscoveryServer: TIdUDPServer;
    fSSDPClient: TIdIPMCastClient;
    fSSDPServer: TIdUDPServer;
//    fControlServer: TIdTCPServer;

    procedure DiscoveryUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);

    procedure SSDPClientRead(Sender: TObject; const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure SSDPServerRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);

    procedure ControlTCPConnect(aContext: TIdContext);
    procedure ControlTCPExecute(aContext: TIdContext);

    procedure ServerException(AContext: TIdContext; AException: Exception);

    function CreateSSDPDiscoverPacket: String;
    function TryCreateSSDPResponsePacket(const aRequestHost: String; out aPacket: String): Boolean;
    function TryCreateSSDPAlivePacket(const aRequestHost: String; out aPacket: String): Boolean;

    procedure DiscoverCetonDevices;

    function GetActive: Boolean;

    property ConfigManager: IServiceConfigManager read fConfigManager;
    property Client: TCetonClient read fClient;
  protected
    // IServiceConfigEvents
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
    procedure Log(const aLogName: String; const aMessage: String);
    procedure LogError(const aLogName: String; const aMessage: String);
    procedure DiscoveredCetonDevicesChanged;
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

  // Used to respond to UPnP discovery requests for HDHomeRun and listen
  // for "alive" messages
  fSSDPClient := TIdIPMCastClient.Create(Self);
  fSSDPClient.MulticastGroup := SSDP_MULTICAST_GROUP;
  fSSDPClient.OnIPMCastRead := SSDPClientRead;
  fSSDPClient.ThreadedEvent := True;

  // Used to discover Ceton devices
  fSSDPServer := TIdUDPServer.Create(Self);
  fSSDPServer.ThreadedEvent := True;
  fSSDPServer.OnUDPRead := SSDPServerRead;

//  fControlServer := TIdTCPServer.Create(Self);
//  fControlServer.OnConnect := ControlTCPConnect;
//  fControlServer.OnExecute := ControlTCPExecute;
end;

procedure TProxyServerModule.DataModuleDestroy(Sender: TObject);
begin
  fConfigManager.RemoveListener(Self);

  fSSDPClient.Active := False;
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
  lLocalIPs: TLocalIPInfoArray;
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

    lLocalIPs := TSocketUtils.GetLocalIPs;
    // FastMM detected memory corruption unless separated into 2 assignments
    lLocalIPs := lLocalIPs.Keep(4);

    try
      fServer.Bindings.Clear;
      with fServer.Bindings.Add do
      begin
        IP := lListenIP;
        Port := lHTTPPort;
      end;
      fServer.Active := True;
      // Needed by NextPVR to find lineup.xml
      with fServer.Bindings.Add do
      begin
        IP := lListenIP;
        Port := 80;
      end;
    except
      TLogger.Log(cLogDefault, 'Unable to bind HTTP server listening port');
    end;

    try
      fDiscoveryServer.Bindings.Clear;

      if lListenIP = '' then
      begin
        // If listening on all IPs, have to explicitly create a binding for each local IP
        // so that we can properly read which IP that UDP packets come in on
        for lLocalIPInfo in lLocalIPs do
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
      TLogger.Log(cLogDefault, 'Unable to bind discovery listening port');
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
      TLogger.Log(cLogDefault, 'Unable to bind control listening port');
    end;}

    try
      fSSDPClient.Bindings.Clear;

      if lListenIP = '' then
      begin
        // If listening on all IPs, have to explicitly create a binding for each local IP
        // so that we can properly read which IP that UDP packets come in on
        for lLocalIPInfo in lLocalIPs do
          with fSSDPClient.Bindings.Add do
          begin
            IP := lLocalIPInfo.IP;
            Port := SSDP_PORT;
          end;
      end
      else
      begin
        with fSSDPClient.Bindings.Add do
        begin
          IP := lListenIP;
          Port := SSDP_PORT;
        end;
      end;

      fSSDPClient.ReuseSocket := rsTrue;
      fSSDPClient.Active := True;
    except
      TLogger.Log(cLogDefault, 'Unable to bind SSDP listening port');
    end;

    try
      fSSDPServer.Bindings.Clear;
      for lLocalIPInfo in lLocalIPs do
        with fSSDPServer.Bindings.Add do
        begin
          IP := lLocalIPInfo.IP;
          Port := 0;
        end;
      fSSDPServer.Active := True;
    except
      TLogger.Log(cLogDefault, 'Unable to create SSDP server');
    end;

    // Send broadcast for UPnP devices
    DiscoverCetonDevices;
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

  try
    fSSDPServer.Active := False;
    fSSDPServer.Bindings.Clear;
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
  lAddress: String;
  lDeviceID: UInt32;
  lTunerCount: Integer;
begin
  if Length(AData) > 0 then
  begin
    SetLength(lHex, Length(AData)*2);
    BinToHex(AData, PAnsiChar(lHex), Length(AData));
    TLogger.LogFmt(cLogDiscovery,'Received control data from %s on %s: %s', [ABinding.PeerIP, ABinding.IP, lHex]);

    if TPacket.TryFromBytes(TBytes(AData), lPacket) then
    begin
      if lPacket.IsValid then
      begin
        case lPacket._Type of
          HDHOMERUN_TYPE_DISCOVER_REQ: begin
            lDiscovery := lPacket.ToDiscovery;
            TLogger.LogFmt(cLogDiscovery,'Received discovery request: Device type: %d, Device ID: %s', [lDiscovery.DeviceType, IntToHex(lDiscovery.DeviceID, 8)]);

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
                lTunerCount := Client.EnabledTunerCount;

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
                TLogger.LogFmt(cLogDiscovery,'Sending discovery response: Device ID: %s, Base URL: %s, %s', [IntToHex(lDiscovery.DeviceID, 8), lDiscovery.BaseURL, lHex]);

                AThread.Server.SendBuffer(ABinding.PeerIP, ABinding.PeerPort, TIdBytes(lBytes));
              end;
            end;
          end;
          HDHOMERUN_TYPE_DISCOVER_RPY: begin
            lDiscovery := lPacket.ToDiscovery;
            TLogger.LogFmt(cLogDiscovery,'Received discovery reply: Device type: %d, Device ID: %s', [lDiscovery.DeviceType, IntToHex(lDiscovery.DeviceID, 8)]);
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
      if TServiceConfigSection.Server in aSections then
      begin
        RestartServersTimer.Enabled := True;
      end;
    end);
end;

procedure TProxyServerModule.RestartServersTimerTimer(Sender: TObject);
begin
  RestartServersTimer.Enabled := False;

  StopServer;
  StartServer;
end;

function TProxyServerModule.TryGetAddress(const aRequestLocalIP: String; out aAddress: String): Boolean;
var
  lConfig: TServiceConfig;
  lAddresses: TLocalIPInfoArray;
  lModel: TCetonModel;
begin
  // The goal here is to return an IP address that the remote end will be able to reach
  // us on as an HDHomeRun.  The easiest thing to do is take whatever IP the remote already
  // used to reach us as the IP address it should continue to use, but of course it's not
  // that easy.

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
  // app by checking if the ARequestLocalIP is found in the local address list.

  lAddresses := TSocketUtils.GetLocalIPs;

  if (lAddresses.IndexOf(ARequestLocalIP) > -1) then
  begin
    if ProxyServiceModule.Client.EnabledTunerCount = 0 then
      Exit(False);

    // If it's a USB/PCI device, exclude the local ip that was created by
    // the Ceton device
    // TODO: This is not a good assumption to make if it's a USB/PCI device
    // and set to bridged mode.  If there is only one local IP address,
    // don't remove it.
    lModel := ProxyServiceModule.Client.Model;
    if (lModel <> TCetonModel.Ethernet) and (Length(lAddresses) > 1) then
      lAddresses := lAddresses.Remove(ProxyServiceModule.Client.ListenIP);

    // Choose a local IP that we should respond to
    aAddress := lAddresses.LowestMetric(4).IP;
    // Return whether the local ip that the request came in on is equal to that chosen IP
    Result := SameText(aRequestLocalIP, aAddress);
  end
  else
  begin
    aAddress := aRequestLocalIP;
    Result := aAddress <> '';
  end;
end;

procedure TProxyServerModule.ControlTCPConnect(aContext: TIdContext);
begin
  TLogger.Log(cLogDefault, 'Control connect');
end;

procedure TProxyServerModule.ControlTCPExecute(aContext: TIdContext);
begin
  TLogger.Log(cLogDefault, 'Control execute');
end;

{Description: Ceton InfiniTV MOCUR (00-00-22-00-00-XX-XX-XX)
DeviceType: urn:schemas-cetoncorp-com:device:SecureContainer:1
FriendlyName: Ceton InfiniTV Ethernet (00-XX-XX-XX)
Manufacturer: Ceton Corporation
ManufacturerUrl: http://www.cetoncorp.com/
ModelName: Ceton InfiniTV MOCUR (00-00-22-00-00-XX-XX-XX)
ModelNumber: 0.0.0.9
PresentationUrl: http://192.168.1.132/Services/System.html
SerialNumber: 00-00-22-00-00-XX-XX-XX
UDN: uuid:XXX-XXX-XXX
}
{Process cetonproxy.exe (25300)
Debug Output:
[2020-03-08 14:29:28.305] Received NOTIFY from 192.168.1.132 on 192.168.1.8: NOTIFY * HTTP/1.1
HOST: 239.255.255.250:1900
CACHE-CONTROL: max-age=1800
LOCATION: http://192.168.1.132/description.xml
NT: uuid:89333102-EBE5-11D8-AC9A-000008098100
NTS: ssdp:alive
SERVER: Linux/3.0.1+, UPnP/1.0
USN: uuid:XXX-XXX-XXX
}
{Process cetonproxy.exe (25300)
Debug Output:
[2020-03-08 14:29:28.302] Received NOTIFY from 192.168.1.132 on 192.168.1.8: NOTIFY * HTTP/1.1
HOST: 239.255.255.250:1900
CACHE-CONTROL: max-age=1800
LOCATION: http://192.168.1.132/description.xml
NT: upnp:rootdevice
NTS: ssdp:alive
SERVER: Linux/3.0.1+, UPnP/1.0
USN: uuid:XXX-XXX-XXX::upnp:rootdevice
}
{Debug Output:
[2020-03-08 14:29:28.309] Received NOTIFY from 192.168.1.132 on 192.168.1.116: NOTIFY * HTTP/1.1
HOST: 239.255.255.250:1900
CACHE-CONTROL: max-age=1800
LOCATION: http://192.168.1.132/description.xml
NT: upnp:rootdevice
NTS: ssdp:alive
SERVER: Linux/3.0.1+, UPnP/1.0
USN: uuid:XXX-XXX-XXX::upnp:rootdevice
}
{[2020-03-08 14:29:28.313] Received NOTIFY from 192.168.1.132 on 192.168.1.8: NOTIFY * HTTP/1.1
HOST: 239.255.255.250:1900
CACHE-CONTROL: max-age=1800
LOCATION: http://192.168.1.132/description.xml
NT: urn:schemas-cetoncorp-com:device:SecureContainer:1
NTS: ssdp:alive
SERVER: Linux/3.0.1+, UPnP/1.0
USN: uuid:XXX-XXX-XXX::urn:schemas-cetoncorp-com:device:SecureContainer:1
}
{Debug Output:
[2020-03-08 14:51:25.922] Received M-SEARCH from 192.168.1.10 on 192.168.1.8: M-SEARCH * HTTP/1.1
Host: 239.255.255.250:1900
ST: urn:schemas-cetoncorp-com:device:SecureContainer:1
Man: "ssdp:discover"
MX: 3
}
{Debug Output:
[2020-03-08 14:51:26.021] Received M-SEARCH from 192.168.1.10 on 192.168.1.8: M-SEARCH * HTTP/1.1
Host: 239.255.255.250:1900
ST: uuid:89333102-EBE5-11D8-AC9A-000008085F10
Man: "ssdp:discover"
MX: 3
}

function TProxyServerModule.CreateSSDPDiscoverPacket: String;
const
  cSSDPDiscover =
    'M-SEARCH * HTTP/1.1'#13#10+
    'Host: 239.255.255.250:1900'#13#10+
//    'ST: ssdp:all'#13#10+
    'ST: urn:schemas-cetoncorp-com:device:SecureContainer:1'#13#10+
//    'ST: upnp:rootdevice'#13#10+
    'Man: "ssdp:discover"'#13#10+
    'MX: 3'#13#10#13#10;
begin
  Result := Format(cSSDPDiscover, []);
end;

procedure TProxyServerModule.SSDPClientRead(Sender: TObject;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  lData: UTF8String;
  lPacket: String;
  lValues: TStringList;
  lLocation: String;
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
        TLogger.LogFmt(cLogDiscovery, 'Received M-SEARCH for rootdevice from %s on %s', [ABinding.PeerIP, ABinding.IP]);

        if TryCreateSSDPResponsePacket(ABinding.IP, lPacket) then
          fDiscoveryServer.Send(ABinding.PeerIP, ABinding.PeerPort, lPacket);
      end;
    end
    else if String(lData).StartsWith('NOTIFY', True) then
    begin
      if String(lData).ToLower.Contains('cetoncorp') then
      begin
        TLogger.LogFmt(cLogDiscovery, 'Received NOTIFY from ceton device %s on %s: %s', [ABinding.PeerIP, ABinding.IP, String(lData)]);

        lValues := TStringList.Create;
        try
          lValues.CaseSensitive := False;
          lValues.NameValueSeparator := ':';
          lValues.Text := String(lData);

          lLocation := Trim(lValues.Values['LOCATION']);
          if lLocation <> '' then
            ProxyServiceModule.CetonDeviceDiscovered(ABinding.PeerIP, lLocation);
        finally
          lValues.Free;
        end;
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

procedure TProxyServerModule.Log(const aLogName: String; const aMessage: String);
begin
  // Nothing
end;

procedure TProxyServerModule.DiscoverCetonDevices;
var
  i: Integer;
begin
  if fSSDPServer.Active then
  begin
    ProxyServiceModule.DiscoveredCetonDeviceList.DiscoveryStarted;

    for i := 0 to fSSDPServer.Bindings.Count-1 do
      fSSDPServer.Bindings[i].SendTo(SSDP_MULTICAST_GROUP, SSDP_PORT, CreateSSDPDiscoverPacket);
  end;
end;

{Debug Output:
[2020-03-08 21:09:18.964] Ceton Incoming from 192.168.1.116: HTTP/1.1 200 OK
CACHE-CONTROL: max-age=300
DATE: Thu, 15 Jan 1970 03:19:09 GMT
EXT:
LOCATION: http://192.168.1.132/description.xml
SERVER: Linux/3.0.1+, UPnP/1.0
ST: urn:schemas-cetoncorp-com:device:SecureContainer:1
USN: uuid:XXX-XXX-XXX::urn:schemas-cetoncorp-com:device:SecureContainer:1}

procedure TProxyServerModule.SSDPServerRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  lData: UTF8String;
  lValues: TStringList;
  lLocation: String;
begin
  if Length(AData) > 0 then
  begin
    SetLength(lData, Length(AData));
    Move(AData[0], lData[Low(lData)], Length(AData));

    TLogger.LogFmt(cLogDiscovery, 'Received SSDP discovery response from %s on %s: %s', [ABinding.PeerIP, ABinding.IP, String(lData)]);

    lValues := TStringList.Create;
    try
      lValues.CaseSensitive := False;
      lValues.NameValueSeparator := ':';
      lValues.Text := String(lData);

      if Trim(lValues.Values['ST']).ToLower.Contains('cetoncorp') then
      begin
        lLocation := Trim(lValues.Values['LOCATION']);

        ProxyServiceModule.CetonDeviceDiscovered(ABinding.PeerIP, lLocation);
      end;
    finally
      lValues.Free;
    end;
  end;
end;

procedure TProxyServerModule.DiscoveredCetonDevicesChanged;
begin
  // Nothing
end;

function DateTimeToRFC1123(aDate: TDateTime): string;
const
  StrWeekDay: string = 'MonTueWedThuFriSatSun';
  StrMonth: string = 'JanFebMarAprMayJunJulAugSepOctNovDec';
var
  Year, Month, Day: Word;
  Hour, Min, Sec, MSec: Word;
  DayOfWeek: Word;
begin
  DecodeDate(aDate, Year, Month, Day);
  DecodeTime(aDate, Hour, Min, Sec, MSec);
  DayOfWeek := ((Trunc(aDate) - 2) mod 7);
  Result    := Copy(StrWeekDay, 1 + DayOfWeek * 3, 3) + ', ' +
    Format('%2.2d %s %4.4d %2.2d:%2.2d:%2.2d',
    [Day, Copy(StrMonth, 1 + 3 * (Month - 1), 3),
    Year, Hour, Min, Sec]);
end;

{
HTTP/1.1 200 OK
Server: HDHomeRun/1.0 UPnP/1.0
ST: upnp:rootdevice
Location: http://192.168.1.43:80/dri/device.xml
Cache-Control: max-age=1800
USN: uuid:473366D2-A765-3D61-B466-E73B254C632B::upnp:rootdevice
Ext:
Content-Length: 0
Date: Wed, 11 Mar 2020 21:34:52 GMT
}

function TProxyServerModule.TryCreateSSDPResponsePacket(
  const aRequestHost: String; out aPacket: String): Boolean;
const
  cSSDPResponse =
    'Server: HDHomeRun/1.0 UPnP/1.0'#13#10+
    'ST: upnp:rootdevice'#13#10+
    'Location: http://%s:%d/device.xml'#13#10+
    'Cache-Control: max-age=%d'#13#10+
    'USN: uuid:%s::upnp:rootdevice'#13#10+
    'Ext:'#13#10+
    'Content-Length: 0'#13#10+
    'Date: %s'#13#10#13#10;
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

    aPacket := Format(cSSDPResponse, [lAddress, lPort, cSSDPAliveIntervalSec+30, lDeviceUUID, DateTimeToRFC1123(TTimeZone.Local.ToUniversalTime(Now))+' GMT']);
    Result := True;
  end
  else
    Result := False;
end;

procedure TProxyServerModule.LogError(const aLogName, aMessage: String);
begin
  // Nothing
end;

end.
