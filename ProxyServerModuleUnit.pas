unit ProxyServerModuleUnit;

interface

uses
  FastMM4,

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

  ProxyServiceModuleUnit,

  Ceton,
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

    procedure GetConfig(const aConfig: TServiceConfig);

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
var
  lConfig: TServiceConfig;
begin
  if not fServer.Active then
  begin
    lConfig := TServiceConfig.Create;
    try
      lConfig.Ceton.ChannelMap.Exclude := lConfig.Ceton.ChannelMap.Exclude + [TChannelMapSection.Items];
      GetConfig(lConfig);

      fServer.Bindings.Clear;
      with fServer.Bindings.Add do
      begin
        IP := lConfig.Ceton.ListenIP;
        Port := 80;
      end;
      with fServer.Bindings.Add do
      begin
        IP := lConfig.Ceton.ListenIP;
        Port := HDHR_HTTP_PORT;
      end;
      fServer.Active := True;

      fDiscoveryServer.Bindings.Clear;
      with fDiscoveryServer.Bindings.Add do
      begin
        IP := lConfig.Ceton.ListenIP;
        Port := HDHR_DISCOVERY_PORT;
      end;
      fDiscoveryServer.Active := True;
    finally
      lConfig.Free;
    end;
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
  lPacket: TPacket;
  lDiscovery: TDiscoveryData;
  lConfig: TServiceConfig;
  lBytes: TBytes;
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
              lConfig := TServiceConfig.Create;
              try
                lConfig.Ceton.ChannelMap.Exclude := lConfig.Ceton.ChannelMap.Exclude + [TChannelMapSection.Items];
                GetConfig(lConfig);

                lDiscovery.DeviceID := lConfig.DeviceID;
                lDiscovery.TunerCount := 6;
                lDiscovery.BaseURL := AnsiString(Format('http://%s:80', [lConfig.Ceton.ListenIP]));
                lDiscovery.LineupURL := AnsiString(Format('%s/lineup.json', [lDiscovery.BaseURL]));
              finally
                lConfig.Free;
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

procedure TProxyServerModule.GetConfig(const aConfig: TServiceConfig);
begin
  ProxyServiceModule.GetConfig(aConfig);
end;

end.
