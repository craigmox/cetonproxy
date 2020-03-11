unit ProxyServiceModuleUnit;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.DateUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SyncObjs,
  System.Diagnostics,
  System.StrUtils,
  FMX.Types,

  Winapi.ActiveX,

  REST.Client,
  REST.Types,
  REST.Json,
  REST.Json.Types,
  REST.JsonReflect,

  Xml.XmlDoc,
  Xml.XmlIntf,

  IdStack,

  HDHR,
  Ceton,
  LogUtils,
  FileUtils;

const
  cServiceConfigVersion = 2;

  cLogSizeRollover = 2000000;
  cMaxLogFiles = 5;

type
  TServiceConfigSection = (Channels, Other);
  TServiceConfigSections = set of TServiceConfigSection;

  TServiceConfig = class(TPersistent)
  private
    fDeviceID: UInt32;
    fCeton: TCetonConfig;
    fListenIP: String;
    fHTTPPort: Integer;
    fDeviceUUID: String;
    fExternalAddress: String;
    fExternalHTTPPort: Integer;
    fVersion: Integer;

    procedure CreateDeviceID;
    procedure CreateDeviceUUID;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignTo(Dest: TPersistent); override;

    function ToJSON: String;
    class function FromJSON(const aJSON: String): TServiceConfig; static;

    property Ceton: TCetonConfig read fCeton;

    property Version: Integer read fVersion write fVersion;
    property DeviceID: UInt32 read fDeviceID write fDeviceID;
    property DeviceUUID: String read fDeviceUUID write fDeviceUUID;
    property ListenIP: String read fListenIP write fListenIP;
    property HTTPPort: Integer read fHTTPPort write fHTTPPort;
    property ExternalAddress: String read fExternalAddress write fExternalAddress;
    property ExternalHTTPPort: Integer read fExternalHTTPPort write fExternalHTTPPort;
  end;

  IServiceConfigEvents = interface
    ['{E51631F5-FC88-4FEC-BCF6-9A0F5616CE79}']
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
    procedure DiscoveredCetonDevicesChanged;
    procedure Log(const aLogName: String; const aMessage: String);
  end;

  IServiceConfigManager = interface
    ['{4393C786-9DE0-42DE-BE36-21EA00CFBE70}']
    procedure LockConfig(out aConfig: TServiceConfig);
    procedure UnlockConfig(var aConfig: TServiceConfig);

    procedure Log(const aLogName: String; const aMessage: String);
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
    procedure DiscoveredCetonDevicesChanged;

    procedure AddListener(const aListener: IServiceConfigEvents);
    procedure RemoveListener(const aListener: IServiceConfigEvents);
  end;

  TServiceConfigManager = class(TInterfacedObject, IServiceConfigManager)
  private
    fConfig: TServiceConfig;
    fEventListeners: TList<IServiceConfigEvents>;

    procedure Lock;
    procedure Unlock;
  protected
    // IServiceConfigManager
    procedure LockConfig(out aConfig: TServiceConfig);
    procedure UnlockConfig(var aConfig: TServiceConfig);

    procedure Log(const aLogName: String; const aMessage: String);
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
    procedure DiscoveredCetonDevicesChanged;

    procedure AddListener(const aListener: IServiceConfigEvents);
    procedure RemoveListener(const aListener: IServiceConfigEvents);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TProxyServiceModule = class;

  TServiceThread = class(TThread, IInterface, IServiceConfigEvents)
  private
    fServiceModule: TProxyServiceModule;
    fChangeEvent: TEvent;
    fConfigChanged: Boolean;
    fCetonDeviceDiscovered: Boolean;
    fLogCaches: TArray<TStringBuilder>;

    procedure SaveLogs;

    procedure QueryDiscoveredCetonDevices;
  protected
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    // IServiceConfigEvents
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
    procedure Log(const aLogName: String; const aMessage: String);
    procedure DiscoveredCetonDevicesChanged;
  public
    constructor Create(const aServiceModule: TProxyServiceModule);
    destructor Destroy; override;

    procedure CetonDeviceDiscovered;

    procedure Execute; override;
  end;

  TProxyServiceModule = class(TDataModule, IServiceConfigEvents, ILogger)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fConfigManager: IServiceConfigManager;
    fClient: TCetonClient;
    fThread: TServiceThread;
    fDiscoveredCetonDeviceList: TDiscoveredCetonDeviceList;
  protected
    procedure LoadConfig;
    procedure SaveConfig;

    procedure MoveLog(const aFilename: String);
  protected
    // IServiceConfigEvents
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
    procedure Log(const aLogName: String; const aMessage: String);
    procedure DiscoveredCetonDevicesChanged;

    // ILogger
    procedure ILogger.Log = HandleLoggerLog;
    procedure HandleLoggerLog(const aLogName: String; const aMessage: String);
  public
    { Public declarations }
    property ConfigManager: IServiceConfigManager read fConfigManager;
    property Client: TCetonClient read fClient;

    property DiscoveredCetonDeviceList: TDiscoveredCetonDeviceList read fDiscoveredCetonDeviceList;

    procedure CetonDeviceDiscovered(const aIP, aDescriptionXMLURL: String);

    function GetConfigPath: String;
  end;

var
  ProxyServiceModule: TProxyServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TServiceConfig }

procedure TServiceConfig.CreateDeviceID;
begin
  fDeviceID := THDHRUtils.CreateDeviceID;
end;

function TServiceConfig.ToJSON: String;
var
  m: TJSONMarshal;
  lJSONValue: TJSONValue;
begin
  m := TJSONMarshal.Create(TJSONConverter.Create);
  try
    m.RegisterConverter(TChannelMap, 'fList',
      function(Data: TObject; Field: String): TListOfObjects
      var
        i: Integer;
      begin
        SetLength(Result, TChannelMap(Data).Count);
        for i := 0 to High(Result) do
          Result[i] := TChannelMap(Data)[i];
      end);

    lJSONValue := m.Marshal(Self);
    try
      Result := lJSONValue.ToString;
    finally
      lJSONValue.Free;
    end;
  finally
    m.Free;
  end;
end;

class function TServiceConfig.FromJSON(const aJSON: String): TServiceConfig;
var
  m: TJSONUnMarshal;
  lR: TReverterEvent;
  lJSONObject: TJSONObject;
begin
  Result := TServiceConfig.Create;
  try
    Result.Version := 0;

    m := TJSONUnMarshal.Create;
    try
      lR := TReverterEvent.Create(TChannelMapItem, 'list');
      lR.ObjectsReverter :=
        procedure(Data: TObject; Field: string; Args: TListOfObjects)
        var
          i: Integer;
        begin
          for i := 0 to High(Args) do
            TChannelMap(Data).Add(TChannelMapItem(Args[i]));
        end;
      m.RegisterReverter(TChannelMap, 'list', lR);

      lJSONObject := TJSONObject(TJSONObject.ParseJSONValue(aJSON));
      try
        Result := m.CreateObject(TServiceConfig, lJSONObject, Result) as TServiceConfig;
      finally
        lJSONObject.Free;
      end;
    finally
      m.Free;
    end;

    if Result.DeviceID = 0 then
      Result.CreateDeviceID;
  except
    FreeAndNil(Result);
    Result := TServiceConfig.Create;
  end;
end;

constructor TServiceConfig.Create;
begin
  fCeton := TCetonConfig.Create;

  fHTTPPort := HDHR_HTTP_PORT;
  fExternalHTTPPort := fHTTPPort;

  fVersion := cServiceConfigVersion;

  CreateDeviceID;
  CreateDeviceUUID;
end;

destructor TServiceConfig.Destroy;
begin
  fCeton.Free;

  inherited;
end;

procedure TServiceConfig.AssignTo(Dest: TPersistent);
var
  lDest: TServiceConfig;
begin
  if Dest is TServiceConfig then
  begin
    lDest := TServiceConfig(Dest);

    lDest.fVersion := fVersion;
    lDest.fCeton.Assign(fCeton);
    lDest.fDeviceID := fDeviceID;
    lDest.fDeviceUUID := fDeviceUUID;
    lDest.fHTTPPort := fHTTPPort;
    lDest.fListenIP := fListenIP;
    lDest.fExternalAddress := fExternalAddress;
    lDest.fExternalHTTPPort := fExternalHTTPPort;
  end
  else
    inherited;
end;


procedure TServiceConfig.CreateDeviceUUID;
begin
  fDeviceUUID := TGUID.NewGuid.ToString.Substring(1, 36);
end;

{ TServiceConfigManager }

constructor TServiceConfigManager.Create;
begin
  fConfig := TServiceConfig.Create;
  fEventListeners := TList<IServiceConfigEvents>.Create;
end;

procedure TServiceConfigManager.Unlock;
begin
  System.TMonitor.Exit(Self);
end;

procedure TServiceConfigManager.LockConfig(out aConfig: TServiceConfig);
begin
  Lock;
  aConfig := fConfig;
end;

procedure TServiceConfigManager.Lock;
begin
  System.TMonitor.Enter(Self);
end;

procedure TServiceConfigManager.Changed(const aSender: TObject;
  const aSections: TServiceConfigSections);
var
  i: Integer;
begin
  Lock;
  try
    for i := 0 to fEventListeners.Count-1 do
    begin
      fEventListeners[i].Changed(aSender, aSections);
    end;
  finally
    Unlock;
  end;
end;

procedure TServiceConfigManager.AddListener(
  const aListener: IServiceConfigEvents);
begin
  fEventListeners.Add(aListener as IServiceConfigEvents);
end;

procedure TServiceConfigManager.RemoveListener(
  const aListener: IServiceConfigEvents);
begin
  fEventListeners.Remove(aListener as IServiceConfigEvents);
end;

destructor TServiceConfigManager.Destroy;
begin
  fConfig.Free;
  fEventListeners.Free;

  inherited;
end;

procedure TServiceConfigManager.UnlockConfig(var aConfig: TServiceConfig);
begin
  aConfig := nil;
  Unlock;
end;

procedure TServiceConfigManager.Log(const aLogName: String; const aMessage: String);
var
  i: Integer;
begin
  Lock;
  try
    for i := 0 to fEventListeners.Count-1 do
    begin
      fEventListeners[i].Log(aLogName, aMessage);
    end;
  finally
    Unlock;
  end;
end;

procedure TServiceConfigManager.DiscoveredCetonDevicesChanged;
var
  i: Integer;
begin
  Lock;
  try
    for i := 0 to fEventListeners.Count-1 do
    begin
      fEventListeners[i].DiscoveredCetonDevicesChanged;
    end;
  finally
    Unlock;
  end;
end;

{ TProxyServiceModule }

procedure TProxyServiceModule.DataModuleCreate(Sender: TObject);
var
  i: Integer;
begin
  fDiscoveredCetonDeviceList := TDiscoveredCetonDeviceList.Create;

  fConfigManager := TServiceConfigManager.Create;

  for i := 0 to High(cLogNames) do
  begin
    try
      MoveLog(GetConfigPath+'cetonproxy'+cLogNames[i]+'.log');
    except
      //
    end;
  end;

  fThread := TServiceThread.Create(Self);

  TLogger.SetLogger(Self);

  TLogger.Log(cLogDefault, 'Starting cetonproxy');

  fClient := TCetonClient.Create;

  LoadConfig;

  fConfigManager.AddListener(Self);

  fThread.Start;
end;

procedure TProxyServiceModule.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(fThread);

  fConfigManager.RemoveListener(Self);

  fClient.Free;

  fDiscoveredCetonDeviceList.Free;

  SaveConfig;

  TLogger.Log(cLogDefault, 'Closing cetonproxy');
end;

function TProxyServiceModule.GetConfigPath: String;
begin
  if FindCmdLineSwitch('config', Result) then
    Result := IncludeTrailingPathDelimiter(Result)
  else
    Result := TPath.GetHomePath + TPath.DirectorySeparatorChar + 'cetonproxy' + TPath.DirectorySeparatorChar;
end;

procedure TProxyServiceModule.Changed(const aSender: TObject; const aSections: TServiceConfigSections);
begin
  // If anything changes the config (besides the TCetonClient itself), then update the
  // client with the latest config
  if aSender <> Client then
  begin
    TThread.ForceQueue(nil,
      procedure()
      var
        lCetonConfig: TCetonConfig;
        lConfig: TServiceConfig;
        lExcludeCetonSections: TCetonConfigSections;
      begin
        lCetonConfig := TCetonConfig.Create;
        try
          // Don't copy channels from config manager if we don't have to
          lExcludeCetonSections := [];
          if not (TServiceConfigSection.Channels in aSections) then
            lExcludeCetonSections := lExcludeCetonSections + [TCetonConfigSection.Channels];

          ConfigManager.LockConfig(lConfig);
          try
            lCetonConfig.Assign(lConfig.Ceton, lExcludeCetonSections);
          finally
            ConfigManager.UnlockConfig(lConfig);
          end;

          Client.AssignConfig(lCetonConfig, lExcludeCetonSections);
        finally
          lCetonConfig.Free;
        end;
      end);
  end;
end;

procedure TProxyServiceModule.LoadConfig;
var
  lJSON: String;
  lConfig, lTempConfig: TServiceConfig;
begin
  try
    lJSON := TFile.ReadAllText(GetConfigPath + 'config.js');
  except
    // Ignore
  end;
  lTempConfig := TServiceConfig.FromJSON(lJSON);
  try
    if lTempConfig.Version < 2 then
    begin
      // In older versions of the app, if ListenIP is blank, then it meant
      // use the first enumerated local IP.  Newer versions use different strategies
      // for listening on ports and for reporting the HDHomeRun URL for clients to connect
      // to, which could cause DVR software to see us as a different HDHomeRun device.
      // So for backward compatibility, assign a listen IP on older configs.
      // NOTE: Purposefully using Indy to match old behavior.
      TIdStack.IncUsage;
      try
        lTempConfig.ListenIP := GStack.LocalAddress;
      finally
        TIdStack.DecUsage;
      end;

      lTempConfig.Version := 2;
    end;

    lTempConfig.Version := cServiceConfigVersion;

    fConfigManager.LockConfig(lConfig);
    try
      lConfig.Assign(lTempConfig);
    finally
      fConfigManager.UnlockConfig(lConfig);
    end;

    fClient.AssignConfig(lTempConfig.Ceton, []);
  finally
    lTempConfig.Free;
  end;
end;

procedure TProxyServiceModule.SaveConfig;
var
  lConfig: TServiceConfig;
begin
  try
    ConfigManager.LockConfig(lConfig);
    try
      ForceDirectories(GetConfigPath);
      TFile.WriteAllText(GetConfigPath + 'config.js', lConfig.ToJSON);
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;
  except
    on e: Exception do
      TLogger.LogFmt(cLogDefault, 'Unable to save config: %s', [e.Message]);
  end;
end;

procedure TProxyServiceModule.Log(const aLogName: String; const aMessage: String);
begin
  // Nothing
end;

procedure TProxyServiceModule.HandleLoggerLog(const aLogName: String; const aMessage: String);
begin
  // Pass to config manager to allow broadcasting it to multiple recipients
  ConfigManager.Log(aLogName, aMessage);
end;

procedure TProxyServiceModule.MoveLog(const aFilename: String);
var
  lPath, lFilename: String;
  lBaseFilename, lNewFilename: String;
  i, lIndex: Integer;
  lFiles: TArray<String>;
begin
  lPath := IncludeTrailingPathDelimiter(ExtractFilePath(aFilename));
  lFilename := ExtractFileName(aFilename);
  if TFile.Exists(aFilename) then
  begin
    lBaseFilename := lPath + ChangeFileExt(lFilename, FormatDateTime('yyyymmddhhhnnss', Now));
    lNewFilename := lBaseFilename + '.log';
    lIndex := 1;
    while TFile.Exists(lNewFilename) do
    begin
      lNewFilename := lBaseFilename + '-' + IntToStr(lIndex) + '.log';
      Inc(lIndex);
    end;

    TFile.Move(aFilename, lNewFilename);
  end;

  // Check for a maximum number of log files
  lFiles := TDirectory.GetFiles(lPath, ChangeFileExt(lFilename,'*.log'), TSearchOption.soTopDirectoryOnly);
  if Length(lFiles) > cMaxLogFiles then
  begin
    TArray.Sort<String>(lFiles, TComparer<String>.Default);
    for i := 0 to High(lFiles)-cMaxLogFiles do
      TFile.Delete(lFiles[i]);
  end;
end;

procedure TProxyServiceModule.CetonDeviceDiscovered(const aIP,
  aDescriptionXMLURL: String);
var
  lDevice: TDiscoveredCetonDevice;
begin
  lDevice := DiscoveredCetonDeviceList.FindByIP(aIP);
  if aDescriptionXMLURL <> lDevice.DescriptionXMLURL then
  begin
    lDevice.IP := aIP;
    lDevice.DescriptionXMLURL := aDescriptionXMLURL;
    lDevice.Queried := False;
    lDevice.FriendlyName := '';
    lDevice.UpdateTicks := TStopWatch.GetTimestamp;
    DiscoveredCetonDeviceList.Update(lDevice);

    fThread.CetonDeviceDiscovered;
  end
  else
  begin
    lDevice.UpdateTicks := TStopWatch.GetTimestamp;
    DiscoveredCetonDeviceList.Update(lDevice);
  end;
end;

procedure TProxyServiceModule.DiscoveredCetonDevicesChanged;
begin
  // Nothing
end;

{ TServiceThread }

constructor TServiceThread.Create(const aServiceModule: TProxyServiceModule);
var
  i: Integer;
begin
  fServiceModule := aServiceModule;

  fChangeEvent := TEvent.Create(nil, False, False, '');

  SetLength(fLogCaches, Length(cLogNames));
  for i := 0 to High(cLogNames) do
    fLogCaches[i] := TStringBuilder.Create;

  fServiceModule.ConfigManager.AddListener(Self);

  inherited Create(True);
end;

procedure TServiceThread.Changed(const aSender: TObject;
  const aSections: TServiceConfigSections);
begin
  fConfigChanged := True;
  fChangeEvent.SetEvent;
end;

destructor TServiceThread.Destroy;
var
  i: Integer;
begin
  Terminate;
  fChangeEvent.SetEvent;
  WaitFor;

  fServiceModule.ConfigManager.RemoveListener(Self);

  fChangeEvent.Free;
  for i := 0 to High(cLogNames) do
    fLogCaches[i].Free;

  inherited;
end;

procedure TServiceThread.QueryDiscoveredCetonDevices;
var
  lClient: TRESTClient;
  lDeviceArray: TArray<TDiscoveredCetonDevice>;
  lDevice: TDiscoveredCetonDevice;
  i: Integer;
  lRequest: TRESTRequest;
begin
  lDeviceArray := fServiceModule.DiscoveredCetonDeviceList.ToArray;

  for i := 0 to High(lDeviceArray) do
  begin
    if not lDeviceArray[i].Queried then
    begin
      lDevice := lDeviceArray[i];

      try
        lClient := TRESTClient.Create(lDevice.DescriptionXMLURL);
        try
          lRequest := TRESTRequest.Create(nil);
          try
            lRequest.Timeout := 3000;

            lRequest.Client := lClient;
            lRequest.Method := TRESTRequestMethod.rmGet;
      //            lRequest.Resource := 'Services/6/Status.html';

            lRequest.Execute;

            if lRequest.Response.StatusCode = 200 then
            begin
              try
                TDiscoveredCetonDevice.UpdateFromDescriptionXML(lDevice, lRequest.Response.Content);
              finally
                lDevice.Queried := True;
                fServiceModule.DiscoveredCetonDeviceList.Update(lDevice);
              end;
            end
            else
              TLogger.LogFmt(cLogDiscovery, 'Unable to reach %s at discovered Ceton device with IP %s: Received status code %d (%s)', [lDevice.DescriptionXMLURL, lDevice.IP, lRequest.Response.StatusCode, lRequest.Response.StatusText]);
          finally
            lRequest.Free;
          end;
        finally
          lClient.Free;
        end;
      except
        on e: Exception do
          TLogger.LogFmt(cLogDiscovery, 'Unable to reach %s at discovered Ceton device with IP %s: %s', [lDevice.DescriptionXMLURL, lDevice.IP, e.Message]);
      end;
    end;
  end;
end;

procedure TServiceThread.Execute;
begin
  Coinitialize(nil);
  try
    while not Terminated do
    begin
      fChangeEvent.WaitFor(1000);

      if fConfigChanged then
      begin
        fConfigChanged := False;

        fServiceModule.SaveConfig;
      end;

      SaveLogs;

      try
        fServiceModule.Client.CheckTuner;
      except
        on e: Exception do
          TLogger.Log(cLogDefault, e.Message);
      end;

      if fCetonDeviceDiscovered then
      begin
        fCetonDeviceDiscovered := False;

        QueryDiscoveredCetonDevices;

        fServiceModule.ConfigManager.DiscoveredCetonDevicesChanged;
      end;

      if fServiceModule.DiscoveredCetonDeviceList.Clean then
        fServiceModule.ConfigManager.DiscoveredCetonDevicesChanged;
    end;
  finally
    CoUninitialize;
  end;
end;

function TServiceThread.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TServiceThread._AddRef: Integer;
begin
  Result := -1;
end;

function TServiceThread._Release: Integer;
begin
  Result := -1;
end;

procedure TServiceThread.Log(const aLogName: String; const aMessage: String);
var
  lMsg: String;
  lIndex: Integer;
begin
  lMsg := Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), aMessage]);

  FMX.Types.Log.d('%s', [lMsg]);

  lIndex := IndexText(aLogName, cLogNames);
  if lIndex > -1 then
  begin
    TMonitor.Enter(fLogCaches[lIndex]);
    try
      fLogCaches[lIndex].AppendLine(lMsg);
    finally
      TMonitor.Exit(fLogCaches[lIndex]);
    end;
  end;
end;

procedure TServiceThread.SaveLogs;
var
  lText: String;
  lPath: String;
  i: Integer;
begin
  for i := 0 to High(cLogNames) do
  begin
    TMonitor.Enter(fLogCaches[i]);
    try
      if fLogCaches[i].Length = 0 then
        Continue;

      lText := fLogCaches[i].ToString;
      fLogCaches[i].Length := 0;
    finally
      TMonitor.Exit(fLogCaches[i]);
    end;

    lPath := fServiceModule.GetConfigPath+'cetonproxy'+cLogNames[i]+'.log';

    try
      TFile.AppendAllText(lPath, lText);
    except
      // Ignore
    end;

    try
      if TFile.GetSize(lPath) >= cLogSizeRollover then
        fServiceModule.MoveLog(lPath);
    except
      // Ignore
    end;
  end;
end;

procedure TServiceThread.CetonDeviceDiscovered;
begin
  fCetonDeviceDiscovered := True;
end;

procedure TServiceThread.DiscoveredCetonDevicesChanged;
begin
  // Nothing
end;

end.
