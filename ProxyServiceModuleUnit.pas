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

  REST.Types,
  REST.Json,
  REST.Json.Types,
  REST.JsonReflect,

  HDHR,
  Ceton,
  LogUtils,
  FileUtils;

const
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

    procedure CreateDeviceID;
    procedure CreateDeviceUUID;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignTo(Dest: TPersistent); override;

    function ToJSON: String;
    class function FromJSON(const aJSON: String): TServiceConfig; static;

    property Ceton: TCetonConfig read fCeton;

    property DeviceID: UInt32 read fDeviceID write fDeviceID;
    property DeviceUUID: String read fDeviceUUID write fDeviceUUID;
    property ListenIP: String read fListenIP write fListenIP;
    property HTTPPort: Integer read fHTTPPort write fHTTPPort;
  end;

  IServiceConfigEvents = interface
    ['{E51631F5-FC88-4FEC-BCF6-9A0F5616CE79}']
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
    procedure Log(const aMessage: String);
  end;

  IServiceConfigManager = interface
    ['{4393C786-9DE0-42DE-BE36-21EA00CFBE70}']
    procedure LockConfig(out aConfig: TServiceConfig);
    procedure UnlockConfig(var aConfig: TServiceConfig);

    procedure Log(const aMessage: String);
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);

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

    procedure Log(const aMessage: String);
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);

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
    fLogCache: TStringBuilder;

    procedure SaveLog;
  protected
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    // IServiceConfigEvents
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
    procedure Log(const aMessage: String);
  public
    constructor Create(const aServiceModule: TProxyServiceModule);
    destructor Destroy; override;

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
  protected
    procedure LoadConfig;
    procedure SaveConfig;

    procedure MoveLog;
  protected
    // IServiceConfigEvents
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
    procedure Log(const aMessage: String);

    // ILogger
    procedure ILogger.Log = HandleLoggerLog;
    procedure HandleLoggerLog(const aMessage: String);
  public
    { Public declarations }
    property ConfigManager: IServiceConfigManager read fConfigManager;
    property Client: TCetonClient read fClient;

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
  Result := nil;
  try
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
        Result := m.CreateObject(TServiceConfig, lJSONObject) as TServiceConfig;
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

    lDest.fCeton.Assign(fCeton);
    lDest.fDeviceID := fDeviceID;
    lDest.fDeviceUUID := fDeviceUUID;
    lDest.fHTTPPort := fHTTPPort;
    lDest.fListenIP := fListenIP;
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

procedure TServiceConfigManager.Log(const aMessage: String);
var
  i: Integer;
begin
  Lock;
  try
    for i := 0 to fEventListeners.Count-1 do
    begin
      fEventListeners[i].Log(aMessage);
    end;
  finally
    Unlock;
  end;
end;

{ TProxyServiceModule }

procedure TProxyServiceModule.DataModuleCreate(Sender: TObject);
begin
  fConfigManager := TServiceConfigManager.Create;

  try
    MoveLog;
  except
    //
  end;

  fThread := TServiceThread.Create(Self);

  TLogger.SetLogger(Self);

  TLogger.Log('Starting cetonproxy');

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

  SaveConfig;

  TLogger.Log('Closing cetonproxy');
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
      TLogger.LogFmt('Unable to save config: %s', [e.Message]);
  end;
end;

procedure TProxyServiceModule.Log(const aMessage: String);
begin
  // Nothing
end;

procedure TProxyServiceModule.HandleLoggerLog(const aMessage: String);
begin
  // Pass to config manager to allow broadcasting it to multiple recipients
  ConfigManager.Log(aMessage);
end;

procedure TProxyServiceModule.MoveLog;
var
  lPath: String;
  lBaseFilename, lNewFilename: String;
  i, lIndex: Integer;
  lFiles: TArray<String>;
begin
  lPath := GetConfigPath;
  if TFile.Exists(lPath+'cetonproxy.log') then
  begin
    lBaseFilename := lPath + 'cetonproxy' + FormatDateTime('yyyymmddhhhnnss', Now);
    lNewFilename := lBaseFilename + '.log';
    lIndex := 1;
    while TFile.Exists(lNewFilename) do
    begin
      lNewFilename := lBaseFilename + '-' + IntToStr(lIndex) + '.log';
      Inc(lIndex);
    end;

    TFile.Move(lPath+'cetonproxy.log', lNewFilename);
  end;

  // Check for a maximum number of log files
  lFiles := TDirectory.GetFiles(lPath, 'cetonproxy*.log', TSearchOption.soTopDirectoryOnly);
  if Length(lFiles) > cMaxLogFiles then
  begin
    TArray.Sort<String>(lFiles, TComparer<String>.Default);
    for i := 0 to High(lFiles)-cMaxLogFiles do
      TFile.Delete(lFiles[i]);
  end;
end;


{ TServiceThread }

constructor TServiceThread.Create(const aServiceModule: TProxyServiceModule);
begin
  fServiceModule := aServiceModule;

  fChangeEvent := TEvent.Create(nil, False, False, '');

  fLogCache := TStringBuilder.Create;

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
begin
  Terminate;
  fChangeEvent.SetEvent;
  WaitFor;

  fServiceModule.ConfigManager.RemoveListener(Self);

  fChangeEvent.Free;
  fLogCache.Free;

  inherited;
end;

procedure TServiceThread.Execute;
begin
  while not Terminated do
  begin
    fChangeEvent.WaitFor(1000);

    if fConfigChanged then
    begin
      fConfigChanged := False;

      fServiceModule.SaveConfig;
    end;

    SaveLog;

    try
      fServiceModule.Client.CheckTuner;
    except
      on e: Exception do
        TLogger.Log(e.Message);
    end;
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

procedure TServiceThread.Log(const aMessage: String);
var
  lMsg: String;
begin
  lMsg := Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), aMessage]);

  TMonitor.Enter(fLogCache);
  try
    fLogCache.AppendLine(lMsg);
  finally
    TMonitor.Exit(fLogCache);
  end;
end;

procedure TServiceThread.SaveLog;
var
  lText: String;
  lPath: String;
begin
  TMonitor.Enter(fLogCache);
  try
    if fLogCache.Length = 0 then
      Exit;

    lText := fLogCache.ToString;
    fLogCache.Length := 0;
  finally
    TMonitor.Exit(fLogCache);
  end;

  lPath := fServiceModule.GetConfigPath+'cetonproxy.log';

  try
    TFile.AppendAllText(lPath, lText);
  except
    // Ignore
  end;

  try
    if TFile.GetSize(lPath) >= cLogSizeRollover then
      fServiceModule.MoveLog;
  except
    // Ignore
  end;
end;

end.
