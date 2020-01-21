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

  REST.Types,
  REST.Json,
  REST.Json.Types,
  REST.JsonReflect,

  HDHR,
  Ceton;

type
  TServiceConfigSection = (Channels, Other);
  TServiceConfigSections = set of TServiceConfigSection;

  TServiceConfig = class(TPersistent)
  private
    fDeviceID: UInt32;
    fCeton: TCetonConfig;
    fHTTPPort: Integer;

    procedure CreateDeviceID;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignTo(Dest: TPersistent); override;

    function ToJSON: String;
    class function FromJSON(const aJSON: String): TServiceConfig; static;

    property Ceton: TCetonConfig read fCeton;
    property DeviceID: UInt32 read fDeviceID write fDeviceID;
    property HTTPPort: Integer read fHTTPPort write fHTTPPort;
  end;

  IServiceConfigEvents = interface
    ['{E51631F5-FC88-4FEC-BCF6-9A0F5616CE79}']
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
  end;

  IServiceConfigManager = interface
    ['{4393C786-9DE0-42DE-BE36-21EA00CFBE70}']
    procedure LockConfig(out aConfig: TServiceConfig);
    procedure UnlockConfig(var aConfig: TServiceConfig);

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

    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);

    procedure AddListener(const aListener: IServiceConfigEvents);
    procedure RemoveListener(const aListener: IServiceConfigEvents);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TProxyServiceModule = class(TDataModule, IServiceConfigEvents)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fConfigManager: IServiceConfigManager;
    fClient: TCetonClient;

    function GetConfigPath: String;
  protected
    // IServiceConfigEvents
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
  public
    { Public declarations }
    property ConfigManager: IServiceConfigManager read fConfigManager;
    property Client: TCetonClient read fClient;
  end;

var
  ProxyServiceModule: TProxyServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TServiceConfig }

procedure TServiceConfig.CreateDeviceID;
begin
  fDeviceID := UInt32(Random(Integer($FFFFFFFF))+1);
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
    lDest.fHTTPPort := fHTTPPort;
  end
  else
    inherited;
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

{ TProxyServiceModule }

procedure TProxyServiceModule.DataModuleCreate(Sender: TObject);
var
  lJSON: String;
  lConfig, lTempConfig: TServiceConfig;
begin
  fConfigManager := TServiceConfigManager.Create;

  fClient := TCetonClient.Create;

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

  fConfigManager.AddListener(Self);
end;

procedure TProxyServiceModule.DataModuleDestroy(Sender: TObject);
var
  lConfig: TServiceConfig;
begin
  fConfigManager.RemoveListener(Self);

  fClient.Free;

  fConfigManager.LockConfig(lConfig);
  try
    ForceDirectories(GetConfigPath);
    TFile.WriteAllText(GetConfigPath + 'config.js', lConfig.ToJSON);
  finally
    fConfigManager.UnlockConfig(lConfig);
  end;
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

end.
