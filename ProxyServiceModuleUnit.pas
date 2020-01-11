unit ProxyServiceModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.DateUtils,

  REST.Types,
  REST.Json,
  REST.Json.Types,
  REST.JsonReflect,

  Ceton;

type
  TServiceConfig = class(TPersistent)
  private
    fDeviceID: UInt32;
    fCeton: TCetonConfig;

    procedure CreateDeviceID;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignTo(Dest: TPersistent); override;

    function ToJSON: String;
    class function FromJSON(const aJSON: String): TServiceConfig; static;

    property Ceton: TCetonConfig read fCeton;
    property DeviceID: UInt32 read fDeviceID write fDeviceID;
  end;

  TProxyServiceModule = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fConfig: TServiceConfig;
    fClient: TCetonClient;

    procedure Lock;
    procedure Unlock;
  public
    { Public declarations }
    procedure GetConfig(const aConfig: TServiceConfig);
    procedure SetConfig(const aConfig: TServiceConfig);

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
  end
  else
    inherited;
end;

{ TProxyServiceModule }

procedure TProxyServiceModule.DataModuleCreate(Sender: TObject);
var
  lJSON: String;
begin
  fClient := TCetonClient.Create;

  try
    lJSON := TFile.ReadAllText(TPath.GetHomePath + TPath.DirectorySeparatorChar + 'cetonproxy' + TPath.DirectorySeparatorChar + 'config.js');
  except
    // Ignore
  end;
  fConfig := TServiceConfig.FromJSON(lJSON);

  fConfig.Ceton.TunerAddress := '192.168.1.132';

  fClient.SetConfig(fConfig.Ceton);
end;

procedure TProxyServiceModule.DataModuleDestroy(Sender: TObject);
begin
  Client.GetConfig(fConfig.Ceton);

  FreeAndNil(fClient);

  ForceDirectories(TPath.GetHomePath + TPath.DirectorySeparatorChar + 'cetonproxy');
  TFile.WriteAllText(TPath.GetHomePath + TPath.DirectorySeparatorChar + 'cetonproxy' + TPath.DirectorySeparatorChar + 'config.js', fConfig.ToJSON);

  FreeAndNil(fConfig);
end;

procedure TProxyServiceModule.GetConfig(const aConfig: TServiceConfig);
begin
  Lock;
  try
    aConfig.Assign(fConfig);
    Client.GetConfig(aConfig.Ceton);
  finally
    Unlock;
  end;
end;

procedure TProxyServiceModule.SetConfig(const aConfig: TServiceConfig);
begin
  Lock;
  try
    fConfig.Assign(aConfig);
    Client.SetConfig(fConfig.Ceton);
  finally
    Unlock;
  end;
end;

procedure TProxyServiceModule.Lock;
begin
  TMonitor.Enter(Self);
end;

procedure TProxyServiceModule.Unlock;
begin
  TMonitor.Exit(Self);
end;

end.
