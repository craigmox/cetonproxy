unit HDHR;

interface

uses
  System.Generics.Collections,
  System.JSON,
  REST.JsonReflect;

const
  HDHR_HTTP_PORT = 5004;
  HDHR_DISCOVERY_PORT = 65001;

type
(*
discoverData = {
    'FriendlyName': 'tvhProxy',
    'Manufacturer' : 'Silicondust',
    'ModelNumber': 'HDTC-2US',
    'FirmwareName': 'hdhomeruntc_atsc',
    'TunerCount': int(config['tunerCount']),
    'FirmwareVersion': '20150826',
    'DeviceID': '12345678',
    'DeviceAuth': 'test1234',
    'BaseURL': '%s' % config['tvhProxyURL'],
    'LineupURL': '%s/lineup.json' % config['tvhProxyURL']
}
*)

  TDiscoverResponse = class
  private
    fManufacturor: String;
    fLineupURL: String;
    fTunerCount: Integer;
    fBaseURL: String;
    fDeviceAuth: String;
    fFirmwareVersion: String;
    fFirmwareName: String;
    fFriendlyName: String;
    fDeviceId: String;
    fModelNumber: String;
  public
    constructor Create;

    property FriendlyName: String read fFriendlyName write fFriendlyName;
    property Manufacturer: String read fManufacturor write fManufacturor;
    property ModelNumber: String read fModelNumber write fModelNumber;
    property FirmwareName: String read fFirmwareName write fFirmwareName;
    property TunerCount: Integer read fTunerCount write fTunerCount;
    property FirmwareVersion: String read fFirmwareVersion write fFirmwareVersion;
    property DeviceID: String read fDeviceId write fDeviceID;
    property DeviceAuth: String read fDeviceAuth write fDeviceAuth;
    property BaseURL: String read fBaseURL write fBaseURL;
    property LineupURL: String read fLineupURL write fLineupURL;
  end;

  TLineupItem = class
  private
    fGuideName: String;
    fURL: String;
    fGuideNumber: String;
  public
    property GuideNumber: String read fGuideNumber write fGuideNumber;
    property GuideName: String read fGuideName write fGuideName;
    property URL: String read fURL write fURL;
  end;

  TLineup = class
  private
    fList: TObjectList<TLineupItem>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const aGuideNumber, aGuideName, aURL: String);

    function ToJSON: String;
  end;

implementation

{ TDiscoverResponse }

constructor TDiscoverResponse.Create;
begin
  // {"FriendlyName":"HDHomeRun PRIME","ModelNumber":"HDHR3-CC","FirmwareName":"hdhomerun3_cablecard","FirmwareVersion":"20160630atest2","DeviceID":"FFFFFFFF","DeviceAuth":"FFFFFFFF","TunerCount":3,"ConditionalAccess":1,"BaseURL":"http://192.168.1.182:80","LineupURL":"http://192.168.1.182:80/lineup.json"}
  fFriendlyName := 'HDHomeRun PRIME';
  fManufacturor := 'Silicondust';
  fModelNumber := 'HDHR3-CC';
  fFirmwareName := 'hdhomerun3_cablecard';
  fTunerCount := 4;
  fFirmwareVersion := '20160630atest2';
  fDeviceID := 'cetonproxyid';
  fDeviceAuth := 'cetonproxyauth';
  fBaseURL := 'http://192.168.1.116:80';
  fLineupURL := 'http://192.168.1.116:80/lineup.json';
end;

{ TLineup }

constructor TLineup.Create;
begin
  fList := TObjectList<TLineupItem>.Create(True);
end;

destructor TLineup.Destroy;
begin
  fList.Free;

  inherited;
end;

procedure TLineup.Add(const aGuideNumber, aGuideName, aURL: String);
var
  lItem: TLineupItem;
begin
  lItem := TLineupItem.Create;
  lItem.GuideNumber := aGuideNumber;
  lItem.GuideName := aGuideName;
  lItem.URL := aURL;
  fList.Add(lItem);
end;

function TLineup.ToJSON: String;
var
  m: TJSONMarshal;
  lObj: TJSONValue;
begin
  m := TJSONMarshal.Create(TJSONConverter.Create);
  try
    m.RegisterConverter(TLineup, 'fList',
      function(Data: TObject; Field: String): TListOfObjects
      var
        obj: TLineupItem;
        I: Integer;
      begin
        SetLength(Result, TLineup(Data).fList.Count);
        I := Low(Result);
        for obj in TLineup(Data).fList do
        begin
          Result[I] := obj;
          Inc(I);
        end;
      end);
    lObj := m.Marshal(Self);

    Result := TJSONObject(lObj).Values['list'].ToString;
  finally
    m.Free;
  end;
end;

end.
