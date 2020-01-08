unit HDHR;

interface

{$SCOPEDENUMS ON}

uses
  System.Types,
  System.SysUtils,
  System.Generics.Collections,
  System.JSON,
  REST.JsonReflect,
  Xml.XmlDoc,
  Xml.XmlIntf,
  SocketUtils,
  System.ZLib;

const
  HDHR_HTTP_PORT = 5004;
  HDHR_DISCOVERY_PORT = 65001;

  HDHOMERUN_TYPE_DISCOVER_REQ = 2;
  HDHOMERUN_TYPE_DISCOVER_RPY = 3;

  HDHOMERUN_TAG_DEVICE_TYPE = 1;
  HDHOMERUN_TAG_DEVICE_ID = 2;
  HDHOMERUN_TAG_BASE_URL = $2A;
  HDHOMERUN_TAG_DEVICE_AUTH_STR = $2B;
  HDHOMERUN_TAG_LINEUP_URL = $27;
  HDHOMERUN_TAG_TUNER_COUNT = $10;

  HDHOMERUN_DEVICE_TYPE_TUNER = 1;

  HDHOMERUN_DEVICE_ID_WILDCARD = $FFFFFFFF;

type
  PUInt16 = ^UInt16;
  PUInt8 = ^UInt8;

(*
  HDHomeRun discovery

  sample request: 0002000C0104000000010204FFFFFFFF4E507F35

  type: uint16
  length: uint16
  data
  CRC: unit32

  discovery request:

  type: 2 (HDHOMERUN_TYPE_DISCOVER_REQ)
  data:
    sequence of:
      tag: uint8
      length: (<= 127: uint8, else: uint16 - first byte is least-significant 7 bits with most-significant bit set, second byte is length shr 7(??))
      value

  two tags
    tag: 1 (HDHOMERUN_TAG_DEVICE_TYPE)
    length: 4
    value: 1 (HDHOMERUN_DEVICE_TYPE_TUNER)
    tag: 2 (HDHOMERUN_TAG_DEVICE_ID)
    length: 4
    value: $FFFFFFFF (HDHOMERUN_DEVICE_ID_WILDCARD)

  discovery reply:

  type: 3 (HDHOMERUN_TYPE_DISCOVER_RPY)
  data:
    tag: 1 (HDHOMERUN_TAG_DEVICE_TYPE)
    length: 4
    value: 1 (HDHOMERUN_DEVICE_TYPE_TUNER)
    tag: 2 (HDHOMERUN_TAG_DEVICE_ID)
    length: 4
    value: uint32 for deviceid
*)
  TTag = record
  private
    fType: UInt8;
    fLength: UInt16;
    fValue: TBytes;
    function GetValueAsUInt32: UInt32;
    procedure SetValueAsUInt32(const Value: UInt32);
    function GetValueAsAnsiString: AnsiString;
    procedure SetValueAsAnsiString(const Value: AnsiString);
    function GetValueAsUInt8: UInt8;
    procedure SetValueAsUInt8(const Value: UInt8);
  public
    class function TryFromBytes(const aBytes: TBytes; var aPos: Integer; out aTag: TTag): Boolean; static;
    function ToBytes: TBytes;

    property _Type: UInt8 read fType write fType;
    property _Length: UInt16 read fLength write fLength;
    property Value: TBytes read fValue write fValue;

    property ValueAsUInt8: UInt8 read GetValueAsUInt8 write SetValueAsUInt8;
    property ValueAsUInt32: UInt32 read GetValueAsUInt32 write SetValueAsUInt32;
    property ValueAsAnsiString: AnsiString read GetValueAsAnsiString write SetValueAsAnsiString;
  end;

  TTagArray = TArray<TTag>;

  TDiscoveryData = record
    DeviceType: UInt32;
    DeviceID: UInt32;
    DeviceAuthStr: AnsiString;
    TunerCount: Integer;
    BaseURL: AnsiString;
    LineupURL: AnsiString;
  end;

  TPacket = record
  private
    fType: UInt16;
    fLength: UInt16;
    fData: TBytes;
    fCRC: UInt32;
    function GetTagArray: TTagArray;
    procedure SetTagArray(const Value: TTagArray);
  public
    property _Type: UInt16 read fType write fType;
    property _Length: UInt16 read fLength write fLength;
    property Data: TBytes read fData write fData;
    property CRC: UInt32 read fCRC write fCRC;

    function CalcCRC: UInt32;
    function IsValid: Boolean;

    function ToBytes: TBytes;
    class function TryFromBytes(const aBytes: TBytes; out aPacket: TPacket): Boolean; static;

    function ToDiscovery: TDiscoveryData;
    class function FromDiscovery(const aRequest: Boolean; const aData: TDiscoveryData): TPacket; static;

    property Tags: TTagArray read GetTagArray write SetTagArray;
  end;


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
    function ToXML: String;
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
  fDeviceID := 'FFFFFFFF';
  fDeviceAuth := 'FFFFFFFF';
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

function TLineup.ToXML: String;
var
  lXMLDoc: IXMLDocument;
  lLineupNode, lProgramNode, lNode: IXMLNode;
  lItem: TLineupItem;
  i: Integer;
begin
  lXMLDoc := TXMLDocument.Create(nil);
  lXMLDoc.Active := True;

  lLineupNode := lXMLDoc.AddChild('Lineup');

  for i := 0 to fList.Count-1 do
  begin
    lItem := fList[i];

    lProgramNode := lLineupNode.AddChild('Program');

    lNode := lProgramNode.AddChild('GuideNumber');
    lNode.NodeValue := lItem.GuideNumber;

    lNode := lProgramNode.AddChild('GuideName');
    lNode.NodeValue := lItem.GuideName;

    lNode := lProgramNode.AddChild('URL');
    lNode.NodeValue := lItem.URL;
  end;

  Result := lXMLDoc.XML.Text;
end;

{ TTag }

class function TTag.TryFromBytes(const aBytes: TBytes;
  var aPos: Integer; out aTag: TTag): Boolean;
begin
  Result := False;
  if Length(aBytes) >= aPos+2 then
  begin
    aTag.fType := aBytes[aPos];
    Inc(aPos);
    if aBytes[aPos] and $80 <> 0 then
    begin
      aTag.fLength := PUInt16(@aBytes[aPos])^;
      // TODO: ....
      Inc(aPos,2);
    end
    else
    begin
      aTag.fLength := aBytes[aPos];
      Inc(aPos);
    end;

    if Length(aBytes) >= aPos+aTag.fLength then
    begin
      SetLength(aTag.fValue, aTag.fLength);
      if Length(aTag.fValue) > 0 then
        Move(aBytes[aPos], aTag.fValue[0], Length(aTag.fValue));
      Inc(aPos, Length(aTag.fValue));

      Result := True;
    end;
  end;
end;

function TTag.ToBytes: TBytes;
begin
  if fLength <= 127 then
  begin
    SetLength(Result, 1+1+fLength);
    Result[0] := fType;
    Result[1] := fLength;
    if Length(fValue) > 0 then
      Move(fValue[0], Result[2], Length(fValue));
  end
  else
  begin
    SetLength(Result, 1+2+fLength);
    Result[0] := fType;
    // TODO: ....
    PUInt16(@Result[1])^ := fLength;
    if Length(fValue) > 0 then
      Move(fValue[0], Result[3], Length(fValue));
  end;
end;

function TTag.GetValueAsUInt32: UInt32;
begin
  if Length(fValue) >= 4 then
    Result := BEToN32(PUInt32(@fValue[0])^)
  else
    Result := 0;
end;

procedure TTag.SetValueAsUInt32(const Value: UInt32);
begin
  SetLength(fValue, 4);
  PUInt32(@fValue[0])^ := NToBE32(Value);
  fLength := 4;
end;

function TTag.GetValueAsAnsiString: AnsiString;
begin
  if Length(fValue) > 0 then
  begin
    SetLength(Result, Length(fValue));
    Move(fValue[0], Result[1], Length(fValue));
  end
  else
    Result := '';
end;

procedure TTag.SetValueAsAnsiString(const Value: AnsiString);
begin
  SetLength(fValue, Length(Value));
  if Length(Value) > 0 then
    Move(Value[1], fValue[0], Length(Value));
  fLength := Length(Value);
end;

function TTag.GetValueAsUInt8: UInt8;
begin
  if Length(fValue) > 0 then
    Result := fValue[0]
  else
    Result := 0;
end;

procedure TTag.SetValueAsUInt8(const Value: UInt8);
begin
  SetLength(fValue, 1);
  fValue[0] := Value;
  fLength := 1;
end;

{ TPacket }

function TPacket.CalcCRC: UInt32;
var
  lBytes: TBytes;
begin
  lBytes := ToBytes;
  if Length(lBytes) > 0 then
    Result := UInt32(crc32(0, @lBytes[0], Length(lBytes)-4))
  else
    Result := 0;
end;

function TPacket.ToBytes: TBytes;
begin
  SetLength(Result, 4+Length(fData)+4);
  PUInt16(@Result[0])^ := NToBE16(fType);
  PUInt16(@Result[2])^ := NToBE16(fLength);
  if Length(fData) > 0 then
    Move(fData[0], Result[4], Length(fData));
  PUInt32(@Result[4+Length(fData)])^ := fCRC;
end;

class function TPacket.TryFromBytes(const aBytes: TBytes; out aPacket: TPacket): Boolean;
begin
  Result := False;
  if Length(aBytes) > 4 then
  begin
    aPacket.fType := BEToN16(PUInt16(@aBytes[0])^);
    aPacket.fLength := BEToN16(PUInt16(@aBytes[2])^);

    if Length(aBytes) >= 4+aPacket.fLength+4 then
    begin
      SetLength(aPacket.fData, aPacket.fLength);
      if Length(aPacket.fData) > 0 then
        Move(aBytes[4], aPacket.fData[0], Length(aPacket.fData));

      aPacket.fCRC := PUInt32(@aBytes[4+Length(aPacket.fData)])^;
      Result := True;
    end;
  end;
end;

function TPacket.ToDiscovery: TDiscoveryData;
var
  i: Integer;
  lTags: TTagArray;
begin
  Result := Default(TDiscoveryData);

  lTags := Tags;
  for i := 0 to High(lTags) do
  begin
    case lTags[i]._Type of
      HDHOMERUN_TAG_DEVICE_TYPE: begin
        Result.DeviceType := lTags[i].ValueAsUInt32;
      end;
      HDHOMERUN_TAG_DEVICE_ID: begin
        Result.DeviceID := lTags[i].ValueAsUInt32;
      end;
      HDHOMERUN_TAG_TUNER_COUNT: begin
        Result.TunerCount := lTags[i].ValueAsUInt8;
      end;
      HDHOMERUN_TAG_BASE_URL: begin
        Result.BaseURL := lTags[i].ValueAsAnsiString;
      end;
      HDHOMERUN_TAG_LINEUP_URL: begin
        Result.LineupURL := lTags[i].ValueAsAnsiString;
      end;
      HDHOMERUN_TAG_DEVICE_AUTH_STR: begin
        Result.DeviceAuthStr := lTags[i].ValueAsAnsiString;
      end;
    end;
  end;
end;

class function TPacket.FromDiscovery(const aRequest: Boolean;
  const aData: TDiscoveryData): TPacket;
var
  lTags: TTagArray;
begin
  SetLength(lTags, 6);
  lTags[0]._Type := HDHOMERUN_TAG_DEVICE_TYPE;
  lTags[0].ValueAsUInt32 := aData.DeviceType;

  lTags[1]._Type := HDHOMERUN_TAG_DEVICE_ID;
  lTags[1].ValueAsUInt32 := aData.DeviceID;

  lTags[2]._Type := HDHOMERUN_TAG_DEVICE_AUTH_STR;
  lTags[2].ValueAsAnsiString := aData.DeviceAuthStr;

  lTags[3]._Type := HDHOMERUN_TAG_TUNER_COUNT;
  lTags[3].ValueAsUInt8 := aData.TunerCount;

  lTags[4]._Type := HDHOMERUN_TAG_BASE_URL;
  lTags[4].ValueAsAnsiString := aData.BaseURL;

  lTags[5]._Type := HDHOMERUN_TAG_LINEUP_URL;
  lTags[5].ValueAsAnsiString := aData.LineupURL;

  Result := Default(TPacket);
  if aRequest then
    Result._Type := HDHOMERUN_TYPE_DISCOVER_REQ
  else
    Result._Type := HDHOMERUN_TYPE_DISCOVER_RPY;
  Result.Tags := lTags;
  Result.CRC := Result.CalcCRC;
end;

function TPacket.GetTagArray: TTagArray;
var
  lPos: Integer;
  lList: TList<TTag>;
  lTag: TTag;
begin
  lList := TList<TTag>.Create;
  try
    lPos := 0;
    while lPos < Length(fData) do
    begin
      if not TTag.TryFromBytes(fData, lPos, lTag) then
        Exit;
      lList.Add(lTag);
    end;
    Result := lList.ToArray;
  finally
    lList.Free;
  end;
end;

procedure TPacket.SetTagArray(const Value: TTagArray);
var
  i: Integer;
  lBytes: TBytes;
begin
  fData := nil;
  for i := 0 to High(Value) do
  begin
    lBytes := Value[i].ToBytes;
    fData := fData + lBytes;
  end;
  fLength := Length(fData);
end;

function TPacket.IsValid: Boolean;
begin
  Result := fCRC = CalcCRC;
end;

end.
