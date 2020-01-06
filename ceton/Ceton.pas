unit Ceton;

interface

uses
  FMX.Types,

  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.SysUtils,
  System.StrUtils,
  System.NetEncoding,
  System.IOUtils,
  System.SyncObjs,
  System.Math,
  System.JSON,
  System.DateUtils,

  Xml.XmlDoc, Xml.XmlIntf,

  IdUDPServer, IdGlobal, IdSocketHandle, IdStack, IdStackConsts,

  REST.Client,
  REST.Types,
  REST.Json,
  REST.Json.Types,
  REST.JsonReflect;

type
  ECetonClientError = class(Exception);
  ECetonTunerError = class(Exception);

  TChannelMapItem = class(TPersistent)
  private
    fName: String;
    fModulation: String;
    fChannel: Integer;
    fFrequency: Integer;
    fEIA: Single;
    fItemProgram: Integer;
    fSourceID: Integer;
    fVisible: Boolean;
  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure ApplyConfig(const aItem: TChannelMapItem);

    property SourceID: Integer read fSourceID write fSourceID;
    property Channel: Integer read fChannel write fChannel;
    property Name: String read fName write fName;
    property Modulation: String read fModulation write fModulation;
    property Frequency: Integer read fFrequency write fFrequency;
    property ItemProgram: Integer read fItemProgram write fItemProgram;
    property EIA: Single read fEIA write fEIA;

    property Visible: Boolean read fVisible write fVisible;
  end;

  TChannelMap = class
  private
    [JSONOwned(False)]
    fList: TObjectList<TChannelMapItem>;
    fRequestDateTime: TDateTime;
    function GetCount: Integer;
    function GetItem(const aIndex: Integer): TChannelMapItem;
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(const aChannel: Integer): Integer;
    function Add(const aChannel: Integer): TChannelMapItem;
    procedure Clear;

    procedure CopyFromXML(const aXML: String);
    procedure CopyFromHTML(const aHTML: String);

    procedure Assign(const aChannelMap: TChannelMap);
    procedure ApplyConfig(const aChannelMap: TChannelMap);

    function Expired(const aDays: Single): Boolean;

    property Count: Integer read GetCount;
    property Items[const aIndex: Integer]: TChannelMapItem read GetItem; default;

    property RequestDateTime: TDateTime read fRequestDateTime write fRequestDateTime;
  end;

  TChannelMapRequest = class(TCustomRESTRequest)
  private
    fXML: Boolean;
    fPage: Integer;
  protected
    procedure DoBeforeExecute; override;
  public
    constructor Create(AOwner: TComponent); override;

    property Page: Integer read fPage write fPage;
    property XML: Boolean read fXML write fXML;
  end;

  TStartStreamRequest = class(TCustomRESTRequest)
  private
    fInstanceID: Integer;
    fPort: Integer;
    fIP: String;
  protected
    procedure DoBeforeExecute; override;
  public
    constructor Create(AOwner: TComponent); override;

    property InstanceID: Integer read fInstanceID write fInstanceID;
    property IP: String read fIP write fIP;
    property Port: Integer read fPort write fPort;
  end;

  TStopStreamRequest = class(TCustomRESTRequest)
  private
    fInstanceID: Integer;
  protected
    procedure DoBeforeExecute; override;
  public
    constructor Create(AOwner: TComponent); override;

    property InstanceID: Integer read fInstanceID write fInstanceID;
  end;

  TTuneChannelRequest = class(TCustomRESTRequest)
  private
    fChannel: Integer;
    fInstanceID: Integer;
  protected
    procedure DoBeforeExecute; override;
  public
    constructor Create(AOwner: TComponent); override;

    property InstanceID: Integer read fInstanceID write fInstanceID;
    property Channel: Integer read fChannel write fChannel;
  end;

  TREST = class abstract
  public
    class procedure RequestChannelMap(const aClient: TRestClient; const aChannelMap: TChannelMap); static;
    class procedure StartStream(const aClient: TRestClient; const aTunerID: Integer; const aIP: String; const aPort: Integer); static;
    class procedure StopStream(const aClient: TRestClient; const aTunerID: Integer); static;
    class procedure TuneChannel(const aClient: TRestClient; const aTunerID, aChannel: Integer); static;
  end;

  {

    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |V=2|P|X|  CC   |M|     PT      |       sequence number         |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                           timestamp                           |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |           synchronization source (SSRC) identifier            |
   +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+

  }
  PRTPHeader = ^TRTPHeader;
  TRTPHeader = packed record
  private
    // HEADER 12 bytes
    RawVersion  : UInt8;      // $80  : Version = 2
    RawPayload  : UInt8;      //   8  : PCMA
    RawSequence : UInt16;
    RawTimestamp: UInt32;
    RawSSRC     : UInt32;

    class function Swap16(const aValue: UInt16): UInt16; static;

    function GetPayloadType: UInt8;
    function GetSequenceNumber: UInt16;
    function GetVersion: UInt8;
  public
    property Version: UInt8 read GetVersion;
    property SequenceNumber: UInt16 read GetSequenceNumber;
    property PayloadType: UInt8 read GetPayloadType;
  end;

  PVideoPacket = ^TVideoPacket;
  TVideoPacket = record
    Data: TBytes;
    Size: Integer;
    ReadCount: Integer;
    Ready: Boolean;

    Index: Integer;

    class function Empty: TVideoPacket; static;
  end;

  TRTPVideoSink = class
  private
    fServer: TIdUDPServer;
    fReaderCount: Integer;
    fBufferEvent, fReadEvent: TEvent;

    fLastHeader: TRTPHeader;

    fPackets: TArray<TVideoPacket>;
    fPacketIndex: Integer;

    function GetPort: Integer;
  protected
    procedure Lock;
    procedure Unlock;

    procedure UDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
  public
    constructor Create(const aPacketSize, aPacketCount: Integer);
    destructor Destroy; override;

    procedure Read(var aPacket: TVideoPacket);

    procedure RegisterReader(out aPacket: TVideoPacket);
    procedure UnregisterReader;

    property Port: Integer read GetPort;
  end;

  TTuner = class
  private
    fStreaming: Boolean;
    fChannel: Integer;
    fRefCount: Integer;
    fSink: TRTPVideoSink;
    function GetSink: TRTPVideoSink;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveSink;

    property Sink: TRTPVideoSink read GetSink;

    property Streaming: Boolean read fStreaming write fStreaming;
    property Channel: Integer read fChannel write fChannel;
    property RefCount: Integer read fRefCount write fRefCount;
  end;

  TTunerList = class
  private
    fList: TObjectList<TTuner>;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    function GetItem(const aIndex: Integer): TTuner;
  public
    constructor Create;
    destructor Destroy; override;

    function Find(const aChannel: Integer): Integer;

    property Count: Integer read GetCount write SetCount;
    property Items[const aIndex: Integer]: TTuner read GetItem; default;
  end;

  TCetonConfig = class(TPersistent)
  private
    [JSONOwned(False)]
    fChannelMap: TChannelMap;
    fTunerAddress: String;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON: String;
    class function FromJSON(const aJSON: String): TCetonConfig; static;

    property TunerAddress: String read fTunerAddress write fTunerAddress;
    property ChannelMap: TChannelMap read fChannelMap;
  end;

  TCetonClient = class
  private
    fConfig: TCetonConfig;
    fClient: TRESTClient;
    fTunerList: TTunerList;

    procedure Lock;
    procedure Unlock;

    procedure NeedConfig;

    function GetLocalIP: String;
    function GetTunerAddress: String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetConfig(const aConfig: TCetonConfig);

    procedure RequestChannelMap;
    procedure GetChannelMap(const aChannelMap: TChannelMap);
    function ChannelMapExpired(const aDays: Single): Boolean;
    procedure ApplyChannelConfig(const aChannelMap: TChannelMap);

    procedure StartStream(const aChannel: Integer; out aTunerIndex: Integer; out aPacket: TVideoPacket);
    procedure StopStream(const aTunerIndex: Integer);
    procedure ReadStreamPacket(const aTunerIndex: Integer; var aPacket: TVideoPacket);

    property TunerAddress: String read GetTunerAddress;
  end;

  TRingBuffer = class
  private
    fData: TBytes;
    fStartIndex, fSize: Integer;
    procedure Realign;
  public
    procedure Write(var Buffer; Count: Longint);
    function Read(var Buffer; Count: Longint): Longint;
    property Size: Integer read fSize;
  end;

  TCetonVideoStream = class(TStream)
  private
    fBuffer: TRingBuffer;
    fClient: TCetonClient;
    fTunerIndex: Integer;
    fPacket: TVideoPacket;
  public
    constructor Create(const aClient: TCetonClient; const aChannel: Integer); reintroduce;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

implementation

{ TChannelMap }

constructor TChannelMap.Create;
begin
  fList := TObjectList<TChannelMapItem>.Create(
    TDelegatedComparer<TChannelMapItem>.Construct(
      function(const Left, Right: TChannelMapItem): Integer
      begin
        Result := Left.Channel - Right.Channel;
      end), True);
end;

destructor TChannelMap.Destroy;
begin
  fList.Free;

  inherited;
end;

procedure TChannelMap.CopyFromHTML(const aHTML: String);
var
  lPos: Integer;

  function MoveToTable: Boolean;
  begin
    lPos := aHTML.IndexOf('<table', lPos);
    Result := lPos > -1;
  end;

  function NextRow: Boolean;
  begin
    lPos := aHTML.IndexOf('<tr>', lPos);
    Result := lPos > -1;
  end;

  function NextColumn: Boolean;
  begin
    lPos := aHTML.IndexOf('<td>', lPos);
    Result := lPos > -1;
  end;

  function GetCellContent: String;
  var
    i: Integer;
  begin
    if SameText(aHTML.Substring(lPos, 4), '<td>') then
    begin
      i := aHTML.IndexOf('</td>', lPos);
      try
        if i > -1 then
        begin
          Exit(aHtml.Substring(lPos+4, i-lPos-4));
        end;
      finally
        lPos := i;
      end;
    end;
    Result := '';
  end;

var
  lItem: TChannelMapItem;
  lChannel: Integer;
begin
  lPos := 0;
  if MoveToTable then
  begin
    while NextRow do
    begin
      if NextColumn then
        lChannel := StrToIntDef(GetCellContent, 0)
      else
        lChannel := 0;

      lItem := Add(lChannel);

      if NextColumn then
        lItem.Name := GetCellContent;

      if NextColumn then
        lItem.Modulation := GetCellContent;

      if NextColumn then
        lItem.Frequency := StrToIntDef(GetCellContent, 0);

      if NextColumn then
        lItem.ItemProgram := StrToIntDef(GetCellContent, 0);

      if NextColumn then
        lItem.EIA := StrToFloatDef(GetCellContent, 0);
    end;
  end;
end;

function TChannelMap.Add(const aChannel: Integer): TChannelMapItem;
var
  lIndex: Integer;
begin
  Result := TChannelMapItem.Create;
  Result.Channel := aChannel;

  if fList.BinarySearch(Result, lIndex) then
  begin
    Result.Free;
    Result := fList[lIndex];
  end
  else
    fList.Insert(lIndex, Result);
end;

procedure TChannelMap.CopyFromXML(const aXML: String);
var
  lXML: IXMLDocument;
  lChannelsNode, lChannelNode: IXMLNode;
  i: Integer;
  lChannel: Integer;
  lItem: TChannelMapItem;
begin
  lXML := TXMLDocument.Create(nil);
  lXML.LoadFromXML(aXML);

  lChannelsNode := lXML.ChildNodes.FindNode('channels');
  if Assigned(lChannelsNode) then
  begin
    for i := 0 to lChannelsNode.ChildNodes.Count-1 do
    begin
      lChannelNode := lChannelsNode.ChildNodes[i];

      lChannel := lChannelNode.ChildNodes.Nodes['number'].NodeValue;

      lItem := Add(lChannel);

      lItem.Name := TNetEncoding.Base64.Decode(lChannelNode.ChildNodes.Nodes['name'].NodeValue);
      lItem.Modulation := lChannelNode.ChildNodes.Nodes['modulation'].NodeValue;
      lItem.Frequency := lChannelNode.ChildNodes.Nodes['frequency'].NodeValue;
      lItem.ItemProgram := lChannelNode.ChildNodes.Nodes['program'].NodeValue;
      lItem.EIA := lChannelNode.ChildNodes.Nodes['eia'].NodeValue;
      lItem.SourceID := lChannelNode.ChildNodes.Nodes['sourceid'].NodeValue;
    end;
  end;
end;

function TChannelMap.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TChannelMap.IndexOf(const aChannel: Integer): Integer;
var
  lItem: TChannelMapItem;
begin
  lItem := TChannelMapItem.Create;
  try
    lItem.Channel := aChannel;

    if not fList.BinarySearch(lItem, Result) then
      Result := -1;
  finally
    lItem.Free;
  end;
end;

function TChannelMap.GetItem(const aIndex: Integer): TChannelMapItem;
begin
  Result := fList[aIndex];
end;

procedure TChannelMap.Clear;
begin
  fList.Clear;
end;

procedure TChannelMap.Assign(const aChannelMap: TChannelMap);
var
  i: Integer;
begin
  Clear;
  for i := 0 to aChannelMap.Count-1 do
    Add(aChannelMap[i].Channel).Assign(aChannelMap[i]);
  RequestDateTime := aChannelMap.RequestDateTime;
end;

procedure TChannelMap.ApplyConfig(const aChannelMap: TChannelMap);
var
  i: Integer;
  lDestIndex: Integer;
begin
  lDestIndex := 0;
  for i := 0 to aChannelMap.Count-1 do
  begin
    if (lDestIndex = -1) or (aChannelMap[i].Channel <> fList[lDestIndex].Channel) then
      lDestIndex := IndexOf(aChannelMap[i].Channel);
    if lDestIndex <> -1 then
    begin
      fList[lDestIndex].ApplyConfig(aChannelMap[i]);
      Inc(lDestIndex);
    end;
  end;
end;

function TChannelMap.Expired(const aDays: Single): Boolean;
begin
  Result := TTimeZone.Local.ToUniversalTime(Now)-aDays >= RequestDateTime
end;

{ TChannelMapRequest }

constructor TChannelMapRequest.Create(AOwner: TComponent);
begin
  inherited;

  Method := TRESTRequestMethod.rmGet;
  Resource := 'view_channel_map.cgi?page={PAGE}&xml={XML}';
end;

procedure TChannelMapRequest.DoBeforeExecute;
begin
  inherited;

  AddParameter('PAGE', IntToStr(Page), TRESTRequestParameterKind.pkURLSEGMENT);
  AddParameter('XML', IfThen(XML,'1','0'), TRESTRequestParameterKind.pkURLSEGMENT);
end;

{ TREST }

class procedure TREST.RequestChannelMap(const aClient: TRestClient;
  const aChannelMap: TChannelMap);
var
  lRequest: TChannelMapRequest;
  lPage, lLastCount: Integer;
  lChannelMap: TChannelMap;
begin
  lChannelMap := TChannelMap.Create;
  try
    lChannelMap.Assign(aChannelMap);
    aChannelMap.Clear;

    lRequest := TChannelMapRequest.Create(nil);
    try
      lRequest.Client := aClient;

      lRequest.XML := True;

      // Page zero loads up to 1024 items
      lRequest.Page := 0;
      lRequest.Execute;
      aChannelMap.CopyFromXML(lRequest.Response.Content);

      // Then request the rest
      lPage := 21;
      repeat
        lLastCount := aChannelMap.Count;

        lRequest.Page := lPage;
        Inc(lPage);

        lRequest.Execute;
        aChannelMap.CopyFromXML(lRequest.Response.Content);
      until aChannelMap.Count = lLastCount;
    finally
      lRequest.Free;
    end;

    aChannelMap.ApplyConfig(lChannelMap);

    aChannelMap.RequestDateTime := TTimeZone.Local.ToUniversalTime(Now);
  finally
    lChannelMap.Free;
  end;
end;

class procedure TREST.StartStream(const aClient: TRestClient;
  const aTunerID: Integer; const aIP: String; const aPort: Integer);
var
  lRequest: TStartStreamRequest;
begin
  lRequest := TStartStreamRequest.Create(nil);
  try
    lRequest.Client := aClient;

    lRequest.InstanceID := aTunerID;
    lRequest.IP := aIP;
    lRequest.Port := aPort;

    lRequest.Execute;
  finally
    lRequest.Free;
  end;
end;

class procedure TREST.StopStream(const aClient: TRestClient;
  const aTunerID: Integer);
var
  lRequest: TStopStreamRequest;
begin
  lRequest := TStopStreamRequest.Create(nil);
  try
    lRequest.Client := aClient;

    lRequest.InstanceID := aTunerID;

    lRequest.Execute;
  finally
    lRequest.Free;
  end;
end;

class procedure TREST.TuneChannel(const aClient: TRestClient; const aTunerID,
  aChannel: Integer);
var
  lRequest: TTuneChannelRequest;
begin
  lRequest := TTuneChannelRequest.Create(nil);
  try
    lRequest.Client := aClient;

    lRequest.InstanceID := aTunerID;
    lRequest.Channel := aChannel;

    lRequest.Execute;
  finally
    lRequest.Free;
  end;
end;

{ TTunerList }

constructor TTunerList.Create;
begin
  fList := TObjectList<TTuner>.Create(True);
end;

destructor TTunerList.Destroy;
begin
  fList.Free;

  inherited;
end;

function TTunerList.GetCount: Integer;
begin
  Result := fList.Count;
end;

procedure TTunerList.SetCount(const Value: Integer);
begin
  while fList.Count > Value do
    fList.Delete(fList.Count-1);
  while fList.Count < Value do
    fList.Add(TTuner.Create);
end;

function TTunerList.Find(const aChannel: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to fList.Count-1 do
  begin
    if fList[i].Channel = aChannel then
      Exit(i);
  end;

  for i := 0 to fList.Count-1 do
  begin
    if not fList[i].Streaming then
      Exit(i);
  end;

  Result := -1;
end;

function TTunerList.GetItem(const aIndex: Integer): TTuner;
begin
  Result := fList[aIndex];
end;

{ TCetonClient }

constructor TCetonClient.Create;
begin
  fTunerList := TTunerList.Create;
  fConfig := TCetonConfig.Create;
end;

destructor TCetonClient.Destroy;
begin
  fClient.Free;
  fTunerList.Free;

  inherited;
end;

procedure TCetonClient.RequestChannelMap;
begin
  Lock;
  try
    NeedConfig;

    TREST.RequestChannelMap(fClient, fConfig.ChannelMap);
  finally
    Unlock;
  end;
end;

procedure TCetonClient.StartStream(const aChannel: Integer; out aTunerIndex: Integer; out aPacket: TVideoPacket);
var
  lTuner: TTuner;
begin
  Lock;
  try
    NeedConfig;

    aTunerIndex := fTunerList.Find(aChannel);
    if aTunerIndex = -1 then
      raise ECetonTunerError.Create('All tuners are busy');

    lTuner := fTunerList[aTunerIndex];

    lTuner.RefCount := lTuner.RefCount + 1;
    lTuner.Sink.RegisterReader(aPacket);

    if lTuner.Channel <> aChannel then
    begin
      lTuner.Channel := aChannel;

      TREST.TuneChannel(fClient, aTunerIndex, aChannel);
    end;

    if not lTuner.Streaming then
    begin
      lTuner.Streaming := True;

      TREST.StartStream(fClient, aTunerIndex, GetLocalIP, lTuner.Sink.Port);
    end;
  finally
    Unlock;
  end;
end;

procedure TCetonClient.StopStream(const aTunerIndex: Integer);
var
  lTuner: TTuner;
begin
  Lock;
  try
    NeedConfig;

    lTuner := fTunerList[aTunerIndex];

    lTuner.RefCount := lTuner.RefCount - 1;
    lTuner.Sink.UnregisterReader;

    if lTuner.RefCount = 0 then
    begin
      lTuner.RemoveSink;

      lTuner.Channel := 0;
      lTuner.Streaming := False;

      TREST.StopStream(fClient, aTunerIndex);
    end;
  finally
    Unlock;
  end;
end;

function TCetonClient.GetLocalIP: String;
begin
  TIdStack.IncUsage;
  try
    Result := GStack.LocalAddress;
  finally
    TIdStack.DecUsage;
  end;
end;

procedure TCetonClient.ReadStreamPacket(const aTunerIndex: Integer;
  var aPacket: TVideoPacket);
begin
  Lock;
  try
    NeedConfig;

    fTunerList[aTunerIndex].Sink.Read(aPacket);
  finally
    Unlock;
  end;
end;

procedure TCetonClient.Lock;
begin
  TMonitor.Enter(Self);
end;

procedure TCetonClient.Unlock;
begin
  TMonitor.Exit(Self);
end;

procedure TCetonClient.GetChannelMap(const aChannelMap: TChannelMap);
begin
  Lock;
  try
    NeedConfig;

    aChannelMap.Assign(fConfig.ChannelMap);
  finally
    Unlock;
  end;
end;

procedure TCetonClient.SetConfig(const aConfig: TCetonConfig);
begin
  Lock;
  try
    FreeAndNil(fClient);
    fConfig := aConfig;
    fClient := TRESTClient.Create(Format('http://%s', [fConfig.TunerAddress]));

    fTunerList.Count := 6;
  finally
    Unlock;
  end;
end;

procedure TCetonClient.NeedConfig;
begin
  if not Assigned(fConfig) then
    raise ECetonClientError.Create('Configuration is not assigned');
end;

function TCetonClient.ChannelMapExpired(const aDays: Single): Boolean;
begin
  Lock;
  try
    NeedConfig;

    Result := fConfig.ChannelMap.Expired(aDays);
  finally
    Unlock;
  end;
end;

procedure TCetonClient.ApplyChannelConfig(const aChannelMap: TChannelMap);
begin
  Lock;
  try
    NeedConfig;

    fConfig.ChannelMap.ApplyConfig(aChannelMap);
  finally
    Unlock;
  end;
end;

function TCetonClient.GetTunerAddress: String;
begin
  Lock;
  try
    NeedConfig;

    Result := fConfig.TunerAddress;
  finally
    Unlock;
  end;
end;

{ TStartStreamRequest }

procedure TStartStreamRequest.DoBeforeExecute;
begin
  inherited;

  AddParameter('instance_id', IntToStr(InstanceID));
  AddParameter('dest_ip', IP);
  AddParameter('dest_port', IntToStr(Port));
  AddParameter('protocol', '0');
  AddParameter('start', '1');
end;

constructor TStartStreamRequest.Create(AOwner: TComponent);
begin
  inherited;

  Method := TRESTRequestMethod.rmPost;
  Resource := 'stream_request.cgi';
end;

{ TStopStreamRequest }

procedure TStopStreamRequest.DoBeforeExecute;
begin
  inherited;

  AddParameter('instance_id', IntToStr(InstanceID));
  AddParameter('dest_ip', '192.168.200.1');
  AddParameter('dest_port', '8000');
  AddParameter('protocol', '0');
  AddParameter('start', '0');
end;

constructor TStopStreamRequest.Create(AOwner: TComponent);
begin
  inherited;

  Method := TRESTRequestMethod.rmPost;
  Resource := 'stream_request.cgi';
end;

{ TTuneChannelRequest }

procedure TTuneChannelRequest.DoBeforeExecute;
begin
  inherited;

  AddParameter('instance_id', IntToStr(InstanceID));
  AddParameter('channel', IntToStr(Channel));
end;

constructor TTuneChannelRequest.Create(AOwner: TComponent);
begin
  inherited;

  Method := TRESTRequestMethod.rmPost;
  Resource := 'channel_request.cgi';
end;

{ TRTPVideoSink }

constructor TRTPVideoSink.Create(const aPacketSize, aPacketCount: Integer);
var
  i: Integer;
begin
  fServer := TIdUDPServer.Create(nil);
  fServer.DefaultPort := 0;
  fServer.ThreadedEvent := True;
  fServer.OnUDPRead := UDPRead;
  fServer.Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_RCVBUF, 1024*1024);

  SetLength(fPackets, aPacketCount);
  for i := 0 to High(fPackets) do
    SetLength(fPackets[i].Data, aPacketSize);

  fBufferEvent := TEvent.Create(nil, True, False, '');
  fReadEvent := TEvent.Create(nil, True, False, '');

  fServer.Active := True;
end;

destructor TRTPVideoSink.Destroy;
begin
  fServer.Free;

  fBufferEvent.Free;
  fReadEvent.Free;

  inherited;
end;

function TRTPVideoSink.GetPort: Integer;
begin
  if fServer.Bindings.Count > 0 then
  begin
    if fServer.Bindings[0].IPVersion = Id_IPv4 then
      Exit(fServer.Bindings[0].Port);
  end;
  Result := 0;
end;

procedure TRTPVideoSink.UDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  lPacket: PVideoPacket;
  lToWrite, lWritten: Integer;
  lHeader: PRTPHeader;
begin
//  Log.D('Received %d byte packet', [Length(aData)]);
//  TFile.WriteAllBytes('d:\temp\temp.rtp', TBytes(aData));

  if Length(AData) < 12 then
    Exit;

  lHeader := @AData[0];

  if fLastHeader.Version > 0 then
  begin
    if lHeader.SequenceNumber <> fLastHeader.SequenceNumber+1 then
      Log.D('Out of order %d', [lHeader.SequenceNumber - fLastHeader.SequenceNumber]);
    if lHeader.PayloadType <> fLastHeader.PayloadType then
      Log.D('Payload type %d', [lHeader.PayloadType]);
  end;
  fLastHeader := lHeader^;

  lWritten := 12; // Skip RTP header

  repeat
    Lock;
    try
      repeat
        lPacket := @fPackets[fPacketIndex];
        if not lPacket.Ready then
        begin
          lToWrite := Min(Length(AData)-lWritten, Length(lPacket.Data)-lPacket.Size);

          if lToWrite > 0 then
          begin
            Move(AData[lWritten], lPacket.Data[lPacket.Size], lToWrite);
            Inc(lWritten, lToWrite);
            Inc(lPacket.Size, lToWrite);

            if lWritten >= Length(AData) then
            begin
              // Written entire packet
              Exit;
            end;
          end
          else
          begin
            // Can't fit anything more into packet
            if fReaderCount > 0 then
              lPacket.Ready := True;

//            Log.D('Created packet %d', [lPacket.Size]);

            // Move to next
            fPacketIndex := (fPacketIndex + 1) mod Length(fPackets);
            lPacket := @fPackets[fPacketIndex];

//            Log.D('WRITE: set buffer');
            fBufferEvent.SetEvent;
          end;
        end;

        if AThread.Terminated then
          Exit;
      until lPacket.Ready;

//      Log.D('WRITE: reset read');
      fReadEvent.ResetEvent;
    finally
      Unlock;
    end;

    // We weren't able to copy the packet into the packet array because readers
    // are behind.  Wait for a reader to finish.
//    Log.D('WRITE: waitfor read');
    fReadEvent.WaitFor(1000);
  until False;
end;

procedure TRTPVideoSink.Lock;
begin
  TMonitor.Enter(Self);
end;

procedure TRTPVideoSink.Unlock;
begin
  TMonitor.Exit(Self);
end;

procedure TRTPVideoSink.Read(var aPacket: TVideoPacket);
var
  lPacket: PVideoPacket;
begin
  repeat
    Lock;
    try
      lPacket := @fPackets[aPacket.Index];
      if lPacket.Ready then
      begin
        if Length(aPacket.Data) <> Length(lPacket.Data) then
          SetLength(aPacket.Data, Length(lPacket.Data));

        Move(lPacket.Data[0], aPacket.Data[0], lPacket.Size);
        aPacket.Size := lPacket.Size;

        aPacket.Index := (aPacket.Index + 1) mod Length(fPackets);

        Inc(lPacket.ReadCount);
        if lPacket.ReadCount >= fReaderCount then
        begin
          // Allow packet to be used again
          lPacket.Size := 0;
          lPacket.Ready := False;
          lPacket.ReadCount := 0;
        end;

//        Log.D('READ: set read');
        fReadEvent.SetEvent;

        Exit;
      end;

//      Log.D('READ: reset buffer');
      fBufferEvent.ResetEvent;
    finally
      Unlock;
    end;

    // Packet is not yet ready
//    Log.D('READ: waitfor buffer');
    fBufferEvent.WaitFor(1000);
  until False;
end;

procedure TRTPVideoSink.RegisterReader(out aPacket: TVideoPacket);
begin
  Lock;
  try
    Inc(fReaderCount);

    // Initialize packet index
    aPacket := TVideoPacket.Empty;
    aPacket.Index := fPacketIndex;
  finally
    Unlock;
  end;
end;

procedure TRTPVideoSink.UnregisterReader;
begin
  Lock;
  try
    Inc(fReaderCount);
  finally
    Unlock;
  end;
  fReadEvent.SetEvent;
end;

{ TTuner }

constructor TTuner.Create;
begin
  fSink := nil;
end;

destructor TTuner.Destroy;
begin
  FreeAndNil(fSink);

  inherited;
end;

function TTuner.GetSink: TRTPVideoSink;
begin
  if not Assigned(fSink) then
    fSink := TRTPVideoSink.Create(4096*8, 100);
  Result := fSink;
end;

procedure TTuner.RemoveSink;
begin
  FreeAndNil(fSink);
end;

{ TVideoPacket }

class function TVideoPacket.Empty: TVideoPacket;
begin
  Result := Default(TVideoPacket);
end;

{ TCetonVideoStream }

constructor TCetonVideoStream.Create(const aClient: TCetonClient;
  const aChannel: Integer);
begin
  fClient := aClient;

  fBuffer := TRingBuffer.Create;

  fClient.StartStream(aChannel, fTunerIndex, fPacket);
end;

function TCetonVideoStream.Read(var Buffer; Count: Longint): Longint;
begin
  while fBuffer.Size < Count do
  begin
    fClient.ReadStreamPacket(fTunerIndex, fPacket);
    fBuffer.Write(fPacket.Data[0], fPacket.Size);
  end;

  Result := fBuffer.Read(Buffer, Count);
end;

destructor TCetonVideoStream.Destroy;
begin
  fClient.StopStream(fTunerIndex);

  fBuffer.free;

  inherited;
end;

function TCetonVideoStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := -1;
end;

{ TChannelMapItem }

procedure TChannelMapItem.AssignTo(Dest: TPersistent);
var
  lDestItem: TChannelMapItem;
begin
  if Dest is TChannelMapItem then
  begin
    lDestItem := TChannelMapItem(Dest);
    lDestItem.fName := fName;
    lDestItem.fModulation := fModulation;
    lDestItem.fChannel := fChannel;
    lDestItem.fFrequency := fFrequency;
    lDestItem.fEIA := fEIA;
    lDestItem.fItemProgram := fItemProgram;
    lDestItem.fSourceID := fSourceID;
    lDestItem.fVisible := fVisible;
  end
  else
    inherited;
end;

procedure TChannelMapItem.ApplyConfig(const aItem: TChannelMapItem);
begin
  Visible := aItem.Visible;
end;

{ TRingBuffer }

procedure TRingBuffer.Write(var Buffer; Count: Longint);
var
  lIndex: Integer;
  lToWrite: Integer;
begin
  if Length(fData) < fSize+Count then
  begin
    Realign;
    SetLength(fData, fSize+Count);
  end;

  lIndex := (fStartIndex+fSize) mod Length(fData);
  lToWrite := Min(Count, Length(fData)-lIndex);
  if lToWrite > 0 then
    Move(Buffer, fData[lIndex], lToWrite);
  Inc(fSize, lToWrite);

  if lToWrite < Count then
    Write(PByteArray(@Buffer)[lToWrite], Count-lToWrite);
end;

function TRingBuffer.Read(var Buffer; Count: Longint): Longint;
var
  lToRead: Integer;
begin
  lToRead := Min(Count, Length(fData)-fStartIndex);
  if lToRead > 0 then
    Move(fData[fStartIndex], Buffer, lToRead);
  fStartIndex := (fStartIndex + lToRead) mod Length(fData);
  Dec(fSize, lToRead);
  Result := lToRead;

  if (lToRead < Count) and (fSize > 0) then
  begin
    Inc(Result, Read(PByteArray(@Buffer)[lToRead], Count-lToRead));
  end;
end;

procedure TRingBuffer.Realign;
var
  lData: TBytes;
  lSize: Integer;
begin
  if fStartIndex > 0 then
  begin
    SetLength(lData, Length(fData));
    lSize := fSize;
    Read(lData[0], lSize);
    fData := lData;
    fSize := lSize;
    fStartIndex := 0;
  end;
end;

{ TCetonConfig }

constructor TCetonConfig.Create;
begin
  fChannelMap := TChannelMap.Create;
end;

destructor TCetonConfig.Destroy;
begin
  fChannelMap.Free;

  inherited;
end;

function TCetonConfig.ToJSON: String;
var
  m: TJSONMarshal;
begin
  m := TJSONMarshal.Create(TJSONConverter.Create);
  try
    m.RegisterConverter(TChannelMap, 'fList',
      function(Data: TObject; Field: String): TListOfObjects
      var
        obj: TChannelMapItem;
        I: Integer;
      begin
        SetLength(Result, TChannelMap(Data).fList.Count);
        I := Low(Result);
        for obj in TChannelMap(Data).fList do
        begin
          Result[I] := obj;
          Inc(I);
        end;
      end);

    Result := m.Marshal(Self).ToString;
  finally
    m.Free;
  end;
end;

class function TCetonConfig.FromJSON(const aJSON: String): TCetonConfig;
var
  m: TJSONUnMarshal;
  lR: TReverterEvent;
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
          for i := Low(Args) to High(Args) do
          begin
            TChannelMap(Data).fList.Add(TChannelMapItem(Args[i]));
          end;
        end;
      m.RegisterReverter(TChannelMap, 'list', lR);

      Result := m.CreateObject(TCetonConfig, TJSONObject(TJSONObject.ParseJSONValue(aJSON))) as TCetonConfig;
    finally
      m.Free;
    end;
  except
    FreeAndNil(Result);
    Result := TCetonConfig.Create;
  end;
end;

{ TRTPHeader }

function TRTPHeader.GetPayloadType: UInt8;
begin
  Result := RawPayload and $7F;
end;

function TRTPHeader.GetSequenceNumber: UInt16;
begin
  Result := Swap16(RawSequence);
end;

class function TRTPHeader.Swap16(const aValue: UInt16): UInt16;
begin
  Result := Swap(aValue);
end;

function TRTPHeader.GetVersion: UInt8;
begin
  Result := RawVersion shr 6;
end;

end.
