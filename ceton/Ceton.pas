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

  TRingBuffer = class
  private
    fData: TBytes;
    fStartIndex, fSize: Integer;
    procedure Resize(const aSize: Integer);
  public
    procedure Write(var Buffer; Count: Longint);
    function Read(var Buffer; Count: Longint): Longint;
    function Seek(const Offset: Integer; Origin: TSeekOrigin): Integer;
    property Size: Integer read fSize;
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
  end;

  TVideoReader = record
    Packet: TVideoPacket;
    ReaderIndex: Integer;
  end;

  TRTPVideoSink = class
  private
    fServer: TIdUDPServer;
    fWriteEvent: TEvent;

    fLastHeader: TRTPHeader;

    fPackets: TArray<TVideoPacket>;
    fWritePacketIndex: Integer;

    fReaderPacketIndexes: TArray<Integer>;

    function GetPort: Integer;
    procedure TimeoutError;
  protected
    procedure Lock;
    procedure Unlock;

    procedure UDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
  public
    constructor Create(const aPacketSize, aPacketCount: Integer);
    destructor Destroy; override;

    procedure Read(var aReader: TVideoReader; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);
    function WaitForSignal(const aReader: TVideoReader; const aTimeoutMs: Integer): Boolean;

    procedure RegisterReader(out aReader: TVideoReader);
    procedure UnregisterReader(var aReader: TVideoReader);

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

  TCetonViewer = record
    TunerIndex: Integer;
    Reader: TVideoReader;

    function IsValid: Boolean;

    class function Invalid: TCetonViewer; static;
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

    procedure StartStream(const aChannel: Integer; out aViewer: TCetonViewer);
    procedure StopStream(var aViewer: TCetonViewer);
    procedure ReadStream(var aViewer: TCetonViewer; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);

    property TunerAddress: String read GetTunerAddress;
  end;

  TCetonVideoStream = class(TStream)
  private
    fBuffer: TRingBuffer;
    fClient: TCetonClient;
    fViewer: TCetonViewer;
  public
    constructor Create(const aClient: TCetonClient; const aChannel: Integer); reintroduce;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
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
  Log.d('Starting stream on tuner %d to %s:%d', [aTunerID, aIP, aPort]);

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
  Log.d('Stopping stream on tuner %d', [aTunerID]);

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
  Log.d('Setting channel on tuner %d to %d', [aTunerID, aChannel]);

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

procedure TCetonClient.StartStream(const aChannel: Integer; out aViewer: TCetonViewer);
var
  lTuner: TTuner;
  lReader: TVideoReader;
  lReceivedPacket: Boolean;
  lCount: Integer;
begin
  Lock;
  try
    NeedConfig;

    aViewer.TunerIndex := fTunerList.Find(aChannel);
    if aViewer.TunerIndex = -1 then
      raise ECetonTunerError.Create('All tuners are busy');

    lTuner := fTunerList[aViewer.TunerIndex];

    lTuner.Channel := aChannel;
    lTuner.Streaming := True;
    lTuner.RefCount := lTuner.RefCount + 1;

    lTuner.Sink.RegisterReader(aViewer.Reader);

    // Use another reader to wait for signal before returning
    // If there's an issue, then we can fix it right now
    lTuner.Sink.RegisterReader(lReader);
    try
      lCount := 0;
      repeat
        // Only send tuning commands if we are the first to request from the tuner or
        // we're on a second/third try
        if (lCount > 0) or (lTuner.RefCount = 1) then
        begin
          TREST.StopStream(fClient, aViewer.TunerIndex);
          TREST.TuneChannel(fClient, aViewer.TunerIndex, aChannel);
          TREST.StartStream(fClient, aViewer.TunerIndex, GetLocalIP, lTuner.Sink.Port);
        end;

        lReceivedPacket := lTuner.Sink.WaitForSignal(lReader, 5000);
        Inc(lCount);

        if lCount >= 3 then
        begin
          Log.D('No video data on tuner %d for channel %d', [aViewer.TunerIndex, aChannel]);

          raise ECetonTunerError.Create('No video data');
        end;
      until lReceivedPacket or (lCount >= 3);
    finally
      lTuner.Sink.UnregisterReader(lReader);
    end;
  finally
    Unlock;
  end;
end;

procedure TCetonClient.StopStream(var aViewer: TCetonViewer);
var
  lTuner: TTuner;
begin
  try
    Lock;
    try
      NeedConfig;

      lTuner := fTunerList[aViewer.TunerIndex];

      lTuner.Sink.UnregisterReader(aViewer.Reader);

      lTuner.RefCount := lTuner.RefCount - 1;

      if lTuner.RefCount = 0 then
      begin
        lTuner.RemoveSink;

        lTuner.Channel := 0;
        lTuner.Streaming := False;

        TREST.StopStream(fClient, aViewer.TunerIndex);
      end;
    finally
      Unlock;
    end;
  finally
    aViewer := TCetonViewer.Invalid;
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

procedure TCetonClient.ReadStream(var aViewer: TCetonViewer; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);
begin
  Lock;
  try
    NeedConfig;

    fTunerList[aViewer.TunerIndex].Sink.Read(aViewer.Reader, aBuffer, aCount, aTimeoutMs);
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

  fWriteEvent := TEvent.Create(nil, True, False, '');

  fServer.Active := True;
end;

destructor TRTPVideoSink.Destroy;
begin
  fServer.Free;

  fWriteEvent.Free;

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
  lWritten, lToWrite: Integer;
  lHeader: PRTPHeader;
  i: Integer;
begin
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
  lToWrite := Length(AData)-lWritten;

  Lock;
  try
    lPacket := @fPackets[fWritePacketIndex];

    if Length(lPacket.Data) < lToWrite then
      SetLength(lPacket.Data, lToWrite);

    Move(AData[lWritten], lPacket.Data[0], lToWrite);
    lPacket.Size := lToWrite;

    // Decide where we'll write the next packet to come in
    fWritePacketIndex := (fWritePacketIndex + 1) mod Length(fPackets);

    // If any readers will be looking at that packet index next, push them along to
    // the next packet.  Do not allow a slow reader to drag us behind because that
    // can lead to packet loss.
    for i := 0 to High(fReaderPacketIndexes) do
    begin
      if fReaderPacketIndexes[i] = fWritePacketIndex then
      begin
        fReaderPacketIndexes[i] := (fReaderPacketIndexes[i] + 1) mod Length(fPackets);
        Break;
      end;
    end;

    fWriteEvent.SetEvent;
  finally
    Unlock;
  end;
end;

procedure TRTPVideoSink.Lock;
begin
  TMonitor.Enter(Self);
end;

procedure TRTPVideoSink.Unlock;
begin
  TMonitor.Exit(Self);
end;

procedure TRTPVideoSink.TimeoutError;
begin
  raise ECetonTunerError.Create('Timeout waiting for video data');
end;

procedure TRTPVideoSink.Read(var aReader: TVideoReader; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);
var
  lPacket: PVideoPacket;
begin
  // Attempt to read packets until the size of aBuffer reaches aCount
  repeat
    Lock;
    try
      while aBuffer.Size < aCount do
      begin
        lPacket := @fPackets[fReaderPacketIndexes[aReader.ReaderIndex]];
        // Do not advance into a packet index that the writer is writing to next
        if (fReaderPacketIndexes[aReader.ReaderIndex] <> fWritePacketIndex) then
        begin
          if Length(aReader.Packet.Data) < lPacket.Size then
            SetLength(aReader.Packet.Data, lPacket.Size);

          if lPacket.Size > 0 then
            Move(lPacket.Data[0], aReader.Packet.Data[0], lPacket.Size);
          aReader.Packet.Size := lPacket.Size;

          if aReader.Packet.Size > 0 then
            aBuffer.Write(aReader.Packet.Data[0], aReader.Packet.Size);

          fReaderPacketIndexes[aReader.ReaderIndex] := (fReaderPacketIndexes[aReader.ReaderIndex] + 1) mod Length(fPackets);
        end
        else
          Break;
      end;

      if aBuffer.Size >= aCount then
        Exit;

      fWriteEvent.ResetEvent;
    finally
      Unlock;
    end;

    // Packet is not yet ready
    if fWriteEvent.WaitFor(aTimeoutMs) = wrTimeout then
      TimeoutError;
  until False;
end;

procedure TRTPVideoSink.RegisterReader(out aReader: TVideoReader);
var
  i: Integer;
begin
  Lock;
  try
    aReader.Packet := Default(TVideoPacket);

    aReader.ReaderIndex := -1;
    for i := 0 to High(fReaderPacketIndexes) do
      if fReaderPacketIndexes[i] = -1 then
      begin
        aReader.ReaderIndex := i;
        fReaderPacketIndexes[i] := fWritePacketIndex;
        Break;
      end;

    if aReader.ReaderIndex = -1 then
    begin
      aReader.ReaderIndex := Length(fReaderPacketIndexes);
      Insert(fWritePacketIndex, fReaderPacketIndexes, Length(fReaderPacketIndexes));
    end;
  finally
    Unlock;
  end;
end;

procedure TRTPVideoSink.UnregisterReader(var aReader: TVideoReader);
begin
  Lock;
  try
    fReaderPacketIndexes[aReader.ReaderIndex] := -1;
    aReader.ReaderIndex := -1;
  finally
    Unlock;
  end;
end;

function TRTPVideoSink.WaitForSignal(const aReader: TVideoReader; const aTimeoutMs: Integer): Boolean;
begin
  repeat
    Lock;
    try
      Result := fReaderPacketIndexes[aReader.ReaderIndex] <> fWritePacketIndex;
      if Result then
        fReaderPacketIndexes[aReader.ReaderIndex] := fWritePacketIndex
      else
        fWriteEvent.ResetEvent;
    finally
      Unlock;
    end;

    if not Result then
      if fWriteEvent.WaitFor(aTimeoutMs) = wrTimeout then
        Break;
  until Result;
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
    fSink := TRTPVideoSink.Create(1024, 1000);
  Result := fSink;
end;

procedure TTuner.RemoveSink;
begin
  FreeAndNil(fSink);
end;

{ TCetonVideoStream }

constructor TCetonVideoStream.Create(const aClient: TCetonClient;
  const aChannel: Integer);
begin
  fClient := aClient;

  fBuffer := TRingBuffer.Create;

  fClient.StartStream(aChannel, fViewer);
end;

function TCetonVideoStream.Read(var Buffer; Count: Longint): Longint;
begin
  fClient.ReadStream(fViewer, fBuffer, Count - fBuffer.Size, 5000);

  Result := fBuffer.Read(Buffer, Count);
end;

destructor TCetonVideoStream.Destroy;
begin
  fClient.StopStream(fViewer);

  fBuffer.free;

  inherited;
end;

function TCetonVideoStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
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
    Resize(fSize+Count);

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
  lToRead := Min(Count, Min(fSize, Length(fData)-fStartIndex));
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

procedure TRingBuffer.Resize(const aSize: Integer);
var
  lData: TBytes;
begin
  if (fStartIndex > 0) and (aSize > 0) then
  begin
    SetLength(lData, aSize);
    fSize := Read(lData[0], aSize);
    fData := lData;
    fStartIndex := 0;
  end
  else
  begin
    SetLength(fData, aSize);
    fSize := Min(fSize, aSize);
  end;
end;

function TRingBuffer.Seek(const Offset: Integer; Origin: TSeekOrigin): Integer;
var
  lToRead: Integer;
begin
  if Origin = TSeekOrigin.soCurrent then
  begin
    lToRead := Min(Offset, Length(fData)-fStartIndex);
    fStartIndex := (fStartIndex + lToRead) mod Length(fData);
    Dec(fSize, lToRead);

    if (lToRead < Offset) and (fSize > 0) then
    begin
      Seek(Offset-lToRead, soCurrent);
    end;
  end;

  Result := -1;
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

{ TCetonViewer }

class function TCetonViewer.Invalid: TCetonViewer;
begin
  Result := Default(TCetonViewer);
  Result.TunerIndex := -1;
end;

function TCetonViewer.IsValid: Boolean;
begin
  Result := TunerIndex > -1;
end;

end.
