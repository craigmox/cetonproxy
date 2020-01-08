unit Ceton;

interface

{$SCOPEDENUMS ON}

uses
  FMX.Types,

  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.SysUtils,
  System.NetEncoding,
  System.Math,
  System.JSON,
  System.DateUtils,
  System.StrUtils,

  Xml.XmlDoc,
  Xml.XmlIntf,

  IdStack,

  REST.Client,
  REST.Types,
  REST.Json,
  REST.Json.Types,
  REST.JsonReflect,

  SocketUtils;

type
  ECetonClientError = class(Exception);
  ECetonTunerError = class(Exception);

  TChannelMap = class;

  TCetonConfig = class(TPersistent)
  private
    [JSONOwned(False)]
    fChannelMap: TChannelMap;
    fTunerAddress: String;
    fListenIP: String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignTo(Dest: TPersistent); override;

    property ListenIP: String read fListenIP write fListenIP;
    property TunerAddress: String read fTunerAddress write fTunerAddress;
    property ChannelMap: TChannelMap read fChannelMap;
  end;

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

  TChannelMapSection = (Items);
  TChannelMapSections = set of TChannelMapSection;

  TChannelMap = class
  private
    [JSONOwned(False)]
    fList: TObjectList<TChannelMapItem>;
    fRequestDateTime: TDateTime;
    [JSONMarshalled(False)]
    fExclude: TChannelMapSections;
    function GetCount: Integer;
    function GetItem(const aIndex: Integer): TChannelMapItem;
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(const aChannel: Integer): Integer;
    function Add(const aChannel: Integer): TChannelMapItem; overload;
    procedure Add(const aItem: TChannelMapItem); overload;
    procedure Clear;

    procedure CopyFromXML(const aXML: String);
    procedure CopyFromHTML(const aHTML: String);

    procedure Assign(const aChannelMap: TChannelMap);
    procedure ApplyConfig(const aChannelMap: TChannelMap);

    function Expired(const aDays: Single): Boolean;

    property Count: Integer read GetCount;
    property Items[const aIndex: Integer]: TChannelMapItem read GetItem; default;

    property RequestDateTime: TDateTime read fRequestDateTime write fRequestDateTime;

    property Exclude: TChannelMapSections read fExclude write fExclude;
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

  TTuneProgramRequest = class(TCustomRESTRequest)
  private
    fProgramNumber: Integer;
    fInstanceID: Integer;
  protected
    procedure DoBeforeExecute; override;
  public
    constructor Create(AOwner: TComponent); override;

    property InstanceID: Integer read fInstanceID write fInstanceID;
    property ProgramNumber: Integer read fProgramNumber write fProgramNumber;
  end;

  TREST = class abstract
  public
    class procedure RequestChannelMap(const aClient: TRestClient; const aChannelMap: TChannelMap); static;
    class procedure StartStream(const aClient: TRestClient; const aTunerID: Integer; const aIP: String; const aPort: Integer); static;
    class procedure StopStream(const aClient: TRestClient; const aTunerID: Integer); static;
    class procedure TuneChannel(const aClient: TRestClient; const aTunerID, aChannel: Integer); static;
    class procedure TuneProgram(const aClient: TRestClient; const aTunerID, aProgram: Integer); static;
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

    function GetListenIP: String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetConfig(const aConfig: TCetonConfig);
    procedure GetConfig(const aConfig: TCetonConfig);

    procedure RequestChannelMap;

    procedure StartStream(const aTuner: Integer; const aChannel: Integer; out aViewer: TCetonViewer);
    procedure StopStream(var aViewer: TCetonViewer);
    procedure ReadStream(var aViewer: TCetonViewer; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);

    property ListenIP: String read GetListenIP;
  end;

  TCetonVideoStream = class(TStream)
  private
    fBuffer: TRingBuffer;
    fClient: TCetonClient;
    fViewer: TCetonViewer;
  public
    constructor Create(const aClient: TCetonClient; const aTuner: Integer; const aChannel: Integer); reintroduce;
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
  if not (TChannelMapSection.Items in aChannelMap.Exclude + fExclude) then
  begin
    Clear;
    for i := 0 to aChannelMap.Count-1 do
      Add(aChannelMap[i].Channel).Assign(aChannelMap[i]);
  end;
  fRequestDateTime := aChannelMap.fRequestDateTime;
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

procedure TChannelMap.Add(const aItem: TChannelMapItem);
begin
  fList.Add(aItem);
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

class procedure TREST.TuneProgram(const aClient: TRestClient; const aTunerID,
  aProgram: Integer);
var
  lRequest: TTuneProgramRequest;
begin
  Log.d('Setting program on tuner %d to %d', [aTunerID, aProgram]);

  lRequest := TTuneProgramRequest.Create(nil);
  try
    lRequest.Client := aClient;

    lRequest.InstanceID := aTunerID;
    lRequest.ProgramNumber := aProgram;

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
  fConfig := TCetonConfig.Create;
  fTunerList := TTunerList.Create;
end;

destructor TCetonClient.Destroy;
begin
  fClient.Free;
  fTunerList.Free;
  fConfig.Free;

  inherited;
end;

procedure TCetonClient.RequestChannelMap;
begin
  Lock;
  try
    TREST.RequestChannelMap(fClient, fConfig.ChannelMap);
  finally
    Unlock;
  end;
end;

procedure TCetonClient.StartStream(const aTuner: Integer; const aChannel: Integer; out aViewer: TCetonViewer);
var
  lTuner: TTuner;
  lReader: TVideoReader;
  lReceivedPacket: Boolean;
  lCount: Integer;
  lIndex: Integer;
begin
  Lock;
  try
    aViewer.TunerIndex := aTuner;
    if aViewer.TunerIndex < 0 then
    begin
      aViewer.TunerIndex := fTunerList.Find(aChannel);
      if aViewer.TunerIndex = -1 then
        raise ECetonTunerError.Create('All tuners are busy');
    end;
    if aViewer.TunerIndex >= fTunerList.Count then
      raise ECetonTunerError.CreateFmt('Invalid tuner: %d', [aViewer.TunerIndex]);

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
          TREST.TuneChannel(fClient, aViewer.TunerIndex, 0);
          TREST.TuneChannel(fClient, aViewer.TunerIndex, aChannel);
          lIndex := fConfig.ChannelMap.IndexOf(aChannel);
          if lIndex > -1 then
            TREST.TuneProgram(fClient, aViewer.TunerIndex, fConfig.ChannelMap[lIndex].ItemProgram);
          TREST.StartStream(fClient, aViewer.TunerIndex, ListenIP, lTuner.Sink.Port);
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

function TCetonClient.GetListenIP: String;
begin
  Lock;
  try
    Result := fConfig.ListenIP;
  finally
    Unlock;
  end;

  if Result = '' then
  begin
    TIdStack.IncUsage;
    try
      Result := GStack.LocalAddress;
    finally
      TIdStack.DecUsage;
    end;
  end;
end;

procedure TCetonClient.ReadStream(var aViewer: TCetonViewer; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);
begin
  Lock;
  try
    try
      fTunerList[aViewer.TunerIndex].Sink.Read(aViewer.Reader, aBuffer, aCount, aTimeoutMs);
    except
      on e: EVideoError do
        raise ECetonTunerError.Create(e.Message);
    end;
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

procedure TCetonClient.SetConfig(const aConfig: TCetonConfig);
begin
  Lock;
  try
    if fConfig.TunerAddress <> aConfig.TunerAddress then
    begin
      FreeAndNil(fClient);
      fConfig.Assign(aConfig);
      fClient := TRESTClient.Create(Format('http://%s', [fConfig.TunerAddress]));

      // TODO: Request tuner count from Ceton
      fTunerList.Count := 6;
    end
    else
      fConfig.Assign(aConfig);
  finally
    Unlock;
  end;
end;

procedure TCetonClient.GetConfig(const aConfig: TCetonConfig);
begin
  Lock;
  try
    aConfig.Assign(fConfig);
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
  AddParameter('dest_ip', '192.168.200.1'); // Just set to a dummy ip to help with debugging
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

{ TTuneProgramRequest }

procedure TTuneProgramRequest.DoBeforeExecute;
begin
  inherited;

  AddParameter('instance_id', IntToStr(InstanceID));
  AddParameter('program', IntToStr(ProgramNumber));
end;

constructor TTuneProgramRequest.Create(AOwner: TComponent);
begin
  inherited;

  Method := TRESTRequestMethod.rmPost;
  Resource := 'program_request.cgi';
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
    fSink := TRTPVideoSink.Create(2048, 1000);
  Result := fSink;
end;

procedure TTuner.RemoveSink;
begin
  FreeAndNil(fSink);
end;

{ TCetonVideoStream }

constructor TCetonVideoStream.Create(const aClient: TCetonClient;
  const aTuner: Integer; const aChannel: Integer);
begin
  fClient := aClient;

  fBuffer := TRingBuffer.Create;

  fClient.StartStream(aTuner, aChannel, fViewer);
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

procedure TCetonConfig.AssignTo(Dest: TPersistent);
var
  lDest: TCetonConfig;
begin
  if Dest is TCetonConfig then
  begin
    lDest := TCetonConfig(Dest);

    lDest.fChannelMap.Assign(fChannelMap);
    lDest.fTunerAddress := fTunerAddress;
    lDest.fListenIP := fListenIP;
  end
  else
    inherited;
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
