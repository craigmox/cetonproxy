unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, Datasnap.DSHTTPCommon,
  Datasnap.DSHTTPWebBroker, Datasnap.DSServer,
  Web.WebFileDispatcher, Web.HTTPProd,
  DataSnap.DSAuth,
  Datasnap.DSProxyJavaScript, IPPeerServer, Datasnap.DSMetadata,
  Datasnap.DSServerMetadata, Datasnap.DSClientMetadata, Datasnap.DSCommonServer,
  Datasnap.DSHTTP,
  FMX.Types,
  IdHTTPWebBrokerBridge,
  IDGlobal,
  ProxyService,

  hdhr, ceton,

  REST.Client,
  REST.JSON,
  System.JSON;

type
  TIdHTTPAppChunkedResponse = class(TIdHTTPAppResponse)
  private
  public
    procedure SendChunkedStream(const aStream: TStream);
  end;

  TWebModule1 = class(TWebModule)
    DSHTTPWebDispatcher1: TDSHTTPWebDispatcher;
    ServerFunctionInvoker: TPageProducer;
    ReverseString: TPageProducer;
    WebFileDispatcher1: TWebFileDispatcher;
    DSProxyGenerator1: TDSProxyGenerator;
    DSServerMetaDataProvider1: TDSServerMetaDataProvider;
    procedure ServerFunctionInvokerHTMLTag(Sender: TObject; Tag: TTag;
      const TagString: string; TagParams: TStrings; var ReplaceText: string);
    procedure WebModuleDefaultAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleBeforeDispatch(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebFileDispatcher1BeforeDispatch(Sender: TObject;
      const AFileName: string; Request: TWebRequest; Response: TWebResponse;
      var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModule1DiscoverActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1LineupActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1AutoActionAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleException(Sender: TObject; E: Exception;
      var Handled: Boolean);
  private
    { Private declarations }
    FServerFunctionInvokerAction: TWebActionItem;
    function AllowServerFunctionInvoker: Boolean;

    function Client: TCetonClient;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses ServerMethodsUnit1, ServerContainerUnit1, Web.WebReq;

procedure TWebModule1.ServerFunctionInvokerHTMLTag(Sender: TObject; Tag: TTag;
  const TagString: string; TagParams: TStrings; var ReplaceText: string);
begin
  if SameText(TagString, 'urlpath') then
    ReplaceText := string(Request.InternalScriptName)
  else if SameText(TagString, 'port') then
    ReplaceText := IntToStr(Request.ServerPort)
  else if SameText(TagString, 'host') then
    ReplaceText := string(Request.Host)
  else if SameText(TagString, 'classname') then
    ReplaceText := ServerMethodsUnit1.TServerMethods1.ClassName
  else if SameText(TagString, 'loginrequired') then
    if DSHTTPWebDispatcher1.AuthenticationManager <> nil then
      ReplaceText := 'true'
    else
      ReplaceText := 'false'
  else if SameText(TagString, 'serverfunctionsjs') then
    ReplaceText := string(Request.InternalScriptName) + '/js/serverfunctions.js'
  else if SameText(TagString, 'servertime') then
    ReplaceText := DateTimeToStr(Now)
  else if SameText(TagString, 'serverfunctioninvoker') then
    if AllowServerFunctionInvoker then
      ReplaceText :=
      '<div><a href="' + string(Request.InternalScriptName) +
      '/ServerFunctionInvoker" target="_blank">Server Functions</a></div>'
    else
      ReplaceText := '';
end;

procedure TWebModule1.WebModuleDefaultAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Log.d('Received unknown request '+Request.PathInfo);

{  if (Request.InternalPathInfo = '') or (Request.InternalPathInfo = '/')then
    Response.Content := ReverseString.Content
  else
    Response.SendRedirect(Request.InternalScriptName + '/');}
end;

procedure TWebModule1.WebModuleBeforeDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  if FServerFunctionInvokerAction <> nil then
    FServerFunctionInvokerAction.Enabled := AllowServerFunctionInvoker;
end;

function TWebModule1.AllowServerFunctionInvoker: Boolean;
begin
  Result := (Request.RemoteAddr = '127.0.0.1') or
    (Request.RemoteAddr = '0:0:0:0:0:0:0:1') or (Request.RemoteAddr = '::1');
end;

procedure TWebModule1.WebFileDispatcher1BeforeDispatch(Sender: TObject;
  const AFileName: string; Request: TWebRequest; Response: TWebResponse;
  var Handled: Boolean);
var
  D1, D2: TDateTime;
begin
  Handled := False;
  if SameFileName(ExtractFileName(AFileName), 'serverfunctions.js') then
    if not FileExists(AFileName) or (FileAge(AFileName, D1) and FileAge(WebApplicationFileName, D2) and (D1 < D2)) then
    begin
      DSProxyGenerator1.TargetDirectory := ExtractFilePath(AFileName);
      DSProxyGenerator1.TargetUnitName := ExtractFileName(AFileName);
      DSProxyGenerator1.Write;
    end;
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FServerFunctionInvokerAction := ActionByName('ServerFunctionInvokerAction');
  DSServerMetaDataProvider1.Server := DSServer;
  DSHTTPWebDispatcher1.Server := DSServer;
  if DSServer.Started then
  begin
    DSHTTPWebDispatcher1.DbxContext := DSServer.DbxContext;
    DSHTTPWebDispatcher1.Start;
  end;
end;

procedure TWebModule1.WebModule1DiscoverActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lResponse: TDiscoverResponse;
begin
  lResponse := TDiscoverResponse.Create;
  try
    Response.Content := TJson.ObjectToJsonString(lResponse);
  finally
    lResponse.Free;
  end;

  Log.d('Received discover, Response: '+Response.Content);
end;

procedure TWebModule1.WebModule1LineupActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lChannelMap: TChannelMap;
  lChannelMapItem: TChannelMapItem;
  lLineup: TLineup;
  lTunerAddress: String;
  i: Integer;
begin
  Handled := True;
  Log.d('Received lineup request');

  if Client.ChannelMapExpired(3) then
    Client.RequestChannelMap;

  lChannelMap := TChannelMap.Create;
  try
    Client.GetChannelMap(lChannelMap);

    lTunerAddress := Client.TunerAddress;

    lLineup := TLineup.Create;
    try
      for i := 0 to lChannelMap.Count-1 do
      begin
        lChannelMapItem := lChannelMap[i];
        if lChannelMapItem.Visible then
          lLineup.Add(IntToStr(lChannelMapItem.Channel), lChannelMapItem.Name, Format('http://%s/auto/v%d', [lTunerAddress, lChannelMapItem.Channel]));
      end;

      Response.Content := lLineup.ToJSON;
    finally
      lLineup.Free;
    end;
  finally
    lChannelMap.Free;
  end;
end;

procedure TWebModule1.WebModule1AutoActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lChannel: Integer;
  lStream: TCetonVideoStream;
begin
  Handled := True;
  Log.d('Received tune request '+Request.PathInfo);

  lChannel := StrToIntDef(Request.PathInfo.Substring(7),0);
  if lChannel > 0 then
  begin
    Response.ContentType := 'video/mpeg';

    lStream := TCetonVideoStream.Create(Client, lChannel);
    try
      TIdHTTPAppChunkedResponse(Response).SendChunkedStream(lStream);
    finally
      lStream.Free;
    end;
  end;
end;

function TWebModule1.Client: TCetonClient;
begin
  Result := ProxyServiceModule.Client;
end;

{ TIdHTTPAppChunkedResponse }

procedure TIdHTTPAppChunkedResponse.SendChunkedStream(const aStream: TStream);
var
  lBuffer: TBytes;
  lSize: Integer;
begin
  FResponseInfo.TransferEncoding := 'chunked';
  FResponseInfo.WriteHeader;

  SetLength(lBuffer, 8192);
  repeat
    lSize := aStream.Read(lBuffer, Length(lBuffer));
    if lSize > 0 then
    begin
      FThread.Connection.IOHandler.WriteLn(IntToHex(lSize, 1));
      FThread.Connection.IOHandler.Write(TIdBytes(lBuffer), lSize);
      FThread.Connection.IOHandler.WriteLn;
    end;
  until lSize = 0;

  FThread.Connection.IOHandler.WriteLn('0');
  FThread.Connection.IOHandler.WriteLn;

  FSent := True;
end;

procedure TWebModule1.WebModuleException(Sender: TObject; E: Exception;
  var Handled: Boolean);
begin
  Handled := True;
  if not TCustomWebDispatcher(Sender).Response.Sent then
  begin
    try
      TCustomWebDispatcher(Sender).Response.SendResponse;
    except
      // Ignore
    end;
  end;
end;

initialization
finalization
  Web.WebReq.FreeWebModules;

end.

