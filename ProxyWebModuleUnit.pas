unit ProxyWebModuleUnit;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp,
  FMX.Types,
  IdHTTPWebBrokerBridge,
  IDGlobal,
  ProxyServiceModuleUnit,
  Winapi.ShellApi,
  Web.WebReq,

  HDHR,
  Ceton,

  REST.JSON;

type
  TIdHTTPAppChunkedResponse = class(TIdHTTPAppResponse)
  private
  public
    procedure SendChunkedHeader;
    procedure SendChunkedStream(const aStream: TStream);

    function Connected: Boolean;
  end;

  TProxyWebModule = class(TWebModule)
    procedure WebModuleDefaultAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ProxyWebModuleDiscoverActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ProxyWebModuleLineupJSONActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ProxyWebModuleAutoActionAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure ProxyWebModuleLineupXMLActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ProxyWebModuleTunerActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
    procedure HandleException;

    function GetClient: TCetonClient;

    procedure GetConfig(const aConfig: TServiceConfig);
    procedure GetLineup(const aLineup: TLineup);
    procedure SendTuneResponse(const aTuner, aChannel: Integer; const Response: TWebResponse);

    property Client: TCetonClient read GetClient;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TProxyWebModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TIdHTTPAppChunkedResponse }

procedure TIdHTTPAppChunkedResponse.SendChunkedHeader;
begin
  FResponseInfo.TransferEncoding := 'chunked';
  FResponseInfo.WriteHeader;

  FSent := True;
end;

procedure TIdHTTPAppChunkedResponse.SendChunkedStream(const aStream: TStream);
var
  lBuffer: TBytes;
  lSize: Integer;
begin
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
end;

function TIdHTTPAppChunkedResponse.Connected: Boolean;
begin
  Result := FThread.Connection.Connected;
end;

{ TWebModule1 }

procedure TProxyWebModule.WebModuleDefaultAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Log.d('Received unknown request '+Request.PathInfo);

{  if (Request.InternalPathInfo = '') or (Request.InternalPathInfo = '/')then
    Response.Content := ReverseString.Content
  else
    Response.SendRedirect(Request.InternalScriptName + '/');}
end;

procedure TProxyWebModule.ProxyWebModuleDiscoverActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lResponse: TDiscoverResponse;
  lConfig: TServiceConfig;
begin
  Handled := True;
  try
    lResponse := TDiscoverResponse.Create;
    try
      lConfig := TServiceConfig.Create;
      try
        lConfig.Ceton.ChannelMap.Exclude := lConfig.Ceton.ChannelMap.Exclude + [TChannelMapSection.Items];
        GetConfig(lConfig);

        lResponse.BaseURL := Format('http://%s:80', [Client.ListenIP]);
        lResponse.LineupURL := Format('%s/lineup.json', [lResponse.BaseURL]);
        lResponse.DeviceID := IntToHex(lConfig.DeviceID);
      finally
        lConfig.Free;
      end;

      Response.ContentType := 'application/json';
      Response.Content := TJson.ObjectToJsonString(lResponse);
    finally
      lResponse.Free;
    end;

    Log.d('Received discover, Response: '+Response.Content);
  except
    HandleException;
  end;
end;

procedure TProxyWebModule.ProxyWebModuleLineupJSONActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lLineup: TLineup;
begin
  Handled := True;
  try
    Log.d('Received lineup.json request');

    lLineup := TLineup.Create;
    try
      GetLineup(lLineup);

      Response.ContentType := 'application/json';
      Response.Content := lLineup.ToJSON;
    finally
      lLineup.Free;
    end;
  except
    HandleException;
  end;
end;

procedure TProxyWebModule.SendTuneResponse(const aTuner, aChannel: Integer; const Response: TWebResponse);
var
  lStream: TCetonVideoStream;
begin
  // TODO: Grab content type from video stream
  Response.ContentType := 'video/mpeg';
  TIdHTTPAppChunkedResponse(Response).SendChunkedHeader;

  repeat
    lStream := TCetonVideoStream.Create(Client, aTuner, aChannel);
    try
      try
        TIdHTTPAppChunkedResponse(Response).SendChunkedStream(lStream);
      except
        on e: ECetonError do
          // Loop around to try to start stream again
        else
          raise;
      end;
    finally
      lStream.Free;
    end;
  until not TIdHTTPAppChunkedResponse(Response).Connected;
end;

procedure TProxyWebModule.ProxyWebModuleAutoActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lParts: TArray<String>;
  lChannel: Integer;
begin
  Handled := True;
  try
    Log.d('Received tune request '+Request.PathInfo);

    lParts := Request.PathInfo.Split(['/'], TStringSplitOptions.ExcludeEmpty);
    if (Length(lParts) >= 2) and (lParts[1].StartsWith('v',True)) then
    begin
      lChannel := StrToIntDef(lParts[1].Substring(1),0);
      if lChannel > 0 then
      begin
        SendTuneResponse(-1, lChannel, Response);
      end;
    end;
  except
    HandleException;
  end;
end;

function TProxyWebModule.GetClient: TCetonClient;
begin
  Result := ProxyServiceModule.Client;
end;

procedure TProxyWebModule.GetConfig(const aConfig: TServiceConfig);
begin
  ProxyServiceModule.GetConfig(aConfig);
end;

procedure TProxyWebModule.HandleException;
begin
  // Send the response ourselves in an exception handler that eats all exceptions to
  // prevent the default handler from doing it and showing an error message box
  if not Response.Sent then
  begin
    Response.StatusCode := 500;
    Response.Content := Exception(ExceptObject).Message;
    try
      Response.SendResponse;
    except
      // Ignore
    end;
  end;
end;

procedure TProxyWebModule.ProxyWebModuleLineupXMLActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lLineup: TLineup;
begin
  Handled := True;
  try
    Log.d('Received lineup.xml request');

    lLineup := TLineup.Create;
    try
      GetLineup(lLineup);

      Response.ContentType := 'application/xml';
      Response.Content := lLineup.ToXML;
    finally
      lLineup.Free;
    end;
  except
    HandleException;
  end;
end;

procedure TProxyWebModule.GetLineup(const aLineup: TLineup);
var
  lTunerAddress: String;
  lChannelMapItem: TChannelMapItem;
  i: Integer;
  lConfig: TServiceConfig;
begin
  lConfig := TServiceConfig.Create;
  try
    lConfig.Ceton.ChannelMap.Exclude := lConfig.Ceton.ChannelMap.Exclude + [TChannelMapSection.Items];
    GetConfig(lConfig);

    if lConfig.Ceton.ChannelMap.Expired(3) then
      Client.RequestChannelMap;

    lConfig.Ceton.ChannelMap.Exclude := [];
    GetConfig(lConfig);

    lTunerAddress := lConfig.Ceton.TunerAddress;

    for i := 0 to lConfig.Ceton.ChannelMap.Count-1 do
    begin
      lChannelMapItem := lConfig.Ceton.ChannelMap[i];
      if lChannelMapItem.Visible then
        aLineup.Add(IntToStr(lChannelMapItem.Channel), lChannelMapItem.Name, Format('http://%s/auto/v%d', [lTunerAddress, lChannelMapItem.Channel]));
    end;
  finally
    lConfig.Free;
  end;
end;

procedure TProxyWebModule.ProxyWebModuleTunerActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lTuner, lChannel: Integer;
  lParts: TArray<String>;
begin
  Handled := True;
  try
    Log.d('Received tune request '+Request.PathInfo);

    lParts := Request.PathInfo.Split(['/'], TStringSplitOptions.ExcludeEmpty);
    if (Length(lParts) >= 2) and (lParts[0].StartsWith('tuner',True)) and (lParts[1].StartsWith('v',True)) then
    begin
      lTuner := StrToIntDef(lParts[0].Substring(5),-1);
      lChannel := StrToIntDef(lParts[1].Substring(1),0);
      if (lTuner > -1) and (lChannel > 0) then
      begin
        SendTuneResponse(lTuner, lChannel, Response);
      end;
    end;
  except
    HandleException;
  end;
end;

initialization
finalization
  Web.WebReq.FreeWebModules;

end.

