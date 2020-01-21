unit ProxyWebModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  Web.WebReq,
  FMX.Types,
  IdHTTPWebBrokerBridge,
  IDGlobal,
  REST.JSON,

  ProxyServiceModuleUnit,

  HDHR,
  Ceton;

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
    procedure WebModuleCreate(Sender: TObject);
  private
    { Private declarations }
    fConfigManager: IServiceConfigManager;
    fClient: TCetonClient;

    procedure HandleException;

    procedure GetLineup(const aLineup: TLineup);
    procedure SendTuneResponse(const aTuner, aChannel: Integer; const Response: TWebResponse);

    property ConfigManager: IServiceConfigManager read fConfigManager;
    property Client: TCetonClient read fClient;
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
  Log.d('Received unknown request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);

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
  lListenIP: String;
begin
  Handled := True;
  try
    lResponse := TDiscoverResponse.Create;
    try
      lListenIP := Client.ListenIP;

      ConfigManager.LockConfig(lConfig);
      try
        lResponse.BaseURL := Format('http://%s:%d', [lListenIP, lConfig.HTTPPort]);
        lResponse.DeviceID := IntToHex(lConfig.DeviceID);
      finally
        ConfigManager.UnlockConfig(lConfig);
      end;

      lResponse.LineupURL := Format('%s/lineup.json', [lResponse.BaseURL]);

      Response.ContentType := 'application/json';
      Response.Content := TJson.ObjectToJsonString(lResponse);
    finally
      lResponse.Free;
    end;

    Log.d('Received discover from %s, Response: %s', [Request.RemoteAddr, Response.Content]);
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
    Log.d('Received lineup.json request from %s', [Request.RemoteAddr]);
    try
      lLineup := TLineup.Create;
      try
        GetLineup(lLineup);

        Response.ContentType := 'application/json';
        Response.Content := lLineup.ToJSON;
      finally
        lLineup.Free;
      end;
    finally
      Log.d('Finished lineup.json request from %s', [Request.RemoteAddr]);
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
    // If Create here
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
    Log.d('Received tune request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    try
      lParts := Request.PathInfo.Split(['/'], TStringSplitOptions.ExcludeEmpty);
      if (Length(lParts) >= 2) and (lParts[1].StartsWith('v',True)) then
      begin
        lChannel := StrToIntDef(lParts[1].Substring(1),0);
        if lChannel > 0 then
        begin
          SendTuneResponse(-1, lChannel, Response);
        end;
      end;
    finally
      Log.d('Finished tune request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    end;
  except
    HandleException;
  end;
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
    Log.d('Received lineup.xml request from %s', [Request.RemoteAddr]);
    try
      lLineup := TLineup.Create;
      try
        GetLineup(lLineup);

        Response.ContentType := 'application/xml';
        Response.Content := lLineup.ToXML;
      finally
        lLineup.Free;
      end;
    finally
      Log.d('Finished lineup.json request from %s', [Request.RemoteAddr]);
    end;
  except
    HandleException;
  end;
end;

procedure TProxyWebModule.GetLineup(const aLineup: TLineup);
var
  lChannelMapItem: TChannelMapItem;
  i: Integer;
  lConfig: TServiceConfig;
  lCetonConfig: TCetonConfig;
  lRequestChannels: Boolean;
  lTunerAddress: String;
  lChannelMap: TChannelMap;
begin
  ConfigManager.LockConfig(lConfig);
  try
    lRequestChannels := lConfig.Ceton.ChannelMap.Expired(3);
  finally
    ConfigManager.UnlockConfig(lConfig);
  end;

  if lRequestChannels then
  begin
    Client.RequestChannelMap;

    lCetonConfig := TCetonConfig.Create;
    try
      Client.GetConfig(lCetonConfig);

      ConfigManager.LockConfig(lConfig);
      try
        lConfig.Ceton.ChannelMap.Assign(lCetonConfig.ChannelMap, False);
      finally
        ConfigManager.UnlockConfig(lConfig);
      end;
    finally
      lCetonConfig.Free;
    end;

    ConfigManager.Changed(Client, [TServiceConfigSection.Channels]);
  end;

  lChannelMap := TChannelMap.create;
  try
    ConfigManager.LockConfig(lConfig);
    try
      lTunerAddress := lConfig.Ceton.TunerAddress;
      lChannelMap.Assign(lConfig.Ceton.ChannelMap, False);
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    for i := 0 to lChannelMap.Count-1 do
    begin
      lChannelMapItem := lChannelMap[i];
      if lChannelMapItem.Visible then
        aLineup.Add(IntToStr(lChannelMapItem.Channel), lChannelMapItem.Name, Format('http://%s/auto/v%d', [lTunerAddress, lChannelMapItem.Channel]));
    end;
  finally
    lChannelMap.Free;
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
    Log.d('Received tune request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    try
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
    finally
      Log.d('Finished tune request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    end;
  except
    HandleException;
  end;
end;

procedure TProxyWebModule.WebModuleCreate(Sender: TObject);
begin
  fConfigManager := ProxyServiceModule.ConfigManager;
  fClient := ProxyServiceModule.Client;
end;

initialization
finalization
  Web.WebReq.FreeWebModules;

end.

