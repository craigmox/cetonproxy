unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp,
  FMX.Types,
  IdHTTPWebBrokerBridge,
  IDGlobal,
  ProxyService,
  Winapi.ShellApi,
  Web.WebReq,

  hdhr, ceton,

  REST.JSON;

type
  TIdHTTPAppChunkedResponse = class(TIdHTTPAppResponse)
  private
  public
    procedure SendChunkedHeader;
    procedure SendChunkedStream(const aStream: TStream);

    function Connected: Boolean;
  end;

  TWebModule1 = class(TWebModule)
    procedure WebModuleDefaultAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1DiscoverActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1LineupActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1AutoActionAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
    procedure HandleException;

    function Client: TCetonClient;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

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

procedure TWebModule1.WebModuleDefaultAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Log.d('Received unknown request '+Request.PathInfo);

{  if (Request.InternalPathInfo = '') or (Request.InternalPathInfo = '/')then
    Response.Content := ReverseString.Content
  else
    Response.SendRedirect(Request.InternalScriptName + '/');}
end;

procedure TWebModule1.WebModule1DiscoverActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lResponse: TDiscoverResponse;
begin
  Handled := True;
  try
    lResponse := TDiscoverResponse.Create;
    try
      Response.Content := TJson.ObjectToJsonString(lResponse);
    finally
      lResponse.Free;
    end;

    Log.d('Received discover, Response: '+Response.Content);
  except
    HandleException;
  end;
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
  try
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
  except
    HandleException;
  end;
end;

procedure TWebModule1.WebModule1AutoActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lChannel: Integer;
  lStream: TCetonVideoStream;
begin
  Handled := True;
  try
    Log.d('Received tune request '+Request.PathInfo);

    lChannel := StrToIntDef(Request.PathInfo.Substring(7),0);
    if lChannel > 0 then
    begin
      Response.ContentType := 'video/mpeg';
      TIdHTTPAppChunkedResponse(Response).SendChunkedHeader;

      repeat
        lStream := TCetonVideoStream.Create(Client, lChannel);
        try
          try
            TIdHTTPAppChunkedResponse(Response).SendChunkedStream(lStream);
          except
            on e: ECetonTunerError do
              // Loop around to try to start stream again
            else
              raise;
          end;
        finally
          lStream.Free;
        end;
      until not TIdHTTPAppChunkedResponse(Response).Connected;
    end;
  except
    HandleException;
  end;
end;

function TWebModule1.Client: TCetonClient;
begin
  Result := ProxyServiceModule.Client;
end;

procedure TWebModule1.HandleException;
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

initialization
finalization
  Web.WebReq.FreeWebModules;

end.

