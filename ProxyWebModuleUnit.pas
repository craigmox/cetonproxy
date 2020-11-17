unit ProxyWebModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Math,
  Web.HTTPApp,
  Web.WebReq,
  FMX.Types,
  IdHTTPWebBrokerBridge,
  IdGlobal,
  IdStack,
  REST.JSON,
  REST.Json.Types,
  REST.JsonReflect,
  System.JSON,

  ProxyServiceModuleUnit,
  ProxyServerModuleUnit,

  HDHR,
  Ceton,

  LogUtils,
  SocketUtils;

type
  TChunkedStreamPacketCallback = reference to procedure(const aPacketSize: Integer; var aContinue: Boolean);

  TIdHTTPAppChunkedResponse = class(TIdHTTPAppResponse)
  private
  public
    procedure SendChunkedHeader;
    procedure SendChunkedStream(const aStream: TStream; const aPacketCallback: TChunkedStreamPacketCallback = nil);

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
    procedure ProxyWebModuleLineupM3UActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ProxyWebModuleTunerActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure ProxyWebModuleLineupStatusActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ProxyWebModuleDeviceXMLActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ProxyWebModuleSpeedTestActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ProxyWebModuleVideoTestActionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
    fConfigManager: IServiceConfigManager;
    fClient: TCetonClient;

    procedure HandleException;

    function GetAddress(const aRequest: TWebRequest): String;

    procedure CheckClientChannelMapUpdated;

    procedure GetLineup(const aLineup: TLineup);
    procedure SendTuneResponse(const aTuner, aChannel: Integer; const aAllowedDisabledTuners: Boolean; const aTest: Boolean; const aDurationSec: Integer; const aRemux: Boolean; const Response: TWebResponse);

    function CreateDeviceXML: String;

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

procedure TIdHTTPAppChunkedResponse.SendChunkedStream(const aStream: TStream; const aPacketCallback: TChunkedStreamPacketCallback = nil);
var
  lBuffer: TBytes;
  lSize: Integer;
  lContinue: Boolean;
begin
  FThread.Binding.UseNagle := False;

  FThread.Connection.IOHandler.WriteBufferOpen;

  lContinue := True;

  SetLength(lBuffer, 8192);
  repeat
    lSize := aStream.Read(lBuffer, Length(lBuffer));
    if lSize > 0 then
    begin
      FThread.Connection.IOHandler.WriteLn(IntToHex(lSize, 1));
      FThread.Connection.IOHandler.Write(TIdBytes(lBuffer), lSize);
      FThread.Connection.IOHandler.WriteLn;
      FThread.Connection.IOHandler.WriteBufferFlush;

      if Assigned(aPacketCallback) then
        aPacketCallback(lSize, lContinue);
    end;
  until (lSize = 0) or (not lContinue);

  FThread.Connection.IOHandler.WriteLn('0');
  FThread.Connection.IOHandler.WriteLn;
  FThread.Connection.IOHandler.WriteBufferFlush;
end;

function TIdHTTPAppChunkedResponse.Connected: Boolean;
begin
  Result := FThread.Connection.Connected;
end;

{ TWebModule1 }

procedure TProxyWebModule.WebModuleDefaultAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  TLogger.LogFmt(cLogDefault, 'Received unknown request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);

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
  lAddress: String;
  lTunerCount: Integer;
begin
  Handled := True;
  try
    lResponse := TDiscoverResponse.Create;
    try
      lAddress := GetAddress(Request);
      lTunerCount := Client.EnabledTunerCount;

      ConfigManager.LockConfig(lConfig);
      try
        lResponse.BaseURL := Format('http://%s:%d', [lAddress, lConfig.ExternalHTTPPort]);
        lResponse.DeviceID := IntToHex(lConfig.DeviceID);
        lResponse.TunerCount := lTunerCount;
      finally
        ConfigManager.UnlockConfig(lConfig);
      end;

      lResponse.LineupURL := Format('%s/lineup.json', [lResponse.BaseURL]);

      Response.ContentType := 'application/json';
      Response.Content := TJson.ObjectToJsonString(lResponse);
    finally
      lResponse.Free;
    end;

    TLogger.LogFmt(cLogDiscovery, 'Received discover from %s, Response: %s', [Request.RemoteAddr, Response.Content]);
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
    TLogger.LogFmt(cLogDefault, 'Received lineup.json request from %s', [Request.RemoteAddr]);
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
      TLogger.LogFmt(cLogDefault, 'Finished lineup.json request from %s', [Request.RemoteAddr]);
    end;
  except
    HandleException;
  end;
end;

procedure TProxyWebModule.SendTuneResponse(const aTuner, aChannel: Integer; const aAllowedDisabledTuners: Boolean; const aTest: Boolean; const aDurationSec: Integer; const aRemux: Boolean; const Response: TWebResponse);
var
  lStream: TCetonVideoStream;
  lStatsWatch, lDurationWatch: TStopWatch;
  lStatsArray: TArray<TTunerStats>;
begin
  // TODO: Grab content type from video stream
  Response.ContentType := 'video/mpeg';
  TIdHTTPAppChunkedResponse(Response).SendChunkedHeader;

  repeat
    lStatsWatch := TStopWatch.StartNew;
    lDurationWatch := TStopWatch.StartNew;

    // If Create here
    lStream := TCetonVideoStream.Create(Client, aTuner, aChannel, aAllowedDisabledTuners, aRemux);
    try
      CheckClientChannelMapUpdated;

      lStream.OnCheckAbort :=
        procedure(var aAbort: Boolean)
        begin
          aAbort := not TIdHTTPAppChunkedResponse(Response).Connected;
        end;

      try
        TIdHTTPAppChunkedResponse(Response).SendChunkedStream(lStream,
          procedure(const aPacketSize: Integer; var aContinue: Boolean)
          var
            lTunerStats: PTunerStats;
            lClientstats: PTunerClientStats;
          begin
            if (aTest) and (lStatsWatch.ElapsedMilliseconds >= 1000) then
            begin
              lStatsArray := Client.GetTunerStats;
              lTunerStats := @lStatsArray[lStream.Viewer.TunerIndex];
              lClientStats := lTunerStats.Clients[lStream.Viewer.Reader.ReaderIndex];

              if Assigned(lClientStats) then
                TLogger.LogFmt(cLogDefault, 'Sending video to client %d: Tuner %d, Channel %d, Program %d, From tuner %d packets at %0.2fMbps, Buffer free %0.0f%%, To encoder %d packets at %0.2fMbps, To client %d bytes at %0.2fMbps, Lost packets %d', [
                  lStream.Viewer.TunerIndex, lStream.Viewer.Reader.ReaderIndex, lTunerStats.Channel, lStream.ProgramFilter, lTunerStats.InPackets, lTunerStats.InMeter.GetBytesPerSecond(True)*8/1000000, lTunerStats.BufferFree*100, lClientStats.OutPackets, lClientStats.OutMeter.GetBytesPerSecond(True)*8/1000000, lStream.Stats.OutBytes, lStream.Stats.OutMeter.GetBytesPerSecond(True)*8/1000000, lClientStats.Lost])
              else
                TLogger.LogFmt(cLogDefault, 'Sending video to client %d: Tuner %d, Channel %d, Program %d, From tuner %d packets at %0.2fMbps, Buffer free %0.0f%%, To client %d bytes at %0.2fMbps', [
                  lStream.Viewer.TunerIndex, lStream.Viewer.Reader.ReaderIndex, lTunerStats.Channel, lStream.ProgramFilter, lTunerStats.InPackets, lTunerStats.InMeter.GetBytesPerSecond(True)*8/1000000, lTunerStats.BufferFree*100, lStream.Stats.OutBytes, lStream.Stats.OutMeter.GetBytesPerSecond(True)*8/1000000]);

              lStatsWatch.Reset;
              lStatsWatch.Start;
            end;

            if (aDurationSec > 0) and (lDurationWatch.ElapsedMilliseconds >= aDurationSec*1000) then
              aContinue := False;
          end);

        Exit;
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
    TLogger.LogFmt(cLogDefault, 'Received tune request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    try
      lParts := Request.PathInfo.Split(['/'], TStringSplitOptions.ExcludeEmpty);
      if (Length(lParts) >= 2) and (lParts[1].StartsWith('v',True)) then
      begin
        lChannel := StrToIntDef(lParts[1].Substring(1),0);
        if lChannel > 0 then
        begin
          SendTuneResponse(-1, lChannel, False, False, 0, True, Response);
        end;
      end;
    finally
      TLogger.LogFmt(cLogDefault, 'Finished tune request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    end;
  except
    HandleException;
  end;
end;

procedure TProxyWebModule.HandleException;
var
  lError: Boolean;
begin
  // Ignore
  lError := True;
  if Exception(ExceptObject) is EIdSocketError then
  begin
    case EIdSocketError(ExceptObject).LastError of
      10053, 10054: begin
        // 10053: Software cause connection abort
        // 10054: Socket Error
        lError := False;
      end;
    end;
  end
  else if Exception(ExceptObject) is ECetonClosedError then
  begin
    // Ignore client-initiated close exceptions
    lError := False;
  end;

  if lError then
    TLogger.LogErrorFmt(cLogDefault, 'Service handler error: %s', [Exception(ExceptObject).Message])
  else
    TLogger.LogFmt(cLogDefault, 'Service handler error: %s', [Exception(ExceptObject).Message]);

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
    TLogger.LogFmt(cLogDefault, 'Received lineup.xml request from %s', [Request.RemoteAddr]);
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
      TLogger.LogFmt(cLogDefault, 'Finished lineup.json request from %s', [Request.RemoteAddr]);
    end;
  except
    HandleException;
  end;
end;

procedure TProxyWebModule.ProxyWebModuleLineupM3UActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lLineup: TLineup;
begin
  Handled := True;
  try
    TLogger.LogFmt(cLogDefault, 'Received lineup.m3u request from %s', [Request.RemoteAddr]);
    try
      lLineup := TLineup.Create;
      try
        GetLineup(lLineup);

        Response.ContentType := 'audio/x-mpegurl';
        Response.Content := lLineup.ToM3U;
      finally
        lLineup.Free;
      end;
    finally
      TLogger.LogFmt(cLogDefault, 'Finished lineup.m3u request from %s', [Request.RemoteAddr]);
    end;
  except
    HandleException;
  end;
end;

procedure TProxyWebModule.CheckClientChannelMapUpdated;
var
  lCetonConfig: TCetonConfig;
  lConfig: TServiceConfig;
  lDateTime: TDateTime;
begin
  lDateTime := Client.ChannelMapRequestDateTime;

  ConfigManager.LockConfig(lConfig);
  try
    if CompareValue(lDateTime, lConfig.Ceton.ChannelMap.RequestDateTime) <= 0 then
      Exit;
  finally
    ConfigManager.UnlockConfig(lConfig);
  end;

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

procedure TProxyWebModule.GetLineup(const aLineup: TLineup);
var
  lChannelMapItem: TChannelMapItem;
  i: Integer;
  lConfig: TServiceConfig;
  lRequestChannels: Boolean;
  lAddress: String;
  lPort: Integer;
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

    CheckClientChannelMapUpdated;
  end;

  lChannelMap := TChannelMap.create;
  try
    lAddress := GetAddress(Request);

    ConfigManager.LockConfig(lConfig);
    try
      lPort := lConfig.ExternalHTTPPort;
      lChannelMap.Assign(lConfig.Ceton.ChannelMap, False);
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    for i := 0 to lChannelMap.Count-1 do
    begin
      lChannelMapItem := lChannelMap[i];
      if lChannelMapItem.Visible then
        aLineup.Add(IntToStr(lChannelMapItem.Channel), lChannelMapItem.Name, Format('http://%s:%d/auto/v%d', [lAddress, lPort, lChannelMapItem.Channel]));
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
    TLogger.LogFmt(cLogDefault, 'Received tune request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    try
      lParts := Request.PathInfo.Split(['/'], TStringSplitOptions.ExcludeEmpty);
      if (Length(lParts) >= 2) and (lParts[0].StartsWith('tuner',True)) and (lParts[1].StartsWith('v',True)) then
      begin
        lTuner := StrToIntDef(lParts[0].Substring(5),-1);
        lChannel := StrToIntDef(lParts[1].Substring(1),0);
        if (lTuner > -1) and (lChannel > 0) then
        begin
          SendTuneResponse(lTuner, lChannel, False, False, 0, True, Response);
        end;
      end;
    finally
      TLogger.LogFmt(cLogDefault, 'Finished tune request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
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

procedure TProxyWebModule.ProxyWebModuleLineupStatusActionAction(
  Sender: TObject; Request: TWebRequest; Response: TWebResponse;
  var Handled: Boolean);
begin
  Handled := True;
  try
    TLogger.LogFmt(cLogDefault, 'Received lineup status request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    try
      Response.ContentType := 'application/json';
      Response.Content := '{"ScanInProgress":0,"ScanPossible":1,"Source":"Cable","SourceList":["Cable"]}';
    finally
      TLogger.LogFmt(cLogDefault, 'Finished lineup status request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    end;
  except
    HandleException;
  end;
end;

procedure TProxyWebModule.ProxyWebModuleDeviceXMLActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Handled := True;
  try
    TLogger.LogFmt(cLogDefault, 'Received device.xml request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    try
      Response.ContentType := 'application/xml';
      Response.Content := CreateDeviceXML;
    finally
      TLogger.LogFmt(cLogDefault, 'Finished device.xml request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    end;
  except
    HandleException;
  end;
end;

{

<?xml version="1.0" encoding="utf-8"?>
<root xmlns="urn:schemas-upnp-org:device-1-0" xmlns:pnpx="http://schemas.microsoft.com/windows/pnpx/2005/11" xmlns:df="http://schemas.microsoft.com/windows/2008/09/devicefoundation">
  <specVersion>
    <major>1</major>
    <minor>0</minor>
  </specVersion>
  <device>
    <pnpx:X_hardwareId>VEN_0115&amp;DEV_1310&amp;SUBSYS_0001&amp;REV_0003 VEN_0115&amp;DEV_1310&amp;SUBSYS_0001 VEN_0115&amp;DEV_1310</pnpx:X_hardwareId>
    <pnpx:X_deviceCategory>MediaDevices</pnpx:X_deviceCategory>
    <df:X_deviceCategory>Multimedia</df:X_deviceCategory>
    <deviceType>urn:schemas-dkeystone-com:device:SecureContainer:1</deviceType>
    <friendlyName>HDHomeRun DRI Tuner XXXXXXXX</friendlyName>
    <presentationURL>/</presentationURL>
    <manufacturer>Silicondust</manufacturer>
    <manufacturerURL>http://www.silicondust.com/</manufacturerURL>
    <modelDescription>HDHR3-CC HDHomeRun PRIME</modelDescription>
    <modelName>HDHomeRun PRIME</modelName>
    <modelNumber>HDHR3-CC</modelNumber>
    <modelURL>http://www.silicondust.com/</modelURL>
    <serialNumber>XXXXXXXX</serialNumber>
    <UDN>uuid:473366D2-A765-3D61-B466-E73B254C632B</UDN>
    <serviceList>
      <service>
        <serviceType>urn:schemas-microsoft-com:service:OCTAMessage:1</serviceType>
        <serviceId>urn:microsoft-com:serviceId:OCTAMessage</serviceId>
        <SCPDURL>/dri/OCTAMessage.xml</SCPDURL>
        <controlURL>http://192.168.1.43:80/dri/OCTAMessage</controlURL>
        <eventSubURL>http://192.168.1.43:80/dri/OCTAMessage</eventSubURL>
      </service>
    </serviceList>
    <deviceList>
      <device>
        <deviceType>urn:schemas-upnp-org:device:MediaServer:1</deviceType>
        <friendlyName>HDHomeRun Prime Tuner XXXXXXXX-0</friendlyName>
        <presentationURL>/</presentationURL>
        <manufacturer>Silicondust</manufacturer>
        <modelName>HDHomeRun PRIME</modelName>
        <UDN>uuid:C94A4A19-9F09-3DC8-AF60-1E918231F33B</UDN>
        <serviceList>
          <service>
            <serviceType>urn:schemas-upnp-org:service:ConnectionManager:1</serviceType>
            <serviceId>urn:upnp-org:serviceId:urn:schemas-upnp-org:service:ConnectionManager</serviceId>
            <SCPDURL>/dri/ConnectionManager.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/ConnectionManager</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/ConnectionManager</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-upnp-org:service:AVTransport:1</serviceType>
            <serviceId>urn:upnp-org:serviceId:urn:schemas-upnp-org:service:AVTransport</serviceId>
            <SCPDURL>/dri/AVTransport.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/AVTransport</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/AVTransport</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-microsoft-com:service:WMDRM:1</serviceType>
            <serviceId>urn:microsoft-com:serviceId:urn:schemas-microsoft-com:service:WMDRM</serviceId>
            <SCPDURL>/dri/WMDRM.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/WMDRM</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/WMDRM</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Security:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Security</serviceId>
            <SCPDURL>/dri/Security.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/Security</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/Security</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:CAS:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:CAS</serviceId>
            <SCPDURL>/dri/CAS.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/CAS</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/CAS</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Encoder:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Encoder</serviceId>
            <SCPDURL>/dri/Encoder.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/Encoder</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/Encoder</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Tuner:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Tuner</serviceId>
            <SCPDURL>/dri/Tuner.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/Tuner</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/Tuner</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:FDC:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:FDC</serviceId>
            <SCPDURL>/dri/FDC.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/FDC</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/FDC</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Mux:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Mux</serviceId>
            <SCPDURL>/dri/Mux.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/Mux</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/Mux</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Diag:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Diag</serviceId>
            <SCPDURL>/dri/Diag.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/Diag</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/Diag</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:UserActivity:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:UserActivity</serviceId>
            <SCPDURL>/dri/UserActivity.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/UserActivity</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/UserActivity</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-silicondust-com:service:EAS:1</serviceType>
            <serviceId>urn:silicondust-com:serviceId:urn:schemas-silicondust-com:service:EAS</serviceId>
            <SCPDURL>/dri/EAS.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner0/EAS</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner0/EAS</eventSubURL>
          </service>
        </serviceList>
      </device>
      <device>
        <deviceType>urn:schemas-upnp-org:device:MediaServer:1</deviceType>
        <friendlyName>HDHomeRun Prime Tuner XXXXXXXX-1</friendlyName>
        <presentationURL>/</presentationURL>
        <manufacturer>Silicondust</manufacturer>
        <modelName>HDHomeRun PRIME</modelName>
        <UDN>uuid:068CCDE0-37DB-3DBE-87F4-C48FDB88CA95</UDN>
        <serviceList>
          <service>
            <serviceType>urn:schemas-upnp-org:service:ConnectionManager:1</serviceType>
            <serviceId>urn:upnp-org:serviceId:urn:schemas-upnp-org:service:ConnectionManager</serviceId>
            <SCPDURL>/dri/ConnectionManager.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/ConnectionManager</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/ConnectionManager</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-upnp-org:service:AVTransport:1</serviceType>
            <serviceId>urn:upnp-org:serviceId:urn:schemas-upnp-org:service:AVTransport</serviceId>
            <SCPDURL>/dri/AVTransport.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/AVTransport</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/AVTransport</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-microsoft-com:service:WMDRM:1</serviceType>
            <serviceId>urn:microsoft-com:serviceId:urn:schemas-microsoft-com:service:WMDRM</serviceId>
            <SCPDURL>/dri/WMDRM.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/WMDRM</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/WMDRM</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Security:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Security</serviceId>
            <SCPDURL>/dri/Security.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/Security</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/Security</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:CAS:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:CAS</serviceId>
            <SCPDURL>/dri/CAS.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/CAS</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/CAS</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Encoder:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Encoder</serviceId>
            <SCPDURL>/dri/Encoder.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/Encoder</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/Encoder</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Tuner:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Tuner</serviceId>
            <SCPDURL>/dri/Tuner.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/Tuner</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/Tuner</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:FDC:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:FDC</serviceId>
            <SCPDURL>/dri/FDC.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/FDC</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/FDC</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Mux:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Mux</serviceId>
            <SCPDURL>/dri/Mux.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/Mux</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/Mux</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Diag:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Diag</serviceId>
            <SCPDURL>/dri/Diag.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/Diag</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/Diag</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:UserActivity:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:UserActivity</serviceId>
            <SCPDURL>/dri/UserActivity.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/UserActivity</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/UserActivity</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-silicondust-com:service:EAS:1</serviceType>
            <serviceId>urn:silicondust-com:serviceId:urn:schemas-silicondust-com:service:EAS</serviceId>
            <SCPDURL>/dri/EAS.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner1/EAS</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner1/EAS</eventSubURL>
          </service>
        </serviceList>
      </device>
      <device>
        <deviceType>urn:schemas-upnp-org:device:MediaServer:1</deviceType>
        <friendlyName>HDHomeRun Prime Tuner XXXXXXXX-2</friendlyName>
        <presentationURL>/</presentationURL>
        <manufacturer>Silicondust</manufacturer>
        <modelName>HDHomeRun PRIME</modelName>
        <UDN>uuid:9C66EFAF-3230-3896-8112-7FD8E6FE7591</UDN>
        <serviceList>
          <service>
            <serviceType>urn:schemas-upnp-org:service:ConnectionManager:1</serviceType>
            <serviceId>urn:upnp-org:serviceId:urn:schemas-upnp-org:service:ConnectionManager</serviceId>
            <SCPDURL>/dri/ConnectionManager.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/ConnectionManager</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/ConnectionManager</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-upnp-org:service:AVTransport:1</serviceType>
            <serviceId>urn:upnp-org:serviceId:urn:schemas-upnp-org:service:AVTransport</serviceId>
            <SCPDURL>/dri/AVTransport.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/AVTransport</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/AVTransport</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-microsoft-com:service:WMDRM:1</serviceType>
            <serviceId>urn:microsoft-com:serviceId:urn:schemas-microsoft-com:service:WMDRM</serviceId>
            <SCPDURL>/dri/WMDRM.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/WMDRM</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/WMDRM</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Security:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Security</serviceId>
            <SCPDURL>/dri/Security.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/Security</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/Security</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:CAS:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:CAS</serviceId>
            <SCPDURL>/dri/CAS.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/CAS</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/CAS</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Encoder:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Encoder</serviceId>
            <SCPDURL>/dri/Encoder.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/Encoder</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/Encoder</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Tuner:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Tuner</serviceId>
            <SCPDURL>/dri/Tuner.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/Tuner</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/Tuner</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:FDC:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:FDC</serviceId>
            <SCPDURL>/dri/FDC.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/FDC</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/FDC</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Mux:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Mux</serviceId>
            <SCPDURL>/dri/Mux.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/Mux</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/Mux</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:Diag:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:Diag</serviceId>
            <SCPDURL>/dri/Diag.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/Diag</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/Diag</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-opencable-com:service:UserActivity:1</serviceType>
            <serviceId>urn:opencable-com:serviceId:urn:schemas-opencable-com:service:UserActivity</serviceId>
            <SCPDURL>/dri/UserActivity.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/UserActivity</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/UserActivity</eventSubURL>
          </service>
          <service>
            <serviceType>urn:schemas-silicondust-com:service:EAS:1</serviceType>
            <serviceId>urn:silicondust-com:serviceId:urn:schemas-silicondust-com:service:EAS</serviceId>
            <SCPDURL>/dri/EAS.xml</SCPDURL>
            <controlURL>http://192.168.1.43:80/dri/tuner2/EAS</controlURL>
            <eventSubURL>http://192.168.1.43:80/dri/tuner2/EAS</eventSubURL>
          </service>
        </serviceList>
      </device>
    </deviceList>
  </device>
</root>
}

function TProxyWebModule.CreateDeviceXML: String;
const
  cDeviceXML =
    '<?xml version="1.0" encoding="utf-8"?>'#13#10+
    '<root xmlns="urn:schemas-upnp-org:device-1-0" xmlns:pnpx="http://schemas.microsoft.com/windows/pnpx/2005/11" xmlns:df="http://schemas.microsoft.com/windows/2008/09/devicefoundation">'#13#10+
    '  <specVersion>'#13#10+
    '    <major>1</major>'#13#10+
    '    <minor>0</minor>'#13#10+
    '  </specVersion>'#13#10+
    '  <device>'#13#10+
    '    <pnpx:X_hardwareId>VEN_0115&amp;DEV_1310&amp;SUBSYS_0001&amp;REV_0003 VEN_0115&amp;DEV_1310&amp;SUBSYS_0001 VEN_0115&amp;DEV_1310</pnpx:X_hardwareId>'#13#10+
    '    <pnpx:X_deviceCategory>MediaDevices</pnpx:X_deviceCategory>'#13#10+
    '    <df:X_deviceCategory>Multimedia</df:X_deviceCategory>'#13#10+
    '    <deviceType>urn:schemas-dkeystone-com:device:SecureContainer:1</deviceType>'#13#10+
    '    <friendlyName>HDHomeRun DRI Tuner %0:s</friendlyName>'#13#10+
    '    <presentationURL>/</presentationURL>'#13#10+
    '    <manufacturer>Silicondust</manufacturer>'#13#10+
    '    <manufacturerURL>http://www.silicondust.com/</manufacturerURL>'#13#10+
    '    <modelDescription>HDHR3-CC HDHomeRun PRIME</modelDescription>'#13#10+
    '    <modelName>HDHomeRun PRIME</modelName>'#13#10+
    '    <modelNumber>HDHR3-CC</modelNumber>'#13#10+
    '    <modelURL>http://www.silicondust.com/</modelURL>'#13#10+
    '    <serialNumber>%0:s</serialNumber>'#13#10+
    '    <UDN>uuid:%1:s</UDN>'#13#10+
    '  </device>'#13#10+
    '</root>';

var
  lConfig: TServiceConfig;
  lDeviceID, lDeviceUUID: String;
begin
  ConfigManager.LockConfig(lConfig);
  try
    lDeviceID := IntToHex(lConfig.DeviceID);
    lDeviceUUID := lConfig.DeviceUUID;
  finally
    ConfigManager.UnlockConfig(lConfig);
  end;

  Result := Format(cDeviceXML, [lDeviceID, lDeviceUUID]);
end;

type
  TRandomStream = class(TStream)
  private
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

function TRandomStream.Read(var Buffer; Count: Longint): Longint;
begin
  FillChar(Buffer, Count, $FF);
  Result := Count;
end;

function TRandomStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := -1;
end;

procedure TProxyWebModule.ProxyWebModuleSpeedTestActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lStream: TRandomStream;
  lStopWatch: TStopWatch;
  lMeter: TDataMeter;
begin
  Handled := True;
  try
    TLogger.LogFmt(cLogDefault, 'Received speedtest request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    try
      Response.ContentType := 'video/mpeg';
      TIdHTTPAppChunkedResponse(Response).SendChunkedHeader;

      lStopWatch := TStopWatch.StartNew;
      lMeter := Default(TDataMeter);

      lStream := TRandomStream.Create;
      try
        TIdHTTPAppChunkedResponse(Response).SendChunkedStream(lStream,
          procedure(const aPacketSize: Integer; var aContinue: Boolean)
          begin
            lMeter.Add(aPacketSize);

            if (lStopWatch.ElapsedMilliseconds >= 1000) then
            begin
              TLogger.LogFmt(cLogDefault, 'Send rate: %0.2fmbps', [lMeter.GetBytesPerSecond(False)*8/1000000]);
              lStopWatch.Reset;
              lStopWatch.Start;
            end;
          end);
      finally
        lStream.Free;
      end;
    finally
      TLogger.LogFmt(cLogDefault, 'Finished speedtest request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    end;
  except
    HandleException;
  end;
end;

function TProxyWebModule.GetAddress(const aRequest: TWebRequest): String;
var
  lConfig: TServiceConfig;
begin
  ConfigManager.LockConfig(lConfig);
  try
    Result := lConfig.ExternalAddress;
    if Result <> '' then
      Exit;

    Result := lConfig.ListenIP;
    if Result <> '' then
      Exit;
  finally
    ConfigManager.UnlockConfig(lConfig);
  end;

  Result := aRequest.Host;
end;

procedure TProxyWebModule.ProxyWebModuleVideoTestActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  lParts: TArray<String>;
  lChannel, lDuration, lTunerIndex: Integer;
  lRemux: Boolean;
begin
  Handled := True;
  try
    TLogger.LogFmt(cLogDefault, 'Received test video request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    try
      lParts := Request.PathInfo.Split(['/'], TStringSplitOptions.ExcludeEmpty);
      if (Length(lParts) >= 2) and (lParts[1].StartsWith('v',True)) then
      begin
        lChannel := StrToIntDef(lParts[1].Substring(1),0);
        if lChannel > 0 then
        begin
          lTunerIndex := StrToIntDef(Request.QueryFields.Values['tuner'], -1);
          lDuration := StrToIntDef(Request.QueryFields.Values['duration'], 45);
          lRemux := Boolean(StrToIntDef(Request.QueryFields.Values['remux'], 0));

          SendTuneResponse(lTunerIndex, lChannel, True, True, lDuration, lRemux, Response);
        end;
      end;
    finally
      TLogger.LogFmt(cLogDefault, 'Finished test video request from %s: %s', [Request.RemoteAddr, Request.PathInfo]);
    end;
  except
    HandleException;
  end;
end;

initialization
finalization
  Web.WebReq.FreeWebModules;

end.

