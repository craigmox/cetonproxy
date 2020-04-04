program cetonproxy;
{$APPTYPE GUI}



uses
  FMX.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  ProxyFormUnit in 'ProxyFormUnit.pas' {MainForm},
  ProxyWebModuleUnit in 'ProxyWebModuleUnit.pas' {ProxyWebModule: TWebModule},
  HDHR in 'hdhr\HDHR.pas',
  Ceton in 'ceton\Ceton.pas',
  ProxyServiceModuleUnit in 'ProxyServiceModuleUnit.pas' {ProxyServiceModule: TDataModule},
  ProxyServerModuleUnit in 'ProxyServerModuleUnit.pas' {ProxyServerModule: TDataModule},
  SocketUtils in 'SocketUtils.pas',
  VideoUtils in 'VideoUtils.pas',
  FFUtils in 'ffmpeg\src\examples\FFUtils.pas',
  libavcodec in 'ffmpeg\src\headers\libavcodec.pas',
  libavformat in 'ffmpeg\src\headers\libavformat.pas',
  libavformat_avio in 'ffmpeg\src\headers\libavformat_avio.pas',
  libavutil in 'ffmpeg\src\headers\libavutil.pas',
  libavutil_error in 'ffmpeg\src\headers\libavutil_error.pas',
  libavutil_mathematics in 'ffmpeg\src\headers\libavutil_mathematics.pas',
  libavutil_mem in 'ffmpeg\src\headers\libavutil_mem.pas',
  FFTypes in 'ffmpeg\src\headers\FFTypes.pas',
  libavutil_buffer in 'ffmpeg\src\headers\libavutil_buffer.pas',
  libavutil_dict in 'ffmpeg\src\headers\libavutil_dict.pas',
  libavutil_frame in 'ffmpeg\src\headers\libavutil_frame.pas',
  libavutil_hwcontext in 'ffmpeg\src\headers\libavutil_hwcontext.pas',
  libavutil_pixfmt in 'ffmpeg\src\headers\libavutil_pixfmt.pas',
  libavutil_rational in 'ffmpeg\src\headers\libavutil_rational.pas',
  libavutil_samplefmt in 'ffmpeg\src\headers\libavutil_samplefmt.pas',
  libavutil_opt in 'ffmpeg\src\headers\libavutil_opt.pas',
  libavutil_bprint in 'ffmpeg\src\headers\libavutil_bprint.pas',
  libavutil_log in 'ffmpeg\src\headers\libavutil_log.pas',
  libavutil_avstring in 'ffmpeg\src\headers\libavutil_avstring.pas',
  LogUtils in 'LogUtils.pas',
  FileUtils in 'FileUtils.pas',
  EmailUtils in 'EmailUtils.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TProxyServiceModule, ProxyServiceModule);
  Application.CreateForm(TProxyServerModule, ProxyServerModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
