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
  SocketUtils in 'SocketUtils.pas';

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
