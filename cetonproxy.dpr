program cetonproxy;
{$APPTYPE GUI}



uses
  FMX.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  FormUnit1 in 'FormUnit1.pas' {MainForm},
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  HDHR in 'hdhr\HDHR.pas',
  Ceton in 'ceton\Ceton.pas',
  UIUtils in 'UIUtils.pas',
  ProxyService in 'ProxyService.pas' {ProxyServiceModule: TDataModule},
  ProxyServer in 'ProxyServer.pas' {ProxyServerModule: TDataModule};

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
