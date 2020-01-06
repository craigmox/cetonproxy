program cetonproxy;
{$APPTYPE GUI}

{$R *.dres}

uses
  FMX.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  FormUnit1 in 'FormUnit1.pas' {MainForm},
  ServerMethodsUnit1 in 'ServerMethodsUnit1.pas',
  ServerContainerUnit1 in 'ServerContainerUnit1.pas' {ServerContainer1: TDataModule},
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  HDHR in 'hdhr\HDHR.pas',
  Ceton in 'ceton\Ceton.pas',
  UIUtils in 'UIUtils.pas',
  ProxyService in 'ProxyService.pas' {ProxyServiceModule: TDataModule};

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TProxyServiceModule, ProxyServiceModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
