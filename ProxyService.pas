unit ProxyService;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,

  Ceton;

type
  TProxyServiceModule = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fClient: TCetonClient;
    fConfig: TCetonConfig;
  public
    { Public declarations }
    property Client: TCetonClient read fClient;
  end;

var
  ProxyServiceModule: TProxyServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TProxyServiceModule.DataModuleCreate(Sender: TObject);
var
  lJSON: String;
begin
  fClient := TCetonClient.Create;

  try
    lJSON := TFile.ReadAllText(TPath.GetHomePath + TPath.DirectorySeparatorChar + 'cetonproxy' + TPath.DirectorySeparatorChar + 'config.js');
  except
    // Ignore
  end;
  fConfig := TCetonConfig.FromJSON(lJSON);
  fConfig.TunerAddress := '192.168.1.132';

  fClient.SetConfig(fConfig);
end;

procedure TProxyServiceModule.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(fClient);

  ForceDirectories(TPath.GetHomePath + TPath.DirectorySeparatorChar + 'cetonproxy');
  TFile.WriteAllText(TPath.GetHomePath + TPath.DirectorySeparatorChar + 'cetonproxy' + TPath.DirectorySeparatorChar + 'config.js', fConfig.ToJSON);

  FreeAndNil(fConfig);
end;

end.
