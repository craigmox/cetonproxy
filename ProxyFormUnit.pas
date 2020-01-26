unit ProxyFormUnit;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Diagnostics,
  System.IOUtils,
  System.Math,
  WinApi.Windows,
  Winapi.ShellApi,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.EditBox,
  FMX.NumberBox,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Utils,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.ListView,
  REST.json,
  REST.Client,

  IdGlobal,

  HDHR,
  Ceton,
  SocketUtils,

  ProxyServiceModuleUnit,
  ProxyServerModuleUnit;

type
  TChannelListBoxItem = class(TListBoxItem)
  private
    fCheck: TCheckBox;
    fChannel: Integer;
    procedure DoCheckMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
  public
    property Channel: Integer read fChannel write fChannel;
  end;

  TMainForm = class(TForm, IServiceConfigEvents)
    lbChannels: TListBox;
    btnEditChannels: TButton;
    Splitter1: TSplitter;
    gbGroup: TGroupBox;
    StyleBook1: TStyleBook;
    gbSettings: TGroupBox;
    gbStats: TGroupBox;
    VertScrollBox1: TVertScrollBox;
    eCetonTunerAddress: TEdit;
    Label1: TLabel;
    SaveTimer: TTimer;
    Label2: TLabel;
    eCetonListenIP: TEdit;
    btnRefreshChannels: TButton;
    lbStats: TListView;
    Splitter2: TSplitter;
    Label3: TLabel;
    eHDHRListenHTTPPort: TEdit;
    eHDHRListenIP: TEdit;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    Splitter3: TSplitter;
    eDebugIP: TEdit;
    Label5: TLabel;
    btnDebugDiscoveryRequest: TButton;
    lblSelectedChannels: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbChannelsChangeCheck(Sender: TObject);
    procedure btnEditChannelsClick(Sender: TObject);
    procedure SaveTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure eCetonListenIPChangeTracking(Sender: TObject);
    procedure eCetonTunerAddressChangeTracking(Sender: TObject);
    procedure btnRefreshChannelsClick(Sender: TObject);
    procedure eHDHRListenHTTPPortChangeTracking(Sender: TObject);
    procedure eHDHRListenIPChangeTracking(Sender: TObject);
    procedure btnDebugDiscoveryRequestClick(Sender: TObject);
  private
    { Private declarations }
    fConfigManager: IServiceConfigManager;

    fEditingChannels: Boolean;
    fInterfaceUpdateCount: Integer;
    fSave: Boolean;
    fSaveSections: TServiceConfigSections;

    function GetClient: TCetonClient;

    procedure BeginInterfaceUpdate;
    procedure EndInterfaceUpdate;
    function InterfaceUpdating: Boolean;
    procedure Save(const aSections: TServiceConfigSections);

    procedure UpdateInterface;
    procedure UpdateChannelCount;
    procedure FillChannels;
    procedure FillTunerStatistics;

    property ConfigManager: IServiceConfigManager read fConfigManager;
    property Client: TCetonClient read GetClient;
  protected
    // IServiceConfigEvents
    procedure Changed(const aSender: TObject; const aSections: TServiceConfigSections);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TChannelListBoxItem }

procedure TChannelListBoxItem.ApplyStyle;
begin
  inherited;

  if FindStyleResource<TCheckBox>('check', fCheck) then
  begin
    fCheck.OnMouseUp := DoCheckMouseUp;
  end;
end;

type
  TCustomListBox = class(FMX.ListBox.TCustomListBox);

procedure TChannelListBoxItem.DoCheckMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  i: Integer;
  lListBox: TCustomListBox;
  lItem: TChannelListBoxItem;
  lStartIndex, lEndIndex: Integer;
  lState: Boolean;
begin
  if TFmxObjectHelper.FindNearestParentOfClass<TChannelListBoxItem>(TFmxObject(Sender), lItem) then
  begin
    lListBox := TCustomListBox(lItem.ListBox);
    lStartIndex := lListBox.ItemIndex;
    lEndIndex := lItem.Index;

    if (Button = TMouseButton.mbLeft) and (ssShift in Shift) and (lListBox.ItemIndex > -1) and (lEndIndex <> lStartIndex) then
    begin
      lState := not lItem.IsChecked;

      lListBox.BeginUpdate;
      try
        for i := Min(lStartIndex,lEndIndex+1) to Max(lStartIndex,lEndIndex-1) do
        begin
          if lListBox.ListItems[i] is TChannelListBoxItem then
          begin
            lItem := TChannelListBoxItem(lListBox.ListItems[i]);
            lItem.IsChecked := lState;
            if not Assigned(lItem.fCheck) then
              // Event does not get fired if checkbox has not yet been drawn to the screen
              lListBox.DoChangeCheck(lItem);
          end;
        end;
      finally
        lListBox.EndUpdate;
      end;
    end;
  end;
end;

procedure TChannelListBoxItem.FreeStyle;
begin
  fCheck := nil;
  inherited;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fConfigManager := ProxyServiceModule.ConfigManager;
  fConfigManager.AddListener(Self);

  ProxyServerModule.StartServer;
end;

function TMainForm.GetClient: TCetonClient;
begin
  Result := ProxyServiceModule.Client;
end;

procedure TMainForm.FillChannels;
var
  lListItem: TChannelListBoxItem;
  i: Integer;
  lConfig: TServiceConfig;
  lChannelMap: TChannelMap;
begin
  if fEditingChannels then
  begin
    lChannelMap := TChannelMap.Create;
    try
      ConfigManager.LockConfig(lConfig);
      try
        lChannelMap.Assign(lConfig.Ceton.ChannelMap, False);
      finally
        ConfigManager.UnlockConfig(lConfig);
      end;

      lbChannels.BeginUpdate;
      try
        lbChannels.Clear;
        for i := 0 to lChannelMap.Count-1 do
        begin
          lListItem := TChannelListBoxItem.Create(lbChannels);
          lListItem.Parent := lbChannels;
          lListItem.Stored := False;
          lListItem.Text := Format('%d. %s', [lChannelMap[i].Channel, lChannelMap[i].Name]);
          lListItem.Channel := lChannelMap[i].Channel;
          lListItem.IsChecked := lChannelMap[i].Visible;
        end;
      finally
        lbChannels.EndUpdate;
      end;
    finally
      lChannelMap.Free;
    end;

    lbChannels.Enabled := True;
  end
  else
    lbChannels.Enabled := False;

  UpdateChannelCount;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fConfigManager.RemoveListener(Self);

  ProxyServerModule.StopServer;
end;

procedure TMainForm.lbChannelsChangeCheck(Sender: TObject);
var
  lConfig: TServiceConfig;
  i: Integer;
begin
  if not InterfaceUpdating then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      i := lConfig.Ceton.ChannelMap.IndexOf(TChannelListBoxItem(Sender).Channel);
      if i > -1 then
      begin
        lConfig.Ceton.ChannelMap[i].Visible := TListBoxItem(Sender).IsChecked;
      end;
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    updateChannelCount;

    Save([TServiceConfigSection.Channels]);
  end;
end;

procedure TMainForm.BeginInterfaceUpdate;
begin
  Inc(fInterfaceUpdateCount);
end;

procedure TMainForm.EndInterfaceUpdate;
begin
  Dec(fInterfaceUpdateCount);
end;

procedure TMainForm.Save(const aSections: TServiceConfigSections);
begin
  fSave := True;
  fSaveSections := fSaveSections + aSections;
end;

function TMainForm.InterfaceUpdating: Boolean;
begin
  Result := fInterfaceUpdateCount > 0;
end;

procedure TMainForm.UpdateInterface;
var
  lConfig: TServiceConfig;
begin
  BeginInterfaceUpdate;
  try
    ConfigManager.LockConfig(lConfig);
    try
      eCetonTunerAddress.Text := lConfig.Ceton.TunerAddress;
      eCetonListenIP.Text := lConfig.Ceton.ListenIP;
      eHDHRListenIP.Text := lConfig.ListenIP;
      eHDHRListenHTTPPort.Text := IntToStr(lConfig.HTTPPort);
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    FillChannels;
  finally
    EndInterfaceUpdate;
  end;
end;

procedure TMainForm.btnEditChannelsClick(Sender: TObject);
var
  lConfig: TServiceConfig;
  lRequestChannels: Boolean;
  lCetonConfig: TCetonConfig;
begin
  fEditingChannels := True;

  ConfigManager.LockConfig(lConfig);
  try
    lRequestChannels := lConfig.Ceton.ChannelMap.Count = 0;
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

    ConfigManager.Changed(Self, [TServiceConfigSection.Channels]);
  end;

  FillChannels;
end;

procedure TMainForm.SaveTimerTimer(Sender: TObject);
var
  lSections: TServiceConfigSections;
begin
  if fSave then
  begin
    lSections := fSaveSections;

    fSave := False;
    fSaveSections := [];

    ConfigManager.Changed(Self, lSections);
  end;

  FillTunerStatistics;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateInterface;
end;

procedure TMainForm.eCetonListenIPChangeTracking(Sender: TObject);
var
  lConfig: TServiceConfig;
begin
  if not InterfaceUpdating then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lConfig.Ceton.ListenIP := eCetonListenIP.Text;
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    Save([TServiceConfigSection.Other]);
  end;
end;

procedure TMainForm.eCetonTunerAddressChangeTracking(Sender: TObject);
var
  lConfig: TServiceConfig;
begin
  if not InterfaceUpdating then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lConfig.Ceton.TunerAddress := eCetonTunerAddress.Text;
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    Save([TServiceConfigSection.Other]);
  end;
end;

procedure TMainForm.btnRefreshChannelsClick(Sender: TObject);
var
  lConfig: TServiceConfig;
  lCetonConfig: TCetonConfig;
begin
  fEditingChannels := True;

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

  ConfigManager.Changed(Self, [TServiceConfigSection.Channels]);

  FillChannels;
end;

procedure TMainForm.FillTunerStatistics;
var
  lStatsArray: TTunerStatsArray;
  i: Integer;
begin
  lStatsArray := Client.GetTunerStats;
  while lbStats.Items.Count < Length(lStatsArray) do
    lbStats.Items.Add;
  while lbStats.Items.Count > Length(lStatsArray) do
    lbStats.Items.Delete(lbStats.Items.Count-1);

  for i := 0 to High(lStatsArray) do
  begin
    lbStats.Items[i].Text := Format('%d. Channel: %d, From tuner: %d, To client: %d', [i+1, lStatsArray[i].Channel, lStatsArray[i].PacketsReceived, lStatsArray[i].PacketsRead[0]]);
  end;
end;

procedure TMainForm.eHDHRListenHTTPPortChangeTracking(Sender: TObject);
var
  lConfig: TServiceConfig;
begin
  if not InterfaceUpdating then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lConfig.HTTPPort := StrToIntDef(eHDHRListenHTTPPort.Text, HDHR_HTTP_PORT);
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    Save([TServiceConfigSection.Other]);
  end;
end;

procedure TMainForm.Changed(const aSender: TObject;
  const aSections: TServiceConfigSections);
begin
  if aSender <> Self then
  begin
    TThread.ForceQueue(nil,
      procedure()
      begin
        if TServiceConfigSection.Channels in aSections then
        begin
          fEditingChannels := False;
        end;

        UpdateInterface;
      end);
  end;
end;

procedure TMainForm.eHDHRListenIPChangeTracking(Sender: TObject);
var
  lConfig: TServiceConfig;
begin
  if not InterfaceUpdating then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lConfig.ListenIP := eHDHRListenIP.Text;
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    Save([TServiceConfigSection.Other]);
  end;
end;

procedure TMainForm.btnDebugDiscoveryRequestClick(Sender: TObject);
var
  lDiscovery: TDiscoveryData;
  lBytes: TBytes;
begin
  lDiscovery.DeviceType := HDHOMERUN_DEVICE_TYPE_TUNER;
  lDiscovery.DeviceID := HDHOMERUN_DEVICE_ID_WILDCARD;

  lBytes := TPacket.FromDiscovery(True, lDiscovery).ToBytes;

  ProxyServerModule.DiscoveryServer.SendBuffer(eDebugIP.Text, HDHR_DISCOVERY_PORT, TIdBytes(lBytes));
//  ProxyServerModule.DiscoveryServer.Broadcast(TIdBytes(lBytes), HDHR_DISCOVERY_PORT);
end;

procedure TMainForm.UpdateChannelCount;
var
  i, lChannelCount: Integer;
begin
  if fEditingChannels then
  begin
    lChannelCount := 0;
    for i := 0 to lbChannels.Items.Count-1 do
    begin
      if (TChannelListBoxItem(lbChannels.ListItems[i]).IsChecked) then
        Inc(lChannelCount);
    end;
    lblSelectedChannels.Text := Format('(%d selected)', [lChannelCount]);
  end
  else
    lblSelectedChannels.Text := '';
end;

end.
