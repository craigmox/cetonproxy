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
  System.Generics.Collections,

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
  FMX.ComboEdit,
  FMX.NumberBox,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Utils,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.ListView,
  FMX.SpinBox, 
  FMX.Menus,
  FMX.Platform,
  REST.json,
  REST.Client,

  IdGlobal,

  HDHR,
  Ceton,
  SocketUtils,

  ProxyServiceModuleUnit,
  ProxyServerModuleUnit, FMX.Objects;

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
    StyleBook1: TStyleBook;
    VertScrollBox1: TVertScrollBox;
    Label1: TLabel;
    SaveTimer: TTimer;
    Label2: TLabel;
    btnRefreshChannels: TButton;
    lbStats: TListView;
    Label3: TLabel;
    eHDHRListenHTTPPort: TEdit;
    Label4: TLabel;
    lblSelectedChannels: TLabel;
    ceCetonListenIP: TComboEdit;
    ceHDHRListenIP: TComboEdit;
    btnShowConfigFolder: TButton;
    pnlSettings: TExpander;
    pnlAdvancedSettings: TExpander;
    Label6: TLabel;
    eHDHRExternalAddress: TEdit;
    eHDHRExternalHTTPPort: TEdit;
    Label7: TLabel;
    pnlChannels: TExpander;
    pnlStatistics: TExpander;
    Layout1: TLayout;
    Layout2: TLayout;
    PanelContainer: TLayout;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    OuterPanelContainer: TLayout;
    HelpCallout: TCalloutRectangle;
    lblHelp: TLabel;
    eCetonTunerAddress: TComboEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbChannelsChangeCheck(Sender: TObject);
    procedure btnEditChannelsClick(Sender: TObject);
    procedure SaveTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ceCetonListenIPChangeTracking(Sender: TObject);
    procedure eCetonTunerAddressChangeTracking(Sender: TObject);
    procedure btnRefreshChannelsClick(Sender: TObject);
    procedure eHDHRListenHTTPPortChangeTracking(Sender: TObject);
    procedure ceHDHRListenIPChangeTracking(Sender: TObject);
    procedure btnShowConfigFolderClick(Sender: TObject);
    procedure PanelResizing(Sender: TObject);
    procedure eHDHRExternalAddressChangeTracking(Sender: TObject);
    procedure eHDHRExternalHTTPPortChangeTracking(Sender: TObject);
    procedure EditMouseEnter(Sender: TObject);
    procedure EditMouseLeave(Sender: TObject);
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
  private
    { Private declarations }
    fConfigManager: IServiceConfigManager;

    fEditingChannels: Boolean;
    fInterfaceUpdateCount: Integer;
    fSave: Boolean;
    fSaveSections: TServiceConfigSections;

    function GetClient: TCetonClient;

    function GetLocalIPs: TArray<String>;
    procedure UpdateLocalIPs(const aComboEdit: TComboEdit);
    procedure UpdateDiscoveredCetonDevices(const aComboEdit: TComboEdit);

    function ExtractIP(const aComboEdit: TComboEdit): String;

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
    procedure Log(const aLogName: String; const aMessage: String);
    procedure DiscoveredCetonDevicesChanged;
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

  UpdateLocalIPs(ceCetonListenIP);
  UpdateLocalIPs(ceHDHRListenIP);
  UpdateDiscoveredCetonDevices(eCetonTunerAddress);

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
      ceCetonListenIP.Text := lConfig.Ceton.ListenIP;
      ceHDHRListenIP.Text := lConfig.ListenIP;
      eHDHRListenHTTPPort.Text := IntToStr(lConfig.HTTPPort);
      eHDHRExternalAddress.Text := lConfig.ExternalAddress;
      eHDHRExternalHTTPPort.Text := IntToStr(lConfig.ExternalHTTPPort);
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

procedure TMainForm.ceCetonListenIPChangeTracking(Sender: TObject);
var
  lConfig: TServiceConfig;
begin
  if not InterfaceUpdating then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lConfig.Ceton.ListenIP := ExtractIP(ceCetonListenIP);
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
      lConfig.Ceton.TunerAddress := ExtractIP(eCetonTunerAddress);
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
  i, i2: Integer;
  lActiveStr: String;
  lText: String;
  lCount: Integer;
begin
  lStatsArray := Client.GetTunerStats;
  while lbStats.Items.Count < Length(lStatsArray) do
    lbStats.Items.Add;
  while lbStats.Items.Count > Length(lStatsArray) do
    lbStats.Items.Delete(lbStats.Items.Count-1);

  for i := 0 to High(lStatsArray) do
  begin
    if lStatsArray[i].Active then
      lActiveStr := 'Active'
    else
      lActiveStr := 'Not active';

    lText := Format('%d. Channel: %d, %s  (%0.2fMbps, Buffer free: %0.0f%%)', [i+1, lStatsArray[i].Channel, lActiveStr, lStatsArray[i].InMeter.GetBytesPerSecond(True)*8/1000000, lStatsArray[i].BufferFree*100]);
    lCount := 1;

    for i2 := 0 to lStatsArray[i].ClientCount-1 do
    begin
      if lStatsArray[i].Clients[i2].Active then
      begin
        lText := lText + #13#10 + Format('    To client: %0.2fMbps, Lost packets: %d', [lStatsArray[i].Clients[i2].OutMeter.GetBytesPerSecond(True)*8/1000000, lStatsArray[i].Clients[i2].Lost]);
        Inc(lCount);
      end;
    end;

    lbStats.Items[i].Text := lText;
    lbStats.Items[i].Height := 22*lCount;
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

procedure TMainForm.ceHDHRListenIPChangeTracking(Sender: TObject);
var
  lConfig: TServiceConfig;
begin
  if not InterfaceUpdating then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lConfig.ListenIP := ExtractIP(ceHDHRListenIP);
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    Save([TServiceConfigSection.Other]);
  end;
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

function TMainForm.GetLocalIPs: TArray<String>;
var
  lInfoArray: TLocalIPInfoArray;
  i: Integer;
  lCount: Integer;
begin
  lInfoArray := TSocketUtils.GetLocalIPs;
  SetLength(Result, Length(lInfoArray));
  lCount := 0;
  for i := 0 to High(lInfoArray) do
  begin
    if lInfoArray[i].IPVersion = 4 then
    begin
      Result[lCount] := Format('%s (%s)', [lInfoArray[i].IP, lInfoArray[i].FriendlyName]);
      Inc(lCount);
    end;
  end;
  SetLength(Result, lCount);
end;

procedure TMainForm.UpdateLocalIPs(const aComboEdit: TComboEdit);
begin
  aComboEdit.BeginUpdate;
  try
    aComboEdit.Items.Clear;
    aComboEdit.Items.AddStrings(GetLocalIPs);
    if aComboEdit.Count = 0 then
      aComboEdit.Items.Add('');
  finally
    aComboEdit.EndUpdate;
  end;
end;

function TMainForm.ExtractIP(const aComboEdit: TComboEdit): String;
begin
  Result := aComboEdit.Text;
  if Result.Contains('(') then
    Result := Result.Substring(0, Result.IndexOf('(')-1);
  if aComboEdit.Text <> Result then
  begin
    BeginInterfaceUpdate;
    try
      aComboEdit.Text := Result;
    finally
      EndInterfaceUpdate;
    end;
  end;
end;

procedure TMainForm.Log(const aLogName: String; const aMessage: String);
begin
  // TODO
end;

procedure TMainForm.btnShowConfigFolderClick(Sender: TObject);
begin
  ShellExecute(0, 'explore', PWideChar(WideString(ProxyServiceModule.GetConfigPath)), nil, nil, SW_SHOWDEFAULT);
end;

procedure TMainForm.PanelResizing(Sender: TObject);
begin
  // Always keep the outer panel container at content size so the scrolling matches the content
  OuterPanelContainer.Height := Splitter2.Position.Y + Splitter2.Height;
  // But make the panel container much larger so that splitters have lots of room to work with
  PanelContainer.Height := OuterPanelContainer.Height + 1000;
end;

procedure TMainForm.eHDHRExternalAddressChangeTracking(Sender: TObject);
var
  lConfig: TServiceConfig;
begin
  if not InterfaceUpdating then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lConfig.ExternalAddress := eHDHRExternalAddress.Text;
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    Save([TServiceConfigSection.Other]);
  end;
end;

procedure TMainForm.eHDHRExternalHTTPPortChangeTracking(Sender: TObject);
var
  lConfig: TServiceConfig;
begin
  if not InterfaceUpdating then
  begin
    ConfigManager.LockConfig(lConfig);
    try
      lConfig.ExternalHTTPPort := StrToIntDef(eHDHRExternalHTTPPort.Text, HDHR_HTTP_PORT);
    finally
      ConfigManager.UnlockConfig(lConfig);
    end;

    Save([TServiceConfigSection.Other]);
  end;
end;

procedure TMainForm.EditMouseEnter(Sender: TObject);
begin
  HelpCallout.Visible := True;
  HelpCallout.Position.Y := TControl(Sender).LocalToAbsolute(TPointF.Zero).Y + TControl(Sender).Height;
  lblHelp.Text := TControl(Sender).Hint;
end;

procedure TMainForm.EditMouseLeave(Sender: TObject);
begin
  HelpCallout.Visible := False;
end;

type
  TExpander_Access = class(TExpander);

procedure TMainForm.PanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if Y <= TExpander_Access(Sender).EffectiveHeaderHeight then
    TExpander(Sender).IsExpanded := not TExpander(Sender).IsExpanded;
end;

procedure TMainForm.UpdateDiscoveredCetonDevices(const aComboEdit: TComboEdit);
var
  lDevices: TArray<TDiscoveredCetonDevice>;
  i: Integer;
begin
  aComboEdit.BeginUpdate;
  try
    aComboEdit.Items.Clear;
    lDevices := ProxyServiceModule.DiscoveredCetonDeviceList.ToArray;
    for i := 0 to High(lDevices) do
    begin
      if lDevices[i].FriendlyName <> '' then
        aComboEdit.Items.Add(Format('%s (%s)', [lDevices[i].IP, lDevices[i].FriendlyName]))
      else
        aComboEdit.Items.Add(Format('%s', [lDevices[i].IP]));
    end;
    if aComboEdit.Count = 0 then
      aComboEdit.Items.Add('');
  finally
    aComboEdit.EndUpdate;
  end;
end;

procedure TMainForm.DiscoveredCetonDevicesChanged;
begin
  TThread.ForceQueue(nil,
    procedure()
    begin
      UpdateDiscoveredCetonDevices(eCetonTunerAddress);
    end);
end;

type
  TCustomScrollBox_Access = class(TCustomScrollBox);

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  lControl: IControl;
begin
  lControl := ObjectAtPoint(Screen.MousePos);
  if Assigned(lControl) and (not (lControl.GetObject is TListBox)) then
  begin
    // Force all mouse wheel handling to go to scrollbox.  There's probably a better way to do this.
    TCustomScrollBox_Access(VertScrollBox1).MouseWheel(Shift, WheelDelta, Handled);
    Handled := True;
  end;
end;

end.
