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
  Winapi.IpHlpApi,
  Winapi.IpTypes,
  Winapi.IpExport,
  Winapi.WinSock,

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
    btnRefreshChannels: TButton;
    lbStats: TListView;
    Splitter2: TSplitter;
    Label3: TLabel;
    eHDHRListenHTTPPort: TEdit;
    Label4: TLabel;
    gbDebug: TGroupBox;
    DebugSplitter: TSplitter;
    Label5: TLabel;
    btnGetVideoSample: TButton;
    lblSelectedChannels: TLabel;
    ceCetonListenIP: TComboEdit;
    ceHDHRListenIP: TComboEdit;
    seChannel: TSpinBox;
    DebugMenu: TPopupMenu;
    MenuItem1: TMenuItem;
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
    procedure btnGetVideoSampleClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    { Private declarations }
    fConfigManager: IServiceConfigManager;

    fEditingChannels: Boolean;
    fInterfaceUpdateCount: Integer;
    fSave: Boolean;
    fSaveSections: TServiceConfigSections;

    function GetClient: TCetonClient;

    function GetLocalIPs: TArray<String>; overload;
    procedure GetLocalIPs(const aComboEdit: TComboEdit); overload;

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
  gbDebug.Visible := FindCmdLineSwitch('debug');
  DebugSplitter.Visible := gbDebug.Visible;

  fConfigManager := ProxyServiceModule.ConfigManager;
  fConfigManager.AddListener(Self);

  GetLocalIPs(ceCetonListenIP);
  GetLocalIPs(ceHDHRListenIP);

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
  lAdapterAddresses, lCurAdapterAddress: PIP_ADAPTER_ADDRESSES;
  lSize: ULONG;
  lList: TList<String>;
  lIP: String;
  lCurAddress: PIP_ADAPTER_UNICAST_ADDRESS;
begin
  lAdapterAddresses := nil;
  lSize := 0;
  GetAdaptersAddresses(AF_INET, 0, nil, lAdapterAddresses, @lSize);
  if lSize > 0 then
  begin
    GetMem(lAdapterAddresses, lSize);
    try
      if GetAdaptersAddresses(AF_INET, 0, nil, lAdapterAddresses, @lSize) = ERROR_SUCCESS then
      begin
        lList := TList<String>.Create;
        try
          lCurAdapterAddress := lAdapterAddresses;
          while Assigned(lCurAdapterAddress) do
          begin
            if lCurAdapterAddress.IfType = 6 {ETHERNET} then
            begin
              lCurAddress := lCurAdapterAddress.FirstUnicastAddress;
              while Assigned(lCurAddress) do
              begin
                if Assigned(lCurAddress.Address.lpSockaddr) then
                begin
                  lIP := String(inet_ntoa(lCurAddress.Address.lpSockaddr.sin_addr));
                  lList.Add(Format('%s (%s)', [lIP, WideString(lCurAdapterAddress.FriendlyName)]));
                end;

                lCurAddress := lCurAddress.Next;
              end;
            end;
            lCurAdapterAddress := lCurAdapterAddress.Next;
          end;

          Result := lList.ToArray;
        finally
          lList.Free;
        end;
      end;
    finally
      FreeMem(lAdapterAddresses, lSize);
    end;
  end;
end;

procedure TMainForm.GetLocalIPs(const aComboEdit: TComboEdit);
begin
  aComboEdit.Items.Clear;
  aComboEdit.Items.AddStrings(GetLocalIPs);
  aComboEdit.RecalcSize;
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

procedure TMainForm.btnGetVideoSampleClick(Sender: TObject);
var
  lStream: TCetonVideoStream;
  lStopWatch: TStopWatch;
  lBuffer: array[0..8191] of Byte;
  lSize: Integer;
  lFS: TFileStream;
  lChannel: TChannelMapItem;
  lFilename: String;
begin
  lFilename := 'Sample.ts';

  lChannel := TChannelMapItem.Create;
  try
    if Client.TryGetChannel(Round(seChannel.Value), lChannel) then
    begin
      lFilename := Format('SampleCh%dP%d.ts', [lChannel.Channel, lChannel.ItemProgram]);
    end;
  finally
    lChannel.Free;
  end;

  lFS := TFile.Create(ExtractFilePath(ParamStr(0))+lFilename);
  try
    lStream := TCetonVideoStream.Create(Client, -1, Round(seChannel.Value), False);
    try
      lStopWatch := TStopWatch.StartNew;
      while lStopWatch.ElapsedMilliseconds <= 6000 do
      begin
        lSize := lStream.Read(lBuffer, SizeOf(lBuffer));
        if lSize = 0 then
          Break;

        lFS.WriteBuffer(lBuffer, lSize);
      end;
    finally
      lStream.Free;
    end;
  finally
    lFS.Free;
  end;
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  gbDebug.Visible := True;
  DebugSplitter.Visible := True;
end;

end.
