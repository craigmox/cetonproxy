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
  REST.json,
  REST.Client,

  HDHR,
  Ceton,
  SocketUtils,

  ProxyServiceModuleUnit,
  ProxyServerModuleUnit, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView;

type
  TChannelListBoxItem = class(TListBoxItem)
  private
    fCheck: TCheckBox;
    procedure DoCheckMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
  public
  end;

  TMainForm = class(TForm)
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
    eListenIP: TEdit;
    btnRefreshChannels: TButton;
    lbStats: TListView;
    Splitter2: TSplitter;
    Label3: TLabel;
    eListenHTTPPort: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbChannelsChangeCheck(Sender: TObject);
    procedure btnEditChannelsClick(Sender: TObject);
    procedure SaveTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure eListenIPChangeTracking(Sender: TObject);
    procedure eCetonTunerAddressChangeTracking(Sender: TObject);
    procedure btnRefreshChannelsClick(Sender: TObject);
    procedure eListenHTTPPortChangeTracking(Sender: TObject);
  private
    { Private declarations }
    fConfig: TServiceConfig;

    fInterfaceUpdateCount: Integer;
    fSave: Boolean;

    function GetClient: TCetonClient;
    procedure GetConfig;
    procedure SetConfig;

    procedure BeginInterfaceUpdate;
    procedure EndInterfaceUpdate;
    function InterfaceUpdating: Boolean;
    procedure Save;

    procedure UpdateInterface;
    procedure FillChannels;
    procedure FillTunerStatistics;

    property Client: TCetonClient read GetClient;
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
  fConfig := TServiceConfig.Create;

  ProxyServerModule.StartServer;
end;

function TMainForm.GetClient: TCetonClient;
begin
  Result := ProxyServiceModule.Client;
end;

procedure TMainForm.FillChannels;
var
  lListItem: TListBoxItem;
  i: Integer;
begin
  if not (TChannelMapSection.Items in fConfig.Ceton.ChannelMap.Exclude) then
  begin
    lbChannels.BeginUpdate;
    try
      lbChannels.Clear;
      for i := 0 to fConfig.Ceton.ChannelMap.Count-1 do
      begin
        lListItem := TChannelListBoxItem.Create(lbChannels);
        lListItem.Parent := lbChannels;
        lListItem.Stored := False;
        lListItem.Text := Format('%d. %s', [fConfig.Ceton.ChannelMap[i].Channel, fConfig.Ceton.ChannelMap[i].Name]);
        lListItem.Data := fConfig.Ceton.ChannelMap[i];
        lListItem.IsChecked := fConfig.Ceton.ChannelMap[i].Visible;
      end;
    finally
      lbChannels.EndUpdate;
    end;

    lbChannels.Enabled := True;
  end
  else
    lbChannels.Enabled := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ProxyServerModule.StopServer;

  fConfig.Free;
end;

procedure TMainForm.lbChannelsChangeCheck(Sender: TObject);
begin
  if not InterfaceUpdating then
  begin
    TChannelMapItem(TListBoxItem(Sender).Data).Visible := TListBoxItem(Sender).IsChecked;
    Save;
  end;
end;

procedure TMainForm.GetConfig;
var
  lOldConfig, lNewConfig: TServiceConfig;
begin
  lOldConfig := TServiceConfig.Create;
  lNewConfig := TServiceConfig.Create;
  try
    lOldConfig.Ceton.ChannelMap.Exclude := lOldConfig.Ceton.ChannelMap.Exclude + [TChannelMapSection.Items];
    lOldConfig.Assign(fConfig);

    lNewConfig.Ceton.ChannelMap.Exclude := lNewConfig.Ceton.ChannelMap.Exclude + [TChannelMapSection.Items];
    ProxyServiceModule.GetConfig(lNewConfig);

    if lNewConfig.ToJSON <> lOldConfig.ToJSON then
    begin
      // Do not reload channels on every change
      lbChannels.Clear;

      fConfig.Assign(lNewConfig);

      UpdateInterface;
    end;
  finally
    lNewConfig.Free;
    lOldConfig.Free;
  end;
end;

procedure TMainForm.SetConfig;
begin
  ProxyServiceModule.SetConfig(fConfig);
end;

procedure TMainForm.BeginInterfaceUpdate;
begin
  Inc(fInterfaceUpdateCount);
end;

procedure TMainForm.EndInterfaceUpdate;
begin
  Dec(fInterfaceUpdateCount);
end;

procedure TMainForm.Save;
begin
  fSave := True;
end;

function TMainForm.InterfaceUpdating: Boolean;
begin
  Result := fInterfaceUpdateCount > 0;
end;

procedure TMainForm.UpdateInterface;
begin
  BeginInterfaceUpdate;
  try
    eCetonTunerAddress.Text := fConfig.Ceton.TunerAddress;
    eListenIP.Text := fConfig.Ceton.ListenIP;
    eListenHTTPPort.Text := IntToStr(fConfig.HTTPPort);

    FillChannels;
  finally
    EndInterfaceUpdate;
  end;
end;

procedure TMainForm.btnEditChannelsClick(Sender: TObject);
begin
  fConfig.Ceton.ChannelMap.Exclude := [];
  ProxyServiceModule.GetConfig(fConfig);

  FillChannels;
end;

procedure TMainForm.SaveTimerTimer(Sender: TObject);
begin
  if fSave then
  begin
    fSave := False;
    SetConfig;
  end;

  FillTunerStatistics;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  GetConfig;
  UpdateInterface;
end;

procedure TMainForm.eListenIPChangeTracking(Sender: TObject);
begin
  if not InterfaceUpdating then
  begin
    fConfig.Ceton.ListenIP := eListenIP.Text;
    Save;
  end;
end;

procedure TMainForm.eCetonTunerAddressChangeTracking(Sender: TObject);
begin
  if not InterfaceUpdating then
  begin
    fConfig.Ceton.TunerAddress := eCetonTunerAddress.Text;
    Save;
  end;
end;

procedure TMainForm.btnRefreshChannelsClick(Sender: TObject);
begin
  Client.RequestChannelMap;

  fConfig.Ceton.ChannelMap.Exclude := [];
  ProxyServiceModule.GetConfig(fConfig);

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
    lbStats.Items[i].Text := Format('%d. Channel: %d, Received: %d, Read: %d, Wait: %d', [i+1, lStatsArray[i].Channel, lStatsArray[i].PacketsReceived, lStatsArray[i].PacketsRead[0], lStatsArray[i].ReaderWait[0]]);
  end;
end;

procedure TMainForm.eListenHTTPPortChangeTracking(Sender: TObject);
begin
  if not InterfaceUpdating then
  begin
    fConfig.HTTPPort := StrToIntDef(eListenHTTPPort.Text, HDHR_HTTP_PORT);
    Save;
  end;
end;

end.
