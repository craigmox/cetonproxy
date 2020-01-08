unit FormUnit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, IdHTTPWebBrokerBridge, Web.HTTPApp, FMX.Controls.Presentation,
  IdSchedulerOfThread, IdSchedulerOfThreadPool,
  hdhr, REST.json, REST.Client, ceton, IdBaseComponent, IdScheduler,
  IdSchedulerOfThreadDefault, IdThread, ActiveX, FMX.EditBox, FMX.NumberBox,
  System.Diagnostics, System.IOUtils, FMX.Layouts, FMX.ListBox, FMX.Utils,
  System.Math, ProxyService, IdContext, ProxyServer,
  WinApi.Windows, Winapi.ShellApi;

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
    ButtonStart: TButton;
    ButtonStop: TButton;
    ButtonOpenBrowser: TButton;
    Button2: TButton;
    NumberBox1: TNumberBox;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Button5: TButton;
    lbChannels: TListBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbChannelsChangeCheck(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fViewer: TCetonViewer;

    fChannelMap: TChannelMap;

    function Client: TCetonClient;

    procedure FillChannels;

    procedure StartServer;

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

procedure TMainForm.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not ProxyServerModule.Active;
  ButtonStop.Enabled := ProxyServerModule.Active;
end;

procedure TMainForm.ButtonOpenBrowserClick(Sender: TObject);
var
  LURL: string;
begin
  StartServer;
  LURL := Format('http://localhost:%d', [HDHR_HTTP_PORT]);
  ShellExecute(0,
        nil,
        PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  ProxyServerModule.StopServer;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnIdle := ApplicationIdle;

  fViewer := TCetonViewer.Invalid;

  fChannelMap := TChannelMap.Create;

  StartServer;
end;

procedure TMainForm.StartServer;
begin
  ProxyServerModule.StartServer;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  if fViewer.IsValid then
  begin
    Client.StopStream(fViewer);
  end;

  Client.StartStream(Round(NumberBox1.Value), fViewer);
end;

function TMainForm.Client: TCetonClient;
begin
  Result := ProxyServiceModule.Client;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  if fViewer.IsValid then
  begin
    Client.StopStream(fViewer);
  end;
end;

procedure TMainForm.Button4Click(Sender: TObject);
var
  lStopWatch: TStopWatch;
  lFS: TFileStream;
  lBuffer: TRingBuffer;
begin
  lBuffer := TRingBuffer.Create;
  try
    lStopWatch := TStopwatch.StartNew;
    while lStopWatch.ElapsedMilliseconds <= 10 * 1000 do
    begin
      Client.ReadStream(fViewer, lBuffer, 32000, 5000);

      Log.D('Buffer size %d', [lBuffer.Size]);
      lBuffer.Seek(32000, soCurrent);

 {
      lFS := TFile.Open(edit1.Text, TFileMode.fmAppend);
      try
        lFS.WriteData(FPacket.Data, FPacket.Size);
      finally
        lFS.Free;
      end;}
    end;
  finally
    lBuffer.Free;
  end;
end;

procedure TMainForm.Button5Click(Sender: TObject);
var
  lBuffer: TRingBuffer;
  lTestData, lReadData: TBytes;
  i: Integer;
begin
  lBuffer := TRingBuffer.Create;
  try
{    SetLength(lTestData,10);
    for i := 0 to 9 do
      lTestData[i] := i;

    lBuffer.Write(lTestData[0], Length(lTestData));
    lBuffer.Write(lTestData[0], Length(lTestData));

    SetLength(lReadData, 5);
    lBuffer.Read(lReadData[0], Length(lReadData));

    lBuffer.Write(lTestData[2], 3);

    lBuffer.Write(lTestData[2], 6);}



  finally
    lBuffer.Free;
  end;

end;

procedure TMainForm.FillChannels;
var
  lListItem: TListBoxItem;
  i: Integer;
begin
  if Client.ChannelMapExpired(3) then
    Client.RequestChannelMap;

  lbChannels.BeginUpdate;
  try
    lbChannels.Clear;
    Client.GetChannelMap(fChannelMap);

    for i := 0 to fChannelMap.Count-1 do
    begin
      lListItem := TChannelListBoxItem.Create(lbChannels);
      lListItem.Parent := lbChannels;
      lListItem.Stored := False;
      lListItem.Text := Format('%d. %s', [fChannelMap[i].Channel, fChannelMap[i].Name]);
      lListItem.Data := fChannelMap[i];
      lListItem.IsChecked := fChannelMap[i].Visible;
    end;
  finally
    lbChannels.EndUpdate;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fChannelMap.Free;
end;

procedure TMainForm.lbChannelsChangeCheck(Sender: TObject);
begin
  TChannelMapItem(TListBoxItem(Sender).Data).Visible := TListBoxItem(Sender).IsChecked;
  Client.ApplyChannelConfig(fChannelMap);
end;

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

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FillChannels;
end;

end.
