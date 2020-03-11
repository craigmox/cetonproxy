unit SocketUtils;

interface

{$SCOPEDENUMS ON}
{$DEFINE LITTLEENDIAN}

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Math,
  System.Diagnostics,
  System.Timespan,
  System.Generics.Collections,
  System.IOUtils,

  IdUDPServer,
  IdGlobal,
  IdSocketHandle,
  IdStackConsts,

  WinApi.Windows,
  Winapi.IpHlpApi,
  Winapi.IpTypes,
  Winapi.IpExport,
  Winapi.WinSock,

  LogUtils;

type
  EVideoError = class(Exception);
  EVideoClosedError = class(Exception);

  TRingBuffer = class
  private
    fData: TBytes;
    fStartIndex, fSize: Integer;
    procedure Resize(const aSize: Integer);
  public
    procedure Write(var Buffer; Count: Longint);
    function Read(var Buffer; Count: Longint): Longint;
    function Seek(const Offset: Integer; Origin: TSeekOrigin): Integer;
    property Size: Integer read fSize;
  end;

  {

    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |V=2|P|X|  CC   |M|     PT      |       sequence number         |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                           timestamp                           |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |           synchronization source (SSRC) identifier            |
   +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+

  }
  PRTPHeader = ^TRTPHeader;
  TRTPHeader = packed record
  private
    // HEADER 12 bytes
    RawVersion  : UInt8;      // $80  : Version = 2
    RawPayload  : UInt8;      //   8  : PCMA
    RawSequence : UInt16;
    RawTimestamp: UInt32;
    RawSSRC     : UInt32;

    function GetPayloadType: UInt8;
    function GetSequenceNumber: UInt16;
    function GetVersion: UInt8;
  public
    property Version: UInt8 read GetVersion;
    property SequenceNumber: UInt16 read GetSequenceNumber;
    property PayloadType: UInt8 read GetPayloadType;
  end;

  PVideoPacket = ^TVideoPacket;
  TVideoPacket = record
    Data: TBytes;
    Size: Integer;
  end;

  TVideoReader = record
    Packet: TVideoPacket;
    ReaderIndex: Integer;
  end;

  IVideoStats = interface
    procedure PacketOutOfOrder(const aDelta: Integer);
    procedure PayloadTypeChange(const aPayloadType: UInt8);
    procedure PacketReceived(const aPacketIndex: Integer; const aPacket: TVideoPacket);
    procedure PacketRead(const aReaderIndex: Integer; const aPacketIndex: Integer; const aPacket: TVideoPacket);
    procedure ReaderSlow(const aReaderIndex: Integer; const aPacketIndex: Integer; const aPacket: TVideoPacket);
    procedure ReaderWait(const aReaderIndex: Integer; const aPacketIndex: Integer);
    procedure ReaderStopped(const aReaderIndex: Integer);
    procedure BufferAvailability(const aOpenPacketCount, aTotalPacketCount: Integer);
  end;

  IRTPVideoSink = interface
    ['{01F773F3-671F-4562-BB68-ECF69D33DEF7}']
    function GetPort: Integer;

    procedure Read(var aReader: TVideoReader; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);
    function WaitForSignal(const aReader: TVideoReader; const aTimeoutMs: Integer): Boolean;

    procedure RegisterReader(out aReader: TVideoReader);
    procedure UnregisterReader(var aReader: TVideoReader);

    procedure Close;

    property Port: Integer read GetPort;
  end;

  TRTPVideoSink = class(TInterfacedObject, IRTPVideoSink)
  private
    fStats: IVideoStats;
    fServer: TIdUDPServer;
    fClosed: Boolean;
    fWriteEvent: TEvent;

    fLastHeader: TRTPHeader;

    fPackets: TArray<TVideoPacket>;
    fWritePacketIndex: Integer;

    fReaderPacketIndexes: TArray<Integer>;

    procedure TimeoutError;
    procedure ClosedError;
  protected
    procedure Lock;
    procedure Unlock;

    procedure ReceivePacket(const aData: PByte; const aSize: Integer);

    procedure UDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle); virtual;
  protected
    // IRTPVideoSink
    function GetPort: Integer;

    procedure Read(var aReader: TVideoReader; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);
    function WaitForSignal(const aReader: TVideoReader; const aTimeoutMs: Integer): Boolean;

    procedure RegisterReader(out aReader: TVideoReader);
    procedure UnregisterReader(var aReader: TVideoReader);

    procedure Close;
  public
    constructor Create(const aPacketSize, aPacketCount: Integer; const aStats: IVideoStats);
    destructor Destroy; override;
  end;

  TDebugRTPVideoSink = class(TRTPVideoSink)
  private
    fFileStream: TFileStream;
  protected
    procedure UDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle); override;
  public
    constructor Create(const aFilename: String; const aPacketSize, aPacketCount: Integer; const aStats: IVideoStats); reintroduce;
    destructor Destroy; override;
  end;

  TDataMeterWindow = type String;

  TDataMeter = record
  private
    type
      PSnapshot = ^TSnapshot;
      TSnapshot = record
        Ticks, Bytes: Int64;
      end;
  private
    fSnapshots: array[0..3] of TSnapshot;
    fSnapshotIndex: Integer;
    fLastTicks: Int64;
  public
    procedure Add(const aBytes: Int64);
    procedure Reset;
    function GetBytesPerSecond(const aCurrent: Boolean = False; const aWindowTicks: Int64 = 0): Double;
  end;

  TLocalIPVersion = (IPv4, IPv6);

  TLocalIPInfo = record
    IP: String;
    IPVersion: Byte;
    Metric: Integer;
    FriendlyName: String;
  end;

  TLocalIPInfoArray = TArray<TLocalIPInfo>;

  TLocalIPInfoArrayHelper = record helper for TLocalIPInfoArray
  public
    function IndexOfIPVersion(const aIPVersion: Byte): Integer;
    function IndexOfLowestMetric(const aIPVersion: Byte): Integer;
    function IndexOf(const aIP: String): Integer;

    function IPVersion(const aIPVersion: Byte): TLocalIPInfo;
    function LowestMetric(const aIPVersion: Byte): TLocalIPInfo;

    function Remove(const aIP: String): TLocalIPInfoArray;
    function Keep(const aIPVersion: Byte): TLocalIPInfoArray;
  end;

  TSocketUtils = class abstract
  public
    class function GetLocalIPs: TArray<TLocalIPInfo>; static;
  end;

function Swap16(const aValue: UInt16): UInt16;
function Swap32(const aValue: UInt32): UInt32;
function Swap64(const aValue: UInt64): UInt64;

function BEToN16(const aValue: UInt16): UInt16;
function BEToN32(const aValue: UInt32): UInt32;
function BEToN64(const aValue: UInt64): UInt64;
function LEToN16(const aValue: UInt16): UInt16;
function LEToN32(const aValue: UInt32): UInt32;
function LEToN64(const aValue: UInt64): UInt64;

function NToBE16(const aValue: UInt16): UInt16;
function NToBE32(const aValue: UInt32): UInt32;
function NToBE64(const aValue: UInt64): UInt64;
function NToLE16(const aValue: UInt16): UInt16;
function NToLE32(const aValue: UInt32): UInt32;
function NToLE64(const aValue: UInt64): UInt64;

implementation

function Swap16(const aValue: UInt16): UInt16;
begin
  Result := Swap(aValue);
end;

function Swap32(const aValue: UInt32): UInt32;
begin
  Result := Swap16(UInt16(aValue));
  Result := (Result shl 16) or Swap16(UInt16(aValue shr 16));
end;

function Swap64(const aValue: UInt64): UInt64;
begin
  Result := Swap32(UInt32(aValue));
  Result := (Result shl 32) or Swap32(UInt32(aValue shr 32));
end;

function BEToN16(const aValue: UInt16): UInt16;
begin
  {$IFDEF LITTLEENDIAN}Result := Swap16(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function BEToN32(const aValue: UInt32): UInt32;
begin
  {$IFDEF LITTLEENDIAN}Result := Swap32(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function BEToN64(const aValue: UInt64): UInt64;
begin
  {$IFDEF LITTLEENDIAN}Result := Swap64(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function LEToN16(const aValue: UInt16): UInt16;
begin
  {$IFNDEF LITTLEENDIAN}Result := Swap16(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function LEToN32(const aValue: UInt32): UInt32;
begin
  {$IFNDEF LITTLEENDIAN}Result := Swap32(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function LEToN64(const aValue: UInt64): UInt64;
begin
  {$IFNDEF LITTLEENDIAN}Result := Swap64(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function NToBE16(const aValue: UInt16): UInt16;
begin
  {$IFDEF LITTLEENDIAN}Result := Swap16(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function NToBE32(const aValue: UInt32): UInt32;
begin
  {$IFDEF LITTLEENDIAN}Result := Swap32(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function NToBE64(const aValue: UInt64): UInt64;
begin
  {$IFDEF LITTLEENDIAN}Result := Swap64(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function NToLE16(const aValue: UInt16): UInt16;
begin
  {$IFNDEF LITTLEENDIAN}Result := Swap16(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function NToLE32(const aValue: UInt32): UInt32;
begin
  {$IFNDEF LITTLEENDIAN}Result := Swap32(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

function NToLE64(const aValue: UInt64): UInt64;
begin
  {$IFNDEF LITTLEENDIAN}Result := Swap64(aValue);{$ELSE}Result := aValue;{$ENDIF}
end;

{ TRingBuffer }

procedure TRingBuffer.Write(var Buffer; Count: Longint);
var
  lIndex: Integer;
  lToWrite: Integer;
begin
  if Length(fData) < fSize+Count then
    Resize(fSize+Count);

  lIndex := (fStartIndex+fSize) mod Length(fData);
  lToWrite := Min(Count, Length(fData)-lIndex);
  if lToWrite > 0 then
    Move(Buffer, fData[lIndex], lToWrite);
  Inc(fSize, lToWrite);

  if lToWrite < Count then
    Write(PByteArray(@Buffer)[lToWrite], Count-lToWrite);
end;

function TRingBuffer.Read(var Buffer; Count: Longint): Longint;
var
  lToRead: Integer;
begin
  lToRead := Min(Count, Min(fSize, Length(fData)-fStartIndex));
  if lToRead > 0 then
    Move(fData[fStartIndex], Buffer, lToRead);
  fStartIndex := (fStartIndex + lToRead) mod Length(fData);
  Dec(fSize, lToRead);
  Result := lToRead;

  if (lToRead < Count) and (fSize > 0) then
  begin
    Inc(Result, Read(PByteArray(@Buffer)[lToRead], Count-lToRead));
  end;
end;

procedure TRingBuffer.Resize(const aSize: Integer);
var
  lData: TBytes;
begin
  if (fStartIndex > 0) and (aSize > 0) then
  begin
    SetLength(lData, aSize);
    fSize := Read(lData[0], aSize);
    fData := lData;
    fStartIndex := 0;
  end
  else
  begin
    SetLength(fData, aSize);
    fSize := Min(fSize, aSize);
  end;
end;

function TRingBuffer.Seek(const Offset: Integer; Origin: TSeekOrigin): Integer;
var
  lToRead: Integer;
begin
  if Origin = TSeekOrigin.soCurrent then
  begin
    lToRead := Min(Offset, Length(fData)-fStartIndex);
    fStartIndex := (fStartIndex + lToRead) mod Length(fData);
    Dec(fSize, lToRead);

    if (lToRead < Offset) and (fSize > 0) then
    begin
      Seek(Offset-lToRead, soCurrent);
    end;
  end;

  Result := -1;
end;

{ TRTPHeader }

function TRTPHeader.GetPayloadType: UInt8;
begin
  Result := RawPayload and $7F;
end;

function TRTPHeader.GetSequenceNumber: UInt16;
begin
  Result := BEToN16(RawSequence);
end;

function TRTPHeader.GetVersion: UInt8;
begin
  Result := RawVersion shr 6;
end;

{ TRTPVideoSink }

constructor TRTPVideoSink.Create(const aPacketSize, aPacketCount: Integer; const aStats: IVideoStats);
var
  i: Integer;
begin
  fStats := aStats;

  fServer := TIdUDPServer.Create(nil);
  fServer.DefaultPort := 0;
  fServer.ThreadedEvent := True;
  fServer.OnUDPRead := UDPRead;
  fServer.Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_RCVBUF, 1024*1024);

  SetLength(fPackets, aPacketCount);
  for i := 0 to High(fPackets) do
    SetLength(fPackets[i].Data, aPacketSize);

  fWriteEvent := TEvent.Create(nil, True, False, '');

  fServer.Active := True;
end;

destructor TRTPVideoSink.Destroy;
begin
  fServer.Active := False;
  fServer.Free;

  fWriteEvent.Free;

  inherited;
end;

function TRTPVideoSink.GetPort: Integer;
begin
  if fServer.Bindings.Count > 0 then
  begin
    if fServer.Bindings[0].IPVersion = Id_IPv4 then
      Exit(fServer.Bindings[0].Port);
  end;
  Result := 0;
end;

procedure TRTPVideoSink.ReceivePacket(const aData: PByte; const aSize: Integer);
var
  i: Integer;
  lPacket: PVideoPacket;
  lOpenPacketCount: Integer;
begin
  Lock;
  try
    lPacket := @fPackets[fWritePacketIndex];

    if Length(lPacket.Data) < aSize then
      SetLength(lPacket.Data, aSize);

    Move(AData^, lPacket.Data[0], aSize);
    lPacket.Size := aSize;

    if Assigned(fStats) then
      fStats.PacketReceived(fWritePacketIndex, lPacket^);

    lOpenPacketCount := Length(fPackets);

    // Decide where we'll write the next packet to come in
    fWritePacketIndex := (fWritePacketIndex + 1) mod Length(fPackets);

    // If any readers will be looking at that packet index next, push them along to
    // the current packet.  Do not allow a slow reader to drag us behind because that
    // can lead to us losing packets from the tuner and ruining it for everyone.
    for i := 0 to High(fReaderPacketIndexes) do
    begin
      if fReaderPacketIndexes[i] <> -1 then
      begin
        if fReaderPacketIndexes[i] = fWritePacketIndex then
        begin
          if Assigned(fStats) then
            fStats.ReaderSlow(i, fReaderPacketIndexes[i], lPacket^);

          fReaderPacketIndexes[i] := (fWritePacketIndex + Length(fPackets) - 1) mod Length(fPackets);
        end;

        lOpenPacketCount := Min(lOpenPacketCount, ((fReaderPacketIndexes[i] - fWritePacketIndex) + Length(fPackets)) mod Length(fPackets));
      end;
    end;

    if Assigned(fStats) then
      fStats.BufferAvailability(lOpenPacketCount, Length(fPackets));

    // Reset the size of the next packet to zero
    fPackets[fWritePacketIndex].Size := 0;

    fWriteEvent.SetEvent;
  finally
    Unlock;
  end;
end;

procedure TRTPVideoSink.UDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  lHeader: PRTPHeader;
begin
  if Length(AData) < 12 then
    Exit;

  lHeader := @AData[0];

  Lock;
  try
    if fLastHeader.Version > 0 then
    begin
      if lHeader.SequenceNumber <> fLastHeader.SequenceNumber+1 then
        if Assigned(fStats) then
          fStats.PacketOutOfOrder(lHeader.SequenceNumber - fLastHeader.SequenceNumber);
      if lHeader.PayloadType <> fLastHeader.PayloadType then
        if Assigned(fStats) then
          fStats.PayloadTypeChange(lHeader.PayloadType);
    end;
    fLastHeader := lHeader^;
  finally
    Unlock;
  end;

  // Skip RTP header
  ReceivePacket(@AData[12], Length(Adata)-12);
end;

procedure TRTPVideoSink.Lock;
begin
  TMonitor.Enter(Self);
end;

procedure TRTPVideoSink.Unlock;
begin
  TMonitor.Exit(Self);
end;

procedure TRTPVideoSink.ClosedError;
begin
  raise EVideoClosedError.Create('Video stream closed');
end;

procedure TRTPVideoSink.TimeoutError;
begin
  raise EVideoError.Create('Timeout waiting for video data');
end;

procedure TRTPVideoSink.Read(var aReader: TVideoReader; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);
var
  lPacket: PVideoPacket;
  lToRead: Integer;
begin
  // Attempt to read packets until the size of aBuffer reaches aCount
  lToRead := aCount;
  repeat
    Lock;
    try
      while lToRead > 0 do
      begin
        lPacket := @fPackets[fReaderPacketIndexes[aReader.ReaderIndex]];
        // Do not advance into a packet index that the writer is writing to next
        if (fReaderPacketIndexes[aReader.ReaderIndex] <> fWritePacketIndex) then
        begin
          if Length(aReader.Packet.Data) < lPacket.Size then
            SetLength(aReader.Packet.Data, lPacket.Size);

          if lPacket.Size > 0 then
            Move(lPacket.Data[0], aReader.Packet.Data[0], lPacket.Size);
          aReader.Packet.Size := lPacket.Size;

          if aReader.Packet.Size > 0 then
            aBuffer.Write(aReader.Packet.Data[0], aReader.Packet.Size);

          Dec(lToRead, aReader.Packet.Size);

          if Assigned(fStats) then
            fStats.PacketRead(aReader.ReaderIndex, fReaderPacketIndexes[aReader.ReaderIndex], lPacket^);

          fReaderPacketIndexes[aReader.ReaderIndex] := (fReaderPacketIndexes[aReader.ReaderIndex] + 1) mod Length(fPackets);
        end
        else
        begin
          if Assigned(fStats) then
            fStats.ReaderWait(aReader.ReaderIndex, fReaderPacketIndexes[aReader.ReaderIndex]);
          Break;
        end;
      end;

      if lToRead <= 0 then
        Exit;

      fWriteEvent.ResetEvent;
    finally
      Unlock;
    end;

    if fClosed then
      ClosedError;

    // Packet is not yet ready
    if fWriteEvent.WaitFor(aTimeoutMs) = wrTimeout then
      TimeoutError;
  until False;
end;

procedure TRTPVideoSink.RegisterReader(out aReader: TVideoReader);
var
  i: Integer;
begin
  Lock;
  try
    aReader.Packet := Default(TVideoPacket);

    aReader.ReaderIndex := -1;
    for i := 0 to High(fReaderPacketIndexes) do
      if fReaderPacketIndexes[i] = -1 then
      begin
        aReader.ReaderIndex := i;
        fReaderPacketIndexes[i] := fWritePacketIndex;
        Break;
      end;

    if aReader.ReaderIndex = -1 then
    begin
      aReader.ReaderIndex := Length(fReaderPacketIndexes);
      Insert(fWritePacketIndex, fReaderPacketIndexes, Length(fReaderPacketIndexes));
    end;
  finally
    Unlock;
  end;
end;

procedure TRTPVideoSink.UnregisterReader(var aReader: TVideoReader);
begin
  Lock;
  try
    if aReader.ReaderIndex > -1 then
    begin
      if Assigned(fStats) then
        fStats.ReaderStopped(aReader.ReaderIndex);

      fReaderPacketIndexes[aReader.ReaderIndex] := -1;
      aReader.ReaderIndex := -1;
    end;
  finally
    Unlock;
  end;
end;

function TRTPVideoSink.WaitForSignal(const aReader: TVideoReader; const aTimeoutMs: Integer): Boolean;
begin
  repeat
    Lock;
    try
      Result := fReaderPacketIndexes[aReader.ReaderIndex] <> fWritePacketIndex;
      if Result then
        fReaderPacketIndexes[aReader.ReaderIndex] := fWritePacketIndex
      else
        fWriteEvent.ResetEvent;
    finally
      Unlock;
    end;

    if not Result then
    begin
      if fClosed then
        ClosedError;

      if fWriteEvent.WaitFor(aTimeoutMs) = wrTimeout then
        Break;
    end;
  until Result;
end;

procedure TRTPVideoSink.Close;
begin
  fClosed := True;
  Lock;
  try
    fStats := nil;
  finally
    Unlock;
  end;
end;

{ TDataMeter }

procedure TDataMeter.Add(const aBytes: Int64);
var
  lTicks: Int64;
begin
  lTicks := TStopWatch.GetTimestamp;

  if fSnapshots[fSnapshotIndex].Ticks > 0 then
  begin
    if lTicks > fSnapshots[fSnapshotIndex].Ticks+TTimeSpan.TicksPerSecond then
    begin
      fSnapshotIndex := (fSnapshotIndex + 1) mod Length(fSnapshots);
      fSnapshots[fSnapshotIndex].Bytes := 0;
      fSnapshots[fSnapshotIndex].Ticks := lTicks;
    end;
  end
  else
    fSnapshots[fSnapshotIndex].Ticks := lTicks;

  Inc(fSnapshots[fSnapshotIndex].Bytes, aBytes);
  fLastTicks := lTicks;
end;

function TDataMeter.GetBytesPerSecond(const aCurrent: Boolean = False; const aWindowTicks: Int64 = 0): Double;
var
  lIndex, i: Integer;
  lBytes, lStartTicks, lCurrentTicks, lWindowTicks: Int64;
begin
  if aCurrent then
    lCurrentTicks := TStopWatch.GetTimeStamp
  else
    lCurrentTicks := fLastTicks;

  lWindowTicks := aWindowTicks;
  if lWindowTicks = 0 then
    lWindowTicks := (Length(fSnapshots)+1) * TTimeSpan.TicksPerSecond;

  lStartTicks := 0;
  lBytes := 0;
  lIndex := fSnapshotIndex+1;
  for i := lIndex to lIndex+Length(fSnapshots) do
  begin
    lIndex := i mod Length(fSnapshots);
    if fSnapshots[lIndex].Ticks >= lCurrentTicks-lWindowTicks then
    begin
      if lStartTicks = 0 then
        lStartTicks := fSnapshots[lIndex].Ticks;
      Inc(lBytes, fSnapshots[lIndex].Bytes);
    end;
  end;
  if lStartTicks > 0 then
    Result := lBytes / ((lCurrentTicks - lStartTicks) / TTimeSpan.TicksPerSecond)
  else
    Result := 0;
end;

procedure TDataMeter.Reset;
begin
  FillChar(fSnapshots, SizeOf(fSnapshots), 0);
  fLastTicks := 0;
end;

{ TSocketUtils }

function RtlIpv4AddressToString(const Addr: PInAddr; S: PChar): PChar; stdcall; external 'Ntdll.dll' name {$IFDEF UNICODE}'RtlIpv4AddressToStringW'{$ELSE}'RtlIpv4AddressToStringA'{$ENDIF};
function RtlIpv6AddressToString(const Addr: PIn6Addr; S: PChar): PChar; stdcall; external 'Ntdll.dll' name {$IFDEF UNICODE}'RtlIpv6AddressToStringW'{$ELSE}'RtlIpv6AddressToStringA'{$ENDIF};

class function TSocketUtils.GetLocalIPs: TArray<TLocalIPInfo>;
var
  lAdapterAddresses, lCurAdapterAddress: PIP_ADAPTER_ADDRESSES;
  lSize: ULONG;
  lList: TList<TLocalIPInfo>;
  lCurAddress: PIP_ADAPTER_UNICAST_ADDRESS;
  lInfo: TLocalIPInfo;
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
        lList := TList<TLocalIPInfo>.Create;
        try
          lCurAdapterAddress := lAdapterAddresses;
          while Assigned(lCurAdapterAddress) do
          begin
            if (lCurAdapterAddress.IfType <> 24 {LoopBack}) and (lCurAdapterAddress.OperStatus = IfOperStatusUp) then
            begin
              lCurAddress := lCurAdapterAddress.FirstUnicastAddress;
              while Assigned(lCurAddress) do
              begin
                if Assigned(lCurAddress.Address.lpSockaddr) then
                begin
                  lInfo.IPVersion := 0;
                  case lCurAddress.Address.lpSockaddr.sin_family of
                    AF_INET: begin
                      SetLength(lInfo.IP, 16);
                      SetLength(lInfo.IP, (NativeInt(RtlIpv4AddressToString(@lCurAddress.Address.lpSockaddr.sin_addr, PChar(lInfo.IP)))-NativeInt(PChar(lInfo.IP))) div SizeOf(Char));
                      lInfo.IPVersion := 4;
                      lInfo.Metric := lCurAdapterAddress.Ipv4Metric;
                    end;
                    23{AF_INET6}: begin
                      SetLength(lInfo.IP, 46);
                      SetLength(lInfo.IP, (NativeInt(RtlIpv6AddressToString(@lCurAddress.Address.lpSockaddr.sin_addr, PChar(lInfo.IP)))-NativeInt(PChar(lInfo.IP))) div SizeOf(Char));
                      lInfo.IPVersion := 6;
                      lInfo.Metric := lCurAdapterAddress.Ipv6Metric;
                    end;
                  end;

                  if lInfo.IPVersion > 0 then
                  begin
                    lInfo.FriendlyName := WideString(lCurAdapterAddress.FriendlyName);
                    lList.Add(lInfo);
                  end;
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

{ TLocalIPInfoArrayHelper }

function TLocalIPInfoArrayHelper.IndexOfIPVersion(
  const aIPVersion: Byte): Integer;
var
  i: Integer;
begin
  for i := 0 to High(Self) do
    if (Self[i].IPVersion = aIPVersion) then
      Exit(i);
  Result := -1;
end;

function TLocalIPInfoArrayHelper.IndexOfLowestMetric(
  const aIPVersion: Byte): Integer;
var
  lMetric, i: Integer;
begin
  Result := -1;
  lMetric := MAXINT;
  for i := 0 to High(Self) do
    if (Self[i].IPVersion = aIPVersion) and (Self[i].Metric < lMetric) then
    begin
      Result := i;
      lMetric := Self[i].Metric;
    end;
end;

function TLocalIPInfoArrayHelper.IndexOf(const aIP: String): Integer;
var
  i: Integer;
begin
  for i := 0 to High(Self) do
    if (SameText(Self[i].IP, aIP)) then
      Exit(i);
  Result := -1;
end;

function TLocalIPInfoArrayHelper.IPVersion(
  const aIPVersion: Byte): TLocalIPInfo;
var
  i: Integer;
begin
  i := IndexOfIPVersion(aIPVersion);
  if i > -1 then
    Result := Self[i]
  else
    Result := Default(TLocalIPInfo);
end;

function TLocalIPInfoArrayHelper.LowestMetric(
  const aIPVersion: Byte): TLocalIPInfo;
var
  i: Integer;
begin
  i := IndexOfLowestMetric(aIPVersion);
  if i > -1 then
    Result := Self[i]
  else
    Result := Default(TLocalIPInfo);
end;

function TLocalIPInfoArrayHelper.Remove(const aIP: String): TLocalIPInfoArray;
var
  i, lCount: Integer;
begin
  i := IndexOf(aIP);
  if i > -1 then
  begin
    SetLength(Result, Length(Self)-1);
    lCount := 0;
    for i := 0 to High(Self) do
      if not SameText(Self[i].IP, aIP) then
      begin
        Result[lCount] := Self[i];
        Inc(lCount);
      end;
    SetLength(Result, lCount);
  end
  else
    Result := Self;
end;

function TLocalIPInfoArrayHelper.Keep(
  const aIPVersion: Byte): TLocalIPInfoArray;
var
  i, lCount: Integer;
begin
  SetLength(Result, Length(Self));
  lCount := 0;
  for i := 0 to High(Self) do
    if not Self[i].IPVersion <> aIPVersion then
    begin
      Result[lCount] := Self[i];
      Inc(lCount);
    end;
  SetLength(Result, lCount);
end;

{ TDebugRTPVideoSink }

procedure TDebugRTPVideoSink.UDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  lBuffer: array[0..2048] of Byte;
  lSize: Integer;
begin
  // Read from file instead
  lSize := fFileStream.Read(lBuffer, Length(AData));
  if lSize > 0 then
    ReceivePacket(@lBuffer[0], lSize);
end;

constructor TDebugRTPVideoSink.Create(const aFilename: String; const aPacketSize, aPacketCount: Integer; const aStats: IVideoStats);
begin
  fFileStream := TFile.OpenRead(aFilename);

  inherited Create(aPacketSize, aPacketCount, aStats);
end;

destructor TDebugRTPVideoSink.Destroy;
begin
  fFileStream.Free;

  inherited;
end;

end.
