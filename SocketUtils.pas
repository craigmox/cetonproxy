unit SocketUtils;

interface

{$SCOPEDENUMS ON}
{$DEFINE LITTLEENDIAN}

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Math,
  FMX.Types,
  IdUDPServer,
  IdGlobal,
  IdSocketHandle,
  IdStack,
  IdStackConsts;

type
  EVideoError = class(Exception);

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

  TRTPVideoSink = class
  private
    fServer: TIdUDPServer;
    fWriteEvent: TEvent;

    fLastHeader: TRTPHeader;

    fPackets: TArray<TVideoPacket>;
    fWritePacketIndex: Integer;

    fReaderPacketIndexes: TArray<Integer>;

    function GetPort: Integer;
    procedure TimeoutError;
  protected
    procedure Lock;
    procedure Unlock;

    procedure UDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
  public
    constructor Create(const aPacketSize, aPacketCount: Integer);
    destructor Destroy; override;

    procedure Read(var aReader: TVideoReader; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);
    function WaitForSignal(const aReader: TVideoReader; const aTimeoutMs: Integer): Boolean;

    procedure RegisterReader(out aReader: TVideoReader);
    procedure UnregisterReader(var aReader: TVideoReader);

    property Port: Integer read GetPort;
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

constructor TRTPVideoSink.Create(const aPacketSize, aPacketCount: Integer);
var
  i: Integer;
begin
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

procedure TRTPVideoSink.UDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  lPacket: PVideoPacket;
  lWritten, lToWrite: Integer;
  lHeader: PRTPHeader;
  i: Integer;
begin
  if Length(AData) < 12 then
    Exit;

  lHeader := @AData[0];

  if fLastHeader.Version > 0 then
  begin
    if lHeader.SequenceNumber <> fLastHeader.SequenceNumber+1 then
      Log.D('Out of order %d', [lHeader.SequenceNumber - fLastHeader.SequenceNumber]);
    if lHeader.PayloadType <> fLastHeader.PayloadType then
      Log.D('Payload type %d', [lHeader.PayloadType]);
  end;
  fLastHeader := lHeader^;

  lWritten := 12; // Skip RTP header
  lToWrite := Length(AData)-lWritten;

  Lock;
  try
    lPacket := @fPackets[fWritePacketIndex];

    if Length(lPacket.Data) < lToWrite then
      SetLength(lPacket.Data, lToWrite);

    Move(AData[lWritten], lPacket.Data[0], lToWrite);
    lPacket.Size := lToWrite;

    // Decide where we'll write the next packet to come in
    fWritePacketIndex := (fWritePacketIndex + 1) mod Length(fPackets);

    // If any readers will be looking at that packet index next, push them along to
    // the next packet.  Do not allow a slow reader to drag us behind because that
    // can lead to packet loss.
    for i := 0 to High(fReaderPacketIndexes) do
    begin
      if fReaderPacketIndexes[i] = fWritePacketIndex then
      begin
        fReaderPacketIndexes[i] := (fReaderPacketIndexes[i] + 1) mod Length(fPackets);
        Break;
      end;
    end;

    fWriteEvent.SetEvent;
  finally
    Unlock;
  end;
end;

procedure TRTPVideoSink.Lock;
begin
  TMonitor.Enter(Self);
end;

procedure TRTPVideoSink.Unlock;
begin
  TMonitor.Exit(Self);
end;

procedure TRTPVideoSink.TimeoutError;
begin
  raise EVideoError.Create('Timeout waiting for video data');
end;

procedure TRTPVideoSink.Read(var aReader: TVideoReader; const aBuffer: TRingBuffer; const aCount: Integer; const aTimeoutMs: Integer);
var
  lPacket: PVideoPacket;
begin
  // Attempt to read packets until the size of aBuffer reaches aCount
  repeat
    Lock;
    try
      while aBuffer.Size < aCount do
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

          fReaderPacketIndexes[aReader.ReaderIndex] := (fReaderPacketIndexes[aReader.ReaderIndex] + 1) mod Length(fPackets);
        end
        else
          Break;
      end;

      if aBuffer.Size >= aCount then
        Exit;

      fWriteEvent.ResetEvent;
    finally
      Unlock;
    end;

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
    fReaderPacketIndexes[aReader.ReaderIndex] := -1;
    aReader.ReaderIndex := -1;
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
      if fWriteEvent.WaitFor(aTimeoutMs) = wrTimeout then
        Break;
  until Result;
end;

end.
