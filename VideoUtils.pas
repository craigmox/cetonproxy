unit VideoUtils;

interface

uses
  System.SysUtils,

  libavformat,
  libavformat_avio,
  libavutil,
  libavutil_dict,
  libavutil_mem,
  libavutil_error,
  libavutil_log,
  libavutil_mathematics,
  libavcodec,

  FFUtils;

const
  cConverterPacketSize = 8192;

type
  EVideoConverterError = class(Exception);

  TVideoReadWriteEvent = reference to function(const aBuf: PByte; const aSize: Integer): Integer;

  TVideoLogEvent = reference to procedure(const aMsg: String);

  TVideoConverter = class
  private
    fOnRead, fOnWrite: TVideoReadWriteEvent;
    fOnLog: TVideoLogEvent;

    fInputFormatContext, fOutputFormatContext: PAVFormatContext;
    fPacket: TAVPacket;
    fStreamMapping: TArray<Integer>;
    fStreamInputDTS: TArray<Int64>;

    fProgramFilter: Integer;
    fFinished: Boolean;

    function GetErrorStr(const aErrorCode: Integer): String;

    procedure LogFmt(const aMsg: String; const aArgs: array of const);
    procedure ErrorFmt(const aMsg: String; const aArgs: array of const);

    function ReadPacket(const buf: PByte; const buf_size: Integer): Integer;
    function WritePacket(const buf: PByte; const buf_size: Integer): Integer;

    procedure SetupStreams;
    procedure Open;
    procedure Close;
  public
    constructor Create;

    function Next: Boolean;

    property OnRead: TVideoReadWriteEvent read fOnRead write fOnRead;
    property OnWrite: TVideoReadWriteEvent read fOnWrite write fOnWrite;
    property OnLog: TVideoLogEvent read fOnLog write fOnLog;

    property ProgramFilter: Integer read fProgramFilter write fProgramFilter;
  end;

implementation

function _ReadPacket(opaque: Pointer; buf: PByte; buf_size: Integer): Integer; cdecl;
begin
  Result := TVideoConverter(opaque).ReadPacket(buf, buf_size);
end;

function _WritePacket(opaque: Pointer; buf: PByte; buf_size: Integer): Integer; cdecl;
begin
  Result := TVideoConverter(opaque).WritePacket(buf, buf_size);
end;

procedure _LogCallback(avcl: Pointer; level: Integer; const fmt: PAnsiChar; vl: PAnsiChar); cdecl;
var
  lBuf: array[0..1024] of AnsiChar;
  lInt: Integer;
begin
  if level <= AV_LOG_INFO then
  begin
    if Assigned(avcl) and (AnsiString(PAVClass(PPointer(avcl)^)^.class_name) = 'AVFormatContext') then
    begin
      if Assigned(PAVFormatContext(Avcl)^.pb) and Assigned(PAVFormatContext(Avcl)^.pb.opaque) then
      begin
        lInt := 1;
        av_log_format_line(avcl, level, fmt, vl, @lBuf, SizeOf(lBuf)-1, @lint);
        TVideoConverter(PAVFormatContext(Avcl)^.pb.opaque).LogFmt('%s',[String(AnsiString(lBuf))]);
      end;
    end;
  end;
end;

{ TVideoConverter }

procedure TVideoConverter.Open;
var
  lRet: Integer;
begin
  fInputFormatContext := avformat_alloc_context;
  fInputFormatContext.pb := avio_alloc_context(av_malloc(cConverterPacketSize), cConverterPacketSize, 0, Self, _ReadPacket, nil, nil);

  lRet := avformat_open_input(@fInputFormatContext, nil, nil, nil);
  if lRet < 0 then
    ErrorFmt('Could not open input: %s',[GetErrorStr(lRet)]);

  lRet := avformat_find_stream_info(fInputFormatContext, nil);
  if (lRet < 0) or (not Assigned(fInputFormatContext.iformat)) then
    ErrorFmt('Failed to retrieve input stream information: %s',[GetErrorStr(lRet)]);

//  av_dump_format(ifmt_ctx, 0, in_filename, 0);

  avformat_alloc_output_context2(@fOutputFormatContext, nil, fInputFormatContext.iformat.name, nil);
  if not Assigned(fOutputFormatContext) then
    ErrorFmt('Could not create output context: %s',[GetErrorStr(lRet)]);

  fOutputFormatContext.pb := avio_alloc_context(av_malloc(cConverterPacketSize), cConverterPacketSize, 1, Self, nil, _WritePacket, nil);

  SetupStreams;

  lRet := avformat_write_header(fOutputFormatContext, nil);
  if lRet < 0 then
    ErrorFmt('Error occurred writing output header: %s', [GetErrorStr(lRet)]);
end;

function TVideoConverter.ReadPacket(const buf: PByte;
  const buf_size: Integer): Integer;
begin
  if Assigned(fOnRead) then
    Result := fOnRead(buf, buf_size)
  else
    Result := 0;
  if Result <= 0 then
    Result := AVERROR_EOF;
end;

procedure TVideoConverter.SetupStreams;
var
  i, lStreamIndex, lRet: Integer;
  lInStream, lOutStream: PAVStream;
  lInCodecParams: PAVCodecParameters;
begin
  lStreamIndex := 0;

  SetLength(fStreamMapping, fInputFormatContext.nb_streams);
  SetLength(fStreamInputDTS, Length(fStreamMapping));

  for i := 0 to High(fStreamMapping) do
  begin
    lInStream := PPtrIdx(fInputFormatContext.streams, i);
    lInCodecParams := lInStream.codecpar;

    if fProgramFilter > -1 then
    begin
      if avformat_match_stream_specifier(fInputFormatContext, lInStream, PAnsiChar(AnsiString('p:'+IntToStr(fProgramFilter)))) = 0 then
      begin
        fStreamMapping[i] := -1;
        Continue;
      end;
    end;

    if (lInCodecParams.codec_type <> AVMEDIA_TYPE_AUDIO) and
       (lInCodecParams.codec_type <> AVMEDIA_TYPE_VIDEO) and
       (lInCodecParams.codec_type <> AVMEDIA_TYPE_SUBTITLE) then
    begin
      fStreamMapping[i] := -1;
      Continue;
    end;

    fStreamInputDTS[i] := AV_NOPTS_VALUE;

    fStreamMapping[i] := lStreamIndex;
    Inc(lStreamIndex);

    lOutStream := avformat_new_stream(fOutputFormatContext, nil);
    if not Assigned(lOutStream) then
      ErrorFmt('Failed allocating output stream', []);

//    lOutStream.id := lInStream.id; // Causes black screen
    lOutStream.time_base := lInStream.time_base;
    lOutStream.avg_frame_rate := lInStream.avg_frame_rate;

    // Copy metadata (which includes audio language info)
    if Assigned(lInStream.metadata) then
      av_dict_copy(@lOutStream.metadata, lInStream.metadata, 0);

    lRet := avcodec_parameters_copy(lOutStream.codecpar, lInCodecParams);
    if lRet < 0 then
      ErrorFmt('Failed to copy codec parameters: %s', [GetErrorStr(lRet)]);

    lOutStream.codecpar.codec_tag.tag := 0;
  end;
//  av_dump_format(ofmt_ctx, 0, nil, 1);
end;

function TVideoConverter.WritePacket(const buf: PByte;
  const buf_size: Integer): Integer;
begin
  if Assigned(fOnWrite) then
    Result := fOnWrite(buf, buf_size)
  else
    Result := 0;
  if Result <= 0 then
    Result := AVERROR_EOF;
end;

procedure TVideoConverter.Close;
begin
  fFinished := True;

  if Assigned(fOutputFormatContext) then
    av_write_trailer(fOutputFormatContext);

  av_freep(@fInputFormatContext.pb.buffer);
  avio_context_free(@fInputFormatContext.pb);
  avformat_free_context(fInputFormatContext);

  av_freep(@fOutputFormatContext.pb.buffer);
  avio_context_free(@fOutputFormatContext.pb);
  avformat_free_context(fOutputFormatContext);

  fInputFormatContext := nil;
  fOutputFormatContext := nil;
  fStreamMapping := nil;
end;

constructor TVideoConverter.Create;
begin
  fProgramFilter := -1;
end;

procedure TVideoConverter.ErrorFmt(const aMsg: String;
  const aArgs: array of const);
begin
  raise EVideoConverterError.CreateFmt(aMsg, aArgs);
end;

function TVideoConverter.Next: Boolean;
var
  lRet: Integer;
  lInStream, lOutStream: PAVStream;
begin
  if fFinished then
    Exit(False);

  if not Assigned(fInputFormatContext) then
    Open;

  while True do
  begin
    lRet := av_read_frame(fInputFormatContext, @fPacket);
    if lRet < 0 then
      Break;
    try
      lInStream := PPtrIdx(fInputFormatContext.streams, fPacket.stream_index);
      if (fPacket.stream_index >= Length(fStreamMapping)) or
         (fStreamMapping[fPacket.stream_index] < 0) then
      begin
        Continue;
      end;

      // Drop packets with incorrect DTS values.  They must be sequential.
      if (fStreamInputDTS[fPacket.stream_index] = AV_NOPTS_VALUE) or (fPacket.dts > fStreamInputDTS[fPacket.stream_index]) then
      begin
        fStreamInputDTS[fPacket.stream_index] := fPacket.dts;

        fPacket.stream_index := fStreamMapping[fPacket.stream_index];
        lOutStream := PPtrIdx(fOutputFormatContext.streams, fPacket.stream_index);
    //    log_packet(ifmt_ctx, @pkt, 'in');

        // Copy packet
        fPacket.pts := av_rescale_q_rnd(fPacket.pts, lInStream.time_base, lOutStream.time_base, Ord(AV_ROUND_NEAR_INF) or Ord(AV_ROUND_PASS_MINMAX));
        fPacket.dts := av_rescale_q_rnd(fPacket.dts, lInStream.time_base, lOutStream.time_base, Ord(AV_ROUND_NEAR_INF) or Ord(AV_ROUND_PASS_MINMAX));
        fPacket.duration := av_rescale_q(fPacket.duration, lInStream.time_base, lOutStream.time_base);
        fPacket.pos := -1;
    //    log_packet(ofmt_ctx, @pkt, 'out');

//        lRet := av_interleaved_write_frame(fOutputFormatContext, @fPacket);
        lRet := av_write_frame(fOutputFormatContext, @fPacket);
        if lRet < 0 then
          ErrorFmt('Error muxing packet: %s', [GetErrorStr(lRet)]);
      end
      else
      begin
        LogFmt('Dropped packet with non-increasing timestamp on %s stream %d', [AnsiString(av_get_media_type_string(lInStream.codecpar.codec_type)), fPacket.stream_index]);
        Continue;
      end;
    finally
      av_packet_unref(@fPacket);
    end;

    Exit(True);
  end;

  Close;
  Result := False;
end;

function TVideoConverter.GetErrorStr(const aErrorCode: Integer): String;
var
 errbuf: array[0..AV_ERROR_MAX_STRING_SIZE-1] of AnsiChar;
begin
  FillChar(errbuf[0], SizeOf(errbuf), 0);
  if av_strerror(aErrorCode, @errbuf, AV_ERROR_MAX_STRING_SIZE) = 0 then
    Result := String(errbuf)
  else
    Result := '';
end;

procedure TVideoConverter.LogFmt(const aMsg: String;
  const aArgs: array of const);
begin
  if Assigned(fOnLog) then
    fOnLog(Format(Trim(aMsg), aArgs));
end;

initialization
  av_log_set_callback(_LogCallback);
finalization
end.
