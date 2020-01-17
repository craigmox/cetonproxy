unit VideoUtils;

interface

uses
  System.SysUtils,

  libavformat,
  libavformat_avio,
  libavutil,
  libavutil_mem,
  libavutil_error,
  libavutil_mathematics,
  libavcodec,

  FFUtils;

type
  EVideoConverterError = class(Exception);

  TVideoReadWriteEvent = reference to function(const aBuf: PByte; const aSize: Integer): Integer;

  TVideoConverter = class
  private
    fOnRead, fOnWrite: TVideoReadWriteEvent;

    fInputFormatContext, fOutputFormatContext: PAVFormatContext;
    fPacket: TAVPacket;
    fStreamMapping: TArray<Integer>;

    fProgramFilter: Integer;
    fFinished: Boolean;

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

{ TVideoConverter }

procedure TVideoConverter.Open;
var
  lRet: Integer;
begin
  fInputFormatContext := avformat_alloc_context;
  fInputFormatContext.pb := avio_alloc_context(av_malloc(8192), 8192, 0, Self, _ReadPacket, nil, nil);

  lRet := avformat_open_input(@fInputFormatContext, nil, nil, nil);
  if lRet < 0 then
    ErrorFmt('Could not open input',[]);

  lRet := avformat_find_stream_info(fInputFormatContext, nil);
  if (lRet < 0) or (not Assigned(fInputFormatContext.iformat)) then
    ErrorFmt('Failed to retrieve input stream information',[]);

//  av_dump_format(ifmt_ctx, 0, in_filename, 0);

  avformat_alloc_output_context2(@fOutputFormatContext, nil, fInputFormatContext.iformat.name, nil);
  if not Assigned(fOutputFormatContext) then
    ErrorFmt('Could not create output context',[]);

  fOutputFormatContext.pb := avio_alloc_context(av_malloc(8192), 8192, 1, Self, nil, _WritePacket, nil);

  SetupStreams;

  lRet := avformat_write_header(fOutputFormatContext, nil);
  if lRet < 0 then
    ErrorFmt('Error occurred writing output header', []);
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

    fStreamMapping[i] := lStreamIndex;
    Inc(lStreamIndex);

    lOutStream := avformat_new_stream(fOutputFormatContext, nil);
    if not Assigned(lOutStream) then
      ErrorFmt('Failed allocating output stream', []);

    lOutStream.id := lInStream.id;
    lOutStream.time_base := lInStream.time_base;
    lOutStream.avg_frame_rate := lInStream.avg_frame_rate;

    lRet := avcodec_parameters_copy(lOutStream.codecpar, lInCodecParams);
    if lRet < 0 then
      ErrorFmt('Failed to copy codec parameters', []);

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

      fPacket.stream_index := fStreamMapping[fPacket.stream_index];
      lOutStream := PPtrIdx(fOutputFormatContext.streams, fPacket.stream_index);
  //    log_packet(ifmt_ctx, @pkt, 'in');

      // Copy packet
      fPacket.pts := av_rescale_q_rnd(fPacket.pts, lInStream.time_base, lOutStream.time_base, Ord(AV_ROUND_NEAR_INF) or Ord(AV_ROUND_PASS_MINMAX));
      fPacket.dts := av_rescale_q_rnd(fPacket.dts, lInStream.time_base, lOutStream.time_base, Ord(AV_ROUND_NEAR_INF) or Ord(AV_ROUND_PASS_MINMAX));
      fPacket.duration := av_rescale_q(fPacket.duration, lInStream.time_base, lOutStream.time_base);
      fPacket.pos := -1;
  //    log_packet(ofmt_ctx, @pkt, 'out');

//      lRet := av_interleaved_write_frame(fOutputFormatContext, @fPacket);
      lRet := av_write_frame(fOutputFormatContext, @fPacket);
      if lRet < 0 then
        ErrorFmt('Error muxing packet', []);
    finally
      av_packet_unref(@fPacket);
    end;

    Exit(True);
  end;

  Close;
  Result := False;
end;

end.
