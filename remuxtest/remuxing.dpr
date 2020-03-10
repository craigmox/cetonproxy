(*
 * Copyright (c) 2013 Stefano Sabatini
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

(**
 * @file
 * libavformat/libavcodec demuxing and muxing API example.
 *
 * Remux streams from one container format to another.
 * @example remuxing.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/remuxing.c
 * Ported by CodeCoolie@CNSW 2014/08/30 -> $Date:: 2019-08-24 #$
 *)

(*
FFmpeg Delphi/Pascal Headers and Examples License Agreement

A modified part of FFVCL - Delphi FFmpeg VCL Components.
Copyright (c) 2008-2019 DelphiFFmpeg.com
All rights reserved.
http://www.DelphiFFmpeg.com

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

This source code is provided "as is" by DelphiFFmpeg.com without
warranty of any kind, either expressed or implied, including but not
limited to the implied warranties of merchantability and/or fitness
for a particular purpose.

Please also notice the License agreement of FFmpeg libraries.
*)

program remuxing;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFDEF FPC}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF }
  SysUtils,
  {$ELSE}
  {$IF CompilerVersion >= 23.0}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF }
  System.SysUtils,
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF }
  SysUtils,
  {$IFEND }
  {$ENDIF }
  System.IOUtils,
  System.Classes,
  System.Math,
  FMX.Types,
  FFTypes in '..\ffmpeg\src\headers\FFTypes.pas',
  libavcodec in '..\ffmpeg\src\headers\libavcodec.pas',
  libavcodec_avfft in '..\ffmpeg\src\headers\libavcodec_avfft.pas',
  libavdevice in '..\ffmpeg\src\headers\libavdevice.pas',
  libavfilter in '..\ffmpeg\src\headers\libavfilter.pas',
  libavfilter_buffersink in '..\ffmpeg\src\headers\libavfilter_buffersink.pas',
  libavfilter_buffersrc in '..\ffmpeg\src\headers\libavfilter_buffersrc.pas',
  libavfilter_formats in '..\ffmpeg\src\headers\libavfilter_formats.pas',
  libavformat in '..\ffmpeg\src\headers\libavformat.pas',
  libavformat_avio in '..\ffmpeg\src\headers\libavformat_avio.pas',
  libavformat_url in '..\ffmpeg\src\headers\libavformat_url.pas',
  libavutil in '..\ffmpeg\src\headers\libavutil.pas',
  libavutil_audio_fifo in '..\ffmpeg\src\headers\libavutil_audio_fifo.pas',
  libavutil_avstring in '..\ffmpeg\src\headers\libavutil_avstring.pas',
  libavutil_bprint in '..\ffmpeg\src\headers\libavutil_bprint.pas',
  libavutil_buffer in '..\ffmpeg\src\headers\libavutil_buffer.pas',
  libavutil_channel_layout in '..\ffmpeg\src\headers\libavutil_channel_layout.pas',
  libavutil_common in '..\ffmpeg\src\headers\libavutil_common.pas',
  libavutil_cpu in '..\ffmpeg\src\headers\libavutil_cpu.pas',
  libavutil_dict in '..\ffmpeg\src\headers\libavutil_dict.pas',
  libavutil_display in '..\ffmpeg\src\headers\libavutil_display.pas',
  libavutil_error in '..\ffmpeg\src\headers\libavutil_error.pas',
  libavutil_eval in '..\ffmpeg\src\headers\libavutil_eval.pas',
  libavutil_fifo in '..\ffmpeg\src\headers\libavutil_fifo.pas',
  libavutil_file in '..\ffmpeg\src\headers\libavutil_file.pas',
  libavutil_frame in '..\ffmpeg\src\headers\libavutil_frame.pas',
  libavutil_hwcontext in '..\ffmpeg\src\headers\libavutil_hwcontext.pas',
  libavutil_imgutils in '..\ffmpeg\src\headers\libavutil_imgutils.pas',
  libavutil_log in '..\ffmpeg\src\headers\libavutil_log.pas',
  libavutil_mathematics in '..\ffmpeg\src\headers\libavutil_mathematics.pas',
  libavutil_md5 in '..\ffmpeg\src\headers\libavutil_md5.pas',
  libavutil_mem in '..\ffmpeg\src\headers\libavutil_mem.pas',
  libavutil_motion_vector in '..\ffmpeg\src\headers\libavutil_motion_vector.pas',
  libavutil_opt in '..\ffmpeg\src\headers\libavutil_opt.pas',
  libavutil_parseutils in '..\ffmpeg\src\headers\libavutil_parseutils.pas',
  libavutil_pixdesc in '..\ffmpeg\src\headers\libavutil_pixdesc.pas',
  libavutil_pixfmt in '..\ffmpeg\src\headers\libavutil_pixfmt.pas',
  libavutil_rational in '..\ffmpeg\src\headers\libavutil_rational.pas',
  libavutil_samplefmt in '..\ffmpeg\src\headers\libavutil_samplefmt.pas',
  libavutil_time in '..\ffmpeg\src\headers\libavutil_time.pas',
  libavutil_timestamp in '..\ffmpeg\src\headers\libavutil_timestamp.pas',
  libswresample in '..\ffmpeg\src\headers\libswresample.pas',
  libswscale in '..\ffmpeg\src\headers\libswscale.pas',
  FFUtils in '..\ffmpeg\src\examples\FFUtils.pas';

//function sprintf(CharBuf: PAnsiChar; const Format: PAnsiChar): Integer; cdecl; varargs; external 'msvcrt.dll';

procedure logcallback(avcl: Pointer; level: Integer; const fmt: PAnsiChar; vl: PAnsiChar); cdecl;
var
  lBuf: array[0..1024] of AnsiChar;
  lInt: Integer;
begin
  if level <= AV_LOG_INFO then
  begin
    if Assigned(avcl) then
    begin
      if AnsiString(PAVClass(PPointer(avcl)^)^.class_name) = 'AVFormatContext'  then
      begin
        if Assigned(PAVFormatContext(Avcl)^.pb) and Assigned(PAVFormatContext(Avcl)^.pb.opaque) then
        begin
          lInt := 1;
          av_log_format_line(avcl, level, fmt, vl, @lBuf, SizeOf(lBuf)-1, @lint);
          Write(TFileStream(PAVFormatContext(Avcl)^.pb.opaque).FileName + ': ' + AnsiString(lBuf));
        end;
      end;
    end;
  end;
end;

procedure log_packet(const fmt_ctx: PAVFormatContext; const pkt: PAVPacket; const tag: PAnsiChar);
var
  time_base: PAVRational;
begin
  time_base := @PAVStream(PPtrIdx(fmt_ctx.streams, pkt.stream_index)).time_base;

  Writeln(Format('%s: pts:%s pts_time:%s dts:%s dts_time:%s duration:%s duration_time:%s stream_index:%d',
        [tag,
         av_ts2str(pkt.pts), av_ts2timestr(pkt.pts, time_base),
         av_ts2str(pkt.dts), av_ts2timestr(pkt.dts, time_base),
         av_ts2str(pkt.duration), av_ts2timestr(pkt.duration, time_base),
         pkt.stream_index]));
end;

function readpacket(opaque: Pointer; buf: PByte; buf_size: Integer): Integer; cdecl;
begin
  Result := tfilestream(opaque).Read(buf^, buf_size);
  if Result = 0 then
    Result := AVERROR_EOF;
end;

function writepacket(opaque: Pointer; buf: PByte; buf_size: Integer): Integer; cdecl;
begin
  Result := tfilestream(opaque).Write(buf^, buf_size);
end;

function GetErrorStr(const aErrorCode: Integer): String;
var
 errbuf: array[0..AV_ERROR_MAX_STRING_SIZE-1] of AnsiChar;
begin
  FillChar(errbuf[0], SizeOf(errbuf), 0);
  if av_strerror(aErrorCode, @errbuf, AV_ERROR_MAX_STRING_SIZE) = 0 then
    Result := String(errbuf)
  else
    Result := '';
end;

function main(): Integer;
var
//  ofmt: PAVOutputFormat;
  ifmt_ctx, ofmt_ctx: PAVFormatContext;
  pkt: TAVPacket;
  in_avio: PAVIOContext;
  out_avio: PAVIOContext;

  in_filename, out_filename: PAnsiChar;

  ret, i: Integer;

  stream_index: Integer;
  stream_mapping: TArray<Integer>;

  out_stream: PAVStream;
  in_stream: PAVStream;

  in_codecpar: PAVCodecParameters;

  infile, outfile: TFileStream;

  lastdts, newdts: Int64;
  lStreamdts: array[0..10] of int64;
label
  the_end;
begin
//  av_log_set_level(AV_LOG_TRACE);
  av_log_set_callback(logcallback);

//  ofmt := nil;
  ifmt_ctx := nil;
  ofmt_ctx := nil;
  stream_index := 0;
  stream_mapping := nil;

  if ParamCount < 2 then
  begin
    Writeln(ErrOutput, Format('usage: %s input output' + sLineBreak +
           'API example program to remux a media file with libavformat and libavcodec.' + sLineBreak +
           'The output format is guessed according to the file extension.',
           [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;

  in_filename  := PAnsiChar(AnsiString(ParamStr(1)));
  out_filename := PAnsiChar(AnsiString(ParamStr(2)));

  infile := TFile.OpenRead(ansistring(in_filename));
  outfile := TFile.Create(ansistring(out_filename));

  ifmt_ctx := avformat_alloc_context;
  ifmt_ctx.pb := avio_alloc_context(av_malloc(8192), 8192, 0, infile, readpacket, nil, nil);

  ret := avformat_open_input(@ifmt_ctx, nil, nil, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Could not open input file ''%s''', [in_filename]));
    goto the_end;
  end;

  ret := avformat_find_stream_info(ifmt_ctx, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Failed to retrieve input stream information');
    goto the_end;
  end;

  av_dump_format(ifmt_ctx, 0, in_filename, 0);

  avformat_alloc_output_context2(@ofmt_ctx, nil, ifmt_ctx.iformat.name, nil);
  if not Assigned(ofmt_ctx) then
  begin
    Writeln(ErrOutput, 'Could not create output context');
    ret := AVERROR_UNKNOWN;
    goto the_end;
  end;

  ofmt_ctx.pb := avio_alloc_context(av_malloc(8192), 8192, 1, outfile, nil, writepacket, nil);

  SetLength(stream_mapping, ifmt_ctx.nb_streams);

//  ofmt := ofmt_ctx.oformat;

  for i := 0 to ifmt_ctx.nb_streams - 1 do
  begin
    in_stream := PPtrIdx(ifmt_ctx.streams, i);
    in_codecpar := in_stream.codecpar;

    if avformat_match_stream_specifier(ifmt_ctx, in_stream, 'p:467') = 0 then
    begin
      stream_mapping[i] := -1;
      Continue;
    end;

    if (in_codecpar.codec_type <> AVMEDIA_TYPE_AUDIO) and
       (in_codecpar.codec_type <> AVMEDIA_TYPE_VIDEO) and
       (in_codecpar.codec_type <> AVMEDIA_TYPE_SUBTITLE) then
    begin
      stream_mapping[i] := -1;
      Continue;
    end;

    stream_mapping[i] := stream_index;
    Inc(stream_index);

    out_stream := avformat_new_stream(ofmt_ctx, nil);
    if not Assigned(out_stream) then
    begin
      Writeln(ErrOutput, 'Failed allocating output stream');
      ret := AVERROR_UNKNOWN;
      goto the_end;
    end;

    if Assigned(in_stream.metadata) then
      av_dict_copy(@out_stream.metadata, in_stream.metadata, 0);

//    out_stream.id := in_stream.id;
    out_stream.time_base := in_stream.time_base;
    out_stream.avg_frame_rate := in_stream.avg_frame_rate;

    ret := avcodec_parameters_copy(out_stream.codecpar, in_codecpar);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Failed to copy codec parameters');
      goto the_end;
    end;
    out_stream.codecpar.codec_tag.tag := 0;
  end;
  av_dump_format(ofmt_ctx, 0, nil, 1);

{  if (ofmt.flags and AVFMT_NOFILE) = 0 then
  begin
    ret := avio_open(@ofmt_ctx.pb, out_filename, AVIO_FLAG_WRITE);
    if ret < 0 then
    begin
      Writeln(ErrOutput, Format('Could not open output file ''%s''', [out_filename]));
      goto the_end;
    end;
  end;}

  ret := avformat_write_header(ofmt_ctx, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Error occurred when opening output file');
    goto the_end;
  end;

  for i := 0 to High(lastdts) do
    lStreamdts[i] := 0;

  while True do
  begin
    ret := av_read_frame(ifmt_ctx, @pkt);
    if ret < 0 then
      Break;

    if pkt.size > 0 then
    begin
      in_stream  := PPtrIdx(ifmt_ctx.streams, pkt.stream_index);
      if (pkt.stream_index >= Length(stream_mapping)) or
         (stream_mapping[pkt.stream_index] < 0) then
      begin
        av_packet_unref(@pkt);
        Continue;
      end;

      pkt.stream_index := stream_mapping[pkt.stream_index];
      out_stream := PPtrIdx(ofmt_ctx.streams, pkt.stream_index);
  //    log_packet(ifmt_ctx, @pkt, 'in');

//      if pkt.dts > lstreamdts[pkt.stream_index] then
      begin
        lstreamdts[pkt.stream_index] := pkt.dts;
//        if pkt.stream_index = 1 then
//          log.d('in dts %d', [pkt.dts]);

        (* copy packet *)
        pkt.pts := av_rescale_q_rnd(pkt.pts, in_stream.time_base, out_stream.time_base, Ord(AV_ROUND_NEAR_INF) or Ord(AV_ROUND_PASS_MINMAX));
        pkt.dts := av_rescale_q_rnd(pkt.dts, in_stream.time_base, out_stream.time_base, Ord(AV_ROUND_NEAR_INF) or Ord(AV_ROUND_PASS_MINMAX));
        pkt.duration := av_rescale_q(pkt.duration, in_stream.time_base, out_stream.time_base);
        pkt.pos := -1;
    //    log_packet(ofmt_ctx, @pkt, 'out');

//        if pkt.stream_index = 1 then
//          log.d('out dts %d', [pkt.dts]);

//        ret := av_interleaved_write_frame(ofmt_ctx, @pkt);
            Ret := av_write_frame(ofmt_ctx, @pkt);
          if ret < 0 then
          begin
            Writeln(ErrOutput, 'Error muxing packet '+GetErrorStr(ret));
            Break;
          end;
      end
{      else
        log.d('invalid dts on %d: %d', [pkt.stream_index, pkt.dts])};
    end;

    av_packet_unref(@pkt);
  end;

  av_write_trailer(ofmt_ctx);
the_end:

//  in_buff := ifmt_ctx.pb.buffer;
  av_freep(@ifmt_ctx.pb.buffer);
  avio_context_free(@ifmt_ctx.pb);
  avformat_free_context(ifmt_ctx);
//  avformat_close_input(@ifmt_ctx);

  (* close output *)

  av_freep(@ofmt_ctx.pb.buffer);
  avio_context_free(@ofmt_ctx.pb);
{  if Assigned(ofmt_ctx) and ((ofmt.flags and AVFMT_NOFILE) = 0) then
    avio_closep(@ofmt_ctx.pb);}
  avformat_free_context(ofmt_ctx);

//  av_freep(@stream_mapping);

  if (ret < 0) and (ret <> AVERROR_EOF) then
  begin
    Writeln(ErrOutput, Format('Error occurred: %s', [av_err2str(ret)]));
    Result := 1;
    Exit;
  end;

  Result := 0;
end;

begin
  try
    ExitCode := main();
    ReadLn;
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
