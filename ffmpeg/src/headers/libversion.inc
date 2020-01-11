const
  // libavcodec
  LIBAVCODEC_VERSION_MAJOR = 58;
  LIBAVCODEC_VERSION_MINOR = 35;
  LIBAVCODEC_VERSION_MICRO = 100;
  LIBAVCODEC_VERSION       = '58.35.100';
  LIBAVCODEC_VERSION_INT   = (LIBAVCODEC_VERSION_MAJOR shl 16) or
                             (LIBAVCODEC_VERSION_MINOR shl 8) or
                              LIBAVCODEC_VERSION_MICRO;
  LIBAVCODEC_BUILD         = LIBAVCODEC_VERSION_INT;
  LIBAVCODEC_IDENT         = 'Lavc' + LIBAVCODEC_VERSION;

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *
 * @note, when bumping the major version it is recommended to manually
 * disable each FF_API_* in its own commit instead of disabling them all
 * at once through the bump. This improves the git bisect-ability of the change.
 *)

{$IFNDEF FF_API_LOWRES}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_LOWRES}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_DEBUG_MV}
  {$IF LIBAVCODEC_VERSION_MAJOR < 58}
    {$DEFINE FF_API_DEBUG_MV}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_AVCTX_TIMEBASE}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_AVCTX_TIMEBASE}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_CODED_FRAME}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_CODED_FRAME}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_SIDEDATA_ONLY_PKT}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_SIDEDATA_ONLY_PKT}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_VDPAU_PROFILE}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_VDPAU_PROFILE}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_CONVERGENCE_DURATION}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_CONVERGENCE_DURATION}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_AVPICTURE}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_AVPICTURE}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_AVPACKET_OLD_API}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_AVPACKET_OLD_API}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_RTP_CALLBACK}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_RTP_CALLBACK}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_VBV_DELAY}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_VBV_DELAY}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_CODER_TYPE}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_CODER_TYPE}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_STAT_BITS}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_STAT_BITS}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_PRIVATE_OPT}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_PRIVATE_OPT}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_ASS_TIMING}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_ASS_TIMING}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_OLD_BSF}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_OLD_BSF}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_COPY_CONTEXT}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_COPY_CONTEXT}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_GET_CONTEXT_DEFAULTS}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_GET_CONTEXT_DEFAULTS}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_NVENC_OLD_NAME}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_NVENC_OLD_NAME}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_STRUCT_VAAPI_CONTEXT}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_STRUCT_VAAPI_CONTEXT}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_MERGE_SD_API}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_MERGE_SD_API}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_TAG_STRING}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_TAG_STRING}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_GETCHROMA}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_GETCHROMA}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_CODEC_GET_SET}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_CODEC_GET_SET}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_USER_VISIBLE_AVHWACCEL}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_USER_VISIBLE_AVHWACCEL}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_LOCKMGR}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_LOCKMGR}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_NEXT}
  {$IF LIBAVCODEC_VERSION_MAJOR < 59}
    {$DEFINE FF_API_NEXT}
  {$IFEND}
{$ENDIF}

  // libavdevice
  LIBAVDEVICE_VERSION_MAJOR = 58;
  LIBAVDEVICE_VERSION_MINOR = 5;
  LIBAVDEVICE_VERSION_MICRO = 100;
  LIBAVDEVICE_VERSION       = '58.5.100';
  LIBAVDEVICE_VERSION_INT   = (LIBAVDEVICE_VERSION_MAJOR shl 16) or
                              (LIBAVDEVICE_VERSION_MINOR shl 8) or
                               LIBAVDEVICE_VERSION_MICRO;
  LIBAVDEVICE_BUILD         = LIBAVDEVICE_VERSION_INT;
  LIBAVDEVICE_IDENT         = 'Lavc' + LIBAVDEVICE_VERSION;

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *)

  // libavfilter
  LIBAVFILTER_VERSION_MAJOR  = 7;
  LIBAVFILTER_VERSION_MINOR  = 40;
  LIBAVFILTER_VERSION_MICRO  = 101;
  LIBAVFILTER_VERSION        = '7.40.101';
  LIBAVFILTER_VERSION_INT    = (LIBAVFILTER_VERSION_MAJOR shl 16) or
                               (LIBAVFILTER_VERSION_MINOR shl 8) or
                                LIBAVFILTER_VERSION_MICRO;
  LIBAVFILTER_BUILD          = LIBAVFILTER_VERSION_INT;
  LIBAVFILTER_IDENT          = 'Lavfi' + LIBAVFILTER_VERSION;

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *)

{$IFNDEF FF_API_OLD_FILTER_OPTS_ERROR}
  {$IF LIBAVFILTER_VERSION_MAJOR < 8}
    {$DEFINE FF_API_OLD_FILTER_OPTS_ERROR}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_LAVR_OPTS}
  {$IF LIBAVFILTER_VERSION_MAJOR < 8}
    {$DEFINE FF_API_LAVR_OPTS}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_FILTER_GET_SET}
  {$IF LIBAVFILTER_VERSION_MAJOR < 8}
    {$DEFINE FF_API_FILTER_GET_SET}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_NEXT}
  {$IF LIBAVFILTER_VERSION_MAJOR < 8}
    {$DEFINE FF_API_NEXT}
  {$IFEND}
{$ENDIF}

  // libavformat
  LIBAVFORMAT_VERSION_MAJOR = 58;
  LIBAVFORMAT_VERSION_MINOR = 20;
  LIBAVFORMAT_VERSION_MICRO = 100;
  LIBAVFORMAT_VERSION       = '58.20.100';
  LIBAVFORMAT_VERSION_INT   = (LIBAVFORMAT_VERSION_MAJOR shl 16) or
                              (LIBAVFORMAT_VERSION_MINOR shl 8) or
                               LIBAVFORMAT_VERSION_MICRO;
  LIBAVFORMAT_BUILD         = LIBAVFORMAT_VERSION_INT;
  LIBAVFORMAT_IDENT         = 'Lavf' + LIBAVFORMAT_VERSION;

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *
 * @note, when bumping the major version it is recommended to manually
 * disable each FF_API_* in its own commit instead of disabling them all
 * at once through the bump. This improves the git bisect-ability of the change.
 *
 *)

{$IFNDEF FF_API_COMPUTE_PKT_FIELDS2}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_COMPUTE_PKT_FIELDS2}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_OLD_OPEN_CALLBACKS}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_OLD_OPEN_CALLBACKS}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_LAVF_AVCTX}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_LAVF_AVCTX}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_HTTP_USER_AGENT}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_HTTP_USER_AGENT}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_HLS_WRAP}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_HLS_WRAP}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_HLS_USE_LOCALTIME}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_HLS_USE_LOCALTIME}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_LAVF_KEEPSIDE_FLAG}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_LAVF_KEEPSIDE_FLAG}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_OLD_ROTATE_API}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_OLD_ROTATE_API}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_FORMAT_GET_SET}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_FORMAT_GET_SET}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_OLD_AVIO_EOF_0}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_OLD_AVIO_EOF_0}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_LAVF_FFSERVER}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_LAVF_FFSERVER}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_FORMAT_FILENAME}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_FORMAT_FILENAME}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_OLD_RTSP_OPTIONS}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_OLD_RTSP_OPTIONS}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_NEXT}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_NEXT}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_DASH_MIN_SEG_DURATION}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_DASH_MIN_SEG_DURATION}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_LAVF_MP4A_LATM}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 59}
    {$DEFINE FF_API_LAVF_MP4A_LATM}
  {$IFEND}
{$ENDIF}

{$IFNDEF FF_API_R_FRAME_RATE}
  {$DEFINE FF_API_R_FRAME_RATE}
{$ENDIF}

  // libavutil
  LIBAVUTIL_VERSION_MAJOR = 56;
  LIBAVUTIL_VERSION_MINOR = 22;
  LIBAVUTIL_VERSION_MICRO = 100;
  LIBAVUTIL_VERSION       = '56.22.100';
  LIBAVUTIL_VERSION_INT   = (LIBAVUTIL_VERSION_MAJOR shl 16) or
                            (LIBAVUTIL_VERSION_MINOR shl 8) or
                             LIBAVUTIL_VERSION_MICRO;
  LIBAVUTIL_BUILD         = LIBAVUTIL_VERSION_INT;
  LIBAVUTIL_IDENT         = 'Lavu' + LIBAVUTIL_VERSION;

(**
 * @defgroup lavu_depr_guards Deprecation Guards
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *
 * @note, when bumping the major version it is recommended to manually
 * disable each FF_API_* in its own commit instead of disabling them all
 * at once through the bump. This improves the git bisect-ability of the change.
 *
 * @{
 *)

{$IFNDEF FF_API_VAAPI}
  {$IF LIBAVUTIL_VERSION_MAJOR < 57}
    {$DEFINE FF_API_VAAPI}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_FRAME_QP}
  {$IF LIBAVUTIL_VERSION_MAJOR < 57}
    {$DEFINE FF_API_FRAME_QP}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_PLUS1_MINUS1}
  {$IF LIBAVUTIL_VERSION_MAJOR < 57}
    {$DEFINE FF_API_PLUS1_MINUS1}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_ERROR_FRAME}
  {$IF LIBAVUTIL_VERSION_MAJOR < 57}
    {$DEFINE FF_API_ERROR_FRAME}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_PKT_PTS}
  {$IF LIBAVUTIL_VERSION_MAJOR < 57}
    {$DEFINE FF_API_PKT_PTS}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_CRYPTO_SIZE_T}
  {$IF LIBAVUTIL_VERSION_MAJOR < 57}
    {$DEFINE FF_API_CRYPTO_SIZE_T}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_FRAME_GET_SET}
  {$IF LIBAVUTIL_VERSION_MAJOR < 57}
    {$DEFINE FF_API_FRAME_GET_SET}
  {$IFEND}
{$ENDIF}
{$IFNDEF FF_API_PSEUDOPAL}
  {$IF LIBAVUTIL_VERSION_MAJOR < 57}
    {$DEFINE FF_API_PSEUDOPAL}
  {$IFEND}
{$ENDIF}

(**
 * @}
 *)

  // libswresample
  LIBSWRESAMPLE_VERSION_MAJOR = 3;
  LIBSWRESAMPLE_VERSION_MINOR = 3;
  LIBSWRESAMPLE_VERSION_MICRO = 100;
  LIBSWRESAMPLE_VERSION       = '3.3.100';
  LIBSWRESAMPLE_VERSION_INT   = (LIBSWRESAMPLE_VERSION_MAJOR shl 16) or
                                (LIBSWRESAMPLE_VERSION_MINOR shl 8) or
                                 LIBSWRESAMPLE_VERSION_MICRO;
  LIBSWRESAMPLE_BUILD         = LIBSWRESAMPLE_VERSION_INT;
  LIBSWRESAMPLE_IDENT         = 'SwS' + LIBSWRESAMPLE_VERSION;

  // libswscale
  LIBSWSCALE_VERSION_MAJOR = 5;
  LIBSWSCALE_VERSION_MINOR = 3;
  LIBSWSCALE_VERSION_MICRO = 100;
  LIBSWSCALE_VERSION       = '5.3.100';
  LIBSWSCALE_VERSION_INT   = (LIBSWSCALE_VERSION_MAJOR shl 16) or
                             (LIBSWSCALE_VERSION_MINOR shl 8) or
                              LIBSWSCALE_VERSION_MICRO;
  LIBSWSCALE_BUILD         = LIBSWSCALE_VERSION_INT;
  LIBSWSCALE_IDENT         = 'SwS' + LIBSWSCALE_VERSION;

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *)

{$IFNDEF FF_API_SWS_VECTOR}
  {$IF LIBAVUTIL_VERSION_MAJOR < 6}
    {$DEFINE FF_API_SWS_VECTOR}
  {$IFEND}
{$ENDIF}

{$IFDEF MSWINDOWS}
  CLibPrefix = '';
  CLibConnector = '-';
  CLibExtension = '.dll';
  _PU = '';
{$ENDIF}
{$IFDEF POSIX}
  CLibPrefix = 'lib';
  CLibConnector = '.';
  CLibExtension = '.dylib';
  // On Mac OSX, cdecl names have a preceeding underscore
  // if x86 native backend.
  {$IF Defined(UNDERSCOREIMPORTNAME)}
    _PU = '_';
  {$ELSE}
    _PU = '';
  {$IFEND}
{$ENDIF}

  SLIBAVCODEC_VERSION_MAJOR    = '58';
  SLIBAVDEVICE_VERSION_MAJOR   = '58';
  SLIBAVFILTER_VERSION_MAJOR   = '7';
  SLIBAVFORMAT_VERSION_MAJOR   = '58';
  SLIBAVUTIL_VERSION_MAJOR     = '56';
  SLIBSWRESAMPLE_VERSION_MAJOR = '3';
  SLIBSWSCALE_VERSION_MAJOR    = '5';

  AVCODEC_LIBNAME    = CLibPrefix + 'avcodec'    + CLibConnector + SLIBAVCODEC_VERSION_MAJOR    + CLibExtension;
  AVDEVICE_LIBNAME   = CLibPrefix + 'avdevice'   + CLibConnector + SLIBAVDEVICE_VERSION_MAJOR   + CLibExtension;
  AVFILTER_LIBNAME   = CLibPrefix + 'avfilter'   + CLibConnector + SLIBAVFILTER_VERSION_MAJOR   + CLibExtension;
  AVFORMAT_LIBNAME   = CLibPrefix + 'avformat'   + CLibConnector + SLIBAVFORMAT_VERSION_MAJOR   + CLibExtension;
  AVUTIL_LIBNAME     = CLibPrefix + 'avutil'     + CLibConnector + SLIBAVUTIL_VERSION_MAJOR     + CLibExtension;
  SWRESAMPLE_LIBNAME = CLibPrefix + 'swresample' + CLibConnector + SLIBSWRESAMPLE_VERSION_MAJOR + CLibExtension;
  SWSCALE_LIBNAME    = CLibPrefix + 'swscale'    + CLibConnector + SLIBSWSCALE_VERSION_MAJOR    + CLibExtension;
