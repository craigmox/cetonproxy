unit FileUtils;

interface

uses
  System.SysUtils,
  System.IOUtils;

type
  TFile = System.IOUtils.TFile;

  TFileHelper = record helper for TFile
  public
    class function GetSize(const iFileName: String): Int64; static;
  end;

implementation

uses
  System.Classes
  {$IFDEF MSWINDOWS}
  , Winapi.Windows
  {$ENDIF}

  {$IFDEF POSIX}
  , Posix.SysStat
  {$ENDIF}
  ;

{ TFileHelper }

class function TFileHelper.GetSize(const iFileName: String): Int64;
{$IFDEF MSWINDOWS}
var
  info: TWin32FileAttributeData;
  OrgErrMode: Integer;
begin
  Result := -1;
  if (not TFile.Exists(iFileName)) then
    Exit;

  OrgErrMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if not GetFileAttributesEx(PWideChar(iFileName), GetFileExInfoStandard, @info) then
      Exit;

    Result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
  finally
    SetErrorMode(OrgErrMode);
  end;
end;
{$ENDIF}

{$IFDEF POSIX}
var
  Rec: _stat;
  M: TMarshaller;
begin
  Result := -1;
  if (not TFile.Exists(iFileName)) then
    Exit;

  FillChar(Rec, SizeOf(Rec), 0);

  if (stat(M.AsAnsi(iFileName).ToPointer, Rec) = 0) then
    Result := Rec.st_size;
end;
{$ENDIF}

end.
