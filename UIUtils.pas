unit UIUtils;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows;
{$ELSEIF MACOS}
  Macapi.AppKit;
{$ENDIF}

function IsControlKeyPressed: Boolean;
function IsShiftKeyPressed: Boolean;

implementation

function IsControlKeyPressed: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := GetKeyState(VK_CONTROL) < 0;
{$ELSEIF MACOS}
  Result := NSControlKeyMask and TNSEvent.OCClass.modifierFlags = NSControlKeyMask;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function IsShiftKeyPressed: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := GetKeyState(VK_SHIFT) < 0;
{$ELSEIF MACOS}
  Result := NSShiftKeyMask and TNSEvent.OCClass.modifierFlags = NSShiftKeyMask;
{$ELSE}
  Result := False;
{$ENDIF}
end;

end.
