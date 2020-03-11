unit LogUtils;

interface

uses
  System.SysUtils;

const
  cLogDefault = 'default';
  cLogDiscovery = 'discovery';
  cLogNames: Array[0..1] of String = (cLogDefault, cLogDiscovery);

type
  ILogger = interface
    ['{772B05D3-D06A-4E0D-A259-929772F8704D}']
    procedure Log(const aLogName: String; const aMessage: String);
  end;

  TLogger = class abstract
  private
    class var
      fLogger: ILogger;
  public
    class procedure Log(const aLogName: String; const aMessage: String); static;
    class procedure LogFmt(const aLogName: String; const aMessage: String; const aArgs: array of const); static;

    class procedure SetLogger(const aLogger: ILogger); static;
  end;

implementation

{ TLogger }

class procedure TLogger.Log(const aLogName: String; const aMessage: String);
begin
  if Assigned(fLogger) then
    fLogger.Log(aLogName, aMessage);
end;

class procedure TLogger.SetLogger(const aLogger: ILogger);
begin
  fLogger := aLogger;
end;

class procedure TLogger.LogFmt(const aLogName: String; const aMessage: String; const aArgs: array of const);
begin
  Log(aLogName, Format(aMessage, aArgs));
end;

end.
