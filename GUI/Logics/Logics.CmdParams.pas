unit Logics.CmdParams;

interface

type
  TLogicsCmdParam = class
  private class var
    FCmdValue: String;
  private
    class function GetCmdParamExists(const AName: String): Boolean; static;
    class function GetCmdClient(): Boolean; static;
    class function GetCmdLoader(): Boolean; static;
  public
    class property CmdClient: Boolean read GetCmdClient;
    class property CmdLoader: Boolean read GetCmdLoader;
    class property CmdValue: String read FCmdValue;
  end;

implementation

{ TLogicsCmdParam }

class function TLogicsCmdParam.GetCmdParamExists(const AName: String): Boolean;
var
  TmpIndex: Integer;
begin
  Result := False;

  for TmpIndex := 1 to ParamCount do
    if ParamStr(TmpIndex) = AName then
    begin
      FCmdValue := ParamStr(TmpIndex + 1);
      Exit(True);
    end;
end;

class function TLogicsCmdParam.GetCmdClient(): Boolean;
begin
  Result := GetCmdParamExists('client');
end;

class function TLogicsCmdParam.GetCmdLoader(): Boolean;
begin
  Result := GetCmdParamExists('loader');
end;

end.
