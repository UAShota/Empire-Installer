unit Logics.GenerateLoader;

interface

uses
  System.SysUtils,
  System.IniFiles,

  Logics.Consts;

type
  TGenerateLoader = class
  public
    class procedure Generate(); static;
  end;

implementation

{ TGenerateLoader }

class procedure TGenerateLoader.Generate();
var
  TmpIni: TIniFile;
begin
  // Создадим исходный файл
  TmpIni := TIniFile.Create(TConsts.CS_FILE_ROOT + TConsts.CS_FILE_VERSION);
  try
    TConsts.ZipFile(ParamStr(0), ParamStr(0) + TConsts.CS_ZIP_EXT);
    TmpIni.WriteString(TConsts.CS_SECTION_INFO, TConsts.CS_VALUE_LOADER, TConsts.Version());
  finally
    FreeAndNil(TmpIni);
  end;
end;

end.
