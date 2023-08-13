unit Logics.GenerateClient;

interface

uses
  System.SysUtils,
  System.IniFiles,

  Logics.Consts;

type
  TGenerateClient = class
  class var
    FIniFile: TIniFile;
    FVersion: String;
  private
    class procedure DoPath(const APath: String; AClean: Boolean); static;
    class procedure DoFile(const APath, AFileName: String; APass: Integer = 0); static;
  public
    class procedure Generate(const AVersion: String); static;
  end;

implementation

{ TGenerateClient }

class procedure TGenerateClient.DoFile(const APath, AFileName: String; APass: Integer = 0);
var
  TmpFilePath: String;
begin
  TmpFilePath := IncludeTrailingPathDelimiter(TConsts.CS_FILE_ROOT + FVersion) + APath;
  if (not ForceDirectories(TmpFilePath)) then
    TConsts.ShowException('DoFile', TmpFilePath);
  try
    WriteLn('+ file ', APath, AFileName, ' pass ', APass);
    // Запишем файл в лист хеша
    FIniFile.WriteString(TConsts.CS_SECTION_FILE, APath + AFileName,
      TConsts.FileHashSHA1(TConsts.CS_FILE_CURRENT + APath + AFileName));
    // Зазипуем файл
    TConsts.ZipFile(TConsts.CS_FILE_CURRENT + APath + AFileName,
      TmpFilePath + AFileName + TConsts.CS_ZIP_EXT);
  except
    on E: Exception do
    begin
      WriteLn(' E ', E.Message);
      if (APass < 300) then
      begin
        Inc(APass);
        Sleep(100);
        DoFile(APath, AFileName, APass);
      end else
        raise E;
    end;
  end;
end;

class procedure TGenerateClient.DoPath(const APath: String; AClean: Boolean);
var
  TmpSR: TSearchRec;
  TmpFileName: String;
begin
  // Поищем все доступные файлы
  if (FindFirst(TConsts.CS_FILE_CURRENT + APath + '*.*', faAnyFile, TmpSR) = 0) then
  repeat
    if (TmpSR.Name = TConsts.CS_FILE_VERSION)
      or (TmpSR.Name = '.')
      or (TmpSR.Name = '..')
    then
      Continue;
    // Определим файл или каталог
    if (TmpSR.Attr and faDirectory = faDirectory) then
      DoPath(IncludeTrailingPathDelimiter(APath + TmpSR.Name), AClean)
    else
    begin
      // Архивирование файла
      if (not AClean) then
        DoFile(APath, TmpSR.Name)
      else
      // Удаление исходного файла
      begin
        TmpFileName := TConsts.CS_FILE_CURRENT + APath + TmpSR.Name;
        if (not DeleteFile(TmpFileName)) then
          TConsts.ShowException('DoPath delete file', TmpFileName);
      end;
    end;
  until FindNext(TmpSR) <> 0;
  FindClose(TmpSR);
  // Зачистим пустые каталоги
  if (APath <> EmptyStr) then
    RemoveDir(TConsts.CS_FILE_CURRENT + APath);
end;

class procedure TGenerateClient.Generate(const AVersion: String);
var
  TmpFilePath: String;
begin
  FVersion := AVersion;
  TmpFilePath := IncludeTrailingPathDelimiter(TConsts.CS_FILE_ROOT + FVersion);
  if (DirectoryExists(TmpFilePath)) then
    TConsts.ShowException('Generate already exists', TmpFilePath);
  if (not ForceDirectories(TmpFilePath)) then
    TConsts.ShowException('Generate force dir', TmpFilePath);
  // Создадим исходный файл
  FIniFile := TIniFile.Create(TmpFilePath + TConsts.CS_FILE_VERSION);
  try
    DoPath(EmptyStr, False);
  finally
    FreeAndNil(FIniFile);
  end;
  {$IFNDEF DEBUG}
  DoPath(EmptyStr, True);
  {$ENDIF}
  // Пропишем его для загрузки
  FIniFile := TIniFile.Create(TConsts.CS_FILE_ROOT + TConsts.CS_FILE_VERSION);
  try
    FIniFile.WriteString(TConsts.CS_SECTION_INFO, TConsts.CS_VALUE_CLIENT, AVersion);
  finally
    FreeAndNil(FIniFile);
  end;
end;

end.
