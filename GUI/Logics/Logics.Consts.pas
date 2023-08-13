unit Logics.Consts;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Zip,
  System.UITypes,
  Vcl.Dialogs,
  IdHashSHA;

type
  TConsts = class
  public const
    CS_SECTION_FILE = 'FILES';
    CS_SECTION_INFO = 'INFO';
    CS_VALUE_CLIENT = 'client';
    CS_VALUE_LOADER = 'loader';
    CS_VALUE_NEWS = 'news';
    CS_FILE_ROOT = '.\';
    CS_FILE_CURRENT = CS_FILE_ROOT + 'GalaxyHopes\';
    CS_FILE_VERSION = 'version.txt';
    CS_ZIP_EXT = '.zip';
  private class var
    FIdHashSHA1: TIdHashSHA1;
  public
    class constructor Create();
    class destructor Destroy();
    class function FileHashSHA1(const AFileName: String): String; static;
    class procedure ZipFile(const ASource, ADestination: String); static;
    class procedure UnZipFile(const AFileName: String); static;
    class procedure ShowException(const AMethod, ASource: String); static;
    class function Version(): String; static;
  end;

implementation

{ TConsts }

class constructor TConsts.Create();
begin
  FIdHashSHA1 := TIdHashSHA1.Create;
end;

class destructor TConsts.Destroy();
begin
  FreeAndNil(FIdHashSHA1);
end;

class function TConsts.FileHashSHA1(const AFileName: String): String;
var
  TmpFileStream: TFileStream;
begin
  if (not FileExists(AFileName)) then
    Exit(EmptyStr);
  TmpFileStream := TFileStream.Create(AFileName, fmOpenRead and fmShareDenyNone);
  try
    Result := FIdHashSHA1.HashStreamAsHex(TmpFileStream);
  finally
    FreeAndNil(TmpFileStream);
  end;
end;

class procedure TConsts.ZipFile(const ASource, ADestination: String);
var
  TmpZip: TZipFile;
begin
  TmpZip := TZipFile.Create();
  try
    TmpZip.Open(ADestination, zmWrite);
    TmpZip.Add(ASource);
    TmpZip.Close();
  finally
    FreeAndNil(TmpZip);
  end;
end;

class procedure TConsts.UnZipFile(const AFileName: String);
var
  TmpZip: TZipFile;
begin
  TmpZip := TZipFile.Create();
  try
    TmpZip.ExtractZipFile(AFileName, ExtractFilePath(AFileName));
    DeleteFile(AFileName);
  finally
    FreeAndNil(TmpZip);
  end;
end;

class procedure TConsts.ShowException(const AMethod, ASource: String);
begin
  MessageDlg(AMethod + #13#10 + SysErrorMessage(GetLastError()) + #13#10 + ASource, mtError, [mbAbort], 0);
  Halt(0);
end;

class function TConsts.Version(): String;
var
  TmpVerInfoSize: Cardinal;
  TmpVerInfo: Pointer;
  TmpVerValueSize: Cardinal;
  TmpVerValue: PVSFixedFileInfo;
  TmpHandle: Cardinal;
begin
  TmpVerInfoSize := GetFileVersionInfoSize(PWideChar(ParamStr(0)), TmpHandle);
  GetMem(TmpVerInfo, TmpVerInfoSize);
  try
    if (not GetFileVersionInfo(PChar(ParamStr(0)), 0, TmpVerInfoSize, TmpVerInfo)
      or not VerQueryValue(TmpVerInfo, '\', Pointer(TmpVerValue), TmpVerValueSize))
    then
      ShowException('Version', ParamStr(0));
    with TmpVerValue^ do
      Result := Format('%d.%d.%d.%d', [
      dwFileVersionMS shr 16,
      dwFileVersionMS and $FFFF,
      dwFileVersionLS shr 16,
      dwFileVersionLS and $FFFF
    ]);
  finally
    FreeMem(TmpVerInfo, TmpVerInfoSize);
  end;
end;

end.
