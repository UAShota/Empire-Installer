unit Logics.Updater;

interface

uses
  Winapi.ShellApi,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.IniFiles,

  Logics.Consts,
  Logics.Downloader;

type
  TUpdater = class
  private type
    TStep = (stEmpty, stVersion, stClient, stFiles, stDownload, stDone);
    TUpdateError = procedure (AErrorCode: Integer; const AUrl: String) of object;
  private var
    FDownloader: TDownloader;
    FStep: TStep;
    FIniFile: TIniFile;
    FFileList: TStringList;
    FListIndex: Integer;
    FListFileName: String;
    FLastLoader: String;
    FLastNews: String;
    FLastClient: String;
    FOnVersionChanged: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnError: TUpdateError;
  private const
    CS_URL = 'https://galaxyhopes.ru/client/';
    CS_DOWNLOAD_DIR = '.\gamedata\';
    CS_DOWNLOAD_CLIENT = 'GalaxyHopesClient.exe.zip';
    CS_FILE_REPAIR = '.\repair';
    CS_FILE_GAME = CS_DOWNLOAD_DIR + 'galaxyhopes.exe';
  private
    function GetCount(): Integer;
    function GetDownloadedSize(): String;
    function GetTotalSize(): String;
    procedure DoReadInfo();
    procedure DoDownloadFinish();
    procedure DoDownloadFile(const ASource, ADestination: String; AStep: TStep);
    procedure DoDownloadComplete(ASender: TObject);
    procedure DoCheckVersion();
    procedure DoCheckClient();
    procedure DoCheckDownloadNextFile();
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Start();
  public
    property News: String
             read FLastNews;
    property Client: String
             read FLastClient;
    property Loader: String
             read FLastLoader;
    property Step: TStep
             read FStep;
    property Index: Integer
             read FListIndex;
    property Count: Integer
             read GetCount;
    property FileName: String
             read FListFileName;
    property DownloadedSize: String
             read GetDownloadedSize;
    property TotalSize: String
             read GetTotalSize;
    property OnVersionChanged: TNotifyEvent
             read FOnVersionChanged
             write FOnVersionChanged;
    property OnError: TUpdateError
             read FOnError
             write FOnError;
    property OnFinish: TNotifyEvent
             read FOnFinish
             write FOnFinish;
  end;

implementation

{ TUpdater }

constructor TUpdater.Create();
begin
  if not ForceDirectories(CS_DOWNLOAD_DIR) then
    TConsts.ShowException('Constructor updater ForceDirectories', CS_DOWNLOAD_DIR);
  FIniFile := TIniFile.Create(CS_DOWNLOAD_DIR + TConsts.CS_FILE_VERSION);
  FFileList := TStringList.Create();
  FDownloader := TDownloader.Create();
  FDownloader.OnComplete := DoDownloadComplete;
end;

destructor TUpdater.Destroy();
begin
  FreeAndNil(FDownloader);
  FreeAndNil(FFileList);
  FreeAndNil(FIniFile);
end;

function TUpdater.GetCount(): Integer;
begin
  Result := FFileList.Count;
end;

function TUpdater.GetDownloadedSize(): String;
begin
  Result := FDownloader.SizeToBytes(FDownloader.DownloadedSize);
end;

function TUpdater.GetTotalSize(): String;
begin
  Result := FDownloader.SizeToBytes(FDownloader.FileSize);
end;

procedure TUpdater.DoCheckClient();
var
  TmpFileName: String;
begin
  TmpFileName := '.\' + ExtractFileName(ParamStr(0)) + '.backup';
  if ((FileExists(TmpFileName) and not DeleteFile(TmpFileName))
    or (not RenameFile(ParamStr(0), TmpFileName)))
  then
    TConsts.ShowException('DoCheckClient', TmpFileName);
  TConsts.UnZipFile(FDownloader.FilePath);
  ShellExecute(0, 'open', PWideChar(ParamStr(0)), nil, nil, SW_SHOW);
  Halt(0);
end;

procedure TUpdater.DoCheckDownloadNextFile();
begin
  if (FListIndex = FFileList.Count) then
  begin
    DoDownloadFinish();
    Exit();
  end;
  FListFileName := FFileList.Names[FListIndex];
  if (TConsts.FileHashSHA1(CS_DOWNLOAD_DIR + FListFileName) <> FFileList.ValueFromIndex[FListIndex]) then
    DoDownloadFile(IncludeTrailingPathDelimiter(FLastClient) + FListFileName + TConsts.CS_ZIP_EXT,
      CS_DOWNLOAD_DIR + FListFileName + TConsts.CS_ZIP_EXT, stDownload)
  else
  begin
    Inc(FListIndex);
    DoCheckDownloadNextFile();
  end;
end;

procedure TUpdater.DoCheckVersion();
var
  TmpClient: String;
begin
  TmpClient := FLastClient;
  DoReadInfo();
  {$IFNDEF DEBUG}
  if (FLastLoader <> TConsts.Version()) then
    DoDownloadFile(CS_DOWNLOAD_CLIENT, TConsts.CS_FILE_ROOT, stClient)
  else
  {$ENDIF}
  if ((FLastClient <> TmpClient)
    or (not FileExists(CS_FILE_REPAIR)))
  then
    DoDownloadFile(IncludeTrailingPathDelimiter(FLastClient) + TConsts.CS_FILE_VERSION,
      CS_DOWNLOAD_DIR + TConsts.CS_FILE_VERSION, stFiles)
  else
    DoDownloadFinish();
end;

procedure TUpdater.DoDownloadComplete(ASender: TObject);
begin
  // Проверим на наличие ошибки
  if (FDownloader.Status = udsError) then
  begin
    if (Assigned(FOnError)) then
      FOnError(FDownloader.ErrorCode, FDownloader.URL);
    Exit();
  end;
  // И продолжим обновление
  case FStep of
    stVersion:
      DoCheckVersion();
    stClient:
      DoCheckClient();
    stFiles:
    begin
      if (FileExists(CS_FILE_REPAIR) and not DeleteFile(CS_FILE_REPAIR)) then
        TConsts.ShowException('DoCheckVersion', CS_FILE_REPAIR);
      FIniFile.ReadSectionValues('FILES', FFileList);
      DoCheckDownloadNextFile();
      if (Assigned(FOnVersionChanged)) then
        FOnVersionChanged(Self);
    end;
    stDownload:
    begin
      TConsts.UnZipFile(FDownloader.FilePath);
      if (FListIndex < FFileList.Count - 1) then
      begin
        Inc(FListIndex);
        DoCheckDownloadNextFile();
      end else
        DoDownloadFinish();
    end;
  end;
end;

procedure TUpdater.DoDownloadFile(const ASource, ADestination: String; AStep: TStep);
begin
  FStep := AStep;
  FDownloader.URL := CS_URL + ASource;
  FDownloader.FilePath := ADestination;
  FDownloader.StartDownload();
end;

procedure TUpdater.DoDownloadFinish();
begin
  CloseHandle(FileCreate(CS_FILE_REPAIR));
  ShellExecute(0, 'open', CS_FILE_GAME, nil, nil, SW_SHOW);
  if (Assigned(FOnFinish)) then
    FOnFinish(Self);
end;

procedure TUpdater.DoReadInfo();
begin
  FLastClient := FIniFile.ReadString(TConsts.CS_SECTION_INFO, TConsts.CS_VALUE_CLIENT, FLastClient);
  FLastLoader := FIniFile.ReadString(TConsts.CS_SECTION_INFO, TConsts.CS_VALUE_LOADER, FLastLoader);
  FLastNews := FIniFile.ReadString(TConsts.CS_SECTION_INFO, TConsts.CS_VALUE_NEWS, FLastNews);
  FLastNews := StringReplace(FLastNews, '/r/n', #13#10, [rfReplaceAll]);

  if (Assigned(FOnVersionChanged)) then
    FOnVersionChanged(Self);
end;

procedure TUpdater.Start();
begin
  DoReadInfo();
  DoDownloadFile(TConsts.CS_FILE_VERSION, CS_DOWNLOAD_DIR, stVersion);
end;

end.
