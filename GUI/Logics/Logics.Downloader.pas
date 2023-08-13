unit Logics.Downloader;

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  Winapi.ActiveX,
  WinApi.Windows,
  WinApi.WinInet;

type
  TDownloaderStatus = (
    udsNone,
    udsDownloading,
    udsStopping,
    udsStopped,
    udsError,
    udsDownloaded
  );

  TDownloaderErrorType = (
    udetNoError,
    udetNoDiskSpace,
    udetDownloadOrConnection
  );

  TDownloaderThread = class(TThread)
  const
    // Значение User-agent, передаваемое в HTTP-заголовке
    CS_HTTP_USER_AGENT = 'Galaxy Hopes Updater';
    // Размер буфера загрузки
    CS_BUFFER_SIZE = 8 * 1024;
  private var
    // Хэндл подключения
    FInetHandle: HINTERNET;
    // Хэндл открытого URL
    FURLHandle: HINTERNET;
    // URL
    FUrl: String;
    // Путь к файлу
    FFilePath: String;
    // Расширение файла
    FFileExtension: String;
    // Полный размер загружаемого файла
    FFileSize: Int64;
    // Загружено байт
    FDownloadedSize: Int64;
    // Статус загрузки
    FStatus: TDownloaderStatus;
    // Флаг удаления файла при ошибке
    FRemoveFile: Boolean;
    // Тип ошибки
    FErrorType: TDownloaderErrorType;
    // Код ошибки
    FErrorCode: Cardinal;
    // Событие окончания загрузки файла
    FOnComplete: TNotifyEvent;
  private
    // Очистка внутренних переменных
    procedure Clear();
    // Запуск загрузки
    procedure StartDownload();
    // Инициализация подключения
    function InternetInit(): Boolean;
    // Открытие URL
    function OpenURL(): Boolean;
    // Приведение URL к требуемому для OpenURL виду
    function CanonicalizeURL(): Boolean;
    // Создание/определение пути к файлу
    function CreateFilePath(): Boolean;
    // Запрос размера файла с сервера
    function RequestFileSize(): Boolean;
    // Проверка свободного места на диске для загрузки файла
    function CheckDiskSpace(): Boolean;
    // Установка ссылки загрузки
    procedure SetUrl(const AValue: String);
    // Установка пути сохранения
    procedure SetFilePath(const AValue: String);
    // Установка расширения файла
    procedure SetFileExtension(const AValue: String);
    // Признак удаления файла после завершения операций
    procedure SetRemoveFile(const AValue: Boolean);
    // Получение процента прогресса загрузки файла
    function GetPercentDone(): Integer;
    // Получение состояния загрузки
    function GetStatus(): TDownloaderStatus;
    // Установка состояния загрузки
    procedure SetStatus(const AValue: TDownloaderStatus);
  public
    // Конструктор потока загрузчика
    constructor Create();
    // Запуск потока загрузчика
    procedure Execute(); override;
  public
    // Загружаемый URL
    property Url: String read FURL write SetUrl;
    // Путь к файлу, в которой будет произведена загрузка
    property FilePath: String read FFilePath write SetFilePath;
    // Расширение файла
    property FileExtension: String read FFileExtension write SetFileExtension;
    // % завершения загрузки
    property PercentDone: Integer read GetPercentDone;
    // Статус загрузки
    property Status: TDownloaderStatus read GetStatus write SetStatus;
    // Полный размер загружаемого файла
    property FileSize: Int64 read FFileSize;
    // Загружено байт
    property DownloadedSize: Int64 read FDownloadedSize;
    // Флаг удаления файла при ошибке
    property RemoveFile: Boolean read FRemoveFile write SetRemoveFile;
    // Тип ошибки
    property ErrorType: TDownloaderErrorType read FErrorType;
    // Код ошибки
    property ErrorCode: Cardinal read FErrorCode;
    // Событие окончания загрузки файла
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

  TDownloader = class(TObject)
  private var
    FDownloadThread: TDownloaderThread;
    FUrl: String;
    FFilePath: String;
    FFileExtension: String;
    FFileSize: Int64;
    FDownloadedSize: Int64;
    FPercentDone: Integer;
    FStatus: TDownloaderStatus;
    FRemoveFile: Boolean;
    FErrorType: TDownloaderErrorType;
    FErrorCode: Cardinal;
    FOnComplete: TNotifyEvent;
  private
    function GetUrl(): String;
    procedure SetUrl(const AValue: String);
    function GetFilePath(): String;
    procedure SetFilePath(const AValue: String);
    function GetFileExtension(): String;
    procedure SetFileExtension(const AValue: String);
    function GetPercentDone(): Integer;
    function GetStatus(): TDownloaderStatus;
    function GetFileSize(): Int64;
    function GetRemoveFile(): Boolean;
    procedure SetRemoveFile(const AValue: Boolean);
    function GetErrorType(): TDownloaderErrorType;
    function GetErrorCode(): Cardinal;
    function GetDownloadedSize: Int64;
    procedure GetThreadProps();
    procedure SetThreadProps();
    procedure SetComplete(ASender: TObject);
    procedure SetSynchronize();
  public
    // Конструктор класса загрузчика.
    constructor Create();
    // Деструктор класса загрузчика.
    destructor Destroy(); override;
    // Actions
    // Запуск загрузки файла.
    procedure StartDownload();
    // Остановка загрузки файла.
    procedure StopDownload();
    // Содержимое файла
    function FileToString(): String;
    // Размер файла
    function SizeToBytes(ASize: Int64): String;
  public
    // URL загружаемого файла.
    property URL: String read GetUrl write SetUrl;
    // Путь к загружаемому файлу, если данное свойство не устновлено,
    // то путь к файлу генерируется автоматически, во временной папке, имя файла
    // извлекается из URL (если невозможно - генерируется случайное имя).
    property FilePath: String read GetFilePath write SetFilePath;
    // Расширение файла (если указано, то всегда присваивается загружаемому файлу).
    property FileExtension: String read GetFileExtension write SetFileExtension;
    // % завершения загрузки.
    property PercentDone: Integer read GetPercentDone;
    // Статус процесса загрузки.
    property Status: TDownloaderStatus read GetStatus;
    // Размер загружаемого файла (в байтах).
    property FileSize: Int64 read GetFileSize;
    // Загружено (байт).
    property DownloadedSize: Int64 read GetDownloadedSize;
    // Определяет будет ли удален файл при ошибке (по-умолчанию True).
    property RemoveFile: Boolean read GetRemoveFile write SetRemoveFile;
    // Тип ошибки.
    property ErrorType: TDownloaderErrorType read GetErrorType;
    // Код ошибки (системный).
    property ErrorCode: Cardinal read GetErrorCode;
    // Событие окончания загрузки файла
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

implementation

function TDownloaderThread.CanonicalizeURL(): Boolean;
var
  TmpBuff: String;
  TmpSize: DWORD;
begin
  Result := False;
  if Terminated then
    Exit();
  if (Trim(FUrl) = EmptyStr) then
    Exit();
  TmpSize := INTERNET_MAX_URL_LENGTH;
  SetLength(TmpBuff, TmpSize);
  if not InternetCanonicalizeUrl(PChar(FUrl), PChar(TmpBuff), TmpSize,
    ICU_BROWSER_MODE) then
  begin
    FErrorCode := GetLastError();
    Exit();
  end;
  FUrl := Copy(TmpBuff, 1, TmpSize);
  Result := Trim(FUrl) <> EmptyStr;
end;

function TDownloaderThread.CheckDiskSpace(): Boolean;
var
  TmpFreeBytesAvailable: Int64;
  TmpVal: Int64;
begin
  Result := False;
  if Terminated then
    Exit();
  if not System.SysUtils.GetDiskFreeSpaceEx(PChar(ExtractFileDir(FFilePath)),
    TmpFreeBytesAvailable, TmpVal, nil) then
  begin
    FErrorCode := GetLastError();
    Exit();
  end;
  Result := (FFileSize > 0)
    and (FFileSize < TmpFreeBytesAvailable);
end;

procedure TDownloaderThread.Clear();
begin
  FFileSize := 0;
  FDownloadedSize := 0;
  FErrorType := udetNoError;
  FErrorCode := 0;
  FInetHandle := nil;
  FURLHandle := nil;
end;

constructor TDownloaderThread.Create();
begin
  inherited Create(True);
  FFilePath := EmptyStr;
  FFileExtension := EmptyStr;
  FUrl := EmptyStr;
  FRemoveFile := True;
  FStatus := udsNone;
  Clear();
end;

function TDownloaderThread.CreateFilePath(): Boolean;
var
  TmpPath: String;
  TmpFilePath: String;
  TmpExt: String;
  TmpRes: DWORD;
  TmpFileName: String;
  TmpURLComponents: URL_COMPONENTS;
  TmpI: Integer;
begin
  Result := False;
  if Terminated then
    Exit();

  TmpPath := ExtractFileDir(FFilePath);
  if (TmpPath = EmptyStr) then
  begin
    // Detecting temporary directory
    SetLength(TmpPath, MAX_PATH + 1);
    TmpRes := GetTempPath(MAX_PATH, PChar(TmpPath));
    if TmpRes = 0 then
    begin
      FErrorCode := GetLastError();
      Exit();
    end;
    SetLength(TmpPath, TmpRes);
  end else
    TmpPath := IncludeTrailingPathDelimiter(TmpPath);
  // Создание директории, если ее не существует
  if (not DirectoryExists(TmpPath)) then
    ForceDirectories(TmpPath);

  TmpFileName := ExtractFileName(FFilePath);
  if (TmpFileName = EmptyStr) then
  begin
    // Setting max sizes of url components structure
    with TmpURLComponents do
    begin
      dwSchemeLength := INTERNET_MAX_SCHEME_LENGTH;
      lpszScheme := PChar(StringOfChar(#0, dwSchemeLength + 1));
      dwHostNameLength := INTERNET_MAX_HOST_NAME_LENGTH;
      lpszHostName := PChar(StringOfChar(#0, dwHostNameLength + 1));
      dwUserNameLength := INTERNET_MAX_USER_NAME_LENGTH;
      lpszUserName := PChar(StringOfChar(#0, dwUserNameLength + 1));
      dwPasswordLength := INTERNET_MAX_PASSWORD_LENGTH;
      lpszPassword := PChar(StringOfChar(#0, dwPasswordLength + 1));
      dwUrlPathLength := INTERNET_MAX_PATH_LENGTH;
      lpszUrlPath := PChar(StringOfChar(#0, dwUrlPathLength + 1));
      dwExtraInfoLength := INTERNET_MAX_PATH_LENGTH;
      lpszExtraInfo := PChar(StringOfChar(#0, dwExtraInfoLength + 1));
      dwStructSize := SizeOf(TmpURLComponents);
    end;
    // Cracking URL
    if InternetCrackUrl(PChar(FUrl), Length(FUrl), ICU_DECODE, TmpURLComponents)
    then
    begin
      TmpFileName := Copy(TmpURLComponents.lpszUrlPath, 1,
        TmpURLComponents.dwUrlPathLength);
      if (TmpFileName <> EmptyStr) then
      begin
        TmpI := LastDelimiter('/', TmpFileName);
        if TmpI > 0 then
          TmpFileName := Copy(TmpFileName, TmpI + 1, MaxInt);
        // File name contains incorrect symbols
        if LastDelimiter('\?|><:*"', TmpFileName) > 0 then
          TmpFileName := EmptyStr;
      end;
    end
    else
      FErrorCode := GetLastError();
  end;

  if (TmpFileName <> EmptyStr) then
  begin
    // Checking and modyfying file name if file already exists
    if (FFileExtension <> EmptyStr) then
    begin
      TmpExt := FFileExtension;
      if Pos(TmpExt, '.') = 0 then
        TmpExt := '.' + TmpExt;
    end
    else
      TmpExt := ExtractFileExt(TmpFileName);
    TmpFileName := Copy(TmpFileName, 1, Length(TmpFileName) - Length(TmpExt));
    TmpFilePath := TmpPath + TmpFileName + TmpExt;
  end
  else
  begin
    // Making temp file path
    SetLength(TmpFilePath, MAX_PATH + 1);
    if GetTempFileName(PChar(TmpPath), '', 0, PChar(TmpFilePath)) = 0 then
    begin
      FErrorCode := GetLastError();
      Exit();
    end;
    if (FFileExtension <> EmptyStr) then
    begin
      TmpExt := FFileExtension;
      if Pos(TmpExt, '.') = 0 then
        TmpExt := '.' + TmpExt;
      TmpFilePath := ChangeFileExt(TmpFilePath, TmpExt);
    end;
  end;

  FFilePath := TmpFilePath;
  Result := (FFilePath <> EmptyStr);
end;

procedure TDownloaderThread.Execute();
begin
  CoInitialize(nil);
  try
    StartDownload();
  finally
    CoUninitialize();
  end;
  if (Assigned(FOnComplete)) then
    FOnComplete(Self);
end;

procedure TDownloaderThread.StartDownload();
var
  TmpBuff: RawByteString;
  TmpRes: DWORD;
  TmpBuffSize: Integer;
  TmpFileStream: TFileStream;
begin
  FStatus := udsDownloading;
  Clear();
  try
    // Canocalizing URL
    if not CanonicalizeURL() then
      Exit();
    // Initialize WinInet
    if not InternetInit() then
      Exit();
    try
      // Open URL
      if not OpenURL() then
        Exit();
      try
        // Request file size
        if not RequestFileSize() then
          Exit();
        // Detecting temp directory path and creating a temporary file
        // if FilePath property is not set
        if not CreateFilePath() then
          Exit();
        // Checking disk space
        if not CheckDiskSpace then
        begin
          FErrorType := udetNoDiskSpace;
          Exit();
        end;
        // Creating file stream
        try
          TmpFileStream := TFileStream.Create(FFilePath, fmCreate);
        except
          Exit();
        end;
        // Reading file to stream using buffer
        try
          SetLength(TmpBuff, CS_BUFFER_SIZE);
          repeat
            if Terminated then
              Exit();
            if not InternetReadFile(FURLHandle, Pointer(TmpBuff),
              CS_BUFFER_SIZE, TmpRes)
            then
              Exit();
            TmpBuffSize := TmpRes;
            // Writing to stream and checking that writed data size equals data size
            if TmpFileStream.Write(Pointer(TmpBuff)^, TmpBuffSize) <> TmpBuffSize
            then
              Exit();
            Inc(FDownloadedSize, TmpBuffSize);
          until TmpBuffSize = 0;
          if (FDownloadedSize = FFileSize) then
            FStatus := udsDownloaded;
        finally
          FreeAndNil(TmpFileStream);
        end;
      finally
        InternetCloseHandle(FURLHandle);
      end;
    finally
      InternetCloseHandle(FInetHandle);
    end;
  finally
    if (FStatus <> udsDownloaded) then
    begin
      // Deleting file
      if FRemoveFile
        and FileExists(FFilePath)
      then
        DeleteFile(PChar(FFilePath));
      // Setting status and error type
      if Terminated then
        FStatus := udsStopped
      else
      begin
        FStatus := udsError;
        if FErrorType = udetNoError then
          FErrorType := udetDownloadOrConnection;
      end;
    end;
  end;
end;

function TDownloaderThread.GetPercentDone(): Integer;
begin
  Result := 0;
  if (FFileSize <= 0)
    or (FDownloadedSize > FFileSize)
  then
    Exit();
  Result := Trunc((FDownloadedSize * 100) / FFileSize);
end;

function TDownloaderThread.GetStatus(): TDownloaderStatus;
begin
  if Terminated then
    Result := udsStopping
  else
    Result := FStatus;
end;

function TDownloaderThread.InternetInit(): Boolean;
begin
  Result := False;
  if Terminated then
    Exit();
  FInetHandle := InternetOpen(CS_HTTP_USER_AGENT, INTERNET_OPEN_TYPE_PRECONFIG,
    nil, nil, 0);
  if not Assigned(FInetHandle) then
  begin
    FErrorCode := GetLastError();
    Exit();
  end else
    Result := True;
end;

function TDownloaderThread.OpenURL(): Boolean;
var
  TmpIndex: Cardinal;
  TmpLength: Cardinal;
  TmpBuff: Cardinal;
begin
  Result := False;

  if Terminated then
    Exit();

  FURLHandle := InternetOpenUrl(FInetHandle, PChar(FUrl), nil, 0,
    INTERNET_FLAG_RELOAD
    or INTERNET_FLAG_PRAGMA_NOCACHE
    or INTERNET_FLAG_NO_CACHE_WRITE
    or INTERNET_FLAG_NO_COOKIES,
    0);

  if Assigned(FUrlHandle) then
  begin
    TmpIndex := 0;
    TmpLength := SizeOf(TmpBuff);
    if HttpQueryInfo(FUrlHandle, HTTP_QUERY_FLAG_NUMBER
      or HTTP_QUERY_STATUS_CODE, @TmpBuff, TmpLength, TmpIndex)
    then
      Result := TmpBuff = HTTP_STATUS_OK;
  end;

  if (not Result) then
  begin
    FErrorCode := TmpBuff;
    Exit();
  end;
end;

function TDownloaderThread.RequestFileSize(): Boolean;
var
  TmpBuff: String;
  TmpRes: DWORD;
  TmpVal: DWORD;
begin
  Result := False;
  if Terminated then
    Exit();
  FFileSize := 0;
  TmpRes := 20;
  TmpVal := 0;
  SetLength(TmpBuff, TmpRes);
  try
    // Trying to recieve content length (for recieve buffer size if it longer)
    if not HttpQueryInfo(FURLHandle, HTTP_QUERY_CONTENT_LENGTH, PChar(TmpBuff),
      TmpRes, TmpVal) then
    begin
      TmpVal := GetLastError();
      if (TmpVal = ERROR_INSUFFICIENT_BUFFER) then
      begin
        // Set buffer size
        SetLength(TmpBuff, TmpRes);
        // Buffer resized, trying to recieve content size
        if not HttpQueryInfo(FURLHandle, HTTP_QUERY_CONTENT_LENGTH,
          PChar(TmpBuff), TmpRes, TmpVal) then
        begin
          FErrorCode := GetLastError();
          Exit();
        end;
      end
      else
      begin
        FErrorCode := TmpVal;
        Exit();
      end;
    end;
    // Converting recieved String with file size to Int64 value
    FFileSize := StrToInt64Def(TmpBuff, -1);
  finally
    Result := FFileSize > 0;
  end;
end;

procedure TDownloaderThread.SetFileExtension(const AValue: String);
begin
  if GetStatus in [udsNone, udsStopped, udsError, udsDownloaded] then
    FFileExtension := AValue;
end;

procedure TDownloaderThread.SetFilePath(const AValue: String);
begin
  if GetStatus in [udsNone, udsStopped, udsError, udsDownloaded] then
    FFilePath := AValue;
end;

procedure TDownloaderThread.SetRemoveFile(const AValue: Boolean);
begin
  if GetStatus in [udsNone, udsStopped, udsError, udsDownloaded] then
    FRemoveFile := AValue;
end;

procedure TDownloaderThread.SetUrl(const AValue: String);
begin
  if GetStatus in [udsNone, udsStopped, udsError, udsDownloaded] then
    FUrl := AValue;
end;

{$ENDREGION}

{$REGION 'TATUpdaterDownloader'}

constructor TDownloader.Create();
begin
  inherited;
  FFilePath := EmptyStr;
  FFileExtension := EmptyStr;
  FUrl := EmptyStr;
  FRemoveFile := True;
  FStatus := udsNone;
  FFileSize := 0;
  FDownloadedSize := 0;
  FErrorType := udetNoError;
  FErrorCode := 0;
end;

destructor TDownloader.Destroy();
begin
  StopDownload();
  inherited;
end;

function TDownloader.GetDownloadedSize(): Int64;
begin
  if Assigned(FDownloadThread) then
    FDownloadedSize := FDownloadThread.DownloadedSize;
  Result := FDownloadedSize;
end;

function TDownloader.GetErrorCode(): Cardinal;
begin
  if Assigned(FDownloadThread) then
    FErrorCode := FDownloadThread.ErrorCode;
  Result := FErrorCode;
end;

function TDownloader.GetErrorType(): TDownloaderErrorType;
begin
  if Assigned(FDownloadThread) then
    FErrorType := FDownloadThread.ErrorType;
  Result := FErrorType;
end;

function TDownloader.GetFileExtension(): String;
begin
  if Assigned(FDownloadThread) then
    FFileExtension := FDownloadThread.FileExtension;
  Result := FFileExtension;
end;

function TDownloader.GetFilePath(): String;
begin
  if Assigned(FDownloadThread) then
    FFilePath := FDownloadThread.FilePath;
  Result := FFilePath;
end;

function TDownloader.GetFileSize(): Int64;
begin
  if Assigned(FDownloadThread) then
    FFileSize := FDownloadThread.FileSize;
  Result := FFileSize;
end;

function TDownloader.GetPercentDone(): Integer;
begin
  if Assigned(FDownloadThread) then
    FPercentDone := FDownloadThread.PercentDone;
  Result := FPercentDone;
end;

function TDownloader.GetRemoveFile(): Boolean;
begin
  if Assigned(FDownloadThread) then
    FRemoveFile := FDownloadThread.RemoveFile;
  Result := FRemoveFile;
end;

function TDownloader.GetStatus(): TDownloaderStatus;
begin
  if Assigned(FDownloadThread) then
    FStatus := FDownloadThread.Status;
  Result := FStatus;
end;

procedure TDownloaderThread.SetStatus(
  const AValue: TDownloaderStatus);
begin
  FStatus := AValue;
end;

procedure TDownloader.GetThreadProps();
begin
  if Assigned(FDownloadThread) then
  begin
    FUrl := FDownloadThread.Url;
    FFilePath := FDownloadThread.FilePath;
    FFileExtension := FDownloadThread.FileExtension;
    FFileSize := FDownloadThread.FileSize;
    FDownloadedSize := FDownloadThread.DownloadedSize;
    FStatus := FDownloadThread.Status;
    FRemoveFile := FDownloadThread.RemoveFile;
    FErrorType := FDownloadThread.ErrorType;
    FErrorCode := FDownloadThread.ErrorCode;
  end;
end;

function TDownloader.GetUrl(): String;
begin
  if Assigned(FDownloadThread) then
    FUrl := FDownloadThread.Url;
  Result := FUrl;
end;

procedure TDownloader.SetSynchronize();
begin
  if (Assigned(OnComplete)) then
    OnComplete(Self);
end;

procedure TDownloader.SetComplete(ASender: TObject);
begin
  GetThreadProps();
  if (Assigned(FDownloadThread)) then
    FDownloadThread.Synchronize(SetSynchronize);
end;

procedure TDownloader.SetFileExtension(const AValue: String);
begin
  FFileExtension := AValue;
end;

procedure TDownloader.SetFilePath(const AValue: String);
begin
  FFilePath := AValue;
end;

procedure TDownloader.SetRemoveFile(const AValue: Boolean);
begin
  FRemoveFile := AValue;
end;

procedure TDownloader.SetThreadProps();
begin
  if Assigned(FDownloadThread) then
  begin
    FDownloadThread.Url := FUrl;
    FDownloadThread.FilePath := FFilePath;
    FDownloadThread.FileExtension := FFileExtension;
    FDownloadThread.RemoveFile := FRemoveFile;
    FDownloadThread.Status := FStatus;
    FDownloadThread.OnComplete := SetComplete;
  end;
end;

procedure TDownloader.SetUrl(const AValue: String);
begin
  FUrl := AValue;
end;

procedure TDownloader.StartDownload();
begin
  FDownloadThread := TDownloaderThread.Create();
  SetThreadProps();
  FDownloadThread.Start();
end;

procedure TDownloader.StopDownload();
begin
  if Assigned(FDownloadThread) then
  begin
    GetThreadProps();
    FreeAndNil(FDownloadThread);
  end;
  FStatus := udsNone;
end;

function TDownloader.FileToString(): String;
var
  TmpSl: TStringList;
begin
  TmpSl := TStringList.Create();
  try
    TmpSl.LoadFromFile(FFilePath);
    Result := TmpSl.Text;
  finally
    FreeAndNil(TmpSl);
  end;
end;

function TDownloader.SizeToBytes(ASize: Int64): String;
begin
  if (ASize > 1024 * 1024) then
    Result := Format('%d мб', [ASize div (1024 * 1024)])
  else
    Result := Format('%d кб', [ASize div 1024]);
end;

end.
