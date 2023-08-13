unit ufmMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Imaging.pngimage,
  Vcl.Buttons,

  Logics.Updater;

type
  TfmMain = class(TForm)
    imBackground: TImage;
    pbDefault: TProgressBar;
    lbNews: TLabel;
    lbProgress: TLabel;
    Timer: TTimer;
    lbVersion: TLabel;
    btnRepeat: TSpeedButton;
    btnClose: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(ASender: TObject);
    procedure TimerTimer(ASender: TObject);
    procedure btnRepeatClick(ASender: TObject);
    procedure btnCloseClick(ASender: TObject);
  private var
    FUpdater: TUpdater;
  private
    procedure WMNCHITTEST(var AMessage : TMessage); message  WM_NCHITTEST;
    procedure OnVersionChanged(ASender: TObject);
    procedure OnError(AErrorCode: Integer; const AUrl: String);
    procedure OnFinish(ASender: TObject);
    procedure DoDownloadStart();
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FUpdater := TUpdater.Create();
  FUpdater.OnVersionChanged := OnVersionChanged;
  FUpdater.OnError := OnError;
  FUpdater.OnFinish := OnFinish;
  DoDownloadStart();
end;

procedure TfmMain.FormPaint(ASender: TObject);
begin
  Canvas.Draw(0, 0, imBackground.Picture.Graphic);
end;

procedure TfmMain.WMNCHITTEST(var AMessage: TMessage);
var
  TmpPos: TPoint;
begin
  if (not GetCursorPos(TmpPos)) then
    Exit();
  if (TmpPos.Y - Top < btnClose.Top) then
  begin
    AMessage.Result := HTCAPTION;
    Cursor := crSize;
  end
  else
  begin
    AMessage.Result := HTCLIENT;
    Cursor := crDefault;
  end;
end;

procedure TfmMain.OnVersionChanged(ASender: TObject);
begin
  lbNews.Caption := FUpdater.News;
  if (FUpdater.Client <> EmptyStr) then
    lbVersion.Caption := 'Клиент v.' + FUpdater.Client + ' Загрузчик v.' + FUpdater.Loader
  else
    lbVersion.Caption := 'Поиск сервера...';
  if (FUpdater.Count > 0) then
  begin
    pbDefault.Style := pbstNormal;
    pbDefault.Max := FUpdater.Count;
  end;
  Timer.Enabled := True;
end;

procedure TfmMain.DoDownloadStart();
begin
  btnRepeat.Visible := False;
  pbDefault.Visible := True;
  pbDefault.Style := pbstMarquee;
  Timer.Enabled := True;
  FUpdater.Start();
end;

procedure TfmMain.OnError(AErrorCode: Integer; const AUrl: String);
begin
  pbDefault.Visible := False;
  Timer.Enabled := False;
  btnRepeat.Visible := True;
  lbProgress.Caption := Format('Ошибка загрузки %d для файла %s', [AErrorCode, AURL]);
end;

procedure TfmMain.OnFinish(ASender: TObject);
begin
  Close();
end;

procedure TfmMain.TimerTimer(ASender: TObject);
begin
  case FUpdater.Step of
    stVersion:
      lbProgress.Caption := 'Подключение...';
    stClient:
      lbProgress.Caption := 'Обновление клиента';
    stDownload:
    begin
      pbDefault.Position := FUpdater.Index + 1;
      lbProgress.Caption := Format('Файл: %d из %d , %s/%s'#10#13'%s', [
        pbDefault.Position,
        FUpdater.Count,
        FUpdater.DownloadedSize,
        FUpdater.TotalSize,
        FUpdater.FileName
      ]);
    end;
  end;
end;

procedure TfmMain.btnCloseClick(ASender: TObject);
begin
  Close();
end;

procedure TfmMain.btnRepeatClick(ASender: TObject);
begin
  DoDownloadStart();
end;

end.
