program GalaxyHopesClient;

uses
  Winapi.Windows,
  Vcl.Forms,
  ufmMain in 'Views\ufmMain.pas' {fmMain},
  Logics.CmdParams in '..\Logics\Logics.CmdParams.pas',
  Logics.Downloader in '..\Logics\Logics.Downloader.pas',
  Logics.GenerateClient in '..\Logics\Logics.GenerateClient.pas',
  Logics.Consts in '..\Logics\Logics.Consts.pas',
  Logics.GenerateLoader in '..\Logics\Logics.GenerateLoader.pas',
  Logics.Updater in '..\Logics\Logics.Updater.pas';

{$SETPEFlAGS $0001 or $0020}
{$IFNDEF DEBUG}
  {$WEAKLINKRTTI ON}
{$ENDIF}
{$R *.res}

begin
  if (TLogicsCmdParam.CmdClient) then
  begin
    AllocConsole();
    TGenerateClient.Generate(TLogicsCmdParam.CmdValue);
    Halt(0);
  end else
  if (TLogicsCmdParam.CmdLoader) then
  begin
    AllocConsole();
    TGenerateLoader.Generate();
    Halt(0);
  end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Galaxy Hopes';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
