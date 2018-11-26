program BakaFileTool;

uses
  Vcl.Forms,
  frmBakaFileTool in 'frmBakaFileTool.pas' {BakaWindow},
  wbBethesdaGame in 'wbBethesdaGame.pas',
  wbBSArchive in 'wbBSArchive.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TBakaWindow, BakaWindow);
  Application.Run;
end.
