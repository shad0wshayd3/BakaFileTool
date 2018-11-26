unit wbBethesdaGame;

interface

uses
    Registry,
    System.IOUtils,
    System.SysUtils,
    Winapi.Windows;

const
    sDefaultRegKey     = '\SOFTWARE\Bethesda Softworks\';
    sDefaultRegKey64   = '\SOFTWARE\WOW6432Node\Bethesda Softworks\';
    sAlternateRegKey   = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\';
    sAlternateRegKey64 = '\SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\';

type
    TBethesdaGame = Class(TObject)
        private
            procedure GetDataFolderPath;

        public
            Name,
            ShortName,
            ConfigFileName,
            ConfigFolderName,
            ConfigPath,
            DataFileName,
            DataFilePath,
            DataFolderName,
            DataFolderPath,
            RootFolderPath,
            AppDataName,
            AppDataPath,
            RegKeyName: string;

            isAlternativeRegPath,
            isValidPath: boolean;

            procedure SetAllNames(const Name: string);
            procedure Initialize;

            constructor Create(const ShortName: string; const Name: string; const autoInit: boolean = False);
    End;

implementation

constructor TBethesdaGame.Create(const ShortName: string; const Name: string; const autoInit: boolean);
begin
    self.ShortName   := ShortName;
    self.Name        := Name;

    ConfigFileName   := Name;
    ConfigFolderName := Name;
    ConfigPath       := TPath.Combine(TPath.GetDocumentsPath(), 'My Games');

    DataFolderName   := 'Data';
    DataFileName     := Name;

    AppDataName      := Name;
    AppDataPath      := TPath.GetCachePath();

    RegKeyName       := Name;

    isAlternativeRegPath := False;
    if autoInit then Initialize;
end;

procedure TBethesdaGame.SetAllNames(const Name: string);
begin
    ConfigFileName   := Name;
    ConfigFolderName := Name;
    DataFileName     := Name;
    AppDataName      := Name;
    RegKeyName       := Name;
end;

procedure TBethesdaGame.GetDataFolderPath;
var
    RegPath, RegPath64, RegPathKey: string;

begin
    if not isAlternativeRegPath then begin
        RegPath    := TPath.Combine(sDefaultRegKey, RegKeyName);
        RegPath64  := TPath.Combine(sDefaultRegKey64, RegKeyName);
        RegPathKey := 'Installed Path';
    end else begin
        RegPath    := TPath.Combine(sAlternateRegKey, RegKeyName);
        RegPath64  := TPath.Combine(sAlternateRegKey64, RegKeyName);
        RegPathKey := 'Path';
    end;

    with TRegistry.Create do try
        RootKey := HKEY_LOCAL_MACHINE;

        if not OpenKeyReadOnly(RegPath64) then
            if not OpenKeyReadOnly(RegPath) then begin
                Exit;
            end;

        DataFolderPath := StringReplace(ReadString(RegPathKey), '"', '', [rfReplaceAll]);
        if (DataFolderPath = '') then begin
            Exit;
        end;
    finally
        Free;
    end;
end;

procedure TBethesdaGame.Initialize;
begin
    RegKeyName := RegKeyName + '\';
    GetDataFolderPath;

    RootFolderPath := DataFolderPath;
    DataFolderPath := TPath.Combine(DataFolderPath, DataFolderName);

    DataFileName := DataFileName + '.esm';
    DataFilePath := TPath.Combine(DataFolderPath, DataFileName);

    isValidPath := FileExists(DataFilePath);

    ConfigFileName := ConfigFileName + '.ini';
    ConfigFolderName := TPath.Combine(ConfigPath, ConfigFolderName);
    ConfigPath := TPath.Combine(ConfigFolderName, ConfigFileName);

    AppDataPath := TPath.Combine(AppDataPath, AppDataName);
end;

end.
