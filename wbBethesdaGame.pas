unit wbBethesdaGame;

interface

uses
    Registry,
    System.IOUtils,
    System.SysUtils,
    Winapi.Windows;

const
    sBethRegKey       = '\SOFTWARE\Bethesda Softworks\';
    sUninstallRegKey  = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\';
    sSureAIRegKey     = '\Software\SureAI\';

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
            ConfigCustomName,
            ConfigCustomPath,
            DataFileName,
            DataFilePath,
            DataFolderName,
            DataFolderPath,
            RootFolderPath,
            AppDataName,
            AppDataPath,
            RegKeyName,
            RegKeyPath,
            RegKeyKey,
            ExecutableName,
            ExecutablePath: string;

            RegRootKey: Cardinal;

            isValidPath: boolean;

            procedure SetAllNames(const Name: string);
            procedure Initialize;

            constructor Create(const ShortName: string; const Name: string; const autoInit: boolean = False);
    End;

implementation

constructor TBethesdaGame.Create(const ShortName: string; const Name: string; const autoInit: boolean);
begin
    self.ShortName    := ShortName;
    self.Name         := Name;

    ConfigFileName    := Name;
    ConfigFolderName  := Name;
    ConfigPath        := TPath.Combine(TPath.GetDocumentsPath(), 'My Games');

    DataFolderName    := 'Data';
    DataFileName      := Name;

    AppDataName       := Name;
    AppDataPath       := TPath.GetCachePath();

    RegKeyName        := Name;
    RegKeyPath        := sBethRegKey;
    RegKeyKey         := 'Installed Path';

    RegRootKey        := HKEY_LOCAL_MACHINE;

    if autoInit then Initialize;
end;

procedure TBethesdaGame.SetAllNames(const Name: string);
begin
    ConfigFileName   := Name;
    ConfigFolderName := Name;
    DataFileName     := Name;
    AppDataName      := Name;
    RegKeyName       := Name;
    ExecutableName   := Name;
end;

procedure TBethesdaGame.GetDataFolderPath;
var
    RegPath: string;

begin
    RegPath := TPath.Combine(RegKeyPath, RegKeyName);

    with TRegistry.Create do try
        Access  := KEY_READ or KEY_WOW64_32KEY;
        RootKey := RegRootKey;

        if not OpenKey(RegPath, False) then begin
            Access := KEY_READ or KEY_WOW64_64KEY;
            if not OpenKey(RegPath, False) then begin
                Exit;
            end;
        end;

        DataFolderPath := StringReplace(ReadString(RegKeyKey), '"', '', [rfReplaceAll]);
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

    ExecutableName := ExecutableName + '.exe';
    ExecutablePath := TPath.Combine(RootFolderPath, ExecutableName);

    DataFileName := DataFileName + '.esm';
    DataFilePath := TPath.Combine(DataFolderPath, DataFileName);

    isValidPath := FileExists(DataFilePath);

    ConfigCustomName := ConfigFileName + 'Custom.ini';
    ConfigFileName := ConfigFileName + '.ini';
    ConfigFolderName := TPath.Combine(ConfigPath, ConfigFolderName);
    ConfigCustomPath := TPath.Combine(ConfigFolderName, ConfigCustomName);
    ConfigPath := TPath.Combine(ConfigFolderName, ConfigFileName);

    AppDataPath := TPath.Combine(AppDataPath, AppDataName);
end;

end.
