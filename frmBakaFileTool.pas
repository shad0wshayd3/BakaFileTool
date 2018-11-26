unit frmBakaFileTool;

interface

uses
    FileContainer,
    ShellAPI,
    System.Classes,
    System.Generics.Collections,
    System.Math,
    System.INIFiles,
    System.IOUtils,
    System.StrUtils,
    System.SyncObjs,
    System.SysUtils,
    System.Threading,
    System.Types,
    System.Variants,
    Vcl.ComCtrls,
    Vcl.Controls,
    Vcl.Dialogs,
    Vcl.ExtCtrls,
    Vcl.Forms,
    Vcl.Graphics,
    Vcl.StdCtrls,
    wbBSArchive,
    wbBethesdaGame,
    Winapi.Messages,
    Winapi.Windows;

type
    TArchiveSettings = Record
        AutoCopy    : boolean;
        Texture     : boolean;
        Compress    : boolean;
        BinShare    : boolean;
        MultiThread : boolean;
        UseFlags    : boolean;
        UseArchive2 : boolean;
        ArchFlags   : Cardinal;
        FileFlags   : Cardinal;
    End;

    TBakaGame = Class(TBethesdaGame)
        public
            ArchivePath,
            BSAMainName,
            BSAMainPath,
            BSATextureName,
            BSATexturePath,
            InputDataPath,
            OutputDataPath,
            PluginName: string;

            SelectButton: TButton;
            SelectBox: TEdit;

            PluginFile: TFileContainer;
            ConfigFile: TFileContainer;
            BSAType: TBSArchiveType;
            isBA2: boolean;

            function GetDataPath(const Backslash: boolean = True): string;
            function GetRootPath(const Backslash: boolean = True): string;

            procedure ReInit;

            constructor Create(const shortName: string; const Name: string; const BSAType: TBSArchiveType;
                const isBA2: boolean = False; const autoInit: boolean = False);

    End;

    TBakaThread = Class(TThread)
        private
            lastShownTime, threadStartTime: TDateTime;
            lastShownMessage, Prefix: string;

            procedure ClearProgress;

            procedure AddMessage(const Message: string; const onStatus: boolean = False; const onlyStatus: boolean = False);
            procedure UpdateProgress(const current: integer; const max: integer);
            procedure UpdateUI(const Show, FileList: boolean);

        public
            constructor Create;

    End;

    TBuildFileThread = Class(TBakaThread)
        private
            fileFilter: TStringList;
            procedure UpdateFileList(const NewLine: string);

        protected
            procedure Execute; override;

        public
            constructor Create(const fileFilter: TStringList);

    End;

    TBuildArchiveThread = Class(TBakaThread)
        private
            settings: TArchiveSettings;
            fileList: TStringList;
            thisGame: TBakaGame;

        protected
            procedure Execute; override;

        public
            constructor Create(const thisGame: TBakaGame; const fileList: TStringList; const settings: TArchiveSettings);

    End;

    TArchiveThread = Class(TBakaThread)
        private
            mainList, textList: TStringList;
            settings: TArchiveSettings;
            thisGame: TBakaGame;

        protected
            procedure Execute; override;

        public
            constructor Create(const thisGame: TBakaGame; const settings: TArchiveSettings;
                const mainList: TStringList; const textList: TStringList);

    End;

    TArchiveWrapperThread = Class(TBakaThread)
        private
            FileLines: TStringList;
            Command: string;

        protected
            procedure Execute; override;

        public
            constructor Create(const archivePath: string; const inputFilePath: string);

    End;

    TBakaWindow = Class(TForm)
        private
            ActiveList: TList<TBakaGame>;
            MasterList: TList<TBakaGame>;
            thisGame: TBakaGame;

            isBuildingArchive, isBuildingFileList, hasSounds: boolean;

            archiveList, textureList: TStringList;
            CachedGameIndex: integer;

            BannedFileNamePaths: array of string;
            archivePath: string;

            function AddWrapper(List: TList<TBakaGame>; const shortName: string; const Name: string; const BSAType: TBSArchiveType; const isBA2: boolean = False): TBakaGame;

            procedure GameSelectUpdate(GameIndex: integer; const Force: boolean = False);

            procedure InitWindowSettings;
            procedure LoadSettings;
            procedure SaveSettings;
            procedure UpdatePluginFile;

        public
            procedure AddMessage(const Message: string; const onStatus: boolean = False; const onlyStatus: boolean = False);
            procedure AppendMessage(const Addition: string);

        published
            MetaFrame: TPageControl;

            BlockText01, BlockText02, BlockText03,
            ArchiveFlags, FileFlags: TLabel;

            SettingAdvanced, SettingArchive2, SettingAutoCopy,
            SettingAutoRefresh, SettingAutoScroll, SettingCompress,
            SettingDataManual, SettingMaximized, SettingThreaded,
            SettingSaveLog, SettingShare,
            SettingWarnMissing: TCheckBox;

            AdvancedSettings, ArchivePathBox, ArchiveSettings,
            ButtonBox, BlacklistBox, FO4PathBox, FO4VRPathBox, FO76PathBox,
            GameDataBox, GameModeBox, ProgramSettings, SSEPathBox,
            SSEVRPathBox, TES5PathBox: TGroupBox;

            SheetEditor, SheetLog, SheetPaths,
            SheetSettings: TTabSheet;

            SettingArchiveFlags, SettingArchivePath, SettingFileFlags,
            SettingFO4Path, SettingFO4VRPath, SettingFO76Path,
            SettingSSEPath, SettingSSEVRPath, SettingTES5Path,
            zzFilterInput: TEdit;

            AddFilterButton, ArchiveSelectButton, FO4PathSelectButton,
            FO4VRPathSelectButton, FO76PathSelectButton, RefreshButton,
            RemoveFilterButton, ResetButton, RunButton, SelectButton,
            SSEPathSelectButton, SSEVRPathSelectButton,
            TES5PathSelectButton: TButton;

            Console, GamePath,
            FileListBox: TMemo;

            GameSelect      : TComboBoxEx;
            ProgressBar     : TProgressBar;
            SettingsBlock   : TPanel;
            GamePathBox     : TScrollBox;
            Spacer          : TShape;
            StatusBar       : TStatusBar;
            zzFilterBox     : TListBox;

            BakaFileTES5, BakaFileSSE, BakaFileSSEVR,
            BakaFileFO4, BakaCfgFO4,
            BakaFileFO4VR, BakaCfgFO4VR,
            BakaFileFO76, BakaCfgFO76: TFileContainer;

            procedure AddFilterButtonClick(Sender: TObject);
            procedure ArchiveSelectButtonClick(Sender: TObject);
            procedure FormCreate(Sender: TObject);
            procedure FormDestroy(Sender: TObject);
            procedure FormResize(Sender: TObject);
            procedure GamePathBoxMouseWheel(Sender: TObject; Shift: TShiftState;
                WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
            procedure GamePathBoxResize(Sender: TObject);
            procedure GameSelectChange(Sender: TObject);
            procedure PathButtonClick(Sender: TObject);
            procedure RefreshButtonClick(Sender: TObject);
            procedure RemoveFilterButtonClick(Sender: TObject);
            procedure ResetButtonClick(Sender: TObject);
            procedure RunButtonClick(Sender: TObject);
            procedure SelectButtonClick(Sender: TObject);
            procedure SettingAdvancedClick(Sender: TObject);
            procedure SettingAutoScrollClick(Sender: TObject);
            procedure SettingDataManualClick(Sender: TObject);

    End;

var
    BakaWindow: TBakaWindow;

implementation

{$R *.dfm}

{ Random Stuff }

function HexToInt(s: string): Cardinal;
begin
    if SameText(Copy(s, 1, 2), '0x') then Delete(s, 1, 2);
    Result := StrToInt('$' + s);
end;

function GetFileVersion(const FileName: string): string;
var
    UniqueFileName: string;
    dwHandle, dwLen, lpLen: DWORD;
    lpBuffer: PVSFixedFileInfo;
    lpData: Pointer;

begin
    Result := '';

    UniqueFileName := FileName;
    UniqueString(UniqueFileName);

    dwLen := GetFileVersionInfoSize(PChar(UniqueFileName), dwHandle);

    if (dwLen <> 0) then begin
        GetMem(lpData, dwLen);
        try
            if GetFileVersionInfo(PChar(UniqueFileName), dwHandle, dwLen, lpData) then begin
                if VerQueryValue(lpData, '\', Pointer(lpBuffer), lpLen) then begin
                    Result := Format('%d.%d.%d.%d', [
                        HiWord(lpBuffer.dwFileVersionMS),
                        LoWord(lpBuffer.dwFileVersionMS),
                        HiWord(lpBuffer.dwFileVersionLS),
                        LoWord(lpBuffer.dwFileVersionLS)]);
                end;
            end;
        finally
            FreeMem(lpData);
        end;
    end;
end;

procedure GetDDSFileInfo(aArchive: TwbBSArchive; const aFileName: string; var aInfo: TDDSInfo);
var
    DDSHeader: TDDSHeader;
begin
    with TFileStream.Create(aFileName, fmOpenRead + fmShareDenyNone) do try
        if (Read(DDSHeader, SizeOf(DDSHeader)) <> SizeOf(DDSHeader)) or (DDSHeader.Magic <> 'DDS ') then
            raise Exception.Create('Not a valid DDS file: ' + aFileName);

        aInfo.Width := DDSHeader.dwWidth;
        aInfo.Height := DDSHeader.dwHeight;
        aInfo.MipMaps := DDSHeader.dwMipMapCount;
    finally
        Free;
    end;
end;

procedure DestroyDirectory(const FilePath: string);
var
    ShellOp: TSHFileOpStruct;

begin
    FillChar(ShellOp, SizeOf(ShellOp), 0);
    ShellOp.fFlags := FOF_NOCONFIRMATION or FOF_NOERRORUI or FOF_SILENT;
    ShellOp.pFrom  := PChar(FilePath+#0);
    ShellOp.wFunc  := FO_DELETE;
    SHFileOperation(ShellOp);
end;

{ TBakaGame }

function TBakaGame.GetDataPath(const Backslash: boolean): string;
begin
    if Backslash then begin
        if SameText(Trim(InputDataPath), '') then begin
            Result := IncludeTrailingBackSlash(DataFolderPath);
        end else begin
            Result := IncludeTrailingBackSlash(InputDataPath);
        end;
    end else begin
        if SameText(Trim(InputDataPath), '') then begin
            Result := ExcludeTrailingPathDelimiter(DataFolderPath);
        end else begin
            Result := ExcludeTrailingPathDelimiter(InputDataPath);
        end;
    end;
end;

function TBakaGame.GetRootPath(const Backslash: boolean): string;
begin
    if Backslash then begin
        Result := IncludeTrailingBackSlash(RootFolderPath);
    end else begin
        Result := ExcludeTrailingPathDelimiter(RootFolderPath);
    end;
end;

procedure TBakaGame.ReInit;
begin
    RootFolderPath := DataFolderPath;
    DataFolderPath := TPath.Combine(DataFolderPath, DataFolderName);

    DataFileName := DataFileName;
    DataFilePath := TPath.Combine(DataFolderPath, DataFileName);

    isValidPath := FileExists(DataFilePath);
end;

constructor TBakaGame.Create(const shortName: string; const Name: string; const BSAType: TBSArchiveType; const isBA2: Boolean; const autoInit: boolean);
begin
    inherited Create(shortName, Name, autoInit);

    self.BSAType := BSAType;
    self.isBA2   := isBA2;

    self.OutputDataPath := TPath.Combine('BakaOutput', Name);
    self.InputDataPath := '';

    self.pluginFile := nil;
    self.configFile := nil;

    if self.isBA2 then begin
        self.BSAMainName    := 'BakaFile - Main.ba2';
        self.BSATextureName := 'BakaFile - Textures.ba2';
        self.BSATexturePath := Format('BakaOutput\%s\%s', [self.Name, self.BSATextureName]);

    end else self.BSAMainName := 'BakaFile.bsa';

    self.BSAMainPath := Format('BakaOutput\%s\%s', [self.Name, self.BSAMainName]);
end;

{ TBakaThread }

procedure TBakaThread.ClearProgress;
begin
    Synchronize(nil, procedure
        begin
            BakaWindow.ProgressBar.Position := 0;
        end
    );
end;

procedure TBakaThread.AddMessage(const Message: string; const onStatus: boolean; const onlyStatus: boolean);
var
    currentTime: TDateTime;
    sMessage: string;

begin
    currentTime := Now;
    if (lastShownTime = 0) then
        lastShownTime := currentTime;

    if ((currentTime - lastShownTime) > 1/24/60/60) or (lastShownMessage <> Message) then begin
        sMessage := Format('[%s] %s%s', [FormatDateTime('nn:ss', currentTime - threadStartTime), Prefix, Message]);
        lastShownTime := currentTime; lastShownMessage := Message;

        Synchronize(nil, procedure
        begin
            BakaWindow.AddMessage(sMessage, onStatus, onlyStatus);
        end);
    end;
end;

procedure TBakaThread.UpdateProgress(const current: integer; const max: integer);
var
    Progress: integer;

begin
    Progress := Round((current + 1) / max * 100);
    Synchronize(nil, procedure
        begin
            BakaWindow.ProgressBar.Position := Progress;
        end
    );
end;

procedure TBakaThread.UpdateUI(const Show, FileList: boolean);
begin
    ClearProgress;

    Synchronize(nil, procedure
        begin
            BakaWindow.SettingDataManual.Enabled        := Show;
            BakaWindow.GameSelect.Enabled               := Show;
            BakaWindow.RunButton.Enabled                := Show;
            BakaWindow.RefreshButton.Enabled            := Show;
            BakaWindow.SheetPaths.Enabled               := Show;
            BakaWindow.SheetSettings.Enabled            := Show;

            BakaWindow.ArchiveSelectButton.Enabled      := Show;
            BakaWindow.TES5PathSelectButton.Enabled     := Show;
            BakaWindow.FO4PathSelectButton.Enabled      := Show;
            BakaWindow.SSEPathSelectButton.Enabled      := Show;
            BakaWindow.FO4VRPathSelectButton.Enabled    := Show;
            BakaWindow.SSEVRPathSelectButton.Enabled    := Show;
            BakaWindow.FO76PathSelectButton.Enabled     := Show;

            BakaWindow.SelectButton.Enabled             := Show and BakaWindow.SettingDataManual.Checked;
            BakaWindow.SettingsBlock.Visible            := not Show;

            if FileList then begin
                if not Show then BakaWindow.FileListBox.Clear;
                BakaWindow.FileListBox.Visible := Show;
            end;

            if Show then begin
                if ((BakaWindow.archiveList.Count = 0) and (BakaWindow.textureList.Count = 0)) then begin
                    AddMessage('Error: No valid files were found in the directory!', True);
                    BakaWindow.RunButton.Enabled := False;
                end;
            end;
        end
    );
end;

constructor TBakaThread.Create;
begin
    inherited Create(False);

    threadStartTime := Now;
    Prefix := 'Background Loader: ';
end;

{ TBuildFileThread }

procedure TBuildFileThread.UpdateFileList(const NewLine: string);
begin
    Synchronize(nil, procedure
        begin
            BakaWindow.FileListBox.Lines.Add(NewLine);
        end
    );
end;

procedure TBuildFileThread.Execute;
var
    tempFileList: TStringDynArray;
    fileName, filter: string;
    bFoundInvalid: boolean;
    currentCount: integer;

begin
    try
        UpdateUI(False, True);

        tempFileList := TDirectory.GetFiles(BakaWindow.thisGame.GetDataPath, '*.*', TSearchOption.soAllDirectories);
        BakaWindow.archiveList.Clear; BakaWindow.textureList.Clear; BakaWindow.hasSounds := False;

        bFoundInvalid := False; currentCount := 0;

        for fileName in tempFileList do begin
            Inc(currentCount);

            UpdateProgress(currentCount, Length(tempFileList));
            AddMessage('Building File List...', True);

            if SameText(BakaWindow.thisGame.GetDataPath(False), TPath.GetDirectoryName(fileName)) then Continue;
            for filter in fileFilter do begin
                bFoundInvalid := ContainsText(fileName, filter);
                if bFoundInvalid then Break;
            end;

            if bFoundInvalid then Continue;

            if not BakaWindow.hasSounds then
                BakaWindow.hasSounds := MatchText(TPath.GetExtension(fileName), ['.mp3', '.ogg', '.wav', '.xwm']);

            UpdateFileList(fileName);

            if BakaWindow.thisGame.isBA2 then
                if SameText(TPath.GetExtension(fileName), '.dds') then
                    BakaWindow.textureList.Add(fileName)
                else BakaWindow.archiveList.Add(fileName)
            else BakaWindow.archiveList.Add(fileName);
        end;

        AddMessage('Done.', True);

    finally
        BakaWindow.isBuildingFileList := False;
        UpdateUI(True, True);
        self.Destroy;
    end;
end;

constructor TBuildFileThread.Create(const fileFilter: TStringList);
begin
    inherited Create;
    self.fileFilter := fileFilter;
end;

{ TBuildArchiveThread }

procedure TBuildArchiveThread.Execute;
var
    rootName, fileName: string;
    currentCount, i: integer;
    Archive: TwbBSArchive;
    relList: TStringList;

begin
    rootName := BakaWindow.thisGame.GetDataPath;
    Archive  := TwbBSArchive.Create;
    relList  := TStringList.Create;

    Prefix := 'Background Archiver: ';

    for fileName in fileList do
        relList.Add(Copy(fileName, Succ(Length(rootName)), Length(fileName)));

    try
        if settings.Texture then
            Archive.DDSInfoProc := GetDDSFileInfo;

        if not BakaWindow.hasSounds then
            Archive.Compress := settings.Compress
        else Archive.Compress := False;

        Archive.ShareData := settings.BinShare;

        try
            if not settings.Texture then
                if thisGame.isBA2 then
                    Archive.CreateArchive(thisGame.BSAMainPath, thisGame.BSAType, fileList)
                else Archive.CreateArchive(thisGame.BSAMainPath, thisGame.BSAType, relList)
            else Archive.CreateArchive(thisGame.BSATexturePath, baFO4DDS, fileList)

        except
            on E: Exception do begin
                AddMessage('Error: See Log for details.', True, True);
                AddMessage(Format('Error: Couldn''t create Archive File: %s', [E.Message]));
                Exit;
            end;
        end;

        if thisGame.BSAType in [baTES4, baFO3, baSSE] then begin
            if (settings.UseFlags and (settings.ArchFlags <> 0)) then
                Archive.ArchiveFlags := settings.ArchFlags;

            if (settings.UseFlags and (settings.FileFlags <> 0)) then
                Archive.FileFlags := settings.FileFlags;
        end;

        if settings.MultiThread then
            Archive.Sync := TSimpleRWSync.Create;

        if thisGame.isBA2 then
            if settings.Texture then
                AddMessage('Starting Texture Archive Packing.', True)
            else AddMessage('Starting Main Archive Packing.', True)
        else AddMessage('Starting Archive Packing.', True);

        currentCount := 0;
        if Assigned(Archive.Sync) then begin
            TParallel.&For(0, Pred(fileList.Count), procedure(i: integer)
            begin
                try
                    Archive.AddFile(rootName, fileList[i]);
                except
                    on E: Exception do begin
                        AddMessage('Error: See Log for details.', True, True);
                        AddMessage(Format('Error: Couldn''t pack %s into Archive File: %s', [relList[i], E.Message]));
                        Exit
                    end;
                end;

                Archive.Sync.BeginWrite;
                try
                    Inc(currentCount); UpdateProgress(currentCount, fileList.Count);
                    if thisGame.isBA2 then
                        if settings.Texture then
                            AddMessage('Packing Texture Archive File...', True)
                        else AddMessage('Packing Main Archive File...', True)
                    else AddMessage('Packing Archive File...', True);

                finally
                    Archive.Sync.EndWrite;
                end;
            end)
        end else begin
            for i := 0 to Pred(fileList.Count) do begin
                try
                    Archive.AddFile(rootName, fileList[i]);
                except
                    on E: Exception do begin
                        AddMessage('Error: See Log for details.', True, True);
                        AddMessage(Format('Error: Couldn''t pack %s into Archive File: %s', [relList[i], E.Message]));
                        Exit;
                    end;
                end;

                Inc(currentCount); UpdateProgress(currentCount, fileList.Count);
                if thisGame.isBA2 then
                    if settings.Texture then
                        AddMessage('Packing Texture Archive File...', True)
                    else AddMessage('Packing Main Archive File...', True)
                else AddMessage('Packing Archive File...', True);
            end;
        end;

        try
            Archive.Save;
        except
            on E: Exception do begin
                AddMessage('Error: See Log for details.', True, True);
                AddMessage(Format('Error: Couldn''t save Archive File: %s', [E.Message]));
            end;
        end;

        if thisGame.isBA2 then
            if settings.Texture then
                AddMessage('Texture Archive Packing: Done.', True)
            else AddMessage('Main Archive Packing: Done.', True)
        else AddMessage('Archive Packing: Done.', True);

    finally
        Archive.Free; relList.Free;

    end;
end;

constructor TBuildArchiveThread.Create(const thisGame: TBakaGame; const fileList: TStringList; const settings: TArchiveSettings);
begin
    inherited Create;

    self.thisGame := thisGame;
    self.settings := settings;
    self.fileList := fileList;
end;

{ TArchiveThread }

procedure TArchiveThread.Execute;
var
    mainThread, textThread: TBuildArchiveThread;
    archiveThread: TArchiveWrapperThread;
    textureFile, tempFile: string;
    TempArchiveFile: TextFile;
    manualCopy: boolean;

begin
    manualCopy := False;

    try
        UpdateUI(False, False);

        if DirectoryExists(thisGame.OutputDataPath) then
            DestroyDirectory(thisGame.OutputDataPath);
        Sleep(500); TDirectory.CreateDirectory(thisGame.OutputDataPath);

        if (thisGame.PluginFile <> nil) then
            thisGame.PluginFile.SaveToFile(TPath.Combine(thisGame.OutputDataPath, thisGame.PluginName));

        if (thisGame.ConfigFile <> nil) then
            thisGame.ConfigFile.SaveToFile(TPath.Combine(thisGame.OutputDataPath, 'BakaFile.ini'));

        if (mainList.Count > 0) then begin
            settings.Texture := False;
            mainThread := TBuildArchiveThread.Create(thisGame, mainList, settings);
            mainThread.WaitFor;
            mainThread.Free;
        end;

        if (textList.Count > 0) then begin
            if (settings.UseArchive2 and MatchText(thisGame.ShortName, ['FO76'])) then begin
                tempFile := TPath.Combine(thisGame.OutputDataPath, 'TempArchiveFileList.txt');
                AssignFile(TempArchiveFile, tempFile); ReWrite(TempArchiveFile);

                for textureFile in textList do
                    WriteLn(TempArchiveFile, textureFile);
                CloseFile(TempArchiveFile);

                archiveThread := TArchiveWrapperThread.Create(thisGame.ArchivePath, tempFile);
                archiveThread.WaitFor;
                archiveThread.Free;

            end else begin
                settings.Texture  := True;
                settings.Compress := True;
                textThread := TBuildArchiveThread.Create(thisGame, textList, settings);
                textThread.WaitFor;
                textThread.Free;
            end;
        end;
    finally
        BakaWindow.isBuildingArchive := False;
        BakaWindow.UpdatePluginFile;
        UpdateUI(True, False);

        if FileExists(tempFile) then
            System.SysUtils.DeleteFile(tempFile);

        if Settings.AutoCopy then begin
            if CopyFile(PChar(TPath.Combine(thisGame.OutputDataPath, thisGame.PluginName)), PChar(TPath.Combine(thisGame.DataFolderPath, thisGame.PluginName)), False) then
                AddMessage(Format('Copied %s to Data folder!', [thisGame.PluginName]), True)
            else begin
                AddMessage(Format('Couldn''t copy %s to Data Folder: %s',
                    [thisGame.PluginName, SysErrorMessage(GetLastError)]), True);
                manualCopy := True;
            end;

            if thisGame.configFile <> nil then begin
                if CopyFile(PChar(TPath.Combine(thisGame.OutputDataPath, 'BakaFile.ini')), PChar(TPath.Combine(thisGame.DataFolderPath, 'BakaFile.ini')), False) then
                    AddMessage(Format('Copied %s to Data folder!', ['BakaFile.ini']), True)
                else begin
                    AddMessage(Format('Couldn''t copy %s to Data Folder: %s',
                        ['BakaFile.ini', SysErrorMessage(GetLastError)]), True);
                    manualCopy := True;
                end;
            end;

            if CopyFile(PChar(thisGame.BSAMainPath), PChar(TPath.Combine(thisGame.DataFolderPath, thisGame.BSAMainName)), False) then
                AddMessage(Format('Copied %s to Data folder!', [thisGame.BSAMainName]), True)
            else begin
                AddMessage(Format('Couldn''t copy %s to Data Folder: %s',
                    [thisGame.BSAMainName, SysErrorMessage(GetLastError)]), True);
                manualCopy := True;
            end;

            if (textList.Count > 0) then begin
                if CopyFile(PChar(thisGame.BSATexturePath), PChar(TPath.Combine(thisGame.DataFolderPath, thisGame.BSATextureName)), False) then
                    AddMessage(Format('Copied %s to Data folder!', [thisGame.BSATextureName]), True)
                else begin
                    AddMessage(Format('Couldn''t copy %s to Data Folder: %s',
                        [thisGame.BSATextureName, SysErrorMessage(GetLastError)]), True);
                    manualCopy := True;
                end;
            end;
        end else manualCopy := True;

        if manualCopy then begin
            Prefix := '';
            AddMessage(Format('Packed files can be found in [ %s ]',
                [thisGame.OutputDataPath]), True, True);

            AddMessage(Format('Packed files can be found in [ %s ], and can be installed manually, or with a mod manager of your choice.',
                [TPath.GetFullPath(thisGame.OutputDataPath)]));
        end;

        Destroy;
    end;
end;

constructor TArchiveThread.Create(const thisGame: TBakaGame; const settings: TArchiveSettings; const mainList: TStringList; const textList: TStringList);
begin
    inherited Create;

    self.thisGame := thisGame;
    self.settings := settings;
    self.mainList := mainList;
    self.textList := textList;
end;

{ TArchiveWrapperThread }

procedure TArchiveWrapperThread.Execute;
var
    PI: TProcessInformation;
    SA: TSecurityAttributes;
    SI: TStartupInfo;

    StdOutPipeRead, StdOutPipeWrite: THandle;
    Buffer: array[0..255] of AnsiChar;
    Handle, WasOK: Boolean;
    BytesRead: Cardinal;

begin
    with SA do begin
        nLength := SizeOf(SA);
        bInheritHandle := True;
        lpSecurityDescriptor := nil;
    end;

    CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
    try
        with SI do
        begin
            FillChar(SI, SizeOf(SI), 0);
            cb := SizeOf(SI);
            dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
            wShowWindow := SW_HIDE;
            hStdInput := GetStdHandle(STD_INPUT_HANDLE);
            hStdOutput := StdOutPipeWrite;
            hStdError := StdOutPipeWrite;
        end;

        Handle := CreateProcess(nil, PChar(Command), nil, nil, True, 0, nil,
            PChar(TPath.Combine(GetCurrentDir, 'BakaOutput\Fallout 76\')), SI, PI);

        CloseHandle(StdOutPipeWrite);

        if Handle then
            AddMessage('Starting...', True);

            try
                repeat
                    WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
                    AddMessage('Packing Texture Archive File...');

                    if BytesRead > 0 then begin
                        Buffer[BytesRead] := #0;
                        AddMessage(Trim(String(Buffer)));
                    end;

                until not WasOK or (BytesRead = 0);
                    WaitForSingleObject(PI.hProcess, INFINITE);

            finally
                CloseHandle(PI.hThread);
                CloseHandle(PI.hProcess);
                AddMessage('Texture Archive Packing: Done.', True);
            end;

    finally
        CloseHandle(StdOutPipeRead);
        fileLines.Free;
    end;
end;

constructor TArchiveWrapperThread.Create(const archivePath: string; const inputFilePath: string);
begin
    inherited Create;

    Prefix := 'Archive2: ';

    fileLines := TStringList.Create;
    fileLines.LoadFromFile(inputFilePath);

    Command := Format('"%s" -c="%s" -s="%s" -f=DDS -compression=%s -q',
        [archivePath, 'BakaFile - Textures.ba2', 'TempArchiveFileList.txt', 'Default']);
end;

{ TBakaWindow }

function TBakaWindow.AddWrapper(List: TList<TBakaGame>; const shortName: string; const Name: string; const BSAType: TBSArchiveType; const isBA2: Boolean = False): TBakaGame;
begin
    List.Add(TBakaGame.Create(shortName, Name, BSAType, isBA2, False));
    Result := List.Items[Pred(List.Count)];
end;

procedure TBakaWindow.ArchiveSelectButtonClick(Sender: TObject);
begin
    with TFileOpenDialog.Create(nil) do begin
        try
            Title := 'Select Archive2'; DefaultFolder := GetCurrentDir; FileName := '';
            Options := [fdoStrictFileTypes, fdoFileMustExist, fdoPathMustExist, fdoDontAddToRecent];

            SettingArchivePath.Clear; archivePath := '';
            SettingArchive2.Enabled := False;

            with FileTypes.Add do begin
                DisplayName := 'Archive2'; FileMask := 'Archive2.exe';
            end;

            if Execute then begin
                if not SameText(Trim(FileName), '') then begin
                    if SameText(TPath.GetFileName(FileName), 'Archive2.exe') then begin
                        SettingArchivePath.Text := FileName; archivePath := FileName;
                        SettingArchive2.Enabled := True;
                    end else begin
                        AddMessage('Invalid Archive2 Path Selected!', True);
                    end;
                end;
            end;
        finally
            Free;
        end;
    end;
end;

procedure TBakaWindow.AddFilterButtonClick(Sender: TObject);
begin
    if not SameText(Trim(zzFilterInput.Text), '') then begin
        zzFilterBox.Items.Insert(zzFilterBox.Items.Count, zzFilterInput.Text);

        if SettingAutoRefresh.Checked then
            GameSelectUpdate(GameSelect.ItemIndex, True);
    end;

    zzFilterInput.Clear;
end;

procedure TBakaWindow.AddMessage(const Message: string; const onStatus: boolean; const onlyStatus: boolean);
begin
    if onStatus then begin
        StatusBar.Panels[0].Text := ' ' + Message;
        if onlyStatus then Exit;
    end;

    Console.Lines.Append(Message);
    if SettingAutoScroll.Checked then
        SendMessage(Console.Handle, EM_LINESCROLL, 0, Console.Lines.Count);
end;

procedure TBakaWindow.AppendMessage(const Addition: string);
var
    Original: string;

begin
    Original := Console.Lines[Pred(Console.Lines.Count)];
    Console.Lines[Pred(Console.Lines.Count)] := Original + Addition;
end;

procedure TBakaWindow.FormCreate(Sender: TObject);
var
    i: Integer;

begin
    AddMessage(Format('BakaFileTool (%s) starting session %s', [
        GetFileVersion(Application.ExeName),
        FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]));

    BannedFileNamePaths := ['.ba2', '.bak', '.bik', '.bk2', '.bsa', '.btd', '.cdf', '.dat', '.db', '.dll', '.esl',
                            '.esm', '.esp', '.exe', '.nam', '.rar', '.sdp', '.zip', 'MWSE', 'OBSE', 'FOSE', 'FOSE',
                            'NVSE', 'SKSE', 'F4SE', 'DialogueViews', 'Edit Backups', 'Edit Cache', '.7z'];

    archiveList := TStringList.Create;
    textureList := TStringList.Create;
    MasterList  := TList<TBakaGame>.Create;
    ActiveList  := TList<TBakaGame>.Create;

    LoadSettings;

    with AddWrapper(MasterList, 'GRID', 'Gridiron!', baTES3) do begin
        // Placeholder for <Select Game>
    end;

    with AddWrapper(MasterList, 'TES5', 'Skyrim', baFO3) do begin
        PluginName      := 'BakaFile.esm';
        PluginFile      := BakaFileTES5;
        SelectButton    := TES5PathSelectButton;
        SelectBox       := SettingTES5Path;
        Initialize;
    end;

    with AddWrapper(MasterList, 'SSE', 'Skyrim Special Edition', baSSE) do begin
        ConfigFileName  := 'Skyrim';
        DataFileName    := 'Skyrim';
        PluginName      := 'BakaFile.esl';
        PluginFile      := BakaFileSSE;
        SelectButton    := SSEPathSelectButton;
        SelectBox       := SettingSSEPath;
        Initialize;
    end;

    with AddWrapper(MasterList, 'SSEVR', 'Skyrim VR', baSSE) do begin
        ConfigFileName  := 'Skyrim';
        DataFileName    := 'Skyrim';
        PluginName      := 'BakaFile.esm';
        PluginFile      := BakaFileSSEVR;
        SelectButton    := SSEVRPathSelectButton;
        SelectBox       := SettingSSEVRPath;
        Initialize;
    end;

    with AddWrapper(MasterList, 'FO4', 'Fallout 4', baFO4, True) do begin
        SetAllNames('Fallout4');
        PluginName      := 'BakaFile.esl';
        PluginFile      := BakaFileFO4;
        ConfigFile      := BakaCfgFO4;
        SelectButton    := FO4PathSelectButton;
        SelectBox       := SettingFO4Path;
        Initialize;
    end;

    with AddWrapper(MasterList, 'FO4VR', 'Fallout 4 VR', baFO4, True) do begin
        SetAllNames('Fallout4VR');
        ConfigFileName  := 'Fallout4';
        DataFileName    := 'Fallout4';
        PluginName      := 'BakaFile.esm';
        PluginFile      := BakaFileFO4VR;
        ConfigFile      := BakaCfgFO4VR;
        SelectButton    := FO4VRPathSelectButton;
        SelectBox       := SettingFO4VRPath;
        Initialize;
    end;

    with AddWrapper(MasterList, 'FO76', 'Fallout 76', baFO4, True) do begin
        ConfigFolderName:= 'Fallout 76';
        RegKeyName      := 'Fallout 76';
        ConfigFileName  := 'Fallout76';
        AppDataName     := 'Fallout76';
        DataFileName    := 'SeventySix';
        PluginName      := 'BakaFile.esm';
        PluginFile      := BakaFileFO76;
        ConfigFile      := BakaCfgFO76;
        SelectButton    := FO76PathSelectButton;
        SelectBox       := SettingFO76Path;
        isAlternativeRegPath := True;
        Initialize;
    end;

    SettingsBlock.Top := 5;

    if SettingMaximized.Checked then
        WindowState := wsMaximized;

    if (zzFilterBox.Items.Count = 0) then
        zzFilterBox.Items.Insert(0, 'Scripts\Source\');

    GameSelect.Items.Add('<select game>');
    GameSelect.ItemIndex := 0;

    ActiveList.Add(MasterList[0]);

    for i := 1 to Pred(MasterList.Count) do begin
        if MasterList[i].isValidPath then begin
            GameSelect.Items.Add(MasterList[i].Name);
            ActiveList.Add(MasterList[i]);

        end else begin
            MasterList[i].DataFolderPath := MasterList[i].SelectBox.Text;
            MasterList[i].ReInit;

            if MasterList[i].isValidPath then begin
                GameSelect.Items.Add(MasterList[i].Name);
                ActiveList.Add(MasterList[i]);

            end else if SettingWarnMissing.Checked then begin
                AddMessage(Format('%s''s manually specificed path is incorrect!', [MasterList[i].Name]));
            end;

            if (not MasterList[i].isValidPath and SettingWarnMissing.Checked) then begin
                AddMessage(Format('%s was not detected!', [MasterList[i].Name]));

                if MatchText(MasterList[i].ShortName, ['FO76']) then
                    AppendMessage(' Run Scan and Repair in the Bethesda.net Launcher to restore registry keys.')
                else AppendMessage(' Run its launcher to restore registry keys.');
            end;
        end;

        if MasterList[i].isValidPath then
            MasterList[i].SelectBox.Text := MasterList[i].GetRootPath
        else MasterList[i].SelectBox.Clear;
    end;

    archivePath := TPath.Combine(MasterList[4].GetRootPath, 'Tools\Archive2\Archive2.exe');
    if not FileExists(archivePath) then begin
        archivePath := SettingArchivePath.Text;
        if not FileExists(archivePath) then begin
            if SettingWarnMissing.Checked then
                AddMessage('Archive2''s manually specificed path is incorrect!');

            SettingArchive2.Enabled := False;
        end;

        if not FileExists(archivePath) then begin
            if SettingWarnMissing.Checked then
                AddMessage('Archive2 was not detected! Fallout 76 texture building will be done with BSArch.');

            SettingArchive2.Enabled := False;
            SettingArchivePath.Clear;

        end else begin
            SettingArchivePath.Text := archivePath;
            SettingArchive2.Enabled := True;
        end;
    end else begin
        SettingArchivePath.Text := archivePath;
        SettingArchive2.Enabled := True;
    end;

    AddMessage('I-it''s not like I want to pack your files or anything!!', True, True);
    GameSelectUpdate(0);
end;

procedure TBakaWindow.FormDestroy(Sender: TObject);
var
    logFile: TextFile;
    i: integer;

begin
    SaveSettings;

    if SettingSaveLog.Checked then begin
        AssignFile(logFile, TPath.Combine(GetCurrentDir, 'BakaFileTool.log')); ReWrite(logFile);

        WriteLn(logFile, Console.Lines[0]);
        WriteLn(logFile, 'It''s not like I''m saving this for you or anything!');

        for i := 1 to Pred(Console.Lines.Count) do
            WriteLn(logFile, Console.Lines[i]);
        CloseFile(logFile);
    end;
end;

procedure TBakaWindow.FormResize(Sender: TObject);
var
    combHeight: integer;
begin
    BlockText01.Left := Floor((SettingsBlock.Width - BlockText01.Width) / 2);
    BlockText02.Left := Floor((SettingsBlock.Width - BlockText02.Width) / 2);
    BlockText03.Left := Floor((SettingsBlock.Width - BlockText03.Width) / 2);

    combHeight := BlockText01.Height + BlockText02.Height + BlockText03.Height;
    BlockText01.Top := Floor((SettingsBlock.Height - combHeight) / 3) - 30;
    BlockText02.Top := Floor((SettingsBlock.Height - combHeight) / 3) - 2;
    BlockText03.Top := Floor((SettingsBlock.Height - combHeight) / 3) + 15;
end;

procedure TBakaWindow.InitWindowSettings;
begin
    SettingAdvancedClick(nil);
    SettingDataManualClick(nil);
end;

procedure TBakaWindow.GamePathBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
    GamePathBox.VertScrollBar.Position := GamePathBox.VertScrollBar.Position - (WheelDelta div 6);
end;

procedure TBakaWindow.GamePathBoxResize(Sender: TObject);
begin
    if GamePathBox.VertScrollBar.IsScrollBarVisible then begin
        TES5PathBox.Width  := (ArchivePathBox.Width - 24);
        SSEPathBox.Width   := (ArchivePathBox.Width - 24);
        FO4PathBox.Width   := (ArchivePathBox.Width - 24);
        FO4VRPathBox.Width := (ArchivePathBox.Width - 24);
        SSEVRPathBox.Width := (ArchivePathBox.Width - 24);
        FO76PathBox.Width  := (ArchivePathBox.Width - 24);
    end else begin
        TES5PathBox.Width  := ArchivePathBox.Width;
        SSEPathBox.Width   := ArchivePathBox.Width;
        FO4PathBox.Width   := ArchivePathBox.Width;
        FO4VRPathBox.Width := ArchivePathBox.Width;
        SSEVRPathBox.Width := ArchivePathBox.Width;
        FO76PathBox.Width  := ArchivePathBox.Width;
    end;
end;

procedure TBakaWindow.GameSelectChange(Sender: TObject);
begin
    GameSelectUpdate(GameSelect.ItemIndex);
end;

procedure TBakaWindow.GameSelectUpdate(GameIndex: Integer; const Force: boolean);
var
    buildFileThread: TBuildFileThread;
    filterList: TStringList;
    filter: string;

begin
    if isBuildingFileList then Exit;

    if (GameIndex <> 0) then begin
        if (CachedGameIndex = GameIndex) and not Force then Exit;

        if CachedGameIndex <> 0 then
            if not Force then
                thisGame.InputDataPath := '';

        thisGame := ActiveList[GameIndex];
        thisGame.ArchivePath := archivePath;

        CachedGameIndex := GameIndex;
        isBuildingFileList := True;

        if not Force then AddMessage(format('Selected Game Mode: %s', [thisGame.Name]), True) else
        AddMessage(format('Updating Game Directory: %s', [thisGame.GetDataPath]), True);

        GamePath.Text := thisGame.GetDataPath;

        filterList := TStringList.Create;
        for filter in BannedFileNamePaths do filterList.Add(filter);
        for filter in zzFilterBox.Items do filterList.Add(filter);

        buildFileThread := TBuildFileThread.Create(filterList);
        // filterList.Free;

    end else begin
        SettingDataManual.Enabled := False;
        SelectButton.Enabled   := False;
        RefreshButton.Enabled  := False;
        RunButton.Enabled      := False;

        GamePath.Text := '';
    end;
end;

procedure TBakaWindow.LoadSettings;
var
    Settings: TMemINIFile;
    Filters: TStringList;
    Filter: string;
    Flag: integer;

begin
    Settings := TMemINIFile.Create('settings.baka');
    Filters := TStringList.Create;

    try
        SettingAdvanced.Checked     := Settings.ReadBool('General', 'bArchiveFlags',    False);
        SettingAutoCopy.Checked     := Settings.ReadBool('General', 'bAutoCopy',        False);
        SettingAutoRefresh.Checked  := Settings.ReadBool('General', 'bAutoRefresh',     False);
        SettingAutoScroll.Checked   := Settings.ReadBool('General', 'bAutoScroll',      False);
        SettingShare.Checked        := Settings.ReadBool('General', 'bBinaryShare',     False);
        SettingCompress.Checked     := Settings.ReadBool('General', 'bCompress',        False);
        SettingDataManual.Checked   := Settings.ReadBool('General', 'bManualSelect',    False);
        SettingMaximized.Checked    := Settings.ReadBool('General', 'bMaximized',       False);
        SettingThreaded.Checked     := Settings.ReadBool('General', 'bMultiThreading',  False);
        SettingSaveLog.Checked      := Settings.ReadBool('General', 'bSaveLog',         True);
        SettingArchive2.Checked     := Settings.ReadBool('General', 'bUseArchive2',     False);
        SettingWarnMissing.Checked  := Settings.ReadBool('General', 'bWarnMissing',     True);

        if Settings.ReadBool('General', 'bLewdMode', False) then
            AddMessage('Please don''t lewd me senpai!');

        SettingArchivePath.Text     := Settings.ReadString('Paths', 'sArchivePath',     '');
        SettingFO4Path.Text         := Settings.ReadString('Paths', 'sFO4Path',         '');
        SettingFO4VRPath.Text       := Settings.ReadString('Paths', 'sFO4VRPath',       '');
        SettingFO76Path.Text        := Settings.ReadString('Paths', 'sFO76Path',        '');
        SettingTES5Path.Text        := Settings.ReadString('Paths', 'sTES5Path',        '');
        SettingSSEPath.Text         := Settings.ReadString('Paths', 'sSSEPath',         '');
        SettingSSEVRPath.Text       := Settings.ReadString('Paths', 'sSSEVRPath',       '');

        InitWindowSettings;

        Flag := Settings.ReadInteger('Flags', 'Archive', 0);
        if Flag = 0 then SettingArchiveFlags.Text := ''
        else SettingArchiveFlags.Text := IntToHex(Flag, 8);

        Flag := Settings.ReadInteger('Flags', 'File', 0);
        if Flag = 0 then SettingFileFlags.Text := ''
        else SettingFileFlags.Text := IntToHex(Flag, 8);

        Settings.ReadSection('Filter', Filters);

        for Filter in Filters do
            zzFilterBox.Items.Insert(zzFilterBox.Items.Count, Settings.ReadString('Filter', Filter, ''));

    finally
        Settings.Free;
        Filters.Free;
    end;
end;

procedure TBakaWindow.PathButtonClick(Sender: TObject);
var
    SelectedGame: TBakaGame;
    i: integer;

begin
    SelectedGame := nil;

    for i := 1 to MasterList.Count do begin
        if (MasterList[i].SelectButton = TButton(Sender)) then begin
            SelectedGame := MasterList[i];
            Break;
        end;
    end;

    if (SelectedGame <> nil) then begin
        with TFileOpenDialog.Create(nil) do begin
            try
                Title := 'Select Game Root Path'; DefaultFolder := GetCurrentDir; FileName := '';
                Options := [fdoPickFolders, fdoPathMustExist, fdoDontAddToRecent];

                SelectedGame.SelectBox.Clear;

                if Execute then begin
                    if not SameText(Trim(FileName), '') then begin
                        SelectedGame.DataFolderPath := FileName; SelectedGame.ReInit;
                        SelectedGame.SelectBox.Text := SelectedGame.GetRootPath;
                    end;
                end;
            finally
                Free;
            end;
        end;
    end;

    GameSelect.Items.Clear; GameSelect.Items.Add('<select game>');
    ActiveList.Clear; ActiveList.Add(MasterList[0]);
    GameSelect.ItemIndex := 0;

    for i := 1 to Pred(MasterList.Count) do begin
        if MasterList[i].isValidPath then begin
            GameSelect.Items.Add(MasterList[i].Name); ActiveList.Add(MasterList[i]);
            MasterList[i].SelectBox.Text := MasterList[i].GetRootPath;

            if (thisGame <> nil) then begin
                if SameText(thisGame.ShortName, MasterList[i].ShortName) then begin
                    GameSelect.ItemIndex := Pred(ActiveList.Count);
                end;
            end;
        end else
            if SameText(SelectedGame.ShortName, MasterList[i].ShortName) then
                AddMessage(Format('Data\%s was not found in the specified path!', [MasterList[i].DataFileName]), True);
    end;

    if (thisGame <> nil) then
        GameSelectUpdate(GameSelect.ItemIndex);
end;

procedure TBakaWindow.RefreshButtonClick(Sender: TObject);
begin
    GameSelectUpdate(GameSelect.ItemIndex, True);
end;

procedure TBakaWindow.RemoveFilterButtonClick(Sender: TObject);
var
    index: integer;

begin
    index := zzFilterBox.ItemIndex;
    zzFilterBox.DeleteSelected;

    if index < zzFilterBox.Items.Count then zzFilterBox.Selected[index] := True
    else if zzFilterBox.Items.Count = 1 then zzFilterBox.Selected[0] := True;

    if SettingAutoRefresh.Checked then
        GameSelectUpdate(GameSelect.ItemIndex, True);
end;

procedure TBakaWindow.ResetButtonClick(Sender: TObject);
begin
    if DeleteFile(PChar('settings.baka')) then begin
        zzFilterBox.Clear;
        LoadSettings;

        zzFilterBox.Items.Append('Scripts\Source\');
        SaveSettings;
    end;
end;

procedure TBakaWindow.RunButtonClick(Sender: TObject);
var
    buildThread: TArchiveThread;
    settings: TArchiveSettings;

begin
    if isBuildingArchive then Exit;
    isBuildingArchive := True;

    with settings do begin
        AutoCopy    := SettingAutoCopy.Checked;
        Compress    := SettingCompress.Checked;
        BinShare    := SettingShare.Checked;
        MultiThread := SettingThreaded.Checked;
        UseFlags    := SettingAdvanced.Checked;
        UseArchive2 := SettingArchive2.Checked and SettingArchive2.Enabled;

        if SameText(Trim(SettingArchiveFlags.Text), '') then
            ArchFlags := 0
        else ArchFlags := HexToInt(SettingArchiveFlags.Text);

        if SameText(Trim(SettingFileFlags.Text), '') then
            FileFlags := 0
        else FileFlags := HexToInt(SettingFileFlags.Text);
    end;

    buildThread := TArchiveThread.Create(thisGame, settings, archiveList, textureList);
end;

procedure TBakaWindow.SaveSettings;
var
    Settings: TMemINIFile;
    Filters: TStringList;
    Filter: string;
    i: integer;

begin
    Settings := TMemINIFile.Create('settings.baka');
    Filters := TStringList.Create;

    try
        Settings.WriteBool('General', 'bArchiveFlags',      SettingAdvanced.Checked);
        Settings.WriteBool('General', 'bAutoCopy',          SettingAutoCopy.Checked);
        Settings.WriteBool('General', 'bAutoRefresh',       SettingAutoRefresh.Checked);
        Settings.WriteBool('General', 'bAutoScroll',        SettingAutoScroll.Checked);
        Settings.WriteBool('General', 'bBinaryShare',       SettingShare.Checked);
        Settings.WriteBool('General', 'bCompress',          SettingCompress.Checked);
        Settings.WriteBool('General', 'bLewdMode',          False);
        Settings.WriteBool('General', 'bManualSelect',      SettingDataManual.Checked);
        Settings.WriteBool('General', 'bMaximized',         SettingMaximized.Checked);
        Settings.WriteBool('General', 'bMultiThreading',    SettingThreaded.Checked);
        Settings.WriteBool('General', 'bSaveLog',           SettingSaveLog.Checked);
        Settings.WriteBool('General', 'bUseArchive2',       SettingArchive2.Checked);
        Settings.WriteBool('General', 'bWarnMissing',       SettingWarnMissing.Checked);

        Settings.WriteString('Paths', 'sArchivePath',       SettingArchivePath.Text);
        Settings.WriteString('Paths', 'sFO4Path',           SettingFO4Path.Text);
        Settings.WriteString('Paths', 'sFO4VRPath',         SettingFO4VRPath.Text);
        Settings.WriteString('Paths', 'sFO76Path',          SettingFO76Path.Text);
        Settings.WriteString('Paths', 'sTES5Path',          SettingTES5Path.Text);
        Settings.WriteString('Paths', 'sSSEPath',           SettingSSEPath.Text);
        Settings.WriteString('Paths', 'sSSEVRPath',         SettingSSEVRPath.Text);

        if Trim(SettingArchiveFlags.Text) <> '' then
            Settings.WriteInteger('Flags', 'Archive', HexToInt(SettingArchiveFlags.Text))
        else Settings.WriteInteger('Flags', 'Archive', 0);

        if Trim(SettingFileFlags.Text) <> '' then
            Settings.WriteInteger('Flags', 'File', HexToInt(SettingFileFlags.Text))
        else Settings.WriteInteger('Flags', 'File', 0);

        Settings.ReadSection('Filter', Filters);
        for Filter in Filters do
            Settings.DeleteKey('Filter', Filter);

        for i := 0 to Pred(zzFilterBox.Items.Count) do
            Settings.WriteString('Filter', IntToStr(i), zzFilterBox.Items[i]);

        Settings.UpdateFile;

    finally
        Settings.Free;
        Filters.Free;
    end;
end;

procedure TBakaWindow.SelectButtonClick(Sender: TObject);
begin
    with TFileOpenDialog.Create(nil) do begin
        try
            Title := 'Select Directory to Archive'; DefaultFolder := GetCurrentDir; FileName := '';
            Options := [fdoPickFolders, fdoPathMustExist, fdoDontAddToRecent];

            if Execute then begin
                if not SameText(FileName, '') then begin
                    thisGame.InputDataPath := FileName;
                    GameSelectUpdate(GameSelect.ItemIndex, True);
                end;
            end;
        finally
            Free;
        end;
    end;
end;

procedure TBakaWindow.SettingAdvancedClick(Sender: TObject);
begin
    AdvancedSettings.Enabled    := SettingAdvanced.Checked;
    SettingArchiveFlags.Enabled := SettingAdvanced.Checked;
    SettingFileFlags.Enabled    := SettingAdvanced.Checked;
    ArchiveFlags.Enabled        := SettingAdvanced.Checked;
    FileFlags.Enabled           := SettingAdvanced.Checked;
end;

procedure TBakaWindow.SettingAutoScrollClick(Sender: TObject);
begin
    if SettingAutoScroll.Checked then
        SendMessage(Console.Handle, EM_LINESCROLL, 0, Console.Lines.Count);
end;

procedure TBakaWindow.SettingDataManualClick(Sender: TObject);
begin
    SelectButton.Enabled := SettingDataManual.Checked;
end;

procedure TBakaWindow.UpdatePluginFile;
var
    fixedFile, changedFile: boolean;
    fileLine, filePath: string;
    fileLines: TStringList;
    pluginFile: TextFile;
    i: integer;

begin
    if not DirectoryExists(thisGame.AppDataPath) then
        TDirectory.CreateDirectory(thisGame.AppDataPath);

    filePath := TPath.Combine(thisGame.AppDataPath, 'plugins.txt');
    changedFile := False; fixedFile := False;

    if not FileExists(filePath) then begin
        AssignFile(pluginFile, filePath); ReWrite(pluginFile);
        WriteLn(pluginFile, '# I only generated this file because I had nothing else to do!');
        WriteLn(pluginFile, '# Please don''t get the wrong idea!');

        if MatchText(thisGame.ShortName, ['TES5']) then
            WriteLn(pluginFile, 'BakaFile.esm')
        else if MatchText(thisGame.ShortName, ['FO4', 'SSE']) then
            WriteLn(pluginFile, '*BakaFile.esl')
        else WriteLn(pluginFile, '*BakaFile.esm');

        changedFile := True;
        CloseFile(pluginFile);

    end else begin
        fileLines := TStringList.Create;
        fileLines.LoadFromFile(filePath);

        for i := 0 to Pred(fileLines.Count) do begin
            if MatchText(thisGame.ShortName, ['TES5']) then begin
                if SameText(fileLines[i], 'bakafile.esm') then begin
                    fixedFile := True;
                    Break;
                end;
            end else if MatchText(thisGame.ShortName, ['FO4', 'SSE']) then begin
                if SameText(fileLines[i], '*bakafile.esl') then begin
                    fixedFile := True;
                    Break;
                end else if SameText(fileLines[i], 'bakafile.esl') then begin
                    fileLines[i] := '*BakaFile.esl';
                    changedFile := True; fixedFile := True;
                    Break;
                end;
            end else begin
                if SameText(fileLines[i], '*bakafile.esm') then begin
                    fixedFile := True;
                    Break;
                end else if SameText(fileLines[i], 'bakafile.esm') then begin
                    fileLines[i] := '*BakaFile.esm';
                    changedFile := True; fixedFile := True;
                    Break;
                end;
            end;
        end;

        if not fixedFile then begin
            if MatchText(thisGame.ShortName, ['TES5']) then begin
                fileLines.Append('BakaFile.esm');
            end else if MatchText(thisGame.ShortName, ['FO4', 'SSE']) then begin
                fileLines.Append('*BakaFile.esl');
            end else begin
                fileLines.Append('*BakaFile.esm');
            end;

            changedFile := True;
        end;

        if changedFile then begin
            AssignFile(pluginFile, filePath); ReWrite(pluginFile);
            WriteLn(pluginFile, '# I only generated this file because I had nothing else to do!');
            WriteLn(pluginFile, '# Please don''t get the wrong idea!');

            for fileLine in fileLines do
                if not (fileLine[1] in ['#']) then
                    WriteLn(pluginFile, fileLine);

            CloseFile(pluginFile);
            fileLines.Free;
        end;
    end;

    If changedFile then AddMessage(Format('Updated %s''s Plugins.txt file!', [thisGame.Name]), True);
end;

end.
