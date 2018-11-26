object BakaWindow: TBakaWindow
  Left = 0
  Top = 0
  Caption = 'Baka File-senpai~!'
  ClientHeight = 341
  ClientWidth = 644
  Color = clBtnFace
  Constraints.MinHeight = 380
  Constraints.MinWidth = 660
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    644
    341)
  PixelsPerInch = 96
  TextHeight = 13
  object MetaFrame: TPageControl
    Left = 5
    Top = 5
    Width = 635
    Height = 300
    ActivePage = SheetLog
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 300
    Constraints.MinWidth = 330
    TabOrder = 0
    TabPosition = tpBottom
    TabStop = False
    object SheetEditor: TTabSheet
      Caption = 'Tool'
      DesignSize = (
        627
        274)
      object GamePath: TMemo
        Left = 5
        Top = 5
        Width = 615
        Height = 25
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 1
        WantReturns = False
        WordWrap = False
      end
      object GameDataBox: TGroupBox
        Left = 3
        Top = 35
        Width = 170
        Height = 230
        Anchors = [akLeft, akTop, akBottom]
        Caption = ' Main Controls '
        TabOrder = 0
        object GameModeBox: TGroupBox
          Left = 10
          Top = 20
          Width = 150
          Height = 80
          Caption = ' Game Mode Select '
          TabOrder = 0
          object SettingDataManual: TCheckBox
            Left = 10
            Top = 20
            Width = 130
            Height = 20
            TabStop = False
            Caption = 'Manual Data Selection'
            TabOrder = 0
            OnClick = SettingDataManualClick
          end
          object GameSelect: TComboBoxEx
            Left = 10
            Top = 45
            Width = 130
            Height = 22
            AutoCompleteOptions = []
            ItemsEx = <>
            Style = csExDropDownList
            Constraints.MinHeight = 21
            TabOrder = 1
            TabStop = False
            OnChange = GameSelectChange
            DropDownCount = 15
          end
        end
        object ButtonBox: TGroupBox
          Left = 10
          Top = 105
          Width = 150
          Height = 115
          Caption = ' Run '
          TabOrder = 1
          object RefreshButton: TButton
            Left = 15
            Top = 50
            Width = 120
            Height = 25
            Caption = 'Refresh Data Folder'
            Enabled = False
            TabOrder = 0
            TabStop = False
            OnClick = RefreshButtonClick
          end
          object RunButton: TButton
            Left = 15
            Top = 20
            Width = 120
            Height = 25
            Caption = 'Create Archive'
            Enabled = False
            TabOrder = 1
            TabStop = False
            OnClick = RunButtonClick
          end
          object SelectButton: TButton
            Left = 15
            Top = 80
            Width = 120
            Height = 25
            Caption = 'Select Folder'
            Enabled = False
            TabOrder = 2
            TabStop = False
            OnClick = SelectButtonClick
          end
        end
      end
      object FileListBox: TMemo
        Left = 180
        Top = 41
        Width = 440
        Height = 223
        TabStop = False
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
      end
    end
    object SheetSettings: TTabSheet
      Caption = 'Settings'
      ImageIndex = 1
      DesignSize = (
        627
        274)
      object ProgramSettings: TGroupBox
        Left = 193
        Top = 3
        Width = 180
        Height = 265
        Anchors = [akLeft, akTop, akBottom]
        Caption = ' Program Settings '
        TabOrder = 3
        DesignSize = (
          180
          265)
        object SettingArchive2: TCheckBox
          Left = 10
          Top = 170
          Width = 160
          Height = 25
          TabStop = False
          Caption = 'Use Archive2 for Fallout 76'
          TabOrder = 0
        end
        object SettingWarnMissing: TCheckBox
          Left = 10
          Top = 145
          Width = 160
          Height = 25
          TabStop = False
          Caption = 'Undetected Game Warnings'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object SettingMaximized: TCheckBox
          Left = 10
          Top = 120
          Width = 160
          Height = 25
          TabStop = False
          Caption = 'Start Maximized'
          TabOrder = 2
        end
        object SettingSaveLog: TCheckBox
          Left = 10
          Top = 95
          Width = 160
          Height = 25
          TabStop = False
          Caption = 'Save Log on Exit'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object SettingAutoRefresh: TCheckBox
          Left = 10
          Top = 70
          Width = 160
          Height = 25
          TabStop = False
          Caption = 'Filter Update Auto-Refreshes'
          TabOrder = 4
        end
        object SettingAutoCopy: TCheckBox
          Left = 10
          Top = 45
          Width = 160
          Height = 25
          TabStop = False
          Caption = 'Copy Files After Packing'
          TabOrder = 5
        end
        object ResetButton: TButton
          Left = 9
          Top = 230
          Width = 162
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Reset to Defaults'
          TabOrder = 6
          TabStop = False
          OnClick = ResetButtonClick
        end
        object SettingAutoScroll: TCheckBox
          Left = 10
          Top = 20
          Width = 160
          Height = 25
          TabStop = False
          Caption = 'Auto-Scroll Log'
          TabOrder = 7
          OnClick = SettingAutoScrollClick
        end
      end
      object BlacklistBox: TGroupBox
        Left = 383
        Top = 3
        Width = 240
        Height = 265
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = ' User Blacklist '
        TabOrder = 2
        DesignSize = (
          240
          265)
        object zzFilterBox: TListBox
          Left = 10
          Top = 20
          Width = 220
          Height = 200
          TabStop = False
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          ItemHeight = 15
          ParentFont = False
          Sorted = True
          TabOrder = 0
        end
        object zzFilterInput: TEdit
          Left = 70
          Top = 231
          Width = 160
          Height = 23
          TabStop = False
          Anchors = [akLeft, akRight, akBottom]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object AddFilterButton: TButton
          Left = 9
          Top = 230
          Width = 25
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '+'
          TabOrder = 2
          TabStop = False
          OnClick = AddFilterButtonClick
        end
        object RemoveFilterButton: TButton
          Left = 39
          Top = 230
          Width = 25
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '-'
          TabOrder = 3
          TabStop = False
          OnClick = RemoveFilterButtonClick
        end
      end
      object ArchiveSettings: TGroupBox
        Left = 3
        Top = 3
        Width = 180
        Height = 265
        Anchors = [akLeft, akTop, akBottom]
        Caption = ' Archive Settings '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        DesignSize = (
          180
          265)
        object Spacer: TShape
          Left = 10
          Top = 100
          Width = 160
          Height = 1
          Pen.Color = 14474460
        end
        object SettingCompress: TCheckBox
          Left = 10
          Top = 20
          Width = 150
          Height = 25
          Hint =
            'Forces Archive Compression. Archives with sound files with still' +
            ' be uncompressed.'
          TabStop = False
          Caption = 'Compress Archives'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object SettingShare: TCheckBox
          Left = 10
          Top = 45
          Width = 150
          Height = 25
          Hint =
            'Files with identical binary data will share space in the archive' +
            '. Slower to pack, but saves space if duplicate files exist.'
          TabStop = False
          Caption = 'Identical Files Share Data'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object AdvancedSettings: TGroupBox
          Left = 10
          Top = 135
          Width = 160
          Height = 120
          Hint =
            'Override archive or file flags with a hex value. Oblivion, Fallo' +
            'ut 3/NV and Skyrim LE archives only.'
          Anchors = [akLeft, akTop, akBottom]
          Caption = ' Advanced Settings '
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          object ArchiveFlags: TLabel
            Left = 10
            Top = 20
            Width = 109
            Height = 13
            Hint =
              'Override archive flags with a hex value.\nOblivion, Fallout 3/NV' +
              ' and Skyrim LE archives only.'
            Caption = 'Override Archive Flags'
            Color = clBtnText
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object FileFlags: TLabel
            Left = 10
            Top = 70
            Width = 89
            Height = 13
            Hint =
              'Override file flags with a hex value.\nOblivion, Fallout 3/NV an' +
              'd Skyrim LE archives only.'
            Caption = 'Override File Flags'
            Color = clBtnText
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object SettingArchiveFlags: TEdit
            Left = 10
            Top = 40
            Width = 140
            Height = 21
            Hint =
              'Override archive flags with a hex value.\nOblivion, Fallout 3/NV' +
              ' and Skyrim LE archives only.'
            TabStop = False
            CharCase = ecLowerCase
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
          object SettingFileFlags: TEdit
            Left = 10
            Top = 90
            Width = 140
            Height = 21
            Hint =
              'Override file flags with a hex value.\nOblivion, Fallout 3/NV an' +
              'd Skyrim LE archives only.'
            TabStop = False
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
          end
        end
        object SettingAdvanced: TCheckBox
          Left = 10
          Top = 105
          Width = 150
          Height = 25
          Hint = 'Enables archive flag overrides'
          TabStop = False
          Caption = 'Enable Advanced Settings'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = SettingAdvancedClick
        end
        object SettingThreaded: TCheckBox
          Left = 10
          Top = 70
          Width = 150
          Height = 25
          Hint = 'Multi-Threads archiving. Might be unstable.'
          TabStop = False
          Caption = 'Multi-Threaded Archiving'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
        end
      end
      object SettingsBlock: TPanel
        Left = 3
        Top = -621
        Width = 621
        Height = 525
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelEdges = []
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        DesignSize = (
          621
          525)
        object BlockText01: TLabel
          Left = 68
          Top = 90
          Width = 485
          Height = 43
          Alignment = taCenter
          Anchors = []
          AutoSize = False
          Caption = 'You can'#39't be in here when I'#39'm working!!'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -27
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object BlockText02: TLabel
          Left = 158
          Top = 120
          Width = 305
          Height = 32
          Alignment = taCenter
          Anchors = []
          AutoSize = False
          Caption = 'N-not that I don'#39't like having you around...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object BlockText03: TLabel
          Left = 203
          Top = 140
          Width = 215
          Height = 26
          Alignment = taCenter
          Anchors = []
          AutoSize = False
          Caption = 'D-don'#39't get the w-wrong idea though!'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
      end
    end
    object SheetPaths: TTabSheet
      Caption = 'Paths'
      ImageIndex = 3
      DesignSize = (
        627
        274)
      object GamePathBox: TScrollBox
        Left = 3
        Top = 65
        Width = 619
        Height = 200
        HorzScrollBar.Visible = False
        VertScrollBar.Size = 20
        VertScrollBar.Tracking = True
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsNone
        ParentBackground = True
        TabOrder = 1
        OnMouseWheel = GamePathBoxMouseWheel
        OnResize = GamePathBoxResize
        DesignSize = (
          602
          200)
        object FO76PathBox: TGroupBox
          Left = 0
          Top = 10
          Width = 595
          Height = 60
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Fallout 76 Path '
          TabOrder = 1
          DesignSize = (
            595
            60)
          object FO76PathSelectButton: TButton
            Left = 10
            Top = 20
            Width = 130
            Height = 25
            Caption = 'Select Fallout 76 Path'
            TabOrder = 0
            TabStop = False
            OnClick = PathButtonClick
          end
          object SettingFO76Path: TEdit
            Left = 145
            Top = 21
            Width = 440
            Height = 23
            TabStop = False
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 1
          end
        end
        object FO4PathBox: TGroupBox
          Left = 0
          Top = 75
          Width = 595
          Height = 60
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Fallout 4 Path '
          TabOrder = 0
          DesignSize = (
            595
            60)
          object FO4PathSelectButton: TButton
            Left = 10
            Top = 20
            Width = 130
            Height = 25
            Caption = 'Select Fallout 4 Path'
            TabOrder = 0
            TabStop = False
            OnClick = PathButtonClick
          end
          object SettingFO4Path: TEdit
            Left = 145
            Top = 21
            Width = 440
            Height = 23
            TabStop = False
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 1
          end
        end
        object SSEPathBox: TGroupBox
          Left = 0
          Top = 140
          Width = 595
          Height = 60
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Skyrim Special Edition Path '
          TabOrder = 2
          DesignSize = (
            595
            60)
          object SSEPathSelectButton: TButton
            Left = 10
            Top = 20
            Width = 130
            Height = 25
            Caption = 'Select Skyrim SE Path'
            TabOrder = 0
            TabStop = False
            OnClick = PathButtonClick
          end
          object SettingSSEPath: TEdit
            Left = 145
            Top = 21
            Width = 440
            Height = 23
            TabStop = False
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 1
          end
        end
        object FO4VRPathBox: TGroupBox
          Left = 0
          Top = 270
          Width = 595
          Height = 60
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Fallout 4 VR Path '
          TabOrder = 3
          DesignSize = (
            595
            60)
          object FO4VRPathSelectButton: TButton
            Left = 10
            Top = 20
            Width = 130
            Height = 25
            Caption = 'Select Fallout 4 VR Path'
            TabOrder = 0
            TabStop = False
            OnClick = PathButtonClick
          end
          object SettingFO4VRPath: TEdit
            Left = 145
            Top = 21
            Width = 440
            Height = 23
            TabStop = False
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 1
          end
        end
        object SSEVRPathBox: TGroupBox
          Left = 0
          Top = 335
          Width = 595
          Height = 60
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Skyrim VR Path '
          TabOrder = 4
          DesignSize = (
            595
            60)
          object SSEVRPathSelectButton: TButton
            Left = 10
            Top = 20
            Width = 130
            Height = 25
            Caption = 'Select Skyrim VR Path'
            TabOrder = 0
            TabStop = False
            OnClick = PathButtonClick
          end
          object SettingSSEVRPath: TEdit
            Left = 145
            Top = 21
            Width = 440
            Height = 23
            TabStop = False
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 1
          end
        end
        object TES5PathBox: TGroupBox
          Left = 0
          Top = 205
          Width = 595
          Height = 60
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Skyrim (Classic) Path '
          TabOrder = 5
          DesignSize = (
            595
            60)
          object TES5PathSelectButton: TButton
            Left = 10
            Top = 20
            Width = 130
            Height = 25
            Caption = 'Select Skyrim Path'
            TabOrder = 0
            TabStop = False
            OnClick = PathButtonClick
          end
          object SettingTES5Path: TEdit
            Left = 145
            Top = 21
            Width = 440
            Height = 23
            TabStop = False
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 1
          end
        end
      end
      object ArchivePathBox: TGroupBox
        Left = 3
        Top = 3
        Width = 619
        Height = 60
        Anchors = [akLeft, akTop, akRight]
        Caption = ' Archive2 Path '
        TabOrder = 0
        DesignSize = (
          619
          60)
        object ArchiveSelectButton: TButton
          Left = 10
          Top = 20
          Width = 130
          Height = 25
          Caption = 'Select Archive2 Path'
          TabOrder = 0
          TabStop = False
          OnClick = ArchiveSelectButtonClick
        end
        object SettingArchivePath: TEdit
          Left = 145
          Top = 21
          Width = 462
          Height = 23
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 1
        end
      end
    end
    object SheetLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 2
      DesignSize = (
        627
        274)
      object Console: TMemo
        Left = 0
        Top = 1
        Width = 625
        Height = 270
        TabStop = False
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvNone
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 311
    Width = 644
    Height = 30
    Panels = <
      item
        Width = 50
      end>
  end
  object ProgressBar: TProgressBar
    Left = 470
    Top = 316
    Width = 150
    Height = 20
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object BakaFileTES5: TFileContainer
    Compressed = True
    Left = 40
    Top = 24
    CompressedData = {
      789C0B710D36B1606060606440006D20F6707509E2619835F3A63D58888381C1
      D9CFD117A8CAD73138849B2138BBB22833572FB53897C1C531C49103AE1700D1
      BB0C7B}
  end
  object BakaFileFO4: TFileContainer
    Compressed = True
    Left = 160
    Top = 24
    CompressedData = {
      789C0B710D367163606060646280836620F6707509E26130362EB6070B713030
      38FB39FAF232146724A6189403C9CA1463065FC7E0105E06B7C49C9CFCD21213
      BDD4E25C0617C710470EB84900CA0710DD}
  end
  object BakaCfgFO4: TFileContainer
    Compressed = True
    Left = 161
    Top = 89
    CompressedData = {
      789C8B762C4ACEC82C4B8DE5E52A0E4A2DCE2F2D4A4E0D2E492C2A092D80CAF8
      641697D8BA25E6E4E497969828E82A80654B0BF492128D741490C5331253528B
      8A31C43DF34A528BD212935321324E89D9896E9939A94019DFC4CC3C90200017
      002CC2}
  end
  object BakaFileSSE: TFileContainer
    Compressed = True
    Left = 40
    Top = 88
    CompressedData = {
      789C0B710D367161606060646280031D20F6707509E2619835F3A63D58888381
      C1D9CFD19797A1382331C5A01C4856A61833F83A068770330467571665E6EAA5
      16E732B838863872C0CD0100D3C31129}
  end
  object BakaFileFO4VR: TFileContainer
    Compressed = True
    Left = 280
    Top = 24
    CompressedData = {
      789C0B710D3671636060606440806620F6707509E26130362EB6070B71303038
      FB39FAF232146724A6189403C9CA1463065FC7E0105E06B7C49C9CFCD21213BD
      D4E25C0617C710470EB84900C95D10DB}
  end
  object BakaCfgFO4VR: TFileContainer
    Compressed = True
    Left = 281
    Top = 89
    CompressedData = {
      789C8B762C4ACEC82C4B8DE5E52A0E4A2DCE2F2D4A4E0D2E492C2A092D80CAF8
      641697D8BA25E6E4E497969828E82A80654B0BF492128D741490C5331253528B
      8A31C43DF34A528BD212935321324E89D9896E9939A94019DFC4CC3C90200017
      002CC2}
  end
  object BakaFileSSEVR: TFileContainer
    Compressed = True
    Left = 40
    Top = 152
    CompressedData = {
      789C0B710D3671616060606440001D20F6707509E2619835F3A63D58888381C1
      D9CFD19797A1382331C5A01C4856A61833F83A068770330467571665E6EAA516
      E732B838863872C0CD0100D31D1127}
  end
  object BakaFileFO76: TFileContainer
    Compressed = True
    Left = 400
    Top = 24
    CompressedData = {
      789C0B710D36F1606060606440806D40ECE1EA12C403A43B9CC0421C0C0CCE7E
      8EBEBC0CC519892906E540B232C598C1D73138849F2138B52C35AFA43238B342
      2FB53897C1C531C491036E1600F91111DB}
  end
  object BakaCfgFO76: TFileContainer
    Compressed = True
    Left = 401
    Top = 89
    CompressedData = {
      789C8B762C4ACEC82C4B8DE5E52A0E4A2DCE2F2D4A4E0D2E492C2A092D80CAF8
      64169728D82A04A796A5E6955406675628E82A78E695A416A52526A7EA25251A
      E9A0CAF9E42727E66456259664E6E761910ECE484C492D2AC62603B2B6B40022
      E394989DE89699930A14F74DCC041B04006FAA3B2E}
  end
end
