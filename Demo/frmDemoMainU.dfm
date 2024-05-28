object frmDemoMain: TfrmDemoMain
  Left = 0
  Top = 0
  Caption = 'Demo'
  ClientHeight = 926
  ClientWidth = 1450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'SegoeUI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1444
    Height = 920
    ActivePage = TabSheet9
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Controls'
      object PTLibProportionalSplitter1: TPTLibProportionalSplitter
        Left = 366
        Top = 419
        Height = 470
        ProportionalResize = True
        ExplicitLeft = 384
        ExplicitTop = 207
        ExplicitHeight = 252
      end
      object PTLibProportionalSplitter2: TPTLibProportionalSplitter
        Left = 0
        Top = 416
        Width = 1436
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ProportionalResize = True
        ExplicitTop = 223
        ExplicitWidth = 252
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 1436
        Height = 416
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        Caption = 'Dialog Edits'
        TabOrder = 0
        object PTLibOpenFileEdit1: TPTLibOpenFileEdit
          AlignWithMargins = True
          Left = 3
          Top = 33
          Width = 1430
          Height = 26
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alTop
          TabOrder = 0
          Text = 'PTLibOpenFileEdit1'
          Filename = 'PTLibOpenFileEdit1'
          OpenDialog.Title = 'Open File'
        end
        object PTLibSaveFileEdit1: TPTLibSaveFileEdit
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 1430
          Height = 26
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alTop
          TabOrder = 1
          Text = 'PTLibSaveFileEdit1'
          Filename = 'PTLibSaveFileEdit1'
          SaveDialog.Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
          SaveDialog.Title = 'Save File'
        end
        object PTLibDirectoryEdit1: TPTLibDirectoryEdit
          AlignWithMargins = True
          Left = 3
          Top = 63
          Width = 1430
          Height = 26
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alTop
          TabOrder = 2
          Text = 'PTLibDirectoryEdit1'
          DirectoryDialogOptions.Title = 'Select Directory'
          DirectoryDialogOptions.Options = [sdNewFolder, sdShowEdit, sdShowShares, sdNewUI]
        end
        object Edit1: TEdit
          Left = 1
          Top = 91
          Width = 1434
          Height = 24
          Align = alTop
          TabOrder = 3
          Text = 'Edit1'
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 419
        Width = 366
        Height = 470
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Caption = 'Test'
        Color = 11796403
        ParentBackground = False
        TabOrder = 1
      end
      object Panel3: TPanel
        Left = 369
        Top = 419
        Width = 1067
        Height = 470
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Caption = 'Directory Edit'
        TabOrder = 2
        object PTLibProportionalSplitter3: TPTLibProportionalSplitter
          Left = 367
          Top = 1
          Height = 468
          ProportionalResize = True
          ExplicitLeft = 374
          ExplicitTop = 241
          ExplicitHeight = 258
        end
        object Panel9: TPanel
          Left = 1
          Top = 1
          Width = 366
          Height = 468
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alLeft
          Caption = 'Test'
          Color = 11796403
          ParentBackground = False
          TabOrder = 0
          object PTLibProportionalSplitter4: TPTLibProportionalSplitter
            Left = 1
            Top = 317
            Width = 364
            Height = 3
            Cursor = crVSplit
            Align = alTop
            ProportionalResize = True
            ExplicitTop = 174
            ExplicitWidth = 81
          end
          object Panel13: TPanel
            Left = 1
            Top = 1
            Width = 364
            Height = 316
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alTop
            Caption = 'Test'
            Color = 11796403
            ParentBackground = False
            TabOrder = 0
          end
          object Panel14: TPanel
            Left = 1
            Top = 320
            Width = 364
            Height = 147
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alClient
            Caption = 'Directory Edit'
            TabOrder = 1
          end
        end
        object Panel11: TPanel
          Left = 370
          Top = 1
          Width = 696
          Height = 468
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          Caption = 'Directory Edit'
          TabOrder = 1
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Logging'
      ImageIndex = 1
      object Panel4: TPanel
        Left = 361
        Top = 0
        Width = 1075
        Height = 889
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object chkEnableLogReceiver: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 1069
          Height = 17
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Log Receiver Enabled'
          TabOrder = 0
          OnClick = chkEnableLogReceiverClick
        end
        object RichEdit2: TRichEdit
          AlignWithMargins = True
          Left = 3
          Top = 23
          Width = 1069
          Height = 863
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Lines.Strings = (
            'Remote')
          ParentFont = False
          PlainText = True
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
          Zoom = 100
        end
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 361
        Height = 889
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object chkLogTransmitterEnabled: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 355
          Height = 17
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Log Transmitter Enabled'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = chkLogTransmitterEnabledClick
        end
        object RichEdit1: TRichEdit
          AlignWithMargins = True
          Left = 3
          Top = 23
          Width = 355
          Height = 863
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Lines.Strings = (
            'Remote')
          ParentFont = False
          PlainText = True
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
          Zoom = 100
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Awesome Edit'
      ImageIndex = 2
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 1436
        Height = 142
        Align = alTop
        AutoSize = True
        Color = clWindow
        ParentBackground = False
        TabOrder = 0
        object SearchEdit1: TPTLibSearchEdit
          AlignWithMargins = True
          Left = 31
          Top = 21
          Width = 674
          Height = 100
          Margins.Left = 30
          Margins.Top = 20
          Margins.Right = 30
          Margins.Bottom = 20
          Buttons = <>
          EditMargins.Top = 0
          EditMargins.Right = 0
          EditMargins.Bottom = 0
          BorderRadius = 10
          BorderColor = clSilver
          BorderStyle = aebStandard
          BorderThickness = 1.000000000000000000
          AutoFontSize = True
          ParentColor = False
          TabOrder = 0
          Text = ''
          TextHint = 'Search'
          SearchControls = [scSearch, scSearchDropdown, scClear]
        end
      end
      object GroupBox3: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 145
        Width = 146
        Height = 741
        Align = alLeft
        Caption = 'Style'
        TabOrder = 1
        object Label3: TLabel
          Left = 7
          Top = 20
          Width = 42
          Height = 16
          Caption = 'Border:'
        end
        object Label4: TLabel
          Left = 7
          Top = 65
          Width = 86
          Height = 16
          Caption = 'Border Radius:'
        end
        object cbAwesomeBorder: TComboBox
          Left = 7
          Top = 35
          Width = 130
          Height = 24
          Style = csDropDownList
          ItemIndex = 1
          TabOrder = 0
          Text = 'Standard'
          OnChange = CommonAwesomeEditPropertyChange
          OnClick = CommonAwesomeEditPropertyChange
          Items.Strings = (
            'None'
            'Standard'
            'Platform')
        end
        object edtAwesomeRadius: TSpinEdit
          Left = 7
          Top = 79
          Width = 130
          Height = 26
          Increment = 2
          MaxValue = 90
          MinValue = 0
          TabOrder = 1
          Value = 50
          OnChange = CommonAwesomeEditPropertyChange
          OnClick = CommonAwesomeEditPropertyChange
        end
      end
    end
    object tabQueues: TTabSheet
      Caption = 'Queues'
      ImageIndex = 3
      OnResize = tabQueuesResize
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 1436
        Height = 28
        Align = alTop
        TabOrder = 0
        object btnStart: TButton
          Left = 1
          Top = 1
          Width = 112
          Height = 26
          Align = alLeft
          Caption = 'Start'
          TabOrder = 0
          OnClick = btnStartClick
        end
        object btnStop: TButton
          Left = 113
          Top = 1
          Width = 112
          Height = 26
          Align = alLeft
          Caption = 'Stop'
          Enabled = False
          TabOrder = 1
          OnClick = btnStopClick
        end
        object chkConnected: TCheckBox
          AlignWithMargins = True
          Left = 228
          Top = 4
          Width = 189
          Height = 20
          Align = alLeft
          Caption = 'Connected'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object edtQueueReliability: TSpinEdit
          Left = 352
          Top = 2
          Width = 65
          Height = 26
          MaxValue = 100
          MinValue = 0
          TabOrder = 3
          Value = 100
        end
      end
      object pnlLowPriorityQueue: TPanel
        Left = 0
        Top = 28
        Width = 185
        Height = 861
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        object Panel8: TPanel
          Left = 0
          Top = 0
          Width = 185
          Height = 19
          Align = alTop
          Caption = 'Low Priority Queue'
          TabOrder = 0
        end
        object memLowPriorityQueue: TRichEdit
          Left = 0
          Top = 19
          Width = 185
          Height = 842
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          PlainText = True
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
          Zoom = 100
        end
      end
      object pnlHighPriorityQueue: TPanel
        Left = 370
        Top = 28
        Width = 1066
        Height = 861
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        object Panel10: TPanel
          Left = 0
          Top = 0
          Width = 1066
          Height = 19
          Align = alTop
          Caption = 'High Priority Queue'
          TabOrder = 0
        end
        object memHighPriorityQueue: TRichEdit
          Left = 0
          Top = 19
          Width = 1066
          Height = 842
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          PlainText = True
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
          Zoom = 100
        end
      end
      object pnlMediumPriorityQueue: TPanel
        Left = 185
        Top = 28
        Width = 185
        Height = 861
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 3
        object Panel12: TPanel
          Left = 0
          Top = 0
          Width = 185
          Height = 19
          Align = alTop
          Caption = 'Medium Priority Queue'
          TabOrder = 0
        end
        object memMediumPriorityQueue: TRichEdit
          Left = 0
          Top = 19
          Width = 185
          Height = 842
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          PlainText = True
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
          Zoom = 100
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Console'
      ImageIndex = 4
      object Console1: TConsole
        Left = 0
        Top = 0
        Width = 1436
        Height = 889
        AutoUseInsertMode = True
        InsertMode = True
        InsertCaret = ctHorizontalLine
        OverwriteCaret = ctHalfBlock
        BorderSize = 3
        ExtraLineSpacing = 0
        ShowLastLineOnKeyPress = True
        OnCommandExecute = Console1CommandExecute
        OnGetPrompt = Console1GetPrompt
        Align = alClient
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentColor = False
        TabOrder = 0
        TabStop = False
      end
    end
    object tabMARS: TTabSheet
      Caption = 'MARS Curiosity'
      ImageIndex = 5
    end
    object TabSheet5: TTabSheet
      Caption = 'Map Quest'
      ImageIndex = 6
      object edtGeoAddress: TEdit
        Left = 16
        Top = 38
        Width = 248
        Height = 24
        TabOrder = 0
        Text = '235 Hoyles Lane, Cottam, Preston'
      end
      object Button1: TButton
        Left = 271
        Top = 36
        Width = 75
        Height = 25
        Caption = 'GeoCode'
        TabOrder = 1
        OnClick = Button1Click
      end
      object edtGeoLatRes: TEdit
        Left = 352
        Top = 38
        Width = 121
        Height = 24
        TabOrder = 2
      end
      object edtMapQuestAPIKey: TEdit
        Left = 16
        Top = 11
        Width = 248
        Height = 24
        TabOrder = 3
        Text = 'Fmjtd|luub2gur2q,72=o5-9uag9r'
        TextHint = 'Map Quest API Key'
      end
      object edtGeoAddressRes: TEdit
        Left = 352
        Top = 67
        Width = 248
        Height = 24
        TabOrder = 4
      end
      object Button2: TButton
        Left = 271
        Top = 65
        Width = 75
        Height = 25
        Caption = 'Reverese Geo'
        TabOrder = 5
        OnClick = Button2Click
      end
      object edtGeoLat: TEdit
        Left = 16
        Top = 67
        Width = 121
        Height = 24
        TabOrder = 6
      end
      object edtGeoLng: TEdit
        Left = 143
        Top = 67
        Width = 121
        Height = 24
        TabOrder = 7
      end
      object edtGeoLngRes: TEdit
        Left = 479
        Top = 38
        Width = 121
        Height = 24
        TabOrder = 8
      end
      object Button3: TButton
        Left = 271
        Top = 92
        Width = 75
        Height = 25
        Caption = 'Elevation'
        TabOrder = 9
        OnClick = Button3Click
      end
      object edtGeoElevation: TEdit
        Left = 352
        Top = 94
        Width = 248
        Height = 24
        TabOrder = 10
      end
    end
    object tabSmartCard: TTabSheet
      Caption = 'SmartCard PCSC'
      ImageIndex = 7
      object Panel15: TPanel
        Left = 0
        Top = 0
        Width = 1436
        Height = 32
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Button4: TButton
          AlignWithMargins = True
          Left = 84
          Top = 3
          Width = 75
          Height = 26
          Action = actSmartCardDisable
          Align = alLeft
          TabOrder = 0
        end
        object Button5: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 75
          Height = 26
          Action = actSmartCardEnable
          Align = alLeft
          TabOrder = 1
        end
      end
      object GroupBox1: TGroupBox
        AlignWithMargins = True
        Left = 682
        Top = 35
        Width = 673
        Height = 851
        Align = alLeft
        Caption = 'Errors'
        TabOrder = 1
        object memSmartCardErrors: TRichEdit
          AlignWithMargins = True
          Left = 5
          Top = 21
          Width = 663
          Height = 825
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'SegoeUI'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
          Zoom = 100
        end
      end
      object GroupBox2: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 35
        Width = 673
        Height = 851
        Align = alLeft
        Caption = 'Card Data'
        TabOrder = 2
        object Panel16: TPanel
          AlignWithMargins = True
          Left = 5
          Top = 46
          Width = 663
          Height = 19
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label1: TLabel
            Left = 0
            Top = 0
            Width = 97
            Height = 19
            Align = alLeft
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'UID:'
          end
          object lblSmartCardUID: TLabel
            Left = 97
            Top = 0
            Width = 566
            Height = 19
            Align = alClient
            AutoSize = False
            ExplicitLeft = 102
          end
        end
        object Panel17: TPanel
          AlignWithMargins = True
          Left = 5
          Top = 21
          Width = 663
          Height = 19
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object Label2: TLabel
            Left = 0
            Top = 0
            Width = 97
            Height = 19
            Align = alLeft
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Status:'
          end
          object lblSmartCardStatus: TLabel
            Left = 97
            Top = 0
            Width = 566
            Height = 19
            Align = alClient
            AutoSize = False
            ExplicitLeft = 144
            ExplicitTop = 8
            ExplicitWidth = 38
            ExplicitHeight = 16
          end
        end
        object Panel18: TPanel
          AlignWithMargins = True
          Left = 5
          Top = 121
          Width = 663
          Height = 19
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
          object Label6: TLabel
            Left = 0
            Top = 0
            Width = 97
            Height = 19
            Align = alLeft
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'ATR:'
          end
          object lblSmartCardATR: TLabel
            Left = 97
            Top = 0
            Width = 566
            Height = 19
            Align = alClient
            AutoSize = False
            ExplicitLeft = 144
            ExplicitTop = 8
            ExplicitWidth = 38
            ExplicitHeight = 16
          end
        end
        object Panel19: TPanel
          AlignWithMargins = True
          Left = 5
          Top = 96
          Width = 663
          Height = 19
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 3
          object Label8: TLabel
            Left = 0
            Top = 0
            Width = 97
            Height = 19
            Align = alLeft
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Protocol:'
          end
          object lblSmartCardProtocol: TLabel
            Left = 97
            Top = 0
            Width = 566
            Height = 19
            Align = alClient
            AutoSize = False
            ExplicitLeft = 144
            ExplicitTop = 8
            ExplicitWidth = 38
            ExplicitHeight = 16
          end
        end
        object Panel20: TPanel
          AlignWithMargins = True
          Left = 5
          Top = 71
          Width = 663
          Height = 19
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 4
          object Label10: TLabel
            Left = 0
            Top = 0
            Width = 97
            Height = 19
            Align = alLeft
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Card Type:'
            ExplicitLeft = -6
          end
          object lblSmartCardType: TLabel
            Left = 97
            Top = 0
            Width = 566
            Height = 19
            Align = alClient
            AutoSize = False
            ExplicitLeft = 144
            ExplicitTop = 8
            ExplicitWidth = 38
            ExplicitHeight = 16
          end
        end
      end
    end
    object tabPageControl: TTabSheet
      Caption = 'Page Control'
      ImageIndex = 8
      object PTLibPageControl1: TPTLibPageControl
        Left = 256
        Top = 168
        Width = 289
        Height = 193
        ActivePage = TabSheet8
        TabOrder = 0
        HideTabs = htoRuntime
        object TabSheet6: TTabSheet
          Caption = 'TabSheet6'
          object Memo1: TMemo
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 291
            Height = 172
            Align = alClient
            Lines.Strings = (
              'Memo1')
            TabOrder = 0
          end
        end
        object TabSheet7: TTabSheet
          Caption = 'TabSheet7'
          ImageIndex = 1
          object Button6: TButton
            Left = 0
            Top = 0
            Width = 297
            Height = 178
            Align = alClient
            Caption = 'Button6'
            TabOrder = 0
          end
        end
        object TabSheet8: TTabSheet
          Caption = 'TabSheet8'
          ImageIndex = 2
        end
      end
      object Button7: TButton
        Left = 568
        Top = 184
        Width = 75
        Height = 25
        Caption = 'Tab 1'
        TabOrder = 1
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 568
        Top = 215
        Width = 75
        Height = 25
        Caption = 'Tab 2'
        TabOrder = 2
        OnClick = Button8Click
      end
      object TabControl1: TTabControl
        Left = 792
        Top = 304
        Width = 289
        Height = 193
        TabOrder = 3
        Tabs.Strings = (
          'test'
          'test')
        TabIndex = 0
        StyleElements = []
        object Button9: TButton
          Left = 48
          Top = 64
          Width = 75
          Height = 25
          Caption = 'Button9'
          TabOrder = 0
        end
      end
      object Button10: TButton
        Left = 568
        Top = 246
        Width = 75
        Height = 25
        Caption = 'Tab 3'
        TabOrder = 4
        OnClick = Button10Click
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Virtual Log'
      ImageIndex = 9
      object vtLog: TPTLibVirtualLogTree
        Left = 0
        Top = 32
        Width = 1436
        Height = 857
        SaveLoadWithParentForm = False
        TreeOptions2 = [toAutoCheckSelectedNodes, toSupressEventsOnAutoCheckNodes]
        Align = alClient
        DefaultNodeHeight = 20
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Header.AutoSizeIndex = -1
        Header.Height = 20
        Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHeaderClickAutoSort]
        Images = ImageList1
        NodeDataSize = 4
        ParentFont = False
        TabOrder = 0
        TreeOptions.PaintOptions = [toHideFocusRect, toShowDropmark, toShowHorzGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
        TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
        AutoScroll = True
        RemoveControlCharacters = True
        AutoLogLevelColours = False
        ShowDateColumn = True
        ShowImages = True
        MaximumLines = 0
        PopupMenuItems = [pmiSaveLog, pmiSaveSelected, pmiCopyToClipboard, pmiCopySelectedToClipBoard, pmiClear, pmiDividers]
        ShowTimeOnly = False
        ThreadSafe = True
        Columns = <
          item
            Position = 0
            Text = 'Date'
            Width = 1436
          end
          item
            MinWidth = 300
            Position = 1
            Text = 'Log'
            Width = 300
          end>
      end
      object Panel21: TPanel
        Left = 0
        Top = 0
        Width = 1436
        Height = 32
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Button11: TButton
          Left = 0
          Top = 1
          Width = 75
          Height = 25
          Caption = 'Log'
          TabOrder = 0
          OnClick = Button11Click
        end
      end
    end
  end
  object tmrQueuesSend: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrQueuesSendTimer
    Left = 80
    Top = 847
  end
  object tmrCheckSend: TTimer
    Enabled = False
    OnTimer = tmrCheckSendTimer
    Left = 168
    Top = 847
  end
  object NetHTTPRequest1: TNetHTTPRequest
    ConnectionTimeout = 0
    SendTimeout = 0
    ResponseTimeout = 0
    Left = 649
    Top = 847
  end
  object NetHTTPClient1: TNetHTTPClient
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 552
    Top = 847
  end
  object PTLibMultiPriorityStringDataQueues1: TPTLibMultiPriorityStringDataQueues
    Active = False
    Directory = 'c:\MessageQueues\'
    MaxSendCount = 20
    Left = 297
    Top = 847
  end
  object tmrConsoleReply: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrConsoleReplyTimer
    Left = 448
    Top = 848
  end
  object ImageList1: TImageList
    Left = 431
    Top = 691
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 919
    Top = 86
    object actSmartCardEnable: TAction
      Category = 'SmartCard'
      Caption = 'Enable'
      OnExecute = actSmartCardEnableExecute
    end
    object actSmartCardDisable: TAction
      Category = 'SmartCard'
      Caption = 'Disable'
      OnExecute = actSmartCardDisableExecute
    end
  end
  object VirtualImageList1: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'LogUnknown'
        Disabled = False
        Name = 'LogUnknown'
      end
      item
        CollectionIndex = 1
        CollectionName = 'LogError'
        Disabled = False
        Name = 'LogError'
      end
      item
        CollectionIndex = 2
        CollectionName = 'LogWarning'
        Disabled = False
        Name = 'LogWarning'
      end
      item
        CollectionIndex = 3
        CollectionName = 'LogInfo'
        Disabled = False
        Name = 'LogInfo'
      end
      item
        CollectionIndex = 4
        CollectionName = 'LogDebug1'
        Disabled = False
        Name = 'LogDebug1'
      end
      item
        CollectionIndex = 5
        CollectionName = 'LogDebug2'
        Disabled = False
        Name = 'LogDebug2'
      end
      item
        CollectionIndex = 6
        CollectionName = 'LogDebug3'
        Disabled = False
        Name = 'LogDebug3'
      end>
    ImageCollection = ImageCollection1
    Left = 364
    Top = 144
  end
  object ImageCollection1: TImageCollection
    Images = <
      item
        Name = 'LogUnknown'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA864000002BC49444154384FAD92FB
              4B5A611CC6CFDFD4BA40455954505011085D28936A53CB6E53BB28769A5A7613
              4B995DAC56D6BA59982595D56C5B454441EB97582C367F1ECDD5296129BCCFDE
              7326C87EDEBEF070CE0FE7F3BCCFF37E0FF35F4622B10F99CD6BD1DEDE556230
              AC12965D225D5D6F894EE722EDED3344AD9E244D4D1344A1701099CC462A2A06
              2279795A3686338CC9B41A0D06396C6C7C81CF7723C8E3F90C97EB02E3E32770
              380E61B5EEA1AFCF87EEEE35B4B5CDA3B090BD8BE10C6330AC904020483F3CC3
              C4C405B4DA0D188D8B5858D887D77B8CD1D13DA8546EAA253437BB5053F31A39
              39EDE118CE302CBB480E0E82181B3B875ABD02BFFF23EEEFBF83E3427878B843
              2412C6F9F9376AB08C969639D4D63A909DAD891BE8F5F3C4EFBFA1094E61B79F
              40AF77E3FAFA2B76763EE1F6F60ED16804FCF4F6EEA1B1710652A91D5959EAB8
              4167E72CF17AAF68CF438C8C1C6178F858A8A1D56E22140A0B303F46E33694CA
              37A8AEB62133F365DC40A399226EF725CCE67D0C0E06D0DFBF8F9E1E3FC2E15F
              3114D8DDBDA1B00B0AC5242A2BADC8C8688D1BB4B63A89CB750A83618B9EB205
              96F5617DFD328642D88452398B868669C8E54E94960E2035B5316EA0543A88D3
              7944237B68FF75E8741E582C015AE5034C263FBDF97921BA4C36216C402C3623
              25A53E6EA050D888DDBE078D6699EEF88F6A6B47E1746E637AFABD109D3F9987
              2592111415BD4272B2226E5057672503033E6145BCE4F22904026734FC935061
              73F38A8236A13B1FBFA0408FA42479DC402A1D2246E39A70417C4F996C92FE13
              EFF0F8F844B710A5B53C282FB70870498909B9B91D7F1B88C5A64847C71CAAAA
              868515F1CFB2B221FAEEA09005C5C50621367F320FA7A5352131F1C5CF18CE30
              22919ACDCFD785B2B3559C48A4E232329AB9F4F4462E25A5814B4CACE79E3D53
              084A48E025A792FD484878AE8AE1FF320CF31BC6100659EC80A2A40000000049
              454E44AE426082}
          end>
      end
      item
        Name = 'LogError'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA864000002B649444154384FAD9259
              48546118863FCB713225B145B01C334AD3D02097C9AC28D1226C9122CC36E822
              88A0BA11EA3AE8BA6B31888A0A5AA968DF234AC10B734947321D9D99CA73E6CC
              CC193DB368F67446071ABAAD17DECBE7F9FFEFE393FF92897C3915DDBBCA133A
              6857C6F75728BEC652C5BBAB585176AC52466B0A9491EAA58A7363AE32685FAC
              F4962E543A8BE679DAE6CB89382E12DE57ECC7F91C2E1D829606DC47AAE0DC16
              38BD098EDBE14011D1DA6C425B72D136DBF85269E3638ED517C745C60E5719DC
              3A09A732E9B667D2F3EC15AEB214A84B860D82516EC118E825BA3E9D6F36A17B
              89F03A438C382E123C506970F5185D1519387A1CB8341F6E473F81B224429529
              183E1F51CC4C4E10DE96434796F0625E82C0DF586178CFD4D3DEDCC2A0AAE1F2
              78180DE8F8FABA19770D4DC3BF628258063FE1583E8B87E90902657789C1F906
              D435C2C0D3C7B8CD1F28AA4A606C8C5034CAD4AF38AE38D1CAACB42E10EEA425
              08BED715199CDD09DB2D848A851FCFEFA1FA03E8BA4E381C9E797DA807AD2409
              47B6F0619170233541E0AC5969D0540B1B85C85A0B3F7ABB50CDB96382A8F983
              69815FC1BFD546E77CE18DD96BD604C1C0A6E50647D7122E9F8DB3AF0F8FEA45
              F56AE8E3E3842726FF8C3015C1BB77354FE60857921304FDEBF20CF6143272FB
              125F151F2EB79B51BFCED8400F91FE0E2666F0996883B4ADCCE08224083EDB6D
              46B8BE8848A1E06E7D8B53D3F10EF5A3AFCF402D4FE7A73AC2640C8E98F75693
              C79D64E1425282A0B324CBF0D515F02D4F089659F0BFBB875E9D35BDB0AE8582
              67730E0C77305CBB8CA756E156EA5F82D6FCCC80B37A057DB9E69599D097020B
              ED26D8661ECC7B73E32FD384D665A9DC4F11EECE9D9E9F66F37CE2B8C8937439
              FD6A91557B3457820F522578D3EC75AB04AF59247879B6042F9A6D310FB62549
              82CDB18A68669BE2F8BF44E4370EFC036257EDEA240000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'LogWarning'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA8640000023549444154384FC5905D
              48936118869F0D126309265224B994C2A11E696412520BB29D763289827E8E42
              90828C0C8408B79881983FB31535CA22C93048CA5C3513C91F3469D8AC70CEAD
              39F723DBDCBE4FF72D75EEEE93DEECA0DF93E882FBE47D9EEB7EE0A57FC6C855
              52F41BE463FD3772A7076A49C59EFF1EAB31A58FF79B20CC7662E0569E775C4F
              1BD8E8CF386E93DA3D7406BCE72E7857137CE355E8D1D26536FE3DCE5E4A1EBD
              93E914BC46581F17C0F2488125CF298CB6E60B433ACA626BBFE6BD81AAFDD61A
              70965248933622353D1F9EE1ED083BD5306B24ED6CEDE7B8DA2863AC63E77CC4
              AD87F0AE18A969D9C85114C3F6360B51FF7E583AB6257A3554C2D67FC4729D5A
              03133A04EDD5805F85BD7B766057D13E603953FC8F7C0873D978A8958E8048C2
              94EF4CDCA4A20F5DAA95B0532CB0554098398CFADA0C9C3DA74482CFC1FCEC66
              7CE665B00D4A71EF021D67DA1A92372DB241CEA517E5F3084E9E44E4D32104BC
              8598B21F442CB849BC9E84C528012074EA68E67925C9984BF44A4347A75E9723
              E2B822CAA711B21FC182FB009E35A7C0644C433C9A8CC5054222FEB5606E86D0
              728C6A984ED4D3209F98F7DD4768F2A258508E90438D15772194BBE5282E2915
              A5F5882F8B05A2BC5AB09A2775C4319DA8EF5A813F166803E7D2829BAE00E72E
              8310506289DB82582C4F14246BE2B7745C221FD389DA2BA9CCDC90EBE9D1E7F1
              E66639FFB2792BFFA2295D8C8C3735AE0B773752B8BB81C25DF5147A5A47C107
              55F4D170824A99FE5F21FA02723E577ED8B7FDA10000000049454E44AE426082}
          end>
      end
      item
        Name = 'LogInfo'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA8640000028949444154384FAD925B
              4894511485573D84D5839A2644E08B15153E58D255A3870A5363CCD41429B4A1
              C87B592A5851444AF520269497B0A2890845CB8CA871BCA199A169A58E8E9379
              0D4ACB199051D3D159EDD1BF17C3A75AB03987FFDFDFD96BEF73F0FF1556EF8E
              90AAACE5A71A0DAE49ADD655894DD30E91B5ED087C9E81035A37256B11A9BBA2
              9DD33A2DFEE5D354D5909E45B35C737782AB73CC74BCD2CBA54775268436AA94
              EC0552F7447B64F6DA4E34901B4A499CEDA663520B1DD3F45C996E242E0E11E9
              FD4484CE0A5543A042290A7BEFEE9CD26151BF255D3402E7CB21173ED066EAA3
              D53CC492E6AF44DA80440F71AE9B08281BC1DE674E0A2D0AAECC0A289DA067B1
              8DC89D95B0D123C348EBD80827C7CC2CEA1C17789048112767F4C4B13A62CFA3
              5485061CA2EA0C91153344B624DE9A947592EBB306C9190BC95996CB16A9E220
              B98B486823D4EF88DD9A5A05075C639BADBE4F26881B66E2A6C4751337DEF921
              B05582D47E930392BFCC578FFF441CAF17078F87141C70897933B5B96094B8FA
              9DB8363CB7BAE55978DB4066EBC9938DF6033ECFC331AD447825B14B33A8E0D2
              4284AE7D5D8E8097A4CFB99089271AE994D044AFCC4E22CE40C40A7CBA65DE7E
              D04B625B6185828BFC4A33D65E96E9A6894DFBA4537AE89FD1444EFD9406C6A9
              37C97013E5BF1D0EAD20F617135B73E3155A242F6C5988D6B4E4BC24CD0D4ACF
              C3F94681ED432407A6A58504F91E5E457991C4F67B03F02E58A1D08A026B5538
              F2CA8AA40EB1FC51ECB67153DE2883CA7EC90DF411C1AFEDF72FD37F380EEF12
              1F855AA0835501F07B3A8C886A22AA412AD608A8250EBD20F61549E542A9BC18
              FC47DE3A47F814A660E7FD6AF86A06B0E3413FB6E469E19517F7B7ED7F16F01B
              07F48FE8A4285C350000000049454E44AE426082}
          end>
      end
      item
        Name = 'LogDebug1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA8640000023D49444154384FB5925D
              4853611CC6FF6EB9F6D19A4E639DD5729BE9C6716E16A99B99221E694DA77317
              350559B14082BA338222BA2B88208ABAAB46575D741B0446D027A47521AB8B12
              C9BE48CAB2868BFCD8F1E97F6A4936A39BFAC18F17CEFB3CBCE7FDA0FF46AF7D
              6DD97AAB758F60294D04D7AC2ECF7DFE3BC7885401AFE76447A46B3ED6D383E0
              8E10448F47B697169F6B265A918BE5A16187D926B1C6733A1C8BA12F1E477734
              8A564942BDDF0FBBCB0D8D467381337EF631AB6717296093929A2E5EAAB2C9F5
              1D5D08B6B7A3B5AD0D818606787C3530965760C0AC5F886829C9D9CBAC4A29FE
              8A6AA2AAE0FA7C4444B7594059AD1F8EBA0004DF66689D2E34694D982B278C08
              744BC9FEA8FC86DC624AE1603370B41B4F4201241D1624AD4518158B81660350
              4BF862A3F15C7C09CA160439A09FC2EE6AE0C036E0C87664FA5B307DE63090F0
              018D2B31EB254C5A6996B3CAAD28E7B688722877646FE1243AADC0CE0AA04F44
              E6CA7964D353C04018D94D84CF22E1A54069CE0EB22F94E24FC2AC4376D1356C
              55F3EF1A81B622BCDF62C0D7FB37807DD2F7F2DB4AC298996E735660834A7109
              0B368ACBD584AC8F005EF10D17327707319D90F07A23E1F906424A47FB73F17C
              7444EB6E5A682EED267C60DF9D3884F94F1F31937A88F14627AE1A492E2172E7
              E2CB7236A6A3CE67167A356AE72BAB7322159530B64BC23D134DF4AB29C419E5
              312D8B8DCDB09EB4914A9E9AE8F8C82A1A7A64A0E1073A3A9522B2F09CB2FA0C
              5BC9E6A1BCF3BD2CEFE48F14B2BDB9F15F40F40D60AAE463A6F8358B00000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'LogDebug2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA8640000025549444154384FB5925F
              4853511CC78FA2DB75E286BAE9FEB9A957B7E5AE6EE9DADCD2C54AC9E6DC6044
              530B471A51E0830F8189415131C80469448854520F412F2918153D94891213ED
              A1E8A53F1841F61008450F59AC6FBF9B4B32F7D0437DE0C3E5DEF3391CCEB987
              FD17D46EB551ABD71E54162B7BE4B5723EF5F9AFC814DCC2502814FAD611E980
              7FB71F429590CCD7E55FA4B1ACB5643312729EF40A75C248FBDE7644BBA20887
              C3686A6A42BDAB1E16DE0249AEE43235F5E4335246AE93418E339E5D511FD127
              03DE205A5B5AD1BCAB191EB707B61A3BCA55159004B8EFAC863AC6AE9199E2C4
              DFC9CC8B67DF3DB6DC02B94B855ADE0587C9891AA31D06790598854370D604F9
              90745A6CD7A6FC81E749D9D3EB388C311CC5BE7B3B613E5582B2C122EC9834A1
              6F693BFA5EBA503955B894CA37206E41D3F8985F19F914C185D503187EDD09DF
              40159ACF6F416CA50D7DCF3DE85EB4C13C51B84AADF857C4735B473C9419C71D
              FD87D3CB7B70F25D00CEAE526CEB2D45AE468AAE5BB5E849D8D19EB0A2FCA6E2
              23B5F7C937E2C45FB49165C609C5EDFE170DB45A03FA5FF9109EAC8682E7D031
              2520326B4570CE0CD555D9236A35648B387123A32CBA7FA11ADD0B5B117D6883
              D22643DD392D227355F04F57A271BA04EC2CEB4DD569D1159CE1BE4616ADC837
              73E08AB32017A4F0DE30C03B6380623027493BB7A4DAB4C4998305D5E3796F9D
              633A549E2880F1B802EE073A148D72EF998FF9A9112F535A4AC8CFA4C062AC30
              6398C51497A409595C329F1163C36C8015D398B8FA17D2446E42BCE787C89C9F
              6FE9C9263B53CF7F05633F00E8E4CCE821C44C760000000049454E44AE426082}
          end>
      end
      item
        Name = 'LogDebug3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA8640000023D49444154384FB5926B
              4893511CC69F464AB3E80E362FADB99AE286AD1BFA0649EA9265E562F4615A39
              56890CA22F158445B7619448A958485EA654B294FC10666058165DCC20731241
              50AB88FA9048821FBAD9D3FFC52599067DA81FFC78E13DCF73CE7BCE7BF0DFC8
              5016E8E3E2623CBAE8F93B979B671AC3AFFF0A4DBA622975381C5F5DAE3CAEB7
              E7D06CB68C2C8A9D5325635347231389147BC474EB8A9433CE2D2E1614B8E974
              3A9965B33135358DC6C549D46A236B259326F68B51E2185344BF350175A78AE2
              4794B59B69B76F90F23A2ACA6A5A52AC9C1DBD8485D951DF1523FC926D14356A
              F15734ED25B8CEFB1174AE5948BD298D8644853186959C3E379959D6597CEE07
              6B8AD0A566472BBFF1F12A820C4D23DF79F8B4C5CEC662031BF66918AC0107DA
              C01701B0D38750383E0E750BBAB7CD18FCD60D3218C5C17E137D5E2D9F5D0287
              BBA47C197C2C13B51FC467C9AA7F453DB731D443B9D3770E1F063AC0A14EF0E2
              7130CF0666AF025FB7824FEAC00755E095BD18926C87F84A2DFE64936878548E
              B697CD60A805BC5B0D26EB416F2ED8DF00DEAB046F9581170A715BB23AD1AE16
              C771F330DCBDE7C1DE5AB0AF1E3CED05676865B20AF0C649F9FCA360B913BBC3
              F1896881D80A0FBE74CB6AF999E03223B834418A3EF0DA31F0D0468CC8C693C2
              F149A9CC48446ED31EBC091C008FE48181FDB2EF62F0EC36BCB725204732EA65
              9A94787158B4B46EC7BCEAAD3851BF030F6BDDE829CD45594926A2654C5DFD93
              681227A0DEF35DA2ECE48F4488F9E1E7BF00F8016BC7DD533B1DBAD700000000
              49454E44AE426082}
          end>
      end>
    Left = 364
    Top = 224
  end
end
