object frameLogs: TframeLogs
  Left = 0
  Top = 0
  Width = 1094
  Height = 497
  TabOrder = 0
  object pcLogs: TPageControl
    Left = 0
    Top = 0
    Width = 1094
    Height = 497
    Align = alClient
    PopupMenu = popTabs
    TabOrder = 0
    Visible = False
    OnChange = pcLogsChange
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 56
    Top = 112
    object actShowDateColumn: TAction
      AutoCheck = True
      Caption = 'Show Date'
      OnExecute = actShowDateColumnExecute
    end
    object actSaveLog: TAction
      Caption = 'Save Log'
      OnExecute = actSaveLogExecute
    end
    object actCopyLog: TAction
      Caption = 'Copy Log'
      OnExecute = actCopyLogExecute
    end
    object actClear: TAction
      Caption = '&Clear Log'
      OnExecute = actClearExecute
    end
    object actCopy: TAction
      Caption = '&Copy Selected'
      OnExecute = actCopyExecute
    end
    object actSaveSelected: TAction
      Caption = 'Save Selected'
      OnExecute = actSaveSelectedExecute
    end
    object actPauseLogging: TAction
      Caption = 'Pause Logging'
      OnExecute = actPauseLoggingExecute
    end
    object actCloseTab: TAction
      Category = 'Tabs'
      Caption = 'Close Tab'
      OnExecute = actCloseTabExecute
    end
    object actCloseAllTabs: TAction
      Category = 'Tabs'
      Caption = 'Close All Tabs'
      OnExecute = actCloseAllTabsExecute
    end
    object actCloseTabsToTheRight: TAction
      Category = 'Tabs'
      Caption = 'Close Tabs to the Right'
      OnExecute = actCloseTabsToTheRightExecute
    end
  end
  object popLog: TPopupMenu
    OnPopup = popLogPopup
    Left = 168
    Top = 160
    object MaximumLogEntries1: TMenuItem
      Caption = 'Maximum Log Entries'
      object NoLimit1: TMenuItem
        AutoCheck = True
        Caption = 'No Limit'
        GroupIndex = 10
        RadioItem = True
        OnClick = N100000001Click
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 10
      end
      object N10001: TMenuItem
        Tag = 1000
        AutoCheck = True
        Caption = '1,000'
        GroupIndex = 10
        RadioItem = True
        OnClick = N100000001Click
      end
      object N100001: TMenuItem
        Tag = 10000
        AutoCheck = True
        Caption = '10,000'
        GroupIndex = 10
        RadioItem = True
        OnClick = N100000001Click
      end
      object N1000001: TMenuItem
        Tag = 100000
        AutoCheck = True
        Caption = '100,000'
        Checked = True
        GroupIndex = 10
        RadioItem = True
        OnClick = N100000001Click
      end
      object N5000001: TMenuItem
        Tag = 500000
        Caption = '500,000'
        GroupIndex = 10
        RadioItem = True
        OnClick = N100000001Click
      end
      object N10000001: TMenuItem
        Tag = 1000000
        AutoCheck = True
        Caption = '1,000,000'
        GroupIndex = 10
        RadioItem = True
        OnClick = N100000001Click
      end
      object N100000001: TMenuItem
        Tag = 10000000
        Caption = '10,000,000'
        GroupIndex = 10
        RadioItem = True
        OnClick = N100000001Click
      end
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object MenuItem9: TMenuItem
      Action = actSaveLog
      ShortCut = 49235
    end
    object SaveSelected1: TMenuItem
      Action = actSaveSelected
      ShortCut = 16467
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object MenuItem10: TMenuItem
      Action = actCopyLog
      ShortCut = 49219
    end
    object Copyline1: TMenuItem
      Action = actCopy
      ShortCut = 16451
    end
    object MenuItem11: TMenuItem
      Caption = '-'
    end
    object PauseLogging1: TMenuItem
      Action = actPauseLogging
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Clearlog1: TMenuItem
      Action = actClear
    end
    object MenuItem13: TMenuItem
      Action = actShowDateColumn
      AutoCheck = True
    end
  end
  object popTabs: TPopupMenu
    Left = 256
    Top = 160
    object CloseTab1: TMenuItem
      Action = actCloseTab
    end
    object CloseAllTabs1: TMenuItem
      Action = actCloseAllTabs
    end
    object CloseTabstotheRight1: TMenuItem
      Action = actCloseTabsToTheRight
    end
  end
end
