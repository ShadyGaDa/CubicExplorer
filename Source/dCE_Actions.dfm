object CEActions: TCEActions
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 305
  Width = 385
  object ActionList: TActionList
    Left = 28
    Top = 12
    object act_workspace_forward: TAction
      Tag = 933
      Category = 'Workspace'
      Caption = 'Forward'
      ImageIndex = 6
      OnExecute = ActionExecute
    end
    object act_tabs_closetab: TAction
      Tag = 661
      Category = 'Tabs'
      Caption = 'Close Tab'
      Hint = 'Close Tab'
      ImageIndex = 10
      SecondaryShortCuts.Strings = (
        'Ctrl+F4')
      ShortCut = 16471
      OnExecute = ActionExecute
    end
    object act_gen_exit: TAction
      Tag = 100
      Category = 'General'
      Caption = 'Exit'
      ImageIndex = 0
      ShortCut = 32883
      OnExecute = ActionExecute
    end
    object act_view_dropstack: TAction
      Tag = 305
      Category = 'View'
      Caption = 'Stack'
      ImageIndex = 43
      ShortCut = 16437
      OnExecute = ActionExecute
    end
    object act_help_donate: TAction
      Tag = 505
      Category = 'Help'
      Caption = 'Donate'
      ImageIndex = 47
      OnExecute = ActionExecute
    end
    object act_navi_forward: TCEToolbarAction
      Tag = 604
      Category = 'Navigation'
      Caption = 'Forward'
      ImageIndex = 6
      ShortCut = 32807
      OnExecute = ActionExecute
    end
    object act_navi_back: TCEToolbarAction
      Tag = 603
      Category = 'Navigation'
      Caption = 'Back'
      ImageIndex = 5
      ShortCut = 32805
      OnExecute = ActionExecute
    end
    object act_navi_folderup: TAction
      Tag = 605
      Category = 'Navigation'
      Caption = 'Folder Up'
      ImageIndex = 7
      OnExecute = ActionExecute
    end
    object act_navi_refresh: TAction
      Tag = 606
      Category = 'Navigation'
      Caption = 'Refresh'
      ImageIndex = 8
      ShortCut = 116
      OnExecute = ActionExecute
    end
    object act_view_folders: TAction
      Tag = 301
      Category = 'View'
      Caption = 'Folders'
      ImageIndex = 28
      ShortCut = 16433
      OnExecute = ActionExecute
    end
    object act_edit_undo_delete: TCEToolbarAction
      Tag = 215
      Category = 'Edit'
      Caption = 'Undo Delete'
      ImageIndex = 55
      OnExecute = ActionExecute
    end
    object act_view_bookmark: TAction
      Tag = 302
      Category = 'View'
      Caption = 'Bookmarks'
      ImageIndex = 18
      ShortCut = 16434
      OnExecute = ActionExecute
    end
    object act_view_large: TAction
      Tag = 351
      Category = 'View'
      Caption = 'Large Icons'
      ImageIndex = 11
      OnExecute = ActionExecute
    end
    object act_view_small: TAction
      Tag = 352
      Category = 'View'
      Caption = 'Small Icons'
      ImageIndex = 12
      OnExecute = ActionExecute
    end
    object act_view_list: TAction
      Tag = 353
      Category = 'View'
      Caption = 'List'
      ImageIndex = 13
      OnExecute = ActionExecute
    end
    object act_view_showheaderalways: TAction
      Tag = 333
      Category = 'View'
      Caption = 'Always Show Sort Columns'
      OnExecute = ActionExecute
    end
    object act_view_filters: TAction
      Tag = 304
      Category = 'View'
      Caption = 'Filters'
      ImageIndex = 29
      ShortCut = 16436
      OnExecute = ActionExecute
    end
    object act_view_details: TAction
      Tag = 354
      Category = 'View'
      Caption = 'Details'
      ImageIndex = 14
      OnExecute = ActionExecute
    end
    object act_view_thumbs: TAction
      Tag = 355
      Category = 'View'
      Caption = 'Thumbnails'
      ImageIndex = 16
      OnExecute = ActionExecute
    end
    object act_view_loadskin: TAction
      Tag = 371
      Category = 'View'
      Caption = 'Load theme from file...'
      Hint = 'Load theme from file'
      OnExecute = ActionExecute
    end
    object act_view_tiles: TAction
      Tag = 356
      Category = 'View'
      Caption = 'Tiles'
      ImageIndex = 15
      OnExecute = ActionExecute
    end
    object act_view_alwaysontop: TAction
      Tag = 335
      Category = 'View'
      Caption = 'Always On Top'
      ImageIndex = 60
      OnExecute = ActionExecute
    end
    object act_view_groupby: TCEToolbarAction
      Tag = 374
      Category = 'View'
      Caption = 'Group By'
      ImageIndex = 31
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_view_viewstyle: TCEToolbarAction
      Tag = 373
      Category = 'View'
      Caption = 'View Style'
      ImageIndex = 16
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_view_filmstrip: TAction
      Tag = 358
      Category = 'View'
      Caption = 'Filmstrip'
      ImageIndex = 17
      OnExecute = ActionExecute
    end
    object act_view_checkbox_selection: TAction
      Tag = 337
      Category = 'View'
      Caption = 'Checkbox Selection'
      ImageIndex = 56
      OnExecute = ActionExecute
    end
    object act_edit_copy: TAction
      Tag = 201
      Category = 'Edit'
      Caption = 'Copy'
      ImageIndex = 1
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      ShortCut = 16451
      OnExecute = ActionExecute
    end
    object act_view_workspace: TAction
      Tag = 307
      Category = 'View'
      Caption = 'Workspace'
      ImageIndex = 72
      ShortCut = 16438
      OnExecute = ActionExecute
    end
    object act_edit_cut: TAction
      Tag = 202
      Category = 'Edit'
      Caption = 'Cut'
      ImageIndex = 2
      ShortCut = 16472
      OnExecute = ActionExecute
    end
    object act_edit_paste: TAction
      Tag = 203
      Category = 'Edit'
      Caption = 'Paste'
      ImageIndex = 3
      SecondaryShortCuts.Strings = (
        'Shift+Ins')
      ShortCut = 16470
      OnExecute = ActionExecute
    end
    object act_edit_delete: TAction
      Tag = 204
      Category = 'Edit'
      Caption = 'Delete'
      ImageIndex = 4
      ShortCut = 46
      OnExecute = ActionExecute
    end
    object act_edit_selall: TAction
      Tag = 205
      Category = 'Edit'
      Caption = 'Select All'
      ImageIndex = 57
      ShortCut = 16449
      OnExecute = ActionExecute
    end
    object act_edit_invertsel: TAction
      Tag = 206
      Category = 'Edit'
      Caption = 'Invert Selection'
      ImageIndex = 58
      ShortCut = 24641
      OnExecute = ActionExecute
    end
    object act_edit_properties: TAction
      Tag = 207
      Category = 'Edit'
      Caption = 'Properties...'
      ImageIndex = 26
      ShortCut = 32781
      OnExecute = ActionExecute
    end
    object act_edit_rename: TAction
      Tag = 208
      Category = 'Edit'
      Caption = 'Rename'
      ImageIndex = 38
      ShortCut = 113
      OnExecute = ActionExecute
    end
    object act_help_home: TAction
      Tag = 501
      Category = 'Help'
      Caption = 'CubicExplorer Home Page'
      ImageIndex = 47
      OnExecute = ActionExecute
    end
    object act_help_forum: TAction
      Tag = 502
      Category = 'Help'
      Caption = 'Support Forum'
      ImageIndex = 47
      OnExecute = ActionExecute
    end
    object act_help_about: TAction
      Tag = 503
      Category = 'Help'
      Caption = 'About...'
      ImageIndex = 48
      OnExecute = ActionExecute
    end
    object act_view_quickview: TAction
      Tag = 303
      Category = 'View'
      Caption = 'Quickview'
      ImageIndex = 20
      ShortCut = 16435
      OnExecute = ActionExecute
    end
    object act_edit_duplicate: TAction
      Tag = 209
      Category = 'Edit'
      Caption = 'Duplicate'
      ImageIndex = 37
      ShortCut = 16452
      OnExecute = ActionExecute
    end
    object act_navi_texteditor: TAction
      Tag = 650
      Category = 'Navigation'
      Caption = 'Text Editor'
      ImageIndex = 21
      ShortCut = 118
      OnExecute = ActionExecute
    end
    object act_navi_filesearch: TAction
      Tag = 651
      Category = 'Navigation'
      Caption = 'File Search'
      ImageIndex = 22
      ShortCut = 16454
      OnExecute = ActionExecute
    end
    object act_tools_mapdrive: TAction
      Tag = 451
      Category = 'Tools'
      Caption = 'Map Network Drive...'
      OnExecute = ActionExecute
    end
    object act_tools_disconnectdrive: TAction
      Tag = 452
      Category = 'Tools'
      Caption = 'Disconnect Network Drive...'
      OnExecute = ActionExecute
    end
    object act_edit_newfile: TCEToolbarAction
      Tag = 210
      Category = 'Edit'
      Caption = 'New'
      ImageIndex = 36
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_edit_copypath: TCEToolbarAction
      Tag = 211
      Category = 'Edit'
      Caption = 'Copy Path'
      ImageIndex = 50
      ShortCut = 16466
      OnExecute = ActionExecute
    end
    object act_view_showhints: TAction
      Tag = 330
      Category = 'View'
      Caption = 'Show Hints'
      OnExecute = ActionExecute
    end
    object act_tools_showcustomizer: TAction
      Tag = 401
      Category = 'Tools'
      Caption = 'Customizer...'
      ImageIndex = 40
      OnExecute = ActionExecute
    end
    object act_view_hiddenfiles: TAction
      Tag = 332
      Category = 'View'
      Caption = 'Show Hidden Files'
      ImageIndex = 53
      OnExecute = ActionExecute
    end
    object act_edit_newfolder: TAction
      Tag = 212
      Category = 'Edit'
      Caption = 'New Folder'
      ImageIndex = 25
      ShortCut = 16462
      OnExecute = ActionExecute
    end
    object act_edit_paste_shortcut: TAction
      Tag = 213
      Category = 'Edit'
      Caption = 'Paste Shortcut'
      OnExecute = ActionExecute
    end
    object act_tools_cmd: TAction
      Tag = 454
      Category = 'Tools'
      Caption = 'Open Command Prompt'
      ImageIndex = 27
      OnExecute = ActionExecute
    end
    object act_view_statusbar: TAction
      Tag = 300
      Category = 'View'
      Caption = 'Status Bar'
      OnExecute = ActionExecute
    end
    object act_view_fullscreen: TAction
      Tag = 370
      Category = 'View'
      Caption = 'Fullscreen'
      ImageIndex = 51
      ShortCut = 122
      OnExecute = ActionExecute
    end
    object act_view_showextensions: TAction
      Tag = 334
      Category = 'View'
      Caption = 'Show Extensions'
      ImageIndex = 54
      OnExecute = ActionExecute
    end
    object act_help_poedit_form: TAction
      Tag = 504
      Category = 'Help'
      Caption = 'Translate CubicExplorer'
      ImageIndex = 52
      OnExecute = ActionExecute
    end
    object act_tools_showoptions: TAction
      Tag = 402
      Category = 'Tools'
      Caption = 'Options...'
      ImageIndex = 39
      OnExecute = ActionExecute
    end
    object act_sessions_save: TAction
      Tag = 851
      Category = 'Sessions'
      Caption = 'Save...'
      Hint = 'Save active session.'
      OnExecute = ActionExecute
    end
    object act_sessions_manage: TAction
      Tag = 852
      Category = 'Sessions'
      Caption = 'Manage...'
      Hint = 'Show Session Manager'
      OnExecute = ActionExecute
    end
    object act_view_arrangeby: TCEToolbarAction
      Tag = 372
      Category = 'View'
      Caption = 'Arrange By'
      ImageIndex = 30
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_navi_scrollleft: TAction
      Tag = 608
      Category = 'Navigation'
      Caption = 'Scroll Left'
      OnExecute = ActionExecute
    end
    object act_navi_scrollright: TAction
      Tag = 609
      Category = 'Navigation'
      Caption = 'Scroll Right'
      OnExecute = ActionExecute
    end
    object act_tabs_closeothertabs: TAction
      Tag = 662
      Category = 'Tabs'
      Caption = 'Close Other Tabs'
      Hint = 'Close Other Tabs'
      ImageIndex = 34
      OnExecute = ActionExecute
    end
    object act_tabs_addtab: TAction
      Tag = 663
      Category = 'Tabs'
      Caption = 'Add Tab'
      Hint = 'Add Tab'
      ImageIndex = 9
      ShortCut = 16468
      OnExecute = ActionExecute
    end
    object act_tabs_duplicatetab: TAction
      Tag = 664
      Category = 'Tabs'
      Caption = 'Duplicate Tab'
      Hint = 'Duplicate Tab'
      ImageIndex = 32
      OnExecute = ActionExecute
    end
    object act_tabs_closeonleft: TAction
      Tag = 665
      Category = 'Tabs'
      Caption = 'Close Tabs on Left'
      Hint = 'Close Tabs on Left'
      ImageIndex = 35
      OnExecute = ActionExecute
    end
    object act_tabs_closeonright: TAction
      Tag = 666
      Category = 'Tabs'
      Caption = 'Close Tabs on Right'
      Hint = 'Close Tabs on Right'
      ImageIndex = 33
      OnExecute = ActionExecute
    end
    object act_gen_menu: TCEToolbarAction
      Tag = -1
      Category = 'General'
      Caption = 'Menu'
      ImageIndex = 49
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_navi_quickview: TAction
      Tag = 652
      Category = 'Navigation'
      Caption = 'QuickView'
      ImageIndex = 20
      ShortCut = 119
      OnExecute = ActionExecute
    end
    object act_sessions_addhistoryitem: TAction
      Tag = 853
      Category = 'Sessions'
      Caption = 'Add History Item'
      Hint = 'Show Session Manager'
      OnExecute = ActionExecute
    end
    object act_sessions_clearhistory: TAction
      Tag = 854
      Category = 'Sessions'
      Caption = 'Clear History'
      Hint = 'Show Session Manager'
      OnExecute = ActionExecute
    end
    object act_bookmarks_menu: TCEToolbarAction
      Tag = -1
      Category = 'Bookmarks'
      Caption = 'Bookmarks'
      ImageIndex = 18
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_sessions_menu: TCEToolbarAction
      Tag = -1
      Category = 'Sessions'
      Caption = 'Sessions'
      ImageIndex = 41
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_view_infobar: TAction
      Tag = 336
      Category = 'View'
      Caption = 'Info Bar'
      ImageIndex = 66
      OnExecute = ActionExecute
    end
    object act_edit_create_symlink: TAction
      Tag = 214
      Category = 'Edit'
      Caption = 'Create Symbolic Link'
      OnExecute = ActionExecute
    end
    object act_sessions_enablehistory: TAction
      Tag = 855
      Category = 'Sessions'
      Caption = 'Auto Save History'
      Hint = 'Show Session Manager'
      OnExecute = ActionExecute
    end
    object act_gen_showhide: TAction
      Tag = 101
      Category = 'General'
      Caption = 'Show/Hide'
      OnExecute = ActionExecute
    end
    object act_tabs_undo: TCEToolbarAction
      Tag = 667
      Category = 'Tabs'
      Caption = 'Undo Tab Close'
      Hint = 'Undo Tab Close'
      ImageIndex = 42
      ShortCut = 24660
      OnExecute = ActionExecute
    end
    object act_tabs_next: TAction
      Tag = 668
      Category = 'Tabs'
      Caption = 'Switch to Next Tab'
      SecondaryShortCuts.Strings = (
        'Ctrl+PgDn')
      ShortCut = 16393
      OnExecute = ActionExecute
    end
    object act_tabs_prev: TAction
      Tag = 669
      Category = 'Tabs'
      Caption = 'Swith to Previous Tab'
      SecondaryShortCuts.Strings = (
        'Ctrl+PgUp')
      ShortCut = 24585
      OnExecute = ActionExecute
    end
    object act_focus_addressbar: TAction
      Tag = 951
      Category = 'Focus'
      Caption = 'Set focus to address bar'
      SecondaryShortCuts.Strings = (
        'F4')
      ShortCut = 32836
      OnExecute = ActionExecute
    end
    object act_filters_menu: TCEToolbarAction
      Tag = 375
      Category = 'Filters'
      Caption = 'Filters'
      ImageIndex = 29
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_filters_pattern: TCEToolbarAction
      Category = 'Filters'
      Caption = 'Text Filter'
      ImageIndex = 29
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_filters_clear: TAction
      Tag = 901
      Category = 'Filters'
      Caption = 'Clear Filters'
      ImageIndex = 44
      OnExecute = ActionExecute
    end
    object act_help_versionmgr: TAction
      Tag = 506
      Category = 'Help'
      Caption = 'Version Manager'
      ImageIndex = 46
      OnExecute = ActionExecute
    end
    object act_view_archiver: TAction
      Tag = 306
      Category = 'View'
      Caption = 'Archiver'
      Visible = False
      OnExecute = ActionExecute
    end
    object act_gen_new_instance: TAction
      Tag = 103
      Category = 'General'
      Caption = 'New Window'
      ImageIndex = 61
      OnExecute = ActionExecute
    end
    object act_help_checkupdates: TAction
      Tag = 507
      Category = 'Help'
      Caption = 'Check For Updates'
      ImageIndex = 45
      OnExecute = ActionExecute
    end
    object act_tools_emptytrash: TAction
      Tag = 453
      Category = 'Tools'
      Caption = 'Empty Recycle Bin'
      ImageIndex = 24
      OnExecute = ActionExecute
    end
    object act_tools_systempower: TCEToolbarAction
      Tag = 455
      Category = 'Tools'
      Caption = 'System Power'
      Hint = 'System Power'
      ImageIndex = 59
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_view_lock_toolbars: TAction
      Tag = 390
      Category = 'View'
      Caption = 'Lock Toolbars'
      ImageIndex = 76
      OnExecute = ActionExecute
    end
    object act_view_lock_panels: TAction
      Tag = 391
      Category = 'View'
      Caption = 'Lock Panels'
      ImageIndex = 74
      OnExecute = ActionExecute
    end
    object act_navi_refresh_current: TAction
      Tag = 610
      Category = 'Navigation'
      Caption = 'Refresh Fileview'
      ImageIndex = 62
      ShortCut = 8308
      OnExecute = ActionExecute
    end
    object act_filters_strict: TAction
      Tag = 902
      Category = 'Filters'
      Caption = 'Strict'
      Hint = 'Use strict filtering (wildcards are ? and *)'
      ImageIndex = 63
      OnExecute = ActionExecute
    end
    object act_filters_exclude: TAction
      Tag = 903
      Category = 'Filters'
      Caption = 'Exclude'
      Hint = 'Exclude'
      ImageIndex = 64
      OnExecute = ActionExecute
    end
    object act_help_restore_layout: TAction
      Tag = 508
      Category = 'Help'
      Caption = 'Restore Default Layout'
      OnExecute = ActionExecute
    end
    object act_tabs_menu: TCEToolbarAction
      Tag = 670
      Category = 'Tabs'
      Caption = 'Tabs'
      ImageIndex = 67
      OnExecute = ActionExecute
    end
    object act_edit_newemptyfile: TAction
      Tag = 217
      Category = 'Edit'
      Caption = 'New File'
      ImageIndex = 36
      ShortCut = 24654
      OnExecute = ActionExecute
    end
    object act_stack_open: TCEToolbarAction
      Tag = 921
      Category = 'Stack'
      Caption = 'Open'
      ImageIndex = 68
      OnExecute = ActionExecute
    end
    object act_stack_save: TCEToolbarAction
      Tag = 922
      Category = 'Stack'
      Caption = 'Save'
      ImageIndex = 69
      OnExecute = ActionExecute
    end
    object act_stack_remove: TAction
      Tag = 923
      Category = 'Stack'
      Caption = 'Remove from Stack'
      ImageIndex = 4
      OnExecute = ActionExecute
    end
    object act_stack_clear: TAction
      Tag = 924
      Category = 'Stack'
      Caption = 'Clear List'
      ImageIndex = 44
      OnExecute = ActionExecute
    end
    object act_stack_allowmove: TAction
      Tag = 925
      Category = 'Stack'
      Caption = 'Allow Move'
      ImageIndex = 70
      OnExecute = ActionExecute
    end
    object act_gen_makevisible: TAction
      Tag = 104
      Category = 'General'
      Caption = 'Make Visible'
      OnExecute = ActionExecute
    end
    object act_workspace_folder_up: TAction
      Tag = 931
      Category = 'Workspace'
      Caption = 'Folder Up'
      ImageIndex = 7
      OnExecute = ActionExecute
    end
    object act_workspace_back: TAction
      Tag = 932
      Category = 'Workspace'
      Caption = 'Back'
      ImageIndex = 5
      OnExecute = ActionExecute
    end
    object act_workspace_open: TAction
      Tag = 934
      Category = 'Workspace'
      Caption = 'Browse To'
      ImageIndex = 28
      ShortCut = 16453
      OnExecute = ActionExecute
    end
    object act_tabs_copyhere: TAction
      Tag = 671
      Category = 'Tabs'
      Caption = 'Copy Selected Here'
      Hint = 'Copy Selected Here'
      ImageIndex = 1
      Visible = False
      OnExecute = ActionExecute
    end
    object act_tabs_movehere: TAction
      Tag = 672
      Category = 'Tabs'
      Caption = 'Move Selected Here'
      Hint = 'Move Selected Here'
      ImageIndex = 2
      Visible = False
      OnExecute = ActionExecute
    end
  end
  object UpdateTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = UpdateTimerTimer
    Left = 92
    Top = 12
  end
  object ApplicationEvents: TApplicationEvents
    OnActivate = ApplicationEventsActivate
    OnMessage = ApplicationEventsMessage
    Left = 180
    Top = 12
  end
  object BackgroundCMItems_up: TPopupMenu
    OnPopup = BackgroundCMItems_upPopup
    Left = 64
    Top = 104
    object View1: TMenuItem
      Caption = 'View'
      object LargeIcons1: TMenuItem
        Action = act_view_large
        RadioItem = True
      end
      object SmallIcons1: TMenuItem
        Action = act_view_small
        RadioItem = True
      end
      object List1: TMenuItem
        Action = act_view_list
        RadioItem = True
      end
      object Details1: TMenuItem
        Action = act_view_details
        RadioItem = True
      end
      object Files1: TMenuItem
        Action = act_view_tiles
        RadioItem = True
      end
      object Thumbnails1: TMenuItem
        Action = act_view_thumbs
        RadioItem = True
      end
      object Filmstrip1: TMenuItem
        Action = act_view_filmstrip
        RadioItem = True
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItem_ArragneBy: TMenuItem
      Caption = 'Arrange By'
    end
    object MenuItem_GroupBy: TMenuItem
      Caption = 'Group By'
    end
    object Refresh1: TMenuItem
      Action = act_navi_refresh
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Paste1: TMenuItem
      Action = act_edit_paste
    end
    object PasteShortcut1: TMenuItem
      Action = act_edit_paste_shortcut
    end
    object CopyPath1: TMenuItem
      Action = act_edit_copypath
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object CreateSymbolicLink1: TMenuItem
      Action = act_edit_create_symlink
    end
    object N5: TMenuItem
      Caption = '-'
    end
  end
  object BackgroundCMItems_down: TPopupMenu
    Left = 64
    Top = 168
    object NewFolder1: TMenuItem
      Action = act_edit_newfolder
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Properties1: TMenuItem
      Action = act_edit_properties
    end
  end
end
