 //******************************************************************************
//  CubicExplorer
//  Version: 0.90
//
//  The Original Code is dCE_Actions.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************

unit dCE_Actions;

interface

uses
//  TntSysUtils,
  // CE Units
  CE_GlobalCtrl,  CE_TBActions, CE_Utils,
//  CE_LanguageEngine,
  //CE_Engine,
  CE_SpTabBar, CE_AppSettings, CEJvDockVSNetStyleTBX,
  // PngControls

  // EasyListview & VSTools
  EasyListview, VirtualExplorerTree,
  MPCommonUtilities,
  MPShellUtilities,
  MPCommonObjects,
  VirtualExplorerEasyListview,
  //
  ActnList, Clipbrd, SysUtils, Windows, Classes, Menus,
  // JVCL
  JvDockControlForm,
  // SpTBX

  // GraphicEx

  // System Units
  ImgList, Controls, ExtCtrls, Forms,
  ShellAPI, AppEvnts, Messages, ShlObj, DOM, Contnrs, System.Actions;

const
  MK_XBUTTON1 = $20;
  MK_XBUTTON2 = $40;

  WM_SingleInstance = WM_USER + 1;
  WM_MakeVisible = WM_USER + 100;
  WM_ExecuteAction = WM_USER + 101;
  WM_AdminResult = WM_USER + 102;

{$REGION 'TYPES'}

  type
    TCustomVirtualExplorerEasyListviewHack = class(TCustomVirtualExplorerEasyListview);
    TDOMElementHack = class(TDOMElement);

    TCEHotkeySettings = class(TCECustomSettingStorage)
    private
      fActions: TActionList;
      fModifiedActions: TObjectList;
    protected
      property Actions: TActionList read fActions write fActions;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Load(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
      procedure Save(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
      property ModifiedActions: TObjectList read fModifiedActions;
    end;

    TCEGlobalHotkeys = class(TCECustomSettingStorage)
    private
      fActions: TActionList;
      fIsRegistered: Boolean;
      fMsgHandle: HWND;
      function GetCount: Integer;
    protected
      fHotkeyActions: TObjectList;
      fHotkeys: TList;
    public
      constructor Create;
      destructor Destroy; override;
      function AddHotkey(AAction: TAction; AHotkey: TShortcut): Integer;
      function CanRegister(AAction: TAction; AHotkey: TShortcut): Boolean;
      procedure Clear;
      procedure DeleteHotkey(Index: Integer);
      procedure ExecuteHotkey(id: Integer);
      function GetAction(AIndex: Integer): TAction;
      function GetHotkey(AIndex: Integer): TShortcut; overload;
      function GetHotkey(AAction: TAction): TShortcut; overload;
      function GetHotkeys(AAction: TAction; AResults: TStrings): Integer;
      function IndexOf(AAction: TAction): Integer;
      procedure Load(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
      procedure RegisterAll;
      procedure Replace(AIndex: Integer; AAction: TAction; AHotkey: TShortcut);
      procedure Save(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
      procedure UnRegisterAll;
      property Actions: TActionList read fActions write fActions;
      property Count: Integer read GetCount;
      property IsRegistered: Boolean read fIsRegistered;
      property MsgHandle: HWND read fMsgHandle write fMsgHandle;
    end;

    TCEActions = class(TDataModule)
      ActionList: TActionList;
      act_bookmarks_menu: TCEToolbarAction;
      act_edit_copy: TAction;
      act_edit_copypath: TCEToolbarAction;
      act_edit_create_symlink: TAction;
      act_edit_cut: TAction;
      act_edit_delete: TAction;
      act_edit_duplicate: TAction;
      act_edit_invertsel: TAction;
      act_edit_newemptyfile: TAction;
      act_edit_newfile: TCEToolbarAction;
      act_edit_newfolder: TAction;
      act_edit_paste: TAction;
      act_edit_paste_shortcut: TAction;
      act_edit_properties: TAction;
      act_edit_rename: TAction;
      act_edit_selall: TAction;
      act_edit_undo_delete: TCEToolbarAction;
      act_filters_clear: TAction;
      act_filters_exclude: TAction;
      act_filters_menu: TCEToolbarAction;
      act_filters_pattern: TCEToolbarAction;
      act_filters_strict: TAction;
      act_focus_addressbar: TAction;
      act_gen_exit: TAction;
      act_gen_makevisible: TAction;
      act_gen_menu: TCEToolbarAction;
      act_gen_new_instance: TAction;
      act_gen_showhide: TAction;
      act_help_about: TAction;
      act_help_checkupdates: TAction;
      act_help_donate: TAction;
      act_help_forum: TAction;
      act_help_home: TAction;
      act_help_poedit_form: TAction;
      act_help_restore_layout: TAction;
      act_help_versionmgr: TAction;
      act_navi_back: TCEToolbarAction;
      act_navi_filesearch: TAction;
      act_navi_folderup: TAction;
      act_navi_forward: TCEToolbarAction;
      act_navi_quickview: TAction;
      act_navi_refresh: TAction;
      act_navi_refresh_current: TAction;
      act_navi_scrollleft: TAction;
      act_navi_scrollright: TAction;
      act_navi_texteditor: TAction;
      act_sessions_addhistoryitem: TAction;
      act_sessions_clearhistory: TAction;
      act_sessions_enablehistory: TAction;
      act_sessions_manage: TAction;
      act_sessions_menu: TCEToolbarAction;
      act_sessions_save: TAction;
      act_stack_allowmove: TAction;
      act_stack_clear: TAction;
      act_stack_open: TCEToolbarAction;
      act_stack_remove: TAction;
      act_stack_save: TCEToolbarAction;
      act_tabs_addtab: TAction;
      act_tabs_closeonleft: TAction;
      act_tabs_closeonright: TAction;
      act_tabs_closeothertabs: TAction;
      act_tabs_closetab: TAction;
      act_tabs_copyhere: TAction;
      act_tabs_duplicatetab: TAction;
      act_tabs_menu: TCEToolbarAction;
      act_tabs_movehere: TAction;
      act_tabs_next: TAction;
      act_tabs_prev: TAction;
      act_tabs_undo: TCEToolbarAction;
      act_tools_cmd: TAction;
      act_tools_disconnectdrive: TAction;
      act_tools_emptytrash: TAction;
      act_tools_mapdrive: TAction;
      act_tools_showcustomizer: TAction;
      act_tools_showoptions: TAction;
      act_tools_systempower: TCEToolbarAction;
      act_view_alwaysontop: TAction;
      act_view_archiver: TAction;
      act_view_arrangeby: TCEToolbarAction;
      act_view_bookmark: TAction;
      act_view_checkbox_selection: TAction;
      act_view_details: TAction;
      act_view_dropstack: TAction;
      act_view_filmstrip: TAction;
      act_view_filters: TAction;
      act_view_folders: TAction;
      act_view_fullscreen: TAction;
      act_view_groupby: TCEToolbarAction;
      act_view_hiddenfiles: TAction;
      act_view_infobar: TAction;
      act_view_large: TAction;
      act_view_list: TAction;
      act_view_loadskin: TAction;
      act_view_lock_panels: TAction;
      act_view_lock_toolbars: TAction;
      act_view_quickview: TAction;
      act_view_showextensions: TAction;
      act_view_showheaderalways: TAction;
      act_view_showhints: TAction;
      act_view_small: TAction;
      act_view_statusbar: TAction;
      act_view_thumbs: TAction;
      act_view_tiles: TAction;
      act_view_viewstyle: TCEToolbarAction;
      act_view_workspace: TAction;
      act_workspace_back: TAction;
      act_workspace_folder_up: TAction;
      act_workspace_forward: TAction;
      act_workspace_open: TAction;
      ApplicationEvents: TApplicationEvents;
      BackgroundCMItems_down: TPopupMenu;
      BackgroundCMItems_up: TPopupMenu;
      CopyPath1: TMenuItem;
      CreateSymbolicLink1: TMenuItem;
      Details1: TMenuItem;
      Files1: TMenuItem;
      Filmstrip1: TMenuItem;
      LargeIcons1: TMenuItem;
      List1: TMenuItem;
      MenuItem_ArragneBy: TMenuItem;
      MenuItem_GroupBy: TMenuItem;
      N1: TMenuItem;
      N2: TMenuItem;
      N3: TMenuItem;
      N4: TMenuItem;
      N5: TMenuItem;
      NewFolder1: TMenuItem;
      Paste1: TMenuItem;
      PasteShortcut1: TMenuItem;
      Properties1: TMenuItem;
      Refresh1: TMenuItem;
      SmallIcons1: TMenuItem;
      Thumbnails1: TMenuItem;
      UpdateTimer: TTimer;
      View1: TMenuItem;
      procedure ActionExecute(Sender: TObject);
      procedure ApplicationEventsActivate(Sender: TObject);
      procedure UpdateTimerTimer(Sender: TObject);
      procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
      procedure BackgroundCMItems_upPopup(Sender: TObject);
      procedure DataModuleCreate(Sender: TObject);
      procedure DataModuleDestroy(Sender: TObject);
    private
      fPageActionList: TActionList;
      { Private declarations }
    protected
      procedure DoAssigneByClick(Sender: TObject);
      procedure DoGroupByClick(Sender: TObject);
    public
      GlobalHotkeys: TCEGlobalHotkeys;
      HotkeySettings: TCEHotkeySettings;
      procedure AssignCustomToolbarActions;
      procedure DoGlobalContextMenuCmd(Namespace: TNamespace; Verb: string; MenuItemID: Integer; var Handled: Boolean);
      procedure DoGlobalContextMenuShow(Namespace: TNamespace; Menu: hMenu; var Allow: Boolean);
      procedure HandleGlobalContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
      procedure UpdateAll;
      property PageActionList: TActionList read fPageActionList write fPageActionList;
          { Public declarations }
    end;

{$ENDREGION}

// Global
procedure ExecuteCEAction(ActionID: Integer);
procedure UpdateCEAction(ActionID: Integer; TargetAction: TAction);
procedure ExecuteEditCategory(ActionID: Integer);
procedure ExecuteSessionsCategory(ActionID: Integer);
procedure ExecuteTabsCategory(ActionID: Integer);
procedure ExecuteHelpCategory(ActionID: Integer);
procedure ExecuteNavigationCategory(ActionID: Integer);
procedure ExecuteQuickviewCategory(ActionID: Integer);
procedure ExecuteBookmarksCategory(ActionID: Integer);

// Action Updates
procedure ExecuteQuickOptionsCategory(ActionID: Integer);
function ExecuteShortcut(Action: TAction): Boolean;
procedure ExecuteToolsCategory(ActionID: Integer);
procedure ExecuteViewCategory(ActionID: Integer);
function FindActionByShortcut(AActionList: TActionList; AShortcut: TShortcut; AOffset: Integer = -1): TAction;
function HandleCmdParams(Str: string): Boolean;
procedure HandleInputMessage(var Msg: TMessage; var Handled: Boolean);
function IsSingleInstance: Boolean;
procedure MouseAction(var Msg: tagMSG; var Handled: Boolean);
function OpenFileInTab(FilePath: string; SelectTab: Boolean = true; ActivateApp: Boolean = false): TCESpTabItem;
function OpenFolderInTab(Sender: TObject; FilePath: string; SelectTab: Boolean = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false; PaneNumber: Integer = 0): TCESpTabItem; overload;
function OpenFolderInTab(Sender: TObject; PIDL: PItemIDList; SelectTab: Boolean = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false; PaneNumber: Integer = 0): TCESpTabItem; overload;
procedure UpdateAllActions;
procedure UpdateBookmarksCategory(ActionID: Integer; TargetAction: TAction);
procedure UpdateEditCategory(ActionID: Integer; TargetAction: TAction);
procedure UpdateGeneralCategory(ActionID: Integer; TargetAction: TAction);
//procedure UpdateHelpCategory(ActionID: Integer; TargetAction: TAction);
procedure UpdateNavigationCategory(ActionID: Integer; TargetAction: TAction);
procedure UpdateQuickOptionsCategory(ActionID: Integer; TargetAction: TAction);
procedure UpdateQuickviewCategory(ActionID: Integer; TargetAction: TAction);
procedure UpdateSessionsCategory(ActionID: Integer; TargetAction: TAction);
procedure UpdateTabsCategory(ActionID: Integer; TargetAction: TAction);
procedure UpdateToolsCategory(ActionID: Integer; TargetAction: TAction);
procedure UpdateViewCategory(ActionID: Integer; TargetAction: TAction);

// Action Executes
procedure DoGlobalContextMenuCmd(Sender: TObject; Namespace: TNamespace; Verb: string; MenuItemID: Integer; var Handled: Boolean);
procedure DoGlobalContextMenuShow(Sender: TObject; Namespace: TNamespace; Menu: hMenu; var Allow: Boolean);
procedure ExecuteFocusCategory(ActionID: Integer);
procedure ExecuteGeneralCategory(ActionID: Integer);
procedure ExecuteMiscCategory(ActionID: Integer);
function FindActionByName(AActionList: TActionList; AName: string): TAction;
function HandleExeCommands: Boolean;
function OpenPIDLInTab(Sender: TObject; APIDL: PItemIDList; SelectTab: Boolean = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false): TCESpTabItem;
procedure UpdateMiscCategory(ActionID: Integer; TargetAction: TAction);

var
  CEActions: TCEActions;
  fDisableSingleInstanceTemporarily: Integer = 0;

implementation

{$R *.dfm}

uses
  fCE_FolderPanel, fCE_QuickViewPanel, fCE_BookmarkPanel, fCE_FiltersPanel, fCE_StackPanel, fCE_WorkspacePanel,
  fCE_FileView, fCE_SearchPage, fCE_TabPage,
  //fCE_AboutBox,
  //fCE_VersionMgrForm,
//  fCE_PoEditor,
  fCE_OptionsDialog,
  fCE_Customizer,
  CE_ToolbarButtons,
  CE_Sessions, CE_BaseFileView, CE_CommonObjects, CE_FileView,
  fCE_QuickViewTab, fCE_QuickView,
  fCE_CreateSymlink,
  Main;

{##############################################################################}

{*------------------------------------------------------------------------------
  On Module Create
-------------------------------------------------------------------------------}
procedure TCEActions.DataModuleCreate(Sender: TObject);
begin
  AssignCustomToolbarActions;
  // Hotkey Settings
  HotkeySettings:= TCEHotkeySettings.Create;
  HotkeySettings.Actions:= ActionList;
  GlobalAppSettings.AddItem('Hotkeys', HotkeySettings, false, false, '', '', false);
  // Global Hotkey Settings
  GlobalHotkeys:= TCEGlobalHotkeys.Create;
  GlobalHotkeys.Actions:= ActionList;
  GlobalHotkeys.MsgHandle:= MainForm.Handle;
  GlobalAppSettings.AddItem('GlobalHotkeys', GlobalHotkeys, false, false, '', '', false);
end;

{*------------------------------------------------------------------------------
  On Module Destroy
-------------------------------------------------------------------------------}
procedure TCEActions.DataModuleDestroy(Sender: TObject);
begin
  HotkeySettings.Free;
  GlobalHotkeys.Free;
end;

{*------------------------------------------------------------------------------
  Assign Custom Toolbar Actions
-------------------------------------------------------------------------------}
procedure TCEActions.AssignCustomToolbarActions;
begin
  act_bookmarks_menu.ItemClass:= TCEBookmarksButton;
  act_edit_copypath.ItemClass:= TCEFileViewCopyPathButton;
  act_edit_newfile.ItemClass:= TCENewFileButton;
  act_edit_undo_delete.ItemClass:= TCEUndoDeleteButton;
  act_filters_menu.ItemClass:= TCEFiltersMenuButton;
  act_filters_pattern.ItemClass:= TCEFilterPatternItem2;
  act_gen_menu.ItemClass:= TCEMainMenuButton;
  act_navi_back.ItemClass:= TCEFileViewBackButton;
  act_navi_forward.ItemClass:= TCEFileViewForwardButton;
  act_sessions_menu.ItemClass:= TCESessionsButton;
  act_stack_open.ItemClass:= TCEStackOpenButton;
  act_stack_save.ItemClass:= TCEStackSaveButton;
  act_tabs_menu.ItemClass:= TCETabsButton;
  act_tabs_undo.ItemClass:= TCEClosedTabsListButton;
  act_tools_systempower.ItemClass:= TCESystemPowerButton;
  act_view_arrangeby.ItemClass:= TCEArrangeByButton;
  act_view_groupby.ItemClass:= TCEGroupByButton;
  act_view_viewstyle.ItemClass:= TCEViewStyleButton;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Execute action (ALL CATEGORIES)
-------------------------------------------------------------------------------}
procedure TCEActions.ActionExecute(Sender: TObject);
var
  act: TAction;
begin
  act:= TAction(Sender);
  ExecuteCEAction(act.Tag);
  UpdateTimerTimer(Sender);
end;

{*------------------------------------------------------------------------------
  Get's called on Action Update timer (UPDATE ALL CATEGORIES)
-------------------------------------------------------------------------------}
procedure TCEActions.UpdateTimerTimer(Sender: TObject);
begin
  UpdateAll;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Execute Action.
-------------------------------------------------------------------------------}
procedure ExecuteCEAction(ActionID: Integer);
begin
  if csDestroying in MainForm.ComponentState then
  Exit;

  case ActionID of
    100..199: ExecuteGeneralCategory(ActionID);
    200..299: ExecuteEditCategory(ActionID);
    300..399: ExecuteViewCategory(ActionID);
    400..499: ExecuteToolsCategory(ActionID);
    500..599: ExecuteHelpCategory(ActionID);
    600..659: ExecuteNavigationCategory(ActionID);
    660..699: ExecuteTabsCategory(ActionID);
    700..799: ExecuteQuickviewCategory(ActionID);
    800..849: ExecuteBookmarksCategory(ActionID);
    850..899: ExecuteSessionsCategory(ActionID);
    900..949: ExecuteMiscCategory(ActionID);
    950..999: ExecuteFocusCategory(ActionID);
    1000..1100: ExecuteQuickOptionsCategory(ActionID);
  end;
end;

{*------------------------------------------------------------------------------
  Update Action
-------------------------------------------------------------------------------}
procedure UpdateCEAction(ActionID: Integer; TargetAction: TAction);
begin
  case ActionID of
    100..199: UpdateGeneralCategory(ActionID, TargetAction);
    200..299: UpdateEditCategory(ActionID, TargetAction);
    300..399: UpdateViewCategory(ActionID, TargetAction);
    400..499: UpdateToolsCategory(ActionID, TargetAction);
//    500..599: UpdateHelpCategory(ActionID, TargetAction);
    600..659: UpdateNavigationCategory(ActionID, TargetAction);
    660..699: UpdateTabsCategory(ActionID, TargetAction);
    700..799: UpdateQuickviewCategory(ActionID, TargetAction);
    800..849: UpdateBookmarksCategory(ActionID, TargetAction);
    850..899: UpdateSessionsCategory(ActionID, TargetAction);
    900..949: UpdateMiscCategory(ActionID, TargetAction);
    1000..1100: UpdateQuickOptionsCategory(ActionID, TargetAction);
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  File Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteGeneralCategory(ActionID: Integer);
begin
  case ActionID of
    100: begin
      Application.MainForm.Close; // Exit
    end;
    101: MainForm.ToggleVisibility;
    102: begin
      if Application.MainForm.CloseQuery then
      begin
        MainForm.Shutdown;
        MainForm.SaveSettingsOnClose:= false;
        fDisableSingleInstanceTemporarily:= GetTickCount;
//        ExecShellEx(WideParamStr(0), '', '', SW_SHOWNORMAL, false, false, true);
        ExecShellEx(ParamStr(0), '', '', SW_SHOWNORMAL, false, false, true);
        Sleep(1000);
        Application.MainForm.Close;
      end;
    end;
    103: begin
      fDisableSingleInstanceTemporarily:= GetTickCount;
//      WideShellExecute(0, 'open', WideParamStr(0), '', '', SW_SHOWNORMAL);
      WideShellExecute(0, 'open', ParamStr(0), '', '', SW_SHOWNORMAL);
    end;
    104: MainForm.MakeVisible;
  end;
end;

{*------------------------------------------------------------------------------
  File Category Update
-------------------------------------------------------------------------------}
procedure UpdateGeneralCategory(ActionID: Integer; TargetAction: TAction);
begin
  TargetAction.Enabled:= true;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Edit Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteEditCategory(ActionID: Integer);
var
  fileview: TCEFileView;
  NS: TNamespace;
  s: String;
begin
  fileview:= nil;
  if CEWorkspacePanel.FileView.Focused then
  fileview:= CEWorkspacePanel.FileView
  else if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  fileview:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;

  if CEFolderPanel.FolderTree.Focused then
  begin
    case ActionID of
      201: CEFolderPanel.FolderTree.CopyToClipBoard;
      202: CEFolderPanel.FolderTree.CutToClipBoard;
      203: CEFolderPanel.FolderTree.PasteFromClipboard;
      204: CEFolderPanel.FolderTree.DeleteSelectedNodes;
      208: if CEFolderPanel.FolderTree.FocusedNode <> nil then
           CEFolderPanel.FolderTree.EditNode(CEFolderPanel.FolderTree.FocusedNode, -1);
      213: CEFolderPanel.FolderTree.PasteShortcutFromClipboard;
    end;
  end
  else if assigned(fileview) then
  begin
    if fileview.Focused then
    begin
      case ActionID of
        201: fileview.CopyToClipboard;
        202: fileview.CutToClipboard;
        203: fileview.PasteFromClipboard;
        204: fileview.SelectedFilesDelete;
        208: if fileview.Selection.FocusedItem <> nil then
             fileview.Selection.FocusedItem.Edit;
        209: begin
               fileview.CopyToClipboard;
               fileview.PasteFromClipboard;
             end;
        213: fileview.PasteShortcutFromClipboard;
      end;
    end;
  end;

  if assigned(fileview) then
  begin
    case ActionID of
      205: fileview.Selection.SelectAll;
      206: fileview.Selection.Invert;
      207: begin
             if fileview.ValidateNamespace(fileview.Selection.First, NS) then
             NS.ShowPropertySheetMulti(MainForm, fileview.SelectedToNamespaceArray, false)
             else
             fileview.RootFolderNamespace.ShowPropertySheet(MainForm);
           end;
      211: begin
             try
               if fileview.Selection.Count > 1 then
               Clipboard.AsText:= fileview.SelectedPaths.Text
               else if fileview.Selection.Count = 1 then
               Clipboard.AsText:= fileview.SelectedPath
               else
               Clipboard.AsText:= IncludeTrailingBackslashW(fileview.RootFolderNamespace.NameForParsing);
             except
               s:= SysErrorMessage(GetLastError);
               MessageBox(0, PChar(s), 'Clipboard error!', MB_ICONERROR or MB_OK);
             end;
           end;
      212: fileview.CreateNewFolder;
      214: ShowCreateSymlinkDialog(fileview.RootFolderNamespace.NameForParsing, '');
      215: CERecycleBinCtrl.RestoreLastDeleted;
      217: fileview.CreateEmptyFile;
    end;
  end;

end;

{*------------------------------------------------------------------------------
  Edit Category Update
-------------------------------------------------------------------------------}
procedure UpdateEditCategory(ActionID: Integer; TargetAction: TAction);
var
  fileview: TCEFileView;
  NS: TNamespace;
begin
  TargetAction.Enabled:= false;
  fileview:= nil;
  if CEWorkspacePanel.FileView.Focused then
  fileview:= CEWorkspacePanel.FileView
  else if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  fileview:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;
  // FolderTree focused
  if CEFolderPanel.FolderTree.Focused then
  begin
    case ActionID of
      201,202,204: begin
        TargetAction.Enabled:= CEFolderPanel.FolderTree.SelectedCount > 0;
      end;
      203,213: TargetAction.Enabled:= ClipboardContainsShellFormats;
      208: begin
             if CEFolderPanel.FolderTree.FocusedNode <> nil then
             begin
               if CEFolderPanel.FolderTree.ValidateNamespace(CEFolderPanel.FolderTree.FocusedNode, NS) then
               TargetAction.Enabled:= NS.CanRename
               else
               TargetAction.Enabled:= false
             end;
           end;
    end;
  end
  // FileView focused
  else if assigned(fileview) then
  begin
    if fileview.Focused then
    begin
      case ActionID of
        201,202,204,209:
          TargetAction.Enabled:=  fileview.Selection.Count > 0;
        203,213: TargetAction.Enabled:= ClipboardContainsShellFormats;
        208: begin
               if fileview.Selection.FocusedItem <> nil then
               begin
                 if fileview.ValidateNamespace(fileview.Selection.FocusedItem, NS) then
                 TargetAction.Enabled:= NS.CanRename
                 else
                 TargetAction.Enabled:= false
               end;
             end;
      end;
    end;
  end;

  if assigned(fileview) then
  begin
    case ActionID of
      205,206,207,210,211,212,217: TargetAction.Enabled:= true;
      214: TargetAction.Enabled:= (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4);
      215: TargetAction.Enabled:= not CERecycleBinCtrl.IsRecycleBinEmpty;
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Focus Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteFocusCategory(ActionID: Integer);
begin
  case ActionID of
    951: if MainForm.AddressBarToolbar.Visible then
         begin
           if MainForm.AddressBarToolbar.AddressBar.Breadcrumb then
           begin
             MainForm.AddressBarToolbar.AddressBar.AutoSwitchToBreadcrumb:= true;
             MainForm.AddressBarToolbar.AddressBar.Breadcrumb:= false;
           end;
           MainForm.AddressBarToolbar.AddressBar.TextEditor.SetFocus;
           MainForm.AddressBarToolbar.AddressBar.TextEditor.SelectAll;
         end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  View Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteViewCategory(ActionID: Integer);
begin
  case ActionID of
    300: MainForm.StatusBar.Visible:= not MainForm.StatusBar.Visible;
    301: if GetFormVisible(CEFolderPanel) then HideDockForm(CEFolderPanel) else ShowDockForm(CEFolderPanel);
    302: if GetFormVisible(CEBookmarkPanel) then HideDockForm(CEBookmarkPanel) else ShowDockForm(CEBookmarkPanel);
    303: if GetFormVisible(CEQuickviewPanel) then HideDockForm(CEQuickviewPanel) else ShowDockForm(CEQuickviewPanel);
    304: if GetFormVisible(CEFiltersPanel) then HideDockForm(CEFiltersPanel) else ShowDockForm(CEFiltersPanel);
    305: if GetFormVisible(CEStackPanel) then HideDockForm(CEStackPanel) else ShowDockForm(CEStackPanel);
//    306: if GetFormVisible(CEArchiverPanel) then HideDockForm(CEArchiverPanel) else ShowDockForm(CEArchiverPanel);
    307: if GetFormVisible(CEWorkspacePanel) then HideDockForm(CEWorkspacePanel) else ShowDockForm(CEWorkspacePanel);

    330: MainForm.ShowHint:= not MainForm.ShowHint;
    332: begin
      GlobalFileViewSettings.HiddenFiles:= not GlobalFileViewSettings.HiddenFiles;
      CEFolderPanel.FolderTree.HiddenFiles:= GlobalFileViewSettings.HiddenFiles;
    end;
    333: GlobalFileViewSettings.ShowHeaderAlways:= not GlobalFileViewSettings.ShowHeaderAlways;
    334: GlobalFileViewSettings.ShowExtensions:= not GlobalFileViewSettings.ShowExtensions;
    335: if MainForm.FormStyle = fsStayOnTop then
         MainForm.FormStyle:= fsNormal
         else
         MainForm.FormStyle:= fsStayOnTop;
    336: GlobalFileViewSettings.ShowInfoBar:= not GlobalFileViewSettings.ShowInfoBar;
    337: GlobalFileViewSettings.CheckBoxSelection:= not GlobalFileViewSettings.CheckBoxSelection;
    351..358: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
              begin
                TCEFileViewPage(GlobalPathCtrl.ActivePage).ViewStyle:= TEasyListStyle(Ord(ActionID - 351));
              end;
    370: MainForm.Fullscreen:= not MainForm.Fullscreen;
    371: MainForm.OpenSkin;
    390: MainForm.LockToolbars:= not MainForm.LockToolbars;
    391: CE_LockPanels:= not CE_LockPanels;
  end;
end;

{*------------------------------------------------------------------------------
  View Category Update
-------------------------------------------------------------------------------}
procedure UpdateViewCategory(ActionID: Integer; TargetAction: TAction);
begin
  TargetAction.Enabled:= true;
  case ActionID of
    300: TargetAction.Checked:= MainForm.StatusBar.Visible;
    301: TargetAction.Checked:= CEFolderPanel.IsVisible;
    302: TargetAction.Checked:= CEBookmarkPanel.IsVisible;
    303: TargetAction.Checked:= CEQuickviewPanel.IsVisible;
    304: TargetAction.Checked:= CEFiltersPanel.IsVisible;
    305: TargetAction.Checked:= CEStackPanel.IsVisible;
//    306: TargetAction.Checked:= CEArchiverPanel.IsVisible;
    307: TargetAction.Checked:= CEWorkspacePanel.IsVisible;
    330: TargetAction.Checked:= MainForm.ShowHint;
    332: TargetAction.Checked:= GlobalFileViewSettings.HiddenFiles;
    333: TargetAction.Checked:= GlobalFileViewSettings.ShowHeaderAlways;
    334: TargetAction.Checked:= GlobalFileViewSettings.ShowExtensions;
    335: TargetAction.Checked:= MainForm.FormStyle = fsStayOnTop;
    336: TargetAction.Checked:= GlobalFileViewSettings.ShowInfoBar;
    337: TargetAction.Checked:= GlobalFileViewSettings.CheckBoxSelection;
    351..358: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
              begin
                if ActionID = (Ord(TCEFileViewPage(GlobalPathCtrl.ActivePage).ViewStyle) + 351) then
                begin
                  TargetAction.Checked:= true;
                  CEActions.act_view_viewstyle.ImageIndex:= TargetAction.ImageIndex;
                end
                else
                TargetAction.Checked:= false;
              end
              else
              begin
                TargetAction.Enabled:= false;
                TargetAction.Checked:= false;
              end;
    370: TargetAction.Checked:= MainForm.Fullscreen;
    372..374: TargetAction.Enabled:= GlobalPathCtrl.ActivePage is TCEFileViewPage;
    390: TargetAction.Checked:= MainForm.LockToolbars;
    391: TargetAction.Checked:= CE_LockPanels;
  end;
end;



{##############################################################################}

{*------------------------------------------------------------------------------
  Tools Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteToolsCategory(ActionID: Integer);
var
  ws: String;
begin
  case ActionID of
    401: ShowCustomizer(MainForm);
    402: ShowOptionsDialog;
    451: WNetConnectionDialog(MainForm.Handle, RESOURCETYPE_DISK);
    452: WNetDisconnectDialog(MainForm.Handle, RESOURCETYPE_DISK);
    453: EmptyRecycleBin;
    454: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           ws:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.NameForParsing;
           if not WideDirectoryExists(ws) then
           ws:= '';
           ShellExecuteW(0,'open','cmd.exe','',PWideChar(ws), SW_SHOW);
         end;
  end;
end;

{*------------------------------------------------------------------------------
  Tools Category Update
-------------------------------------------------------------------------------}
procedure UpdateToolsCategory(ActionID: Integer; TargetAction: TAction);
begin
  TargetAction.Enabled:= true;
  case ActionID of
    453: begin
      TargetAction.Enabled:= not CERecycleBinCtrl.IsRecycleBinEmpty;
      if TargetAction.Enabled then
      TargetAction.ImageIndex:= 24
      else
      TargetAction.ImageIndex:= 23;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Help Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteHelpCategory(ActionID: Integer);
var
//  form: TCEPoEditorForm;
  h: HWND;
begin
  case ActionID of
//    501: ShellExecute(0,'open','http://www.cubicreality.com','','',SW_NORMAL);
//    502: ShellExecute(0,'open','http://www.cubicreality.com/forum','','',SW_NORMAL);
//    503: ShowAboutBox;
//    504: begin
//      form:= TCEPoEditorForm.CreateNew(MainForm,0);
//      form.PoEditor.LocaleDir:= exePath + 'Locale\';
//      form.PoEditor.OnTranslateUI:= MainForm.TranslateUI;
//      if WideFileExists(exePath + 'Locale\default.pot') then
//      form.PoEditor.POTFile.LoadFromFile(exePath + 'Locale\default.pot');
//      form.PoEditor.TabControl.ActiveTabIndex:= 0;
////      form.PoEditor.POT_Rev:= GetFileVersionBuild(WideParamStr(0));
//      form.PoEditor.POT_Rev:= GetFileVersionBuild(ParamStr(0));
//      form.PoEditor.ActiveLanguage:= MainForm.ActiveLanguage;
//      form.Show;
//    end;
//    505: ShellExecute(0,'open','http://www.cubicreality.com/donate/','','',SW_NORMAL);
//    506: ShowVersionManager;
//    507: MainForm.AutoUpdater.CheckForUpdates(true);
    508: begin
      if assigned(CEOptionsDialog) then
      h:= CEOptionsDialog.Handle
      else
      h:= MainForm.Handle;
      if MessageBox(h, 'Restore Default Layout',
                        'Are you sure you want to restore default layout?',
                        MB_ICONQUESTION or MB_YESNO) = idYes then
      begin
        MainForm.Layouts.LoadDefaultLayout;
        if assigned(MainForm.TabSet.ActiveTab) then
        begin
          MainForm.Layouts.LoadLayout(MainForm.TabSet.ActiveTab.Page.Layout,
                                      MainForm.TabSet.ActiveTab.Page.Settings.RememberInnerToolbarLayout,
                                      MainForm.TabSet.ActiveTab.Page.Settings.RememberOuterToolbarLayout,
                                      MainForm.TabSet.ActiveTab.Page.Settings.RememberPanelLayout,
                                      True);
        end;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Help Category Update
-------------------------------------------------------------------------------}
//procedure UpdateHelpCategory(ActionID: Integer; TargetAction: TAction);
//begin
//  case ActionID of
//    507: TargetAction.Enabled:= not MainForm.AutoUpdater.CheckingUpdate;
//    else
//    TargetAction.Enabled:= true;
//  end;
//end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Misc Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteMiscCategory(ActionID: Integer);
begin
  case ActionID of
    901: CEFiltersPanel.ClearFilters;
    902: CEFiltersPanel.Filters.UseWildcards:= not
      CEFiltersPanel.Filters.UseWildcards;
    903: CEFiltersPanel.Filters.ExcludeFromResults:= not
      CEFiltersPanel.Filters.ExcludeFromResults;
    // stack
    923: CEStackPanel.StackTree.DeleteSelectedNodes;
    924: CEStackPanel.ClearList;
    925: CEStackPanel.StackTree.SafeOperationsOnly:= not CEStackPanel.StackTree.SafeOperationsOnly;
    // Workspace
    931: CEWorkspacePanel.FileView.GoFolderUp;
    932: CEWorkspacePanel.FileView.GoBackInHistory;
    933: CEWorkspacePanel.FileView.GoForwardInHistory;
    934: begin
      if GlobalPathCtrl.ActivePage is TCEFileViewPage then
      CEWorkspacePanel.FileView.BrowseToByPIDL(TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.AbsolutePIDL);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  Update Misc Category
-------------------------------------------------------------------------------}
procedure UpdateMiscCategory(ActionID: Integer; TargetAction: TAction);
begin
  TargetAction.Enabled:= true;
  case ActionID of
    902: TargetAction.Checked:= CEFiltersPanel.Filters.UseWildcards;
    903: TargetAction.Checked:= CEFiltersPanel.Filters.ExcludeFromResults;
    // stack
    923: TargetAction.Enabled:= CEStackPanel.StackTree.SelectedCount > 0;
    924: TargetAction.Enabled:= CEStackPanel.StackTree.RootNode.ChildCount > 0;
    925: TargetAction.Checked:= not CEStackPanel.StackTree.SafeOperationsOnly;
    // Workspace
    932: TargetAction.Enabled:= CEWorkspacePanel.FileView.History.HasBackItems;
    933: TargetAction.Enabled:= CEWorkspacePanel.FileView.History.HasNextItems;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Navigation Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteNavigationCategory(ActionID: Integer);
var
  page: TCECustomTabPage;
  ws,ext: String;
  ns: TNamespace;
  item: TEasyItem;
  quickview: TCEQuickViewPage;
begin
  case ActionID of
    601: begin
           ExecuteTabsCategory(663);
         end;
    602: begin
           MainForm.TabSet.CloseSelectedTab;
         end;
    603: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.GoBackInHistory;
         end;
    604: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.GoForwardInHistory;
         end;
    605: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.GoFolderUp;
         end;
    606: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           begin
             if GlobalFileViewSettings.Thumbnails.UseStorage then
             TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.ClearThumbnailCache; // refresh thumbnails
             TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Rebuild(true);
           end;
           CEFolderPanel.FolderTree.Refresh;
           CEWorkspacePanel.FileView.Rebuild(true);
           MainForm.DriveToolbar.Populate;
           MainForm.StatusBar.UpdateLabels(true);
         end;
    607: begin
           ExecuteTabsCategory(664);
         end;
    608: MainForm.TabSet.ScrollLeft;
    609: MainForm.TabSet.ScrollRight;
    610: begin
      if GlobalPathCtrl.ActivePage is TCEFileViewPage then
      TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Rebuild;
    end;
    // Open Editor
    650: begin
           GlobalFileViewSettings.AssignFromActivePage;
           ws:= '';
           if (GlobalPathCtrl.ActivePage is TCEFileViewPage) then
           begin
             item:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Selection.First;
             if TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end
           else if (GlobalPathCtrl.ActivePage is TCESearchPage) then
           begin
             item:= TCESearchPage(GlobalPathCtrl.ActivePage).ResultView.Selection.First;
             if TCESearchPage(GlobalPathCtrl.ActivePage).ResultView.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end;

           quickview:= TCEQuickViewPage(MainForm.TabSet.AddTab(TCEQuickViewPage, MainForm.TabSet.Settings.NewTabSelect).Page);
           quickview.QuickView.OpenTextFile(ws);
           if quickview.Visible then
           quickview.QuickView.SetFocusToMediaPlayer;
         end;
    // Open Search
    651: begin
           GlobalFileViewSettings.AssignFromActivePage;
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           ws:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.NameForParsing;
           page:= TCECustomTabPage(MainForm.TabSet.AddTab(TCESearchPage, MainForm.TabSet.Settings.NewTabSelect).Page);
           if WideDirectoryExists(ws) then
           begin
             TCESearchPage(page).edit_location.Text:= ws;
           end;
         end;
    // Open QuickView
    652: begin
           GlobalFileViewSettings.AssignFromActivePage;
           ws:= '';
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           begin
             item:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Selection.First;
             if TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end
           else if (GlobalPathCtrl.ActivePage is TCESearchPage) then
           begin
             item:= TCESearchPage(GlobalPathCtrl.ActivePage).ResultView.Selection.First;
             if TCESearchPage(GlobalPathCtrl.ActivePage).ResultView.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end;

           quickview:= TCEQuickViewPage(MainForm.TabSet.AddTab(TCEQuickViewPage, MainForm.TabSet.Settings.NewTabSelect).Page);
           if ws <> '' then
           quickview.OpenFile(ws);
         end;
  end;
end;

{*------------------------------------------------------------------------------
  Navigation Category  Update
-------------------------------------------------------------------------------}
procedure UpdateNavigationCategory(ActionID: Integer; TargetAction: TAction);
var
  L,R: Boolean;
begin
  case ActionID of
    //602: TargetAction.Enabled:= MainForm.TabSet.TabCount > 1;
    603: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.History.HasBackItems
         else
         TargetAction.Enabled:= false;
    604: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.History.HasNextItems
         else
         TargetAction.Enabled:= false;
    605: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= (TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.Parent <> nil)
         else
         TargetAction.Enabled:= false;
    606,607: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= True
         else
         TargetAction.Enabled:= false;
    608, 609: begin
      MainForm.TabSet.ScrollState(L, R);
      if L or R then
      begin
        if not CEActions.act_navi_scrollleft.Visible then
        begin
          CEActions.act_navi_scrollleft.Visible:= true;
          CEActions.act_navi_scrollright.Visible:= true;
        end;
        CEActions.act_navi_scrollleft.Enabled:= L;
        CEActions.act_navi_scrollright.Enabled:= R;
      end
      else
      begin
        if CEActions.act_navi_scrollleft.Visible then
        begin
          CEActions.act_navi_scrollleft.Visible:= false;
          CEActions.act_navi_scrollright.Visible:= false;
        end;
      end;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Tabs Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteTabsCategory(ActionID: Integer);
var
  pidl: PItemIDList;
  SrcFileView, DestFileView: TCustomVirtualExplorerEasyListview;
begin
  if not assigned(MainForm.TabSet.ActivePopupTab) then
  begin
    MainForm.TabSet.ActivePopupTab:= MainForm.TabSet.GetActiveTab;
  end;

  case ActionID of
    // Close Tab
    661: MainForm.TabSet.CloseTab(MainForm.TabSet.ActivePopupTab);
    // Close Other Tabs
    662: MainForm.TabSet.CloseAllTabs(MainForm.TabSet.ActivePopupTab);
    // Add Tab
    663: begin
           GlobalFileViewSettings.AssignFromActivePage;
           if MainForm.TabSet.Settings.NewTabType = 1 then // Clone active tab
           begin
             if GlobalPathCtrl.ActivePage is TCEFileViewPage then
             begin
              if MainForm.TabSet.Toolbar.GetTabsCount(true) = 0 then
              pidl:= DrivesFolder.AbsolutePIDL
              else
              pidl:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.AbsolutePIDL
             end
             else
             pidl:= nil;
             OpenFolderInTab(nil, pidl, MainForm.TabSet.Settings.NewTabSelect, false, true);
           end
           else if MainForm.TabSet.Settings.NewTabType = 3 then // Open custom path
           begin
             OpenFolderInTab(nil, MainForm.TabSet.Settings.NewTabNamespace.AbsolutePIDL, MainForm.TabSet.Settings.NewTabSelect, false, true);
           end
           else // Open desktop
           begin
             OpenFolderInTab(nil, nil, MainForm.TabSet.Settings.NewTabSelect, false, true);
           end;
         end;
    // Duplicate Tab
    664: begin
           if assigned(MainForm.TabSet.ActivePopupTab) then
           begin
             if MainForm.TabSet.ActivePopupTab.Page is TCEFileViewPage then
             OpenFolderInTab(MainForm.TabSet.ActivePopupTab,
                             TCEFileViewPage(MainForm.TabSet.ActivePopupTab.Page).FileView.RootFolderNamespace.AbsolutePIDL, MainForm.TabSet.Settings.NewTabSelect, false, true);
           end;
         end;
    // Close Tabs on Left
    665: MainForm.TabSet.CloseTabsOnLeft(MainForm.TabSet.ActivePopupTab);
    // Close Tabs on Right
    666: MainForm.TabSet.CloseTabsOnRight(MainForm.TabSet.ActivePopupTab);
    // Undo Tab Close
    667: MainForm.TabSet.UndoTabClose;
    // Switch to Next Tab
    668: MainForm.TabSet.SelectNextTab(true);
    // Swith to Previous Tab
    669: MainForm.TabSet.SelectNextTab(false);
    // Copy/Move Selected Here
    671, 672: begin
       if assigned(MainForm.TabSet.ActivePopupTab) and
          ((MainForm.TabSet.ActiveTab.Page is TCEFileViewPage) or (MainForm.TabSet.ActiveTab.Page is TCESearchPage)) and
          (MainForm.TabSet.ActivePopupTab.Page is TCEFileViewPage) then
       begin
         if (MainForm.TabSet.ActiveTab.Page is TCESearchPage) then
         SrcFileView:= TCESearchPage(MainForm.TabSet.ActiveTab.Page).ResultView
         else
         SrcFileView:= TCEFileViewPage(MainForm.TabSet.ActiveTab.Page).FileView;

         DestFileView:= TCEFileViewPage(MainForm.TabSet.ActivePopupTab.Page).FileView;

         if ActionID = 672 then
         SrcFileView.CutToClipboard
         else
         SrcFileView.CopyToClipboard;

         DestFileView.PasteFromClipboard;
       end;
    end;
  end;

  MainForm.TabSet.ActivePopupTab:= nil; // popup has closed so set this to nil
end;

{*------------------------------------------------------------------------------
  Tabs Category  Update
-------------------------------------------------------------------------------}
procedure UpdateTabsCategory(ActionID: Integer; TargetAction: TAction);
var
  b: Boolean;
begin
  case ActionID of
    661..662,664: TargetAction.Enabled:= assigned(MainForm.TabSet.ActivePopupTab) or (MainForm.TabSet.GetActiveTab <> nil);
    667: TargetAction.Enabled:= MainForm.TabSet.CanUndoTabClose;
    671, 672: begin
      if assigned(MainForm.TabSet.ActivePopupTab) and assigned(MainForm.TabSet.ActiveTab) and (MainForm.TabSet.ActivePopupTab <> MainForm.TabSet.ActiveTab) then
      begin
        // TODO: hunting bug here, remove this!:
        b:= MainForm.TabSet.ActiveTab.Page is TCEFileViewPage;
        b:= b or (MainForm.TabSet.ActiveTab.Page is TCESearchPage);
        b:= b and (MainForm.TabSet.ActivePopupTab.Page is TCEFileViewPage);
        TargetAction.Visible:= b;

//        TargetAction.Visible:= ((MainForm.TabSet.ActiveTab.Page is TCEFileViewPage) or (MainForm.TabSet.ActiveTab.Page is TCESearchPage)) and
//                               (MainForm.TabSet.ActivePopupTab.Page is TCEFileViewPage);

        if TargetAction.Visible then
        begin
          if (MainForm.TabSet.ActiveTab.Page is TCEFileViewPage) then
          TargetAction.Enabled:= TCEFileViewPage(MainForm.TabSet.ActiveTab.Page).FileView.Selection.Count > 0
          else
          TargetAction.Enabled:= TCESearchPage(MainForm.TabSet.ActiveTab.Page).ResultView.Selection.Count > 0
        end;
      end
      else
      begin
        TargetAction.Visible:= false;
      end;
      MainForm.sep_tabs_copymove.Visible:= TargetAction.Visible;
    end;
    670: TargetAction.Enabled:= MainForm.TabSet.TabCount > 1;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Quickview Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteQuickviewCategory(ActionID: Integer);
begin

end;

{*------------------------------------------------------------------------------
  Quickview Category Update
-------------------------------------------------------------------------------}
procedure UpdateQuickviewCategory(ActionID: Integer; TargetAction: TAction);
begin

end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Bookmarks Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteBookmarksCategory(ActionID: Integer);
begin
/// TODO: What's going on here?

//
//  case ActionID of
//    801: MainForm.BookmarkTree.InsertBookmark(MainForm.BookmarkTree.FocusedNode,amAddChildLast,nil, 'Category', true);
//    802: begin
//           if GlobalPathCtrl.ActiveFileView <> nil then
//           begin
//             MainForm.BookmarkTree.InsertBookmark(MainForm.BookmarkTree.FocusedNode,
//                                                  amAddChildLast,
//                                                  PIDLMgr.CopyPIDL(GlobalPathCtrl.ActiveFileView.RootFolderNamespace.AbsolutePIDL));
//           end;
//         end;
//    803: MainForm.BookmarkTree.EditNode(MainForm.BookmarkTree.FocusedNode, 0);
//    804: begin
//           MainForm.BookmarkTree.SafeDeleteSelectedNodes;
//         end;
//  end;
end;

{*------------------------------------------------------------------------------
  Bookmarks Category Update
-------------------------------------------------------------------------------}
procedure UpdateBookmarksCategory(ActionID: Integer; TargetAction: TAction);
begin
  TargetAction.Enabled:= true;
end;


{##############################################################################}

{*------------------------------------------------------------------------------
  Session Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteSessionsCategory(ActionID: Integer);
begin
  case ActionID of
    851: GlobalSessions.SaveSessionDlg;
    852: GlobalSessions.ShowSessionManager;
    853: GlobalSessions.AddHistorySession;
    854: GlobalSessions.ClearHistory;
    855: GlobalSessions.AutoSaveHistory:= not GlobalSessions.AutoSaveHistory;
  end;
end;

{*------------------------------------------------------------------------------
  Session Category Update
-------------------------------------------------------------------------------}
procedure UpdateSessionsCategory(ActionID: Integer; TargetAction: TAction);
begin
  TargetAction.Enabled:= true;
  if ActionID = 855 then
  TargetAction.Checked:= GlobalSessions.AutoSaveHistory;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Mouse Input actions
-------------------------------------------------------------------------------}
procedure MouseAction(var Msg: tagMSG; var Handled: Boolean);
begin

// No need for this anymore, AppCommand is used instead.
//
//  if (Msg.message = 523) or (Msg.message = 525) then // Navigation buttons
//  begin
//    Shift:= Lo(Msg.wParam);
//    if Shift = MK_XBUTTON1 then  // back
//    ExecuteNavigationCategory(603)
//    else if Shift = MK_XBUTTON2 then // forward
//    ExecuteNavigationCategory(604);
//  end
end;

{*------------------------------------------------------------------------------
  Open file in a new tab
-------------------------------------------------------------------------------}
function OpenFileInTab(FilePath: String; SelectTab: Boolean = true;
    ActivateApp: Boolean = false): TCESpTabItem;
var
  quickview: TCEQuickViewPage;
begin
  Result:= nil;
  if WideFileExists(FilePath) then
  begin
    GlobalFileViewSettings.AssignFromActivePage;

    if GlobalQuickViewSettings.IsSupported(WideExtractFileExt(FilePath), false) then
    begin
      Result:= MainForm.TabSet.AddTab(TCEQuickViewPage, SelectTab);
      quickview:= TCEQuickViewPage(Result.Page);
      quickview.OpenFile(FilePath);
      if quickview.Visible then
      quickview.QuickView.SetFocusToMediaPlayer;
    end
    else
    begin
      Result:= MainForm.TabSet.AddTab(TCEQuickViewPage, SelectTab);
      quickview:= TCEQuickViewPage(Result.Page);
      quickview.QuickView.OpenTextFile(FilePath);
      if quickview.Visible then
      quickview.QuickView.SetFocusToMediaPlayer;
    end;

    if ActivateApp then
    MainForm.MakeVisible;
  end;
end;

{*------------------------------------------------------------------------------
  Open Folder in a new tab
-------------------------------------------------------------------------------}
function OpenFolderInTab(Sender: TObject; FilePath: String; SelectTab:
    Boolean = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false;
    PaneNumber: Integer = 0): TCESpTabItem;
var
  PIDL: PItemIDList;
begin
  PIDL:= PathToPIDL(FilePath);

  Result:= OpenFolderInTab(Sender, PIDL, SelectTab, ActivateApp, ForceNewTab, PaneNumber);
end;

{*------------------------------------------------------------------------------
  Open Folder in a new tab
-------------------------------------------------------------------------------}
function OpenFolderInTab(Sender: TObject; PIDL: PItemIDList; SelectTab: Boolean
    = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false;
    PaneNumber: Integer = 0): TCESpTabItem;
var
  page: TCEFileViewPage;
  eItem: TExplorerItem;
  i: Integer;
  ns: TNamespace;
  isFolder: Boolean;
  browsePIDL: PItemIDList;
begin
  ns:= TNamespace.Create(PIDL, nil);
  ns.FreePIDLOnDestroy:= false;
  isFolder:= ns.FileSystem and ns.Folder;

  if not isFolder then
  begin
    browsePIDL:= FindBrowseableRootPIDL(ns);
  end
  else
  begin
    browsePIDL:= PIDLMgr.CopyPIDL(PIDL);
  end;

  try
    Result:= nil;
    GlobalFileViewSettings.AssignFromActivePage;
    if MainForm.TabSet.Settings.ReuseTabs and not ForceNewTab then
    begin
      for i:= 0 to MainForm.TabSet.Items.Count -1 do
      begin
        if MainForm.TabSet.Items.Items[i] is TCESpTabItem then
        begin
          Result:= TCESpTabItem(MainForm.TabSet.Items.Items[i]);
          if Result.Page is TCEFileViewPage then
          begin
            page:= TCEFileViewPage(Result.Page);
            if ILIsEqual(browsePIDL, page.FileView.RootFolderNamespace.AbsolutePIDL) then
            begin
              MainForm.TabSet.SelectTab(Result);
              if ActivateApp then
              MainForm.MakeVisible;
              break;
            end
            else
            Result:= nil;
          end
          else
          Result:= nil;
        end;
      end;
    end;

    if not assigned(Result) then
    begin
      Result:= MainForm.TabSet.AddTab(TCEFileViewPage, false, false);
      if assigned(Result) then
      begin
        page:= TCEFileViewPage(Result.Page);
        page.FileView.BeginHistoryUpdate;

        page.FileView.Selection.ClearAll;

        page.FileView.RootFolderCustomPIDL:= browsePIDL;

        if page.FileView.Selection.First <> nil then
        page.FileView.Selection.First.MakeVisible(emvMiddle);

        page.FileView.ClearHistory;
        page.FileView.History.Add(TNamespace.Create(PIDLMgr.CopyPIDL(browsePIDL),nil),true);
        page.FileView.EndHistoryUpdate;
        page.Active:= true;

        if SelectTab or (MainForm.TabSet.Toolbar.GetTabsCount(true) = 1) then
        MainForm.TabSet.SelectTab(Result);
        if ActivateApp then
        MainForm.MakeVisible;
      end;
    end;

    if not isFolder then
    begin
      if assigned(Result) then
      begin
        page:= TCEFileViewPage(Result.Page);
        eItem:= page.FileView.FindItemByPIDL(PIDL);
        if assigned(eItem) then
        begin
          page.FileView.Selection.ClearAll;
          eItem.Selected:= true;
          eItem.Focused:= true;
          eItem.MakeVisible(emvMiddle);
        end;
      end;
    end;

  finally
    ns.Free;
    PIDLMgr.FreeAndNilPIDL(browsePIDL);
  end;
end;

{*------------------------------------------------------------------------------
  Handle Is Single Instance query
-------------------------------------------------------------------------------}
function IsSingleInstance: Boolean;
begin
  Result:= MainForm.SingleInstance;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Quick Options Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteQuickOptionsCategory(ActionID: Integer);
begin
  //
end;

{*------------------------------------------------------------------------------
  Quick Options Category Update
-------------------------------------------------------------------------------}
procedure UpdateQuickOptionsCategory(ActionID: Integer; TargetAction:
    TAction);
begin
  //
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Handle Command Line input
-------------------------------------------------------------------------------}
function HandleCmdParams(Str: string): Boolean;
var
  list: TStrings;
  i: Integer;
  pidl: PItemIDList;
  path, path2: string;
  TabOpened: Boolean;
  IsShortcut: Boolean;
  IsFile: Boolean;
  IsIcon: Boolean;
  NS: TNamespace;
begin
  Result:= false;
  list:= TStringList.Create;
  try
    list.Delimiter:= ',';
    list.StrictDelimiter:= true;
    list.DelimitedText:= Str;

    i:= 0;
    TabOpened:= false;
    IsShortcut:= false;
    IsFile:= false;
    IsIcon:= false;
    while i < list.Count do
    begin
      if IsSameText(list.Strings[i], '/idlist') and not TabOpened then
      begin
        i:= i + 1;
        if i < list.Count then
        begin
          pidl:= StringToPIDL(list.Strings[i]);
          if pidl <> nil then
          begin
            OpenFolderInTab(nil, pidl, true, true);
            Result:= true;
            TabOpened:= true;
          end;
        end;
        IsShortcut:= false;
        IsFile:= false;
        IsIcon:= false;
      end
      else if IsSameText(list.Strings[i], '/n') then
      begin
        TabOpened:= false;
        IsShortcut:= false;
        IsFile:= false;
        IsIcon:= false;
      end
      else if IsSameText(list.Strings[i], '/link') then
      begin
        TabOpened:= false;
        IsShortcut:= true;
        IsFile:= false;
        IsIcon:= false;
      end
      else if IsSameText(list.Strings[i], '/f') then
      begin
        TabOpened:= false;
        IsShortcut:= false;
        IsFile:= true;
        IsIcon:= false;
      end
      else if IsSameText(list.Strings[i], '/icon') then
      begin
        TabOpened:= false;
        IsShortcut:= false;
        IsFile:= false;
        IsIcon:= true;
      end
      else if list.Strings[i] <> '' then
      begin
        path:= list.Strings[i];
        if IsIcon then
        begin
          if not WideFileExists(path) then
          path:= DecodeRelativePath(path);
          if WideFileExists(path) then
          begin
            try
              Application.Icon.LoadFromFile(path);
            except
            end;
          end;
          IsIcon:= false;
        end
        else
        begin
          // Open File
          if IsFile then
          begin
            if not WideFileExists(path) then
            path:= DecodeRelativePath(path);
            if WideFileExists(path) then
            begin
              OpenFileInTab(path, true, false);
              Result:= true;
              TabOpened:= true;
            end;
            IsFile:= false;
          end;
          // Open Shortcut
          if IsShortcut then
          begin
            if not WideFileExists(path) then
            path:= DecodeRelativePath(path);
            if WideFileExists(path) then
            begin
              NS:= TNamespace.CreateFromFileName(path);
              try
                if NS.Link then
                begin
                  OpenFolderInTab(nil, NS.ShellLink.TargetIDList, true, false);
                  Result:= true;
                  TabOpened:= true;
                end;
              finally
                NS.Free;
              end;
            end;
            IsShortcut:= false;
          end;
          // Open normal folder
          if not TabOpened then
          begin
            path := ReplaceSystemVariablePath(path);

            if path[1] = ':' then
            begin

              // Open Control Panel in explorer.exe. CE won't work properly with Vista+ control panels.
//              if IsWindowsVista and (Pos('::{26EE0668-A00A-44D7-9371-BEB064C98683}', path) = 1) then
//              begin
//                path:= 'shell:' + path;
//                path2:= WideExpandEnviromentString('%windir%\explorer.exe');
//                ShellExecuteW(0, 'open', PWideChar(path2), PWideChar(path), '', SW_SHOWNORMAL);
//                Result:= false;
//              end
//              else
//              begin
                OpenFolderInTab(nil, path, true, false);
                Result:= true;
                TabOpened:= true;
//              end;
            end
            else
            begin
              if not DirExistsVET(path, false) then
              path:= DecodeRelativePath(path);
              if DirExistsVET(path, false) then
              begin
                OpenFolderInTab(nil, path, true, false);
                Result:= true;
                TabOpened:= true;
              end
            end;
          end;
        end;
      end;
      i:= i + 1;
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Update All Actions
-------------------------------------------------------------------------------}
procedure UpdateAllActions;
begin
  if assigned(CEActions) then
  CEActions.UpdateAll
end;

{-------------------------------------------------------------------------------
  Handle Input message
-------------------------------------------------------------------------------}
procedure HandleInputMessage(var Msg : TMessage; var Handled: Boolean);
var
  ws: String;
  h: HWND;
begin
  case msg.Msg of
    // Single instance question
    WM_SingleInstance: begin
      if ((GetTickCount - fDisableSingleInstanceTemporarily) > 30000) and IsSingleInstance then
      Msg.Result:= 0
      else
      Msg.Result:= -1;
      Handled:= true;
    end;
    // Copy Data for command line parameters
    WM_COPYDATA: begin
      ws:= PWideChar(TWMCopyData(Msg).CopyDataStruct.lpData);
      TWMCopyData(Msg).Result:= 0;
      HandleCmdParams(ws);
      Handled:= true;
    end;
    // Make CE Visible
    WM_MakeVisible: begin
      MainForm.MakeVisible;
    end;
    // Execute Action
    WM_ExecuteAction: begin
      ExecuteCEAction(Msg.WParam);
    end;
    // Admin command result
    WM_AdminResult: begin
      case Msg.WParam of
        // Register as Default File Manager
        100: begin
          if assigned(CEOptionsDialog) then
          h:= CEOptionsDialog.Handle
          else
          h:= MainForm.Handle;

          if Msg.LParam = 0 then
          MessageBox(h, 'Default File Manager', 'Registered successfully!', MB_ICONINFORMATION or MB_OK)
          else
          MessageBox(h, 'Default File Manager', 'Registration failed!', MB_ICONERROR or MB_OK);
        end;
        // UnRegister as Default File Manager
        101: begin
          if assigned(CEOptionsDialog) then
          h:= CEOptionsDialog.Handle
          else
          h:= MainForm.Handle;

          if Msg.LParam = 0 then
          MessageBox(h, 'Default File Manager', 'Unregistered successfully!', MB_ICONINFORMATION or MB_OK)
          else
          MessageBox(h, 'Default File Manager', 'Unregistration failed!', MB_ICONERROR or MB_OK);
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Execute Shortcuts
-------------------------------------------------------------------------------}
function ExecuteShortcut(Action: TAction): Boolean;
begin
  if assigned(Action) then
  Result:= Action.Execute
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Find Action by shortcut
-------------------------------------------------------------------------------}
function FindActionByShortcut(AActionList: TActionList; AShortcut: TShortcut;
    AOffset: Integer = -1): TAction;
var
  i,i2: Integer;
  act: TAction;
begin
  Result:= nil;
  for i:= 0 to AActionList.ActionCount-1 do
  begin
    act:= TAction(AActionList.Actions[i]);
    if act.ShortCut = AShortcut then
    begin
      Result:= act;
      break;
    end
    else
    begin
      for i2:= 0 to act.SecondaryShortCuts.Count - 1 do
      begin
        if TShortCut(act.SecondaryShortCuts.Objects[i2]) = AShortcut then
        begin
          Result:= act;
          break;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  FindActionByName
-------------------------------------------------------------------------------}
function FindActionByName(AActionList: TActionList; AName: String): TAction;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to AActionList.ActionCount-1 do
  begin
    if AActionList.Actions[i].Name = AName then
    begin
      Result:= TAction(AActionList.Actions[i]);
      break;
    end;
  end;
end;

{##############################################################################}
// Global context menu items

{-------------------------------------------------------------------------------
  Do GlobalContextMenuShow
-------------------------------------------------------------------------------}
procedure DoGlobalContextMenuShow(Sender: TObject; Namespace: TNamespace; Menu:
    hMenu; var Allow: Boolean);

  procedure DoInsertMenuItem(const ACaption: String; AID: Cardinal; APos: Cardinal = 0);
  var
    info: TMenuItemInfo;
  begin
    FillChar(info, SizeOf(info), #0);
    info.cbSize:= SizeOf(info);
    info.fMask:= MIIM_TYPE or MIIM_ID or MIIM_STATE;
    info.fType:= MFT_STRING;
    info.dwTypeData:= PChar(ACaption);
    info.cch:= Length(ACaption) + 1;
    info.wID:= AID;
    InsertMenuItem(Menu, APos, true, info);
  end;


var
  ws: String;
  pos: Integer;
begin
  pos:= 0;
  if Namespace.FileSystem then
  begin
    // Add "Open in new tab" item
    ws:= 'Open in new tab';
    DoInsertMenuItem(ws, 664, pos);
    pos:= pos + 1;

    if Namespace.Folder then
    begin
      // Add "Open in Workspace" item
      ws:= 'Open in Workspace';
      DoInsertMenuItem(ws, 934, pos);
      pos:= pos + 1;
    end;

  end;
end;


{-------------------------------------------------------------------------------
  Do GlobalContextMenuCmd
-------------------------------------------------------------------------------}
procedure DoGlobalContextMenuCmd(Sender: TObject; Namespace: TNamespace; Verb:
    String; MenuItemID: Integer; var Handled: Boolean);
var
  item: TEasyItem;
  ns: TNamespace;
begin
  // Handle "Open in new tab"
  if MenuItemID = 664 then
  begin
    // quick hack to open multiple folders!
    if (Sender is TCECustomFileView) and
       (TCECustomFileView(Sender).Selection.Count > 0) then
    begin
      item:= TCECustomFileView(Sender).Selection.First;
      while assigned(item) do
      begin
        if TCECustomFileView(Sender).ValidateNamespace(item, ns) then
        begin
          if ns.Folder then
          begin
            OpenFolderInTab(Sender, ns.AbsolutePIDL,
              MainForm.TabSet.Settings.OpenTabSelect);
          end
          else if ns.FileSystem then
          begin
            //if ns.Link then
            //ns.ShellLink.
            OpenFileInTab(ns.NameForParsing, MainForm.TabSet.Settings.OpenTabSelect);
          end;
        end;
        item:= TCECustomFileView(Sender).Selection.Next(item);
      end;
    end
    else
    OpenFolderInTab(Sender, Namespace.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect);
  end
  // Handle "Open in Workspace" item
  else if MenuItemID = 934 then
  begin
    CEWorkspacePanel.FileView.BrowseToByPIDL(Namespace.AbsolutePIDL);
  end;
end;

{-------------------------------------------------------------------------------
  Handle Exe Commands (Runs before UI has been created!
    If Result=true, CE will terminate after this function)
-------------------------------------------------------------------------------}
function HandleExeCommands: Boolean;
var
  doShell: Boolean;
  ws, path: string;
  index, count: Integer;
begin
  Result:= false;
//  if IsWindowsVista then // NOTICE: at the moment there's nothing to do prior to vista.
//  begin
//    doShell:= false;
//    count:= ParamCount;
//    for index:= 1 to count do
//    begin
//      ws:= ParamStr(index);
//      if not doShell then
//      begin
//        if ws = '/shell' then
//        begin
//          doShell:= true;
//        end;
//      end
//      else
//      begin
//        if IsWindowsVista then
//        begin
//          // Control Panel
//          if (Pos('::{26EE0668-A00A-44D7-9371-BEB064C98683}', ws) = 1) or
//             (Pos('::{21EC2020-3AEA-1069-A2DD-08002B30309D}', ws) = 1) then
//          begin
//            path:= WideExpandEnviromentString('%windir%\explorer.exe');
//            ws:= 'shell:' + ws;
//            ShellExecute(0, 'open', PChar(path), PChar(ws), '', SW_SHOWNORMAL);
//            Result:= true;
//          end;
//        end;
//      end;
//    end;
//  end;
end;

{-------------------------------------------------------------------------------
  OpenPIDLInTab
-------------------------------------------------------------------------------}
function OpenPIDLInTab(Sender: TObject; APIDL: PItemIDList; SelectTab: Boolean
    = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false):
    TCESpTabItem;
var
  ns: TNamespace;
begin
  Result:= nil;
  if not assigned(APIDL) then
  Exit;

  ns:= TNamespace.Create(APIDL, nil);
  try
    ns.FreePIDLOnDestroy:= false;
    if ns.FileSystem and not ns.Folder then
    Result:= OpenFileInTab(ns.NameForParsing, SelectTab, ActivateApp)
    else
    Result:= OpenFolderInTab(Sender, APIDL, SelectTab, ActivateApp, ForceNewTab);
  finally
    ns.Free;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  On Application Activate
-------------------------------------------------------------------------------}
procedure TCEActions.ApplicationEventsActivate(Sender: TObject);
begin
  //UpdateAll;  // why is this called here?
end;

{*------------------------------------------------------------------------------
  Application Messages
-------------------------------------------------------------------------------}
procedure TCEActions.ApplicationEventsMessage(var Msg: tagMSG;
  var Handled: Boolean);
begin
  // Do nothing for now
//  case Msg.message of
//    512..525: MouseAction(Msg, Handled);
//  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  On BackgroundCMItems_up Popup
-------------------------------------------------------------------------------}
procedure TCEActions.BackgroundCMItems_upPopup(Sender: TObject);
var
  item: TMenuItem;
  page: TCEFileViewPage;
  col: TEasyColumn;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    MenuItem_ArragneBy.Clear;
    page:= TCEFileViewPage(GlobalPathCtrl.ActivePage);
    col:= page.FileView.Header.FirstVisibleColumn;
    while assigned(col) do
    begin
      item:= BackgroundCMItems_up.CreateMenuItem;
      item.Caption:= col.Caption;
      item.OnClick:= DoAssigneByClick;
      item.Tag:= Integer(col);
      item.RadioItem:= true;
      if col.SortDirection <> esdNone then
      item.Checked:= true;
      MenuItem_ArragneBy.Add(item);
      col:= page.FileView.Header.NextVisibleColumn(col);
    end;
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= '-';
    item.Tag:= 0;
    MenuItem_ArragneBy.Add(item);

    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= 'More...';
    item.Tag:= -1;
    item.OnClick:= DoAssigneByClick;
    MenuItem_ArragneBy.Add(item);

    // Group By
      // Sho in Groups
    MenuItem_GroupBy.Clear;
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= 'Show in Groups';
    item.Checked:= page.FileView.Grouped;
    item.Tag:= -2;
    item.OnClick:= DoGroupByClick;
    MenuItem_GroupBy.Add(item);
      // separator
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= '-';
    item.Tag:= 0;
    MenuItem_GroupBy.Add(item);
      // group by items
    col:= page.FileView.Header.FirstVisibleColumn;
    while assigned(col) do
    begin
      item:= BackgroundCMItems_up.CreateMenuItem;
      item.Caption:= col.Caption;
      item.OnClick:= DoGroupByClick;
      item.Tag:= Integer(col);
      item.RadioItem:= true;
      item.Checked:= page.FileView.GroupingColumn = col.Index;
      MenuItem_GroupBy.Add(item);
      col:= page.FileView.Header.NextVisibleColumn(col);
    end;
      // separator
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= '-';
    item.Tag:= 0;
    MenuItem_GroupBy.Add(item);

    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= 'More...';
    item.Tag:= -1;
    item.OnClick:= DoGroupByClick;
    MenuItem_GroupBy.Add(item);
  end;
end;

{-------------------------------------------------------------------------------
  Do AssigneBy Click
-------------------------------------------------------------------------------}
procedure TCEActions.DoAssigneByClick(Sender: TObject);
var
  item: TMenuItem;
  col, tmpCol: TEasyColumn;
  view: TCECustomFileView;
begin
  item:= TMenuItem(Sender);
  if item.Tag = -1 then
  begin
    if GlobalPathCtrl.ActivePage is TCEFileViewPage then
    begin
      TCEFileViewPage(GlobalPathCtrl.ActivePage).ShowHeaderSelector;
    end;
  end
  else
  begin
    col:= TEasyColumn(item.Tag);
    view:= TCECustomFileView(col.OwnerListview);
    view.BeginUpdate;
    try
      tmpCol:= view.Header.FirstColumn;
      while assigned(tmpCol) do
      begin
        if tmpCol <> col then
        tmpCol.SortDirection:= esdNone
        else
        tmpCol.SortDirection:= esdAscending;
        tmpCol:= view.Header.NextColumn(tmpCol);
      end;
    finally
      view.EndUpdate(true);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do GlobalContextMenuCmd
-------------------------------------------------------------------------------}
procedure TCEActions.DoGlobalContextMenuCmd(Namespace: TNamespace; Verb:
    String; MenuItemID: Integer;  var Handled: Boolean);
begin
  dCE_Actions.DoGlobalContextMenuCmd(MainForm, Namespace, Verb, MenuItemID, Handled);
end;


{-------------------------------------------------------------------------------
  Do GlobalContextMenuShow
-------------------------------------------------------------------------------}
procedure TCEActions.DoGlobalContextMenuShow(Namespace: TNamespace; Menu:
    hMenu; var Allow: Boolean);
begin
  dCE_Actions.DoGlobalContextMenuShow(MainForm, Namespace, Menu, Allow);
end;

{-------------------------------------------------------------------------------
  Do GroupBy Click
-------------------------------------------------------------------------------}
procedure TCEActions.DoGroupByClick(Sender: TObject);
var
  item: TMenuItem;
  col: TEasyColumn;
  page: TCEFileViewPage;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  page:= TCEFileViewPage(GlobalPathCtrl.ActivePage)
  else
  Exit;

  item:= TMenuItem(Sender);
  if item.Tag = -2 then
  begin
    page.FileView.Grouped:= not page.FileView.Grouped;
  end
  else if item.Tag = -1 then
  begin
    page.ShowHeaderSelector;
  end
  else
  begin
    col:= TEasyColumn(item.Tag);
    if assigned(col) then
    page.FileView.GroupingColumn:= col.Index;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Global ContextPopup
-------------------------------------------------------------------------------}
procedure TCEActions.HandleGlobalContextPopup(Sender: TObject; MousePos:
    TPoint; var Handled: Boolean);
begin
  if Sender = MainForm then
  begin
    MousePos:= MainForm.ClientToScreen(MousePos);
    MainForm.PanelsPopupMenu.Popup(MousePos.X, MousePos.Y);
  end;
  Handled:= true;
end;

{-------------------------------------------------------------------------------
  Update All actions
-------------------------------------------------------------------------------}
procedure TCEActions.UpdateAll;
var
  act: TAction;
  i: Integer;
begin
  UpdateTimer.Enabled:= false;

  if MainForm.CEIsClosing then
  Exit;

  try
    for i:= 0 to ActionList.ActionCount - 1 do
    begin
      act:= TAction(ActionList.Actions[i]);
      UpdateCEAction(act.Tag, act);
    end;
  finally
    UpdateTimer.Enabled:= true;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEHotkeySettings
-------------------------------------------------------------------------------}
constructor TCEHotkeySettings.Create;
begin
  inherited;
  fModifiedActions:= TObjectList.Create(false);
end;

{-------------------------------------------------------------------------------
  Destroy TCEHotkeySettings
-------------------------------------------------------------------------------}
destructor TCEHotkeySettings.Destroy;
begin
  fModifiedActions.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Load
-------------------------------------------------------------------------------}
procedure TCEHotkeySettings.Load(AAppStorage: TCEAppSettings; ANode: TDOMNode);
var
  chNode: TDOMNode;
  act: TAction;
  i: Integer;
  s: TShortcut;
  list: TStrings;
begin
  if not assigned(Actions) then
  Exit;

  if ANode.HasChildNodes then
  begin
    fModifiedActions.Clear;

    // load shortcuts
    list:= TStringList.Create;
    try
      list.Delimiter:= ',';
      // get action node
      chNode:= ANode.FirstChild;
      while assigned(chNode) do
      begin
        // find action
        act:= FindActionByName(Actions, chNode.NodeName);
        if assigned(act) then
        begin
          // clear default shortcuts
          act.ShortCut:= 0;
          act.SecondaryShortCuts.Clear;
          fModifiedActions.Add(act);
          // add shortcuts
          list.DelimitedText:= chNode.TextContent;
          for i:= 0 to list.Count - 1 do
          begin
            s:= StrToIntDef(list.Strings[i], 0);
            if (i = 0) or (act.ShortCut = 0) then
            act.ShortCut:= s
            else
            begin
              if s <> 0 then
              act.SecondaryShortCuts.AddObject(ShortCutToText(s), TObject(s));
            end;
          end;
        end;
        // get next action node
        chNode:= chNode.NextSibling;
      end;
    finally
      list.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Save
-------------------------------------------------------------------------------}
procedure TCEHotkeySettings.Save(AAppStorage: TCEAppSettings; ANode: TDOMNode);
var
  chNode: TDOMNode;
  act: TAction;
  i,i2: Integer;
  s: String;
begin
  if assigned(Actions) then
  begin
    // clear all first
    TDOMElementHack(ANode).FreeChildren;
    // loop modified actions
    for i:= 0 to ModifiedActions.Count - 1 do
    begin
      act:= TAction(ModifiedActions.Items[i]);
      // create node with action's name
      chNode:= AAppStorage.XML.CreateElement(act.Name);
      ANode.AppendChild(chNode);
      if act.ShortCut <> 0 then
      begin
        // convert shortcuts to string: '32,144,98...'
        s:= IntToStr(act.ShortCut);
        for i2:= 0 to act.SecondaryShortCuts.Count - 1 do
        s:= s + ',' + IntToStr(act.SecondaryShortCuts.ShortCuts[i2]);
        // add shortcuts to node's text content
        chNode.TextContent:= s;
      end;
    end;
  end;
end;



{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEGlobalHotkeys
-------------------------------------------------------------------------------}
constructor TCEGlobalHotkeys.Create;
begin
  inherited;
  fHotkeyActions:= TObjectList.Create(false);
  fHotkeys:= TList.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCEGlobalHotkeys
-------------------------------------------------------------------------------}
destructor TCEGlobalHotkeys.Destroy;
begin
  Clear;
  fHotkeys.Free;
  fHotkeyActions.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add Hotkey (Return index if RegisterHotKey succeeded, else -1)
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.AddHotkey(AAction: TAction; AHotkey:
    TShortcut): Integer;
begin
  Result:= -1;
  if IsRegistered then
  Raise Exception.CreateFmt('Unregister hotkeys before modifying list!', []);

  if assigned(AAction) and (AHotkey <> 0) then
  begin
    Result:= fHotkeyActions.Add(AAction);
    fHotkeys.Add(Pointer(AHotkey));
  end;
end;

{-------------------------------------------------------------------------------
  Can Register (returns true if can register)
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.CanRegister(AAction: TAction; AHotkey: TShortcut):
    Boolean;
var
  key: Word;
  shift: TShiftState;
  i: Integer;
begin
  Result:= false;
  if assigned(AAction) and (AHotkey <> 0) then
  begin
    // Check if hotkey is already on the list
    for i:= 0 to fHotkeys.Count - 1 do
    begin
      if AHotkey = TShortcut(fHotkeys.Items[i]) then
      Exit;
    end;
    // Register Hotkey
    ShortCutToKey(AHotkey, key, shift);
    if RegisterHotKey(MsgHandle, Integer(AAction), ShiftState2Modifier(shift), key) then
    begin
      Result:= true;
      UnregisterHotKey(MsgHandle, Integer(AAction));
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.Clear;
begin
  UnRegisterAll;
  fHotkeyActions.Clear;
  fHotkeys.Clear;
end;

{-------------------------------------------------------------------------------
  Delete Hotkey
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.DeleteHotkey(Index: Integer);
begin
  if IsRegistered then
  Raise Exception.CreateFmt('Unregister hotkeys before modifying list!', []);

  if (Index > -1) and (Index < fHotkeyActions.Count) then
  begin
    fHotkeyActions.Delete(Index);
    fHotkeys.Delete(Index);
  end;
end;

{-------------------------------------------------------------------------------
  Execute Hotkey
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.ExecuteHotkey(id: Integer);
begin
  if (id > -1) and (id < fHotkeyActions.Count) then
  begin
    TAction(fHotkeyActions.Items[id]).Execute;
  end;
end;

{-------------------------------------------------------------------------------
  Get Action
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.GetAction(AIndex: Integer): TAction;
begin
  Result:= TAction(fHotkeyActions.Items[AIndex]);
end;

{-------------------------------------------------------------------------------
  Get Count
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.GetCount: Integer;
begin
  Result:= fHotkeyActions.Count;
end;

{-------------------------------------------------------------------------------
  Get Hotkey (by index)
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.GetHotkey(AIndex: Integer): TShortcut;
begin
  Result:= Integer(fHotkeys.Items[AIndex]);
end;

{-------------------------------------------------------------------------------
  Get Hotkey (by action)
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.GetHotkey(AAction: TAction): TShortcut;
var
  i: Integer;
begin
  Result:= 0;
  i:= fHotkeyActions.IndexOf(AAction);
  if i > -1 then
  Result:= TShortcut(fHotkeys.Items[i]);
end;

{-------------------------------------------------------------------------------
  Get Hotkeys (Returns number of hotkeys found. Object field in the list will contain index of the hotkey)
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.GetHotkeys(AAction: TAction; AResults: TStrings):
    Integer;
var
  i: Integer;
begin
  Result:= 0;
  if assigned(AResults) then
  begin
    AResults.Clear;
    for i:= 0 to fHotkeyActions.Count - 1 do
    begin
      if AAction = fHotkeyActions.Items[i] then
      begin
        AResults.AddObject(ShortCutToText(TShortcut(fHotkeys.Items[i])), TObject(i));
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Index Of
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.IndexOf(AAction: TAction): Integer;
begin
  Result:= fHotkeyActions.IndexOf(AAction);
end;

{-------------------------------------------------------------------------------
  Replace
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.Replace(AIndex: Integer; AAction: TAction; AHotkey:
    TShortcut);
begin
  if IsRegistered then
  Raise Exception.CreateFmt('Unregister hotkeys before modifying list!', []);

  fHotkeyActions.Items[AIndex]:= AAction;
  fHotkeys.Items[AIndex]:= Pointer(AHotkey);
end;

{-------------------------------------------------------------------------------
  Load
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.Load(AAppStorage: TCEAppSettings; ANode:
    TDOMNode);
var
  chNode: TDOMNode;
  act: TAction;
begin
  if not assigned(Actions) then
  Exit;

  UnRegisterAll;

  if ANode.HasChildNodes then
  begin
    // Clear default shortcuts
    Clear;
    // get action node
    chNode:= ANode.FirstChild;
    while assigned(chNode) do
    begin
      // find action
      act:= FindActionByName(Actions, chNode.NodeName);
      if assigned(act) then
      begin
        // add hotkey
        AddHotkey(act, StrToIntDef(chNode.TextContent, 0));
      end;
      // get next action node
      chNode:= chNode.NextSibling;
    end;
  end;
  RegisterAll;
end;

{-------------------------------------------------------------------------------
  Save
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.Save(AAppStorage: TCEAppSettings; ANode:
    TDOMNode);
var
  chNode: TDOMNode;
  i: Integer;
begin
  // clear all first
  TDOMElementHack(ANode).FreeChildren;
  // loop actions
  for i:= 0 to fHotkeyActions.Count - 1 do
  begin
    // create node with action's name
    chNode:= AAppStorage.XML.CreateElement(TAction(fHotkeyActions.Items[i]).Name);
    ANode.AppendChild(chNode);
    // add hotkey to text content
    chNode.TextContent:= IntToStr(Integer(fHotkeys.Items[i]));
  end;
end;

{-------------------------------------------------------------------------------
  Register All
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.RegisterAll;
var
  i: Integer;
  key: Word;
  shift: TShiftState;
begin
  if not IsRegistered then
  begin
    for i:= 0 to fHotkeyActions.Count - 1 do
    begin
      ShortCutToKey(TShortcut(fHotkeys.Items[i]), key, shift);
      RegisterHotKey(MsgHandle, i, ShiftState2Modifier(shift), key);
    end;
    fIsRegistered:= true;
  end;
end;

{-------------------------------------------------------------------------------
  UnRegister All
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.UnRegisterAll;
var
  i: Integer;
begin
  if IsRegistered then
  begin
    for i:= 0 to fHotkeyActions.Count - 1 do
    begin
      UnregisterHotKey(MsgHandle, i);
    end;
    fIsRegistered:= false;
  end;
end;

end.
