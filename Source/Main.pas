//******************************************************************************
//  CubicExplorer
//  Version: 0.90
//
//  The Original Code is Main.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************

unit Main;

interface

uses
  fCE_TextEditorOptions,
//  TEST,
  // CubicCore
  ccStrings,
  ccSysUtils,
//  ccFileUtils,
  // CE Frames
  fCE_ExtAppPage, fCE_FiltersPanel, fCE_BookmarkPropDlg, fCE_DockHostForm,
  fCE_DockableForm, fCE_TabPage, fCE_FileView, fCE_FolderPanel,
  fCE_QuickViewPanel, fCE_BookmarkPanel, fCE_Customizer,
//  fCE_PoEditor,
  //CE_VersionUpdater,
  //fCE_PluginLog,
  VisualThemesGUI,
  // CE Data Modules
  dCE_Actions, dCE_Images, dCE_Input,
  // CE Units
  CE_Layout, CE_Sessions, CE_AppSettings,
  CE_Utils, CE_GlobalCtrl, CE_AddressToolbar, CE_DriveBar,
  CE_BookmarkBar, CE_StatusBar,
  //CE_VistaFuncs,
  CE_Breadcrumb,
  CE_ToolbarButtons, CE_TBActions,
  //CE_LanguageCodes,
  //CE_LanguageUtils,
  //CE_Engine,
  CE_SpTabBar, CE_Toolbar,
  AppCommand, CEJvDockVSNetStyleTBX,
  //CE_Plugins, CE_PluginsIntf,
  // Toolbar2000
  TB2Dock, TB2Item, TB2Toolbar, TB2ToolWindow, TB2ExtItems,
  // SpTBX
  SpTBXItem, SpTBXEditors, SpTBXTabs, SpTBXControls, SpTBXSkins,
  // VSTools
  VirtualTrees,
  VirtualShellNewMenu, VirtualShellNotifier, VirtualResources,
  EasyListview, VirtualExplorerEasyListview,
  MPCommonObjects,
  //MPShellTypes,
  MPShellUtilities,
  MPCommonUtilities,
  MPDataObject,
  //  Controls
  //System, ActnList, Classes, Menus, StdCtrls, SysUtils, Dialogs, Forms,
  // JVCL
  JvComponentBase, JvTrayIcon,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ShellAPI, Menus, ShlObj, XPMan, ActiveX,
  ImgList, System.Win.Registry, AppEvnts, ActnList, Math, Contnrs,
  //Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc,
  //JvAppXMLStorage,
  XMLRead, XMLWrite,
 // WideSupport,
  DOM;

{$REGION '============================================================================= > | FORM'}

type
  TMainFormSettings = class;

    TMainForm = class(TForm)
      TopToolDock: TSpTBXDock;
      RightToolDock: TSpTBXDock;
      BottomToolDock: TSpTBXDock;
      LeftToolDock: TSpTBXDock;
      MainToolbar: TCEToolbar;
      MainPanel: TPanel;
      AlphaSubmenuItem: TSpTBXSubmenuItem;
      ApplicationEvents: TApplicationEvents;
      AutoUpdateTimer: TTimer;
      bookmarkMenuItem: TSpTBXSubmenuItem;
      but_reset_layout: TSpTBXItem;
      editMenuItem: TSpTBXSubmenuItem;
      EditToolbar: TCEToolbar;
      fileMenuItem: TSpTBXSubmenuItem;
      helpMenuItem: TSpTBXSubmenuItem;
      item_lock_panels: TSpTBXItem;
      LanguageMenuItem: TSpTBXSubmenuItem;
      MainMenuPopupMenu: TSpTBXPopupMenu;
      NavigationToolbar: TCEToolbar;
      panel_curtain: TPanel;
      PanelsPopupMenu: TSpTBXPopupMenu;
      sep_tabs_copymove: TSpTBXSeparatorItem;
      sessionHistoryMenuItem: TSpTBXSubmenuItem;
      sessionsMenuItem: TSpTBXSubmenuItem;
      SkinGroupItem: TSpTBXSkinGroupItem;
      SpTBXItem1: TSpTBXItem;
      SpTBXItem2: TSpTBXItem;
      SpTBXItem3: TSpTBXItem;
      SpTBXItem4: TSpTBXItem;
      SpTBXItem5: TSpTBXItem;
      SpTBXItem6: TSpTBXItem;
      SpTBXItem7: TSpTBXItem;
      SpTBXItem8: TSpTBXItem;
      SpTBXItem9: TSpTBXItem;
      SpTBXItem10: TSpTBXItem;
      SpTBXItem11: TSpTBXItem;
      SpTBXItem12: TSpTBXItem;
      SpTBXItem13: TSpTBXItem;
      SpTBXItem14: TSpTBXItem;
      SpTBXItem15: TSpTBXItem;
      SpTBXItem16: TSpTBXItem;
      SpTBXItem17: TSpTBXItem;
      SpTBXItem18: TSpTBXItem;
      SpTBXItem19: TSpTBXItem;
      SpTBXItem20: TSpTBXItem;
      SpTBXItem21: TSpTBXItem;
      SpTBXItem22: TSpTBXItem;
      SpTBXItem23: TSpTBXItem;
      SpTBXItem24: TSpTBXItem;
      SpTBXItem25: TSpTBXItem;
      SpTBXItem26: TSpTBXItem;
      SpTBXItem27: TSpTBXItem;
      SpTBXItem28: TSpTBXItem;
      SpTBXItem29: TSpTBXItem;
      SpTBXItem30: TSpTBXItem;
      SpTBXItem31: TSpTBXItem;
      SpTBXItem32: TSpTBXItem;
      SpTBXItem33: TSpTBXItem;
      SpTBXItem34: TSpTBXItem;
      SpTBXItem35: TSpTBXItem;
      SpTBXItem36: TSpTBXItem;
      SpTBXItem37: TSpTBXItem;
      SpTBXItem38: TSpTBXItem;
      SpTBXItem39: TSpTBXItem;
      SpTBXItem40: TSpTBXItem;
      SpTBXItem41: TSpTBXItem;
      SpTBXItem42: TSpTBXItem;
      SpTBXItem43: TSpTBXItem;
      SpTBXItem44: TSpTBXItem;
      SpTBXItem45: TSpTBXItem;
      SpTBXItem46: TSpTBXItem;
      SpTBXItem47: TSpTBXItem;
      SpTBXItem48: TSpTBXItem;
      SpTBXItem49: TSpTBXItem;
      SpTBXItem50: TSpTBXItem;
      SpTBXItem51: TSpTBXItem;
      SpTBXItem52: TSpTBXItem;
      SpTBXItem53: TSpTBXItem;
      SpTBXItem54: TSpTBXItem;
      SpTBXItem55: TSpTBXItem;
      SpTBXItem56: TSpTBXItem;
      SpTBXItem57: TSpTBXItem;
      SpTBXItem58: TSpTBXItem;
      SpTBXItem59: TSpTBXItem;
      SpTBXItem60: TSpTBXItem;
      SpTBXItem61: TSpTBXItem;
      SpTBXItem62: TSpTBXItem;
      SpTBXItem63: TSpTBXItem;
      SpTBXItem64: TSpTBXItem;
      SpTBXItem65: TSpTBXItem;
      SpTBXItem66: TSpTBXItem;
      SpTBXItem67: TSpTBXItem;
      SpTBXItem68: TSpTBXItem;
      SpTBXItem69: TSpTBXItem;
      SpTBXItem70: TSpTBXItem;
      SpTBXItem71: TSpTBXItem;
      SpTBXItem72: TSpTBXItem;
      SpTBXItem73: TSpTBXItem;
      SpTBXItem74: TSpTBXItem;
      SpTBXItem75: TSpTBXItem;
      SpTBXItem76: TSpTBXItem;
      SpTBXItem77: TSpTBXItem;
      SpTBXItem78: TSpTBXItem;
      SpTBXItem79: TSpTBXItem;
      SpTBXItem80: TSpTBXItem;
      SpTBXItem81: TSpTBXItem;
      SpTBXItem82: TSpTBXItem;
      SpTBXItem83: TSpTBXItem;
      SpTBXItem84: TSpTBXItem;
      SpTBXItem85: TSpTBXItem;
      SpTBXItem86: TSpTBXItem;
      SpTBXItem87: TSpTBXItem;
      SpTBXItem88: TSpTBXItem;
      SpTBXItem89: TSpTBXItem;
      SpTBXItem90: TSpTBXItem;
      SpTBXItem91: TSpTBXItem;
      SpTBXItem92: TSpTBXItem;
      SpTBXItem93: TSpTBXItem;
      SpTBXItem94: TSpTBXItem;
      SpTBXItem95: TSpTBXItem;
      SpTBXItem96: TSpTBXItem;
      SpTBXItem97: TSpTBXItem;
      SpTBXItem98: TSpTBXItem;
      SpTBXItem99: TSpTBXItem;
      SpTBXItem100: TSpTBXItem;
      SpTBXSeparatorItem1: TSpTBXSeparatorItem;
      SpTBXSeparatorItem2: TSpTBXSeparatorItem;
      SpTBXSeparatorItem3: TSpTBXSeparatorItem;
      SpTBXSeparatorItem4: TSpTBXSeparatorItem;
      SpTBXSeparatorItem5: TSpTBXSeparatorItem;
      SpTBXSeparatorItem6: TSpTBXSeparatorItem;
      SpTBXSeparatorItem7: TSpTBXSeparatorItem;
      SpTBXSeparatorItem8: TSpTBXSeparatorItem;
      SpTBXSeparatorItem9: TSpTBXSeparatorItem;
      SpTBXSeparatorItem10: TSpTBXSeparatorItem;
      SpTBXSeparatorItem11: TSpTBXSeparatorItem;
      SpTBXSeparatorItem12: TSpTBXSeparatorItem;
      SpTBXSeparatorItem13: TSpTBXSeparatorItem;
      SpTBXSeparatorItem14: TSpTBXSeparatorItem;
      SpTBXSeparatorItem15: TSpTBXSeparatorItem;
      SpTBXSeparatorItem16: TSpTBXSeparatorItem;
      SpTBXSeparatorItem17: TSpTBXSeparatorItem;
      SpTBXSeparatorItem18: TSpTBXSeparatorItem;
      SpTBXSeparatorItem19: TSpTBXSeparatorItem;
      SpTBXSeparatorItem20: TSpTBXSeparatorItem;
      SpTBXSeparatorItem21: TSpTBXSeparatorItem;
      SpTBXSeparatorItem22: TSpTBXSeparatorItem;
      SpTBXSeparatorItem23: TSpTBXSeparatorItem;
      SpTBXSeparatorItem24: TSpTBXSeparatorItem;
      SpTBXSeparatorItem25: TSpTBXSeparatorItem;
      SpTBXSeparatorItem26: TSpTBXSeparatorItem;
      SpTBXSeparatorItem27: TSpTBXSeparatorItem;
      SpTBXSeparatorItem28: TSpTBXSeparatorItem;
      SpTBXSeparatorItem29: TSpTBXSeparatorItem;
      SpTBXSeparatorItem30: TSpTBXSeparatorItem;
      SpTBXSeparatorItem31: TSpTBXSeparatorItem;
      SpTBXSeparatorItem32: TSpTBXSeparatorItem;
      SpTBXSubmenuItem1: TSpTBXSubmenuItem;
      SpTBXSubmenuItem6: TSpTBXSubmenuItem;
      StartUpTimer: TTimer;
      sub_closed_tab_list: TSpTBXSubmenuItem;
      sub_panels: TSpTBXSubmenuItem;
      TabPopupMenu: TSpTBXPopupMenu;
      ToolbarPopupMenu: TSpTBXPopupMenu;
      toolbarsMenuItem: TSpTBXSubmenuItem;
      toolsMenuItem: TSpTBXSubmenuItem;
      TrayIcon: TJvTrayIcon;
      TrayPopupMenu: TSpTBXPopupMenu;
      viewMenuItem: TSpTBXSubmenuItem;
      ViewToolbar: TCEToolbar;
      SpTBXItem101: TSpTBXItem;
      SpTBXItem102: TSpTBXItem;
      SpTBXItem103: TSpTBXItem;
      SpTBXItem104: TSpTBXItem;
      procedure AutoUpdateTimerTimer(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure TransparencyClick(Sender: TObject);
      procedure TransparencyPopup(Sender: TTBCustomItem; FromLink: Boolean);
      procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
      procedure FormShow(Sender: TObject);
//      procedure LanguageMenuItemPopup(Sender: TTBCustomItem; FromLink: Boolean);
      procedure StartUpTimerTimer(Sender: TObject);
      procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
      procedure sub_closed_tab_listPopup(Sender: TTBCustomItem; FromLink: Boolean);
      procedure TabPopupMenuPopup(Sender: TObject);
      procedure FormResize(Sender: TObject);
      procedure TrayIconClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure TrayIconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure TrayPopupMenuPopup(Sender: TObject);
      procedure SpTBXItem101Click(Sender: TObject);
      procedure SpTBXItem102Click(Sender: TObject);
      procedure SpTBXItem103Click(Sender: TObject);
      procedure SpTBXItem104Click(Sender: TObject);

    private
      fFullscreen: Boolean;
      fActiveLanguage: String;
      fCEIsClosing: Boolean;
      fLanguageList: TStrings;
      fLockToolbars: Boolean;
      fOldWindowState: TWindowState;
      fPanels: TComponentList;
      fPathInTitle: Boolean;
      fSaveSettingsOnClose: Boolean;
      fSingleInstance: Boolean;
      fUpdatingCount: Integer;
      procedure SetFullscreen(const Value: Boolean);
//      procedure SetActiveLanguage(const Value: String);
      procedure SetLockToolbars(const Value: Boolean);
      procedure SetPathInTitle(const Value: Boolean);
      procedure SetSingleInstance(const Value: Boolean);
    protected
      fIsReady: Boolean;
      procedure ForceIconCachRebuild;
//      procedure GetLanguageList;
      procedure GetSkinsFromFolder(AFolderPath: String);
      procedure ConvertCustomActions(Root: TTBCustomItem; Recursive: Boolean = true);
//      procedure InitLanguage;
//      procedure LanguageItemClick(Sender: TObject);
      procedure WMDeviceChange(var Message: TMessage); message WM_DEVICECHANGE;
      procedure WMPowerBroadcast(var Message: TMessage); message WM_POWERBROADCAST;
      procedure WMShellNotify(var Msg: TMessage); message WM_SHELLNOTIFY;
      procedure WMSyscommand(var Message: TWmSysCommand); message WM_SYSCOMMAND;
      procedure CreateParams(var Params: TCreateParams); override;
      procedure HandleTabSetOleDrop(ASender: TCESpTabSet; ADropTab: TCESpTabItem; const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint; var hr: HResult);
//      procedure HandleUpdateFound(Sender: TObject; BuildType: TCEBuildType; Version: TCEVersionNumber; Notes: String; var DoUpdate: Boolean);
      procedure WMAppCommand(var Message: TWMAppCommand); message WM_APPCOMMAND;
      procedure WMHotkey(var Message: TMessage); message WM_HOTKEY;
    public
      DockHostForm: TCEDockHostForm;
      Layouts: TCELayoutController;
      TabSet: TCESpTabSet;
      AddressBarToolbar: TCEAddressBarToolbar;
//      AutoUpdater: TCEAutoUpdater;
      BreadcrumbBar: TCEBreadcrumbBar;
      DriveToolbar: TCEDriveToolbar;
      BookmarkToolbar: TCEBookmarkToolbar;
      SessionsToolbar: TCESessionsToolbar;
      Settings: TMainFormSettings;
      StatusBar: TCEStatusBar;
      procedure BeginUIUpdate;
//      procedure DoCustomTranslate;
      procedure EndUIUpdate;
      procedure InitializeUI;
      procedure FinalizeUI;
      procedure MakeVisible;
//      procedure MenuItemTranslateHandler(Obj: TObject; var IsIgnored: Boolean);
      procedure OpenSkin;
      procedure Shutdown;
      procedure StartUp;
      procedure ToggleVisibility;
//      procedure TranslateUI(Sender: TObject);
      property Fullscreen: Boolean read fFullscreen write SetFullscreen;
//      property ActiveLanguage: String read fActiveLanguage write SetActiveLanguage;
      property LockToolbars: Boolean read fLockToolbars write SetLockToolbars;
      property Panels: TComponentList read fPanels write fPanels;
      property PathInTitle: Boolean read fPathInTitle write SetPathInTitle;
      property SaveSettingsOnClose: Boolean read fSaveSettingsOnClose write fSaveSettingsOnClose;
      property SingleInstance: Boolean read fSingleInstance write SetSingleInstance;
    published
      property CEIsClosing: Boolean read fCEIsClosing;


end;

  TCEStartupType = (stNormal, stSession, stLastSession);

  TMainFormSettings = class(TPersistent)

  private
    fAutoCheckUpdates: Boolean;
    fCloseToTray: Boolean;
    fExitOnLastTabClose: Boolean;
    fHeight: Integer;
    fLeft: Integer;
    fMinimizeToTray: Boolean;
    fShowCmd: Integer;
    fStartInTray: Boolean;
    fStartupType: TCEStartupType;
    fTop: Integer;
    fWidth: Integer;
    function GetAlphaBlend: Integer;
    function GetAlwaysOnTop: Boolean;
//    function GetLanguage: String;
    function GetPathInTitle: Boolean;
    function GetShowHint: Boolean;
    function GetSingleInstance: Boolean;
    function GetSkin: string;
    function GetAutoLoadSession: String;
    function GetChangeCurrentDirVar: Boolean;
//    function GetCheckForUpdateTypes: TCEBuildTypes;
    function GetLastUpdateCheck: TDateTime;
    function GetLockToolbars: Boolean;
    function GetProxyAddress: String;
    function GetProxyPassword: string;
    function GetProxyUsername: string;
    function GetShowTray: Boolean;
    function GetUpdateURL: String;
    function GetUseProxy: Boolean;
    function GetUseSystemProxy: Boolean;
    procedure SetAlphaBlend(const Value: Integer);
    procedure SetAlwaysOnTop(const Value: Boolean);
//    procedure SetLanguage(const Value: String);
    procedure SetPathInTitle(const Value: Boolean);
    procedure SetShowHint(const Value: Boolean);
    procedure SetSingleInstance(const Value: Boolean);
    procedure SetSkin(const Value: string);
    procedure SetAutoLoadSession(const Value: String);
    procedure SetChangeCurrentDirVar(const Value: Boolean);
//    procedure SetCheckForUpdateTypes(const Value: TCEBuildTypes);
    procedure SetLastUpdateCheck(const Value: TDateTime);
    procedure SetLockToolbars(const Value: Boolean);
    procedure SetProxyAddress(const Value: String);
    procedure SetProxyPassword(const Value: string);
    procedure SetProxyUsername(const Value: string);
    procedure SetShowTray(const Value: Boolean);
    procedure SetUpdateURL(const Value: String);
    procedure SetUseProxy(const Value: Boolean);
    procedure SetUseSystemProxy(const Value: Boolean);
  protected
    fWow64Enabled: Boolean;
    function GetFullscreen: Boolean;
    function GetLockPanels: Boolean;
    procedure SetFullscreen(const Value: Boolean);
    procedure SetLockPanels(const Value: Boolean);
    procedure SetWow64Enabled(const Value: Boolean);
  public
    Form: TMainForm;
    constructor Create;
    procedure UpdatePositionInfo;
    procedure ApplyPositionInfo(AHideForm: Boolean = false);
  published
    property AlphaBlend: Integer read GetAlphaBlend write SetAlphaBlend;
    property AlwaysOnTop: Boolean read GetAlwaysOnTop write SetAlwaysOnTop;
    property AutoCheckUpdates: Boolean read fAutoCheckUpdates write fAutoCheckUpdates;
    property Left: Integer read fLeft write fLeft;
    property Top: Integer read fTop write fTop;
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;
//    property Language: String read GetLanguage write SetLanguage;
    property PathInTitle: Boolean read GetPathInTitle write SetPathInTitle;
    property ShowCmd: Integer read fShowCmd write fShowCmd;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
    property SingleInstance: Boolean read GetSingleInstance write SetSingleInstance;
    property Skin: string read GetSkin write SetSkin;
    property AutoLoadSession: String read GetAutoLoadSession write SetAutoLoadSession;
    property ChangeCurrentDirVar: Boolean read GetChangeCurrentDirVar write SetChangeCurrentDirVar;
//    property CheckForUpdateTypes: TCEBuildTypes read GetCheckForUpdateTypes write SetCheckForUpdateTypes;
    property CloseToTray: Boolean read fCloseToTray write fCloseToTray;
    property ExitOnLastTabClose: Boolean read fExitOnLastTabClose write fExitOnLastTabClose;
    property Fullscreen: Boolean read GetFullscreen write SetFullscreen;
    property LastUpdateCheck: TDateTime read GetLastUpdateCheck write SetLastUpdateCheck;
    property LockPanels: Boolean read GetLockPanels write SetLockPanels;
    property LockToolbars: Boolean read GetLockToolbars write SetLockToolbars;
    property MinimizeToTray: Boolean read fMinimizeToTray write fMinimizeToTray;
    property ProxyAddress: String read GetProxyAddress write SetProxyAddress;
    property ProxyPassword: string read GetProxyPassword write SetProxyPassword;
    property ProxyUsername: string read GetProxyUsername write SetProxyUsername;
    property ShowTray: Boolean read GetShowTray write SetShowTray;
    property StartInTray: Boolean read fStartInTray write fStartInTray;
    property StartupType: TCEStartupType read fStartupType write fStartupType;
    property UpdateURL: String read GetUpdateURL write SetUpdateURL;
    property UseProxy: Boolean read GetUseProxy write SetUseProxy;
    property UseSystemProxy: Boolean read GetUseSystemProxy write SetUseSystemProxy;
    property Wow64Enabled: Boolean read fWow64Enabled write SetWow64Enabled;

  end;

{$ENDREGION '========================================================================== > | FORM'}

var
  MainForm: TMainForm;

implementation

uses
  madExcept,
  MPResources,
  CE_Stacks, CE_Consts, CE_CommonObjects, CE_ElevatedActions, CE_FileUtils,
  fCE_OptionsDialog, fCE_StackPanel, fCE_WorkspacePanel,
  Clipbrd, ComCtrls;

{$R *.dfm}

{*------------------------------------------------------------------------------
  BeginUIUpdate
-------------------------------------------------------------------------------}
procedure TMainForm.BeginUIUpdate;
begin
  fUpdatingCount := fUpdatingCount + 1;
  if fUpdatingCount > 1 then
    Exit;
  //LockWindowUpdate(GetDesktopWindow); // this should be used for drag operations only!
  SendMessage(MainForm.DockHostForm.Handle, WM_SETREDRAW, 0, 0);
  Self.TopToolDock.BeginUpdate;
  Self.RightToolDock.BeginUpdate;
  Self.BottomToolDock.BeginUpdate;
  Self.LeftToolDock.BeginUpdate;
  Self.MainToolbar.BeginUpdate;
end;


{*------------------------------------------------------------------------------
  EndUIUpdate
-------------------------------------------------------------------------------}
procedure TMainForm.EndUIUpdate;
begin
  if fUpdatingCount = 0 then
    Exit;
  fUpdatingCount := fUpdatingCount - 1;
  if fUpdatingCount > 0 then
    Exit;

  DockHostForm.DockServer.LeftDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.TopDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.BottomDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.RightDockPanel.DockManager.ResetBounds(true);

  Self.TopToolDock.EndUpdate;
  Self.RightToolDock.EndUpdate;
  Self.BottomToolDock.EndUpdate;
  Self.LeftToolDock.EndUpdate;
  Self.MainToolbar.EndUpdate;
  //LockWindowUpdate(0);
  SendMessage(MainForm.DockHostForm.Handle, WM_SETREDRAW, 1, 0);
  RedrawWindow(MainForm.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

{*------------------------------------------------------------------------------
  Get's called when MainForm is created.
-------------------------------------------------------------------------------}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // show curtain
  // - we added the curtain panel in design time because otherwise toolbars will
  //  flash on screen before the curtain is drawn.
  panel_curtain.BringToFront;

  // Fix date formating bug in Delphi
//  if IsWindowsVista then
//  begin
//    SetThreadLocale(LOCALE_USER_DEFAULT);
//    GetFormatSettings;
//  end;

  // fix taskbar button for Vista+
  ShowWindow(Application.Handle, SW_HIDE);
  SetWindowLong(Application.Handle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE) and not WS_EX_APPWINDOW or WS_EX_TOOLWINDOW);
  ShowWindow(Application.Handle, SW_SHOW);

  // create instance
  fLanguageList := TStringList.Create;
  fLanguageList.NameValueSeparator := '=';
    // mainform settings
  Settings := TMainFormSettings.Create;
  Settings.Form := Self;
  GlobalAppSettings.AddItem('MainForm', Settings, true, true);

  fPanels := TComponentList.Create(false);

  // init values
  fIsReady := false;
  fUpdatingCount := 0;
  fCEIsClosing := false;
  fPathInTitle := false;
  fSaveSettingsOnClose := true;
  StackDirPath := SettingsDirPath + 'Stacks\';

//  SetVistaFont(Self.Font);
  Self.OnContextPopup := CEActions.HandleGlobalContextPopup;

  // VSTools stuff
  ChangeNotifier.RegisterShellChangeNotify(Self);

  if Assigned(MP_SHSetInstanceExplorer) then
    MP_SHSetInstanceExplorer(ExplorerThreadInstance);
  if Assigned(MP_SHSetThreadRef) then
    MP_SHSetThreadRef(ExplorerThreadInstance);
end;

{-------------------------------------------------------------------------------
  Create Params
-------------------------------------------------------------------------------}
procedure TMainForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle and not WS_EX_TOOLWINDOW or WS_EX_APPWINDOW;
end;

{*------------------------------------------------------------------------------
  Get's called when MainForm is destroyed.
-------------------------------------------------------------------------------}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ChangeNotifier.UnRegisterShellChangeNotify(Self);

  fIsReady := false;
  CEActions.UpdateTimer.Enabled := false;

  // unload plugins
//  GlobalPluginHost.Finalize;

  // finalize UI
  FinalizeUI;

  // free instances
  fLanguageList.Free;
  Settings.Free;
  fPanels.Free;
end;

{*------------------------------------------------------------------------------
  Initialize MainForm UI.
-------------------------------------------------------------------------------}
procedure TMainForm.InitializeUI;
begin
//  SetDesktopIconFonts(Font);
  // Create Data Modules
  CEActions := TCEActions.Create(self);
  CE_Images := TCE_Images.Create(self);
  CEInput := TCEInput.Create(self);

  // Assign settings
  GlobalAppSettings.AddItem('TextEditor', GlobalTextEditorSettings, true);

  // Create/Init layout controller
  Layouts := TCELayoutController.Create(nil);
  // Create/Init DockingFrame
  DockHostForm := TCEDockHostForm.Create(self);
  DockHostForm.Name := 'DockHost';
  DockHostForm.Parent := MainPanel;
  DockHostForm.Align := alClient;
  DockHostForm.Show;

  // Setup MainMenuPopup
  MainMenuPopupMenu.LinkSubitems := MainToolbar.Items;

  // Create TabSet
  TabSet := TCESpTabSet.Create(nil);
  tabSet.OnOleDrop := HandleTabSetOleDrop;
  TabSet.Parent := MainPanel;
  TabSet.Align := alTop;
  TabSet.Name := 'TabBar';
  TabSet.Toolbar.Caption := 'Tabs';//_('Tabs');
  TabSet.Toolbar.Name := 'TabToolbar';
  TabSet.TabDragReorder := true;
  TabSet.TabPageHost := DockHostForm.CenterPanel;
  TabSet.LayoutController := Layouts;
  TabSet.Toolbar.Customizable := true;
  TabSet.Images := CE_Images.SmallIcons;
  //TabSet.TabPopupMenu:= TabPopupMenu;
  TabSet.PopupMenu := TabPopupMenu;
  GlobalFocusCtrl.CtrlList.Add(TabSet.Toolbar);

  //TCESpTabToolbar(TabSet.Toolbar).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;

  // Create FolderCombo Toolbar
  AddressBarToolbar := TCEAddressBarToolbar.Create(self);
  AddressBarToolbar.Name := 'AddressBarToolbar';
  AddressBarToolbar.Caption := 'Address Bar'; //_('Address Bar');
  AddressBarToolbar.DockableTo := [TB2Dock.dpTop, TB2Dock.dpBottom];
  AddressBarToolbar.Visible := false;
  AddressBarToolbar.CurrentDock := TopToolDock;
  AddressBarToolbar.Tag := 1;
  // Create DriveToolbar
  DriveToolbar := TCEDriveToolbar.Create(self);
  DriveToolbar.Name := 'DriveToolbar';
  DriveToolbar.Caption := 'Drives';//_('Drives');
  DriveToolbar.Stretch := true;
  DriveToolbar.ChevronMoveItems := false;
  DriveToolbar.Visible := false;
  DriveToolbar.CurrentDock := TopToolDock;
  DriveToolbar.Populate;
  DriveToolbar.PopupMenu := ToolbarPopupMenu;
  DriveToolbar.Tag := 1;
  // Create BookmarkBar
  BookmarkToolbar := TCEBookmarkToolbar.Create(self);
  BookmarkToolbar.Name := 'BookmarkToolbar';
  BookmarkToolbar.Caption := 'Bookmarks'; //_('Bookmarks');
  BookmarkToolbar.Stretch := true;
  BookmarkToolbar.ChevronMoveItems := false;
  BookmarkToolbar.Visible := false;
  BookmarkToolbar.CurrentDock := TopToolDock;
  BookmarkToolbar.PopupMenu := ToolbarPopupMenu;
  BookmarkToolbar.Tag := 1;
  // Create SessionsBar
  SessionsToolbar := TCESessionsToolbar.Create(self);
  SessionsToolbar.Name := 'SessionsToolbar';
  SessionsToolbar.Caption := 'Sessions';//_('Sessions');
  SessionsToolbar.Stretch := true;
  SessionsToolbar.ChevronMoveItems := false;
  SessionsToolbar.Visible := false;
  SessionsToolbar.CurrentDock := TopToolDock;
  SessionsToolbar.PopupMenu := ToolbarPopupMenu;
  SessionsToolbar.Tag := 1;
  // Create BreadcrumbBar
  BreadcrumbBar := TCEBreadcrumbBar.Create(self);
  BreadcrumbBar.Name := 'BreadcrumbBar';
  BreadcrumbBar.Caption := 'Breadcrumb Navigation'; //_('Breadcrumb Navigation');
  BreadcrumbBar.Stretch := true;
  BreadcrumbBar.Visible := false;
  BreadcrumbBar.DockableTo := [TB2Dock.dpTop, TB2Dock.dpBottom];
  BreadcrumbBar.CurrentDock := TopToolDock;
  BreadcrumbBar.PopupMenu := ToolbarPopupMenu;
  BreadcrumbBar.Tag := 1;
  // Create Status bar
  StatusBar := TCEStatusBar.Create(Self);
  StatusBar.Name := 'StatusBar';
  StatusBar.Parent := Self;
  panel_curtain.BringToFront; // bring curtain over statusbar
  StatusBar.Initialize;
  StatusBar.PopupMenu := MainMenuPopupMenu;
  GlobalPathCtrl.RegisterNotify(StatusBar);
  // Add Layout items
  CELayoutItems.Add(MainToolbar);
  CELayoutItems.Add(NavigationToolbar);
  CELayoutItems.Add(ViewToolbar);
  CELayoutItems.Add(AddressBarToolbar);
  CELayoutItems.Add(DriveToolbar);
  CELayoutItems.Add(BookmarkToolbar);
  CELayoutItems.Add(SessionsToolbar);
  CELayoutItems.Add(EditToolbar);
  CELayoutItems.Add(BreadcrumbBar);
  CELayoutItems.Add(TabSet);
  CELayoutItems.Add(StatusBar);
  // Add Toolbar Docks
  CEToolbarDocks.Add(LeftToolDock);
  CEToolbarDocks.Add(TopToolDock);
  CEToolbarDocks.Add(RightToolDock);
  CEToolbarDocks.Add(BottomToolDock);
  CEToolbarDocks.Add(DockHostForm.LeftGroupToolDock, true);
  CEToolbarDocks.Add(DockHostForm.TopGroupToolDock, true);
  CEToolbarDocks.Add(DockHostForm.RightGroupToolDock, true);
  CEToolbarDocks.Add(DockHostForm.BottomGroupToolDock, true);

  // Add Session menu items
  sessionsMenuItem.Add(TCESessionsMenuItem.Create(self));
  sessionHistoryMenuItem.Add(TCESessionHistoryMenuItem.Create(self));

  // Add custom menu items
  MainToolbar.BeginUpdate;
  try
    ConvertCustomActions(MainToolbar.Items);
  finally
    MainToolbar.EndUpdate;
  end;


  // Enable transparency control?
//  case GetWinVersion of
//    wvWin2000, wvWinXP, wvWin2003, wvWinVista:
      AlphaSubmenuItem.Visible := true;
//  end;

//  InitLanguage;

  // Create Auto Updater
//  AutoUpdater := TCEAutoUpdater.Create;
//  AutoUpdater.CurrentVersionStr := GetAppVersionStr;
//  AutoUpdater.VersionFolder := SettingsDirPath + 'Versions\';
//  AutoUpdater.OutputFolder := exePath;
//  AutoUpdater.OnUpdateFound := HandleUpdateFound;
end;

{*------------------------------------------------------------------------------
  Finalize MainForm UI.
-------------------------------------------------------------------------------}
procedure TMainForm.FinalizeUI;
begin
//  AutoUpdater.Free;
  TabSet.Free;
  Layouts.Free;
  DockHostForm.Free;
end;

{-------------------------------------------------------------------------------
  ForceIconCachRebuild
-------------------------------------------------------------------------------}
procedure TMainForm.ForceIconCachRebuild;
var
  Reg: TRegistry;
  LargeIconSize: integer;
begin
  Reg := TRegistry.Create;
  try
    try
      Reg.Access := KEY_READ or KEY_WRITE;
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Control Panel\Desktop\WindowMetrics', False) then
      begin
        FlushImageLists;
        { Flush the Icon Cache by changing the size of the icons }
        if Reg.ValueExists('Shell Icon Size') then
          LargeIconSize := StrToInt(Reg.ReadString('Shell Icon Size'))
        else
          LargeIconSize := GetSystemMetrics(SM_CXICON);
        Reg.WriteString('Shell Icon Size', IntToStr(LargeIconSize + 1));
        SendMessage(Handle, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS, Integer(PChar('WindowMetrics')));
        FileIconInit(True); // Flush the cached Icons
        Reg.WriteString('Shell Icon Size', IntToStr(LargeIconSize));
        SendMessage(Handle, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS, Integer(PChar('WindowMetrics')));
        FileIconInit(True); // Flush the cached Icons
      end;
    except // Quiet failure
    end
  finally
    Reg.Free;
  end
end;

{*------------------------------------------------------------------------------
  Load default startup settings.
-------------------------------------------------------------------------------}
procedure TMainForm.StartUp;

  procedure enumToolbar(item: TSpTBXItem);
  var
    i2: Integer;
    chItem: TSpTBXItem;
  begin
    for i2 := 0 to item.Count - 1 do
    begin
      if item.Items[i2] is TSpTBXItem then
      begin
        chItem := TSpTBXItem(item.Items[i2]);
        chItem.Caption := '&' + chItem.Caption;
        if chItem.ClassType = TSpTBXSubmenuItem then
          enumToolbar(chItem);
      end;
    end;
  end;

var
  i: Integer;
  TabsOpened: Boolean;
  ws: String;
begin
  //Wow64Enabled:= IsWindows64;

  // Do panel startup
  for i := 0 to Panels.Count - 1 do
  begin
    if Panels.Items[i] is TCECustomDockableForm then
    begin
      TCECustomDockableForm(Panels.Items[i]).DoStartUp;
    end;
  end;

  // Populate menu items
  CELayoutItems.PopulateMenuItem(toolbarsMenuItem);
  CELayoutItems.PopulateMenuItem(ToolbarPopupMenu.Items);


  // Load skins
  GetSkinsFromFolder(exePath + 'Skins\');
  SkinGroupItem.Recreate;

  // Load Sessions
  if FileExists(SettingsDirPath + 'sessions.xml') then
    GlobalSessions.LoadFromFile(SettingsDirPath + 'sessions.xml')
  else
    GlobalSessions.LoadFromFile(exePath + 'sessions.xml');
  SessionsToolbar.Recreate;

  // Set PerFolder Storage path
  GlobalFileViewSettings.StorageFilePath := SettingsDirPath + 'perfolder.dat';

  // Load Settings
  GlobalFileViewSettings.BeginUpdate;
  try
    if FileExists(SettingsDirPath + 'settings.xml') then
      GlobalAppSettings.LoadFromFile(SettingsDirPath + 'settings.xml')
    else if FileExists(exePath + 'settings.xml') then
      GlobalAppSettings.LoadFromFile(exePath + 'settings.xml')
    else
      GlobalAppSettings.LoadFromResource(hInstance, 'DEFAULT_SETTINGS_DATA');
  finally
    GlobalFileViewSettings.EndUpdate(true);
  end;
  Settings.ApplyPositionInfo(Settings.StartInTray);
  if not Settings.StartInTray then
    MainForm.Show;

  // TODO: remove this!!!!
  //ShowPluginLog;

  // Load Plugins
//  GlobalPluginHost.LoadPlugins(ExePath + 'Plugins\', 'dll', true);
//  if not WideIsSameText(SettingsDirPath, ExePath) then
//    GlobalPluginHost.LoadPlugins(SettingsDirPath + 'Plugins\', 'dll', true);
//  GlobalPluginHost.SetSettingsFolder(SettingsDirPath + 'Plugins\.settings\');

  // Load Bookmarks
  CEBookmarkPanel.BookmarkMenuItems.Add(BookmarkToolbar.Items);
  CEBookmarkPanel.BookmarkMenuItems.Add(bookmarkMenuItem);
  if FileExists(SettingsDirPath + 'bookmarks.xml') then
    CEBookmarkPanel.BookmarksPath := SettingsDirPath + 'bookmarks.xml'
  else
    CEBookmarkPanel.BookmarksPath := exePath + 'bookmarks.xml';
  CEBookmarkPanel.LoadBookmarks;
  CEBookmarkPanel.BookmarksPath := SettingsDirPath + 'bookmarks.xml';

  // Load Layouts
  Layouts.AutoSave := false;
  if FileExists(SettingsDirPath + 'layout.xml') then
    Layouts.LoadFromFile(SettingsDirPath + 'layout.xml')
  else
    Layouts.LoadFromFile(exePath + 'layout.xml');
  Layouts.LoadSettingsForToolbars;
  Layouts.FilePath := SettingsDirPath + 'layout.xml';

  TabsOpened := false;
  if ParamCount > 0 then
  begin
    for i := 1 to ParamCount do
    begin
      ws := ws + ParamStr(i);
      if i < ParamCount then
        ws := ws + ',';
    end;
    TabsOpened := HandleCmdParams(ws);
  end;

  if not TabsOpened then
  begin
    case Settings.StartupType of
      stNormal:
        CEActions.act_tabs_addtab.Execute;
      stSession:
        GlobalSessions.LoadAutoSession;
      stLastSession:
        GlobalSessions.LoadLatestHistorySession;
    end;
  end;

  // Atleast one tab has to be open
  if TabSet.TabCount = 0 then
  begin
    CEActions.act_tabs_addtab.Execute;
  end;

  if Application.MainForm.AlphaBlendValue < 255 then
    Application.MainForm.AlphaBlend := true;

  fIsReady := true;

  // Start Update timer
  CEActions.UpdateTimer.Enabled := true;



  // Testing stuff!!!
  if DebugHook <> 0 then
  begin

  end;
end;

{*------------------------------------------------------------------------------
  A delayed Startup code.
-------------------------------------------------------------------------------}
procedure TMainForm.StartUpTimerTimer(Sender: TObject);
begin
  StartUpTimer.Enabled := false;
  EndUIUpdate;

  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
    TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.SetFocus;

  if TrayIcon.Active and Settings.StartInTray then
    TrayIcon.HideApplication;

  // Load Startup Stack
  CEStackPanel.LoadStartupStack;

  if Settings.AutoCheckUpdates then
    AutoUpdateTimer.Enabled := true;

  // destroy curtain
  panel_curtain.Free;
  panel_curtain := nil;
end;

{-------------------------------------------------------------------------------
  On AutoUpdateTimer.Timer
-------------------------------------------------------------------------------}
procedure TMainForm.AutoUpdateTimerTimer(Sender: TObject);
begin
  AutoUpdateTimer.Enabled := false;
//  AutoUpdater.CheckForUpdates;
end;

{*------------------------------------------------------------------------------
  Get's called before shutdown
-------------------------------------------------------------------------------}
procedure TMainForm.Shutdown;
begin
  if not ReadOnlySettings and SaveSettingsOnClose then
  begin
    if GlobalFileViewSettings.PerFolderSettings then
      GlobalFileViewSettings.SaveStorage;

    GlobalSessions.SaveToFile(SettingsDirPath + 'sessions.xml');

    Settings.UpdatePositionInfo;
    GlobalAppSettings.SaveToFile(SettingsDirPath + 'settings.xml');

    Layouts.SaveSettingsForToolbars;
    Layouts.SaveToFile(SettingsDirPath + 'layout.xml');
  end;
end;


{*------------------------------------------------------------------------------
  Get's called on MainForm Close.
-------------------------------------------------------------------------------}
procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Shutdown;
end;

{*------------------------------------------------------------------------------
  Get's called on MainForm CloseQuery.
-------------------------------------------------------------------------------}
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  h: HWND;
begin
  // Close options dialog
  if assigned(CEOptionsDialog) then
    CEOptionsDialog.Close;

  // Check if dialogs are present. If so, show a warning.
  h := FindDialogWindow;
  if h <> 0 then
  begin
    BringWindowToTop(h);
//    if WideMessageBox(h,
//     _('Dialog found!'),
//     _('File operation might be still running!') + #13#10 + _('Are you sure you want to quit?'),
//     MB_ICONQUESTION or MB_YESNO) <> idYes then
      if MessageBox(h, 'Dialog found!', 'File operation might be still running!' + #13#10 + 'Are you sure you want to quit?', MB_ICONQUESTION or MB_YESNO) <> idYes then
    begin
      CanClose := false;
      Exit;
    end;
  end;

  fCEIsClosing := true;
  CEActions.UpdateTimer.Enabled := false;

  if assigned(CEOptionsDialog) then
  begin
    CEOptionsDialog.Release;
    CEOptionsDialog := nil;
  end;

  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
    GlobalFileViewSettings.AssignColumnSettingsFrom(TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView);

  if assigned(GlobalSessions.ActiveSession) then
  begin
    if GlobalSessions.ActiveSession.AutoSave then
      GlobalSessions.SaveActiveSession;
  end;

  if GlobalSessions.AutoSaveHistory then
    GlobalSessions.AddHistorySession
  else if Settings.StartupType = stLastSession then
    GlobalSessions.AddHistorySession(true);

  if GlobalPathCtrl.ActivePage is TCECustomTabPage then
    Layouts.SaveCurrentLayout;

  CanClose := TabSet.CloseAllTabs;
  if not CanClose then
  begin
    CEActions.UpdateTimer.Enabled := true;
    fCEIsClosing := false;
  end;
end;

{*------------------------------------------------------------------------------
  On Transparency popup
-------------------------------------------------------------------------------}
procedure TMainForm.TransparencyPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  if not Self.AlphaBlend then
    Sender.Items[0].Checked := true
  else
  begin
    case Self.AlphaBlendValue of
      255:
        Sender.Items[0].Checked := true;
      229..254:
        Sender.Items[1].Checked := true;
      204..228:
        Sender.Items[2].Checked := true;
      178..203:
        Sender.Items[3].Checked := true;
      153..177:
        Sender.Items[4].Checked := true;
      127..152:
        Sender.Items[5].Checked := true;
      102..126:
        Sender.Items[6].Checked := true;
      76..101:
        Sender.Items[7].Checked := true;
      51..75:
        Sender.Items[8].Checked := true;
      0..50:
        Sender.Items[9].Checked := true;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Set transparency
-------------------------------------------------------------------------------}
procedure TMainForm.TransparencyClick(Sender: TObject);
var
  i: Integer;
begin
  i := TSpTBXItem(Sender).Tag;
  case i of
    0:
      begin
        Self.AlphaBlend := false;
        Self.AlphaBlendValue := 255;
      end;
    1..9:
      begin
        Self.AlphaBlendValue := 255 - Round(25.5 * i);
        Self.AlphaBlend := true;
      end;
  end;
end;

{*------------------------------------------------------------------------------
  Set Fullscreen
-------------------------------------------------------------------------------}
procedure TMainForm.SetFullscreen(const Value: Boolean);
begin
  fFullscreen := Value;
  if fFullscreen then
  begin
    Self.Hide;
    try
      fOldWindowState := Self.WindowState;
      Self.BorderStyle := bsNone;
      if Self.WindowState = wsMaximized then
        Self.WindowState := wsNormal;
      Self.WindowState := wsMaximized;
    finally
      Self.Show;
    end;
  end
  else
  begin
    Self.BorderStyle := bsSizeable;
    Self.WindowState := fOldWindowState;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on Form show
-------------------------------------------------------------------------------}
procedure TMainForm.FormShow(Sender: TObject);
begin
  if fUpdatingCount > 1 then
    Exit;

  DockHostForm.DockServer.LeftDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.TopDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.BottomDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.RightDockPanel.DockManager.ResetBounds(true);

  // TODO: SpTabSet is visible even when hidden bug. Make proper fix.
  if not TabSet.Visible then
  begin
    TabSet.Visible := true;
    TabSet.Visible := false;
  end;
end;

{*------------------------------------------------------------------------------
  Get Skins From folder
-------------------------------------------------------------------------------}
procedure TMainForm.GetSkinsFromFolder(AFolderPath: String);
var
  SearchRec: TSearchRec;//TCCSearchRec;
  list: TStrings;
  path: String;
  i: Integer;
begin
  AFolderPath := IncludeTrailingPathDelimiter(AFolderPath);
  if DirectoryExists(AFolderPath) then
  begin
    list := TStringList.Create;
    try
      path := AFolderPath + '*.skn';
      if FindFirst(path, faAnyFile, SearchRec) = 0 then
      begin
        try
          repeat
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
              list.Add(SearchRec.Name);
          until FindNext(SearchRec) <> 0;
        finally
          FindClose(SearchRec);
        end;
      end;
      for i := 0 to list.Count - 1 do
      begin
        path := AFolderPath + list.Strings[i];
        if FileExists(path) then
          SkinManager.SkinsList.AddSkinFromFile(path);
      end;
    finally
      list.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Convert custom toolbar actions
-------------------------------------------------------------------------------}
procedure TMainForm.ConvertCustomActions(Root: TTBCustomItem; Recursive: Boolean = true);

  procedure EnumItem(Item: TTBCustomItem);
  var
    chItem, newItem: TTBCustomItem;
    i, index: Integer;
  begin
    if assigned(Item) then
    begin
      for i := 0 to Item.Count - 1 do
      begin
        chItem := Item.Items[i];
        if chItem.Action is TCEToolbarAction then
        begin
          newItem := TCEToolbarAction(chItem.Action).ItemClass.Create(Item);
          newItem.Action := chItem.Action;
          index := Item.IndexOf(chItem);
          FreeAndNil(chItem);
          Item.Insert(index, newItem);
        end;
        if Recursive then
          EnumItem(chItem);
      end;
    end;
  end;

begin
  EnumItem(Root);
end;


{-------------------------------------------------------------------------------
  OpenSkin
-------------------------------------------------------------------------------}
procedure TMainForm.OpenSkin;
var
  open: TOpenDialog;
  i: Integer;
begin
  open := TOpenDialog.Create(nil);
  open.InitialDir := exePath + 'Skins\';
  open.Filter := 'Skin File|*.skn|All Files|*.*';
  try
    if open.Execute then
    begin
      i := SkinManager.SkinsList.AddSkinFromFile(open.FileName);
      if i > -1 then
      begin
        SkinManager.SetSkin(SkinManager.SkinsList[i]);
        SkinGroupItem.Recreate;
      end;
    end;
  finally
    open.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Handle suspend events
-------------------------------------------------------------------------------}
procedure TMainForm.WMPowerBroadcast(var Message: TMessage);
begin
  case Message.WParam of
    PBT_APMSUSPEND:
      begin
        Layouts.SaveSettingsForToolbars;
        Layouts.SaveCurrentLayout;
        Layouts.SaveToFile(SettingsDirPath + 'layout.xml');
      end;
  end;
  Message.Result := 1;
  inherited;
end;

{*------------------------------------------------------------------------------
  On Device Change (CD Insert/Eject)
-------------------------------------------------------------------------------}
procedure TMainForm.WMDeviceChange(var Message: TMessage);
begin
  inherited;

  if not fIsReady then
    Exit;

  if (Message.wParam = $8000) or (Message.wParam = $8004) then
  begin
    //CEActions.act_navi_refresh.Execute;
  end;
end;

{-------------------------------------------------------------------------------
  Make MainForm visible
-------------------------------------------------------------------------------}
procedure TMainForm.MakeVisible;
begin
  if TrayIcon.Active and not TrayIcon.ApplicationVisible then
    TrayIcon.ShowApplication;
  // restore if needed
  if IsIconic(Handle) then
    ShowWindow(Handle, SW_RESTORE);
  // set foreground
  SetForegroundWindow(Handle);
end;

{-------------------------------------------------------------------------------
  Set Path In Title
-------------------------------------------------------------------------------}
procedure TMainForm.SetPathInTitle(const Value: Boolean);
begin
  if fPathInTitle <> Value then
  begin
    fPathInTitle := Value;
    if fPathInTitle then
    begin
      Caption := GlobalPathCtrl.GlobalPathCaption;
      Application.Title := Caption;
    end
    else
    begin
      Caption := 'CubicExplorer';
      Application.Title := Caption;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set Single Instance
-------------------------------------------------------------------------------}
procedure TMainForm.SetSingleInstance(const Value: Boolean);
begin
  fSingleInstance := Value;
end;

{*------------------------------------------------------------------------------
  Handle WMShellNotify messages
-------------------------------------------------------------------------------}
procedure TMainForm.WMShellNotify(var Msg: TMessage);
var
  ShellEventList: TVirtualShellEventList;
  ShellEvent: TVirtualShellEvent;
  List: TList;
  i, Count, i2: Integer;
  item: TCESpTabItem;
begin
  ShellEventList := TVirtualShellEventList(Msg.wParam);
  List := ShellEventList.LockList;
  try
    Count := List.Count;
    for i := 0 to Count - 1 do
    begin
      ShellEvent := TVirtualShellEvent(List.Items[i]);
      case ShellEvent.ShellNotifyEvent of
        vsneAssoccChanged:
          begin
            ForceIconCachRebuild;
            Application.ProcessMessages;
          // Rebuild all fileview tabs
            for i2 := 0 to TabSet.Items.Count - 1 do
            begin
              if TabSet.Items.Items[i2] is TCESpTabItem then
              begin
                item := TCESpTabItem(MainForm.TabSet.Items.Items[i2]);
                if item.Page is TCEFileViewPage then
                begin
                  TCEFileViewPage(item.Page).FileView.Rebuild(true);
                end;
              end;
            end;
          // refresh foldertree
            CEFolderPanel.FolderTree.Refresh;
          end;

        vsneDriveRemoved, vsneMediaRemoved:
          begin
          // Clear all tabs using removed drive
            i2 := 0;
            while i2 < TabSet.Items.Count do
            begin
              if TabSet.Items.Items[i2] is TCESpTabItem then
              begin
                item := TCESpTabItem(MainForm.TabSet.Items.Items[i2]);
                if item.Page is TCEFileViewPage then
                begin
                  if TCEFileViewPage(item.Page).FileView.RootFolderNamespace.IsParentByPIDL(ShellEvent.PIDL1, false) then
                  begin
                    if item.CloseTab then
                    begin
                      i2 := i2 - 1;
                    end;
                  end;
                end;
              end;
              i2 := i2 + 1;
            end;
          end;

        vsneUpdateDir:
          begin
          // refresh recycle bin empty state
            CERecycleBinCtrl.QueIsEmptyCheck;
          end;
      end;
    end;
  finally
    ShellEventList.UnlockList;
    ShellEventList.Release;
  end;
end;

{-------------------------------------------------------------------------------
  Handle WM_SysCommand (fix Vista issues)
-------------------------------------------------------------------------------}
procedure TMainForm.WMSyscommand(var Message: TWmSysCommand);
begin
  case (Message.CmdType and $FFF0) of
    SC_MINIMIZE:
      begin
        ShowWindow(Handle, SW_MINIMIZE);
        if TrayIcon.Active and Settings.MinimizeToTray then
          TrayIcon.HideApplication;
        Message.Result := 0;
      end;
    SC_RESTORE:
      begin
        if TrayIcon.Active and not TrayIcon.ApplicationVisible then
          TrayIcon.ShowApplication;
        ShowWindow(Handle, SW_RESTORE);
        Message.Result := 0;
      end;
    SC_CLOSE:
      begin
        if TrayIcon.Active and Settings.CloseToTray then
          TrayIcon.HideApplication
        else
          Self.Close;
        Message.Result := 0;
      end
  else
    inherited;
  end;
end;

{-------------------------------------------------------------------------------
  On Shortcut
-------------------------------------------------------------------------------}
procedure TMainForm.FormShortCut(var Msg: TWMKey; var Handled: Boolean);

  function DoExecuteAction(AShortcut: TShortcut; AActionList: TActionList): Boolean;
  var
    i, i2: Integer;
    action: TAction;
  begin
    Result := false;
    if assigned(AActionList) then
    begin
      for i := 0 to AActionList.ActionCount - 1 do
      begin
        action := TAction(AActionList.Actions[i]);
        if action.ShortCut = AShortcut then
        begin
          if ExecuteShortcut(action) then
          begin
            Result := true;
            break;
          end;
        end
        else
        begin
          for i2 := 0 to action.SecondaryShortCuts.Count - 1 do
          begin
            if TShortCut(action.SecondaryShortCuts.Objects[i2]) = AShortcut then
            begin
              if ExecuteShortcut(action) then
              begin
                Result := true;
                break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

var
  AShortcut: TShortcut;
begin
  Handled := false;
  AShortcut := Shortcut(Msg.CharCode, KeyDataToShiftState(Msg.KeyData));

  if assigned(GlobalPathCtrl.ActivePage) then
    Handled := DoExecuteAction(AShortcut, TCECustomTabPage(GlobalPathCtrl.ActivePage).PageActionList);
  if not Handled then
    Handled := DoExecuteAction(AShortcut, CEActions.ActionList);
end;

{-------------------------------------------------------------------------------
  Handle TabSet.OleDrop
-------------------------------------------------------------------------------}
procedure TMainForm.HandleTabSetOleDrop(ASender: TCESpTabSet; ADropTab: TCESpTabItem; const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint; var hr: HResult);
var
  hd: TCommonShellIDList;
  i: Integer;
  ns: TNamespace;
begin
  // open new tab(s)
  if not assigned(ADropTab) and assigned(dataObj) then
  begin
    dwEffect := DROPEFFECT_LINK;
    hd := TCommonShellIDList.Create;
    try
      hd.LoadFromDataObject(dataObj);
      for i := 0 to hd.PIDLCount - 1 do
      begin
        OpenPIDLInTab(TabSet, hd.AbsolutePIDL(i), TabSet.Settings.OpenTabSelect);
      end;
    finally
      hd.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set Lock Toolbars
-------------------------------------------------------------------------------}
procedure TMainForm.SetLockToolbars(const Value: Boolean);
var
  i: Integer;
begin
  fLockToolbars := Value;
  for i := 0 to CEToolbarDocks.OuterDocks.Count - 1 do
  begin
    TTBDock(CEToolbarDocks.OuterDocks.Items[i]).AllowDrag := not fLockToolbars;
  end;
  for i := 0 to CEToolbarDocks.InnerDocks.Count - 1 do
  begin
    TTBDock(CEToolbarDocks.InnerDocks.Items[i]).AllowDrag := not fLockToolbars;
  end;
end;

{-------------------------------------------------------------------------------
  Toggle Visibility (Restore/Minimize)
-------------------------------------------------------------------------------}
procedure TMainForm.ToggleVisibility;
begin
  if IsIconic(Handle) then
  begin
    if TrayIcon.Active and not TrayIcon.ApplicationVisible then
      TrayIcon.ShowApplication;

    ShowWindow(Handle, SW_RESTORE);

    Application.BringToFront;
  end
  else
  begin
    if TrayIcon.Active and Settings.MinimizeToTray then
    begin
      if not TrayIcon.ApplicationVisible then
        TrayIcon.ShowApplication
      else
        TrayIcon.HideApplication;
    end
    else
      ShowWindow(Handle, SW_MINIMIZE);
  end;
end;

{-------------------------------------------------------------------------------
  On TrayIcon.Click
-------------------------------------------------------------------------------}
procedure TMainForm.TrayIconClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    ToggleVisibility;
end;

{-------------------------------------------------------------------------------
  On TrayIcon.MouseUp
-------------------------------------------------------------------------------}
procedure TMainForm.TrayIconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (MainForm.WindowState = wsNormal) then
    MakeVisible;
end;

{-------------------------------------------------------------------------------
  On WM_Hotkey Message
-------------------------------------------------------------------------------}
procedure TMainForm.WMHotkey(var Message: TMessage);
begin
  CEActions.GlobalHotkeys.ExecuteHotkey(Message.WParam);
  inherited;
end;

{-------------------------------------------------------------------------------
  On sub_closed_tab_list.Popup
-------------------------------------------------------------------------------}
procedure TMainForm.sub_closed_tab_listPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  TabSet.PopulateUndoList(Sender);
end;

{-------------------------------------------------------------------------------
  On TabPopupMenu.Popup
-------------------------------------------------------------------------------}
procedure TMainForm.TabPopupMenuPopup(Sender: TObject);
begin
  sub_closed_tab_list.Enabled := TabSet.CanUndoTabClose;
  CEActions.UpdateAll;
end;

{-------------------------------------------------------------------------------
  On TMainForm.Resize
-------------------------------------------------------------------------------}
procedure TMainForm.FormResize(Sender: TObject);
begin
  // resize curtain
  if assigned(panel_curtain) then
    panel_curtain.BoundsRect := ClientRect;
end;

{-------------------------------------------------------------------------------
  On TrayPopupMenu.Popup
-------------------------------------------------------------------------------}
procedure TMainForm.TrayPopupMenuPopup(Sender: TObject);
var
  item: TSpTBXItem;
begin
  PopulateBookmarkItem(TrayPopupMenu.Items, CEBookmarkPanel.BookmarkTree);
  // separator
  TrayPopupMenu.Items.Add(TSpTBXSeparatorItem.Create(TrayPopupMenu));
  // exit
  item := TSpTBXItem.Create(TrayPopupMenu);
  item.Action := CEActions.act_gen_exit;
  TrayPopupMenu.Items.Add(item);
end;

{-------------------------------------------------------------------------------
  On WM_APPCOMMAND message
-------------------------------------------------------------------------------}
procedure TMainForm.WMAppCommand(var Message: TWMAppCommand);
begin
  Message.Result := 1;
  case Message.Cmd of
    APPCOMMAND_BROWSER_BACKWARD:
      CEActions.act_navi_back.Execute;
    APPCOMMAND_BROWSER_FORWARD:
      CEActions.act_navi_forward.Execute;
    APPCOMMAND_BROWSER_SEARCH:
      CEActions.act_navi_filesearch.Execute;
  else
    begin
      Message.Result := 0;
      inherited;
    end;
  end;
end;



{$REGION 'MyRegion'}



procedure TMainForm.SpTBXItem101Click(Sender: TObject);
begin
  VisualThemesFrm.Visible := True;
end;

procedure TMainForm.SpTBXItem102Click(Sender: TObject);
// b4 shutdown .... CE_AppSettings
var
  FileStream: TStream;
begin
  if not ReadOnlySettings then
  begin
    // CE_FileView
    if GlobalFileViewSettings.PerFolderSettings then
       GlobalFileViewSettings.SaveStorage;

    // CE_Sessions
    GlobalSessions.SaveToFile(SettingsDirPath + 'sessions.xml');

{-------------------------------------------------------------------------------
  Save To File     fcl-xml XMLWrite  DOM
-------------------------------------------------------------------------------}
//procedure TCESessions.SaveToFile(AFilePath: String);
//var
//  FileStream: TStream;
//begin
//  try
////     FileStream:= TWideFileStream.Create(AFilePath, fmCreate);
//     FileStream:= TFileStream.Create(SettingsDirPath + 'sessions.xml', fmCreate); //faulty
//  except
//    Exit;
//  end;
//
//  try
//    if not assigned(XML.DocumentElement) then
//    XML.AppendChild(XML.CreateElement('CubicExplorer'));
//    WriteXML(XML.DocumentElement, FileStream);
//  finally
//    FileStream.Free;
//  end;
//end;
//------------------------------------------------------------------------------



    // internal
    Settings.UpdatePositionInfo;

    // CE_AppSettings   ReRoute
    GlobalAppSettings.SaveToFile(SettingsDirPath + 'settings.xml');  //..............zzzzzZZZZ

{-------------------------------------------------------------------------------
  Save To File
-------------------------------------------------------------------------------}
//procedure TCEAppSettings.SaveToFile(AFilePath: String);
//var
//  FileStream: TStream;
//begin
//  try
//    //FileStream:= TWideFileStream.Create(AFilePath, fmCreate);
//    FileStream:= TFileStream.Create(AFilePath, fmCreate);
//  except
//    Exit;
//  end;
//
//  try
//    SaveProperties;
//    WriteXML(XML.DocumentElement, FileStream);
//  finally
//    FileStream.Free;
//  end;
//end;
//------------------------------------------------------------------------------

    // CE_Layout
    Layouts.SaveSettingsForToolbars;
    Layouts.SaveToFile(SettingsDirPath + 'layout.xml');



{*------------------------------------------------------------------------------
  Save layout configuration to file
-------------------------------------------------------------------------------}
//procedure TCELayoutController.SaveToFile(AFilePath: String = '');
//var
//  s: TStream;
//begin
//  try
//    if AFilePath <> '' then
//    fFilePath:= AFilePath;
//    s:= TFileStream.Create(fFilePath, fmCreate);
//    try
//      s.Position:= 0;
//      AppStorage.Xml.SaveToStream(s);
//    finally
//      s.Free;
//    end;
//  except
//  end;
//end;
  end;
end;

//function LoadFromFile(s: String): Boolean;
//  var
//    s: string;
//
//begin
//  if FileExists(s) then
//  begin
//    fXML.Free;
//    Result:= true;
//    try
//      ReadXMLFile(fXML, s);
//    except on EXMLReadError do
//    begin
//      if not assigned(fXML) then
//      begin
//        fXML:= TXMLDocument.Create;
//        XML.AppendChild(XML.CreateElement('CubicExplorer'));
//      end
//      else if not assigned(XML.DocumentElement) then
//      begin
//        XML.AppendChild(XML.CreateElement('CubicExplorer'));
//      end
//      else
//      Result:= false;
//    end;
//  end;
//
//
//  if Result then
//  LoadProperties;
//  end;
//end

procedure TMainForm.SpTBXItem103Click(Sender: TObject);
//var
// sd: string = SettingsDirPath + 'settings.xml';
// se: string = exePath + 'settings.xml';
Begin
  // Load Sessions
  if FileExists(SettingsDirPath + 'sessions.xml') then
    GlobalSessions.LoadFromFile(SettingsDirPath + 'sessions.xml')
  else
    GlobalSessions.LoadFromFile(exePath + 'sessions.xml');
  SessionsToolbar.Recreate;
//
//   Set PerFolder Storage path
//  GlobalFileViewSettings.StorageFilePath := SettingsDirPath + 'perfolder.dat';

  // Load Settings
  GlobalFileViewSettings.BeginUpdate;
  try
    if FileExists(SettingsDirPath + 'settings.xml') then
      GlobalAppSettings.LoadFromFile(SettingsDirPath + 'settings.xml')
    else if FileExists(exePath + 'settings.xml') then
      GlobalAppSettings.LoadFromFile(exePath + 'settings.xml')
    else
      GlobalAppSettings.LoadFromResource(hInstance, 'DEFAULT_SETTINGS_DATA');
  finally
    GlobalFileViewSettings.EndUpdate(true);
  end;


  //Settings.ApplyPositionInfo(Settings.StartInTray);

  //  if not Settings.StartInTray then
  //    MainForm.Show;

  // Load Bookmarks
  CEBookmarkPanel.BookmarkMenuItems.Add(BookmarkToolbar.Items);
  CEBookmarkPanel.BookmarkMenuItems.Add(bookmarkMenuItem);
  if FileExists(SettingsDirPath + 'bookmarks.xml') then
    CEBookmarkPanel.BookmarksPath := SettingsDirPath + 'bookmarks.xml'
  else
    CEBookmarkPanel.BookmarksPath := exePath + 'bookmarks.xml';
  CEBookmarkPanel.LoadBookmarks;
  CEBookmarkPanel.BookmarksPath := SettingsDirPath + 'bookmarks.xml';

  // Load Layouts
  Layouts.AutoSave := false;
  if FileExists(SettingsDirPath + 'layout.xml') then
    Layouts.LoadFromFile(SettingsDirPath + 'layout.xml')
  else
    Layouts.LoadFromFile(exePath + 'layout.xml');
  Layouts.LoadSettingsForToolbars;
  Layouts.FilePath := SettingsDirPath + 'layout.xml';


end;


procedure TMainForm.SpTBXItem104Click(Sender: TObject);
begin
  //CETextEditorOptionsForm.Visible := True;
end;

{$ENDREGION}



{$REGION '========================================================================== > | PROPERTIES'}

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TMainFormSettings
-------------------------------------------------------------------------------}
constructor TMainFormSettings.Create;
begin
  inherited;
  fLeft := 50;
  fTop := 50;
  fHeight := 480;
  fWidth := 640;
  fShowCmd := 1;
  fStartupType := stNormal;
  fMinimizeToTray := false;
  fCloseToTray := false;
  fStartInTray := false;
  fExitOnLastTabClose := false;
  fAutoCheckUpdates := false;
  fWow64Enabled := false;
end;
{-------------------------------------------------------------------------------
  Get PositionInfo
-------------------------------------------------------------------------------}

procedure TMainFormSettings.UpdatePositionInfo;
var
  Placement: TWindowPlacement;
  r: TRect;
begin
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(MainForm.Handle, @Placement);
  r := Placement.rcNormalPosition;
  Left := r.Left;
  Top := r.Top;
  Width := Max(r.Right - r.Left, 50);
  Height := Max(r.Bottom - r.Top, 50);
  ShowCmd := Placement.showCmd;
end;

{-------------------------------------------------------------------------------
  Set PositionInfo
-------------------------------------------------------------------------------}
procedure TMainFormSettings.ApplyPositionInfo(AHideForm: Boolean = false);
var
  Placement: TWindowPlacement;
  r: TRect;
begin
  r.Left := Left;
  r.Top := Top;
  r.Right := Left + Max(Width, 50);
  r.Bottom := Top + Max(Height, 50);
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(MainForm.Handle, @Placement);
  Placement.rcNormalPosition := r;
  if AHideForm then
    Placement.showCmd := SW_HIDE
  else
    Placement.showCmd := ShowCmd;
  if Placement.showCmd = SW_SHOWMINIMIZED then
    Placement.showCmd := SW_SHOWNORMAL;
  SetWindowPlacement(MainForm.Handle, @Placement);
end;

{-------------------------------------------------------------------------------
  Get/Set AlphaBlend value
-------------------------------------------------------------------------------}
function TMainFormSettings.GetAlphaBlend: Integer;
begin
  Result := MainForm.AlphaBlendValue;
end;

procedure TMainFormSettings.SetAlphaBlend(const Value: Integer);
begin
  MainForm.AlphaBlendValue := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set Language
-------------------------------------------------------------------------------}
//function TMainFormSettings.GetLanguage: String;
//begin
//  Result := MainForm.ActiveLanguage;
//end;
//
//procedure TMainFormSettings.SetLanguage(const Value: String);
//begin
//  MainForm.ActiveLanguage := Value;
//end;

{-------------------------------------------------------------------------------
  Get/Set Skin
-------------------------------------------------------------------------------}
function TMainFormSettings.GetSkin: string;
begin
  Result := SkinManager.CurrentSkinName;
end;

procedure TMainFormSettings.SetSkin(const Value: string);
begin
  SkinManager.SetSkin(Value);
end;

{-------------------------------------------------------------------------------
  Get/Set ShowHint
-------------------------------------------------------------------------------}
function TMainFormSettings.GetShowHint: Boolean;
begin
  Result := MainForm.ShowHint;
end;

procedure TMainFormSettings.SetShowHint(const Value: Boolean);
begin
  MainForm.ShowHint := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set SingleInstance
-------------------------------------------------------------------------------}
function TMainFormSettings.GetSingleInstance: Boolean;
begin
  Result := MainForm.SingleInstance;
end;

procedure TMainFormSettings.SetSingleInstance(const Value: Boolean);
begin
  MainForm.SingleInstance := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set PathInTitle
-------------------------------------------------------------------------------}
function TMainFormSettings.GetPathInTitle: Boolean;
begin
  Result := MainForm.PathInTitle;
end;

procedure TMainFormSettings.SetPathInTitle(const Value: Boolean);
begin
  MainForm.PathInTitle := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set AlwaysOnTop
-------------------------------------------------------------------------------}
function TMainFormSettings.GetAlwaysOnTop: Boolean;
begin
  Result := MainForm.FormStyle = fsStayOnTop;
end;

procedure TMainFormSettings.SetAlwaysOnTop(const Value: Boolean);
begin
  if Value then
    MainForm.FormStyle := fsStayOnTop
  else
    MainForm.FormStyle := fsNormal;
end;

{-------------------------------------------------------------------------------
  Get/Set StartupSession
-------------------------------------------------------------------------------}
function TMainFormSettings.GetAutoLoadSession: String;
begin
  if assigned(GlobalSessions.AutoLoadSession) then
    Result := GlobalSessions.AutoLoadSession.Name
  else
    Result := '';
end;

procedure TMainFormSettings.SetAutoLoadSession(const Value: String);
begin
  GlobalSessions.AutoLoadSession := GlobalSessions.Sessions.FindSession(Value);
end;

{-------------------------------------------------------------------------------
  Get/Set ShowTray
-------------------------------------------------------------------------------}
function TMainFormSettings.GetShowTray: Boolean;
begin
  Result := MainForm.TrayIcon.Active;
end;

procedure TMainFormSettings.SetShowTray(const Value: Boolean);
begin
  if not Value and not MainForm.TrayIcon.ApplicationVisible then
    MainForm.TrayIcon.ShowApplication;

  MainForm.TrayIcon.Active := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set LastUpdateCheck
-------------------------------------------------------------------------------}
function TMainFormSettings.GetLastUpdateCheck: TDateTime;
begin
//  Result := CELastVersionCheck;
end;

procedure TMainFormSettings.SetLastUpdateCheck(const Value: TDateTime);
begin
//  CELastVersionCheck := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set CheckForUpdateTypes
-------------------------------------------------------------------------------}
//function TMainFormSettings.GetCheckForUpdateTypes: TCEBuildTypes;
//begin
//  Result := MainForm.AutoUpdater.BuildTypes;
//end;
//
//procedure TMainFormSettings.SetCheckForUpdateTypes(const Value: TCEBuildTypes);
//begin
//  MainForm.AutoUpdater.BuildTypes := Value;
//end;

{-------------------------------------------------------------------------------
  Get/Set ProxyAddress
-------------------------------------------------------------------------------}
function TMainFormSettings.GetProxyAddress: String;
begin
  Result := CE_ProxyAddress;
end;

procedure TMainFormSettings.SetProxyAddress(const Value: String);
begin
  CE_ProxyAddress := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set UseProxy
-------------------------------------------------------------------------------}
function TMainFormSettings.GetUseProxy: Boolean;
begin
  Result := CE_UseProxy;
end;

procedure TMainFormSettings.SetUseProxy(const Value: Boolean);
begin
  CE_UseProxy := Value
end;

{-------------------------------------------------------------------------------
  Get/Set UseSystemProxy
-------------------------------------------------------------------------------}
function TMainFormSettings.GetUseSystemProxy: Boolean;
begin
  Result := CE_UseSystemProxy;
end;

procedure TMainFormSettings.SetUseSystemProxy(const Value: Boolean);
begin
  CE_UseSystemProxy := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set UpdateURL
-------------------------------------------------------------------------------}
function TMainFormSettings.GetUpdateURL: String;
begin
//  Result := UpdateConfURL;
end;

procedure TMainFormSettings.SetUpdateURL(const Value: String);
begin
  if Value <> '' then
  begin
//    UpdateConfURL := Value;
  end;
end;

{-------------------------------------------------------------------------------
  Get/Set ProxyUsername
-------------------------------------------------------------------------------}
function TMainFormSettings.GetProxyUsername: string;
begin
  Result := CE_ProxyUsername;
end;

procedure TMainFormSettings.SetProxyUsername(const Value: string);
begin
  CE_ProxyUsername := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set ProxyPassword
-------------------------------------------------------------------------------}
function TMainFormSettings.GetProxyPassword: string;
begin
  Result := CE_ProxyPassword;
end;

procedure TMainFormSettings.SetProxyPassword(const Value: string);
begin
  CE_ProxyPassword := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set LockToolbars
-------------------------------------------------------------------------------}
function TMainFormSettings.GetLockToolbars: Boolean;
begin
  Result := Form.LockToolbars;
end;

procedure TMainFormSettings.SetLockToolbars(const Value: Boolean);
begin
  Form.LockToolbars := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set ChangeCurrentDirVar
-------------------------------------------------------------------------------}
function TMainFormSettings.GetChangeCurrentDirVar: Boolean;
begin
  Result := GlobalPathCtrl.ChangeCurrentDirVar;
end;

procedure TMainFormSettings.SetChangeCurrentDirVar(const Value: Boolean);
begin
  GlobalPathCtrl.ChangeCurrentDirVar := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set Fullscreen
-------------------------------------------------------------------------------}
function TMainFormSettings.GetFullscreen: Boolean;
begin
  Result := Form.Fullscreen;
end;

procedure TMainFormSettings.SetFullscreen(const Value: Boolean);
begin
  Form.Fullscreen := Value;
end;

{-------------------------------------------------------------------------------
  Get/Set LockPanels
-------------------------------------------------------------------------------}
function TMainFormSettings.GetLockPanels: Boolean;
begin
  Result := CE_LockPanels;
end;

procedure TMainFormSettings.SetLockPanels(const Value: Boolean);
begin
  CE_LockPanels := Value;
end;

{-------------------------------------------------------------------------------
  Set Wow64Enabled
-------------------------------------------------------------------------------}
procedure TMainFormSettings.SetWow64Enabled(const Value: Boolean);
begin
  fWow64Enabled := Value;
  if IsWindows64 then
    MPCommonUtilities.Wow64Enabled := fWow64Enabled;
end;

{##############################################################################}

{$ENDREGION '========================================================================== > | PROPERTIES'}


{$IFDEF madExcept}
procedure LayoutExceptHandler(const exceptIntf: IMEException; var handled: boolean);
begin
  exceptIntf.BugReportHeader['user name'] := '';          // for privacy
  exceptIntf.BugReportHeader['registered owner'] := '';   // for privacy
  exceptIntf.BugReportHeader['computer name'] := '';      // for privacy
  exceptIntf.BugReportHeader['system up time'] := '';     // for privacy
  exceptIntf.BugReportHeader['free disk space'] := '';    // for privacy
  exceptIntf.BugReportHeader['compiled with'] := '';
end;

initialization
  RegisterExceptionHandler(LayoutExceptHandler, stDontSync);

{$ENDIF}



{*------------------------------------------------------------------------------
  Set Language
-------------------------------------------------------------------------------}
//procedure TMainForm.SetActiveLanguage(const Value: String);
//var
//  ws: String;
//begin
//  ws := fLanguageList.Values[Value];
//  if ws = '' then
//  begin
////    CEGlobalTranslator.ResetToOld(false);
//    fActiveLanguage := '';
//    DoCustomTranslate;
//  end
//  else
//  begin
////    CEGlobalTranslator.LoadPOFromFile(ws);
//    TranslateUI(self);
//    fActiveLanguage := Value;
//    DoCustomTranslate;
//  end;
//end;

{-------------------------------------------------------------------------------
  DoCustomTranslate
-------------------------------------------------------------------------------}
//procedure TMainForm.DoCustomTranslate;
//begin
//  // TabBar Left/Right arrow hints
//  TCESpTabToolbar(TabSet.Toolbar).LeftArrow.Hint := _('Show more tabs');
//  TCESpTabToolbar(TabSet.Toolbar).RightArrow.Hint := TCESpTabToolbar(TabSet.Toolbar).LeftArrow.Hint;
//
//  // VSTools strings
//  S_WARNING := _('Warning');
//  S_OPEN := _('Open');
//
//  STR_GROUPMODIFIEDHOUR := _('Last hour');
//  STR_GROUPMODIFIEDTODAY := _('Last twenty-four hours');
//  STR_GROUPMODIFIEDTHISWEEK := _('This week');
//  STR_GROUPMODIFIEDTWOWEEKS := _('Two weeks ago');
//  STR_GROUPMODIFIEDTHREEWEEKS := _('Three weeks ago');
//  STR_GROUPMODIFIEDMONTH := _('A month ago');
//  STR_GROUPMODIFIEDTWOMONTHS := _('Two months ago');
//  STR_GROUPMODIFIEDTHREEMONTHS := _('Three months ago');
//  STR_GROUPMODIFIEDFOURMONTHS := _('Four months ago');
//  STR_GROUPMODIFIEDFIVEMONTHS := _('Five months ago');
//  STR_GROUPMODIFIEDSIXMONTHS := _('Six months ago');
//  STR_GROUPMODIFIEDEARLIERTHISYEAR := _('Earlier this year');
//  STR_GROUPMODIFIEDLONGTIMEAGO := _('A long time ago');
//
//  STR_GROUPSIZEZERO := _('Zero');
//  STR_GROUPSIZETINY := _('Tiny');
//  STR_GROUPSIZESMALL := _('Small');
//  STR_GROUPSIZEMEDIUM := _('Medium');
//  STR_GROUPSIZELARGE := _('Large');
//  STR_GROUPSIZEGIGANTIC := _('Gigantic');
//  STR_GROUPSIZESYSFOLDER := _('System Folders');
//  STR_GROUPSIZEFOLDER := _('Folders');
//
//  STR_FILE_SIZE_IN_KB := _('KB');
//  STR_FILE_SIZE_IN_MB := _('MB');
//  STR_FILE_SIZE_IN_TB := _('TB');
//  STR_ZERO_KB := '0 ' + STR_FILE_SIZE_IN_KB;
//  STR_ONE_KB := '1 ' + STR_FILE_SIZE_IN_KB;
//
//  StatusBar.UpdateLabels(false);
//end;

{*------------------------------------------------------------------------------
  Initialize Language stuff
-------------------------------------------------------------------------------}
//procedure TMainForm.InitLanguage;
//begin
//  GetLanguageList;
//  // Add Ignore items
//  CEGlobalTranslator.RegisterIgnoredClass(TFont);
//
//  // Add Include items
//  CEGlobalTranslator.UseIncludeList := true;
//  CEGlobalTranslator.IncludeInheritedAlso := true;
//  CEGlobalTranslator.IncludeClasses.Add(TAction);
//  CEGlobalTranslator.IncludeClasses.Add(TSpTBXItem);
//  CEGlobalTranslator.IncludeClasses.Add(TMenuItem);
//  CEGlobalTranslator.IncludeClasses.Add(TSpTBXTabItem);
//  CEGlobalTranslator.IncludeClasses.Add(TLabel);
//  CEGlobalTranslator.IncludeClasses.Add(TButton);
//  CEGlobalTranslator.IncludeClasses.Add(TCheckBox);
//  CEGlobalTranslator.IncludeClasses.Add(TGroupBox);
//  CEGlobalTranslator.IncludeClasses.Add(TSpTBXButton);
//  CEGlobalTranslator.IncludeClasses.Add(TForm);
//  CEGlobalTranslator.IncludeClasses.Add(TTBCustomDockableWindow);
//  CEGlobalTranslator.IncludeClasses.Add(TRadioButton);
//  CEGlobalTranslator.IncludeClasses.Add(TSpTBXCheckBox);
//  CEGlobalTranslator.IncludeClasses.Add(TSpTBXGroupBox);
//  CEGlobalTranslator.IncludeClasses.Add(TSpTBXRadioGroup);
//  CEGlobalTranslator.IncludeClasses.Add(TSpTBXRadioButton);
//  CEGlobalTranslator.IncludeClasses.Add(TSpTBXLabel);
//  CEGlobalTranslator.IncludeClasses.Add(TSpTBXLabelItem);
//  CEGlobalTranslator.IncludeClasses.Add(TTabSheet);
//
//  //CEGlobalTranslator.IgnoredProperties.Add('Name'); <-- name is ignored always
//  //CEGlobalTranslator.IgnoredProperties.Add('Category'); <-- hotkeys list shows the category for actions so keep it translated.
//  CEGlobalTranslator.IgnoredProperties.Add('HelpKeyword');
//  CEGlobalTranslator.IgnoredProperties.Add('ImeName');
//  CEGlobalTranslator.RegisterIgnoredClassHandler(TSpTBXItem, MenuItemTranslateHandler);
//end;

{*------------------------------------------------------------------------------
  Get's called when language menu is opened
-------------------------------------------------------------------------------}
//procedure TMainForm.LanguageMenuItemPopup(Sender: TTBCustomItem; FromLink: Boolean);
//var
//  i: Integer;
//  id: Integer;
//begin
//  GEtLanguageList;
//  id := fLanguageList.IndexOfName(ActiveLanguage);
//  for i := 0 to LanguageMenuItem.Count - 1 do
//  begin
//    if id = LanguageMenuItem.Items[i].Tag then
//      LanguageMenuItem.Items[i].Checked := true
//    else
//      LanguageMenuItem.Items[i].Checked := false;
//  end;
//end;

{*------------------------------------------------------------------------------
  Get's called when language menu item is clicked
-------------------------------------------------------------------------------}
//procedure TMainForm.LanguageItemClick(Sender: TObject);
//var
//  i: Integer;
//begin
//  i := TTBItem(Sender).Tag;
//  if (i = -1) or (i >= fLanguageList.Count) then
//  begin
//    ActiveLanguage := '';
//  end
//  else
//  begin
//    ActiveLanguage := fLanguageList.Names[i];
//  end;
//end;

{*------------------------------------------------------------------------------
  MenuItem Translate Handler
-------------------------------------------------------------------------------}
//procedure TMainForm.MenuItemTranslateHandler(Obj: TObject; var IsIgnored: Boolean);
//begin
//  IsIgnored := TSpTBXItem(Obj).Action <> nil;
//end;

{-------------------------------------------------------------------------------
  Translate UI
--------------------------------------------------------------------------------}
//procedure TMainForm.TranslateUI(Sender: TObject);
//begin
//  BeginUIUpdate;
//  CEGlobalTranslator.TranslatedCount := 0;
//
//  if Sender is TCEPoEditor then
//  begin
//    if fActiveLanguage <> '' then
//      CEGlobalTranslator.LoadPOFromFile(fLanguageList.Values[fActiveLanguage]);
//  end;
//
//  if CEGlobalTranslator.IsTranslated then
//    CEGlobalTranslator.ReTranslateAll
//  else
//  begin
//    CEGlobalTranslator.TranslateComponent(Self);
//  end;
//  CEGlobalTranslator.TranslateComponent(CEQuickViewPanel.QuickView);
//  EndUIUpdate;
//end;


{-------------------------------------------------------------------------------
  Handle UpdateFound
-------------------------------------------------------------------------------}
//procedure TMainForm.HandleUpdateFound(Sender: TObject; BuildType: TCEBuildType; Version: TCEVersionNumber; Notes: String; var DoUpdate: Boolean);
//var
//  ws: String;
//  list: TStrings;
//begin
//  list := TStringList.Create;
//  list.Text := Notes;
//  if list.Count > 20 then
//  begin
//    while list.Count > 20 do
//      list.Delete(list.Count - 1);
//    list.Add('...');
//  end;
//
//  try
//    try
//      ws := Format(_('New version (%s) is available. Do you want to update now?'), [VersionNumberToStr(Version)]);
//    except
//      ws := Format('New version (%s) is available. Do you want to update now?', [VersionNumberToStr(Version)]);
//    end;
//
//    ws := ws + #13#10 + '____________________' + #13#10;
//    ws := ws + _('Build:') + ' ' + VersionNumberToStr(Version) + #13#10;
//    ws := ws + _('Update type:') + ' ' + GetBuildTypeDescription(BuildType) + #13#10#13#10;
//    ws := ws + list.Text;
//    if TaskDialog(Self.Handle, _('New Version Available'), _('Do you want to update?'), ws, TD_ICON_QUESTION, TD_BUTTON_YES + TD_BUTTON_NO) = mrYes then
//    begin
//      DoUpdate := true;
//    end;
//  finally
//
//  end;
//end;


{*------------------------------------------------------------------------------
  Get Language List
-------------------------------------------------------------------------------}
//procedure TMainForm.GetLanguageList;
//var
//  i: Integer;
//  item: TSpTBXItem;
//  sep: TSpTBXSeparatorItem;
//begin
//  fLanguageList.Clear;
//  LanguageMenuItem.Clear;
//  // Get Language List
//  GetPOLanguageList(exePath + 'Locale\', 'default', fLanguageList);
//
//  // Disable Language menu if there is no translations
//  if fLanguageList.Count = 0 then
//  begin
//    LanguageMenuItem.Enabled := false;
//  end;
//  // Add default menu items
//  item := TSpTBXItem.Create(self);
//  item.Caption := _('English (default)');
//  item.OnClick := LanguageItemClick;
//  item.Tag := -1;
//  LanguageMenuItem.Add(item);
//  sep := TSpTBXSeparatorItem.Create(self);
//  LanguageMenuItem.Add(sep);
//  // Loop through language list
//  for i := 0 to fLanguageList.Count - 1 do
//  begin
//    // Add language menu items
//    item := TSpTBXItem.Create(self);
//    item.Caption := fLanguageList.Names[i];
//    item.OnClick := LanguageItemClick;
//    item.Tag := i;
//    LanguageMenuItem.Add(item);
//  end;
//end;

end.

