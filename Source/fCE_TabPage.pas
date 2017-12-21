//******************************************************************************
//  CubicExplorer
//  Version: 0.90
//
//  The Original Code is fCE_TabPage.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************

unit fCE_TabPage;

interface

uses
  // CE Units
  CE_GlobalCtrl,
  //CE_VistaFuncs,
  // SpTBX
  SpTBXTabs,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, Contnrs, ActnList, ActiveX;

type
  TCECustomTabPageSettings = class(TPersistent)
  private
    fRememberPanelLayout: Boolean;
    fRememberInnerToolbarLayout: Boolean;
    fRememberOuterToolbarLayout: Boolean;
  protected
    function GetRememberPanelLayout: Boolean; virtual;
    function GetRememberInnerToolbarLayout: Boolean; virtual;
    function GetRememberOuterToolbarLayout: Boolean; virtual;
  public
    property RememberPanelLayout: Boolean read GetRememberPanelLayout write fRememberPanelLayout;
    property RememberInnerToolbarLayout: Boolean read GetRememberInnerToolbarLayout write fRememberInnerToolbarLayout;
    property RememberOuterToolbarLayout: Boolean read GetRememberOuterToolbarLayout write fRememberOuterToolbarLayout;
  published
  end;

  TCECustomTabPageSettingsClass = class of TCECustomTabPageSettings;

  TCECustomTabPageClass = class of TCECustomTabPage;

  TCECustomTabPage = class(TFrame, ICEPathChangeHandler, IDropTarget)
  private
    fActive: Boolean;
    fImageIndex: Integer;
    fImages: TImageList;
    fLayout: string;
    fPaneNumber: Integer;
    fSettings: TCECustomTabPageSettings;
    fTabCaption: string;
    procedure SetTabCaption(const Value: string);
  protected
    fTabItem: TSpTBXTabItem;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; virtual; stdcall;
    function DragLeave: HResult; virtual; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; reintroduce; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; virtual; stdcall;
    function GetPageActionList: TActionList; virtual;
    function GetSettingsClass: TCECustomTabPageSettingsClass; virtual;
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); virtual; stdcall;
    procedure GlobalContentChange(Sender: TObject); virtual; stdcall;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: string); virtual; stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: string); virtual; stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); virtual; stdcall;
    procedure SetActive(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HidePage; virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    procedure SelectPage; virtual;
    function TabClosing: Boolean; virtual;
    procedure UpdateCaption; virtual;
    property Active: Boolean read fActive write SetActive;
    property ImageIndex: Integer read fImageIndex write fImageIndex;
    property Images: TImageList read fImages write fImages;
    property Layout: string read fLayout write fLayout;
    property PageActionList: TActionList read GetPageActionList;
    property PaneNumber: Integer read fPaneNumber write fPaneNumber;
    property TabCaption: string read fTabCaption write SetTabCaption;
    property TabItem: TSpTBXTabItem read fTabItem;
  published
    property Settings: TCECustomTabPageSettings read fSettings write fSettings;
  end;

  TCETabClassList = class(TObject)
  private
  protected
    ClassList: TClassList;
    NameList: TStrings;
    SettingsList: TClassList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetClass(AName: string): TCECustomTabPageClass;
    function GetName(AClass: TCECustomTabPageClass): string;
    function GetSettings(AName: string): TCECustomTabPageSettingsClass;
    procedure RegisterClass(AName: string; AClass: TCECustomTabPageClass; ASettings: TCECustomTabPageSettingsClass);
    procedure UnRegisterClass(AClass: TCECustomTabPageClass);
  end;

function TabPageClassList: TCETabClassList;

var
  fTabPageClassList: TCETabClassList;

implementation

uses
  dCE_Actions;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Get TabPageClassList
-------------------------------------------------------------------------------}
function TabPageClassList: TCETabClassList;
begin
  if fTabPageClassList = nil then
    fTabPageClassList := TCETabClassList.Create;
  Result := fTabPageClassList;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Get's called when TCECustomTabPage is created.
-------------------------------------------------------------------------------}
constructor TCECustomTabPage.Create(AOwner: TComponent);
begin
  inherited;
//  SetVistaFont(Font);
  fSettings := GetSettingsClass.Create;
  fImageIndex := -1;
  fActive := false;
  Layout := 'CustomPage';

  Self.OnContextPopup := CEActions.HandleGlobalContextPopup;
end;

{*------------------------------------------------------------------------------
  Get's called when TCECustomTabPage is destoyed.
-------------------------------------------------------------------------------}
destructor TCECustomTabPage.Destroy;
begin
  if GlobalPathCtrl.ActivePage = Self then
    GlobalPathCtrl.ActivePage := nil;
  fSettings.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  DragEnter
-------------------------------------------------------------------------------}
function TCECustomTabPage.DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  Result := S_OK;
end;

{-------------------------------------------------------------------------------
  DragLeave
-------------------------------------------------------------------------------}
function TCECustomTabPage.DragLeave: HResult;
begin
  Result := S_OK;
end;

{-------------------------------------------------------------------------------
  DragOver
-------------------------------------------------------------------------------}
function TCECustomTabPage.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  Result := S_OK;
end;

{-------------------------------------------------------------------------------
  Drop
-------------------------------------------------------------------------------}
function TCECustomTabPage.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  Result := S_OK;
end;

{-------------------------------------------------------------------------------
  Get Page Action List
-------------------------------------------------------------------------------}
function TCECustomTabPage.GetPageActionList: TActionList;
begin
  Result := nil;
  // Override from descendant
end;

{-------------------------------------------------------------------------------
  Get Settings Class
-------------------------------------------------------------------------------}
function TCECustomTabPage.GetSettingsClass: TCECustomTabPageSettingsClass;
begin
  Result := TCECustomTabPageSettings;
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when Active page has changed
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.GlobalActivePageChange(OldPage, NewPage: TComponent);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when global content has changed
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.GlobalContentChange(Sender: TObject);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when Global focus has changed
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.GlobalFocusChanged(Sender: TObject; NewPath: string);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.GlobalPathChanged(Sender: TObject; NewPath: string);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Hide page
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.HidePage;
begin
  if Visible then
    Visible := false;
end;

{-------------------------------------------------------------------------------
  Load from stream
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.LoadFromStream(AStream: TStream);
begin
  // Override from descendant.
end;

{-------------------------------------------------------------------------------
  Save to stream
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.SaveToStream(AStream: TStream);
begin
  // Override from descendant.
end;

{*------------------------------------------------------------------------------
  Select page
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.SelectPage;
begin
  GlobalPathCtrl.ActivePage := Self;
end;

{*------------------------------------------------------------------------------
  Set Active Value
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.SetActive(const Value: Boolean);
begin
  fActive := Value;
end;

{*------------------------------------------------------------------------------
  Get's called when tab is about to be closed.
    -If returns true the tab is closed.
-------------------------------------------------------------------------------}
function TCECustomTabPage.TabClosing: Boolean;
begin
  Result := true;
end;

{*------------------------------------------------------------------------------
  Update tab caption
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.UpdateCaption;
begin
  TabCaption := 'CustomPage';
end;

{*------------------------------------------------------------------------------
  Set Tab Caption
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.SetTabCaption(const Value: string);
begin
  fTabCaption := Value;
  TabItem.Caption := Value;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCETabClassList
-------------------------------------------------------------------------------}
constructor TCETabClassList.Create;
begin
  inherited;
  ClassList := TClassList.Create;
  SettingsList := TClassList.Create;
  NameList := TStringList.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCETabClassList
-------------------------------------------------------------------------------}
destructor TCETabClassList.Destroy;
begin
  ClassList.Free;
  SettingsList.Free;
  NameList.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCETabClassList.Clear;
begin
  ClassList.Clear;
  SettingsList.Clear;
  NameList.Clear;
end;

{-------------------------------------------------------------------------------
  Get Class
-------------------------------------------------------------------------------}
function TCETabClassList.GetClass(AName: string): TCECustomTabPageClass;
var
  i: Integer;
begin
  i := NameList.IndexOf(AName);
  if i > -1 then
    Result := TCECustomTabPageClass(ClassList.Items[i])
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------
  Get Name
-------------------------------------------------------------------------------}
function TCETabClassList.GetName(AClass: TCECustomTabPageClass): string;
var
  i: Integer;
begin
  i := ClassList.IndexOf(AClass);
  if i > -1 then
    Result := NameList.Strings[i]
  else
    Result := '';
end;

{-------------------------------------------------------------------------------
  Get Settings
-------------------------------------------------------------------------------}
function TCETabClassList.GetSettings(AName: string): TCECustomTabPageSettingsClass;
var
  i: Integer;
begin
  i := NameList.IndexOf(AName);
  if i > -1 then
    Result := TCECustomTabPageSettingsClass(SettingsList.Items[i])
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------
  RegisterClass
-------------------------------------------------------------------------------}
procedure TCETabClassList.RegisterClass(AName: string; AClass: TCECustomTabPageClass; ASettings: TCECustomTabPageSettingsClass);
begin
  NameList.Add(AName);
  ClassList.Add(AClass);
  SettingsList.Add(ASettings);
end;

{-------------------------------------------------------------------------------
  RegisterClass
-------------------------------------------------------------------------------}
procedure TCETabClassList.UnRegisterClass(AClass: TCECustomTabPageClass);
var
  i: Integer;
begin
  i := ClassList.IndexOf(AClass);
  if i > -1 then
  begin
    ClassList.Delete(i);
    SettingsList.Delete(i);
    NameList.Delete(i);
  end;
end;

{-------------------------------------------------------------------------------
  Get RememberPanelLayout
-------------------------------------------------------------------------------}
function TCECustomTabPageSettings.GetRememberPanelLayout: Boolean;
begin
  Result := fRememberPanelLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberInnerToolbarLayout
-------------------------------------------------------------------------------}
function TCECustomTabPageSettings.GetRememberInnerToolbarLayout: Boolean;
begin
  Result := fRememberInnerToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberOuterToolbarLayout
-------------------------------------------------------------------------------}
function TCECustomTabPageSettings.GetRememberOuterToolbarLayout: Boolean;
begin
  Result := fRememberOuterToolbarLayout;
end;

{##############################################################################}

initialization

finalization
  if assigned(fTabPageClassList) then
    FreeAndNil(fTabPageClassList);

end.

