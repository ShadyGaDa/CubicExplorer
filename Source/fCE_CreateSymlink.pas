//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90
//                                                                                            
//  The Original Code is fCE_CreateSymlink.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_CreateSymlink;

interface

uses
//  TntSysUtils,
  // CE Units
  CE_ElevatedActions, CE_FileUtils,
  //
//  StdCtrls, SysUtils, Forms,
  // SpTBX
  SpTBXControls, SpTBXEditors, SpTBXTabs,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileCtrl, TB2Item, SpTBXItem;

type
  TCreateSymlinkDlg = class(TForm)
    but_cancel: TSpTBXButton;
    but_create: TSpTBXButton;
    edit_linkname: TSpTBXEdit;
    edit_targetpath: TSpTBXButtonEdit;
    SpTBXTabControl1: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    SpTBXPanel1: TSpTBXPanel;
    SpTBXLabel1: TSpTBXLabel;
    SpTBXLabel2: TSpTBXLabel;
    procedure FormCreate(Sender: TObject);
    procedure but_createClick(Sender: TObject);
    procedure edit_targetpathSubEditButton0Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    ParentLinkFolderPath: String;
    { Public declarations }
  end;

procedure ShowCreateSymlinkDialog(AParentFolderPath: string; ALinkName: string; ATargetPath: string = '');

implementation

uses
  MPCommonUtilities,
  CE_Engine;
//  CE_VistaFuncs


{$R *.dfm}

{-------------------------------------------------------------------------------
  Show Create Symbolic link Dialog
-------------------------------------------------------------------------------}
procedure ShowCreateSymlinkDialog(AParentFolderPath: String; ALinkName:
    String; ATargetPath: String = '');
var
  dlg: TCreateSymlinkDlg;
begin
  dlg:= TCreateSymlinkDlg.Create(nil);
  try
    dlg.ParentLinkFolderPath:= AParentFolderPath;
    dlg.edit_linkname.Text:= ALinkName;
    dlg.edit_targetpath.Text:= ATargetPath;
    dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  On TCreateSymlinkDlg Create
-------------------------------------------------------------------------------}
procedure TCreateSymlinkDlg.FormCreate(Sender: TObject);
begin
//  SetVistaFont(Font);
//  CEGlobalTranslator.TranslateComponent(Self);
end;

{-------------------------------------------------------------------------------
  On Create button click
-------------------------------------------------------------------------------}
procedure TCreateSymlinkDlg.but_createClick(Sender: TObject);
var
  ws: String;
begin
//  if FileOrFolderExists(WideIncludeTrailingPathDelimiter(ParentLinkFolderPath) + edit_linkname.Text) then
  if FileOrFolderExists(IncludeTrailingPathDelimiter(ParentLinkFolderPath) + edit_linkname.Text) then
  begin
    ws:= '"' + edit_linkname.Text + '" ' + 'already exists. Please choose another name.';
    MessageBox(0, PWideChar('Duplicate name'), PWideChar(ws), MB_ICONINFORMATION or MB_OK);
    Exit;
  end;
  
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    // Vista or Win7
    if Win32MajorVersion >= 6 then
    begin
      if Elevated_CreateJunction(IncludeTrailingPathDelimiter(ParentLinkFolderPath) + edit_linkname.Text,
                                 edit_targetpath.Text,
                                 Self.Handle) then
      begin
        Self.ModalResult:= mrOK;
        Self.CloseModal;
      end;
    end
    else if Win32MajorVersion > 4 then // 2000 and XP
    begin
      if CreateJunction(IncludeTrailingPathDelimiter(ParentLinkFolderPath) + edit_linkname.Text,
                     edit_targetpath.Text) then
      begin
        Self.ModalResult:= mrOK;
        Self.CloseModal;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  CreateParams
-------------------------------------------------------------------------------}
procedure TCreateSymlinkDlg.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent:= Application.MainFormHandle;
end;

{-------------------------------------------------------------------------------
  On target path edit button click
-------------------------------------------------------------------------------}
procedure TCreateSymlinkDlg.edit_targetpathSubEditButton0Click(Sender: TObject);
var
  ws: String;
begin
  if edit_targetpath.Text <> '' then
  ws:= edit_targetpath.Text
  else
  ws:= ParentLinkFolderPath;
  
  if SelectDirectory('Select target folder', '', ws) then
  begin
    edit_targetpath.Text:= ws;
  end;
end;

{-------------------------------------------------------------------------------
  On Form KeyPress
-------------------------------------------------------------------------------}
procedure TCreateSymlinkDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  but_cancel.Click;
end;

end.
