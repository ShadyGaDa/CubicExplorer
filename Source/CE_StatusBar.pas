//******************************************************************************
//  CubicExplorer
//  Version: 0.90
//
//  The Original Code is CE_StatusBar.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************

unit CE_StatusBar;

interface

uses
  // CE Unit
  CE_GlobalCtrl, CE_FileView, fCE_FileView,
//  CE_LanguageEngine,
  //CE_Engine,
  // TB2k, TBX, SpTBX
  SpTBXItem,
  // VSTools
  EasyListView, VirtualExplorerEasyListView, MPShellUtilities,
  // System Units
  Classes, Windows, SysUtils, ShlObj;

type

  TCEStatusBar = class(TSpTBXStatusBar, ICEPathChangeHandler)
  private
    SelectionLabel: TSpTBXLabelItem;
    SelectionSizeLabel: TSpTBXLabelItem;
    CurrentItemLabel: TSpTBXLabelItem;
  protected
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); virtual;
        stdcall;
    procedure GlobalContentChange(Sender: TObject); virtual; stdcall;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: String); stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: String); stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Initialize;
    procedure UpdateLabels(InvalidateParent: Boolean = false);
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEStatusBar
-------------------------------------------------------------------------------}
constructor TCEStatusBar.Create(AOwner: TComponent);
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Initialize items
-------------------------------------------------------------------------------}
procedure TCEStatusBar.Initialize;
begin
  SelectionLabel:= TSpTBXLabelItem.Create(self);
  SelectionLabel.Caption:= '0/0 ' + 'Selected';
  Self.Items.Add(SelectionLabel);

  Self.Items.Add(TSpTBXSeparatorItem.Create(Self));

  SelectionSizeLabel:= TSpTBXLabelItem.Create(self);
  SelectionSizeLabel.Caption:= '0/0 ' + 'KB';
  Self.Items.Add(SelectionSizeLabel);

  Self.Items.Add(TSpTBXSeparatorItem.Create(Self));
  Self.Items.Add(TSpTBXRightAlignSpacerItem.Create(Self));

  CurrentItemLabel:= TSpTBXLabelItem.Create(Self);
  CurrentItemLabel.Alignment:= taRightJustify;
  Self.Items.Add(CurrentItemLabel);
end;

{*------------------------------------------------------------------------------
  Get's called when Active page has changed
-------------------------------------------------------------------------------}
procedure TCEStatusBar.GlobalActivePageChange(OldPage, NewPage: TComponent);
begin
  UpdateLabels;
end;

{*------------------------------------------------------------------------------
  Get's called when global content has changed
-------------------------------------------------------------------------------}
procedure TCEStatusBar.GlobalContentChange(Sender: TObject);
begin
  UpdateLabels;
end;

{*------------------------------------------------------------------------------
 Get's called when global focus has changed
-------------------------------------------------------------------------------}
procedure TCEStatusBar.GlobalFocusChanged(Sender: TObject; NewPath: String);
begin
  UpdateLabels;
end;

{*------------------------------------------------------------------------------
  Get's called when global path has changed
-------------------------------------------------------------------------------}
procedure TCEStatusBar.GlobalPathChanged(Sender: TObject; NewPath: String);
begin
  // Do nothing?
end;

{*------------------------------------------------------------------------------
  Get's called when global PIDL has changed
-------------------------------------------------------------------------------}
procedure TCEStatusBar.GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList);
begin
  // Do nothing?
end;

{*------------------------------------------------------------------------------
  Update Labels
-------------------------------------------------------------------------------}
procedure TCEStatusBar.UpdateLabels(InvalidateParent: Boolean = false);
var
  i,it: Int64;
  Item: TEasyItem;
  fileView: TCEFileView;
  si, st: String;
  ws: String;
  NS: TNamespace;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;
    ws:= IntToStr(fileView.Selection.Count) + '/' +
         IntToStr(fileView.ItemCount) + ' ' + 'Selected';
    SelectionLabel.Caption:= ws;

    i:= 0;
    Item := fileView.Selection.First;
    while Assigned(Item) do
    begin
      i:= i + TExplorerItem(Item).Namespace.SizeOfFileInt64;
      Item:= fileView.Selection.Next(Item);
    end;

    it:= fileView.RootFolderNamespace.FolderSize(InvalidateParent);

    if (i > 1024) and (i < 1048576) then
    si:= FloatToStrF(i / 1024, ffFixed, 4,0) + ' ' + 'KB'
    else if i >= 1048576 then
    si:= FloatToStrF(i / 1048576, ffFixed, 6,2) + ' ' + 'MB'
    else
    si:= IntToStr(i) + ' ' + 'Bytes';

    if (it > 1024) and (it < 1048576) then
    st:= FloatToStrF(it / 1024, ffFixed , 4,0) + ' ' + 'KB'
    else if it >= 1048576 then
    st:= FloatToStrF(it / 1048576, ffFixed , 6,2) + ' ' + 'MB'
    else
    st:= IntToStr(it) + ' ' + 'Bytes';

    SelectionSizeLabel.Caption:= si + ' / ' + st;

    if fileView.Selection.Count > 1 then
    begin
      ws:= IntToStr(fileView.Selection.Count) + ' ' + 'Selected' + ' | ' + 'Total Size' + ': ' + st;
    end
    else if assigned(fileView.Selection.FocusedItem) then
    begin
      NS:= TExplorerItem(fileView.Selection.FocusedItem).Namespace;
      ws:= NS.FileType + ' | ' + NS.LastWriteTime + ' | ' + si;
    end
    else
    begin
      ws:= '';
    end;

    CurrentItemLabel.Caption:= ws;
  end
  else
  begin
    SelectionLabel.Caption:= '0/0 ' + 'Selected';
    SelectionSizeLabel.Caption:= '0/0 ' + 'KB';
  end;
end;

end.
