//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90                                                                             
//                                                                                            
//  The Original Code is fCE_ItemSelectSaveDlg.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_ItemSelectSaveDlg;

interface

uses
  //
  StdCtrls, Dialogs, Forms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  SpTBXControls, SpTBXEditors, SpTBXItem;


type
  TCEItemSelectSaveDlg = class(TForm)
    panel_background: TSpTBXPanel;
    label_combotitle: TSpTBXLabel;
    combo: TSpTBXComboBox;
    but_ok: TSpTBXButton;
    but_cancel: TSpTBXButton;
    procedure but_okClick(Sender: TObject);
    procedure comboChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    fAllowEmptyText: Boolean;
    fExistsWarningContent: String;
    fExistsWarningDescription: String;
    fExistsWarningTitle: String;
    fShowExistsWarning: Boolean;
    procedure SetAllowEmptyText(const Value: Boolean);
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  published
    property AllowEmptyText: Boolean read fAllowEmptyText write SetAllowEmptyText;
    property ExistsWarningContent: string read fExistsWarningContent write fExistsWarningContent;
    property ExistsWarningDescription: string read fExistsWarningDescription write fExistsWarningDescription;
    property ExistsWarningTitle: string read fExistsWarningTitle write fExistsWarningTitle;
    property ShowExistsWarning: Boolean read fShowExistsWarning write fShowExistsWarning;
  end;

implementation

uses
  //CE_Engine,
  CE_Utils;// CE_VistaFuncs

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEItemSelectSaveDlg
-------------------------------------------------------------------------------}
constructor TCEItemSelectSaveDlg.Create(AOwner: TComponent);
begin
  inherited;
//  SetVistaFont(Font);
//  CEGlobalTranslator.TranslateComponent(Self);
  AllowEmptyText:= false;
  PopupParent:= Application.MainForm;
end;

{-------------------------------------------------------------------------------
  On but_ok Click
-------------------------------------------------------------------------------}
procedure TCEItemSelectSaveDlg.but_okClick(Sender: TObject);
begin
  if ShowExistsWarning then
  begin
    if combo.Items.IndexOf(combo.Text) > -1 then
    begin
      if (TaskDialog(Self.Handle,
                     ExistsWarningTitle,
                     ExistsWarningDescription,
                     ExistsWarningContent,
                     TD_ICON_QUESTION,
                     TD_BUTTON_YES + TD_BUTTON_NO) = TD_RESULT_YES) then
      ModalResult:= mrOK
      else
      combo.SetFocus;
    end
    else
    ModalResult:= mrOK;
  end
  else
  ModalResult:= mrOK;
end;

{-------------------------------------------------------------------------------
  On combo.Change
-------------------------------------------------------------------------------}
procedure TCEItemSelectSaveDlg.comboChange(Sender: TObject);
begin
  if not fAllowEmptyText then
  but_ok.Enabled:= combo.Text <> '';
end;

{-------------------------------------------------------------------------------
  CreateParams
-------------------------------------------------------------------------------}
procedure TCEItemSelectSaveDlg.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent:= Application.MainFormHandle;
end;

{-------------------------------------------------------------------------------
  On Form KeyPress
-------------------------------------------------------------------------------}
procedure TCEItemSelectSaveDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  but_cancel.Click;
end;

{-------------------------------------------------------------------------------
  Set Allow Empty Text
-------------------------------------------------------------------------------}
procedure TCEItemSelectSaveDlg.SetAllowEmptyText(const Value: Boolean);
begin
  fAllowEmptyText:= Value;
  if fAllowEmptyText then
  but_ok.Enabled:= true
  else
  but_ok.Enabled:= combo.Text <> '';
end;

end.
