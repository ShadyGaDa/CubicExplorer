unit VisualThemesGUI;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TVisualThemesFrm = class(TForm)
    SaveAndExitBtn: TButton;
    StylesCmb: TComboBox;
    Label1: TLabel;
    DefaultThemeBtn: TButton;
    ThemesOffBtn: TButton;
    procedure SaveAndExitBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DefaultThemeBtnClick(Sender: TObject);
    procedure StylesCmbChange(Sender: TObject);
    procedure ThemesOffBtnClick(Sender: TObject);
  private

  public

  end;

var
  VisualThemesFrm: TVisualThemesFrm;

implementation

{$R *.dfm}

uses
  Vcl.Themes;

procedure TVisualThemesFrm.SaveAndExitBtnClick(Sender: TObject);
begin
//  SaveIni(True);
//  LoadIni;
  Visible := False;
end;

procedure TVisualThemesFrm.StylesCmbChange(Sender: TObject);
begin
  try
    TStyleManager.TrySetStyle(StylesCmb.Items[StylesCmb.ItemIndex]);
  except
    on E: Exception do begin
      if Pos('Focus', E.Message) < 1 then
        raise;
    end;
  end;
end;

procedure TVisualThemesFrm.ThemesOffBtnClick(Sender: TObject);
begin
  StylesCmb.ItemIndex := StylesCmb.Items.IndexOf('Windows');
  StylesCmbChange(Sender);
end;

procedure TVisualThemesFrm.DefaultThemeBtnClick(Sender: TObject);
begin
  StylesCmb.ItemIndex := StylesCmb.Items.IndexOf('Windows');{Glossy}
  StylesCmbChange(Sender);
end;

procedure TVisualThemesFrm.FormShow(Sender: TObject);
var
  s: String;
begin
  StylesCmb.Items.BeginUpdate;
  try
    StylesCmb.Items.Clear;
    for s in TStyleManager.StyleNames do
       StylesCmb.Items.Add(s);
    StylesCmb.Sorted := True;
    StylesCmb.ItemIndex := StylesCmb.Items.IndexOf(TStyleManager.ActiveStyle.Name);
  finally
    StylesCmb.Items.EndUpdate;
  end;
end;

//
//procedure LoadIni;
//var i: Integer;
//    s, sa: String;
//    f: TextFile;
//    bFound, bCopyFile: LongBool;
//begin
//    SetLength(IniHigherVersion, 0);
//    for i := 0 to high(IniDirs) do IniDirs[i] := AppFolder;
//    IniDirs[1] := AppFolder + 'M3Parameter';
//    IniDirs[3] := AppFolder + 'M3Formulas';
//    IniDirs[8] := AppFolder + 'BigRenders';
//    IniDirs[9] := AppFolder + 'M3Maps';
//    //new: local ini in appdata folder:
//    s := AppFolder + 'Mandelbulb3D.ini';
//    bCopyFile := False;
//    HistoryFolder := AppFolder + 'History';
//    if not DirectoryExists(HistoryFolder) then CreateDir(HistoryFolder);
//    sa := AppDataDir + 'Mandelbulb3D.ini';
//    if (not FileExists(s)) and FileExists(sa) then
//    begin
//      if MessageDlg('The m3d Ini-file is now stored in the application folder.' + #13#10 +
//        'Do you want to copy the existing Ini-file from the appdata folder?' + #13#10 +
//        'Note: If you want a completly new installation and access the folders' + #13#10 +
//        'in the applications directory, select "No"', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
//      begin
//        s := sa; //AppDataDir + 'Mandelbulb3D.ini';
//        bCopyFile := True;
//      end;
//    end;
//    if FileExists(s) then
//    begin
//      AssignFile(f, s);
//      try
//        IniFileDate := GetFileModDate(s);
//        Reset(f);
//        for i := 0 to 5 do Readln(f, IniDirs[i]);
//        if not EOF(f) then Readln(f, IniDirs[6]);
//        while not EOF(f) do
//        begin
//          bFound := False;
//          Readln(f, s);
//          for i := 0 to IniMax do
//          if StrFirstWord(s) = IniItem[i] then
//          begin
//            IniVal[i] := StrLastWords(s);
//            bFound := True;
//            Break;
//          end;
//          if (not bFound) and (Length(Trim(s)) > 3) and (Length(IniHigherVersion) < 100) then
//          begin
//            SetLength(IniHigherVersion, Length(IniHigherVersion) + 1);
//            IniHigherVersion[Length(IniHigherVersion) - 1] := s;
//          end;
//        end;
//      finally
//        CloseFile(f);
//      end;
//      if bCopyFile then
//      begin
//        CopyFile(PChar(AppDataDir + 'Mandelbulb3D.ini') , PChar(AppFolder + 'Mandelbulb3D.ini'), True);
//        if FileExists(AppDataDir + IncludeTrailingPathDelimiter('Formulas') + 'FavouriteList.txt') then
//          CopyFile(PChar(AppDataDir + IncludeTrailingPathDelimiter('Formulas') + 'FavouriteList.txt') ,
//                   PChar(IncludeTrailingPathDelimiter(IniDirs[3]) + 'FavouriteList.txt'), True);
//      end;
//      if IniVal[12] <> '' then IniDirs[7] := IniVal[12];
//      if IniVal[17] <> '' then IniDirs[8] := IniVal[17];
//      if IniVal[18] <> '' then IniDirs[9] := IniVal[18];
//      if IniVal[22] <> '' then IniDirs[10] := IniVal[22];
//      if IniVal[32] <> '' then IniDirs[11] := IniVal[32];
//      if not CheckAuthorValid(IniVal[33]) then IniVal[33] := '';
//    end;
//end;
//
//procedure SaveIni(ForceSave: LongBool);
//var i: Integer;
//    f: TextFile;
//    s: String;
//begin
//    s := AppFolder + 'Mandelbulb3D.ini';
//    if (not ForceSave) and (IniFileDate < GetFileModDate(s)) then Exit;
//    AssignFile(f, s);
//    try
//      IniVal[12] := IniDirs[7]; //m3l paras folder
//      IniVal[17] := IniDirs[8]; //BigRenders folder
//      IniVal[18] := IniDirs[9]; //LightMaps folder
//      IniVal[22] := IniDirs[10]; //voxel folder
//      IniVal[32] := IniDirs[11]; //m3c folder
//      IniVal[35] := TStyleManager.ActiveStyle.Name;
//      Rewrite(f);
//      for i := 0 to 6 do Writeln(f, IniDirs[i]);
//      for i := 0 to IniMax do
//      begin
//        if i = 0 then
//          s := IntToStr(FormsSticky[1] +
//                     (GUI.bAniFormStick shl 2) +
//                     (FormsSticky[0] shl 4) +
//                     (FormsSticky[2]) shl 6)
//        else if i = 1 then s := GUI.Edit_M_4.Text
//        else if i = 23 then
//        begin
//          if GUI.cb_par_in_png.Checked then s := 'Yes' else s := 'No';
//        end
//        else if i = 30 then
//        begin
//          if GUI.CheckBox_M_14.Checked then s := 'Yes' else s := 'No';
//        end
//        else if i = 31 then
//        begin
//          if GUI.CheckBox_M_16.Checked then s := 'Yes' else s := 'No';
//        end
//        else s := IniVal[i];
//        Writeln(f, IniItem[i] + '  ' + s);
//      end;
//      for i := 0 to Length(IniHigherVersion) - 1 do
//        Writeln(f, IniHigherVersion[i]);
//    finally
//      CloseFile(f);
//    end;
//end;

end.
