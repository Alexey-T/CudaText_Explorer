unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Math,
  ATFlatThemes,
  ATShellBase,
  ATShellTreeview,
  formexplorer,
  formmenubox;

type
  { TfmMain }

  TfmMain = class(TForm)
    BtnTabAdd: TButton;
    BtnTabClose: TButton;
    BtnFolder: TButton;
    BtnFolderClose: TButton;
    BtnFocusFile: TButton;
    chkShowBrackets: TCheckBox;
    chkShowDotNames: TCheckBox;
    chkShowIcons: TCheckBox;
    chkShowRoot: TCheckBox;
    ListTabs: TListBox;
    PanelLeft: TPanel;
    Panel2: TPanel;
    PanelRt: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    TimerTabs: TTimer;
    procedure BtnFocusFileClick(Sender: TObject);
    procedure BtnTabAddClick(Sender: TObject);
    procedure BtnTabCloseClick(Sender: TObject);
    procedure BtnFolderClick(Sender: TObject);
    procedure BtnFolderCloseClick(Sender: TObject);
    procedure chkShowBracketsChange(Sender: TObject);
    procedure chkShowDotNamesChange(Sender: TObject);
    procedure chkShowIconsChange(Sender: TObject);
    procedure chkShowRootChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListTabsSelectionChange(Sender: TObject; User: boolean);
    procedure TimerTabsTimer(Sender: TObject);
  private
    exp: TfmExplorer;
    FTabsSelChanged: boolean;
    procedure FakeOpenFile(const fn: string; AsTemp: boolean);
    function ExplorerGetLexer(const fn: string): string;
    procedure ExplorerClick(const fn: string; Kind: TATShellTreeviewClick);
    procedure ExplorerGetTabs(out ACount: integer; out ASelected: integer);
    procedure ExplorerGetTabProp(AIndex: integer; out ACaption, AFilename: string; out AModified: boolean);
    procedure UpdateTabs(ASelChange: boolean);
    procedure ExplorerTabSelect(AIndex: integer);
    procedure ExplorerTabClose(AIndex: integer);
  public

  end;

var
  fmMain: TfmMain;

implementation

uses
  FileUtil;

{$R *.lfm}

const
  cTempPrefix = '* ';

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  exp:= TfmExplorer.Create(Self);
  exp.Parent:= PanelLeft;
  exp.Align:= alClient;
  exp.BorderStyle:= bsNone;
  exp.Show;

  ATShellOptions.DirOfIcons:= ExtractFilePath(Application.ExeName)+'vscode_16x16';

  ATShellIcons.OnDetect:= @ExplorerGetLexer;
  exp.OnItemClick:= @ExplorerClick;
  exp.OnGetTabs:= @ExplorerGetTabs;
  exp.OnGetTabProp:= @ExplorerGetTabProp;
  exp.OnTabSelect:= @ExplorerTabSelect;
  exp.OnTabClose:= @ExplorerTabClose;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  exp.UpdateUI;
  exp.UpdateCaptionTabs;
  exp.UpdateCaptionTree;
  exp.UpdatePanelSizes;
  exp.Folder:= ExtractFileDir(Application.ExeName);
end;

procedure TfmMain.ListTabsSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateTabs(true);
end;

procedure TfmMain.TimerTabsTimer(Sender: TObject);
begin
  TimerTabs.Enabled:= false;
  exp.UpdateTabs(FTabsSelChanged);
end;

function TfmMain.ExplorerGetLexer(const fn: string): string;
begin
  case LowerCase(ExtractFileExt(fn)) of
    '.pp', '.inc': Result:= 'Pascal';
    '.lpi', '.lpr', '.lps': Result:= 'XML';
    else Result:= '';
  end;
end;

procedure TfmMain.ExplorerClick(const fn: string; Kind: TATShellTreeviewClick);
const
  sKind: array[TATShellTreeviewClick] of string = ('-', 'click', 'dbl-click', 'fold', 'unfold');
begin
  StatusBar1.SimpleText:= '"'+fn+'", '+sKind[Kind];

  case Kind of
    astcFileClick:
      FakeOpenFile(fn, true);
    astcFileDblClick:
      FakeOpenFile(fn, false);
  end;
end;

procedure TfmMain.FakeOpenFile(const fn: string; AsTemp: boolean);
var
  cap: string;
  n, i: integer;
begin
 try
  cap:= ExtractFileName(fn) + ' ('+ExtractFileDir(fn)+')';

  n:= ListTabs.Items.IndexOf(cap);
  if n>=0 then
  begin
    ListTabs.ItemIndex:= n;
    exit;
  end;

  n:= ListTabs.Items.IndexOf(cTempPrefix+cap);
  if n>=0 then
  begin
    if AsTemp then
      cap:= cTempPrefix+cap;
    ListTabs.Items[n]:= cap;
    ListTabs.ItemIndex:= n;
    exit;
  end;

  if AsTemp then
  begin
    n:= -1;
    for i:= 0 to ListTabs.Items.Count-1 do
      if Pos(cTempPrefix, ListTabs.Items[i])=1 then
      begin
        ListTabs.Items[i]:= cTempPrefix+cap;
        ListTabs.ItemIndex:= n;
        exit;
      end;
  end;

  if AsTemp then
    cap:= cTempPrefix+cap;
  ListTabs.Items.Add(cap);
  ListTabs.ItemIndex:= ListTabs.Items.Count-1;
 finally
   UpdateTabs(false);
 end;
end;

procedure TfmMain.ExplorerGetTabs(out ACount: integer; out ASelected: integer);
begin
  ACount:= ListTabs.Items.Count;
  ASelected:= ListTabs.ItemIndex;
end;

procedure TfmMain.ExplorerGetTabProp(AIndex: integer; out ACaption,
  AFilename: string; out AModified: boolean);
begin
  if (AIndex>=0) and (AIndex<ListTabs.Items.Count) then
  begin
    ACaption:= ListTabs.Items[AIndex];
    if Pos(cTempPrefix, ACaption)=1 then
      Delete(ACaption, 1, Length(cTempPrefix));
    AFilename:= 'file'+IntToStr(AIndex);
    AModified:= AIndex mod 10=0;
  end
  else
  begin
    ACaption:= '';
    AFilename:= '';
    AModified:= false;
  end;
end;

procedure TfmMain.UpdateTabs(ASelChange: boolean);
begin
  FTabsSelChanged:= ASelChange;
  TimerTabs.Enabled:= true;
end;

procedure TfmMain.ExplorerTabSelect(AIndex: integer);
begin
  ListTabs.ItemIndex:= AIndex;
end;

procedure TfmMain.ExplorerTabClose(AIndex: integer);
var
  N: integer;
begin
  if AIndex<ListTabs.Items.Count then
  begin
    N:= ListTabs.ItemIndex;
    ListTabs.Items.Delete(AIndex);
    if ListTabs.Items.Count>0 then
      ListTabs.ItemIndex:= Max(0, Min(N, ListTabs.Items.Count-1));
    UpdateTabs(false);
  end;
end;

procedure TfmMain.BtnFolderClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    exp.Folder:= SelectDirectoryDialog1.FileName;
end;

procedure TfmMain.BtnFolderCloseClick(Sender: TObject);
begin
  exp.Folder:= '';
end;

procedure TfmMain.chkShowBracketsChange(Sender: TObject);
begin
  ATShellOptions.ShowFolderBrackets:= chkShowBrackets.Checked;
  exp.Refresh;
end;

var
  MaxTabIndex: integer = 0;

procedure TfmMain.BtnTabAddClick(Sender: TObject);
begin
  Inc(MaxTabIndex);
  ListTabs.Items.Add('Tab '+IntToStr(MaxTabIndex));
  ListTabs.ItemIndex:= ListTabs.Items.Count-1;
  UpdateTabs(false);
end;

procedure TfmMain.BtnFocusFileClick(Sender: TObject);
var
  LFiles: TStringList;
  fm: TfmMenuBox;
  NSel: integer;
  fn: string;
begin
  LFiles:= TStringList.Create;
  try
    FindAllFiles(LFiles, exp.Folder, '*', true);
    if LFiles.Count=0 then exit;
    LFiles.Sort;

    fm:= TfmMenuBox.Create(nil);
    try
      fm.ListBox.Items.AddStrings(LFiles);
      fm.ListBox.ItemIndex:= 0;
      if fm.ShowModal<>mrOk then exit;
      NSel:= fm.ListBox.ItemIndex;
    finally
      FreeAndNil(fm);
    end;

    if NSel<0 then exit;
    fn:= LFiles[NSel];
  finally
    LFiles.Free;
  end;

  exp.FocusFilename(fn);
end;

procedure TfmMain.BtnTabCloseClick(Sender: TObject);
var
  N: integer;
begin
  with ListTabs do
    if ItemIndex>=0 then
    begin
      N:= ItemIndex;
      Items.Delete(ItemIndex);
      ItemIndex:= Min(N, Items.Count-1);
      UpdateTabs(false);
    end;
end;

procedure TfmMain.chkShowDotNamesChange(Sender: TObject);
begin
  ATShellOptions.ShowDotNames:= chkShowDotNames.Checked;
  exp.Refresh;
end;

procedure TfmMain.chkShowIconsChange(Sender: TObject);
begin
  ATShellOptions.ShowIcons:= chkShowIcons.Checked;
  exp.Refresh;
end;

procedure TfmMain.chkShowRootChange(Sender: TObject);
begin
  ATShellOptions.ShowRootNode:= chkShowRoot.Checked;
  exp.Refresh;
end;

initialization

  ATFlatTheme.FontName:= 'default';
  ATFlatTheme.FontSize:= 10;
  ATFlatTheme.ColorFont:= clBlack;

end.

