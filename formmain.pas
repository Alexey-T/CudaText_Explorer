unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Math,
  ATFlatThemes,
  ATShellBase,
  ATShellTreeview,
  formexplorer;

type

  { TfmMain }

  TfmMain = class(TForm)
    BtnTabAdd: TButton;
    BtnTabClose: TButton;
    BtnFolder: TButton;
    BtnFolderClose: TButton;
    chkShowDotNames: TCheckBox;
    chkShowIcons: TCheckBox;
    chkShowRoot: TCheckBox;
    L: TListBox;
    PanelLeft: TPanel;
    Panel2: TPanel;
    PanelRt: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    procedure BtnTabAddClick(Sender: TObject);
    procedure BtnTabCloseClick(Sender: TObject);
    procedure BtnFolderClick(Sender: TObject);
    procedure BtnFolderCloseClick(Sender: TObject);
    procedure chkShowDotNamesChange(Sender: TObject);
    procedure chkShowIconsChange(Sender: TObject);
    procedure chkShowRootChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LSelectionChange(Sender: TObject; User: boolean);
  private
    exp: TfmExplorer;
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

  ATShellIcons.OnDetectLexer:= @ExplorerGetLexer;
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

procedure TfmMain.LSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateTabs(true);
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

  n:= L.Items.IndexOf(cap);
  if n>=0 then
  begin
    L.ItemIndex:= n;
    exit;
  end;

  n:= L.Items.IndexOf(cTempPrefix+cap);
  if n>=0 then
  begin
    if AsTemp then
      cap:= cTempPrefix+cap;
    L.Items[n]:= cap;
    L.ItemIndex:= n;
    exit;
  end;

  if AsTemp then
  begin
    n:= -1;
    for i:= 0 to L.Items.Count-1 do
      if Pos(cTempPrefix, L.Items[i])=1 then
      begin
        L.Items[i]:= cTempPrefix+cap;
        L.ItemIndex:= n;
        exit;
      end;
  end;

  if AsTemp then
    cap:= cTempPrefix+cap;
  L.Items.Add(cap);
  L.ItemIndex:= L.Items.Count-1;
 finally
   UpdateTabs(false);
 end;
end;

procedure TfmMain.ExplorerGetTabs(out ACount: integer; out ASelected: integer);
begin
  ACount:= L.Items.Count;
  ASelected:= L.ItemIndex;
end;

procedure TfmMain.ExplorerGetTabProp(AIndex: integer; out ACaption,
  AFilename: string; out AModified: boolean);
begin
  if (AIndex>=0) and (AIndex<L.Items.Count) then
  begin
    ACaption:= L.Items[AIndex];
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
  exp.UpdateTabs(ASelChange);
end;

procedure TfmMain.ExplorerTabSelect(AIndex: integer);
begin
  L.ItemIndex:= AIndex;
end;

procedure TfmMain.ExplorerTabClose(AIndex: integer);
var
  N: integer;
begin
  if AIndex<L.Items.Count then
  begin
    N:= L.ItemIndex;
    L.Items.Delete(AIndex);
    if L.Items.Count>0 then
      L.ItemIndex:= Max(0, Min(N, L.Items.Count-1));
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

var
  MaxTabIndex: integer = 0;

procedure TfmMain.BtnTabAddClick(Sender: TObject);
begin
  Inc(MaxTabIndex);
  L.Items.Add('Tab '+IntToStr(MaxTabIndex));
  L.ItemIndex:= L.Items.Count-1;
  UpdateTabs(false);
end;

procedure TfmMain.BtnTabCloseClick(Sender: TObject);
var
  N: integer;
begin
  with L do
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

