unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Math, formexplorer;

type

  { TfmMain }

  TfmMain = class(TForm)
    BtnAdd: TButton;
    BtnClose: TButton;
    BtnFolder: TButton;
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
    procedure BtnAddClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnFolderClick(Sender: TObject);
    procedure chkShowDotNamesChange(Sender: TObject);
    procedure chkShowIconsChange(Sender: TObject);
    procedure chkShowRootChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LSelectionChange(Sender: TObject; User: boolean);
  private
    exp: TfmExplorer;
    procedure FakeOpenFile(const fn: string; AsTemp: boolean);
    function ExplorerGetLexer(const fn: string): string;
    procedure ExplorerClick(const fn: string; Kind: TExplorerClickKind);
    procedure ExplorerGetTabs(out ACount: integer; out ASelected: integer);
    procedure ExplorerGetTabProp(AIndex: integer; out ACaption, AFilename: string; out AModified: boolean);
    procedure UpdateTabs(ASelChange: boolean);
    procedure ExplorerTabSelect(AIndex: integer);
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

  ExplorerOptions.DirOfIcons:= ExtractFilePath(Application.ExeName)+'vscode_16x16';
  exp.OnGetLexer:= @ExplorerGetLexer;
  exp.OnItemClick:= @ExplorerClick;
  exp.OnGetTabs:= @ExplorerGetTabs;
  exp.OnGetTabProp:= @ExplorerGetTabProp;
  exp.OnTabSelect:= @ExplorerTabSelect;
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

procedure TfmMain.ExplorerClick(const fn: string; Kind: TExplorerClickKind);
const
  sKind: array[TExplorerClickKind] of string = ('-', 'click', 'dbl-click', 'fold', 'unfold');
begin
  StatusBar1.SimpleText:= '"'+fn+'", '+sKind[Kind];

  case Kind of
    eckFileClick:
      FakeOpenFile(fn, true);
    eckFileDblClick:
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
    AFilename:= 'filename'+IntToStr(AIndex);
    AModified:= Odd(AIndex);
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

procedure TfmMain.BtnFolderClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    exp.Folder:= SelectDirectoryDialog1.FileName;
end;

var
  NTab: integer = 0;

procedure TfmMain.BtnAddClick(Sender: TObject);
begin
  Inc(NTab);
  L.Items.Add('Tab '+IntToStr(NTab));
  L.ItemIndex:= L.Items.Count-1;
  UpdateTabs(false);
end;

procedure TfmMain.BtnCloseClick(Sender: TObject);
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
  ExplorerOptions.ShowDotNames:= chkShowDotNames.Checked;
  exp.Refresh;
end;

procedure TfmMain.chkShowIconsChange(Sender: TObject);
begin
  ExplorerOptions.ShowIcons:= chkShowIcons.Checked;
  exp.Refresh;
end;

procedure TfmMain.chkShowRootChange(Sender: TObject);
begin
  ExplorerOptions.ShowRootNode:= chkShowRoot.Checked;
  exp.Refresh;
end;

end.

