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
    ListBox1: TListBox;
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
  private
    exp: TfmExplorer;
    function ExplorerGetLexer(const fn: string): string;
    procedure ExplorerClick(const fn: string; Kind: TExplorerClickKind);
  public

  end;

var
  fmMain: TfmMain;

implementation

uses
  FileUtil;

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  exp:= TfmExplorer.Create(Self);
  exp.Parent:= PanelLeft;
  exp.Align:= alClient;
  exp.Show;

  ExplorerOptions.IconDir:= ExtractFilePath(Application.ExeName)+'vscode_16x16';
  exp.OnGetLexer:= @ExplorerGetLexer;
  exp.OnItemClick:= @ExplorerClick;
  exp.Folder:= ExtractFileDir(Application.ExeName);

  Caption:= 'Explorer: '+exp.Folder;
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
  Listbox1.Items.Add('Tab '+IntToStr(NTab));
  Listbox1.ItemIndex:= Listbox1.Items.Count-1;
end;

procedure TfmMain.BtnCloseClick(Sender: TObject);
var
  N: integer;
begin
  with Listbox1 do
    if ItemIndex>=0 then
    begin
      N:= ItemIndex;
      Items.Delete(ItemIndex);
      ItemIndex:= Min(N, Items.Count-1);
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

