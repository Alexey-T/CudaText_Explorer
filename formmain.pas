unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, formexplorer;

type

  { TfmMain }

  TfmMain = class(TForm)
    Button1: TButton;
    chkShowRoot: TCheckBox;
    chkShowDotNames: TCheckBox;
    Panel1: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure chkShowDotNamesChange(Sender: TObject);
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
  exp.Parent:= Panel1;
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
  sKind: array[TExplorerClickKind] of string = ('click', 'dbl-click', 'fold', 'unfold');
begin
  StatusBar1.SimpleText:= '"'+fn+'", '+sKind[Kind];
end;

procedure TfmMain.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    exp.Folder:= SelectDirectoryDialog1.FileName;
end;

procedure TfmMain.chkShowDotNamesChange(Sender: TObject);
begin
  ExplorerOptions.ShowDotNames:= chkShowDotNames.Checked;
  exp.Folder:= exp.Folder;
end;

procedure TfmMain.chkShowRootChange(Sender: TObject);
begin
  ExplorerOptions.ShowRootNode:= chkShowRoot.Checked;
  exp.Folder:= exp.Folder;
end;

end.

