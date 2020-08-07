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
    Panel1: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fe: TfmExplorer;
    function ExplorerGetLexer(const fn: string): string;
    procedure ExplorerClick(const fn: string; Dbl: boolean);
  public

  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  fe:= TfmExplorer.Create(Self);
  fe.Parent:= Panel1;
  fe.Align:= alClient;
  fe.Show;

  fe.IconDir:= ExtractFilePath(Application.ExeName)+'vscode_16x16';
  fe.OnGetLexer:= @ExplorerGetLexer;
  fe.OnItemClick:= @ExplorerClick;
  fe.Folder:= '/home/user/test';

  Caption:= 'Explorer: '+fe.Folder;
end;

function TfmMain.ExplorerGetLexer(const fn: string): string;
begin
  case LowerCase(ExtractFileExt(fn)) of
    '.pp', '.inc': Result:= 'Pascal';
    '.lpi', '.lpr', '.lps': Result:= 'XML';
    else Result:= '';
  end;
end;

procedure TfmMain.ExplorerClick(const fn: string; Dbl: boolean);
var
  s: string;
begin
  if Dbl then
    s:= '(dbl click)'
  else
    s:= '(click)';
  StatusBar1.SimpleText:= s+' "'+fn+'"';
end;

procedure TfmMain.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    fe.Folder:= SelectDirectoryDialog1.FileName;
end;

end.

