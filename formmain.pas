unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  formexplorer;

type

  { TfmMain }

  TfmMain = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fe: TfmExplorer;
    function GetLexer(const fn: string): string;
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
  fe.OnGetLexer:= @GetLexer;
  fe.Folder:= '/home/user/test';
end;

function TfmMain.GetLexer(const fn: string): string;
var
  ext: string;
begin
  ext:= ExtractFileExt(fn);
  case ext of
    '.c': Result:= 'C';
    '.cpp': Result:= 'C++';
    '.sh': Result:= 'Bash script';
    '.pas': Result:= 'Pascal';
    '.htm',
    '.html': Result:= 'HTML';
    '.js': Result:= 'JavaScript';
    '.css': Result:= 'CSS';
    '.md': Result:= 'Markdown';
    '.xml': Result:= 'XML';
    '.sql': Result:= 'SQL';
    '.php': Result:= 'PHP';
    '.py': Result:= 'Python';
    '.json': Result:= 'JSON';
    else Result:= '';
  end;
end;

procedure TfmMain.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    fe.Folder:= SelectDirectoryDialog1.FileName;
end;

end.

