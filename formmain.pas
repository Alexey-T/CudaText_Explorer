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

  Caption:= 'Explorer: '+fe.Folder;
end;

function TfmMain.GetLexer(const fn: string): string;
begin
  case LowerCase(ExtractFileExt(fn)) of
    '.pp', '.inc': Result:= 'Pascal';
    '.lpi', '.lpr', '.lps': Result:= 'XML';
    else Result:= '';
  end;
end;

procedure TfmMain.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    fe.Folder:= SelectDirectoryDialog1.FileName;
end;

end.

