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

  fe.Folder:= '/home/user/test';
end;

procedure TfmMain.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    fe.Folder:= SelectDirectoryDialog1.FileName;
end;

end.

