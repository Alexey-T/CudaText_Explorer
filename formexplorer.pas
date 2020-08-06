unit formexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls;

type

  { TfmExplorer }

  TfmExplorer = class(TForm)
    PanelTree: TPanel;
    PanelTabs: TPanel;
    Tree: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    FFolder: string;
    FRootNode: TTreeNode;
    FShowDotNames: boolean;
    function PrettyDirName(const S: string): string;
    procedure FillTreeForFolder(const AFolder: string; ANode: TTreeNode);
    procedure SetFolder(const AValue: string);
  public
    property Folder: string read FFolder write SetFolder;
    property ShowDotNames: boolean read FShowDotNames write FShowDotNames;
  end;

var
  fmExplorer: TfmExplorer;

implementation

uses
  FileUtil;

{$R *.lfm}

type
  TExplorerTreeData = class
  public
    Path: string;
    IsDir: boolean;
    Expanded: boolean;
  end;

{ TfmExplorer }

procedure TfmExplorer.FormCreate(Sender: TObject);
begin
  Tree.ShowRoot:= false;
  Tree.ReadOnly:= true;
  Tree.RowSelect:= true;
  //Tree.HotTrack:= true;

  FShowDotNames:= false;
end;

procedure TfmExplorer.FormDestroy(Sender: TObject);
begin
  Tree.Items.Clear;
end;

procedure TfmExplorer.TreeClick(Sender: TObject);
var
  P: TPoint;
  Node: TTreeNode;
  Data: TExplorerTreeData;
begin
  P:= Tree.ScreenToClient(Mouse.CursorPos);
  Node:= Tree.GetNodeAt(P.X, P.Y);
  if Assigned(Node) then
    if Assigned(Node.Data) then
    begin
      Data:= TExplorerTreeData(Node.Data);
      if Data.IsDir then
        if Node.Expanded then
          Node.Collapse(false)
        else
          Node.Expand(false);
    end;
end;

procedure TfmExplorer.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    TObject(Node.Data).Free;
end;

procedure TfmExplorer.TreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  Data: TExplorerTreeData;
begin
  AllowExpansion:= true;
  Data:= TExplorerTreeData(Node.Data);
  if Data=nil then exit;
  if Data.Expanded then exit;
  if not Data.IsDir then exit;

  Data.Expanded:= true;
  FillTreeForFolder(Data.Path, Node);
  //ShowMessage('fill: '+Data.Path);
end;

function TfmExplorer.PrettyDirName(const S: string): string;
begin
  Result:= '['+S+']';
end;

procedure TfmExplorer.SetFolder(const AValue: string);
begin
  if FFolder=AValue then Exit;
  FFolder:= AValue;

  Tree.Items.Clear;

  if (FFolder='') or not DirectoryExists(FFolder) then
    exit;

  FRootNode:= Tree.Items.Add(nil, PrettyDirName(ExtractFileName(FFolder)));
  FillTreeForFolder(FFolder, FRootNode);
  FRootNode.Expand(false);
end;

function _CompareFilenames(L: TStringList; Index1, Index2: integer): integer;
var
  s1, s2, ext1, ext2: string;
  d1, d2: PtrInt;
begin
  d1:= PtrInt(L.Objects[Index1]);
  d2:= PtrInt(L.Objects[Index2]);
  if d1<>d2 then
    exit(d2-d1);

  s1:= L[Index1];
  s2:= L[Index2];
  ext1:= ExtractFileExt(s1);
  ext2:= ExtractFileExt(s2);

  Result:= stricomp(PChar(ext1), PChar(ext2));
  if Result=0 then
    Result:= stricomp(PChar(s1), PChar(s2));
end;

procedure TfmExplorer.FillTreeForFolder(const AFolder: string; ANode: TTreeNode);
var
  Node: TTreeNode;
  Rec: TSearchRec;
  List: TStringList;
  bDir: boolean;
  NData: PtrInt;
  Data: TExplorerTreeData;
  S: string;
  i: integer;
begin
  if ANode=nil then exit;
  ANode.DeleteChildren;

  List:= TStringList.Create;
  try
    if FindFirst(AFolder+DirectorySeparator+'*', faAnyFile, Rec)=0 then
      repeat
        S:= Rec.Name;
        if (S='.') or (S='..') then Continue;
        if (S[1]='.') and not FShowDotNames then Continue;

        if (Rec.Attr and faDirectory)<>0 then
          NData:= 1
        else
          NData:= 0;
        List.AddObject(AFolder+DirectorySeparator+S, TObject(NData));
      until FindNext(Rec)<>0;
    FindClose(Rec);

    List.CustomSort(@_CompareFilenames);

    for i:= 0 to List.Count-1 do
    begin
      S:= ExtractFileName(List[i]);
      bDir:= List.Objects[i]<>nil;

      Data:= TExplorerTreeData.Create;
      Data.Path:= AFolder+DirectorySeparator+S;
      Data.IsDir:= bDir;
      Data.Expanded:= false;

      if bDir then
        S:= PrettyDirName(S);

      Node:= Tree.Items.AddChildObject(ANode, S, Data);
      //add fictive child, to show expand arrow
      if bDir then
        Tree.Items.AddChild(Node, '?');
    end;
  finally
    FreeAndNil(List);
  end;
end;

end.

