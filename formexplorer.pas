unit formexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  jsonConf;

type
  TExplorerGetLexer = function(const fn: string): string of object;

  TExplorerImageArray = array of
    record
      SavedLexer: string;
      SavedIndex: integer;
    end;

type
  { TfmExplorer }

  TfmExplorer = class(TForm)
    Images: TImageList;
    PanelTree: TPanel;
    PanelTabs: TPanel;
    Tree: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
  private
    FFolder: string;
    FIconDir: string;
    FRootNode: TTreeNode;
    FShowDotNames: boolean;
    FOnGetLexer: TExplorerGetLexer;
    FIconCfg: TJSONConfig;
    FIconNameDefault: string;
    FIconNameDir: string;
    FIconNameZip: string;
    FIconNamePic: string;
    FIconNameBin: string;
    FIconIndexDefault: integer;
    FIconIndexDir: integer;
    FIconIndexZip: integer;
    FIconIndexPic: integer;
    FIconIndexBin: integer;
    FIconCache: TExplorerImageArray;
    function GetImageIndex(const AFileName: string; AIsDir: boolean): integer;
    function GetImageIndexFromPng(const AFilename: string): integer;
    function PrettyDirName(const S: string): string;
    procedure FillTreeForFolder(const AFolder: string; ANode: TTreeNode);
    procedure SetFolder(const AValue: string);
  public
    property Folder: string read FFolder write SetFolder;
    property ShowDotNames: boolean read FShowDotNames write FShowDotNames;
    property IconDir: string read FIconDir write FIconDir;
    property OnGetLexer: TExplorerGetLexer read FOnGetLexer write FOnGetLexer;
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

  SetLength(FIconCache, 0);
end;

procedure TfmExplorer.FormDestroy(Sender: TObject);
begin
  Tree.Items.Clear;
  if Assigned(FIconCfg) then
    FreeAndNil(FIconCfg);
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
const
  StrAllFiles = {$ifdef windows} '*.*' {$else} '*' {$endif};
var
  Node: TTreeNode;
  Rec: TSearchRec;
  List: TStringList;
  bDir: boolean;
  Data: TExplorerTreeData;
  S: string;
  i: integer;
begin
  if ANode=nil then exit;
  ANode.DeleteChildren;

  List:= TStringList.Create;
  try
    if FindFirst(AFolder+DirectorySeparator+StrAllFiles, faAnyFile, Rec)=0 then
      repeat
        S:= Rec.Name;
        if (S='.') or (S='..') then Continue;
        if (S[1]='.') and not FShowDotNames then Continue;

        bDir:= (Rec.Attr and faDirectory)<>0;
        List.AddObject(S, TObject(PtrInt(bDir)));
      until FindNext(Rec)<>0;
    FindClose(Rec);

    List.CustomSort(@_CompareFilenames);

    for i:= 0 to List.Count-1 do
    begin
      S:= List[i];
      bDir:= List.Objects[i]<>nil;

      Data:= TExplorerTreeData.Create;
      Data.Path:= AFolder+DirectorySeparator+S;
      Data.IsDir:= bDir;
      Data.Expanded:= false;

      if bDir then
        S:= PrettyDirName(S);

      Node:= Tree.Items.AddChildObject(ANode, S, Data);
      Node.ImageIndex:= GetImageIndex(Data.Path, Data.IsDir);

      //add fictive child, to show expand arrow
      if bDir then
        Tree.Items.AddChild(Node, '?');
    end;
  finally
    FreeAndNil(List);
  end;
end;

function TfmExplorer.GetImageIndex(const AFileName: string; AIsDir: boolean): integer;
var
  ext: string;
  SLexer: string;
  fnConfig: string;
  fnIcon: string;
  i: integer;
begin
  Result:= -1;
  if not Assigned(OnGetLexer) then exit;

  if not Assigned(FIconCfg) then
  begin
    fnConfig:= FIconDir+DirectorySeparator+'icons.json';
    if not FileExists(fnConfig) then exit;

    FIconCfg:= TJSONConfig.Create(Self);
    FIconCfg.Filename:= fnConfig;

    FIconNameDefault:= FIconCfg.GetValue('_', '');
    FIconNameDir:= FIconCfg.GetValue('_dir', '');
    FIconNameZip:= FIconCfg.GetValue('_zip', '');
    FIconNamePic:= FIconCfg.GetValue('_img', '');
    FIconNameBin:= FIconCfg.GetValue('_bin', '');

    FIconIndexDefault:= GetImageIndexFromPng(FIconNameDefault);
    FIconIndexDir:= GetImageIndexFromPng(FIconNameDir);
    FIconIndexZip:= GetImageIndexFromPng(FIconNameZip);
    FIconIndexPic:= GetImageIndexFromPng(FIconNamePic);
    FIconIndexBin:= GetImageIndexFromPng(FIconNameBin);
  end;

  if AIsDir then
    exit(FIconIndexDir);

  case ExtractFileExt(AFileName) of
    '.zip',
    '.rar',
    '.tar',
    '.xz',
    '.gz',
    '.7z':
      exit(FIconIndexZip);
    '.png',
    '.gif',
    '.bmp',
    '.jpg',
    '.jpeg',
    '.ico':
      exit(FIconIndexPic);
    '.exe',
    '.dll',
    '.dat',
    '.so',
    '.dylib',
    '.dbg',
    '.chm':
      exit(FIconIndexBin);
    '.log',
    '.txt':
      exit(FIconIndexDefault);
  end;

  Result:= FIconIndexDefault;

  SLexer:= OnGetLexer(AFileName);
  if SLexer='' then exit;

  for i:= 0 to High(FIconCache) do
    with FIconCache[i] do
      if SavedLexer=SLexer then
        exit(SavedIndex);

  fnIcon:= FIconCfg.GetValue(SLexer, '');
  if fnIcon='' then exit;

  Result:= GetImageIndexFromPng(fnIcon);

  SetLength(FIconCache, Length(FIconCache)+1);
  with FIconCache[High(FIconCache)] do
  begin
    SavedLexer:= SLexer;
    SavedIndex:= Result;
  end;

  //Showmessage('load ico: '+SLexer);
end;

function TfmExplorer.GetImageIndexFromPng(const AFilename: string): integer;
var
  Img: TPortableNetworkGraphic;
  fn: string;
begin
  fn:= FIconDir+DirectorySeparator+AFilename;
  if not FileExists(fn) then
    exit(FIconIndexDefault);

  Img:= TPortableNetworkGraphic.Create;
  try
    Img.LoadFromFile(fn);
    Result:= Images.Add(Img, nil);
  finally
    FreeAndNil(Img);
  end;
end;

end.

