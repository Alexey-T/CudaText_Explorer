unit formexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  jsonConf;

type
  TExplorerOnGetLexer = function(const fn: string): string of object;

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
    FOnGetLexer: TExplorerOnGetLexer;
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
    ListExt: TStringList;
    ListExtToLexer: TStringList;
    procedure InitIconConfig;
    function GetImageIndex(const AFileName: string; AIsDir: boolean): integer;
    function GetImageIndexFromPng(const AFilename: string): integer;
    function DetectUsualFiles(const AExt: string; var AIndex: integer): boolean;
    function DetectLexerByExt(const AExt: string; var ALexer: string): boolean;
    function PrettyDirName(const S: string): string;
    procedure FillTreeForFolder(const AFolder: string; ANode: TTreeNode);
    procedure SetFolder(const AValue: string);
  public
    property Folder: string read FFolder write SetFolder;
    property ShowDotNames: boolean read FShowDotNames write FShowDotNames;
    property IconDir: string read FIconDir write FIconDir;
    property OnGetLexer: TExplorerOnGetLexer read FOnGetLexer write FOnGetLexer;
  end;

implementation

{$R *.lfm}

type
  TExplorerTreeData = class
  public
    Path: string;
    IsDir: boolean;
    Expanded: boolean;
  end;

  TExplorerStringDataItem = class
    Str: string;
  end;

{ TfmExplorer }

procedure TfmExplorer.FormCreate(Sender: TObject);
  //
  procedure AddLex(const AExt, ALexer: string);
  var
    D: TExplorerStringDataItem;
  begin
    D:= TExplorerStringDataItem.Create;
    D.Str:= ALexer;
    ListExtToLexer.AddObject(AExt, D);
  end;
  //
begin
  Tree.ShowRoot:= false;
  Tree.ReadOnly:= true;
  Tree.RowSelect:= true;
  //Tree.HotTrack:= true;

  FShowDotNames:= false;

  ListExt:= TStringList.Create;
  ListExt.Sorted:= true;

  ListExtToLexer:= TStringList.Create;
  ListExtToLexer.Sorted:= true;

  AddLex('c', 'C');
  AddLex('h', 'C');
  AddLex('cpp', 'C++');
  AddLex('hpp', 'C++');
  AddLex('htm', 'HTML');
  AddLex('html', 'HTML');
  AddLex('css', 'CSS');
  AddLex('js', 'JavaScript');
  AddLex('json', 'JSON');
  AddLex('java', 'Java');
  AddLex('sh', 'Bash script');
  AddLex('cmd', 'Batch files');
  AddLex('md', 'Markdown');
  AddLex('xml', 'XML');
  AddLex('php', 'PHP');
  AddLex('py', 'Python');
  AddLex('ini', 'Ini files');
  AddLex('inf', 'Ini files');
  AddLex('rs', 'Rust');
  AddLex('vbs', 'VBScript');
  AddLex('lua', 'Lua');
  AddLex('sql', 'SQL');
  AddLex('pas', 'Pascal');
  AddLex('yml', 'YAML');
  AddLex('yaml', 'YAML');
  AddLex('asm', 'Assembly');
  AddLex('cs', 'C#');
  AddLex('ts', 'TypeScript');
  AddLex('rb', 'Ruby');
  AddLex('r', 'R');
  AddLex('scss', 'SCSS');
  AddLex('sass', 'Sass');
  AddLex('pl', 'Perl');
  AddLex('go', 'Go');
  AddLex('dart', 'Dart');
  AddLex('coffee', 'CoffeeScript');
end;

procedure TfmExplorer.FormDestroy(Sender: TObject);
begin
  Tree.Items.Clear;
  FreeAndNil(ListExtToLexer);
  FreeAndNil(ListExt);
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
      Node.SelectedIndex:= Node.ImageIndex;

      //add fictive child, to show expand arrow
      if bDir then
        Tree.Items.AddChild(Node, '?');
    end;
  finally
    FreeAndNil(List);
  end;
end;

function TfmExplorer.DetectUsualFiles(const AExt: string; var AIndex: integer): boolean;
begin
  Result:= true;
  case AExt of
    'log',
    'txt':
      AIndex:= FIconIndexDefault;
    'zip',
    'rar',
    'tar',
    'xz',
    'gz',
    '7z':
      AIndex:= FIconIndexZip;
    'png',
    'gif',
    'bmp',
    'jpg',
    'jpeg',
    'svg',
    'ico':
      AIndex:= FIconIndexPic;
    'exe',
    'dll',
    'dat',
    'so',
    'dylib',
    'dbg',
    'chm',
    'pyc',
    'o',
    'a',
    'mp3',
    'mp4',
    'm4a',
    'mpg',
    'mpeg',
    'avi',
    'mov',
    'ogg',
    'flac',
    'webm',
    'pdf',
    'doc',
    'docx',
    'xls',
    'xlsx',
    'ppt',
    'pptx':
      AIndex:= FIconIndexBin;
    else
      Result:= false;
  end;
end;

procedure TfmExplorer.InitIconConfig;
var
  fnConfig: string;
begin
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
end;

function TfmExplorer.GetImageIndex(const AFileName: string; AIsDir: boolean): integer;
var
  SLexer: string;
  fnIcon: string;
  ext: string;
  i: integer;
begin
  Result:= -1;
  if not Assigned(OnGetLexer) then exit;
  InitIconConfig;

  if AIsDir then
    exit(FIconIndexDir);
  Result:= FIconIndexDefault;

  ext:= LowerCase(ExtractFileExt(AFileName));
  if ext<>'' then
    Delete(ext, 1, 1);
  if DetectUsualFiles(ext, Result) then exit;

  if ListExt.Find(ext, i) then
    exit(PtrInt(ListExt.Objects[i]));

  if not DetectLexerByExt(ext, SLexer) then
    SLexer:= OnGetLexer(AFileName);
  if SLexer='' then exit;

  fnIcon:= FIconCfg.GetValue(SLexer, '');
  if fnIcon='' then exit;

  Result:= GetImageIndexFromPng(fnIcon);
  ListExt.AddObject(ext, TObject(PtrInt(Result)));

  //ShowMessage('load ico: '+SLexer);
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

function TfmExplorer.DetectLexerByExt(const AExt: string; var ALexer: string): boolean;
var
  N: integer;
begin
  Result:= ListExtToLexer.Find(AExt, N);
  if Result then
    ALexer:= TExplorerStringDataItem(ListExtToLexer.Objects[N]).Str;
end;

end.

