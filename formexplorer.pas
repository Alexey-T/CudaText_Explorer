unit formexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  jsonConf;

type
  TExplorerOptions = record
    IconDir: string;
    ShowRootNode: boolean;
    ShowDotNames: boolean;
    ShowDotNamesFirst: boolean;
    ShowFolderBrackets: boolean;
    ShowIcons: boolean;
    TextEmpty: string;
    TextEmptyWithHidden: string;
  end;

var
  ExplorerOptions: TExplorerOptions;

type
  TExplorerClickKind = (
    eckFileClick,
    eckFileDblClick,
    eckFolderFold,
    eckFolderUnfold
    );

type
  TExplorerOnItemClick = procedure(const AFileName: string; AKind: TExplorerClickKind) of object;
  TExplorerOnGetLexer = function(const AFileName: string): string of object;

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
    procedure TreeDblClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
  private
    FFolder: string;
    FOnGetLexer: TExplorerOnGetLexer;
    FOnItemClick: TExplorerOnItemClick;
    FIconCfg: TJSONConfig;
    FIconIndexDefault: integer;
    FIconIndexDir: integer;
    FIconIndexZip: integer;
    FIconIndexPic: integer;
    FIconIndexBin: integer;
    ListExt: TStringList;
    ListLexer: TStringList;
    ListExtToLexer: TStringList;
    procedure InitCommonLexers;
    procedure InitUsualExtensions;
    procedure InitIconConfig;
    procedure HandleClick(ADouble: boolean);
    procedure ReadFolder(const AFolder: string; AList: TStringList; out ACountHidden: integer);
    function GetImageIndex(const AFileName: string; AIsDir: boolean): integer;
    function GetImageIndexFromPng(const AFilename: string): integer;
    function PrettyDirName(const S: string): string;
    procedure FillTreeForFolder(const AFolder: string; ANode: TTreeNode);
    procedure SetFolder(const AValue: string);
  public
    procedure Refresh;
    property Folder: string read FFolder write SetFolder;
    property OnGetLexer: TExplorerOnGetLexer read FOnGetLexer write FOnGetLexer;
    property OnItemClick: TExplorerOnItemClick read FOnItemClick write FOnItemClick;
  end;

implementation

{$R *.lfm}

type
  TExplorerNodeData = class
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
begin
  Tree.ShowRoot:= false;
  Tree.ReadOnly:= true;
  Tree.RowSelect:= true;

  ListExt:= TStringList.Create;
  ListExt.Sorted:= true;

  ListLexer:= TStringList.Create;
  ListLexer.Sorted:= true;

  ListExtToLexer:= TStringList.Create;
  ListExtToLexer.Sorted:= true;
  ListExtToLexer.OwnsObjects:= true;

  InitCommonLexers;
end;

procedure TfmExplorer.FormDestroy(Sender: TObject);
begin
  Tree.Items.Clear;
  FreeAndNil(ListExtToLexer);
  FreeAndNil(ListLexer);
  FreeAndNil(ListExt);
  if Assigned(FIconCfg) then
    FreeAndNil(FIconCfg);
end;

procedure TfmExplorer.InitCommonLexers;
  //
  procedure AddLex(const AExt, ALexer: string); inline;
  var
    D: TExplorerStringDataItem;
  begin
    D:= TExplorerStringDataItem.Create;
    D.Str:= ALexer;
    ListExtToLexer.AddObject(AExt, D);
  end;
  //
begin
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
  AddLex('bat', 'Batch files');
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
  AddLex('less', 'LESS');
  AddLex('pl', 'Perl');
  AddLex('go', 'Go');
  AddLex('dart', 'Dart');
  AddLex('hs', 'Haskell');
  AddLex('erl', 'Erlang');
  AddLex('clj', 'Clojure');
  AddLex('coffee', 'CoffeeScript');
  AddLex('lisp', 'Lisp');
  AddLex('rst', 'reStructuredText');
  AddLex('haml', 'Haml');
  AddLex('scala', 'Scala');
  AddLex('tex', 'LaTeX');
  AddLex('tcl', 'Tcl');
  AddLex('textile', 'Textile');
  AddLex('au3', 'AutoIt');
  AddLex('ahk', 'AutoHotkey');
  AddLex('diff', 'Diff');
  AddLex('cmake', 'CMake');
  AddLex('ps1', 'PowerShell');
  AddLex('d', 'D');
end;

procedure TfmExplorer.HandleClick(ADouble: boolean);
var
  P: TPoint;
  Node: TTreeNode;
  Data: TExplorerNodeData;
  Kind: TExplorerClickKind;
begin
  P:= Tree.ScreenToClient(Mouse.CursorPos);
  Node:= Tree.GetNodeAt(P.X, P.Y);
  if Assigned(Node) then
    if Assigned(Node.Data) then
    begin
      Data:= TExplorerNodeData(Node.Data);
      if Data.IsDir then
      begin
        if Node.Expanded then
        begin
          Node.Collapse(false);
          Kind:= eckFolderFold;
        end
        else
        begin
          Node.Expand(false);
          Kind:= eckFolderUnfold;
        end;
      end
      else
      begin
        if ADouble then
          Kind:= eckFileDblClick
        else
          Kind:= eckFileClick;
      end;
      if Assigned(FOnItemClick) then
        FOnItemClick(Data.Path, Kind);
    end;
end;

procedure TfmExplorer.TreeClick(Sender: TObject);
begin
  HandleClick(false);
end;

procedure TfmExplorer.TreeDblClick(Sender: TObject);
begin
  HandleClick(true);
end;

procedure TfmExplorer.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    TObject(Node.Data).Free;
end;

procedure TfmExplorer.TreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  Data: TExplorerNodeData;
begin
  AllowExpansion:= true;
  Data:= TExplorerNodeData(Node.Data);
  if Data=nil then exit;
  if Data.Expanded then exit;
  if not Data.IsDir then exit;

  Data.Expanded:= true;
  FillTreeForFolder(Data.Path, Node);
  //ShowMessage('fill: '+Data.Path);
end;

function TfmExplorer.PrettyDirName(const S: string): string;
begin
  if ExplorerOptions.ShowFolderBrackets then
    Result:= '['+S+']'
  else
    Result:= S;
end;

procedure TfmExplorer.SetFolder(const AValue: string);
var
  RootNode: TTreeNode;
  NIcon: integer;
begin
  FFolder:= AValue;

  Tree.Items.Clear;
  if FFolder='' then exit;
  if not DirectoryExists(FFolder) then exit;

  InitIconConfig;

  Tree.ShowRoot:= not ExplorerOptions.ShowRootNode;

  if ExplorerOptions.ShowRootNode then
  begin
    RootNode:= Tree.Items.Add(nil, PrettyDirName(ExtractFileName(FFolder)));
    if ExplorerOptions.ShowIcons then
      NIcon:= FIconIndexDir
    else
      NIcon:= -1;
    RootNode.ImageIndex:= NIcon;
    RootNode.SelectedIndex:= NIcon;
  end
  else
    RootNode:= nil;

  FillTreeForFolder(FFolder, RootNode);

  if Assigned(RootNode) then
    RootNode.Expand(false);
end;

procedure TfmExplorer.Refresh;
begin
  if FFolder<>'' then
    SetFolder(FFolder);
end;

function _CompareFilenames(L: TStringList; Index1, Index2: integer): integer;
var
  s1, s2, ext1, ext2: string;
  d1, d2: PtrInt;
  dot1, dot2: boolean;
begin
  //show dirs first
  d1:= PtrInt(L.Objects[Index1]);
  d2:= PtrInt(L.Objects[Index2]);
  if d1<>d2 then
    exit(d2-d1);

  s1:= L[Index1];
  s2:= L[Index2];

  if ExplorerOptions.ShowDotNamesFirst then
  begin
    dot1:= s1[1]='.';
    dot2:= s2[1]='.';
    if dot1<>dot2 then
      exit(ord(dot2)-ord(dot1));
  end;

  ext1:= ExtractFileExt(s1);
  ext2:= ExtractFileExt(s2);

  Result:= CompareText(ext1, ext2);
  if Result=0 then
    Result:= CompareText(s1, s2);
end;

procedure TfmExplorer.ReadFolder(const AFolder: string; AList: TStringList; out ACountHidden: integer);
const
  MaskAll = {$ifdef windows} '*.*' {$else} '*' {$endif};
var
  Rec: TSearchRec;
  bDir: boolean;
  S: string;
begin
  AList.Clear;
  ACountHidden:= 0;
  if FindFirst(AFolder+DirectorySeparator+MaskAll, faAnyFile, Rec)=0 then
  begin
    repeat
      S:= Rec.Name;
      if (S='.') or (S='..') then Continue;
      if (S[1]='.') then
      begin
        Inc(ACountHidden);
        if not ExplorerOptions.ShowDotNames then
          Continue;
      end;

      bDir:= (Rec.Attr and faDirectory)<>0;
      AList.AddObject(S, TObject(PtrInt(bDir)));
    until FindNext(Rec)<>0;
    FindClose(Rec);
  end;
end;

procedure TfmExplorer.FillTreeForFolder(const AFolder: string; ANode: TTreeNode);
var
  Node: TTreeNode;
  List: TStringList;
  bDir: boolean;
  NodeData: TExplorerNodeData;
  CountHidden, NIcon: integer;
  S: string;
  i: integer;
begin
  if Assigned(ANode) then
    ANode.DeleteChildren
  else
    Tree.Items.Clear;

  List:= TStringList.Create;
  try
    ReadFolder(AFolder, List, CountHidden);

    if List.Count=0 then
    begin
      if CountHidden=0 then
        S:= ExplorerOptions.TextEmpty
      else
        S:= Format(ExplorerOptions.TextEmptyWithHidden, [CountHidden]);
      Node:= Tree.Items.AddChild(ANode, S);
      exit;
    end;

    List.CustomSort(@_CompareFilenames);

    for i:= 0 to List.Count-1 do
    begin
      S:= List[i];
      bDir:= List.Objects[i]<>nil;

      NodeData:= TExplorerNodeData.Create;
      NodeData.Path:= AFolder+DirectorySeparator+S;
      NodeData.IsDir:= bDir;
      NodeData.Expanded:= false;

      if bDir then
        S:= PrettyDirName(S);

      Node:= Tree.Items.AddChildObject(ANode, S, NodeData);
      if ExplorerOptions.ShowIcons then
        NIcon:= GetImageIndex(NodeData.Path, NodeData.IsDir)
      else
        NIcon:= -1;
      Node.ImageIndex:= NIcon;
      Node.SelectedIndex:= NIcon;

      //add fictive child, to show expand arrow
      if bDir then
        Tree.Items.AddChild(Node, '?');
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmExplorer.InitIconConfig;
var
  fnConfig: string;
  fnDefault, fnDir, fnZip, fnPic, fnBin: string;
begin
  if not Assigned(FIconCfg) then
  begin
    fnConfig:= ExplorerOptions.IconDir+DirectorySeparator+'icons.json';
    if not FileExists(fnConfig) then exit;

    FIconCfg:= TJSONConfig.Create(Self);
    FIconCfg.Filename:= fnConfig;

    fnDefault:= FIconCfg.GetValue('_', '');
    fnDir:= FIconCfg.GetValue('_dir', '');
    fnZip:= FIconCfg.GetValue('_zip', '');
    fnPic:= FIconCfg.GetValue('_img', '');
    fnBin:= FIconCfg.GetValue('_bin', '');

    FIconIndexDefault:= GetImageIndexFromPng(fnDefault);
    FIconIndexDir:= GetImageIndexFromPng(fnDir);
    FIconIndexZip:= GetImageIndexFromPng(fnZip);
    FIconIndexPic:= GetImageIndexFromPng(fnPic);
    FIconIndexBin:= GetImageIndexFromPng(fnBin);

    InitUsualExtensions;
  end;
end;

procedure TfmExplorer.InitUsualExtensions;
  //
  procedure AddExt(const AExt: string; AIndex: integer); inline;
  begin
    ListExt.AddObject(AExt, TObject(PtrInt(AIndex)));
  end;
  //
begin
  AddExt('log', FIconIndexDefault);
  AddExt('txt', FIconIndexDefault);
  AddExt('csv', FIconIndexDefault);

  AddExt('zip', FIconIndexZip);
  AddExt('rar', FIconIndexZip);
  AddExt('tar', FIconIndexZip);
  AddExt('xz', FIconIndexZip);
  AddExt('gz', FIconIndexZip);
  AddExt('7z', FIconIndexZip);
  AddExt('deb', FIconIndexZip);
  AddExt('rpm', FIconIndexZip);
  AddExt('iso', FIconIndexZip);

  AddExt('png', FIconIndexPic);
  AddExt('gif', FIconIndexPic);
  AddExt('bmp', FIconIndexPic);
  AddExt('jpg', FIconIndexPic);
  AddExt('jpeg', FIconIndexPic);
  AddExt('svg', FIconIndexPic);
  AddExt('ico', FIconIndexPic);

  AddExt('exe', FIconIndexBin);
  AddExt('dll', FIconIndexBin);
  AddExt('dat', FIconIndexBin);
  AddExt('so', FIconIndexBin);
  AddExt('dylib', FIconIndexBin);
  AddExt('dbg', FIconIndexBin);
  AddExt('chm', FIconIndexBin);
  AddExt('pyc', FIconIndexBin);
  AddExt('o', FIconIndexBin);
  AddExt('a', FIconIndexBin);
  AddExt('mp3', FIconIndexBin);
  AddExt('mp4', FIconIndexBin);
  AddExt('m4a', FIconIndexBin);
  AddExt('mpg', FIconIndexBin);
  AddExt('mpeg', FIconIndexBin);
  AddExt('avi', FIconIndexBin);
  AddExt('mov', FIconIndexBin);
  AddExt('ogg', FIconIndexBin);
  AddExt('flac', FIconIndexBin);
  AddExt('webm', FIconIndexBin);
  AddExt('pdf', FIconIndexBin);
  AddExt('doc', FIconIndexBin);
  AddExt('docx', FIconIndexBin);
  AddExt('xls', FIconIndexBin);
  AddExt('xlsx', FIconIndexBin);
  AddExt('ppt', FIconIndexBin);
  AddExt('pptx', FIconIndexBin);
end;

function TfmExplorer.GetImageIndex(const AFileName: string; AIsDir: boolean): integer;
var
  SLexer: string;
  fnIcon: string;
  ext: string;
  N: integer;
begin
  if AIsDir then
    exit(FIconIndexDir);
  Result:= FIconIndexDefault;

  //show dot-names with default icon
  if AFileName[1]='.' then exit;

  ext:= LowerCase(ExtractFileExt(AFileName));
  if ext<>'' then
    Delete(ext, 1, 1);

  //read cache for extensions
  if ListExt.Find(ext, N) then
    exit(PtrInt(ListExt.Objects[N]));

  if ListExtToLexer.Find(ext, N) then
    SLexer:= TExplorerStringDataItem(ListExtToLexer.Objects[N]).Str
  else
  if Assigned(OnGetLexer) then
    SLexer:= OnGetLexer(AFileName);
  if SLexer='' then exit;

  //read cache for lexers
  if ListLexer.Find(SLexer, N) then
    exit(PtrInt(ListLexer.Objects[N]));

  fnIcon:= FIconCfg.GetValue(SLexer, '');
  if fnIcon='' then exit;
  Result:= GetImageIndexFromPng(fnIcon);

  //save to 2 caches
  ListExt.AddObject(ext, TObject(PtrInt(Result)));
  ListLexer.AddObject(SLexer, TObject(PtrInt(Result)));

  //ShowMessage('Load icon: '+ext);
end;

function TfmExplorer.GetImageIndexFromPng(const AFilename: string): integer;
var
  Img: TPortableNetworkGraphic;
  fn: string;
begin
  fn:= ExplorerOptions.IconDir+DirectorySeparator+AFilename;
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

initialization

  with ExplorerOptions do
  begin
    ShowRootNode:= true;
    ShowDotNames:= false;
    ShowDotNamesFirst:= true;
    ShowFolderBrackets:= true;
    ShowIcons:= true;
    TextEmpty:= '(Empty)';
    TextEmptyWithHidden:= '(Empty, %d hidden item(s))';
  end;

end.

