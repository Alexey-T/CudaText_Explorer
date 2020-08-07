unit formexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  jsonConf;

type
  TExplorerOptions = record
    ShowDotNames: boolean;
    ShowFolderBrackets: boolean;
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
    FIconDir: string;
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
    procedure HandleClick(ADouble: boolean);
    procedure InitCommonLexers;
    procedure InitUsualExtensions;
    procedure InitIconConfig;
    function GetImageIndex(const AFileName: string; AIsDir: boolean): integer;
    function GetImageIndexFromPng(const AFilename: string): integer;
    function PrettyDirName(const S: string): string;
    procedure FillTreeForFolder(const AFolder: string; ANode: TTreeNode);
    procedure SetFolder(const AValue: string);
  public
    property Folder: string read FFolder write SetFolder;
    property IconDir: string read FIconDir write FIconDir;
    property OnGetLexer: TExplorerOnGetLexer read FOnGetLexer write FOnGetLexer;
    property OnItemClick: TExplorerOnItemClick read FOnItemClick write FOnItemClick;
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
begin
  Tree.ShowRoot:= false;
  Tree.ReadOnly:= true;
  Tree.RowSelect:= true;
  //Tree.HotTrack:= true;

  ListExt:= TStringList.Create;
  ListExt.Sorted:= true;

  ListLexer:= TStringList.Create;
  ListLexer.Sorted:= true;

  ListExtToLexer:= TStringList.Create;
  ListExtToLexer.Sorted:= true;
  ListExtToLexer.OwnsObjects:= true;

  InitCommonLexers;
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

procedure TfmExplorer.FormDestroy(Sender: TObject);
begin
  Tree.Items.Clear;
  FreeAndNil(ListExtToLexer);
  FreeAndNil(ListLexer);
  FreeAndNil(ListExt);
  if Assigned(FIconCfg) then
    FreeAndNil(FIconCfg);
end;

procedure TfmExplorer.HandleClick(ADouble: boolean);
var
  P: TPoint;
  Node: TTreeNode;
  Data: TExplorerTreeData;
  Kind: TExplorerClickKind;
begin
  P:= Tree.ScreenToClient(Mouse.CursorPos);
  Node:= Tree.GetNodeAt(P.X, P.Y);
  if Assigned(Node) then
    if Assigned(Node.Data) then
    begin
      Data:= TExplorerTreeData(Node.Data);
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
  if ExplorerOptions.ShowFolderBrackets then
    Result:= '['+S+']'
  else
    Result:= S;
end;

procedure TfmExplorer.SetFolder(const AValue: string);
begin
  if FFolder=AValue then Exit;
  FFolder:= AValue;

  Tree.Items.Clear;

  if (FFolder='') or not DirectoryExists(FFolder) then
    exit;

  FillTreeForFolder(FFolder, nil);
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
  if Assigned(ANode) then
    ANode.DeleteChildren
  else
    Tree.Items.Clear;

  List:= TStringList.Create;
  try
    if FindFirst(AFolder+DirectorySeparator+StrAllFiles, faAnyFile, Rec)=0 then
      repeat
        S:= Rec.Name;
        if (S='.') or (S='..') then Continue;
        if (S[1]='.') then
          if not ExplorerOptions.ShowDotNames then Continue;

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

procedure TfmExplorer.InitIconConfig;
var
  fnConfig: string;
  fnDefault, fnDir, fnZip, fnPic, fnBin: string;
begin
  if not Assigned(FIconCfg) then
  begin
    fnConfig:= FIconDir+DirectorySeparator+'icons.json';
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
  InitIconConfig;

  if AIsDir then
    exit(FIconIndexDir);
  Result:= FIconIndexDefault;

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

initialization

  with ExplorerOptions do
  begin
    ShowDotNames:= false;
    ShowFolderBrackets:= true;
  end;

end.

