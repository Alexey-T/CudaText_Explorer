(*****************************************************
ATShellTreeview component
Copyright (c) 2020 Alexey Torgashin (uvviewsoft.com)
License: MPL 2.0 or LGPL
******************************************************)

unit ATShellTreeview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ComCtrls,
  IniFiles;

type
  TATShellOptions = record
    DirOfIcons: string;
    ShowDotNames: boolean;
    ShowDotNamesFirst: boolean;
    ShowFolderBrackets: boolean;
    ShowIcons: boolean;
    ShowIconsDirs: boolean;
    ShowRootNode: boolean;
    FoldDirsByClick: boolean;
    TextEmpty: string;
    TextEmptyWithHidden: string;
  end;

var
  ATShellOptions: TATShellOptions;

type
  TATShellTreeviewClick = (
    astcNone,
    astcFileClick,
    astcFileDblClick,
    astcFolderFold,
    astcFolderUnfold
    );

type
  TATShellTreeviewItemClick = procedure(const AFileName: string; AKind: TATShellTreeviewClick) of object;
  TATShellOnGetLexer = function(const AFileName: string): string of object;

type
  { TATShellTreeview }

  TATShellTreeview = class(TTreeView)
  private
    FFolder: string;
    FOnShellItemClick: TATShellTreeviewItemClick;
    procedure SetFolder(const AValue: string);
    procedure HandleClick(ADouble: boolean);
    procedure FillTreeForFolder(const AFolder: string; ANode: TTreeNode);
    procedure ReadFolder(const AFolder: string; AList: TStringList; out ACountHidden: integer);
    procedure TreeClick(Sender: TObject);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
  public
    property Folder: string read FFolder write SetFolder;
    constructor Create(AOwner: TComponent); override;
  published
    property OnShellItemClick: TATShellTreeviewItemClick read FOnShellItemClick write FOnShellItemClick;
  end;

type
  { TATShellIcons }

  TATShellIcons = class
  private const
    cPicSection = 'p';
  private
    ListExt: TStringList;
    ListLexer: TStringList;
    ListExtToLexer: TStringList;
    FIconCfg: TMemIniFile;
    FIconIndexDefault: integer;
    FIconIndexDir: integer;
    FIconIndexPic: integer;
    FOnGetLexer: TATShellOnGetLexer;
    function DetectLexer(const fn, ext: string): string;
    function GetImageIndexFromPng(const AFilename: string): integer;
    procedure InitCommonLexers;
    procedure InitUsualExtensions;
  public
    Images: TImageList;
    constructor Create;
    destructor Destroy; override;
    procedure InitIconConfig;
    function GetImageIndex(const AFileName: string; AIsDir: boolean): integer;
    property IconIndexDefault: integer read FIconIndexDefault;
    property IconIndexDir: integer read FIconIndexDir;
    property OnGetLexer: TATShellOnGetLexer read FOnGetLexer write FOnGetLexer;
  end;

var
  ATShellIcons: TATShellIcons = nil;

implementation

type
  TExplorerNodeData = class
  public
    Path: string;
    IsDir: boolean;
    Expanded: boolean;
  end;

type
  TExplorerStringDataItem = class
    Str: string;
  end;

function PrettyDirName(const S: string): string;
begin
  if ATShellOptions.ShowFolderBrackets then
    Result:= '['+S+']'
  else
    Result:= S;
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

  if ATShellOptions.ShowDotNamesFirst then
  begin
    dot1:= s1[1]='.';
    dot2:= s2[1]='.';

    if dot1<>dot2 then
      exit(ord(dot2)-ord(dot1));

    //compare dot-names w/o extensions (like VSCode)
    if dot1 then
      exit(CompareText(s1, s2));
  end;

  ext1:= ExtractFileExt(s1);
  ext2:= ExtractFileExt(s2);

  Result:= CompareText(ext1, ext2);
  if Result=0 then
    Result:= CompareText(s1, s2);
end;


procedure TATShellTreeview.HandleClick(ADouble: boolean);
var
  P: TPoint;
  Node: TTreeNode;
  Data: TExplorerNodeData;
  Kind: TATShellTreeviewClick;
begin
  P:= ScreenToClient(Mouse.CursorPos);
  Node:= GetNodeAt(P.X, P.Y);
  if Assigned(Node) then
    if Assigned(Node.Data) then
    begin
      Kind:= astcNone;
      Data:= TExplorerNodeData(Node.Data);
      if Data.IsDir then
      begin
        if ATShellOptions.FoldDirsByClick then
        begin
          if Node.Expanded then
          begin
            Node.Collapse(false);
            Kind:= astcFolderFold;
          end
          else
          begin
            Node.Expand(false);
            Kind:= astcFolderUnfold;
          end;
          if Assigned(FOnShellItemClick) then
            FOnShellItemClick(Data.Path, Kind);
        end;
      end
      else
      begin
        if ADouble then
          Kind:= astcFileDblClick
        else
          Kind:= astcFileClick;
        if Assigned(FOnShellItemClick) then
          FOnShellItemClick(Data.Path, Kind);
      end;
    end;
end;

procedure TATShellTreeview.TreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
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

procedure TATShellTreeview.ReadFolder(const AFolder: string;
  AList: TStringList; out ACountHidden: integer);
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
        if not ATShellOptions.ShowDotNames then
          Continue;
      end;

      bDir:= (Rec.Attr and faDirectory)<>0;
      AList.AddObject(S, TObject(PtrInt(bDir)));
    until FindNext(Rec)<>0;
    FindClose(Rec);
  end;
end;

procedure TATShellTreeview.TreeClick(Sender: TObject);
begin
  HandleClick(false);
end;

procedure TATShellTreeview.TreeDblClick(Sender: TObject);
begin
  HandleClick(true);
end;

procedure TATShellTreeview.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    TObject(Node.Data).Free;
end;

constructor TATShellTreeview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ShowRoot:= false;
  ReadOnly:= true;
  RowSelect:= true;

  OnClick:= @TreeClick;
  OnDblClick:= @TreeDblClick;
  OnDeletion:= @TreeDeletion;
  OnExpanding:= @TreeExpanding;

  Images:= ATShellIcons.Images;
end;

procedure TATShellTreeview.SetFolder(const AValue: string);
var
  RootNode: TTreeNode;
  NIcon: integer;
begin
  FFolder:= AValue;

  Items.Clear;
  if FFolder='' then exit;
  if not DirectoryExists(FFolder) then exit;

  ATShellIcons.InitIconConfig;

  ShowRoot:= not ATShellOptions.ShowRootNode;

  if ATShellOptions.ShowRootNode then
  begin
    RootNode:= Items.Add(nil, PrettyDirName(ExtractFileName(FFolder)));
    if ATShellOptions.ShowIcons then
      NIcon:= ATShellIcons.IconIndexDir
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


procedure TATShellTreeview.FillTreeForFolder(const AFolder: string; ANode: TTreeNode);
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
    Items.Clear;

  List:= TStringList.Create;
  try
    ReadFolder(AFolder, List, CountHidden);

    if List.Count=0 then
    begin
      {
      if ATShellOptions.ShowNodeForEmpty then
      begin
        if CountHidden=0 then
          S:= ATShellOptions.TextEmpty
        else
          S:= Format(ATShellOptions.TextEmptyWithHidden, [CountHidden]);
        Node:= Tree.Items.AddChild(ANode, S);
      end;
      }
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

      Node:= Items.AddChildObject(ANode, S, NodeData);
      NIcon:= ATShellIcons.GetImageIndex(NodeData.Path, NodeData.IsDir);
      Node.ImageIndex:= NIcon;
      Node.SelectedIndex:= NIcon;

      //add fictive child, to show expand arrow
      if bDir then
        Items.AddChild(Node, '?');
    end;
  finally
    FreeAndNil(List);
  end;
end;


{ TATShellIcons }

procedure TATShellIcons.InitCommonLexers;
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
  AddLex('h', 'C header');
  AddLex('cpp', 'C++');
  AddLex('hpp', 'C++ header');
  AddLex('htm', 'HTML');
  AddLex('html', 'HTML');
  AddLex('css', 'CSS');
  AddLex('js', 'JavaScript');
  AddLex('json', 'JSON');
  AddLex('java', 'Java');
  AddLex('sh', 'Bash script');
  AddLex('bat', 'Batch files');
  AddLex('cmd', 'Batch files');
  AddLex('log', 'Log files');
  AddLex('md', 'Markdown');
  AddLex('xml', 'XML');
  AddLex('xsl', 'XSLT');
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
  AddLex('elm', 'Elm');
  AddLex('ex', 'Elixir');
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
  AddLex('kt', 'Kotlin');
  AddLex('styl', 'Stylus');
  AddLex('slim', 'Slim');
  AddLex('swift', 'Swift');
  AddLex('toml', 'TOML');
  AddLex('twig', 'Twig');
  AddLex('svg', 'SVG');
  AddLex('psd', 'Photoshop');
  AddLex('d', 'D');
  AddLex('po', 'PO');
  AddLex('csproj', 'csproj');
  AddLex('fsproj', 'fsproj');
  AddLex('vbproj', 'vbproj');
  AddLex('vcxproj', 'vcxproj');

  AddLex('ai', 'AI');
  AddLex('pdf', 'PDF');
  AddLex('doc', 'Word');
  AddLex('docx', 'Word');
  AddLex('xls', 'Excel');
  AddLex('xlsx', 'Excel');
  AddLex('ppt', 'PowerPoint');
  AddLex('pptx', 'PowerPoint');

  AddLex('zip', '_zip');
  AddLex('rar', '_zip');
  AddLex('tar', '_zip');
  AddLex('xz', '_zip');
  AddLex('gz', '_zip');
  AddLex('z', '_zip');
  AddLex('7z', '_zip');
  AddLex('deb', '_zip');
  AddLex('pkg', '_zip');
  AddLex('rpm', '_zip');
  AddLex('iso', '_zip');
  AddLex('jar', '_zip');
  AddLex('arj', '_zip');
  AddLex('dmg', '_zip');
  AddLex('vcd', '_zip');

  AddLex('exe', '_bin');
  AddLex('dll', '_bin');
  AddLex('so', '_bin');
  AddLex('ocx', '_bin');
  AddLex('dbg', '_bin');
  AddLex('chm', '_bin');
  AddLex('pyc', '_bin');
  AddLex('o', '_bin');
  AddLex('a', '_bin');
  AddLex('db', '_bin');
  AddLex('dbf', '_bin');
  AddLex('mdb', '_bin');
  AddLex('apk', '_bin');
  AddLex('dat', '_bin');
  AddLex('bin', '_bin');
  AddLex('msi', '_bin');

  AddLex('wav', '_audio');
  AddLex('mp3', '_audio');
  AddLex('mpa', '_audio');
  AddLex('ogg', '_audio');
  AddLex('flac', '_audio');
  AddLex('wma', '_audio');
  AddLex('wpl', '_audio');
  AddLex('aif', '_audio');
  AddLex('cda', '_audio');
  AddLex('mid', '_audio');
  AddLex('midi', '_audio');
  AddLex('mka', '_audio');
  AddLex('s3m', '_audio');
  AddLex('xm', '_audio');
  AddLex('it', '_audio');

  AddLex('mp4', '_video');
  AddLex('m4a', '_video');
  AddLex('mpg', '_video');
  AddLex('mpeg', '_video');
  AddLex('avi', '_video');
  AddLex('mov', '_video');
  AddLex('webm', '_video');
  AddLex('mkv', '_video');

  AddLex('fnt', '_font');
  AddLex('fon', '_font');
  AddLex('ttf', '_font');
  AddLex('otf', '_font');
end;

constructor TATShellIcons.Create;
begin
  Images:= TImageList.Create(nil);
  Images.AllocBy:= 10;

  ListExt:= TStringList.Create;
  ListExt.Sorted:= true;

  ListLexer:= TStringList.Create;
  ListLexer.Sorted:= true;

  ListExtToLexer:= TStringList.Create;
  ListExtToLexer.Sorted:= true;
  ListExtToLexer.OwnsObjects:= true;

  InitCommonLexers;
end;

destructor TATShellIcons.Destroy;
begin
  FreeAndNil(ListExtToLexer);
  FreeAndNil(ListLexer);
  FreeAndNil(ListExt);
  if Assigned(FIconCfg) then
    FreeAndNil(FIconCfg);
  FreeAndNil(Images);
  inherited;
end;

procedure TATShellIcons.InitIconConfig;
var
  fnConfig: string;
begin
  if not Assigned(FIconCfg) then
  begin
    fnConfig:= ATShellOptions.DirOfIcons+DirectorySeparator+'icons.ini';
    if not FileExists(fnConfig) then exit;

    FIconCfg:= TMemIniFile.Create(fnConfig);

    FIconIndexDefault:= GetImageIndexFromPng(FIconCfg.ReadString(cPicSection, '_', ''));

    if ATShellOptions.ShowIconsDirs then
      FIconIndexDir:= GetImageIndexFromPng(FIconCfg.ReadString(cPicSection, '_dir', ''))
    else
      FIconIndexDir:= -1;

    FIconIndexPic:= GetImageIndexFromPng(FIconCfg.ReadString(cPicSection, '_img', ''));

    InitUsualExtensions;
  end;
end;

procedure TATShellIcons.InitUsualExtensions;
  //
  procedure AddExt(const AExt: string; AIndex: integer); inline;
  begin
    ListExt.AddObject(AExt, TObject(PtrInt(AIndex)));
  end;
  //
begin
  AddExt('txt', FIconIndexDefault);
  AddExt('csv', FIconIndexDefault);

  AddExt('png', FIconIndexPic);
  AddExt('gif', FIconIndexPic);
  AddExt('bmp', FIconIndexPic);
  AddExt('jpg', FIconIndexPic);
  AddExt('jpeg', FIconIndexPic);
  AddExt('ico', FIconIndexPic);
  AddExt('tif', FIconIndexPic);
  AddExt('tiff', FIconIndexPic);
end;

function TATShellIcons.DetectLexer(const fn, ext: string): string;
var
  N: integer;
begin
  Result:= '';

  if fn='Dockerfile' then
    exit(fn);

  if ListExtToLexer.Find(ext, N) then
    exit(TExplorerStringDataItem(ListExtToLexer.Objects[N]).Str);

  if Assigned(FOnGetLexer) then
    Result:= FOnGetLexer(fn);
end;

function TATShellIcons.GetImageIndex(const AFileName: string; AIsDir: boolean): integer;
var
  SLexer: string;
  fnIcon: string;
  fn, ext: string;
  N: integer;
begin
  if not ATShellOptions.ShowIcons then
    exit(-1);
  if AIsDir then
    exit(FIconIndexDir);
  Result:= FIconIndexDefault;

  fn:= ExtractFileName(AFileName);
  //show dot-names with default icon
  if fn[1]='.' then exit;

  ext:= LowerCase(ExtractFileExt(fn));
  if ext<>'' then
    Delete(ext, 1, 1);

  //read cache for extensions
  if ListExt.Find(ext, N) then
    exit(PtrInt(ListExt.Objects[N]));

  SLexer:= DetectLexer(fn, ext);
  if SLexer='' then exit;

  //read cache for lexers
  if ListLexer.Find(SLexer, N) then
    exit(PtrInt(ListLexer.Objects[N]));

  fnIcon:= FIconCfg.ReadString(cPicSection, SLexer, '');
  if fnIcon='' then exit;
  Result:= GetImageIndexFromPng(fnIcon);

  //save to 2 caches
  if ext<>'' then
    ListExt.AddObject(ext, TObject(PtrInt(Result)));
  if SLexer<>'' then
    ListLexer.AddObject(SLexer, TObject(PtrInt(Result)));

  //ShowMessage('Load icon: '+ext);
end;

function TATShellIcons.GetImageIndexFromPng(const AFilename: string): integer;
var
  Img: TPortableNetworkGraphic;
  fn: string;
begin
  fn:= ATShellOptions.DirOfIcons+DirectorySeparator+AFilename;
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

  with ATShellOptions do
  begin
    DirOfIcons:= '';
    ShowDotNames:= false;
    ShowDotNamesFirst:= true;
    ShowFolderBrackets:= true;
    ShowIcons:= true;
    ShowIconsDirs:= true;
    ShowRootNode:= true;
    FoldDirsByClick:= true;
    TextEmpty:= '(Empty)';
    TextEmptyWithHidden:= '(Empty, %d hidden item(s))';
  end;

  ATShellIcons:= TATShellIcons.Create;

finalization

  FreeAndNil(ATShellIcons);

end.
