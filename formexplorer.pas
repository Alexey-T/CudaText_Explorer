unit formexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  ATListbox, Math, jsonConf;

type
  TExplorerOptions = record
    DirOfIcons: string;
    ShowRootNode: boolean;
    ShowDotNames: boolean;
    ShowDotNamesFirst: boolean;
    ShowFolderBrackets: boolean;
    ShowIcons: boolean;
    ShowIconsDirs: boolean;
    ShowNodeForEmpty: boolean;
    FoldDirsByClick: boolean;
    TextEmpty: string;
    TextEmptyWithHidden: string;
  end;

var
  ExplorerOptions: TExplorerOptions;

type
  TExplorerClickKind = (
    eckNone,
    eckFileClick,
    eckFileDblClick,
    eckFolderFold,
    eckFolderUnfold
    );

type
  TExplorerOnItemClick = procedure(const AFileName: string; AKind: TExplorerClickKind) of object;
  TExplorerOnGetLexer = function(const AFileName: string): string of object;
  TExplorerOnGetTabs = procedure(out ACount: integer; out ASelected: integer) of object;
  TExplorerOnGetTabProp = procedure(AIndex: integer; out ACaption, AFilename: string; out AModified: boolean) of object;
  TExplorerOnTabSelect = procedure(AIndex: integer) of object;

type
  { TfmExplorer }

  TfmExplorer = class(TForm)
    ListTabs: TATListbox;
    Images: TImageList;
    PanelTabsCap: TPanel;
    PanelTreeCap: TPanel;
    PanelTree: TPanel;
    PanelTabs: TPanel;
    Splitter1: TSplitter;
    Tree: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListTabsChangedSel(Sender: TObject);
    procedure ListTabsClickXMark(Sender: TObject);
    procedure ListTabsDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
    procedure TreeClick(Sender: TObject);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
  private
    FFolder: string;
    FOnGetLexer: TExplorerOnGetLexer;
    FOnItemClick: TExplorerOnItemClick;
    FOnGetTabs: TExplorerOnGetTabs;
    FOnGetTabProp: TExplorerOnGetTabProp;
    FOnTabSelect: TExplorerOnTabSelect;
    FOnTabClose: TExplorerOnTabSelect;
    FIconCfg: TJSONConfig;
    FIconIndexDefault: integer;
    FIconIndexDir: integer;
    FIconIndexPic: integer;
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
    procedure UpdateTabs(ASelChange: boolean);
    property Folder: string read FFolder write SetFolder;
    property OnGetLexer: TExplorerOnGetLexer read FOnGetLexer write FOnGetLexer;
    property OnItemClick: TExplorerOnItemClick read FOnItemClick write FOnItemClick;
    property OnGetTabs: TExplorerOnGetTabs read FOnGetTabs write FOnGetTabs;
    property OnGetTabProp: TExplorerOnGetTabProp read FOnGetTabProp write FOnGetTabProp;
    property OnTabSelect: TExplorerOnTabSelect read FOnTabSelect write FOnTabSelect;
    property OnTabClose: TExplorerOnTabSelect read FOnTabClose write FOnTabClose;
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

procedure TfmExplorer.ListTabsChangedSel(Sender: TObject);
begin
  if Assigned(FOnTabSelect) then
    FOnTabSelect(ListTabs.ItemIndex);
end;

procedure TfmExplorer.ListTabsClickXMark(Sender: TObject);
begin
  if Assigned(FOnTabClose) then
    FOnTabClose(ListTabs.ItemIndex);
end;

procedure TfmExplorer.ListTabsDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  list: TATListbox;
  NIcon: integer;
  fn, S: string;
  NIconSize, NIndent, NIndent2: integer;
  N: integer;
begin
  list:= ListTabs;
  NIcon:= -1;
  NIconSize:= Images.Width;
  NIndent:= 12;
  NIndent2:= 4;

  if AIndex=list.ItemIndex then
    C.Brush.Color:= clLtGray
  else
  if list.HotTrack and (AIndex=list.HotTrackIndex) then
    C.Brush.Color:= clMoneyGreen
  else
    C.Brush.Color:= clWhite;
  C.FillRect(ARect);

  S:= List.Items[AIndex];
  fn:= '';
  N:= Pos(' (', S);
  if N>0 then
    fn:= Copy(S, 1, N-1);

  if fn<>'' then
    NIcon:= GetImageIndex(fn, false);

  if NIcon>=0 then
    Images.Draw(C,
      ARect.Left+NIndent,
      (ARect.Top+ARect.Bottom-NIconSize) div 2,
      NIcon
      );

  C.Font.Color:= clBlack;
  C.TextOut(
    ARect.Left+NIndent+NIconSize+NIndent2,
    (ARect.Top+ARect.Bottom-C.TextHeight(S)) div 2,
    S);
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
      Kind:= eckNone;
      Data:= TExplorerNodeData(Node.Data);
      if Data.IsDir then
      begin
        if ExplorerOptions.FoldDirsByClick then
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
          if Assigned(FOnItemClick) then
            FOnItemClick(Data.Path, Kind);
        end;
      end
      else
      begin
        if ADouble then
          Kind:= eckFileDblClick
        else
          Kind:= eckFileClick;
        if Assigned(FOnItemClick) then
          FOnItemClick(Data.Path, Kind);
      end;
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

procedure TfmExplorer.UpdateTabs(ASelChange: boolean);
var
  NCount, NSel, i: integer;
  SCaption, SFilename: string;
  bModified: boolean;
  Data: TATListboxItemProp;
begin
  OnGetTabs(NCount, NSel);

  if ASelChange then
  begin
    if (NSel>=0) and (NSel<ListTabs.Items.Count) then
    begin
      ListTabs.ItemIndex:= NSel;
      ListTabs.Invalidate;
    end;
    exit;
  end;

  ListTabs.Items.BeginUpdate;
  try
    ListTabs.Items.Clear;
    for i:= 0 to NCount-1 do
    begin
      OnGetTabProp(i, SCaption, SFilename, bModified);
      Data:= TATListboxItemProp.Create(0, bModified, '');
      ListTabs.Items.AddObject(SCaption, Data);
    end;
    ListTabs.ItemIndex:= NSel;
    ListTabs.ItemTop:= Max(0, Min(ListTabs.ItemTop, ListTabs.ItemCount-ListTabs.VisibleItems));
  finally
    ListTabs.Items.EndUpdate;
    ListTabs.Invalidate;
  end;
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
      if ExplorerOptions.ShowNodeForEmpty then
      begin
        if CountHidden=0 then
          S:= ExplorerOptions.TextEmpty
        else
          S:= Format(ExplorerOptions.TextEmptyWithHidden, [CountHidden]);
        Node:= Tree.Items.AddChild(ANode, S);
      end;
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
begin
  if not Assigned(FIconCfg) then
  begin
    fnConfig:= ExplorerOptions.DirOfIcons+DirectorySeparator+'icons.json';
    if not FileExists(fnConfig) then exit;

    FIconCfg:= TJSONConfig.Create(Self);
    FIconCfg.Filename:= fnConfig;

    FIconIndexDefault:= GetImageIndexFromPng(FIconCfg.GetValue('_', ''));

    if ExplorerOptions.ShowIconsDirs then
      FIconIndexDir:= GetImageIndexFromPng(FIconCfg.GetValue('_dir', ''))
    else
      FIconIndexDir:= -1;

    FIconIndexPic:= GetImageIndexFromPng(FIconCfg.GetValue('_img', ''));

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
  AddExt('txt', FIconIndexDefault);
  AddExt('csv', FIconIndexDefault);

  AddExt('png', FIconIndexPic);
  AddExt('gif', FIconIndexPic);
  AddExt('bmp', FIconIndexPic);
  AddExt('jpg', FIconIndexPic);
  AddExt('jpeg', FIconIndexPic);
  AddExt('ico', FIconIndexPic);
  AddExt('ai', FIconIndexPic);
  AddExt('tif', FIconIndexPic);
  AddExt('tiff', FIconIndexPic);
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
  fn:= ExplorerOptions.DirOfIcons+DirectorySeparator+AFilename;
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
    ShowIconsDirs:= true;
    ShowNodeForEmpty:= false;
    FoldDirsByClick:= true;
    TextEmpty:= '(Empty)';
    TextEmptyWithHidden:= '(Empty, %d hidden item(s))';
  end;

end.

