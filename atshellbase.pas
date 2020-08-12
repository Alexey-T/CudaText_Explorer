{*****************************************************
ATShellBase
Copyright (c) 2020 Alexey Torgashin (UVviewsoft.com)
License: MPL 2.0 or LGPL
******************************************************}

unit ATShellBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
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
    ShowTextForEmpty: boolean;
    FoldDirsByClick: boolean;
    TextEmpty: string;
    TextEmptyWithHidden: string;
  end;

var
  ATShellOptions: TATShellOptions;

type
  TATShellIconsOnDetect = function(const AFileName: string): string of object;

type
  { TATShellIcons }

  TATShellIcons = class
  private const
    cPicSection = 'p';
  private
    FImages: TImageList;
    ListExt: TStringList;
    ListLexer: TStringList;
    ListExtToLexer: TStringList;
    FIconCfg: TMemIniFile;
    FIconIndexDefault: integer;
    FIconIndexDir: integer;
    FIconIndexPic: integer;
    FOnDetect: TATShellIconsOnDetect;
    function DetectLexer(const fn, ext: string): string;
    function GetImageIndexFromPng(const AFilename: string): integer;
    procedure InitCommonLexers;
    procedure InitUsualExtensions;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitConfig;
    property Images: TImageList read FImages;
    property ImageIndexDefault: integer read FIconIndexDefault;
    property ImageIndexDir: integer read FIconIndexDir;
    function ImageIndex(const AFileName: string): integer;
    property OnDetect: TATShellIconsOnDetect read FOnDetect write FOnDetect;
  end;

var
  ATShellIcons: TATShellIcons = nil;

type
  EShellConfigError = class(Exception);

implementation

type
  TATShellStringClass = class
    Str: string;
  end;

{ TATShellIcons }

constructor TATShellIcons.Create;
begin
  FImages:= TImageList.Create(nil);
  FImages.AllocBy:= 10;

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
  FreeAndNil(FImages);
  inherited;
end;

procedure TATShellIcons.InitCommonLexers;
  //
  procedure AddLex(const AExt, ALexer: string); inline;
  var
    D: TATShellStringClass;
  begin
    D:= TATShellStringClass.Create;
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
  AddLex('f', 'Fortran');
  AddLex('for', 'Fortran');
  AddLex('f2k', 'Fortran');
  AddLex('f90', 'Fortran');
  AddLex('f95', 'Fortran');
  AddLex('feature', 'Gherkin');
  AddLex('gql', 'GraphQL');
  AddLex('graphql', 'GraphQL');
  AddLex('gradle', 'Groovy');
  AddLex('groovy', 'Groovy');
  AddLex('tcl', 'Tcl');
  AddLex('tk', 'Tcl');
  AddLex('tm', 'Tcl');
  AddLex('po', 'PO');
  AddLex('csproj', 'csproj');
  AddLex('fsproj', 'fsproj');
  AddLex('vbproj', 'vbproj');
  AddLex('vcxproj', 'vcxproj');

  AddLex('ai', 'AI');
  AddLex('pdf', 'PDF');
  AddLex('doc', 'Word');
  AddLex('docx', 'Word');
  AddLex('rtf', 'Word');
  AddLex('odt', 'Word');
  AddLex('wpd', 'Word');
  AddLex('wps', 'Word');
  AddLex('xls', 'Excel');
  AddLex('xlsx', 'Excel');
  AddLex('xlr', 'Excel');
  AddLex('ods', 'Excel');
  AddLex('ppt', 'PowerPoint');
  AddLex('pps', 'PowerPoint');
  AddLex('pptx', 'PowerPoint');

  AddLex('zip', '_zip');
  AddLex('zipx', '_zip');
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
  AddLex('vcd', '_zip');
  AddLex('cab', '_zip');
  AddLex('cbr', '_zip');
  AddLex('sitx', '_zip');

  AddLex('exe', '_bin');
  AddLex('com', '_bin');
  AddLex('dll', '_bin');
  AddLex('so', '_bin');
  AddLex('ocx', '_bin');
  AddLex('cpl', '_bin');
  AddLex('drv', '_bin');
  AddLex('lnk', '_bin');
  AddLex('sys', '_bin');
  AddLex('gadget', '_bin');
  AddLex('dbg', '_bin');
  AddLex('chm', '_bin');
  AddLex('pyc', '_bin');
  AddLex('o', '_bin');
  AddLex('a', '_bin');
  AddLex('db', '_bin');
  AddLex('dbf', '_bin');
  AddLex('mdb', '_bin');
  AddLex('pdb', '_bin');
  AddLex('accdb', '_bin');
  AddLex('apk', '_bin');
  AddLex('dat', '_bin');
  AddLex('bin', '_bin');
  AddLex('msi', '_bin');
  AddLex('cue', '_bin');
  AddLex('mdf', '_bin');
  AddLex('dmg', '_bin');
  AddLex('toast', '_bin');

  AddLex('wav', '_audio');
  AddLex('mp3', '_audio');
  AddLex('mpa', '_audio');
  AddLex('m4a', '_audio');
  AddLex('m3u', '_audio');
  AddLex('ogg', '_audio');
  AddLex('flac', '_audio');
  AddLex('wma', '_audio');
  AddLex('wpl', '_audio');
  AddLex('aif', '_audio');
  AddLex('iff', '_audio');
  AddLex('cda', '_audio');
  AddLex('mid', '_audio');
  AddLex('midi', '_audio');
  AddLex('mka', '_audio');
  AddLex('s3m', '_audio');
  AddLex('xm', '_audio');
  AddLex('it', '_audio');
  AddLex('ra', '_audio');

  AddLex('mp4', '_video');
  AddLex('mpg', '_video');
  AddLex('mpeg', '_video');
  AddLex('m4v', '_video');
  AddLex('avi', '_video');
  AddLex('mov', '_video');
  AddLex('webm', '_video');
  AddLex('wmv', '_video');
  AddLex('mkv', '_video');
  AddLex('flv', '_video');
  AddLex('ogv', '_video');
  AddLex('3gp', '_video');
  AddLex('3g2', '_video');
  AddLex('asf', '_video');
  AddLex('rm', '_video');
  AddLex('rmvb', '_video');
  AddLex('rv', '_video');
  AddLex('rp', '_video');
  AddLex('viv', '_video');
  AddLex('swf', '_video');
  AddLex('vob', '_video');

  AddLex('fnt', '_font');
  AddLex('fon', '_font');
  AddLex('ttf', '_font');
  AddLex('otf', '_font');
end;

procedure TATShellIcons.InitConfig;
var
  fnConfig: string;
begin
  if not Assigned(FIconCfg) then
  begin
    if not DirectoryExists(ATShellOptions.DirOfIcons) then
      raise EShellConfigError.Create('Icons folder not found: '+ATShellOptions.DirOfIcons);

    fnConfig:= ATShellOptions.DirOfIcons+DirectorySeparator+'icons.ini';
    if not FileExists(fnConfig) then
      raise EShellConfigError.Create('Icons config not found: '+fnConfig);

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

  AddExt('bmp', FIconIndexPic);
  AddExt('png', FIconIndexPic);
  AddExt('gif', FIconIndexPic);
  AddExt('jpg', FIconIndexPic);
  AddExt('jpeg', FIconIndexPic);
  AddExt('ico', FIconIndexPic);
  AddExt('cur', FIconIndexPic);
  AddExt('icns', FIconIndexPic);
  AddExt('tif', FIconIndexPic);
  AddExt('tiff', FIconIndexPic);
  AddExt('dds', FIconIndexPic);
  AddExt('heic', FIconIndexPic);
  AddExt('pspimage', FIconIndexPic);
  AddExt('tga', FIconIndexPic);
  AddExt('thm', FIconIndexPic);
  AddExt('yuv', FIconIndexPic);
  AddExt('eps', FIconIndexPic);

  AddExt('3dm', FIconIndexPic);
  AddExt('3ds', FIconIndexPic);
  AddExt('max', FIconIndexPic);
end;

function TATShellIcons.DetectLexer(const fn, ext: string): string;
var
  N: integer;
begin
  Result:= '';

  if fn='Dockerfile' then
    exit(fn);
  if ChangeFileExt(fn, '')='makefile' then
    exit('Makefile');

  if ListExtToLexer.Find(ext, N) then
    exit(TATShellStringClass(ListExtToLexer.Objects[N]).Str);

  if Assigned(FOnDetect) then
    Result:= FOnDetect(fn);
end;

function TATShellIcons.ImageIndex(const AFileName: string): integer;
var
  SLexer: string;
  fnIcon: string;
  fn, ext: string;
  N: integer;
begin
  if not ATShellOptions.ShowIcons then
    exit(-1);
  if FIconCfg=nil then
    exit(-1);
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
    ShowTextForEmpty:= false;
    FoldDirsByClick:= true;
    TextEmpty:= '(Empty)';
    TextEmptyWithHidden:= '(Empty, %d hidden item(s))';
  end;

  ATShellIcons:= TATShellIcons.Create;

finalization

  FreeAndNil(ATShellIcons);

end.
