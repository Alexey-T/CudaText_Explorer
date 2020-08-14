unit formexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Math,
  ATListbox,
  ATButtons,
  ATFlatThemes,
  ATShellBase,
  ATShellTreeview;

type
  TExplorerOptions = record
    ShowCaptionButtonsX: boolean;
    ShowDirInTreeCaption: boolean;
    TabsIndent1: integer;
    TabsIndent2: integer;
    TabsIndentV: integer;
    TabsHeightPercents: integer;
    TabsHeightMaxPercents: integer;
    TabsAutoSize: boolean;
    TabsBelow: boolean;
    CaptionTabsText: string;
    CaptionTreeText: string;
    CaptionPanelHeight: integer;
    CaptionPanelColorBg: TColor;
    CaptionPanelColorFont: TColor;
    CaptionButtonXSize: integer;
    CaptionButtonX: array[boolean] of string;
    CaptionFontName: string;
    CaptionFontSize: integer;
  end;

var
  ExplorerOptions: TExplorerOptions;

type
  TExplorerOnGetTabs = procedure(out ACount: integer; out ASelected: integer) of object;
  TExplorerOnGetTabProp = procedure(AIndex: integer; out ACaption, AFilename: string; out AModified: boolean) of object;
  TExplorerOnTabSelect = procedure(AIndex: integer) of object;

type
  { TfmExplorer }

  TfmExplorer = class(TForm)
    BtnTreeX: TATButton;
    BtnTabsX: TATButton;
    ListTabs: TATListbox;
    PanelTabsCap: TPanel;
    PanelTreeCap: TPanel;
    PanelTree: TPanel;
    PanelTabs: TPanel;
    Splitter1: TSplitter;
    procedure BtnTreeXClick(Sender: TObject);
    procedure BtnTabsXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListTabsChangedSel(Sender: TObject);
    procedure ListTabsClickXMark(Sender: TObject);
    procedure ListTabsDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
    procedure ListTabsResize(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
  private
    Tree: TATShellTreeview;
    FOnGetTabs: TExplorerOnGetTabs;
    FOnGetTabProp: TExplorerOnGetTabProp;
    FOnTabSelect: TExplorerOnTabSelect;
    FOnTabClose: TExplorerOnTabSelect;
    FTabsSizeOld: integer;
    FTabsSizeAuto: boolean;
    function GetFolder: string;
    function GetOnItemClick: TATShellTreeviewItemClick;
    procedure SetFolder(const AValue: string);
    procedure SetOnItemClick(AValue: TATShellTreeviewItemClick);
  public
    procedure Refresh;
    procedure UpdateTabs(ASelChange: boolean);
    procedure UpdateTabsTopIndex;
    procedure UpdatePanelSizes;
    procedure UpdateUI;
    procedure UpdateCaptionTabs;
    procedure UpdateCaptionTree;
    procedure FocusFilename(const fn: string);
    property Folder: string read GetFolder write SetFolder;
    property OnItemClick: TATShellTreeviewItemClick read GetOnItemClick write SetOnItemClick;
    property OnGetTabs: TExplorerOnGetTabs read FOnGetTabs write FOnGetTabs;
    property OnGetTabProp: TExplorerOnGetTabProp read FOnGetTabProp write FOnGetTabProp;
    property OnTabSelect: TExplorerOnTabSelect read FOnTabSelect write FOnTabSelect;
    property OnTabClose: TExplorerOnTabSelect read FOnTabClose write FOnTabClose;
  end;

implementation

{$R *.lfm}

{ TfmExplorer }

procedure TfmExplorer.FormCreate(Sender: TObject);
begin
  Tree:= TATShellTreeview.Create(Self);
  Tree.Parent:= PanelTree;
  Tree.Align:= alClient;

  FTabsSizeAuto:= true;
end;

procedure TfmExplorer.BtnTreeXClick(Sender: TObject);
begin
  Tree.Visible:= not Tree.Visible;
  UpdatePanelSizes;
end;

procedure TfmExplorer.BtnTabsXClick(Sender: TObject);
begin
  ListTabs.Visible:= not ListTabs.Visible;
  UpdatePanelSizes;
end;

procedure TfmExplorer.FormDestroy(Sender: TObject);
begin
  Tree.Items.Clear;
end;

procedure TfmExplorer.FormResize(Sender: TObject);
begin
  if (Folder='') and (ListTabs.Items.Count=0) then exit;
  UpdatePanelSizes;
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
  NIconSize, NIndent1, NIndent2: integer;
  N: integer;
begin
  list:= ListTabs;
  if ATShellOptions.ShowIcons then
    NIconSize:= ATShellIcons.Images.Width
  else
    NIconSize:= 0;
  NIndent1:= ExplorerOptions.TabsIndent1;
  NIndent2:= ExplorerOptions.TabsIndent2;

  if AIndex=list.ItemIndex then
    C.Brush.Color:= list.Theme^.ColorBgListboxSel
  else
  if list.HotTrack and (AIndex=list.HotTrackIndex) then
    C.Brush.Color:= list.Theme^.ColorBgListboxHottrack
  else
    C.Brush.Color:= list.Theme^.ColorBgListbox;
  C.FillRect(ARect);

  S:= List.Items[AIndex];
  fn:= '';
  N:= Pos(' (', S);
  if N>0 then
    fn:= Copy(S, 1, N-1);

  NIcon:= -1;
  if fn<>'' then
    NIcon:= ATShellIcons.ImageIndex(fn);

  if NIcon>=0 then
    ATShellIcons.Images.Draw(C,
      ARect.Left+NIndent1,
      (ARect.Top+ARect.Bottom-NIconSize) div 2,
      NIcon
      );

  C.Font.Color:= list.Theme^.ColorFont;
  C.TextOut(
    ARect.Left+NIndent1+NIconSize+NIndent2,
    (ARect.Top+ARect.Bottom-C.TextHeight(S)) div 2,
    S);
end;

procedure TfmExplorer.ListTabsResize(Sender: TObject);
begin
end;

procedure TfmExplorer.Splitter1Moved(Sender: TObject);
begin
  UpdateTabsTopIndex;
  FTabsSizeOld:= ListTabs.Height;
  FTabsSizeAuto:= false;
end;

procedure TfmExplorer.SetFolder(const AValue: string);
begin
  Tree.Folder:= AValue;
  UpdateCaptionTree;
end;

procedure TfmExplorer.SetOnItemClick(AValue: TATShellTreeviewItemClick);
begin
  Tree.OnShellItemClick:= AValue;
end;

function TfmExplorer.GetFolder: string;
begin
  Result:= Tree.Folder;
end;

function TfmExplorer.GetOnItemClick: TATShellTreeviewItemClick;
begin
  Result:= Tree.OnShellItemClick;
end;

procedure TfmExplorer.Refresh;
begin
  Tree.Refresh;
  ListTabs.Invalidate;
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
  finally
    ListTabs.Items.EndUpdate;
    ListTabs.Invalidate;
  end;

  UpdatePanelSizes;
  UpdateTabsTopIndex;
  UpdateCaptionTabs;
end;

procedure TfmExplorer.UpdateTabsTopIndex;
begin
  ListTabs.ItemTop:= Max(0, Min(ListTabs.ItemTop, ListTabs.ItemCount-ListTabs.VisibleItems));
end;

procedure TfmExplorer.UpdateUI;
var
  N: integer;
  b: boolean;
  NColor: TColor;
begin
  PanelTabsCap.Font.Name:= ExplorerOptions.CaptionFontName;
  PanelTreeCap.Font.Name:= ExplorerOptions.CaptionFontName;
  PanelTabsCap.Font.Size:= ExplorerOptions.CaptionFontSize;
  PanelTreeCap.Font.Size:= ExplorerOptions.CaptionFontSize;

  Tree.Options:= Tree.Options-[tvoThemedDraw];
  Tree.Font.Name:= ATFlatTheme.FontName;
  Tree.Font.Size:= ATFlatTheme.FontSize;
  Tree.Font.Color:= ATFlatTheme.ColorFont;

  N:= ExplorerOptions.CaptionPanelHeight;
  PanelTabsCap.Height:= N;
  PanelTreeCap.Height:= N;

  NColor:= ExplorerOptions.CaptionPanelColorBg;
  PanelTabsCap.Color:= NColor;
  PanelTreeCap.Color:= NColor;

  NColor:= ExplorerOptions.CaptionPanelColorFont;
  PanelTabsCap.Font.Color:= NColor;
  PanelTreeCap.Font.Color:= NColor;

  N:= ExplorerOptions.CaptionButtonXSize;
  BtnTabsX.Width:= N;
  BtnTreeX.Width:= N;

  b:= ExplorerOptions.ShowCaptionButtonsX;
  BtnTabsX.Visible:= b;
  BtnTreeX.Visible:= b;

  if ExplorerOptions.TabsBelow then
    PanelTabs.Align:= alBottom
  else
    PanelTabs.Align:= alTop;
end;

procedure TfmExplorer.UpdateCaptionTabs;
var
  S: string;
begin
  S:= ExplorerOptions.CaptionTabsText;
  PanelTabsCap.Caption:= S;
end;

procedure TfmExplorer.UpdateCaptionTree;
var
  S: string;
begin
  S:= ExplorerOptions.CaptionTreeText;
  if ExplorerOptions.ShowDirInTreeCaption then
    if Folder<>'' then
      //S:= S+' "'+ExtractFileName(FFolder)+'"';
      S:= '"'+ExtractFileName(Folder)+'"';
  PanelTreeCap.Caption:= S;
end;

procedure TfmExplorer.UpdatePanelSizes;
var
  N, NSize, NSizeAuto, NSizeNormal, NSizeMax: integer;
  bTabs, bTree: boolean;
begin
  bTabs:= ListTabs.Visible;
  bTree:= Tree.Visible;
  BtnTabsX.Caption:= ExplorerOptions.CaptionButtonX[bTabs];
  BtnTreeX.Caption:= ExplorerOptions.CaptionButtonX[bTree];

  {
  if ExplorerOptions.TabsAutoSize then
    BtnTabsX.Enabled:= ListTabs.ItemCount>0;
    }

  if not bTabs then
  begin
    Splitter1.Visible:= false;
    PanelTabs.Constraints.MaxHeight:= PanelTabsCap.Height;
    PanelTabs.Height:= PanelTabsCap.Height;
    FTabsSizeAuto:= true;
    exit
  end;

  N:= ClientHeight;
  NSizeNormal:= N * ExplorerOptions.TabsHeightPercents div 100;
  NSizeMax:= N * ExplorerOptions.TabsHeightMaxPercents div 100;

  if ExplorerOptions.TabsAutoSize then
    NSizeAuto:= ListTabs.ItemCount*ListTabs.ItemHeightDefault + ExplorerOptions.TabsIndentV
  else
    NSizeAuto:= NSizeNormal;

  if FTabsSizeAuto then
    NSize:= Min(NSizeAuto, NSizeNormal)
  else
  begin
    if NSize>FTabsSizeOld then
      NSize:= FTabsSizeOld
    else
    begin
      NSize:= Min(NSizeAuto, NSizeNormal);
      FTabsSizeAuto:= true;
    end;
  end;

  Splitter1.Visible:= NSizeAuto>NSizeNormal;
  PanelTabs.Constraints.MaxHeight:= PanelTabsCap.Height + Min(NSizeAuto, NSizeMax);
  PanelTabs.Height:= PanelTabsCap.Height + NSize;
end;

procedure TfmExplorer.FocusFilename(const fn: string);
begin
  Tree.FocusNodeOfFilename(fn);
end;


initialization

  with ExplorerOptions do
  begin
    ShowCaptionButtonsX:= true;
    ShowDirInTreeCaption:= true;
    TabsIndent1:= 12;
    TabsIndent2:= 4;
    TabsIndentV:= 0;
    TabsHeightPercents:= 25;
    TabsHeightMaxPercents:= 70;
    TabsAutoSize:= true;
    TabsBelow:= false;
    CaptionTabsText:= 'Tabs';
    CaptionTreeText:= 'Folder';
    CaptionPanelHeight:= 20;
    CaptionPanelColorBg:= $e0d0d0;
    CaptionPanelColorFont:= clNavy;
    CaptionButtonXSize:= 22;
    CaptionButtonX[false]:= '+';
    CaptionButtonX[true]:= '–';
    CaptionFontName:= 'default';
    CaptionFontSize:= 9;
  end;

end.

