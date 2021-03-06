unit virtuallistview;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  LCLType,
  LMessages,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ActnList,
  ComCtrls,
  ExtCtrls,
  Menus,
  StdCtrls,
  Types,
  contnrs;

type
  TVirtualListviewTextLayout = (vtlTop, vtlCenter, vtlBottom);

type
  TCustomVirtualListview = class;
  TVirtualListviewItem = class;
  TVirtualListviewItemList = class;

  TOnVirtualListviewFocusedChanged = procedure(Sender: TObject; FocusedItem, OldFocusedItem: TVirtualListviewItem) of object;
  TOnVirtualListviewSelectedChanged = TNotifyEvent;
  TOnVirtualListviewSort = function(Sender: TObject; Item1, Item2: TVirtualListviewItem): Integer of object;

  type
    TExpandImagePostition = (eip_Left, eip_Right);

  { TVirtualListviewItem }

  TVirtualListviewItem = class(TComponent)
  private
    FAllowFocus: Boolean;
    FAllowSelect: Boolean;
    FCaptionRect: TRect;
    FCaptions: TStringList;
    FClientRect: TRect;
    FClientWithChildrenRect: TRect;
    FColor: TColor;
    FDetailsRect: TRect;
    FDragTarget: Boolean;
    FEnabled: Boolean;
    FAllowExpand: Boolean;
    FExpanded: Boolean;
    FExpandImageRect: TRect;
    FFocused: Boolean;
    FHeight: Integer;
    FImageIndex: Integer;
    FImageRect: TRect;
    FIndex: Integer;
    FLeft: Integer;
    FLevel: Integer;
    FOwnerListview: TCustomVirtualListview;
    FSelected: Boolean;
    FChildItems: TVirtualListviewItemList;
    FStateImageIndex: Integer;
    FStateImageRect: TRect;
    FTag: Integer;
    FTop: Integer;
    FVisible: Boolean;
    FVisibleIndex: Integer;
    FWidth: Integer;
    function GetBottom: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetAllowExpand(AValue: Boolean);
    procedure SetAllowFocus(AValue: Boolean);
    procedure SetAllowSelect(AValue: Boolean);
    procedure SetCaptions(AValue: TStringList);
    procedure SetColor(AValue: TColor);
    procedure SetDragTarget(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetExpanded(AValue: Boolean);
    procedure SetFocused(AValue: Boolean);
    procedure SetImageIndex(AValue: Integer);
    procedure SetSelected(AValue: Boolean);
    procedure SetStateImageIndex(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
  protected
    // These Rectangles are all referenced from the Items Top/Left coordinates, i.e. (0, 0) referenced
    // And the item must have been painted once for them to be valid
    TextRects: array of TRect;
    property ExpandImageRect: TRect read FExpandImageRect write FExpandImageRect;
    property ImageRect: TRect read FImageRect write FImageRect;
    property StateImageRect: TRect read FStateImageRect write FStateImageRect;
    property CaptionRect: TRect read FCaptionRect write FCaptionRect;
    property DetailsRect: TRect read FDetailsRect write FDetailsRect;

    // Helpers to decide if the item can perform/display certin actions based on
    // local and the owner listview's properites.
    function CaptionPropertiesValid: Boolean;
    function ExpandablePropertiesValid: Boolean;
    function ImagePropertiesValid: Boolean;
    function StateImagePropertiesValid: Boolean;

    // Alloctes memory for the number of TextRects required based on the Captions property
    // Only valid after the item has been painted once
    procedure CreateTextRects;

    //
    procedure EndDrag(Target: TObject; X, Y: Integer); virtual;
    procedure DragEnter; virtual;
    procedure DragLeave; virtual;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); virtual;

    procedure Paint(Canvas: TCanvas; OffsetX, OffsetY: Integer);

    //
    procedure SetClientRect(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetClientWithChildrenRect(ALeft, ATop, AWidth, AHeight: Integer);

  public
    // Properties to enable/disable aspects of the itme
    property AllowExpand: Boolean read FAllowExpand write SetAllowExpand;
    property AllowFocus: Boolean read FAllowFocus write SetAllowFocus;
    property AllowSelect: Boolean read FAllowSelect write SetAllowSelect;

    // Item bounds properites.  They return in Viewport Coordinates
    property Bottom: Integer read GetBottom;
    property ClientRect: TRect read FClientRect;
    property ClientWithChildrenRect: TRect read FClientWithChildrenRect;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;

    // Item states
    property Captions: TStringList read FCaptions write SetCaptions;
    property ChildItems: TVirtualListviewItemList read FChildItems write FChildItems;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property Focused: Boolean read FFocused write SetFocused;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Selected: Boolean read FSelected write SetSelected;
    property StateImageIndex: Integer read FStateImageIndex write SetStateImageIndex;
    property Tag: Integer read FTag write FTag;
    property Visible: Boolean read FVisible write SetVisible;

    // Dynamic Item states
    property DragTarget: Boolean read FDragTarget write SetDragTarget;  // True when the mouse is over the item when the uses is dragging

    // Housekeeping properites
    property Index: Integer read FIndex; // Index back into the Item List of the owner listview
    property Level: Integer read FLevel; // Subchild Level the item is
    property OwnerListview: TCustomVirtualListview read FOwnerListview; // Owner
    property VisibleIndex: Integer read FVisibleIndex; // Index back into the Visible List of the owner Listview


    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CollapseAllChildren(IncludeSubChildren: Boolean);
    procedure ExpandAllChildren(IncludeSubChildren: Boolean);
    procedure Invalidate(Update: Boolean);
    function PtInExpandZone(ClientPoint: TPoint): Boolean; virtual;
    function PtInImage(ClientPoint: TPoint): Boolean; virtual;
    function PtInItem(ClientPoint: TPoint): Boolean; virtual;
    function PtInStateImage(ClientPoint: TPoint): Boolean; virtual;
    procedure SelectAllChildren(IncludeSubChildren: Boolean);
    procedure UnSelectAllChildren(IncludeSubChildren: Boolean);
  end;

  { TVirtualListviewItemList }

  TVirtualListviewItemList = class(TComponent)
  private
    FItemList: TObjectList;
    FOwnerListview: TCustomVirtualListview;
    function GetCount: Integer;
    function GetItems(Index: Integer): TVirtualListviewItem;
    procedure SetItems(Index: Integer; AValue: TVirtualListviewItem);
  protected
    property ItemList: TObjectList read FItemList write FItemList;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TVirtualListviewItem read GetItems write SetItems; default;
    property OwnerListview: TCustomVirtualListview read FOwnerListview;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Add(ACaption: string; ImageIndex: Integer = -1): TVirtualListviewItem;
    procedure Delete(Index: Integer);
    function Remove(ACaption: string): TVirtualListviewItem;
    function Find(ACaption: string): TVirtualListviewItem;
    function FindIndex(ACaption: string): Integer;
    procedure Sort;
  end;

  { TVirtualListviewItemContainer }

  TVirtualListviewItemContainer = class(TComponent)
  private
    FItemList: TObjectList;
    FOwnerListview: TCustomVirtualListview;
    function GetCount: Integer;
    function GetItems(Index: Integer): TVirtualListviewItem;
    procedure SetItems(Index: Integer; AValue: TVirtualListviewItem);
  protected
    property ItemList: TObjectList read FItemList write FItemList;
    property OwnerListview: TCustomVirtualListview read FOwnerListview;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TVirtualListviewItem read GetItems write SetItems; default;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(Item: TVirtualListviewItem);
    procedure Clear;
    procedure Delete(Index: Integer);
    function IndexOf(AnItem: TVirtualListviewItem): Integer;
    function Remove(Item: TVirtualListviewItem): Integer;
  end;

  TMouseControllerState = (mcs_Captured, mcs_Down, mcs_DragRectPending, mcs_DragItemsPending, mcs_DragItem, mcs_DragRect, mcs_CtrlDown, mcs_Meta {Apple Key, Ctrl in Windows?}, mcs_AltDown, mcs_ShiftDown, mcs_LeftButton, mcs_RightButton, mcs_MiddleButton);
  TMouseControllerStateSet = set of TMouseControllerState;

  { TMustangpeakMouseController }

  TMustangpeakMouseController = class
  private
    FDeltaDragDetect: Integer;   // Number of pixel from the initial mouse down point before it is concidered a drag start
    FMouseLastPt: TPoint;
    FMouseDownPt: TPoint;
    FState: TMouseControllerStateSet;
  protected
    procedure Paint(Canvas: TCanvas; OffsetX, OffsetY: Integer);
  public
    property MouseDownPt: TPoint read FMouseDownPt write FMouseDownPt;
    property MouseLastPt: TPoint read FMouseLastPt write FMouseLastPt;
    property State: TMouseControllerStateSet read FState write FState;
    property DeltaDragDetect: Integer read FDeltaDragDetect write FDeltaDragDetect;

    constructor Create;
    function AnyButtonDown: Boolean;
  end;

  TCustomVirtualListviewInternalState = (
    vlis_SelectionMultiSelect,  // Sets the control state so any newly selected items are added to the selection list
    vlis_SelectionLockUpdate    // Sets the control state so Selection Changed Events are blocked while multiple items are selected as to not slow the selection process down by Event handlers
    );
  TCustomVirtualListviewInternalStateSet = set of TCustomVirtualListviewInternalState;

  { TCustomVirtualListview }

  TCustomVirtualListview = class(TCustomPanel)
  private
    FBkGndGradient2: TColor;
    FBkGkdGradientDirection: TGradientDirection;
    FBkGndColor: TColor;
    FBkGndGradient1: TColor;
    FBkGndGradientEnable: Boolean;
    FDragTargetItem: TVirtualListviewItem;
    FExpandableItems: Boolean;
    FExpandImagePosition: TExpandImagePostition;
    FExpandImages: TImageList;
    FExpandIndexCollapsed: Integer;
    FExpandIndexExpanded: Integer;
    FFocusedItem: TVirtualListviewItem;
    FHorzScrollBar: TScrollBar;
    FHotTrackColor: TColor;
    FHotTrackGradientColor1: TColor;
    FHotTrackGradientColor2: TColor;
    FImages: TImageList;
    FInternalState: TCustomVirtualListviewInternalStateSet;
    FLevelIndent: Integer;
    FMouseController: TMustangpeakMouseController;
    FOnDebugEvent: TNotifyEvent;
    FOnSort: TOnVirtualListviewSort;
    FSelectedGradientHilightColor1: TColor;
    FSelectedGradientHilightColor2: TColor;
    FSelectedHilightColor: TColor;
    FSelectedItem: TVirtualListviewItem;
    FSelectedItems: TVirtualListviewItemContainer;
    FShowFocus: Boolean;
    FStateImages: TImageList;
    FTextLayout: TVirtualListviewTextLayout;
    FCaptionIndent: Integer;
    FCaptionLineCount: Integer;
    FDefaultItemHeight: Integer;
    FDetailsFont: TFont;
    FDetailsIndent: Integer;
    FItems: TVirtualListviewItemList;
    FVertScrollBar: TScrollBar;
    FViewportRect: TRect;
    FVisibleItems: TVirtualListviewItemContainer;
    FUpdateLock: Integer;
    OnFocusChanged: TOnVirtualListviewFocusedChanged;
    OnSelectChanged: TOnVirtualListviewSelectedChanged;
    function GetSelectedCount: Integer;
    procedure SetBkGndGradient2(AValue: TColor);
    procedure SetBkGkdGradientDirection(AValue: TGradientDirection);
    procedure SetBkGndColor(AValue: TColor);
    procedure SetBkGndGradient1(AValue: TColor);
    procedure SetBkGndGradientEnable(AValue: Boolean);
    procedure SetExpandableItems(AValue: Boolean);
    procedure SetExpandImagePosition(AValue: TExpandImagePostition);
    procedure SetFocusedItem(AValue: TVirtualListviewItem);
    procedure SetHotTrackColor(AValue: TColor);
    procedure SetHotTrackGradientColor1(AValue: TColor);
    procedure SetHotTrackGradientColor2(AValue: TColor);
    procedure SetLevelIndent(AValue: Integer);
    procedure SetSelectedGradientHilightColor1(AValue: TColor);
    procedure SetSelectedGradientHilightColor2(AValue: TColor);
    procedure SetSelectedHilightColor(AValue: TColor);
    procedure SetSelectedItem(AValue: TVirtualListviewItem);
    procedure SetShowFocus(AValue: Boolean);
    procedure SetTextLayout(AValue: TVirtualListviewTextLayout);
    procedure SetCaptionIndent(AValue: Integer);
    procedure SetCaptionLineCount(AValue: Integer);
    procedure SetDefaultItemHeight(AValue: Integer);
    procedure SetDetailsFont(AValue: TFont);
    procedure SetDetailsIndent(AValue: Integer);
  protected
    // Colors and Color control properities
    property BkGndColor: TColor read FBkGndColor write SetBkGndColor;
    property BkGndGradientEnable: Boolean read FBkGndGradientEnable write SetBkGndGradientEnable;
    property BkGndGradientDirection: TGradientDirection read FBkGkdGradientDirection write SetBkGkdGradientDirection;
    property BkGndGradient1: TColor read FBkGndGradient1 write SetBkGndGradient1;
    property BkGndGradient2: TColor read FBkGndGradient2 write SetBkGndGradient2;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor;
    property HotTrackGradientColor1: TColor read FHotTrackGradientColor1 write SetHotTrackGradientColor1;
    property HotTrackGradientColor2: TColor read FHotTrackGradientColor2 write SetHotTrackGradientColor2;
    property SelectedHilightColor: TColor read FSelectedHilightColor write SetSelectedHilightColor;
    property SelectedGradientHilightColor1: TColor read FSelectedGradientHilightColor1 write SetSelectedGradientHilightColor1;
    property SelectedGradientHilightColor2: TColor read FSelectedGradientHilightColor2 write SetSelectedGradientHilightColor2;

    // Text properites
    property CaptionIndent: Integer read FCaptionIndent write SetCaptionIndent;
    property CaptionLineCount: Integer read FCaptionLineCount write SetCaptionLineCount;
    property DetailsIndent: Integer read FDetailsIndent write SetDetailsIndent;
    property DetailsFont: TFont read FDetailsFont write SetDetailsFont;
    property TextLayout: TVirtualListviewTextLayout read FTextLayout write SetTextLayout;

    // Expandable Items controls and visual items
    property ExpandableItems: Boolean read FExpandableItems write SetExpandableItems;
    property ExpandImages: TImageList read FExpandImages write FExpandImages;
    property ExpandIndexExpanded: Integer read FExpandIndexExpanded write FExpandIndexExpanded;
    property ExpandIndexCollapsed: Integer read FExpandIndexCollapsed write FExpandIndexCollapsed;
    property ExpandImagePosition: TExpandImagePostition read FExpandImagePosition write SetExpandImagePosition;
    property LevelIndent: Integer read FLevelIndent write SetLevelIndent;

    // Selection Helpers
    property SelectedItem: TVirtualListviewItem read FSelectedItem write SetSelectedItem;
    property FocusedItem: TVirtualListviewItem read FFocusedItem write SetFocusedItem;
    property SelectedCount: Integer read GetSelectedCount;

    // Item control and sizing
    property ShowFocus: Boolean read FShowFocus write SetShowFocus;
    property DefaultItemHeight: Integer read FDefaultItemHeight write SetDefaultItemHeight;

    // Drag and Drop Helpers
    property DragTargetItem: TVirtualListviewItem read FDragTargetItem;

    // Images for the items
    property Images: TImageList read FImages write FImages;
    property StateImages: TImageList read FStateImages write FStateImages;

    // Lists containing items for various tasks
    property Items: TVirtualListviewItemList read FItems write FItems;
    property SelectedItems: TVirtualListviewItemContainer read FSelectedItems write FSelectedItems;
    property VisibleItems: TVirtualListviewItemContainer read FVisibleItems write FVisibleItems;

    // Helper classes
    property MouseController: TMustangpeakMouseController read FMouseController write FMouseController;
    property VertScrollbar: TScrollBar read FVertScrollBar;
    property HorzScrollbar: TScrollBar read FHorzScrollBar;

    // Events
    property OnDebugEvent: TNotifyEvent read FOnDebugEvent write FOnDebugEvent;
    property OnFocusedChanged: TOnVirtualListviewFocusedChanged read OnFocusChanged write OnFocusChanged;
    property OnSelectedChanged: TOnVirtualListviewSelectedChanged read OnSelectChanged write OnSelectChanged;
    property OnSort: TOnVirtualListviewSort read FOnSort write FOnSort;

    // Internal state properties
    property UpdateLock: Integer read FUpdateLock write FUpdateLock;
    property InternalState: TCustomVirtualListviewInternalStateSet read FInternalState write FInternalState;

    // Callback Notifications from the Items
    procedure SelectChangedNotify(Item: TVirtualListviewItem); virtual;

    procedure BeforeDragStart; override;
    procedure CalculateScrollbars;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoOnDebugEvent; virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragCanceled; override;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); override;
    function DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint; ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean):LRESULT; override;
    procedure DoEndDrag(Target: TObject; X,Y: Integer); override;
    procedure DoOnFocusChanged(Item, OldItem: TVirtualListviewItem); virtual;
    procedure DoOnResize; override;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
    function DoOnSort(Item1, Item2: TVirtualListviewItem): Integer; virtual;
    function GetDragImages: TDragImageList; override;
    procedure ItemNotificationSelectionChanged(Item: TVirtualListviewItem);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure WMLButtonDBLCLK(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMRButtonDBLCLK(var Message: TLMRButtonDblClk); message LM_RBUTTONDBLCLK;
    procedure WMMButtonDBLCLK(var Message: TLMMButtonDblClk); message LM_MBUTTONDBLCLK;
    procedure WMContextMenu(var Message: TLMContextMenu); message LM_CONTEXTMENU;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMShowWindow(var Message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure Paint; override;
    procedure RebuildItems(ItemList: TVirtualListviewItemList; ALevel: Integer);
    procedure OnScrollVertical(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure OnScrollHorizontal(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure DoOnSelectedChanged; virtual;
    procedure OnFontChange(Sender: TObject);
  public
    property ViewportRect: TRect read FViewportRect;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    function ClientRectToCurrentViewPt(ARect: TRect): TRect;
    function ClientPtToCurrentViewPt(APt: TPoint): TPoint;
    function ClientPtToVisibleItem(ClientPt: TPoint; CurrentViewOnly: Boolean): TVirtualListviewItem;
    procedure CollapseAll(IncludeSubChildren: Boolean);
    procedure CurrentViewRect(var ARect: TRect);
    procedure EndUpdate;
    procedure ExpandAll(IncludeSubChildren: Boolean);
    function First: TVirtualListviewItem;
    function FirstInView: TVirtualListviewItem;
    function FirstVisible: TVirtualListviewItem;
    function IsInCurrentView(Item: TVirtualListviewItem): Boolean;
    function Last: TVirtualListviewItem;
    function LastVisible: TVirtualListviewItem;
    function Next(Item: TVirtualListviewItem): TVirtualListviewItem;
    function NextVisible(Item: TVirtualListviewItem): TVirtualListviewItem;
    function Previous(Item: TVirtualListviewItem): TVirtualListviewItem;
    function PreviousVisible(Item: TVirtualListviewItem): TVirtualListviewItem;
    procedure SelectAll(IncludeSubChildren: Boolean);
    procedure ScrollDelta(X, Y: Integer);
    procedure ScrollIntoView(Item: TVirtualListviewItem);
    procedure UnSelectAll(IncludeSubChildren: Boolean);
  end;

  TVirtualListview = class(TCustomVirtualListview)
  public
    property Items;
    property SelectedItem;
    property FocusedItem;
    property SelectedCount;
  published
    property Align;
 //   property Alignment;
    property Anchors;
    property AutoSize;
    property BkGndColor;
    property BkGndGradientEnable;
    property BkGndGradient1;
    property BkGndGradient2;
    property BorderSpacing;
    property BevelColor;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property CaptionIndent;
    property CaptionLineCount;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DefaultItemHeight;
    property DetailsFont;
    property DetailsIndent;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExpandableItems;
    property ExpandImages;
    property ExpandIndexExpanded;
    property ExpandIndexCollapsed;
    property ExpandImagePosition;
    property Font;
    property FullRepaint;
    property HorzScrollbar;
    property HotTrackColor;
    property HotTrackGradientColor1;
    property HotTrackGradientColor2;
    property Images;
    property LevelIndent;
    property ParentBackground;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowFocus;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property Wordwrap;
    property OnClick;
    property OnContextPopup;
    property OnDebugEvent;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFocusedChanged;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnSelectedChanged;
    property OnSort;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property SelectedHilightColor;
    property SelectedGradientHilightColor1;
    property SelectedGradientHilightColor2;
    property StateImages;
    property TextLayout;
    property VertScrollbar;
  end;


procedure StackRect(var ResultRect: TRect; Upper, Lower: TRect);
function HeightRect(ARect: TRect): Integer;
function WidthRect(ARect: TRect): Integer;

// graphics
procedure Shade(ShadeFactor: real; var R, G, B: Byte);     // Does not work on System Colors
function Shade(ShadeFactor: real; Color: TColor): TColor;  // Does not work on System Colors
procedure Tint(TintFactor: real; var R, G, B: byte);       // Does not work on System Colors
function Tint(TintFactor: real; Color: TColor): TColor;    // Does not work on System Colors

procedure Register;

implementation

procedure Shade(ShadeFactor: real; var R, G, B: Byte);
begin
  R := Round(R * (1-ShadeFactor));
  G := Round(G * (1-ShadeFactor));
  B := Round(B * (1-ShadeFactor));
end;

function Shade(ShadeFactor: real; Color: TColor): TColor;
var
  R, G, B: Byte;
begin
  R := Red(Color);
  G := Green(Color);
  B := Blue(Color);
  Shade(ShadeFactor, R, G, B);
  Result := RGBToColor(R, G, B);
end;

procedure Tint(TintFactor: real; var R, G, B: byte);
begin
  R := Round(R + (255 - R) * TintFactor);
  G := Round(G + (255 - G) * TintFactor);
  B := Round(B + (255 - B) * TintFactor);
end;

function Tint(TintFactor: real; Color: TColor): TColor;
var
  R, G, B: Byte;
begin
  R := Red(Color);
  G := Green(Color);
  B := Blue(Color);
  Tint(TintFactor, R, G, B);
  Result := RGBToColor(R, G, B);
end;

procedure Register;
begin
 // {$I TVirtualListview.lrs}
   RegisterComponents('VirtualListview', [TVirtualListview]);
end;

procedure EmptyRect(var ARect: TRect);
begin
  ARect := Rect(0, 0, 0, 0);
end;

procedure StackRect(var ResultRect: TRect; Upper, Lower: TRect);
begin
  ResultRect := Lower;
  if Lower.Top < Upper.Bottom then
    OffsetRect(ResultRect, 0, Upper.Bottom - Lower.Top)
  else
    OffsetRect(ResultRect, 0, Lower.Top - Upper.Bottom);
end;

function HeightRect(ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
end;

function WidthRect(ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;


{ TMustangpeakMouseController }

constructor TMustangpeakMouseController.Create;
begin
  inherited Create;
  DeltaDragDetect := 3;
end;

function TMustangpeakMouseController.AnyButtonDown: Boolean;
begin
  Result := State * [mcs_LeftButton, mcs_RightButton, mcs_MiddleButton] <> []
end;

procedure TMustangpeakMouseController.Paint(Canvas: TCanvas; OffsetX, OffsetY: Integer);
var
  StartX, StartY, StopX, StopY: Integer;
begin
 if MouseDownPt.X > MouseLastPt.X then
 begin
   StartX := MouseLastPt.X;
   StopX := MouseDownPt.X
 end else
 begin
   StopX := MouseLastPt.X;
   StartX := MouseDownPt.X
 end;

 if MouseDownPt.Y > MouseLastPt.Y then
 begin
   StartY := MouseLastPt.Y;
   StopY := MouseDownPt.Y
 end else
 begin
   StopY := MouseLastPt.Y;
   StartY := MouseDownPt.Y
 end;

 if mcs_DragRect in State then
   Canvas.DrawFocusRect(Rect(StartX, StartY, StopX, StopY));
end;

{ TVirtualListviewItemContainer }

procedure TVirtualListviewItemContainer.Add(Item: TVirtualListviewItem);
begin
  ItemList.Add(Item);
end;

procedure TVirtualListviewItemContainer.Clear;
begin
  if Assigned(ItemList) then
    ItemList.Clear;
end;

constructor TVirtualListviewItemContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemList := TObjectList.Create;
  ItemList.OwnsObjects := False;
end;

procedure TVirtualListviewItemContainer.Delete(Index: Integer);
begin
  ItemList.Delete(Index);
end;

destructor TVirtualListviewItemContainer.Destroy;
begin
  FreeAndNil(FItemList);
  inherited Destroy;
end;

function TVirtualListviewItemContainer.GetCount: Integer;
begin
  Result := ItemList.Count;
end;

function TVirtualListviewItemContainer.GetItems(Index: Integer): TVirtualListviewItem;
begin
  Result := ItemList[Index] as TVirtualListviewItem
end;

function TVirtualListviewItemContainer.IndexOf(AnItem: TVirtualListviewItem): Integer;
begin
  Result := ItemList.IndexOf(AnItem);
end;

function TVirtualListviewItemContainer.Remove(Item: TVirtualListviewItem): Integer;
begin
  Result := ItemList.Remove(Item);
end;

procedure TVirtualListviewItemContainer.SetItems(Index: Integer; AValue: TVirtualListviewItem);
begin
  ItemList[Index] := AValue
end;

{ TVirtualListviewItem }

constructor TVirtualListviewItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptions := TStringList.Create;
  FImageIndex := -1;
  FStateImageIndex := -1;
  FVisible := True;
  FEnabled := True;
  FColor := clBtnFace;
  FTop := 0;
  FLeft := 0;
  FWidth := 20;
  FHeight := 20;
  FChildItems := TVirtualListviewItemList.Create(nil);   // Will free ourselves
  FAllowSelect := True;
  FAllowFocus := True;
end;

function TVirtualListviewItem.CaptionPropertiesValid: Boolean;
begin
  Result := (Captions.Count > 0) and (OwnerListview.CaptionLineCount > 0)
end;

procedure TVirtualListviewItem.CollapseAllChildren(IncludeSubChildren: Boolean);
var
  i: Integer;
  Item: TVirtualListviewItem;
begin
  OwnerListview.BeginUpdate;
  for i := 0 to ChildItems.Count - 1 do
  begin
    Item := ChildItems[i];
    Item.Expanded := False;
    if IncludeSubChildren then
      Item.CollapseAllChildren(IncludeSubChildren);
  end;
  OwnerListview.EndUpdate;
end;

procedure TVirtualListviewItem.CreateTextRects;
var
  i: Integer;
begin
  SetLength(TextRects, Captions.Count);
  for i := 0 to Length(TextRects) - 1 do
    EmptyRect(TextRects[i]);
end;

destructor TVirtualListviewItem.Destroy;
begin
  FreeAndNil(FCaptions);
  FreeAndNil(FChildItems);
  inherited Destroy;
end;

procedure TVirtualListviewItem.DragEnter;
begin
  DragTarget := True;
end;

procedure TVirtualListviewItem.DragLeave;
begin
  DragTarget := False;
end;

procedure TVirtualListviewItem.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Temp: Boolean;
begin
  Accept := AllowSelect;
 { Temp := PtInRect(FClientRect, Point(X, Y));
  if Temp <> DragTarget then
  begin
    FDragTarget := Temp;
    if DragTarget then
      Invalidate(True);        // Repaint in case we are hot tracking
  end;   }
end;

procedure TVirtualListviewItem.EndDrag(Target: TObject; X, Y: Integer);
begin

end;

function TVirtualListviewItem.ExpandablePropertiesValid: Boolean;
begin
  Result := OwnerListview.ExpandableItems and Assigned(OwnerListview.ExpandImages) and (OwnerListview.ExpandIndexCollapsed > -1) and (OwnerListview.ExpandIndexExpanded > -1)
end;

procedure TVirtualListviewItem.ExpandAllChildren(IncludeSubChildren: Boolean);
var
  i: Integer;
  Item: TVirtualListviewItem;
begin
  OwnerListview.BeginUpdate;
  for i := 0 to ChildItems.Count - 1 do
  begin
    Item := ChildItems[i];
    Item.Expanded := True;
    if IncludeSubChildren then
      Item.ExpandAllChildren(IncludeSubChildren);
  end;
  OwnerListview.EndUpdate;
end;

procedure TVirtualListviewItem.Invalidate(Update: Boolean);
begin
  if Assigned(OwnerListview) then
  begin
    OwnerListview.Invalidate;
    if Update and (OwnerListview.UpdateLock = 0) then
      OwnerListview.Update;
  end;
end;

procedure TVirtualListviewItem.Paint(Canvas: TCanvas; OffsetX, OffsetY: Integer);
var
  TextExtent: TSize;
  LeftDrawingEdge, RightDrawingEdge, ClientH, ImageX, ImageY, ImageW, ImageH,
  i, Indent, LocalOffsetY, ViewPortOffsetX, ViewportOffsetY: Integer;
  ViewportRect, DrawingBoundsRect, TextBoundsRect, LocalClientRect: TRect;
  Points: array of TPoint;
  Color1, Color2: TColor;
begin
  // Assume no images
  DrawingBoundsRect := Rect(0, 0, 0, 0);
  FImageRect := Rect(0, 0, 0, 0);
  FStateImageRect := Rect(0, 0, 0, 0);
  TextBoundsRect := Rect(0, 0, 0, 0);

  LocalClientRect := ClientRect;
  InflateRect(LocalClientRect, -2, -2);

  LeftDrawingEdge := (OwnerListview.LevelIndent * Level);
  RightDrawingEdge := LocalClientRect.Right;
  if OwnerListview.VertScrollbar.Visible then
    Dec(RightDrawingEdge, OwnerListview.VertScrollbar.Width);

  ClientH := LocalClientRect.Bottom-LocalClientRect.Top;

  // This must be the real complete Rect
  ViewportOffsetX := OffsetX + ClientRect.Left;
  ViewportOffsetY := OffsetY + ClientRect.Top;

  // These images can be on the left or right and set the stage for the rest of the painting
  if OwnerListview.ExpandableItems then
    if ExpandablePropertiesValid then
    begin
      ImageW := OwnerListview.ExpandImages.Width;
      ImageH := OwnerListview.ExpandImages.Height;
      case OwnerListview.ExpandImagePosition of
        eip_Left : begin
                     ImageX := LeftDrawingEdge;
                     ImageY := (ClientH - ImageH) div 2;   // Center Vertical
                     Inc(LeftDrawingEdge, ImageW);
                     DrawingBoundsRect.Left := LeftDrawingEdge;
                     DrawingBoundsRect.Right := LeftDrawingEdge;
                   end;
        eip_Right: begin
                     Dec(RightDrawingEdge, ImageW);
                     ImageX := RightDrawingEdge;
                     ImageY := (ClientH - ImageH) div 2;   // Center Vertical
                     DrawingBoundsRect.Left := RightDrawingEdge;
                     DrawingBoundsRect.Left := LeftDrawingEdge;
                   end;
      end;
      FExpandImageRect := Rect(ImageX, ImageY, ImageX+ImageW, ImageY+ImageH);
    end else
      FExpandImageRect := Rect(0, 0, 0, 0)
  else
    FExpandImageRect := Rect(0, 0, 0, 0);

  // Always to the left but to right of Expand image if used
  if StateImagePropertiesValid then
  begin
    ImageW := OwnerListview.StateImages.Width;
    ImageH := OwnerListview.StateImages.Height;
    ImageX := LeftDrawingEdge;
    // Track the Text to keep it aligned
    case OwnerListview.TextLayout of
      vtlTop    : ImageY := 0;                          // Top
      vtlCenter : ImageY := (ClientH - ImageH) div 2;   // Center Vertical
      vtlBottom : ImageY := ClientH - ImageH            // Bottom
    end;
    Inc(LeftDrawingEdge, ImageW);
    FStateImageRect := Rect(ImageX, ImageY, ImageX+ImageW, ImageY+ImageH);
  end;
  if HeightRect(DrawingBoundsRect) = 0 then
    OffsetRect(DrawingBoundsRect, 0,  StateImageRect.Top);
  UnionRect(DrawingBoundsRect, DrawingBoundsRect, StateImageRect);

  // Next come the item images if they exist
  if ImagePropertiesValid then
  begin
    ImageW := OwnerListview.Images.Width;
    ImageH := OwnerListview.Images.Height;
    ImageX := LeftDrawingEdge;
    Inc(LeftDrawingEdge, ImageW);
    // Track the Text to keep it aligned
    case OwnerListview.TextLayout of
      vtlTop    : ImageY := 0;                          // Top
      vtlCenter : ImageY := (ClientH - ImageH) div 2;   // Center Vertical
      vtlBottom : ImageY := ClientH - ImageH            // Bottom
    end;
    FImageRect := Rect(ImageX, ImageY, ImageX+ImageW, ImageY+ImageH);
  end;
  if HeightRect(DrawingBoundsRect) = 0 then
    OffsetRect(DrawingBoundsRect, 0,  ImageRect.Top);
  UnionRect(DrawingBoundsRect, DrawingBoundsRect, ImageRect);

  if CaptionPropertiesValid then
  begin
    CreateTextRects;

    Canvas.Font.Assign(OwnerListview.Font);
    TextExtent := Canvas.TextExtent(Captions[0]);
    TextRects[0].Right := TextExtent.cx;
    TextRects[0].Bottom := TextExtent.cy;
    Indent := LeftDrawingEdge + OwnerListview.CaptionIndent;
    OffsetRect(TextRects[0], Indent, 0);
    if TextRects[0].Right > RightDrawingEdge then
      TextRects[0].Right := RightDrawingEdge;
    TextBoundsRect := TextRects[0];

    Canvas.Font.Assign(OwnerListview.DetailsFont);
    i := 1;
    while (i < OwnerListview.CaptionLineCount) and (i < Captions.Count) do
    begin
      TextExtent := Canvas.TextExtent(Captions[i]);
      TextRects[i].Right := TextExtent.cx;
      TextRects[i].Bottom := TextExtent.cy;
      Indent := LeftDrawingEdge + OwnerListview.DetailsIndent;
      OffsetRect(TextRects[i], Indent, 0);
      if TextRects[i].Right > RightDrawingEdge then
        TextRects[i].Right := RightDrawingEdge;
      StackRect(TextRects[i], TextRects[i-1], TextRects[i]); // Get it oriented below the previous line
      UnionRect(TextBoundsRect, TextBoundsRect, TextRects[i]);
      Inc(i)
    end;

    // Now Vertically adjust the block of texts in the Item
    LocalOffsetY := 0;
    case OwnerListview.TextLayout of
      vtlTop : begin end;  // Do nothing already there
      vtlCenter : if HeightRect(TextBoundsRect) < Height then
                    LocalOffsetY := (Height - HeightRect(TextBoundsRect)) div 2;
      vtlBottom : if TextBoundsRect.Bottom < Bottom then
                    LocalOffsetY := Height - TextBoundsRect.Bottom;
    end;
    // Modify the individual Text Boxes Vert
    for i := 0 to Length(TextRects) - 1 do
    begin
      OffsetRect(TextRects[i], 0, LocalOffsetY);
      if HeightRect(DrawingBoundsRect) = 0 then
          OffsetRect(DrawingBoundsRect, 0,  TextRects[i].Top);
        UnionRect(DrawingBoundsRect, DrawingBoundsRect, TextRects[i]);
    end;
    OffsetRect(TextBoundsRect, 0, LocalOffsetY);
  end else
    SetLength(TextRects, 0);

  // Now everyone is correctly positioned in the Vertical Direction

  // Create a local Bounds rect that is modified by the Scroll Postion
  ViewportRect := ClientRect;
  OffsetRect(ViewportRect, OffsetX, OffsetY);
  if OwnerListview.VertScrollbar.Visible then
    Dec(ViewportRect.Right, OwnerListview.VertScrollbar.Width);

  // Now paint the background
  if OwnerListview.BkGndGradientEnable then
  begin
    if DragTarget then
    begin
      Color1 := ColorToRGB(OwnerListview.HotTrackGradientColor1);
      Color2 := ColorToRGB(OwnerListview.HotTrackGradientColor2);
    end else
    if Selected then
    begin
      Color1 := ColorToRGB(OwnerListview.SelectedGradientHilightColor1);
      Color2 := ColorToRGB(OwnerListview.SelectedGradientHilightColor2);
    end else
    begin
      Color1 := ColorToRGB(OwnerListview.BkGndGradient1);
      Color2 := ColorToRGB(OwnerListview.BkGndGradient2);
    end;

    Canvas.GradientFill(ViewPortRect, Color1, Color2, OwnerListview.BkGndGradientDirection);
    // Tint the Bottom color darker for the outline
    Canvas.Pen.Color := Shade(0.10, Color2);
    SetLength(Points, 4);
    Points[0].Create(ViewportRect.Left, ViewportRect.Top-1);
    Points[1].Create(ViewportRect.Left, ViewportRect.Bottom-1);
    Points[2].Create(ViewportRect.Right-1, ViewportRect.Bottom-1);
    Points[3].Create(ViewportRect.Right-1, ViewportRect.Top-1);
    Canvas.Polyline(Points, 0, 4);

    Canvas.Pen.Color := Shade(0.10, Color1);
    SetLength(Points, 4);
    Points[0].Create(ViewportRect.Left+1, ViewportRect.Bottom-2);
    Points[1].Create(ViewportRect.Left+1, ViewportRect.Top);
    Points[2].Create(ViewportRect.Right-2, ViewportRect.Top);
    Points[3].Create(ViewportRect.Right-2, ViewportRect.Bottom-2);
    Canvas.Polyline(Points, 0, 4);
  end else
  begin
    if DragTarget then
    begin
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := Shade(0.25, ColorToRGB(OwnerListview.HotTrackColor));
      Canvas.Brush.Color := OwnerListview.HotTrackColor;
      Canvas.Rectangle(ViewportRect);
    end else
    if Selected then
    begin
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := Shade(0.25, ColorToRGB(OwnerListview.SelectedHilightColor));
      Canvas.Brush.Color := OwnerListview.SelectedHilightColor;
      Canvas.Rectangle(ViewportRect);
    end else
    begin
      Canvas.Brush.Color := OwnerListview.BkGndColor;
      Canvas.FillRect(ViewportRect);
    end;
  end;

  if CaptionPropertiesValid then
  begin
    // Turn the Text Boxes into Viewport Coordinates
    for i := 0 to Length(TextRects) - 1 do
      OffsetRect(TextRects[i], ViewportOffsetX, ViewportOffsetY);

    Canvas.Font.Assign(OwnerListview.Font);
    Canvas.TextRect(TextRects[0], TextRects[0].Left, TextRects[0].Top, Captions[0]);

    Canvas.Font.Assign(OwnerListview.DetailsFont);
    i := 1;
    while (i < OwnerListview.CaptionLineCount) and (i < Captions.Count) do
    begin
      Canvas.TextRect(TextRects[i], TextRects[i].Left, TextRects[i].Top, Captions[i]);
      Inc(i);
    end;

    // Turn the Text Boxes into Client Coordinates
    for i := 0 to Length(TextRects) - 1 do
      OffsetRect(TextRects[i], -ViewportOffsetX, -ViewportOffsetY);

    if OwnerListview.ShowFocus and Focused then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psDot;
      Canvas.Pen.Color := clHighlightText;
      Canvas.Rectangle(TextBoundsRect.Left-1 + ViewportOffsetX, TextBoundsRect.Top + ViewportOffsetY, TextBoundsRect.Right+1 + ViewportOffsetX, TextBoundsRect.Bottom + ViewportOffsetY);
      Canvas.Pen.Style := psSolid; // StaveState won't save this
    end;
  end;

  if ExpandablePropertiesValid and AllowExpand then
  begin
    if Expanded then
      OwnerListview.ExpandImages.Draw(Canvas, ExpandImageRect.Left+ViewportOffsetX, ExpandImageRect.Top+ViewportOffsetY, OwnerListview.ExpandIndexExpanded)
    else
      OwnerListview.ExpandImages.Draw(Canvas, ExpandImageRect.Left+ViewportOffsetX, ExpandImageRect.Top+ViewportOffsetY, OwnerListview.ExpandIndexCollapsed);
  end;

  if StateImagePropertiesValid and (StateImageIndex > -1) then
    OwnerListview.StateImages.Draw(Canvas, StateImageRect.Left+ViewportOffsetX, StateImageRect.Top+ViewportOffsetY, StateImageIndex);

  if ImagePropertiesValid and (ImageIndex > -1) then
    OwnerListview.Images.Draw(Canvas, ImageRect.Left+ViewportOffsetX, ImageRect.Top+ViewportOffsetY, ImageIndex); ;

end;

function TVirtualListviewItem.PtInExpandZone(ClientPoint: TPoint): Boolean;
begin
  // Not valid until Paint is called for the first time
  // ExpandImageRect Rect is in coordinates relative to the Item (0, 0) point
  Result := PtInRect(FExpandImageRect, Point(OwnerListview.HorzScrollbar.Position + (ClientPoint.X - ClientRect.Left),
                                             OwnerListview.VertScrollbar.Position + (ClientPoint.Y - ClientRect.Top)))
end;

function TVirtualListviewItem.PtInImage(ClientPoint: TPoint): Boolean;
begin
  // Not valid until Paint is called for the first time
  // Image Rect is in coordinates relative to the Item (0, 0) point
  Result := PtInRect(FImageRect, Point(OwnerListview.HorzScrollbar.Position + (ClientPoint.X - ClientRect.Left),
                                       OwnerListview.VertScrollbar.Position + (ClientPoint.Y - ClientRect.Top)))
end;

function TVirtualListviewItem.PtInItem(ClientPoint: TPoint): Boolean;
begin
  // Client Rect is in Viewpoint coordinates relative to the Window (0, 0) point
  Result := PtInRect(FClientRect, Point(OwnerListview.HorzScrollbar.Position + ClientPoint.X,
                                        OwnerListview.VertScrollbar.Position + ClientPoint.Y))
end;

function TVirtualListviewItem.PtInStateImage(ClientPoint: TPoint): Boolean;
begin
  // Not valid until Paint is called for the first time
  // StateImage Rect is in coordinates relative to the Item (0, 0) point
  Result := PtInRect(FStateImageRect, Point(OwnerListview.HorzScrollbar.Position + (ClientPoint.X - ClientRect.Left),
                                       OwnerListview.VertScrollbar.Position + (ClientPoint.Y - ClientRect.Top)))
end;

procedure TVirtualListviewItem.SelectAllChildren(IncludeSubChildren: Boolean);
var
  i: Integer;
  Item: TVirtualListviewItem;
begin
  OwnerListview.BeginUpdate;
  for i := 0 to ChildItems.Count - 1 do
  begin
    Item := ChildItems[i];
    Item.Selected := True;
    Item.Focused := False;
    if IncludeSubChildren then
      Item.SelectAllChildren(IncludeSubChildren);
  end;
  OwnerListview.EndUpdate;
end;

procedure TVirtualListviewItem.SetCaptions(AValue: TStringList);
begin
  FCaptions.Assign(AValue);
  Invalidate(True);  // redraw needed
end;

procedure TVirtualListviewItem.SetClientRect(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FClientRect.Left := ALeft;
  FClientRect.Top := ATop;
  FClientRect.Right := ALeft + AWidth;
  FClientRect.Bottom := ATop + AHeight;
end;

procedure TVirtualListviewItem.SetClientWithChildrenRect(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FClientWithChildrenRect.Left := ALeft;
  FClientWithChildrenRect.Top := ATop;
  FClientWithChildrenRect.Right := ALeft + AWidth;
  FClientWithChildrenRect.Bottom := ATop + AHeight;
end;

function TVirtualListviewItem.GetBottom: Integer;
begin
  Result := ClientRect.Bottom;
end;

function TVirtualListviewItem.GetHeight: Integer;
begin
  Result := ClientRect.Bottom - ClientRect.Top;
end;

function TVirtualListviewItem.GetWidth: Integer;
begin
  Result := ClientRect.Right - ClientRect.Left;
end;

function TVirtualListviewItem.ImagePropertiesValid: Boolean;
begin
  Result := Assigned(OwnerListview.Images)
end;

procedure TVirtualListviewItem.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  Invalidate(True); // redraw needed
end;

procedure TVirtualListviewItem.SetDragTarget(AValue: Boolean);
begin
  if FDragTarget =AValue then Exit;
  FDragTarget :=AValue;
  Invalidate(True);
end;

procedure TVirtualListviewItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled =AValue then Exit;
  FEnabled :=AValue;
  Invalidate(True); // redraw needed
end;

procedure TVirtualListviewItem.SetAllowExpand(AValue: Boolean);
begin
  if AValue then
  begin
    FAllowExpand := True;
    OwnerListview.BeginUpdate;
    OwnerListview.EndUpdate;
  end else
  begin
    FAllowExpand := False;
    FExpanded := False;
    OwnerListview.BeginUpdate;
    OwnerListview.EndUpdate;
  end;
end;

procedure TVirtualListviewItem.SetAllowFocus(AValue: Boolean);
begin
  if AValue then
  begin
    FAllowFocus := True;
    Invalidate(True);
  end else
  begin
    FAllowFocus := False;
    FFocused := False;
    Invalidate(True);
  end;
end;

procedure TVirtualListviewItem.SetAllowSelect(AValue: Boolean);
begin
  if AValue then
  begin
    FAllowSelect := True;
    Invalidate(True);
  end else
  begin
    FAllowSelect := False;
    FSelected := False;
    Invalidate(True);
  end;
end;

procedure TVirtualListviewItem.SetExpanded(AValue: Boolean);
begin
  if AllowExpand then
  begin
    if FExpanded = AValue then Exit;
    FExpanded := AValue;
    if not Expanded then
      UnSelectAllChildren(True);
    OwnerListview.BeginUpdate;
    OwnerListview.EndUpdate; // rebuild needed
  end else
  begin
    if Expanded then
    begin
      FExpanded := False;
      OwnerListview.BeginUpdate;
      OwnerListview.EndUpdate; // rebuild needed
    end;
  end;
end;

procedure TVirtualListviewItem.SetFocused(AValue: Boolean);
begin
  if AllowFocus and OwnerListview.ShowFocus then
  begin
    if (FFocused = AValue) or not Enabled then Exit;
    FFocused := AValue;
    if Assigned(OwnerListview) then
    begin
      if AValue then
      begin
        if Assigned(OwnerListview.FocusedItem) and (OwnerListview.FocusedItem <> Self) then
          OwnerListview.FocusedItem.Focused := False;
        OwnerListview.FocusedItem := Self;
        if not OwnerListview.IsInCurrentView(Self) then
          OwnerListview.ScrollIntoView(Self);
      end;
    end;
    Invalidate(True);  // Redraw needed
  end else
  begin
    if Focused then
    begin
      FFocused := False;
      Invalidate(True);
    end;
  end;
end;

procedure TVirtualListviewItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  Invalidate(True);   // Redraw needed
end;

procedure TVirtualListviewItem.SetSelected(AValue: Boolean);
begin
  if AllowSelect then
  begin
    if FSelected = AValue then Exit;
    FSelected := AValue;
    Invalidate(True);   // Redraw needed
    OwnerListview.SelectChangedNotify(Self);
  end else
  begin
    if Selected then
    begin
      FSelected := False;
      Invalidate(True);   // Redraw needed
      OwnerListview.SelectChangedNotify(Self);
    end;
  end;
end;

procedure TVirtualListviewItem.SetStateImageIndex(AValue: Integer);
begin
  if FStateImageIndex = AValue then Exit;
  FStateImageIndex := AValue;
  Invalidate(True);   // Redraw needed
end;

procedure TVirtualListviewItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
  OwnerListview.BeginUpdate;   // Rebuild needed
  OwnerListview.EndUpdate;
end;

function TVirtualListviewItem.StateImagePropertiesValid: Boolean;
begin
  Result := Assigned(OwnerListview.StateImages);
end;

procedure TVirtualListviewItem.UnSelectAllChildren(IncludeSubChildren: Boolean);
var
  i: Integer;
  Item: TVirtualListviewItem;
begin
  OwnerListview.BeginUpdate;
  for i := 0 to ChildItems.Count - 1 do
  begin
    Item := ChildItems[i];
    Item.Selected := False;
    Item.Focused := False;
    if IncludeSubChildren then
      Item.UnSelectAllChildren(IncludeSubChildren);
  end;
  OwnerListview.EndUpdate;
end;

{ TCustomVirtualListview }

procedure TCustomVirtualListview.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TCustomVirtualListview.DoOnFocusChanged(Item, OldItem: TVirtualListviewItem);
begin
  if Assigned(OnFocusChanged) then
    OnFocusChanged(Self, Item, OldItem);
end;

function TCustomVirtualListview.ClientPtToCurrentViewPt(APt: TPoint): TPoint;
begin
  Result.X := APt.X + HorzScrollBar.Position;
  Result.Y := APt.Y + VertScrollBar.Position;
end;

function TCustomVirtualListview.ClientPtToVisibleItem(ClientPt: TPoint; CurrentViewOnly: Boolean): TVirtualListviewItem;
var
  Item: TVirtualListviewItem;
begin
  Result := nil;
  // If CurrentViewOnly is true then only look at the items that the users can currently see
  // in the window.  If false then run the entire list of items that have their Visible property set to true
  // which could be out of sight of the user
  if CurrentViewOnly then
    Item := FirstInView
  else
    Item := FirstVisible;

  while Assigned(Item) and not Assigned(Result) do
  begin
    if Item.PtInItem(ClientPt) then
    begin
      Result := Item;
      Break
    end else
    begin
      Item := NextVisible(Item);
      if CurrentViewOnly then
      begin
        if not IsInCurrentView(Item) then
        Item := nil
      end
    end;
  end;
end;

function TCustomVirtualListview.ClientRectToCurrentViewPt(ARect: TRect): TRect;
begin
  Result.TopLeft := ClientPtToCurrentViewPt(ARect.TopLeft);
  Result.BottomRight := ClientPtToCurrentViewPt(ARect.BottomRight);
end;

procedure TCustomVirtualListview.CollapseAll(IncludeSubChildren: Boolean);
var
  i: Integer;
  Item: TVirtualListviewItem;
begin
  BeginUpdate;
  for i := 0 to Items.Count - 1 do
  begin
    Item := Items[i];
    Item.Expanded := False;
    if IncludeSubChildren then
      Item.CollapseAllChildren(IncludeSubChildren);
  end;
  EndUpdate;
end;

constructor TCustomVirtualListview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TVirtualListviewItemList.Create(nil);
  FVisibleItems := TVirtualListviewItemContainer.Create(nil);
  FSelectedItems := TVirtualListviewItemContainer.Create(nil);
  FMouseController := TMustangpeakMouseController.Create;
  VisibleItems.FOwnerListview := Self;
  FItems.FOwnerListview := Self;
  FDefaultItemHeight := 44;
  FDetailsFont := TFont.Create;
  DetailsFont.Assign(Font);
  DetailsFont.Color := clGray;
  FDetailsIndent := 8;
  FCaptionIndent := 4;
  FCaptionLineCount := 1;
  FLevelIndent := 16;
  Color := clWindow;
  FVertScrollbar := TScrollBar.Create(Self);
  VertScrollbar.Visible := False;
  VertScrollbar.Align := alRight;
  VertScrollbar.Parent := Self;
  VertScrollbar.Kind := sbVertical;
  VertScrollbar.OnScroll := @OnScrollVertical;

  FHorzScrollbar := TScrollBar.Create(Self);
  HorzScrollbar.Visible := False;
  HorzScrollbar.Align := alBottom;
  HorzScrollbar.Parent := Self;
  HorzScrollbar.Kind := sbHorizontal;
  HorzScrollbar.OnScroll := @OnScrollHorizontal;

  Font.OnChange := @OnFontChange;
  DetailsFont.OnChange := @OnFontChange;

  FBkGndGradient1 := clBtnFace;
  FBkGndGradient2 := Shade(0.25, ColorToRGB(clBtnFace));
  FBkGndColor := clBtnFace;

  FSelectedHilightColor := clHighlight;
  FSelectedGradientHilightColor1 := clHighlight;
  FSelectedGradientHilightColor2 := Shade(0.25, ColorToRGB(clHighlight));

  FHotTrackColor := clHighlight;
  FHotTrackGradientColor1 := clHighlight;
  FHotTrackGradientColor2 := Shade(0.25, ColorToRGB(clHighlight));
end;

procedure TCustomVirtualListview.BeforeDragStart;
begin
  inherited BeforeDragStart;
end;

destructor TCustomVirtualListview.Destroy;
begin
  FreeAndNil(FVisibleItems);
  FreeAndNil(FItems);
  FreeAndNil(FDetailsFont);
  FreeAndNil(FMouseController);
  inherited Destroy;
end;

procedure TCustomVirtualListview.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  inherited DoContextPopup(MousePos, Handled);
end;

function TCustomVirtualListview.DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint; ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean): LRESULT;

    // Just like the LCL does it....
    function GetDragObject: TObject; inline;
    begin
      if ADragObject.AutoCreated then
        Result := ADragObject.Control
      else
        Result := ADragObject;
    end;

var
  HitItem: TVirtualListviewItem;
  ClientPt: TPoint;
  Accepts: Boolean;
begin
  Result := inherited DoDragMsg(ADragMessage, APosition, ADragObject, ATarget, ADocking);

  ClientPt := ScreenToClient(APosition);

  HitItem := ClientPtToVisibleItem(ClientPt, True);
  case ADragMessage of
     dmDragEnter :
       begin
         if HitItem <> DragTargetItem then
         begin
           if Assigned(DragTargetItem) then
             DragTargetItem.DragLeave;
           if Assigned(HitItem) then
             HitItem.DragEnter;
           FDragTargetItem := HitItem;
         end;
       end;
     dmDragLeave :
       begin
         if Assigned(DragTargetItem) then
           DragTargetItem.DragLeave;
         FDragTargetItem := nil;
       end;
     dmDragMove :
       begin
         if HitItem <> DragTargetItem then
         begin
           if Assigned(DragTargetItem) then
             DragTargetItem.DragLeave;
           if Assigned(HitItem) then
             HitItem.DragEnter;
           FDragTargetItem := HitItem;
         end;
         Accepts := True;
         if Assigned(DragTargetItem) then
           DragTargetItem.DragOver(GetDragObject, ClientPt.X, ClientPt.Y, TDragState(ADragMessage), Accepts);
         Result := Ord(Accepts);
       end;
     dmDragDrop :
       begin
       end;
     dmDragCancel :
       begin
         if HitItem <> DragTargetItem then
         begin
           if Assigned(DragTargetItem) then
             DragTargetItem.DragLeave;
           if Assigned(HitItem) then
             HitItem.DragEnter;
         end;
       end;
     dmFindTarget :
       begin
       end;
  end;
end;

procedure TCustomVirtualListview.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited DoEndDrag(Target, X, Y);
end;

procedure TCustomVirtualListview.DoOnResize;
begin
  inherited DoOnResize;
   RebuildItems(nil, 0)
end;

procedure TCustomVirtualListview.DoOnSelectedChanged;
begin
  if [vlis_SelectionLockUpdate] * InternalState = [] then
  begin
    if Assigned(OnSelectedChanged) then
      OnSelectedChanged(Self);
  end;
end;

procedure TCustomVirtualListview.DoOnShowHint(HintInfo: PHintInfo);
var
  LccItem: TVirtualListviewItem;
begin
  inherited DoOnShowHint(HintInfo);
  LccItem := ClientPtToVisibleItem(HintInfo^.CursorPos, True);
  if Assigned(LccItem) then
  begin
//    HintInfo^.HintStr := 'ItemID: ' + LccItem.ItemIDStr;
//    if LccItem.AliasID <> 0 then
 //     HintInfo^.HintStr := HintInfo^.HintStr + #13+#10 + 'AliasID: 0x' + IntToHex(LccItem.AliasID, 4);
  end;
end;

function TCustomVirtualListview.DoOnSort(Item1, Item2: TVirtualListviewItem): Integer;
begin
  Result := 0;
  if Assigned(OnSort) then
    Result := OnSort(Self, Item1, Item2);
end;

function TCustomVirtualListview.GetDragImages: TDragImageList;
begin
  Result :=inherited GetDragImages;
end;

function TCustomVirtualListview.GetSelectedCount: Integer;
begin
  Result := SelectedItems.Count;
end;

procedure TCustomVirtualListview.DoStartDrag(var DragObject: TDragObject);
begin
  inherited DoStartDrag(DragObject);
end;

procedure TCustomVirtualListview.DragCanceled;
begin
  inherited DragCanceled;
end;

procedure TCustomVirtualListview.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
end;

procedure TCustomVirtualListview.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  // If not dragging then test to see if we should be
  if mcs_DragItem in MouseController.State then
  begin
     // This is all handed in the DoDragMsg method
    MouseController.FMouseLastPt.X := X;       // Track them for the fun of it
    MouseController.FMouseLastPt.Y := Y;
  end else
  if mcs_DragRect in MouseController.State then
  begin
    MouseController.FMouseLastPt.X := X;
    MouseController.FMouseLastPt.Y := Y;
    Invalidate;    // The actual change to what is shown on the screen is painted through the Paint method and MouseController.Paint
  end else
  begin
    if [mcs_DragItemsPending, mcs_DragRectPending] * MouseController.State <> [] then  // Detect if we are really starting any Drag yet
    begin
      if ((Abs(X - MouseController.MouseDownPt.X) > MouseController.DeltaDragDetect)) or (Abs(Y - MouseController.MouseDownPt.Y) > MouseController.DeltaDragDetect) then
      begin
        if [mcs_DragRectPending] * MouseController.State <> [] then
          Include(MouseController.FState, mcs_DragRect)
        else
        if [mcs_DragItemsPending] * MouseController.State <> [] then
        begin
          Include(MouseController.FState, mcs_DragItem);
          BeginDrag(True);
        end;
      end;
    end;
  end;
end;

procedure TCustomVirtualListview.EndUpdate;
begin
  Dec(FUpdateLock);
  if FUpdateLock < 1 then
  begin
    FUpdateLock := 0;
    RebuildItems(nil, 0);
    Invalidate;
    Update
  end;
end;

procedure TCustomVirtualListview.ExpandAll(IncludeSubChildren: Boolean);
var
  i: Integer;
  Item: TVirtualListviewItem;
begin
  BeginUpdate;
  for i := 0 to Items.Count - 1 do
  begin
    Item := Items[i];
    Item.Expanded := True;
    if IncludeSubChildren then
      Item.ExpandAllChildren(IncludeSubChildren);
  end;
  EndUpdate;
end;

function TCustomVirtualListview.First: TVirtualListviewItem;
begin
  Result := nil;
  if Items.Count > 0 then
    Result := Items[0];
end;

function TCustomVirtualListview.FirstInView: TVirtualListviewItem;
var
  Index: Integer;
begin
  Result := nil;
  if VisibleItems.Count > 0 then
  begin
    Index := (VertScrollBar.Position div DefaultItemHeight);
    if Index >= VisibleItems.Count then
      Index := VisibleItems.Count - 1;
    Result := VisibleItems[Index];
  end;
end;

function TCustomVirtualListview.FirstVisible: TVirtualListviewItem;
begin
  Result := nil;
  if VisibleItems.Count > 0 then
    Result := VisibleItems[0];
end;

function TCustomVirtualListview.IsInCurrentView(Item: TVirtualListviewItem): Boolean;
var
  ResultRect, AViewRect: TRect;
begin
  Result := False;
  if Assigned(Item) then
  begin
    AViewRect := Rect(0, 0, 0, 0);
    ResultRect := Rect(0, 0, 0, 0);
    CurrentViewRect(AViewRect);
    IntersectRect(ResultRect, AViewRect, Item.ClientRect);
    Result := not IsRectEmpty(ResultRect)
  end;
end;

procedure TCustomVirtualListview.ItemNotificationSelectionChanged(Item: TVirtualListviewItem);
begin
  if Item.Selected then
    SelectedItems.Add(Item)
  else
    SelectedItems.Remove(Item);
end;

function TCustomVirtualListview.Last: TVirtualListviewItem;
begin
  Result := nil;
  if Items.Count > 0 then
    Result := Items[Items.Count - 1];
end;

function TCustomVirtualListview.LastVisible: TVirtualListviewItem;
begin
  Result := nil;
  if VisibleItems.Count > 0 then
    Result := VisibleItems[VisibleItems.Count - 1];
end;

procedure TCustomVirtualListview.ScrollIntoView(Item: TVirtualListviewItem);
begin
  VertScrollBar.Position := Item.ClientRect.Top;
end;

procedure TCustomVirtualListview.SelectAll(IncludeSubChildren: Boolean);
var
  i: Integer;
  Item: TVirtualListviewItem;
begin
  BeginUpdate;
  Include(FInternalState, vlis_SelectionMultiSelect);
  Include(FInternalState, vlis_SelectionLockUpdate);
  for i := 0 to Items.Count - 1 do
  begin
    Item := Items[i];
    Item.Selected := True;
    if IncludeSubChildren then
      Item.SelectAllChildren(IncludeSubChildren);
  end;
  Exclude(FInternalState, vlis_SelectionMultiSelect);
  Exclude(FInternalState, vlis_SelectionLockUpdate);
  DoOnSelectedChanged;
  EndUpdate;
end;

procedure TCustomVirtualListview.SelectChangedNotify(Item: TVirtualListviewItem);
var
  Old: TVirtualListviewItem;
  i: Integer;
begin
  if Assigned(SelectedItem) and (Item.Selected) then  // This should stop recursion
  begin
    Old := SelectedItem;
    FSelectedItem := nil;
    if Assigned(Old) and ([vlis_SelectionMultiSelect] * InternalState = []) then
    begin
      Old.Selected := False;
      SelectedItems.Remove(Old);
    end;
  end;
  if Item.Selected then
  begin
    FSelectedItem := Item;
    SelectedItems.Add(Item);
  end else
  begin
    if SelectedItem = Item then
    begin
      if SelectedItems.Count > 1 then
      begin
        i := SelectedItems.ItemList.IndexOf(Item);
        if i > 0 then
          FSelectedItem := SelectedItems.ItemList[i-1] as TVirtualListviewItem    // Select item before
        else
          FSelectedItem := SelectedItems.ItemList[i+1] as TVirtualListviewItem;    // Select item after
        SelectedItems.ItemList.Delete(i);
      end else
      begin
        SelectedItems.Remove(Item);
        FSelectedItem := nil;
      end;
    end;
    SelectedItems.Remove(Item);
  end;
  DoOnSelectedChanged;
end;

procedure TCustomVirtualListview.SetBkGndGradient2(AValue: TColor);
begin
  if FBkGndGradient2 = AValue then Exit;
  FBkGndGradient2 := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetBkGkdGradientDirection(
  AValue: TGradientDirection);
begin
  if FBkGkdGradientDirection = AValue then Exit;
  FBkGkdGradientDirection := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetBkGndColor(AValue: TColor);
begin
  if FBkGndColor = AValue then Exit;
  FBkGndColor := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetBkGndGradient1(AValue: TColor);
begin
  if FBkGndGradient1 = AValue then Exit;
  FBkGndGradient1 := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetBkGndGradientEnable(AValue: Boolean);
begin
  if FBkGndGradientEnable = AValue then Exit;
  FBkGndGradientEnable := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TVirtualListviewItem;
  CtrlSelectStartIndex, CtrlSelectEndIndex, i: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetFocus;
  MouseCapture := True;

  Include(MouseController.FState, mcs_Captured);

  if ssModifier in Shift then
    Include(MouseController.FState, mcs_Meta);        // Apple Key and ?? Windows Key? ??
  if ssShift in Shift then
    Include(MouseController.FState, mcs_ShiftDown);
  if ssAlt in Shift then
    Include(MouseController.FState, mcs_AltDown);
  if ssCtrl in Shift then
    Include(MouseController.FState, mcs_CtrlDown);
  if ssLeft in Shift then
    Include(MouseController.FState, mcs_LeftButton);
  if ssRight in Shift then
    Include(MouseController.FState, mcs_RightButton);
  if ssMiddle in Shift then
    Include(MouseController.FState, mcs_MiddleButton);
  MouseController.FMouseDownPt.X := X;
  MouseController.FMouseDownPt.Y := Y;
  MouseController.FMouseLastPt.X := X;
  MouseController.FMouseLastPt.Y := Y;

  Item := ClientPtToVisibleItem(Point(X, Y), True);
  if Assigned(Item) then
  begin
    if Item.Enabled then
    begin
      if Item.AllowExpand and Item.PtInExpandZone(Point(X, Y)) then
      begin
        Item.Expanded := not Item.Expanded;
        Exit;   // Do nothing else, leave
      end; // Item Expand Hit

      if [mcs_LeftButton] * MouseController.State <> [] then       // Left click actions
      begin
        // If it can be focused then focus it only if using the left button
        if Item.AllowFocus  then
          FocusedItem := Item;

        if Item.AllowSelect then
        begin
          BeginUpdate;
          try
            if mcs_ShiftDown in MouseController.State then
            begin
              Include(FInternalState, vlis_SelectionMultiSelect);
              Include(FInternalState, vlis_SelectionLockUpdate);

              if Assigned(SelectedItem) then
                CtrlSelectStartIndex := SelectedItem.VisibleIndex
              else begin
                if VisibleItems.Count > 0 then
                  CtrlSelectStartIndex := 0;
              end;

              if VisibleItems.Count > 1 then
              begin
                CtrlSelectEndIndex := Item.VisibleIndex;

                if CtrlSelectEndIndex > CtrlSelectStartIndex then
                begin
                  if VisibleItems.Items[CtrlSelectStartIndex+1].Selected then
                  begin // Selecting back over the existing selection
                    i := CtrlSelectStartIndex;
                    while i <= CtrlSelectEndIndex do
                    begin
                      if VisibleItems.Items[i+1].Selected then
                      begin
                        if i < CtrlSelectEndIndex then // May have clicking in the middle of the selection
                           VisibleItems.Items[i].Selected := not VisibleItems.Items[i].Selected;
                        Inc(i);
                      end else
                      begin
                        Inc(i);  // Skip over the Pivot
                        while i <= CtrlSelectEndIndex do
                        begin
                           VisibleItems.Items[i].Selected := not VisibleItems.Items[i].Selected;
                           Inc(i)
                        end;
                      end;
                    end;
                  end else
                  begin   // Normal adding to the selection as a continuation
                    Inc(CtrlSelectStartIndex);
                    for i := CtrlSelectStartIndex to CtrlSelectEndIndex do
                      VisibleItems.Items[i].Selected := not VisibleItems.Items[i].Selected;
                  end;
                end else
                begin
                  if VisibleItems.Items[CtrlSelectStartIndex-1].Selected then
                  begin // Selecting back over the existing selection
                    i := CtrlSelectStartIndex;
                    while i >= CtrlSelectEndIndex do
                    begin
                      if VisibleItems.Items[i-1].Selected then
                      begin
                        if i > CtrlSelectEndIndex then // May have clicking in the middle of the selection
                           VisibleItems.Items[i].Selected := not VisibleItems.Items[i].Selected;
                        Dec(i);
                      end else
                      begin
                        Dec(i);  // Skip over the Pivot
                        while i >= CtrlSelectEndIndex do
                        begin
                           VisibleItems.Items[i].Selected := not VisibleItems.Items[i].Selected;
                           Dec(i)
                        end;
                      end;
                    end;
                  end else
                  begin   // Normal adding to the selection as a continuation
                    Dec(CtrlSelectStartIndex);
                    for i := CtrlSelectStartIndex downto CtrlSelectEndIndex do
                      VisibleItems.Items[i].Selected := not VisibleItems.Items[i].Selected;
                  end;
                end;
              end;
              Exclude(FInternalState, vlis_SelectionMultiSelect);
              Exclude(FInternalState, vlis_SelectionLockUpdate);
        //      FSelectedItem := OriginalSelectedItem;
              DoOnSelectedChanged;  // Locked update so we need to it
              Exit;    // Do nothing else, leave
            end;
            if mcs_Meta in MouseController.State then
            begin  // Add to the Selection
              Include(FInternalState, vlis_SelectionMultiSelect);
              Item.Selected := not Item.Selected;
              Exclude(FInternalState, vlis_SelectionMultiSelect);
              Exit;    // Do nothing else, leave
            end else
            begin  // Unselect all and this is the only selected item now
              UnSelectAll(True);
              Item.Selected := not Item.Selected;
              Exit;    // Do nothing else, leave
            end;
          finally
            Include(MouseController.FState, mcs_DragItemsPending);
            EndUpdate;
          end;
        end;

        {
        if Item.AllowSelect then
        begin
          if [mcs_Meta, mcs_MiddleButton] * MouseController.State <> [] then    // Unix maps Apple/LeftClick to MiddleClick // Windows maps Ctrl/Left click to Right Click.....
          begin
            Item.Selected := not Item.Selected;
            if SelectedItem = nil then
              SelectedItem := Item;
          end else
          if mcs_ShiftDown in MouseController.State then
          begin
            // TODO figure out how to select a range with Shift is down
            Item.Selected := not Item.Selected;
            if SelectedItem = nil then
              SelectedItem := Item;
          end else
          begin
            if [mcs_CtrlDown, mcs_RightButton] * MouseController.State = [] then   // OSX Ctrl Click = show context menu
            begin
              UnSelectAll(True);
              SelectedItem := Item;
            end;
          end;
        end   }


      end;   // Left Button down
    end;// Item Enabled
  end else // Item Assigned
  begin
    BeginUpdate;
    try
      UnSelectAll(True);
   //   SelectedItem := nil;
      FocusedItem := nil;
      Include(MouseController.FState, mcs_DragRectPending);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomVirtualListview.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  MouseCapture := False;
  MouseController.State := []; // Clear the state

  // Clear the rect
  Invalidate;
  Update
end;

procedure TCustomVirtualListview.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TCustomVirtualListview.MouseLeave;
begin
  inherited MouseLeave;
end;

function TCustomVirtualListview.Next(Item: TVirtualListviewItem): TVirtualListviewItem;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(Item) then
  begin
    i := Item.Index;
    Inc(i);
    if i < Items.ItemList.Count then
      Result := Items.Items[i]
  end;
end;

function TCustomVirtualListview.NextVisible(Item: TVirtualListviewItem): TVirtualListviewItem;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(Item) then
  begin
    i := Item.VisibleIndex;
    Inc(i);
    if i < VisibleItems.ItemList.Count then
      Result := VisibleItems.Items[i]
  end;
end;

procedure TCustomVirtualListview.Paint;
var
  Item: TVirtualListviewItem;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  // TODO:  Find first and last in Viewport
  Item := FirstInView;
  while Assigned(Item) do
  begin
    Canvas.SaveHandleState;
    Item.Paint(Canvas, -HorzScrollbar.Position, -VertScrollbar.Position);
    Canvas.RestoreHandleState;
    Item := NextVisible(Item);
    if not IsInCurrentView(Item) then
      Item := nil
  end;

  MouseController.Paint(Canvas, -HorzScrollbar.Position, -VertScrollbar.Position);
end;

function TCustomVirtualListview.Previous(Item: TVirtualListviewItem): TVirtualListviewItem;
var
  i: Integer;
begin
  i := Items.ItemList.IndexOf(Item);
  if (i > -1) then
  begin
    Dec(i);
    if i < 0 then
      i := Items.Count - 1;                   // Wrap
    Result := Items.Items[i]
  end;
end;

function TCustomVirtualListview.PreviousVisible(Item: TVirtualListviewItem): TVirtualListviewItem;
var
  i: Integer;
begin
  i := VisibleItems.ItemList.IndexOf(Item);
  if (i > -1) then
  begin
    Dec(i);
    if i < 0 then
      i := VisibleItems.Count - 1;                   // Wrap
    Result := VisibleItems.Items[i]
  end;
end;

procedure TCustomVirtualListview.ScrollDelta(X, Y: Integer);
var
  DoInvalidate: Boolean;
begin
  DoInvalidate := True;
  if Y > 0 then
  begin
    if VertScrollbar.Position + Y < 0 then
    begin
      DoInvalidate := VertScrollbar.Position <> 0;
      VertScrollbar.Position := 0;
    end else
      VertScrollbar.Position := VertScrollbar.Position - Abs(Y);
  end else
  if Y < 0 then
  begin
    if VertScrollbar.Position + Y > VertScrollbar.Max then
    begin
      DoInvalidate := VertScrollbar.Position <> VertScrollbar.Max;
      VertScrollbar.Position := VertScrollbar.Max
    end else
      VertScrollbar.Position := VertScrollbar.Position + Abs(Y);
  end else
    DoInvalidate := False;

  if DoInvalidate then
    Invalidate;

  DoInvalidate := True;
  if X > 0 then
  begin
    if HorzScrollbar.Position + X < 0 then
    begin
      DoInvalidate := HorzScrollbar.Position <> 0;
      HorzScrollbar.Position := 0;
    end else
      HorzScrollbar.Position := HorzScrollbar.Position - Abs(X);
  end else
  if X < 0 then
  begin
    if HorzScrollbar.Position + X > HorzScrollbar.Max then
    begin
      DoInvalidate := HorzScrollbar.Position <> HorzScrollbar.Max;
      HorzScrollbar.Position := HorzScrollbar.Max
    end else
      HorzScrollbar.Position := HorzScrollbar.Position + Abs(X);
  end else
    DoInvalidate := False;

  if DoInvalidate then
    Invalidate;
end;

procedure TCustomVirtualListview.RebuildItems(ItemList: TVirtualListviewItemList; ALevel: Integer);

  function RunItems(LocalItemList: TVirtualListviewItemList; Level: Integer; var VisibleIndexCounter, NextTop: Integer): TRect;
  var
    i: Integer;
    Item: TVirtualListviewItem;
  begin
    Result.Top := NextTop;
    Result.Bottom := NextTop;
    Result.Left := 0;
    Result.Right := ClientWidth;
    for i := 0 to LocalItemList.Count - 1 do
    begin
      Item := LocalItemList[i];

      Item.FIndex := i;
      if Item.Visible then
      begin
        Item.FVisibleIndex := VisibleIndexCounter;
        Inc(VisibleIndexCounter);
        VisibleItems.Add(Item);
        Item.SetClientRect(0, NextTop, ClientWidth, DefaultItemHeight);
        Item.FLevel := Level;
        UnionRect(Result, Item.ClientRect, Result);
        NextTop := Result.Bottom;

        if Item.ChildItems.Count > 0 then
        begin
          if Item.Expanded then
          begin
            UnionRect(Result, RunItems(Item.ChildItems, Level + 1, VisibleIndexCounter, NextTop), Result);
            Item.FClientWithChildrenRect := Result;
          end
        end;
      end;
    end;
  end;

var
  VisibleIndexCounter, NextTop: Integer;
begin
  if UpdateLock > 0 then
    Exit;
  FUpdateLock := 1;     // Force the Lock so we don't get recursion within this block
  if Assigned(VisibleItems) and Assigned(Items) then
  begin
    NextTop := 0;
    VisibleItems.Clear;
    VisibleIndexCounter := 0;
    if not Assigned(ItemList) then
      ItemList := Items;
    FViewportRect := RunItems(ItemList, 0, VisibleIndexCounter, NextTop);
    CalculateScrollbars;
    Invalidate;
    Update;
  end;
  FUpdateLock := 0;
end;

procedure TCustomVirtualListview.OnScrollVertical(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  Invalidate; // The component is not updated with the new information until after this returns so don't force an update yet
end;

procedure TCustomVirtualListview.OnScrollHorizontal(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  Invalidate; // The component is not updated with the new information until after this returns so don't force an update yet
end;

procedure TCustomVirtualListview.OnFontChange(Sender: TObject);
begin
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetTextLayout(AValue: TVirtualListviewTextLayout);
begin
  if FTextLayout = AValue then Exit;
  FTextLayout := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.UnSelectAll(IncludeSubChildren: Boolean);
var
  i: Integer;
  Item: TVirtualListviewItem;
begin
  BeginUpdate;
  Include(FInternalState, vlis_SelectionLockUpdate);
  for i := 0 to Items.Count - 1 do
  begin
    Item := Items[i];
    Item.Selected := False;
    if IncludeSubChildren then
      Item.UnSelectAllChildren(IncludeSubChildren);
  end;
  Exclude(FInternalState, vlis_SelectionLockUpdate);
  DoOnSelectedChanged;
  EndUpdate;
end;

procedure TCustomVirtualListview.WMContextMenu(var Message: TLMContextMenu);
begin
  inherited;
  MouseCapture := False;
  MouseController.State := []; // Clear the state
end;

procedure TCustomVirtualListview.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
 // beep;   Only called with Windows
end;

procedure TCustomVirtualListview.CurrentViewRect(var ARect: TRect);
begin
  ARect.Top := VertScrollBar.Position;
  ARect.Bottom := ARect.Top + ClientHeight;
  ARect.Left := HorzScrollBar.Position;
  ARect.Right := ARect.Left + ClientWidth;
end;

procedure TCustomVirtualListview.WMShowWindow(var Message: TLMShowWindow);
begin
  inherited WMShowWindow(Message);
end;

procedure TCustomVirtualListview.WMKeyDown(var Message: TLMKeyDown);
begin
  // Don't do inherited it will swallow keys
  case Message.CharCode of
    VK_DOWN :
      begin
        FocusedItem := NextVisible(FocusedItem);
        if not Assigned(FocusedItem) then
          FocusedItem := FirstVisible;
      end;
    VK_UP :
      begin
        FocusedItem := PreviousVisible(FocusedItem);
        if not Assigned(FocusedItem) then
          FocusedItem := FirstVisible;
      end;
    VK_HOME :
      begin
        FocusedItem := FirstVisible;
      end;
    VK_END :
      begin
        FocusedItem := LastVisible;
      end;
  end;
end;

procedure TCustomVirtualListview.WMLButtonDBLCLK(var Message: TLMLButtonDblClk);
var
  Item: TVirtualListviewItem;
begin
  Item := ClientPtToVisibleItem(Message.Pos, True);
  if Assigned(Item) then
  begin
    if Item.AllowExpand and Item.ExpandablePropertiesValid then
      Item.Expanded := not Item.Expanded;
  end;
end;

procedure TCustomVirtualListview.WMMButtonDBLCLK(var Message: TLMMButtonDblClk);
begin

end;

procedure TCustomVirtualListview.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
end;

procedure TCustomVirtualListview.WMMouseWheel(var Message: TLMMouseEvent);
begin
  if Message.WheelDelta <> 0 then
  begin
    if ssShift in Message.State then
      ScrollDelta(Message.WheelDelta, 0)
    else
      ScrollDelta(0, Message.WheelDelta);
  end;
end;

procedure TCustomVirtualListview.WMRButtonDBLCLK(var Message: TLMRButtonDblClk);
begin

end;

procedure TCustomVirtualListview.SetCaptionIndent(AValue: Integer);
begin
  if FCaptionIndent = AValue then Exit;
  FCaptionIndent := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetCaptionLineCount(AValue: Integer);
begin
  if FCaptionLineCount = AValue then Exit;
  FCaptionLineCount := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetDefaultItemHeight(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FDefaultItemHeight = AValue then Exit;
  FDefaultItemHeight := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetDetailsFont(AValue: TFont);
begin
  FDetailsFont.Assign(AValue);
end;

procedure TCustomVirtualListview.SetDetailsIndent(AValue: Integer);
begin
  if FDetailsIndent = AValue then Exit;
  FDetailsIndent := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetExpandableItems(AValue: Boolean);
begin
  if FExpandableItems = AValue then Exit;
  FExpandableItems := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetExpandImagePosition(
  AValue: TExpandImagePostition);
begin
  if FExpandImagePosition = AValue then Exit;
  FExpandImagePosition := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.CalculateScrollbars;
begin
  if ViewportRect.Bottom > ClientRect.Bottom then
  begin
    VertScrollbar.Max := ViewportRect.Bottom - ClientRect.Bottom;
    VertScrollbar.Visible := True;
  end else
  begin
    VertScrollbar.Visible := False;
    VertScrollbar.Max := 0;
  end;
  if ViewportRect.Right > ClientRect.Right then
  begin
    HorzScrollbar.Max := ViewportRect.Right - ClientRect.Right;
    HorzScrollbar.Visible := True;
  end else
  begin
    HorzScrollbar.Visible := False;
    HorzScrollbar.Max := 0;
  end;
end;

procedure TCustomVirtualListview.DoOnDebugEvent;
begin
  if Assigned(FOnDebugEvent) then
    OnDebugEvent(Self)
end;

procedure TCustomVirtualListview.SetFocusedItem(AValue: TVirtualListviewItem);
var
  OldFocused: TVirtualListviewItem;
begin
  if FFocusedItem = AValue then Exit;
  OldFocused := FocusedItem;
  FFocusedItem := AValue;
  DoOnFocusChanged(AValue, OldFocused);
  if Assigned(AValue) then
    AValue.Focused := True;
  if Assigned(OldFocused) then
    OldFocused.Focused := False;
end;

procedure TCustomVirtualListview.SetHotTrackColor(AValue: TColor);
begin
  if FHotTrackColor = AValue then Exit;
  FHotTrackColor := AValue;
end;

procedure TCustomVirtualListview.SetHotTrackGradientColor1(AValue: TColor);
begin
  if FHotTrackGradientColor1 = AValue then Exit;
  FHotTrackGradientColor1 := AValue;
end;

procedure TCustomVirtualListview.SetHotTrackGradientColor2(AValue: TColor);
begin
  if FHotTrackGradientColor2 = AValue then Exit;
  FHotTrackGradientColor2 := AValue;
end;

procedure TCustomVirtualListview.SetLevelIndent(AValue: Integer);
begin
  if FLevelIndent = AValue then Exit;
  BeginUpdate;
  FLevelIndent := AValue;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetSelectedGradientHilightColor1(AValue: TColor);
begin
  if FSelectedGradientHilightColor1 = AValue then Exit;
  FSelectedGradientHilightColor1 := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetSelectedGradientHilightColor2(AValue: TColor);
begin
  if FSelectedGradientHilightColor2 = AValue then Exit;
  FSelectedGradientHilightColor2 := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetSelectedHilightColor(AValue: TColor);
begin
  if FSelectedHilightColor = AValue then Exit;
  FSelectedHilightColor := AValue;
  BeginUpdate;
  EndUpdate;
end;

procedure TCustomVirtualListview.SetSelectedItem(AValue: TVirtualListviewItem);
begin
  if FSelectedItem = AValue then Exit;
  UnSelectAll(True);
  if Assigned(AValue) then
    AValue.Selected := True     // Eventually called the SelectionChangedNotify to set the SelectedItem
end;

procedure TCustomVirtualListview.SetShowFocus(AValue: Boolean);
begin
  if FShowFocus = AValue then Exit;
  FShowFocus := AValue;
  BeginUpdate;
  EndUpdate;
end;


{ TVirtualListviewItemList }

function TVirtualListviewItemList.Add(ACaption: string; ImageIndex: Integer = -1): TVirtualListviewItem;
begin
  OwnerListview.BeginUpdate;
  try
    Result := TVirtualListviewItem.Create(Self);
    Result.Captions.Add(ACaption);
    Result.ImageIndex := ImageIndex;
    Result.FOwnerListview := OwnerListview;
    Result.ChildItems.FOwnerListview := OwnerListview;
    ItemList.Add(Result);
    OwnerListview.RebuildItems(nil, 0);
  finally
    OwnerListview.EndUpdate;
  end;
end;

procedure TVirtualListviewItemList.Clear;
begin
  OwnerListview.BeginUpdate;
  try
    if Assigned(OwnerListview.VisibleItems) then
      OwnerListview.VisibleItems.Clear;
    if Assigned(ItemList) then
      ItemList.Clear;
  finally
    OwnerListview.EndUpdate;
  end;
end;

constructor TVirtualListviewItemList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemList := TObjectList.Create;
  ItemList.OwnsObjects := True;
end;

procedure TVirtualListviewItemList.Delete(Index: Integer);
begin
  ItemList.Delete(Index);
end;

function TVirtualListviewItemList.Remove(ACaption: string): TVirtualListviewItem;
var
  i: Integer;
begin
  i := FindIndex(ACaption);
  if i > -1 then
  begin
    Result := Items[i];
    ItemList.Delete(i);
  end;
end;

function TVirtualListviewItemList.Find(ACaption: string): TVirtualListviewItem;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while (i < ItemList.Count) and not Assigned(Result) do
  begin
    if Items[i].Captions.Count > 0 then
    begin
      if Items[i].Captions[0] = ACaption then
        Result := Items[i];
    end;
    Inc(i);
  end;
end;

function TVirtualListviewItemList.FindIndex(ACaption: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := 0;
  while (i < ItemList.Count) and (Result < 0) do
  begin
    if Items[i].Captions.Count > 0 then
    begin
      if Items[i].Captions[0] = ACaption then
        Result := i;
    end;
    Inc(i);
  end;
end;

procedure TVirtualListviewItemList.Sort;
var
  i, n, newn: Integer;
  Temp: TVirtualListviewItem;
begin
  if Assigned(OwnerListview) then
  begin
    OwnerListview.BeginUpdate;
    try
      n := Count - 1;
      repeat
       newn := 0;
       for i := 1 to n do
       begin
         if OwnerListview.DoOnSort(Items[i-1], Items[i]) > 0 then
         begin
           Temp := Items[i-1];
           Items[i-1] := Items[i];
           Items[i] := Temp;
           newn := i
         end;
       end;
       n := newn
      until n = 0;
    finally
      OwnerListview.EndUpdate;
    end;
  end;
end;

destructor TVirtualListviewItemList.Destroy;
begin
  Clear;
  FreeAndNil(FItemList);
  inherited Destroy;
end;

function TVirtualListviewItemList.GetCount: Integer;
begin
  Result := ItemList.Count;
end;

function TVirtualListviewItemList.GetItems(Index: Integer): TVirtualListviewItem;
begin
  Result := ItemList[Index] as TVirtualListviewItem
end;

procedure TVirtualListviewItemList.SetItems(Index: Integer; AValue: TVirtualListviewItem);
var
  Old: Boolean;
begin
  Old := ItemList.OwnsObjects;
  ItemList.OwnsObjects := False;
  ItemList[Index] := AValue;
  ItemList.OwnsObjects := Old;
end;

end.

