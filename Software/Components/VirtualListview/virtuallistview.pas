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
  TOnVirtualListviewSort = function(Sender: TObject; Item1, Item2: TVirtualListviewItem): Integer of object;

  type
    TExpandImagePostition = (eip_Left, eip_Right);

  { TVirtualListviewItem }

  TVirtualListviewItem = class(TComponent)
  private
    FBkGkdGradient2: TColor;
    FBkGndColor: TColor;
    FBkGndGradient1: TColor;
    FBkGndGradientEnable: Boolean;
    FCaptionRect: TRect;
    FCaptions: TStringList;
    FClientRect: TRect;
    FClientWithChildrenRect: TRect;
    FColor: TColor;
    FDetailsRect: TRect;
    FEnabled: Boolean;
    FExpandable: Boolean;
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
    procedure SetCaptions(AValue: TStringList);
    procedure SetColor(AValue: TColor);
    procedure SetEnabled(AValue: Boolean);
    procedure SetExpandable(AValue: Boolean);
    procedure SetExpanded(AValue: Boolean);
    procedure SetFocused(AValue: Boolean);
    procedure SetImageIndex(AValue: Integer);
    procedure SetSelected(AValue: Boolean);
    procedure SetStateImageIndex(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
  protected
    TextRects: array of TRect;
    property ExpandImageRect: TRect read FExpandImageRect write FExpandImageRect;
    property ImageRect: TRect read FImageRect write FImageRect;
    property StateImageRect: TRect read FStateImageRect write FStateImageRect;
    property CaptionRect: TRect read FCaptionRect write FCaptionRect;
    property DetailsRect: TRect read FDetailsRect write FDetailsRect;
    function CaptionPropertiesValid: Boolean;
    function ExpandablePropertiesValid: Boolean;
    function ImagePropertiesValid: Boolean;
    procedure CreateTextRects;
    procedure Paint(Canvas: TCanvas; OffsetX, OffsetY: Integer);
    procedure SetClientRect(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetClientWithChildrenRect(ALeft, ATop, AWidth, AHeight: Integer);
    function StateImagePropertiesValid: Boolean;
  public
    property Bottom: Integer read GetBottom;
    property Captions: TStringList read FCaptions write SetCaptions;
    property ChildItems: TVirtualListviewItemList read FChildItems write FChildItems;
    property ClientRect: TRect read FClientRect;
    property ClientWithChildrenRect: TRect read FClientWithChildrenRect;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Expandable: Boolean read FExpandable write SetExpandable;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property Focused: Boolean read FFocused write SetFocused;
    property Height: Integer read GetHeight;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Index: Integer read FIndex;
    property Level: Integer read FLevel;
    property OwnerListview: TCustomVirtualListview read FOwnerListview;
    property Selected: Boolean read FSelected write SetSelected;
    property StateImageIndex: Integer read FStateImageIndex write SetStateImageIndex;
    property Tag: Integer read FTag write FTag;
    property Visible: Boolean read FVisible write SetVisible;
    property VisibleIndex: Integer read FVisibleIndex;
    property Width: Integer read GetWidth;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Invalidate(Update: Boolean);
    function PtInExpandZone(ViewportPoint: TPoint): Boolean; virtual;
    function PtInImage(ViewportPoint: TPoint): Boolean; virtual;
    function PtInItem(ViewportPoint: TPoint): Boolean; virtual;
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

  { TVirtualListviewItemVisibleList }

  TVirtualListviewItemVisibleList = class(TComponent)
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
  end;

  TMouseControllerState = (mcs_Captured, mcs_Down, mcs_Dragging, mcs_DragRect, mcs_CtrlDown, mcs_AltDown, mcs_ShiftDown, mcs_LeftButton, mcs_RightButton, mcs_MiddleButton);
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


  { TCustomVirtualListview }

  TCustomVirtualListview = class(TCustomPanel)
  private
    FBkGkdGradient2: TColor;
    FBkGkdGradientDirection: TGradientDirection;
    FBkGndColor: TColor;
    FBkGndGradient1: TColor;
    FBkGndGradientEnable: Boolean;
    FExpandableItems: Boolean;
    FExpandImagePosition: TExpandImagePostition;
    FExpandImages: TImageList;
    FExpandIndexCollapsed: Integer;
    FExpandIndexExpanded: Integer;
    FFocusedItem: TVirtualListviewItem;
    FHorzScrollBar: TScrollBar;
    FImages: TImageList;
    FLevelIndent: Integer;
    FMouseController: TMustangpeakMouseController;
    FOnDebugEvent: TNotifyEvent;
    FOnSort: TOnVirtualListviewSort;
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
    FVisibleItems: TVirtualListviewItemVisibleList;
    FUpdateLock: Integer;
    OnFocusChanged: TOnVirtualListviewFocusedChanged;
    procedure SetBkGkdGradient2(AValue: TColor);
    procedure SetBkGkdGradientDirection(AValue: TGradientDirection);
    procedure SetBkGndColor(AValue: TColor);
    procedure SetBkGndGradient1(AValue: TColor);
    procedure SetBkGndGradientEnable(AValue: Boolean);
    procedure SetExpandableItems(AValue: Boolean);
    procedure SetExpandImagePosition(AValue: TExpandImagePostition);
    procedure SetFocusedItem(AValue: TVirtualListviewItem);
    procedure SetLevelIndent(AValue: Integer);
    procedure SetTextLayout(AValue: TVirtualListviewTextLayout);
    procedure SetCaptionIndent(AValue: Integer);
    procedure SetCaptionLineCount(AValue: Integer);
    procedure SetDefaultItemHeight(AValue: Integer);
    procedure SetDetailsFont(AValue: TFont);
    procedure SetDetailsIndent(AValue: Integer);
  protected
    property BkGndColor: TColor read FBkGndColor write SetBkGndColor;
    property BkGndGradientEnable: Boolean read FBkGndGradientEnable write SetBkGndGradientEnable;
    property BkGkdGradientDirection: TGradientDirection read FBkGkdGradientDirection write SetBkGkdGradientDirection;
    property BkGndGradient1: TColor read FBkGndGradient1 write SetBkGndGradient1;
    property BkGkdGradient2: TColor read FBkGkdGradient2 write SetBkGkdGradient2;
    property CaptionIndent: Integer read FCaptionIndent write SetCaptionIndent;
    property CaptionLineCount: Integer read FCaptionLineCount write SetCaptionLineCount;
    property DefaultItemHeight: Integer read FDefaultItemHeight write SetDefaultItemHeight;
    property DetailsIndent: Integer read FDetailsIndent write SetDetailsIndent;
    property DetailsFont: TFont read FDetailsFont write SetDetailsFont;
    property ExpandableItems: Boolean read FExpandableItems write SetExpandableItems;
    property ExpandImages: TImageList read FExpandImages write FExpandImages;
    property ExpandIndexExpanded: Integer read FExpandIndexExpanded write FExpandIndexExpanded;
    property ExpandIndexCollapsed: Integer read FExpandIndexCollapsed write FExpandIndexCollapsed;
    property ExpandImagePosition: TExpandImagePostition read FExpandImagePosition write SetExpandImagePosition;
    property FocusedItem: TVirtualListviewItem read FFocusedItem write SetFocusedItem;
    property Images: TImageList read FImages write FImages;
    property Items: TVirtualListviewItemList read FItems write FItems;
    property LevelIndent: Integer read FLevelIndent write SetLevelIndent;
    property MouseController: TMustangpeakMouseController read FMouseController write FMouseController;
    property VisibleItems: TVirtualListviewItemVisibleList read FVisibleItems write FVisibleItems;
    property OnDebugEvent: TNotifyEvent read FOnDebugEvent write FOnDebugEvent;
    property OnFocusedChanged: TOnVirtualListviewFocusedChanged read OnFocusChanged write OnFocusChanged;
    property OnSort: TOnVirtualListviewSort read FOnSort write FOnSort;
    property StateImages: TImageList read FStateImages write FStateImages;
    property TextLayout: TVirtualListviewTextLayout read FTextLayout write SetTextLayout;
    property UpdateLock: Integer read FUpdateLock write FUpdateLock;

    property VertScrollbar: TScrollBar read FVertScrollBar;
    property HorzScrollbar: TScrollBar read FHorzScrollBar;

    procedure CalculateScrollbars;
    procedure DoOnDebugEvent; virtual;
    procedure DoOnFocusChanged(LccItem, OldLccItem: TVirtualListviewItem); virtual;
    procedure DoOnResize; override;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
    function DoOnSort(Item1, Item2: TVirtualListviewItem): Integer; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMShowWindow(var Message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure Paint; override;
    procedure RebuildItems(ItemList: TVirtualListviewItemList; ALevel: Integer);
    procedure OnScrollVertical(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure OnScrollHorizontal(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure OnFontChange(Sender: TObject);
  public
    property ViewportRect: TRect read FViewportRect;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    function ClientRectToCurrentViewPt(ARect: TRect): TRect;
    function ClientPtToCurrentViewPt(APt: TPoint): TPoint;
    function ClientPtToVisibleItem(ClientPt: TPoint; CurrentViewOnly: Boolean): TVirtualListviewItem;
    procedure CurrentViewRect(var ARect: TRect);
    procedure EndUpdate;
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
    procedure ScrollDelta(X, Y: Integer);
    procedure ScrollIntoView(Item: TVirtualListviewItem);
  end;

  TVirtualListview = class(TCustomVirtualListview)
  public
    property FocusedItem;
    property Items;
    property VisibleItems;
  published
    property Align;
 //   property Alignment;
    property Anchors;
    property AutoSize;
    property BkGndColor;
    property BkGndGradientEnable;
    property BkGndGradient1;
    property BkGkdGradient2;
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
    property Images;
    property LevelIndent;
    property ParentBackground;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
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
    property OnSort;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property StateImages;
    property TextLayout;
    property VertScrollbar;
  end;


procedure StackRect(var ResultRect: TRect; Upper, Lower: TRect);
function HeightRect(ARect: TRect): Integer;
function WidthRect(ARect: TRect): Integer;

procedure Register;

implementation

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
 Canvas.DrawFocusRect(Rect(StartX, StartY, StopX, StopY));
end;

{ TVirtualListviewItemVisibleList }

procedure TVirtualListviewItemVisibleList.Add(Item: TVirtualListviewItem);
begin
  ItemList.Add(Item);
end;

procedure TVirtualListviewItemVisibleList.Clear;
begin
  if Assigned(ItemList) then
    ItemList.Clear;
end;

constructor TVirtualListviewItemVisibleList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemList := TObjectList.Create;
  ItemList.OwnsObjects := False;
end;

procedure TVirtualListviewItemVisibleList.Delete(Index: Integer);
begin
  ItemList.Delete(Index);
end;

destructor TVirtualListviewItemVisibleList.Destroy;
begin
  FreeAndNil(FItemList);
  inherited Destroy;
end;

function TVirtualListviewItemVisibleList.GetCount: Integer;
begin
  Result := ItemList.Count;
end;

function TVirtualListviewItemVisibleList.GetItems(Index: Integer): TVirtualListviewItem;
begin
  Result := ItemList[Index] as TVirtualListviewItem
end;

procedure TVirtualListviewItemVisibleList.SetItems(Index: Integer; AValue: TVirtualListviewItem);
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
end;

function TVirtualListviewItem.CaptionPropertiesValid: Boolean;
begin
  Result := (Captions.Count > 0) and (OwnerListview.CaptionLineCount > 0)
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

function TVirtualListviewItem.ExpandablePropertiesValid: Boolean;
begin
  Result := OwnerListview.ExpandableItems and Assigned(OwnerListview.ExpandImages) and (OwnerListview.ExpandIndexCollapsed > -1) and (OwnerListview.ExpandIndexExpanded > -1)
end;

procedure TVirtualListviewItem.Invalidate(Update: Boolean);
begin
  if Assigned(OwnerListview) then
  begin
    OwnerListview.Invalidate;
    if Update then
      OwnerListview.Update;
  end;
end;

procedure TVirtualListviewItem.Paint(Canvas: TCanvas; OffsetX, OffsetY: Integer);
var
  TextExtent: TSize;
  LeftDrawingEdge, RightDrawingEdge, DrawingW, ClientH, ImageX, ImageY, ImageW, ImageH,
  i, Indent, LocalOffsetX, LocalOffsetY, ViewPortOffsetX, ViewportOffsetY: Integer;
  ViewportRect, DrawingBoundsRect, TextBoundsRect: TRect;
begin
  // Assume no images
  DrawingBoundsRect := Rect(0, 0, 0, 0);
  FImageRect := Rect(0, 0, 0, 0);
  FStateImageRect := Rect(0, 0, 0, 0);

  LeftDrawingEdge := (OwnerListview.LevelIndent * Level);
  RightDrawingEdge := ClientRect.Right;
  if OwnerListview.VertScrollbar.Visible then
    Dec(RightDrawingEdge, OwnerListview.VertScrollbar.Width);

  ClientH := ClientRect.Bottom-ClientRect.Top;

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
  end else
    SetLength(TextRects, 0);

  // Now everyone is correctly positioned in the Vertical Direction

  // Create a local Bounds rect that is modified by the Scroll Postion
  ViewportRect := ClientRect;
  OffsetRect(ViewportRect, OffsetX, OffsetY);
  Canvas.ClipRect := ViewportRect;
  Canvas.Clipping := True;

  // Now paint the background
  if OwnerListview.BkGndGradientEnable then
  begin
    Canvas.GradientFill(ViewportRect, OwnerListview.BkGndGradient1, OwnerListview.BkGkdGradient2, OwnerListview.BkGkdGradientDirection);
  end else
  begin
    Canvas.Brush.Color := OwnerListview.BkGndColor;
    Canvas.FillRect(ViewportRect);
  end;

  // Special handling if it focused
  if Focused then
  begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clHighlightedText;
    Canvas.Brush.Color := clHighlight;
    Canvas.RoundRect(ViewportRect, 8, 8);
  end;

  if CaptionPropertiesValid then
  begin
    // Turn the Text Boxes into Viewport Coordinates
    for i := 0 to Length(TextRects) - 1 do
      OffsetRect(TextRects[i], ViewportOffsetX, ViewportOffsetY);

    Canvas.Font.Assign(OwnerListview.Font);
    if Focused then
      Canvas.Font.Color := clHighlightText;
    Canvas.TextRect(TextRects[0], TextRects[0].Left, TextRects[0].Top, Captions[0]);
    Canvas.Font.Assign(OwnerListview.DetailsFont);
    if Focused then
      Canvas.Font.Color := clHighlightText;
    i := 1;
    while (i < OwnerListview.CaptionLineCount) and (i < Captions.Count) do
    begin
      Canvas.TextRect(TextRects[i], TextRects[i].Left, TextRects[i].Top, Captions[i]);
      Inc(i);
    end;

    // Turn the Text Boxes into Client Coordinates
    for i := 0 to Length(TextRects) - 1 do
      OffsetRect(TextRects[i], -ViewportOffsetX, -ViewportOffsetY);
  end;

  if ExpandablePropertiesValid and Expandable then
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

function TVirtualListviewItem.PtInExpandZone(ViewportPoint: TPoint): Boolean;
begin
  // Not valid until Paint is called for the first time
  Result := False;
  if Expandable then
    Result := PtInRect(FExpandImageRect, ViewportPoint)
end;

function TVirtualListviewItem.PtInImage(ViewportPoint: TPoint): Boolean;
begin
  // Not valid until Paint is called for the first time
  Result := False;
  if (ImageIndex > -1) and Assigned(OwnerListview.Images) then
    Result := PtInRect(FImageRect, ViewPortPoint)
end;

function TVirtualListviewItem.PtInItem(ViewportPoint: TPoint): Boolean;
begin
  Result := PtInRect(ClientRect, ViewportPoint);
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

procedure TVirtualListviewItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled =AValue then Exit;
  FEnabled :=AValue;
  Invalidate(True); // redraw needed
end;

procedure TVirtualListviewItem.SetExpandable(AValue: Boolean);
begin
  if FExpandable = AValue then Exit;
  FExpandable := AValue;
  Invalidate(True);  // redraw needed
end;

procedure TVirtualListviewItem.SetExpanded(AValue: Boolean);
begin
  if FExpanded = AValue then Exit;
  FExpanded := AValue;
  OwnerListview.BeginUpdate;
  OwnerListview.EndUpdate; // rebuild needed
end;

procedure TVirtualListviewItem.SetFocused(AValue: Boolean);
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
end;

procedure TVirtualListviewItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  Invalidate(True);   // Redraw needed
end;

procedure TVirtualListviewItem.SetSelected(AValue: Boolean);
begin
  if FSelected = AValue then Exit;
  FSelected := AValue;
  Invalidate(True);   // Redraw needed
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

{ TCustomVirtualListview }

procedure TCustomVirtualListview.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TCustomVirtualListview.DoOnFocusChanged(LccItem, OldLccItem: TVirtualListviewItem);
begin
  if Assigned(OnFocusChanged) then
    OnFocusChanged(Self, LccItem, OldLccItem);
end;

function TCustomVirtualListview.ClientPtToCurrentViewPt(APt: TPoint): TPoint;
begin
  Result.X := APt.X + HorzScrollBar.Position;
  Result.Y := APt.Y + VertScrollBar.Position;
end;

function TCustomVirtualListview.ClientPtToVisibleItem(ClientPt: TPoint; CurrentViewOnly: Boolean): TVirtualListviewItem;
var
  ViewPt: TPoint;
  Item: TVirtualListviewItem;
begin
  Result := nil;
  // Convert the point that is in current Window coordinates to the Logical Viewport coordinates
  ViewPt := ClientPtToCurrentViewPt(ClientPt);

  // If CurrentViewOnly is true then only look at the items that the users can currently see
  // in the window.  If false then run the entire list of items that have their Visible property set to true
  // which could be out of sight of the user
  if CurrentViewOnly then
    Item := FirstInView
  else
    Item := FirstVisible;

  while Assigned(Item) and not Assigned(Result) do
  begin
    if Item.PtInItem(ViewPt) then
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

constructor TCustomVirtualListview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TVirtualListviewItemList.Create(nil);
  VisibleItems := TVirtualListviewItemVisibleList.Create(nil);
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
end;

destructor TCustomVirtualListview.Destroy;
begin
  FreeAndNil(FVisibleItems);
  FreeAndNil(FItems);
  FreeAndNil(FDetailsFont);
  FreeAndNil(FMouseController);
  inherited Destroy;
end;

procedure TCustomVirtualListview.DoOnResize;
begin
  inherited DoOnResize;
   RebuildItems(nil, 0)
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

procedure TCustomVirtualListview.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  // If not dragging then test to see if we should be
  if mcs_DragRect in MouseController.State then
  begin
    MouseController.FMouseLastPt.X := X;
    MouseController.FMouseLastPt.Y := Y;
    Invalidate;
 //   Update;
  end else
  begin
    if MouseController.AnyButtonDown then
    begin

      // TODO distinguish between drag rect and dragging an itme

      // Detect if we are really starting a Drag yet
      if Abs(X - MouseController.MouseDownPt.X) > MouseController.DeltaDragDetect then
        Include(MouseController.FState, mcs_DragRect);
      if Abs(Y - MouseController.MouseDownPt.Y) > MouseController.DeltaDragDetect then
        Include(MouseController.FState, mcs_DragRect);
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
    CurrentViewRect(AViewRect);
    IntersectRect(ResultRect, AViewRect, Item.ClientRect);
    Result := not IsRectEmpty(ResultRect)
  end;
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

procedure TCustomVirtualListview.SetBkGkdGradient2(AValue: TColor);
begin
  if FBkGkdGradient2 = AValue then Exit;
  FBkGkdGradient2 := AValue;
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
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetFocus;
  MouseCapture := True;

  Include(MouseController.FState, mcs_Captured);
  if ssShift in Shift then Include(MouseController.FState, mcs_ShiftDown);
  if ssAlt in Shift then Include(MouseController.FState, mcs_AltDown);
  if ssCtrl in Shift then Include(MouseController.FState, mcs_CtrlDown);
  if ssLeft in Shift then Include(MouseController.FState, mcs_LeftButton);
  if ssRight in Shift then Include(MouseController.FState, mcs_RightButton);
  if ssMiddle in Shift then Include(MouseController.FState, mcs_MiddleButton);
  MouseController.FMouseDownPt.X := X;
  MouseController.FMouseDownPt.Y := Y;
  MouseController.FMouseLastPt.X := X;
  MouseController.FMouseLastPt.Y := Y;

  Item := ClientPtToVisibleItem(Point(X, Y), True);
  if Assigned(Item) then
  begin
    if Item.Enabled then
      FocusedItem := Item
  end else
    FocusedItem := nil;
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
  IntersetRect: TRect;
begin
  Canvas.SaveHandleState;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  // TODO:  Find first and last in Viewport
  Item := FirstInView;
  while Assigned(Item) do
  begin
    Item.Paint(Canvas, -HorzScrollbar.Position, -VertScrollbar.Position);
    Item := NextVisible(Item);
    if not IsInCurrentView(Item) then
      Item := nil
  end;


  Canvas.RestoreHandleState;

  if mcs_DragRect in MouseController.State then
   begin
    MouseController.Paint(Canvas, -HorzScrollbar.Position, -VertScrollbar.Position);
  end;


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
          UnionRect(Result, RunItems(Item.ChildItems, Level + 1, VisibleIndexCounter, NextTop), Result);
          Item.FClientWithChildrenRect := Result;
        end;
      end;
    end;
  end;

var
  VisibleIndexCounter, NextTop: Integer;
begin
  if UpdateLock > 0 then Exit;
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

procedure TCustomVirtualListview.SetLevelIndent(AValue: Integer);
begin
  if FLevelIndent = AValue then Exit;
  BeginUpdate;
  FLevelIndent := AValue;
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

