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

  TOnVirtualListviewFocusedChanged = procedure(Sender: TObject; FocusedItem, OldFocusedItem: TVirtualListviewItem) of object;
  TOnVirtualListviewSort = function(Sender: TObject; Item1, Item2: TVirtualListviewItem): Integer of object;

  { TVirtualListviewItem }

  TVirtualListviewItem = class(TPersistent)
  private
    FCaptions: TStringList;
    FColor: TColor;
    FEnabled: Boolean;
    FFocused: Boolean;
    FHeight: Integer;
    FImageIndex: Integer;
    FIndex: Integer;
    FLeft: Integer;
    FOwnerListview: TCustomVirtualListview;
    FSelected: Boolean;
    FTag: Integer;
    FTop: Integer;
    FVisible: Boolean;
    FVisibleIndex: Integer;
    FWidth: Integer;
    procedure SetCaptions(AValue: TStringList);
    procedure SetColor(AValue: TColor);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFocused(AValue: Boolean);
    procedure SetHeight(AValue: Integer);
    procedure SetImageIndex(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetSelected(AValue: Boolean);
    procedure SetTop(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  protected
    procedure Paint(Canvas: TCanvas);
  public
    property Captions: TStringList read FCaptions write SetCaptions;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Focused: Boolean read FFocused write SetFocused;
    property Height: Integer read FHeight write SetHeight;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Index: Integer read FIndex;
    property Left: Integer read FLeft write SetLeft;
    property OwnerListview: TCustomVirtualListview read FOwnerListview;
    property Selected: Boolean read FSelected write SetSelected;
    property Tag: Integer read FTag write FTag;
    property Top: Integer read FTop write SetTop;
    property Visible: Boolean read FVisible write SetVisible;
    property VisibleIndex: Integer read FVisibleIndex;
    property Width: Integer read FWidth write SetWidth;

    constructor Create;
    destructor Destroy; override;
    function BoundsRect: TRect;
    procedure Invalidate(Update: Boolean);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    function PtInItem(APt: TPoint): Boolean;
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
    function Add(ACaption: string): TVirtualListviewItem;
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
    procedure Paint(Canvas: TCanvas);
  public
    property MouseDownPt: TPoint read FMouseDownPt write FMouseDownPt;
    property MouseLastPt: TPoint read FMouseLastPt write FMouseLastPt;
    property State: TMouseControllerStateSet read FState write FState;
    property DeltaDragDetect: Integer read FDeltaDragDetect write FDeltaDragDetect;

    constructor Create;
    function AnyButtonDown: Boolean;
  end;


  { TCustomVirtualListview }

  TCustomVirtualListview = class(TScrollBox)
  private
    FAlignment: TAlignment;
    FFocusedItem: TVirtualListviewItem;
    FCalculatedHorzScrollRange: Integer;
    FImages: TImageList;
    FMouseController: TMustangpeakMouseController;
    FOnSort: TOnVirtualListviewSort;
    FTextLayout: TVirtualListviewTextLayout;
    FCaptionIndent: Integer;
    FCaptionLineCount: Integer;
    FDefaultItemHeight: Integer;
    FDetailsFont: TFont;
    FDetailsIndent: Integer;
    FItems: TVirtualListviewItemList;
    FVisibleItems: TVirtualListviewItemVisibleList;
    FUpdateLock: Integer;
    OnFocusChanged: TOnVirtualListviewFocusedChanged;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetFocusedItem(AValue: TVirtualListviewItem);
    procedure SetTextLayout(AValue: TVirtualListviewTextLayout);
    procedure SetCaptionIndent(AValue: Integer);
    procedure SetCaptionLineCount(AValue: Integer);
    procedure SetDefaultItemHeight(AValue: Integer);
    procedure SetDetailsFont(AValue: TFont);
    procedure SetDetailsIndent(AValue: Integer);
  protected
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property CaptionIndent: Integer read FCaptionIndent write SetCaptionIndent;
    property CaptionLineCount: Integer read FCaptionLineCount write SetCaptionLineCount;
    property DefaultItemHeight: Integer read FDefaultItemHeight write SetDefaultItemHeight;
    property DetailsIndent: Integer read FDetailsIndent write SetDetailsIndent;
    property DetailsFont: TFont read FDetailsFont write SetDetailsFont;
    property FocusedItem: TVirtualListviewItem read FFocusedItem write SetFocusedItem;
    property Images: TImageList read FImages write FImages;
    property Items: TVirtualListviewItemList read FItems write FItems;
    property MouseController: TMustangpeakMouseController read FMouseController write FMouseController;
    property VisibleItems: TVirtualListviewItemVisibleList read FVisibleItems write FVisibleItems;
    property OnFocusedChanged: TOnVirtualListviewFocusedChanged read OnFocusChanged write OnFocusChanged;
    property OnSort: TOnVirtualListviewSort read FOnSort write FOnSort;
    property TextLayout: TVirtualListviewTextLayout read FTextLayout write SetTextLayout;
    property UpdateLock: Integer read FUpdateLock write FUpdateLock;

    procedure CalculateAutoRanges; override;
    procedure DoOnFocusChanged(LccItem, OldLccItem: TVirtualListviewItem); virtual;
    procedure DoOnResize; override;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
    function DoOnSort(Item1, Item2: TVirtualListviewItem): Integer; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMShowWindow(var Message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure Paint; override;
    procedure RebuildItems;
  public
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
    procedure ScrollIntoView(Item: TVirtualListviewItem);
  end;

  TVirtualListview = class(TCustomVirtualListview)
  public
    property FocusedItem;
    property Items;
    property VisibleItems;
  published
    property Alignment;
    property CaptionIndent;
    property CaptionLineCount;
    property DefaultItemHeight;
    property DetailsFont;
    property DetailsIndent;
    property Images;
    property OnFocusedChanged;
    property OnSort;
    property TextLayout;
  end;

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

procedure TMustangpeakMouseController.Paint(Canvas: TCanvas);
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

function TVirtualListviewItem.BoundsRect: TRect;
begin
  Result.Top := Top;
  Result.Bottom := Top + FHeight;
  Result.Left := Left;
  Result.Right := Left + FWidth;
end;

constructor TVirtualListviewItem.Create;
begin
  inherited Create;
  FCaptions := TStringList.Create;
  FImageIndex := -1;
  FVisible := True;
  FEnabled := True;
  FColor := clBtnFace;
  FTop := 0;
  FLeft := 0;
  FWidth := 20;
  FHeight := 20;
end;

destructor TVirtualListviewItem.Destroy;
begin
  FreeAndNil(FCaptions);
  inherited Destroy;
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

procedure TVirtualListviewItem.Paint(Canvas: TCanvas);
var
  TextBox: TRect;
  i, Offset: Integer;
  TextExtent: TSize;
  TextRects: array of TRect;
  ImageWidth: Integer;
begin
  ImageWidth := 0;
  if Assigned(OwnerListview.Images) and (ImageIndex > -1) then
    ImageWidth := OwnerListview.Images.Width + 4;

  SetLength(TextRects, Captions.Count);
  for i := 0 to Length(TextRects) - 1 do
    EmptyRect(TextRects[i]);

  if (Captions.Count > 0) and (OwnerListview.CaptionLineCount > 0) then
  begin
    Canvas.Font.Assign(OwnerListview.Font);
    TextExtent := Canvas.TextExtent(Captions[0]);
    TextRects[0].Right := TextExtent.cx + ImageWidth;
    TextRects[0].Bottom := TextExtent.cy;
    OffsetRect(TextRects[0], OwnerListview.CaptionIndent + ImageWidth, 0);
    if TextRects[0].Right > Width - OwnerListview.CaptionIndent then
      TextRects[0].Right := Width - OwnerListview.CaptionIndent;

    Canvas.Font.Assign(OwnerListview.DetailsFont);
    i := 1;
    while (i < OwnerListview.CaptionLineCount) and (i < Captions.Count) do
    begin
      TextExtent := Canvas.TextExtent(Captions[i]);
      TextRects[i].Right := TextExtent.cx + ImageWidth;
      TextRects[i].Bottom := TextExtent.cy;
      OffsetRect(TextRects[i], OwnerListview.DetailsIndent + ImageWidth, 0);
      if TextRects[i].Right > Width - OwnerListview.DetailsIndent then
        TextRects[i].Right := Width - OwnerListview.DetailsIndent;
      OffsetRect(TextRects[i], 0, TextRects[i-1].Bottom);
      Inc(i)
    end;
  end;

  TextBox := Rect(0, 0, 0, 0);
  for i := 0 to Length(TextRects) - 1 do
    UnionRect(TextBox, TextBox, TextRects[i]);

  case OwnerListview.TextLayout of
    vtlTop :
      begin
        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], 0, 0);
      end;
    vtlCenter :
      begin
        if TextBox.Bottom < Height then
          Offset := (Height - TextBox.Bottom) div 2
        else
          Offset := 0;

        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], 0, Offset);
      end;
    vtlBottom :
      begin
        if TextBox.Bottom < Height then
        begin
          Offset := Height - TextBox.Bottom
        end else
          Offset := 0;

        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], 0, Offset);
      end;
  end;

  case OwnerListview.Alignment of
    taLeftJustify :
      begin
        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], 0, 0);
      end;
    taCenter :
      begin
        if TextBox.Right < Width then
          Offset := (Width - TextBox.Right) div 2
        else
          Offset := 0;

        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], Offset, 0);
      end;
    taRightJustify :
      begin
        if TextBox.Right < Width then
        begin
          Offset := Width - TextBox.Right
        end else
          Offset := 0;

        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], Offset, 0);
      end;
  end;

  Canvas.ClipRect := BoundsRect;
  Canvas.Clipping := True;

  Canvas.Brush.Color := Color;

  Canvas.FillRect(BoundsRect);
  if Focused then
  begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clHighlightedText;
    Canvas.Brush.Color := clHighlight;
    Canvas.RoundRect(BoundsRect, 8, 8);
  end;

  if Assigned(OwnerListview.Images) and (ImageIndex > -1) then
  case OwnerListview.TextLayout of
    vtlTop    : OwnerListview.Images.Draw(Canvas, Left + 4, Top + 4, ImageIndex);
    vtlCenter : OwnerListview.Images.Draw(Canvas, Left + 4, Top + ((Height - OwnerListview.Images.Height) div 2), ImageIndex);
    vtlBottom : OwnerListview.Images.Draw(Canvas, Left + 4, Top + (Height - OwnerListview.Images.Height - 4), ImageIndex);
  end;


  if (Captions.Count > 0) and (OwnerListview.CaptionLineCount > 0) then
  begin
    Canvas.Font.Assign(OwnerListview.Font);
    if Focused then
      Canvas.Font.Color := clHighlightText;
    OffsetRect(TextRects[0], Left, Top);
    Canvas.TextRect(TextRects[0], TextRects[0].Left, TextRects[0].Top, Captions[0]);
    Canvas.Font.Assign(OwnerListview.DetailsFont);
    if Focused then
      Canvas.Font.Color := clHighlightText;
    i := 1;
    while (i < OwnerListview.CaptionLineCount) and (i < Captions.Count) do
    begin
      OffsetRect(TextRects[i], Left, Top);
      Canvas.TextRect(TextRects[i], TextRects[i].Left, TextRects[i].Top, Captions[i]);
      Inc(i);
    end;
  end;
end;

function TVirtualListviewItem.PtInItem(APt: TPoint): Boolean;
begin
  Result := PtInRect(BoundsRect, APt);
end;

procedure TVirtualListviewItem.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FTop := ATop;
  FLeft := ALeft;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TVirtualListviewItem.SetCaptions(AValue: TStringList);
begin
  FCaptions.Assign(AValue);
  Invalidate(True);
end;

procedure TVirtualListviewItem.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  Invalidate(True);
end;

procedure TVirtualListviewItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled =AValue then Exit;
  FEnabled :=AValue;
  Invalidate(True);
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
  Invalidate(True);
end;

procedure TVirtualListviewItem.SetHeight(AValue: Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
end;

procedure TVirtualListviewItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  Invalidate(True);
end;

procedure TVirtualListviewItem.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;
end;

procedure TVirtualListviewItem.SetSelected(AValue: Boolean);
begin
  if FSelected = AValue then Exit;
  FSelected := AValue;
end;

procedure TVirtualListviewItem.SetTop(AValue: Integer);
begin
  if FTop = AValue then Exit;
  FTop := AValue;
end;

procedure TVirtualListviewItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
  OwnerListview.RebuildItems;
end;

procedure TVirtualListviewItem.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
end;

{ TCustomVirtualListview }

procedure TCustomVirtualListview.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TCustomVirtualListview.CalculateAutoRanges;
begin
  HorzScrollBar.Page := 1;
  VertScrollBar.Page := 1;
  if DefaultItemHeight * VisibleItems.Count > ClientHeight then
  {$IFDEF DARWIN}
    VertScrollBar.Range := (DefaultItemHeight * (VisibleItems.Count + 1))    // Add 1 for OSX it has an issue
  {$ELSE}
    VertScrollBar.Range := (DefaultItemHeight * VisibleItems.Count)
  {$ENDIF}
  else
    VertScrollBar.Range := 0;
  HorzScrollBar.Range := 0;
  AutoScroll := True;
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
  ViewPt := ClientPtToCurrentViewPt(ClientPt);
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
  Color := clWindow;
  VertScrollBar.Tracking := True;
  HorzScrollBar.Tracking := True;
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
   RebuildItems;
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
    RebuildItems;
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
    IntersectRect(ResultRect, AViewRect, Item.BoundsRect);
    Result := not IsRectEmpty(ResultRect)
  end;
end;

procedure TCustomVirtualListview.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
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
  VertScrollBar.Position := Item.Top;
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
    Item.Paint(Canvas);
    Item := NextVisible(Item);
    if not IsInCurrentView(Item) then
      Item := nil
  end;


  Canvas.RestoreHandleState;

  if mcs_DragRect in MouseController.State then
   begin
    MouseController.Paint(Canvas);
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

procedure TCustomVirtualListview.RebuildItems;
var
  i, NextTop, LocalVisibleIndex: Integer;
begin
  if UpdateLock > 0 then Exit;
  if Assigned(VisibleItems) and Assigned(Items) then
  begin
    VisibleItems.Clear;
    NextTop := 0;
    LocalVisibleIndex := 0;
    for i := 0 to Items.Count - 1 do
    begin
      Items[i].FIndex := i;
      if Items[i].Visible then
      begin
        Items[i].FVisibleIndex := LocalVisibleIndex;
        Inc(LocalVisibleIndex);
        VisibleItems.Add(Items[i]);
        Items[i].SetBounds(0, NextTop, ClientWidth, DefaultItemHeight);
        NextTop := Items[i].Top + Items[i].Height;
      end else
      begin
        Items[i].Height := 0;
      end;
    end;
    CalculateAutoRanges;
    Invalidate;
    Update;
  end;
end;

procedure TCustomVirtualListview.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then Exit;
  FAlignment := AValue;
  Invalidate;
  Update;
end;


procedure TCustomVirtualListview.SetTextLayout(AValue: TVirtualListviewTextLayout);
begin
  if FTextLayout = AValue then Exit;
  FTextLayout := AValue;
  Invalidate;
  Update;
end;

procedure TCustomVirtualListview.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  beep;
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
  inherited;
end;

procedure TCustomVirtualListview.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
end;

procedure TCustomVirtualListview.SetCaptionIndent(AValue: Integer);
begin
  if FCaptionIndent = AValue then Exit;
  FCaptionIndent := AValue;
  Invalidate;
  Update;
end;

procedure TCustomVirtualListview.SetCaptionLineCount(AValue: Integer);
begin
  if FCaptionLineCount = AValue then Exit;
  FCaptionLineCount := AValue;
  Invalidate;
  Update;
end;

procedure TCustomVirtualListview.SetDefaultItemHeight(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FDefaultItemHeight = AValue then Exit;
  FDefaultItemHeight := AValue;
  RebuildItems;
end;

procedure TCustomVirtualListview.SetDetailsFont(AValue: TFont);
begin
  FDetailsFont.Assign(AValue);
end;

procedure TCustomVirtualListview.SetDetailsIndent(AValue: Integer);
begin
  if FDetailsIndent = AValue then Exit;
  FDetailsIndent := AValue;
  Invalidate;
  Update
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


{ TVirtualListviewItemList }

function TVirtualListviewItemList.Add(ACaption: string): TVirtualListviewItem;
begin
  Result := TVirtualListviewItem.Create;
  Result.Captions.Add(ACaption);
  Result.FOwnerListview := OwnerListview;
  ItemList.Add(Result);
  if Count > 0 then
    Result.SetBounds(0, Items[Count-1].Top + Items[Count-1].Height, OwnerListview.ClientWidth, OwnerListview.DefaultItemHeight)
  else
    Result.SetBounds(0, 0, OwnerListview.ClientWidth, OwnerListview.DefaultItemHeight);
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

