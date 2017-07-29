unit mustangpeak.dragmanager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Objects, FMX.Graphics, System.Generics.Collections, mustangpeak.xmlutilities,
  FMX.Layouts;

type
  TDragState = (dsNone, dsDragging, dsSelectRect, dsDragPending, dsSelectRectPending);
  TZOrder = (zoTopDown, zoBottomUp);

type
  TDragManager = class;   // forward
  TSelectableObject = class;

  TDragManagerSelectionChanging = procedure(Sender: TObject; SelectableObject: TSelectableObject; Selected: Boolean; var Allow: Boolean) of object;
  TDragManagerSelectionChange = procedure(Sender: TObject; SelectableObject: TSelectableObject; Selected: Boolean) of object;
  TDragManagerSelectableObjectCreate = procedure(Sender: TObject; SelectableObject: TSelectableObject) of object;
  TDragManagerSelectableObjectDestroy = procedure(Sender: TObject; SelectableObject: TSelectableObject) of object;

  TSelectableObject = class(TImage)
  private
    FSelected: Boolean;
    FManager: TDragManager;
    FBrushWidth: single;
    procedure SetSelected(const Value: Boolean);
    procedure SetBrushWidth(const Value: single);
  protected
  public
    property BrushWidth: single read FBrushWidth write SetBrushWidth;
    property Manager: TDragManager read FManager;
    property Selected: Boolean read FSelected write SetSelected;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure LoadFromXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode); virtual;
    procedure MoveBy(DeltaPt: TPointF);
    procedure MoveTo(ViewportPt: TPointF);
    procedure RecalcOpacity; override;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode); virtual;
    procedure VisualPropertyUpdated; virtual;
  end;
  TSelectableImageClass = class of TSelectableObject;

  TSelectableObjectList<T: class> = class(TList<T>)
  private
    FSelection: TObjectList<TSelectableObject>;
  protected
    procedure Notify(const Value: T; Action: TCollectionNotification); override;
  public
    property Selection: TObjectList<TSelectableObject> read FSelection;
  end;

  TDragManager = class(TComponent)
  private
    FDragState: TDragState;                  // What is either occuring or pending in terms of the drag
    FMouseDownViewportPoint: TPointF;    // The point the user intially clicked in the viewport
    FMousePreviousVewportPoint: TPointF; // The point the user was during a drag on the prevous time MouseMove was called
    FMouseCurrentViewportPoint: TPointF; // The point in the current Mouse Move call
    FMouseShift: TShiftState;                 // The state of the Shift keys when the drag started
    FMouseButton: TMouseButton;          // Which button was down when the drag started
    FEditMode: Boolean;                  // Allow dragging
    FDragSelectCurrentRect: TRectF;
    FDragSelectStartRect: TRectF;
    FDragSelectRectangle: TRectangle;
    FSelection: TObjectList<TSelectableObject>;
    FSelectableObject: TObjectList<TSelectableObject>;
    FOnSelectionChange: TDragManagerSelectionChange;
    FOnSelectionChanging: TDragManagerSelectionChanging;
    FOnSelectableObjectDestroy: TDragManagerSelectableObjectDestroy;
    FOnSelectableObjectCreate: TDragManagerSelectableObjectCreate;
    FSelectionModifier: Boolean;
    FClipboard: TMustangpeakXmlDocument;
    procedure SetEditMode(const Value: Boolean);
  protected
    property Clipboard: TMustangpeakXmlDocument read FClipboard write FClipboard;
    procedure DoSelectableObjectCreate(SelectableObject: TSelectableObject);
    procedure DoSelectableObjectDestroy(SelectableObject: TSelectableObject);
    procedure DoSelectionChange(SelectableObject: TSelectableObject; Selected: Boolean);
    procedure DoSelectionChanging(SelectableObject: TSelectableObject; Selected: Boolean; var Allow: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DragSelectCurrentRect: TRectF read FDragSelectCurrentRect write FDragSelectCurrentRect;
    property DragSelectRectangle: TRectangle read FDragSelectRectangle write FDragSelectRectangle;
    property DragSelectStartRect: TRectF read FDragSelectStartRect write FDragSelectStartRect;
    property DragState: TDragState read FDragState write FDragState;
    property EditMode: Boolean read FEditMode write SetEditMode;
    property MouseButton: TMouseButton read FMouseButton write FMouseButton;
    property MouseCurrentViewportPoint: TPointF read FMouseCurrentViewportPoint write FMouseCurrentViewportPoint;
    property MouseDownViewportPoint: TPointF read FMouseDownViewportPoint write FMouseDownViewportPoint;
    property MousePreviousVewportPoint: TPointF read FMousePreviousVewportPoint write FMousePreviousVewportPoint;
    property MouseShift: TShiftState read FMouseShift write FMouseShift;
    property OnSelectableObjectCreate: TDragManagerSelectableObjectCreate read FOnSelectableObjectCreate write FOnSelectableObjectCreate;
    property OnSelectableObjectDestroy: TDragManagerSelectableObjectDestroy read FOnSelectableObjectDestroy write FOnSelectableObjectDestroy;
    property OnSelectionChange: TDragManagerSelectionChange read FOnSelectionChange write FOnSelectionChange;
    property OnSelectionChanging: TDragManagerSelectionChanging read FOnSelectionChanging write FOnSelectionChanging;
    property SelectableObject: TObjectList<TSelectableObject> read FSelectableObject;
    property Selection: TObjectList<TSelectableObject> read FSelection;
    property SelectionModifier: Boolean read FSelectionModifier write FSelectionModifier;

    function CalculateSnap(Target: single; Snap: single): single;
    function CalculateSnapPt(Target: TPointF; SnapX, SnapY: single): TPointF;
    procedure CopySelected;
    procedure DeleteSelected; virtual;
    function FindSelectableObjectByPt(ViewportPt: TPointF; ZOrder: TZOrder): TSelectableObject;
    procedure LoadFromXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode; ParentObject: TFmxObject); virtual;
    procedure MouseDownScrollBox(TargetScrollBox: TScrollBox; Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMoveScrollBox(TargetScrollBox: TScrollBox; Sender: TObject; Shift: TShiftState; X, Y: Single; DragXSnap, DragYSnap: Single);
    procedure MouseUpScrollBox(TargetScrollBox: TScrollBox; Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    function MouseSelectRectOffset: TPointF;
    procedure MoveSelectedBy(DeltaPt: TPointF);
    function NewSelectableObject(ASegmentClass: TSelectableImageClass; AParent: TFmxObject): TSelectableObject;
    procedure PasteTo(ParentObject: TFmxObject);
    procedure SaveToXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode); virtual;
    procedure SelectAll; virtual;
    function SelectByPt(ViewportX, ViewportY: single): Boolean;
    function SelectByRect(SelectRect: TRectF; Combine: Boolean): Boolean;
    function SelectionBounds: TRectF;
    procedure UnselectAll;
  end;

implementation

{ TDragManager }

function TDragManager.CalculateSnap(Target, Snap: single): single;
begin
  if Snap > 0 then
  begin
    if Frac(Target/Snap) < 0.5 then
      Result := Snap * Trunc(Target/Snap)
    else
      Result := (Snap * Trunc(Target/Snap)) + Snap
  end else
    Result := Target;
end;

function TDragManager.CalculateSnapPt(Target: TPointF; SnapX, SnapY: single): TPointF;
begin
  Result.X := CalculateSnap(Target.X, SnapX);
  Result.Y := CalculateSnap(Target.Y, SnapY)
end;

procedure TDragManager.CopySelected;
var
  i: Integer;
  RootNode, ChildNode, SegmentNode: TMustangpeakXmlNode;
begin
  XmlFreeDocument(FClipboard);
  Clipboard := XmlCreateEmptyDocument;
  RootNode := XmlCreateRootNode(Clipboard, 'clipboard', '');
  ChildNode := XmlCreateChildNode(Clipboard, RootNode, 'structure', '');
  for i := 0 to Selection.Count - 1 do
  begin
    SegmentNode := XmlCreateChildNode(Clipboard, ChildNode, 'segment', '');
    Selection[i].SaveToXML(Clipboard, SegmentNode);
  end;

end;

constructor TDragManager.Create(AOwner: TComponent);
begin
  inherited;
  FSelection := TObjectList<TSelectableObject>.Create;
  Selection.OwnsObjects := False;
  FSelectableObject := TObjectList<TSelectableObject>.Create;
  SelectableObject.OwnsObjects := True;
  DragSelectRectangle := TRectangle.Create(nil);
  DragSelectRectangle.Fill.Color := $FFE0E0E0;
  DragSelectRectangle.Fill.Gradient.Color := TAlphaColorRec.Mediumblue;
  DragSelectRectangle.Fill.Gradient.Color1 := TAlphaColorRec.White;
  DragSelectRectangle.Opacity := 0.2;
  DragSelectRectangle.CornerType := TCornerType.Round;
  DragSelectRectangle.XRadius := 5;
  DragSelectRectangle.YRadius := 5;
  DragSelectRectangle.Fill.Kind := TBrushKind.Gradient;
  DragSelectRectangle.Stroke.Color := TAlphAColorRec.Blue;
end;

procedure TDragManager.DeleteSelected;
var
  i: Integer;
begin
  for i := SelectableObject.Count - 1 downto 0 do
  begin
    if SelectableObject[i].Selected then
      SelectableObject.Delete(i);
  end;
end;

destructor TDragManager.Destroy;
begin
  UnSelectAll;
  Selection.Clear;
  SelectableObject.Clear;
  SelectableObject.DisposeOf;
  Selection.DisposeOf;
  DragSelectRectangle.DisposeOf;
  XmlFreeDocument(FClipboard);
  inherited;
end;


procedure TDragManager.DoSelectableObjectCreate(SelectableObject: TSelectableObject);
begin
  if Assigned(OnSelectableObjectCreate) then
    OnSelectableObjectCreate(Self, SelectableObject);
end;

procedure TDragManager.DoSelectableObjectDestroy(SelectableObject: TSelectableObject);
begin
  if Assigned(OnSelectableObjectDestroy) then
    OnSelectableObjectDestroy(Self, SelectableObject);
end;

procedure TDragManager.DoSelectionChange(SelectableObject: TSelectableObject; Selected: Boolean);
begin
  if Assigned(OnSelectionChange) then
    OnSelectionChange(Self, SelectableObject, Selected);
end;

procedure TDragManager.DoSelectionChanging(SelectableObject: TSelectableObject; Selected: Boolean; var Allow: Boolean);
begin
  if Assigned(OnSelectionChanging) then
    OnSelectionChanging(Self, SelectableObject, Selected, Allow);
end;

function TDragManager.FindSelectableObjectByPt(ViewportPt: TPointF; ZOrder: TZOrder): TSelectableObject;
var
  i: Integer;
begin
  Result := nil;
  if ZOrder = zoTopDown then
  begin
    i := SelectableObject.Count - 1;
    while not Assigned(Result) and (i > -1) do
    begin
      if PtInRect(SelectableObject[i].BoundsRect, TPointF.Create(ViewportPt.X, ViewportPt.Y)) then
        Result := SelectableObject[i];
      Dec(i);
    end;
  end else
  if ZOrder = zoBottomUp then
  begin
    i := 0;
    while not Assigned(Result) and (i < SelectableObject.Count) do
    begin
      if PtInRect(SelectableObject[i].BoundsRect, TPointF.Create(ViewportPt.X, ViewportPt.Y)) then
        Result := SelectableObject[i];
      Inc(i);
    end;
  end;
end;

procedure TDragManager.LoadFromXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode; ParentObject: TFmxObject);
var
  Child, ClassNameNode: TMustangpeakXmlNode;
  ASegment: TSelectableObject;
  ClassNameStr: string;
  NewClass: TPersistentClass;
begin
  UnSelectAll;
  Selection.Clear;
  SelectableObject.Clear;
  Child := XmlFirstChild(Node);
  while Assigned(Child) do
  begin
    if XmlNodeName(Child) = 'segment' then
    begin
      ClassNameNode := XmlFindChildNode(Child, 'classname');
      if Assigned(ClassNameNode) then
      begin
        ClassNameStr := XmlNodeTextContent(ClassNameNode);
        NewClass := FindClass(ClassNameStr);
        if Assigned(NewClass) then
        begin
          ASegment := NewSelectableObject(TSelectableImageClass( NewClass), ParentObject);
          ASegment.LoadFromXML(XmlDoc, Child);
        end;
      end;
    end;
    Child := XmlNextSiblingNode(Child)
  end;
end;

procedure TDragManager.MouseDownScrollBox(TargetScrollBox: TScrollBox; Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  TrackSegment: TSelectableObject;
  Modifier: Boolean;
begin
  TargetScrollBox.Root.Captured := TargetScrollBox;

  // Convert the Client Coordinates into Viewport of the ScrollWindow
  FMouseDownViewportPoint.X := TargetScrollBox.ViewportPosition.X + X;
  FMouseDownViewportPoint.Y := TargetScrollBox.ViewportPosition.Y + Y;
  MouseCurrentViewportPoint := MouseDownViewportPoint;
  MousePreviousVewportPoint := MouseDownViewportPoint;
  MouseButton := Button;
  MouseShift := Shift;

  case Button of
    TMouseButton.mbLeft :
      begin
        // Only allow selection and dragging when in Edit Mode;
        if EditMode then
        begin

          Modifier := ((ssCtrl in Shift) or (ssShift in Shift)) or SelectionModifier;

          TrackSegment := FindSelectableObjectByPt(MouseDownViewportPoint, zoTopDown);
          if Assigned(TrackSegment) then
          begin
            if TrackSegment.Selected then    // Already Selected?
            begin
              if Selection.Count = 1 then
              begin
                DragState := TDragState.dsDragPending;
              end else
              begin
                if Modifier then
                  TrackSegment.Selected := False
                else
                  DragState := TDragState.dsDragPending;
              end;
            end else
            begin
              if not Modifier then
                UnselectAll;
              TrackSegment.Selected := True;
              DragState := TDragState.dsDragPending;
            end;
          end else
          begin
            if not Modifier then
              UnselectAll;
            DragState := TDragState.dsSelectRectPending
          end;

          DragSelectStartRect := SelectionBounds;
          DragSelectCurrentRect := DragSelectStartRect;
        end;
      end;
    TMouseButton.mbRight :
      begin

      end;
    TMouseButton.mbMiddle :
      begin

      end;
  end;
end;

procedure TDragManager.MouseMoveScrollBox(TargetScrollBox: TScrollBox; Sender: TObject; Shift: TShiftState; X, Y: Single; DragXSnap, DragYSnap: Single);

  function LocalDragThresholdMet(ViewportPt: TPointF): Boolean;
  const DRAG_THRESHOLD = 5;
  begin
    Result := (Abs(MouseDownViewportPoint.X - ViewportPt.X) > DRAG_THRESHOLD) or (Abs(MouseDownViewportPoint.Y - ViewportPt.Y) > DRAG_THRESHOLD);
  end;

  procedure LocalDoDragSelect;
  var
    IsMultiSelect: Boolean;
  begin
    if MouseCurrentViewportPoint.X < 0 then
      MouseCurrentViewportPoint.Create(0, MouseCurrentViewportPoint.Y);
    if MouseCurrentViewportPoint.Y < 0 then
       MouseCurrentViewportPoint.Create(MouseCurrentViewportPoint.X, 0);

    if MouseCurrentViewportPoint.X - MouseDownViewportPoint.X < 0 then
    begin
      DragSelectRectangle.Position.X := MouseCurrentViewportPoint.X;
      DragSelectRectangle.Width := MouseDownViewportPoint.X - MouseCurrentViewportPoint.X;
    end else
    begin
      DragSelectRectangle.Position.X := MouseDownViewportPoint.X;
      DragSelectRectangle.Width := MouseCurrentViewportPoint.X - MouseDownViewportPoint.X;
    end;
    if MouseCurrentViewportPoint.Y - MouseDownViewportPoint.Y < 0 then
    begin
      DragSelectRectangle.Position.Y := MouseCurrentViewportPoint.Y;
      DragSelectRectangle.Height := MouseDownViewportPoint.Y - MouseCurrentViewportPoint.Y;
    end else
    begin
      DragSelectRectangle.Position.Y := MouseDownViewportPoint.Y;
      DragSelectRectangle.Height := MouseCurrentViewportPoint.Y - MouseDownViewportPoint.Y;
    end;

    IsMultiSelect := ((ssCtrl in Shift) or (ssShift in Shift)) or SelectionModifier;
    SelectByRect(DragSelectRectangle.BoundsRect, IsMultiSelect);
  end;

  procedure LocalDoDrag;
  var
    SelectRectSnapPt, DeltaPt: TPointF;
  begin
    SelectRectSnapPt := TPointF.Create(MouseCurrentViewportPoint.X - MouseSelectRectOffset.X, MouseCurrentViewportPoint.Y - MouseSelectRectOffset.Y);

    SelectRectSnapPt := CalculateSnapPt(SelectRectSnapPt, DragXSnap, DragYSnap);

    if SelectRectSnapPt.X < 0 then
      DeltaPt.X := 0
    else
      DeltaPt.X := SelectRectSnapPt.X - DragSelectCurrentRect.Left;
    if SelectRectSnapPt.Y < 0 then
      DeltaPt.Y := 0
    else
      DeltaPt.Y := SelectRectSnapPt.Y - DragSelectCurrentRect.Top;

    MoveSelectedBy(DeltaPt);

    DragSelectCurrentRect := SelectionBounds;

    if DragSelectCurrentRect.Left < TargetScrollBox.ViewportPosition.X then
      TargetScrollBox.ScrollBy(+(DragSelectCurrentRect.Left - TargetScrollBox.ViewportPosition.X), 0)
    else
      if DragSelectCurrentRect.Right > (TargetScrollBox.ViewportPosition.X + TargetScrollBox.Width) then
      TargetScrollBox.ScrollBy(-(DragSelectCurrentRect.Right - (TargetScrollBox.ViewportPosition.X + TargetScrollBox.Width)), 0);


    if DragSelectCurrentRect.Bottom < TargetScrollBox.ViewportPosition.Y then
      TargetScrollBox.ScrollBy(0, +(DragSelectCurrentRect.Bottom - TargetScrollBox.ViewportPosition.Y))
    else
    if DragSelectCurrentRect.Bottom > (TargetScrollBox.ViewportPosition.Y + TargetScrollBox.Height) then
      TargetScrollBox.ScrollBy(0, -(DragSelectCurrentRect.Bottom - (TargetScrollBox.ViewportPosition.Y + TargetScrollBox.Height)));
  end;

begin
   // Convert the Client Coordinates into Viewport of the ScrollWindow
  FMouseCurrentViewportPoint.X := TargetScrollBox.ViewportPosition.X + X;
  FMouseCurrentViewportPoint.Y := TargetScrollBox.ViewportPosition.Y + Y;

  case MouseButton of
    TMouseButton.mbLeft :
      begin
    //    TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(TrackSegmentManager.MouseCurrentViewportPoint.X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(TrackSegmentManager.MouseCurrentViewportPoint.Y, ffFixed, 4, 4);

        case DragState of
          dsNone :
            begin

            end;
          dsDragPending :
            begin
              if LocalDragThresholdMet(MouseCurrentViewportPoint) then
              begin
                DragState := TDragState.dsDragging;
                LocalDoDrag;
              end;
            end;
          dsDragging :
            begin
              LocalDoDrag;
            end;
          dsSelectRectPending :
            begin
              if LocalDragThresholdMet(MouseCurrentViewportPoint) then
              begin
                DragSelectRectangle.Position.Point := MouseDownViewportPoint;
                DragSelectRectangle.Width := 0;
                DragSelectRectangle.Height := 0;
                DragSelectRectangle.Parent := TargetScrollBox;
                DragState := TDragState.dsSelectRect;
                LocalDoDragSelect;
              end;
            end;
          dsSelectRect :
            begin
              LocalDoDragSelect;
            end;
        end;
      end;
    TMouseButton.mbRight :
      begin
      end;
    TMouseButton.mbMiddle :
      begin

      end;
  end;
  // Update previous to current for next mouse move call
  MousePreviousVewportPoint := MouseCurrentViewportPoint;
end;

function TDragManager.MouseSelectRectOffset: TPointF;
begin
  Result := TPointF.Create(MouseDownViewportPoint.X - DragSelectStartRect.Left, MouseDownViewportPoint.Y - DragSelectStartRect.Top)
end;

procedure TDragManager.MouseUpScrollBox(TargetScrollBox: TScrollBox; Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  HitSegment: TSelectableObject;
begin
  TargetScrollBox.Root.Captured := nil;

     // Convert the Client Coordinates into Viewport of the ScrollWindow
  FMouseCurrentViewportPoint.X := TargetScrollBox.ViewportPosition.X + X;
  FMouseCurrentViewportPoint.Y := TargetScrollBox.ViewportPosition.Y + Y;

  DragSelectRectangle.Parent := nil;
  if not EditMode then
  begin
    HitSegment := FindSelectableObjectByPt(MouseCurrentViewportPoint, zoTopDown);
    if Assigned(HitSegment) then
      HitSegment.Click;
  end;
  DragState := dsNone;
end;

procedure TDragManager.MoveSelectedBy(DeltaPt: TPointF);
var
  i: Integer;
begin
  for i := 0 to Selection.Count - 1 do
    Selection[i].MoveBy(DeltaPt);
end;

function TDragManager.NewSelectableObject(ASegmentClass: TSelectableImageClass; AParent: TFmxObject): TSelectableObject;
begin
  Result := ASegmentClass.Create(nil);
  Result.FManager := Self;
  Result.Parent := AParent;
  Result.BrushWidth := 2;
  SelectableObject.Add(Result);
  DoSelectableObjectCreate(Result);
end;

procedure TDragManager.PasteTo(ParentObject: TFmxObject);
var
  RootNode, ChildNode, ClassNameNode: TMustangpeakXmlNode;
  ASegment: TSelectableObject;
  ClassNameStr: string;
  NewClass: TPersistentClass;
begin
  UnselectAll;
  RootNode := XmlFindRootNode(Clipboard, 'clipboard');
  if Assigned(RootNode) then
  begin
    ChildNode := XmlFindChildNode(RootNode, 'structure');
    if Assigned(ChildNode) then
    begin
     ChildNode := XmlFirstChild(ChildNode);
      while Assigned(ChildNode) do
      begin
        if XmlNodeName(ChildNode) = 'segment' then
        begin
          ClassNameNode := XmlFindChildNode(ChildNode, 'classname');
          if Assigned(ClassNameNode) then
          begin
            ClassNameStr := XmlNodeTextContent(ClassNameNode);
            NewClass := FindClass(ClassNameStr);
            if Assigned(NewClass) then
            begin
              ASegment := NewSelectableObject(TSelectableImageClass( NewClass), ParentObject);
              ASegment.LoadFromXML(Clipboard, ChildNode);
              ASegment.Selected := True;
            end;
          end;
        end;
        ChildNode := XmlNextSiblingNode(ChildNode)
      end;
    end;
  end;
end;

procedure TDragManager.SaveToXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode);
var
  i: Integer;
  Child: TMustangpeakXmlNode;
begin
  for i := 0 to SelectableObject.Count - 1 do
  begin
    Child := XmlCreateChildNode(XmlDoc, Node, 'segment', '');
    SelectableObject[i].SaveToXML(XmlDoc, Child);
  end;
end;

procedure TDragManager.SelectAll;
var
  i: Integer;
begin
  UnSelectAll;
  Selection.Clear;
  if EditMode then
  begin
    for i := 0 to SelectableObject.Count - 1 do
      SelectableObject[i].Selected := True;
  end;
end;

function TDragManager.SelectByPt(ViewportX, ViewportY: single): Boolean;
var
  i: Integer;
begin
  i := 0;
  Result := False;
  while (i < SelectableObject.Count) and not Result do
  begin
    if PtInRect(SelectableObject[i].BoundsRect, TPointF.Create(ViewportX, ViewportY)) then
    begin
      SelectableObject[i].Selected := True;
      Result := True;
    end;
    Inc(i);
  end;
end;

function TDragManager.SelectByRect(SelectRect: TRectF; Combine: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to SelectableObject.Count - 1 do
  begin
    if IntersectRect(SelectableObject[i].BoundsRect, SelectRect) then
    begin
      SelectableObject[i].Selected := True;
      Result := True;
    end else
    begin
      if not Combine then
        SelectableObject[i].Selected := False;
    end;
  end;
end;

function TDragManager.SelectionBounds: TRectF;
var
  i: Integer;
begin
  Result := Result.Empty;
  if Selection.Count > 0 then
    Result := Selection[0].BoundsRect;
  for i := 1 to Selection.Count - 1 do
    UnionRect(Result, Result, Selection[i].BoundsRect)
end;

procedure TDragManager.SetEditMode(const Value: Boolean);
begin
  if FEditMode <> Value then
  begin
    FEditMode := Value;
    if not EditMode then
      UnSelectAll;
  end;
end;

procedure TDragManager.UnselectAll;
var
  i: Integer;
begin
  if True then

  for i := Selection.Count - 1 downto 0 do
    Selection[i].Selected := False;
end;

{ TSelectableObjectList<T> }

procedure TSelectableObjectList<T>.Notify(const Value: T; Action: TCollectionNotification);
begin
  inherited;
end;

{ TSelectableImage }

procedure TSelectableObject.Click;
begin
  inherited;
end;

constructor TSelectableObject.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TSelectableObject.Destroy;
begin
  Selected := False;
  Manager.DoSelectableObjectDestroy(Self);
  inherited;
end;

procedure TSelectableObject.LoadFromStream(Stream: TStream);
begin
  inherited;
  Stream.Read(FOpacity, SizeOf(Opacity));
  Stream.Read(FSelected, SizeOf(Selected));
  Stream.Read(FBrushWidth, SizeOf(BrushWidth));
end;

procedure TSelectableObject.LoadFromXML(XmlDoc: TMustangpeakXmlDocument;  Node: TMustangpeakXmlNode);
var
  ReadStr: string;
  ChildNode: TMustangpeakXmlNode;
begin
  inherited;
  ChildNode := XmlFindChildNode(Node, 'opacity');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    if ReadStr <> '' then FOpacity := StrToFloat(ReadStr)
  end;
  ChildNode := XmlFindChildNode(Node, 'brushwidth');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    if ReadStr <> '' then FBrushWidth := StrToFloat(ReadStr)
  end;
end;

procedure TSelectableObject.MoveBy(DeltaPt: TPointF);
begin
  MoveTo(TPointF.Create(Position.X + DeltaPt.X, Position.Y + DeltaPt.Y))
end;

procedure TSelectableObject.MoveTo(ViewportPt: TPointF);
begin
  Position.X := ViewportPt.X;
  Position.Y := ViewportPt.Y;
end;

procedure TSelectableObject.RecalcOpacity;
begin
  inherited;
  VisualPropertyUpdated;
end;

procedure TSelectableObject.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(Selected, SizeOf(Selected));
  Stream.Write(Opacity, SizeOf(Opacity));
  Stream.Write(BrushWidth, SizeOf(BrushWidth));
end;

procedure TSelectableObject.SaveToXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode);
begin
  inherited;
  XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'classname', ClassName);
  XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'opacity', FloatToStr(Opacity));
  XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'brushwidth', FloatToStr(BrushWidth));
end;

procedure TSelectableObject.SetBrushWidth(const Value: single);
begin
  if FBrushWidth <> Value then
  begin
    FBrushWidth := Value;
    VisualPropertyUpdated;
  end;
end;

procedure TSelectableObject.SetSelected(const Value: Boolean);
var
  Allow: Boolean;
begin
  if Value <> Selected then
  begin
    Allow := True;
    Manager.DoSelectionChanging(Self, Value, Allow);
    if Allow then
    begin
      FSelected := Value;
      VisualPropertyUpdated;
      InvalidateRect(BoundsRect);
      if Value then
      begin
        if Manager.Selection.IndexOf(Self) < 0 then
          Manager.Selection.Add(Self)
      end else
        Manager.Selection.Remove(Self);
      Manager.DoSelectionChange(Self, Value);
    end;
  end;
end;

procedure TSelectableObject.VisualPropertyUpdated;
begin

end;

end.
