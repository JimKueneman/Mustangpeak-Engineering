unit mustangpeak.dragmanager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Objects, FMX.Graphics, System.Generics.Collections, mustangpeak.xmlutilities;

type
  TDragState = (dsNone, dsDragging, dsSelectRect, dsDragPending, dsSelectRectPending);

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

  TDragManager = class(TPersistent)
  private
    FState: TDragState;                  // What is either occuring or pending in terms of the drag
    FMouseDownViewportPoint: TPointF;    // The point the user intially clicked in the viewport
    FMousePreviousVewportPoint: TPointF; // The point the user was during a drag on the prevous time MouseMove was called
    FMouseCurrentViewportPoint: TPointF; // The point in the current Mouse Move call
    FShift: TShiftState;                 // The state of the Shift keys when the drag started
    FMouseButton: TMouseButton;          // Which button was down when the drag started
    FEditMode: Boolean;                  // Allow dragging
    FDragSelectCurrentRect: TRectF;
    FDragSelectStartRect: TRectF;
    FRectangleDragSelect: TRectangle;
    FSelection: TObjectList<TSelectableObject>;
    FSelectableObject: TObjectList<TSelectableObject>;
    FOnSelectionChange: TDragManagerSelectionChange;
    FOnSelectionChanging: TDragManagerSelectionChanging;
    FOnSelectableObjectDestroy: TDragManagerSelectableObjectDestroy;
    FOnSelectableObjectCreate: TDragManagerSelectableObjectCreate;
    procedure SetEditMode(const Value: Boolean);
  protected
    procedure DoSelectableObjectCreate(SelectableObject: TSelectableObject);
    procedure DoSelectableObjectDestroy(SelectableObject: TSelectableObject);
    procedure DoSelectionChange(SelectableObject: TSelectableObject; Selected: Boolean);
    procedure DoSelectionChanging(SelectableObject: TSelectableObject; Selected: Boolean; var Allow: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property EditMode: Boolean read FEditMode write SetEditMode;
    property MouseButton: TMouseButton read FMouseButton write FMouseButton;
    property MouseCurrentViewportPoint: TPointF read FMouseCurrentViewportPoint write FMouseCurrentViewportPoint;
    property MouseDownViewportPoint: TPointF read FMouseDownViewportPoint write FMouseDownViewportPoint;
    property MousePreviousVewportPoint: TPointF read FMousePreviousVewportPoint write FMousePreviousVewportPoint;
    property DragSelectStartRect: TRectF read FDragSelectStartRect write FDragSelectStartRect;
    property OnSelectableObjectCreate: TDragManagerSelectableObjectCreate read FOnSelectableObjectCreate write FOnSelectableObjectCreate;
    property OnSelectableObjectDestroy: TDragManagerSelectableObjectDestroy read FOnSelectableObjectDestroy write FOnSelectableObjectDestroy;
    property OnSelectionChange: TDragManagerSelectionChange read FOnSelectionChange write FOnSelectionChange;
    property OnSelectionChanging: TDragManagerSelectionChanging read FOnSelectionChanging write FOnSelectionChanging;
    property DragSelectCurrentRect: TRectF read FDragSelectCurrentRect write FDragSelectCurrentRect;
    property RectangleDragSelect: TRectangle read FRectangleDragSelect write FRectangleDragSelect;
    property Selection: TObjectList<TSelectableObject> read FSelection;
    property SelectableObject: TObjectList<TSelectableObject> read FSelectableObject;
    property Shift: TShiftState read FShift write FShift;
    property State: TDragState read FState write FState;

    function CalculateSnap(Target: single; Snap: single): single;
    function CalculateSnapPt(Target: TPointF; SnapX, SnapY: single): TPointF;
    procedure DeleteSelected; virtual;
    function FindSelectableObjectByPt(ViewportX, ViewportY: single): TSelectableObject;
    procedure LoadFromXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode; SegmentParent: TFmxObject); virtual;
    function MouseSelectRectOffset: TPointF;
    procedure MoveSelectedBy(DeltaPt: TPointF);
    function NewSelectableObject(ASegmentClass: TSelectableImageClass; AParent: TFmxObject): TSelectableObject;
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

constructor TDragManager.Create;
begin
  inherited;
  FSelection := TObjectList<TSelectableObject>.Create;
  Selection.OwnsObjects := False;
  FSelectableObject := TObjectList<TSelectableObject>.Create;
  SelectableObject.OwnsObjects := True;
  RectangleDragSelect := TRectangle.Create(nil);
  RectangleDragSelect.Fill.Color := $FFE0E0E0;
  RectangleDragSelect.Fill.Gradient.Color := TAlphaColorRec.Mediumblue;
  RectangleDragSelect.Fill.Gradient.Color1 := TAlphaColorRec.White;
  RectangleDragSelect.Opacity := 0.2;
  RectangleDragSelect.CornerType := TCornerType.Round;
  RectangleDragSelect.XRadius := 5;
  RectangleDragSelect.YRadius := 5;
  RectangleDragSelect.Fill.Kind := TBrushKind.Gradient;
  RectangleDragSelect.Stroke.Color := TAlphAColorRec.Blue;

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
  FreeAndNil(FSelectableObject);
  FreeAndNil(FSelection);
  FreeAndNil(FRectangleDragSelect);
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

function TDragManager.FindSelectableObjectByPt(ViewportX, ViewportY: single): TSelectableObject;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while not Assigned(Result) and (i < SelectableObject.Count) do
  begin
    if PtInRect(SelectableObject[i].BoundsRect, TPointF.Create(ViewportX, ViewportY)) then
      Result := SelectableObject[i];
    Inc(i);
  end;
end;

procedure TDragManager.LoadFromXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode; SegmentParent: TFmxObject);
var
  i: Integer;
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
          ASegment := NewSelectableObject(TSelectableImageClass( NewClass), SegmentParent);
          ASegment.LoadFromXML(XmlDoc, Child);
        end;
      end;
    end;
    Child := XmlNextSiblingNode(Child)
  end;
  for i := 0 to SelectableObject.Count - 1 do
  begin
    Child := XmlCreateChildNode(XmlDoc, Node, 'segment', '');
    SelectableObject[i].SaveToXML(XmlDoc, Child);
  end;
end;

function TDragManager.MouseSelectRectOffset: TPointF;
begin
  Result := TPointF.Create(MouseDownViewportPoint.X - DragSelectStartRect.Left, MouseDownViewportPoint.Y - DragSelectStartRect.Top)
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
  ChildNode := XmlFindChildNode(Node, 'selected');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    FSelected := ReadStr = 'true'
  end;
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
  if Selected then
    XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'selected', 'true')
  else
    XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'selected', 'false');
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
