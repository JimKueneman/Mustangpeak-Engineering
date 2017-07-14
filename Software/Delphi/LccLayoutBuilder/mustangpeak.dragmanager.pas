unit mustangpeak.dragmanager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Objects, FMX.Graphics;

type
  TDragState = (dsNone, dsDragging, dsSelectRect, dsDragPending, dsSelectRectPending);

type
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
    procedure SetEditMode(const Value: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property EditMode: Boolean read FEditMode write SetEditMode;
    property MouseButton: TMouseButton read FMouseButton write FMouseButton;
    property MouseCurrentViewportPoint: TPointF read FMouseCurrentViewportPoint write FMouseCurrentViewportPoint;
    property MouseDownViewportPoint: TPointF read FMouseDownViewportPoint write FMouseDownViewportPoint;
    property MousePreviousVewportPoint: TPointF read FMousePreviousVewportPoint write FMousePreviousVewportPoint;
    property DragSelectStartRect: TRectF read FDragSelectStartRect write FDragSelectStartRect;
    property DragSelectCurrentRect: TRectF read FDragSelectCurrentRect write FDragSelectCurrentRect;
    property RectangleDragSelect: TRectangle read FRectangleDragSelect write FRectangleDragSelect;
    property Shift: TShiftState read FShift write FShift;
    property State: TDragState read FState write FState;

    function CalculateSnap(Target: single; Snap: single): single;
    function CalculateSnapPt(Target: TPointF; SnapX, SnapY: single): TPointF;
    function MouseSelectRectOffset: TPointF;
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

destructor TDragManager.Destroy;
begin
  FreeAndNil(FRectangleDragSelect);
  inherited;
end;


function TDragManager.MouseSelectRectOffset: TPointF;
begin
  Result := TPointF.Create(MouseDownViewportPoint.X - DragSelectStartRect.Left, MouseDownViewportPoint.Y - DragSelectStartRect.Top)
end;

procedure TDragManager.SetEditMode(const Value: Boolean);
begin
  if FEditMode <> Value then
  begin
    FEditMode := Value;
  end;
end;

end.
