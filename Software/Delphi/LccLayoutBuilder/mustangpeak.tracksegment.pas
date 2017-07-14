unit mustangpeak.tracksegment;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics, System.Generics.Collections;

const
  BASE_SEGMENT_WIDTH = 80;
  BASE_SEGMENT_HEIGHT = 20;

type
  TTurnoutRouting = (Straight, Diverging);

type
  TTrackSegmentManager = class;

  TTrackSegment = class(TImage)
  private
    FOpacity: single;
    FBrushColor: TAlphaColor;
    FBrushWidth: single;
    FBitmapGenerated: Boolean;
    FBkGndColor: TAlphaColor;
    FOccupied: Boolean;
    FManager: TTrackSegmentManager;
    FSelected: Boolean;
    FOnSelectedChanged: TNotifyEvent;
    procedure SetSelected(const Value: Boolean);
    procedure SetBrushWidth(const Value: single);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetOpacity(const Value: single);
    procedure SetBkGndColor(const Value: TAlphaColor);
    procedure SetOccupied(const Value: Boolean);
    function GetSelected: Boolean;
  protected
    procedure DoSelectedChanged; virtual;
    procedure GenerateBitmap; virtual;
    procedure Paint; override;
    procedure PaintSelection; virtual;
    procedure DrawIndicatorBulb(ACanvas: TCanvas; CenterPt: TPointF; Diameter: single; Lit: Boolean; Opacity: single);
    procedure SetSize(const AValue: TControlSize); override;
    procedure SetWidth(const Value: Single); override;
    procedure SetHeight(const Value: Single); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Click; override;
    procedure InvalidateBitmap;
    procedure MoveBy(DeltaPt: TPointF);
    procedure MoveTo(ViewportPt: TPointF);

    property BitmapGenerated: Boolean read FBitmapGenerated;
    property Opacity: single read FOpacity write SetOpacity;
    property BkGndColor: TAlphaColor read FBkGndColor write SetBkGndColor;
    property BrushColor: TAlphaColor read FBrushColor write SetColor;
    property BrushWidth: single read FBrushWidth write SetBrushWidth;
    property Occupied: Boolean read FOccupied write SetOccupied;
    property Manager: TTrackSegmentManager read FManager;
    property Selected: Boolean read GetSelected write SetSelected;

    property OnSelectedChanged: TNotifyEvent read FOnSelectedChanged write FOnSelectedChanged;
  end;
  TTrackSegmentClass = class of TTrackSegment;

  TTrackSegmentStraight = class(TTrackSegment)
  protected
    procedure GenerateBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TTrackSegmentTurnout = class(TTrackSegment)
  private
    FRouting: TTurnoutRouting;
    procedure SetRouting(const Value: TTurnoutRouting);
  protected
    procedure GenerateBitmap; override;
  public
    property Routing: TTurnoutRouting read FRouting write SetRouting;

    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TTrackSegmentManager = class(TPersistent)
  private
    FSegment: TObjectList<TTrackSegment>;
    FSelection: TObjectList<TTrackSegment>;
  public
    constructor Create;
    destructor Destroy; override;
    property Segment: TObjectList<TTrackSegment> read FSegment;
    property Selection: TObjectList<TTrackSegment> read FSelection;

    function FindSegmentByPt(ViewportX, ViewportY: single): TTrackSegment;

    function NewSegment(ASegmentClass: TTrackSegmentClass; AParent: TFmxObject): TTrackSegment;
    function CalculateSnap(Target: single; Snap: single): single;
    function CalculateSnapPt(Target: TPointF; SnapX, SnapY: single): TPointF;
    procedure MoveSelectedBy(DeltaPt: TPointF);
    procedure SelectAll;
    function SelectByPt(ViewportX, ViewportY: single): Boolean;
    function SelectByRect(SelectRect: TRectF; Combine: Boolean): Boolean;
    function SelectionBounds: TRectF;
    procedure UnSelectAll;
  end;

implementation

{ TTrackSegment }

procedure TTrackSegment.Click;
begin

end;

constructor TTrackSegment.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FBrushColor := TAlphaColorRec.Black;
  FBkGndColor := TAlphaColorRec.White;
  FBrushWidth := 3;
  FOpacity := 1;
end;

destructor TTrackSegment.Destroy;
begin
  inherited;
end;

procedure TTrackSegment.DoSelectedChanged;
begin
  if Assigned(Manager) then
  begin
    if Selected then
    begin
      if Manager.Selection.IndexOf(Self) < 0 then
        Manager.Selection.Add(Self)
    end else
      Manager.Selection.Remove(Self);
  end;
  if Assigned(OnSelectedChanged) then
    OnSelectedChanged(Self);
end;

procedure TTrackSegment.GenerateBitmap;
begin
  FBitmapGenerated := True;
end;

function TTrackSegment.GetSelected: Boolean;
begin
  Result := FSelected;
end;

procedure TTrackSegment.InvalidateBitmap;
begin
  if BitmapGenerated then
    GenerateBitmap;
end;

procedure TTrackSegment.Paint;
begin
  if not BitmapGenerated then
    GenerateBitmap;
  inherited;
end;

procedure TTrackSegment.PaintSelection;
var
  R: TRectF;
begin
  if Selected then
  begin
    R := LocalRect;
    R.Inflate(-0.5, -0.5);
    Bitmap.Canvas.DrawDashRect(R, 0, 0, AllCorners, 1000, $FF1072C5);
  end;
end;

procedure TTrackSegment.DrawIndicatorBulb(ACanvas: TCanvas; CenterPt: TPointF; Diameter: single; Lit: Boolean; Opacity: single);
var
  CanvasState: TCanvasSaveState;
  ARect: TRectF;
begin  CanvasState := ACanvas.SaveState;
  try
    ACanvas.StrokeThickness := 2;
    ACanvas.Stroke.Color := TAlphaColorRec.Black;
    ACanvas.Fill.Kind := TBrushKind.Gradient;
    ACanvas.Fill.Gradient.Style := TGradientStyle.Radial;

    ARect := TRectF.Create(CenterPt.X-(Diameter/2), CenterPt.Y-(Diameter/2), CenterPt.X+(Diameter/2), CenterPt.Y+(Diameter/2));
    ACanvas.Fill.Color := TAlphaColorRec.Black;
    ACanvas.Fill.Gradient.Color := TAlphaColorRec.Gray;
    ACanvas.FillEllipse(ARect, Opacity);
    InflateRect(ARect, -2, -2);

    if Lit then
    begin
      ACanvas.Fill.Color := TAlphaColorRec.Green;
      ACanvas.Fill.Gradient.Color := TAlphaColorRec.Green;
  //    ACanvas.Fill.Gradient.Color1 := TAlphaColorRec.Silver;
    end else
    begin
      ACanvas.Fill.Color := TAlphaColorRec.Silver;
      ACanvas.Fill.Gradient.Color := TAlphaColorRec.Silver;
   //   ACanvas.Fill.Gradient.Color1 := TAlphaColorRec.Black;
    end;

    ACanvas.FillEllipse(ARect, Opacity);
  finally
    ACanvas.RestoreState(CanvasState);
  end;
end;

procedure TTrackSegment.SetBkGndColor(const Value: TAlphaColor);
begin
  if FBkGndColor <> Value then
  begin
    FBkGndColor := Value;
    InvalidateBitmap;
  end;
end;

procedure TTrackSegment.SetBrushWidth(const Value: single);
begin
  if FBrushWidth <> Value then
  begin
    FBrushWidth := Value;
    InvalidateBitmap;
  end;
end;

procedure TTrackSegment.SetColor(const Value: TAlphaColor);
begin
  if FBrushColor <> Value then
  begin
    FBrushColor := Value;
    InvalidateBitmap;
  end;
end;

procedure TTrackSegment.SetHeight(const Value: Single);
begin
  inherited;
//  Selection.Height := Value;
end;

procedure TTrackSegment.SetOccupied(const Value: Boolean);
begin
  if FOccupied <> Value then
  begin
    FOccupied := Value;
    InvalidateBitmap;
  end;
end;

procedure TTrackSegment.SetOpacity(const Value: single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    InvalidateBitmap;
  end;
end;

procedure TTrackSegment.SetSelected(const Value: Boolean);
begin
  if Value <> Selected then
  begin
    FSelected := Value;
    InvalidateBitmap;
    InvalidateRect(BoundsRect);
 //   Selection.HideSelection := not Value;
    DoSelectedChanged;
  end;
end;

procedure TTrackSegment.SetSize(const AValue: TControlSize);
begin
  inherited;
end;

procedure TTrackSegment.MoveBy(DeltaPt: TPointF);
begin
  MoveTo(TPointF.Create(Position.X + DeltaPt.X, Position.Y + DeltaPt.Y))
end;

procedure TTrackSegment.MoveTo(ViewportPt: TPointF);
begin
  Position.X := ViewportPt.X;
  Position.Y := ViewportPt.Y;
end;

procedure TTrackSegment.SetWidth(const Value: Single);
begin
  inherited;
//  Selection.Width := Value;
end;

{ TTrackSegmentStraight }

procedure TTrackSegmentStraight.Click;
begin
  inherited;
  Occupied := not Occupied;
end;

constructor TTrackSegmentStraight.Create(AOwner: TComponent);
begin
  inherited;
  Width := BASE_SEGMENT_WIDTH;
  Height := BASE_SEGMENT_HEIGHT;
end;

procedure TTrackSegmentStraight.GenerateBitmap;
var
  MiddleX, MiddleY: single;
  Pt1, Pt2: TPointF;
begin
  inherited;
  MiddleX := BASE_SEGMENT_WIDTH/2;
  MiddleY := BASE_SEGMENT_HEIGHT/2;

  Bitmap.SetSize(BASE_SEGMENT_WIDTH, BASE_SEGMENT_HEIGHT);

  Bitmap.Canvas.BeginScene;
  try
    Bitmap.Canvas.StrokeThickness := BrushWidth;
    Bitmap.Canvas.Stroke.Color := BrushColor;

    Bitmap.Canvas.Clear(BkGndColor);

    Pt1.X := Bitmap.Canvas.StrokeThickness + 1;
    Pt1.Y := MiddleY;
    Pt2.X := BASE_SEGMENT_WIDTH - BrushWidth - 1;
    Pt2.Y := MiddleY;
    Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);

    Pt1.X := Bitmap.Canvas.StrokeThickness + 1;
    Pt1.Y := MiddleY + BrushWidth*2;
    Pt2.X := Bitmap.Canvas.StrokeThickness + 1;
    Pt2.Y := MiddleY - BrushWidth*2;
    Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);

    Pt1.X := BASE_SEGMENT_WIDTH - BrushWidth - 1;
    Pt1.Y := MiddleY + BrushWidth*2;
    Pt2.X := BASE_SEGMENT_WIDTH - BrushWidth - 1;
    Pt2.Y := MiddleY - BrushWidth*2;
    Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);

    DrawIndicatorBulb(Bitmap.Canvas, TPointF.Create(BASE_SEGMENT_WIDTH/2, BASE_SEGMENT_HEIGHT/2), BASE_SEGMENT_HEIGHT-2, Occupied, Opacity);
    PaintSelection;
  finally
    Bitmap.Canvas.EndScene;
  end;
end;

{ TTrackSegmentTurnout }

procedure TTrackSegmentTurnout.Click;
begin
  if Routing = TTurnoutRouting.Straight then
    Routing := TTurnoutRouting.Diverging
  else
    Routing := TTurnoutRouting.Straight;
  InvalidateBitmap;
end;

constructor TTrackSegmentTurnout.Create(AOwner: TComponent);
begin
  inherited;
  Width := BASE_SEGMENT_WIDTH;
  Height := BASE_SEGMENT_HEIGHT * 2;
end;

procedure TTrackSegmentTurnout.GenerateBitmap;
var
  OneQuarterX, OneQuarterY, ThreeQuartersX, ThreeQuartersY, MiddleX, MiddleY, RealHeight, Slope: single;
  Pt1, Pt2: TPointF;
begin
  inherited;
  RealHeight := BASE_SEGMENT_HEIGHT * 2.0;

  OneQuarterX := BASE_SEGMENT_WIDTH * 0.25;
  OneQuarterY := RealHeight * 0.25;
  MiddleX := BASE_SEGMENT_WIDTH * 0.5;
  MiddleY := RealHeight * 0.5;
  ThreeQuartersX := BASE_SEGMENT_WIDTH * 0.75;
  ThreeQuartersY := RealHeight * 0.75;

  Bitmap.SetSize(BASE_SEGMENT_WIDTH, Round( RealHeight));

  Bitmap.Canvas.BeginScene;
  try
    Bitmap.Canvas.StrokeThickness := BrushWidth;
    Bitmap.Canvas.Stroke.Color := BrushColor;
    Bitmap.Canvas.Clear(BkGndColor);

    // Draw the straight through route and the end caps
    Pt1.X := BrushWidth + 1;
    Pt1.Y := OneQuarterY;
    Pt2.X := BASE_SEGMENT_WIDTH - BrushWidth - 1;
    Pt2.Y := OneQuarterY;
    Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);

    Pt1.X := BrushWidth + 1;
    Pt1.Y := OneQuarterY + BrushWidth*2;
    Pt2.X := BrushWidth + 1;
    Pt2.Y := OneQuarterY - BrushWidth*2;
    Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);

    Pt1.X := BASE_SEGMENT_WIDTH - BrushWidth - 1;
    Pt1.Y := OneQuarterY + BrushWidth*2;
    Pt2.X := BASE_SEGMENT_WIDTH - BrushWidth - 1;
    Pt2.Y := OneQuarterY - BrushWidth*2;
    Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);


    // Draw the diverging route short straight track and the end cap
    Pt1.X := ThreeQuartersX;
    Pt1.Y := ThreeQuartersY;
    Pt2.X := BASE_SEGMENT_WIDTH - BrushWidth - 1;
    Pt2.Y := ThreeQuartersY;
    Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);

    Pt1.X := BASE_SEGMENT_WIDTH - BrushWidth - 1;
    Pt1.Y := ThreeQuartersY + BrushWidth*2;
    Pt2.X := BASE_SEGMENT_WIDTH - BrushWidth - 1;
    Pt2.Y := ThreeQuartersY - BrushWidth*2;
    Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);

    Pt1.X := OneQuarterX;
    Pt1.Y := OneQuarterY;
    Pt2.X := ThreeQuartersX;
    Pt2.Y := ThreeQuartersY;

    if Routing = TTurnoutRouting.Straight then
    begin
      // y = mx+b
      // y2-y1 = m(x2-x1)  -> (y2-y1)/m = x2-x1 -> -[(y2-y1)/m - x2] = x1
      Slope := (Pt1.Y - Pt2.Y)/(Pt1.X - Pt2.X);
      Pt1.Y := Pt1.Y + (BrushWidth*2);
      Pt1.X := ((Pt1.Y-Pt2.Y)/Slope) + Pt2.X;
      Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);

      // Draw the little nub
      Pt2.X := Pt1.X;
      Pt2.Y := Pt1.Y;
      Pt1.X := Pt1.X - (BrushWidth);
      Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);

    end else
    begin
      Bitmap.Canvas.DrawLine(Pt1, Pt2, Opacity);
    end;

    PaintSelection;
  finally
    Bitmap.Canvas.EndScene;
  end;
end;

procedure TTrackSegmentTurnout.SetRouting(const Value: TTurnoutRouting);
begin
  if FRouting <> Value then
  begin
    FRouting := Value;
    InvalidateBitmap;
  end;
end;

{ TTrackSegmentManager }


function TTrackSegmentManager.CalculateSnap(Target, Snap: single): single;
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

function TTrackSegmentManager.CalculateSnapPt(Target: TPointF; SnapX, SnapY: single): TPointF;
begin
  Result.X := CalculateSnap(Target.X, SnapX);
  Result.Y := CalculateSnap(Target.Y, SnapY)
end;

constructor TTrackSegmentManager.Create;
begin
  FSegment := TObjectList<TTrackSegment>.Create;
  FSelection := TObjectList<TTrackSegment>.Create;
  Selection.OwnsObjects := False;
end;

destructor TTrackSegmentManager.Destroy;
begin
  Selection.Clear;
  FreeAndNil(FSelection);
  Segment.Clear;
  FreeAndNil(FSegment);
end;

function TTrackSegmentManager.FindSegmentByPt(ViewportX, ViewportY: single): TTrackSegment;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while not Assigned(Result) and (i < Segment.Count) do
  begin
    if PtInRect(Segment[i].BoundsRect, TPointF.Create(ViewportX, ViewportY)) then
      Result := Segment[i];
    Inc(i);
  end;
end;

procedure TTrackSegmentManager.MoveSelectedBy(DeltaPt: TPointF);
var
  i: Integer;
begin
  for i := 0 to Selection.Count - 1 do
    Selection[i].MoveBy(DeltaPt);
end;

function TTrackSegmentManager.NewSegment(ASegmentClass: TTrackSegmentClass; AParent: TFmxObject): TTrackSegment;
begin
  Result := ASegmentClass.Create(nil);
  Result.FManager := Self;
  Result.Parent := AParent;
  Result.BrushWidth := 2;
  Segment.Add(Result);
end;

procedure TTrackSegmentManager.SelectAll;
var
  i: Integer;
begin
  Selection.Clear;
  for i := 0 to Segment.Count - 1 do
    Segment[i].Selected := True;
end;

function TTrackSegmentManager.SelectByPt(ViewportX, ViewportY: single): Boolean;
var
  i: Integer;
begin
  i := 0;
  Result := False;
  while (i < Segment.Count) and not Result do
  begin
    if PtInRect(Segment[i].BoundsRect, TPointF.Create(ViewportX, ViewportY)) then
    begin
      Segment[i].Selected := True;
      Result := True;
    end;
    Inc(i);
  end;
end;

function TTrackSegmentManager.SelectByRect(SelectRect: TRectF; Combine: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Segment.Count - 1 do
  begin
    if IntersectRect(Segment[i].BoundsRect, SelectRect) then
    begin
      Segment[i].Selected := True;
      Result := True;
    end else
    begin
      if not Combine then
        Segment[i].Selected := False;
    end;
  end;
end;

function TTrackSegmentManager.SelectionBounds: TRectF;
var
  i: Integer;
begin
  Result := Result.Empty;
  if Selection.Count > 0 then
    Result := Selection[0].BoundsRect;
  for i := 1 to Selection.Count - 1 do
    UnionRect(Result, Result, Selection[i].BoundsRect)
end;

procedure TTrackSegmentManager.UnSelectAll;
var
  i: Integer;
begin
  for i := Selection.Count - 1 downto 0 do
    Selection[i].Selected := False;
end;

end.
