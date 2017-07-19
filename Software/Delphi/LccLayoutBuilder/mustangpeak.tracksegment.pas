unit mustangpeak.tracksegment;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics, System.Generics.Collections,
  mustangpeak.dragmanager, mustangpeak.xmlutilities;

const
  BASE_SEGMENT_WIDTH = 80;
  BASE_SEGMENT_HEIGHT = 20;

type
  TTurnoutRouting = (Straight, Diverging);

type
  TTrackSegmentManager = class;

  TTrackSegment = class(TSelectableObject)
  private
    FBrushColor: TAlphaColor;
    FBrushWidth: single;
    FBitmapGenerated: Boolean;
    FBkGndColor: TAlphaColor;
    FOccupied: Boolean;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetBkGndColor(const Value: TAlphaColor);
    procedure SetOccupied(const Value: Boolean);
  protected
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
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode); override;
    procedure InvalidateBitmap;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode); override;
    procedure VisualPropertyUpdated; override;

    property BitmapGenerated: Boolean read FBitmapGenerated;
    property BkGndColor: TAlphaColor read FBkGndColor write SetBkGndColor;
    property BrushColor: TAlphaColor read FBrushColor write SetColor;
    property Occupied: Boolean read FOccupied write SetOccupied;
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
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode); override;
  end;

  TTrackSegmentManager = class(TDragManager)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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

procedure TTrackSegment.GenerateBitmap;
begin
  FBitmapGenerated := True;
end;

procedure TTrackSegment.InvalidateBitmap;
begin
  if BitmapGenerated then
    GenerateBitmap;
end;

procedure TTrackSegment.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FBkGndColor, SizeOf(BkGndColor));
  Stream.Read(FBrushColor, SizeOf(BrushColor));
  Stream.Read(FOccupied, SizeOf(Occupied));
end;

procedure TTrackSegment.LoadFromXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode);
var
  ReadStr: string;
  ChildNode: TMustangpeakXmlNode;
begin
  inherited;
  ChildNode := XmlFindChildNode(Node, 'bkgndcolor');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    if ReadStr <> '' then FBkGndColor := StrToInt64(ReadStr)
  end;
  ChildNode := XmlFindChildNode(Node, 'brushcolor');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    if ReadStr <> '' then FBrushColor := StrToInt64(ReadStr)
  end;
  ChildNode := XmlFindChildNode(Node, 'position.x');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    if ReadStr <> '' then Position.X := StrToFloat(ReadStr)
  end;
  ChildNode := XmlFindChildNode(Node, 'position.y');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    if ReadStr <> '' then Position.Y := StrToFloat(ReadStr)
  end;
  ChildNode := XmlFindChildNode(Node, 'width');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    if ReadStr <> '' then Width := StrToFloat(ReadStr)
  end;
  ChildNode := XmlFindChildNode(Node, 'height');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    if ReadStr <> '' then Height := StrToFloat(ReadStr)
  end;

  ChildNode := XmlFindChildNode(Node, 'occupied');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    FOccupied := ReadStr = 'true'
  end;
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

procedure TTrackSegment.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(BkGndColor, SizeOf(BkGndColor));
  Stream.Write(BrushColor, SizeOf(BrushColor));
  Stream.Write(Occupied, SizeOf(Occupied));
end;

procedure TTrackSegment.SaveToXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode);
begin
  inherited;
  XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'bkgndcolor', IntToStr(BkGndColor));
  XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'brushcolor', IntToStr(BrushColor));
  if Occupied then
    XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'occupied', 'true')
  else
    XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'occupied', 'false');
  XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'position.x', FloatToStr(Position.X));
  XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'position.y', FloatToStr(Position.Y));
  XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'width', FloatToStr(Width));
  XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'height', FloatToStr(Height));
end;

procedure TTrackSegment.SetBkGndColor(const Value: TAlphaColor);
begin
  if FBkGndColor <> Value then
  begin
    FBkGndColor := Value;
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

procedure TTrackSegment.SetSize(const AValue: TControlSize);
begin
  inherited;
end;

procedure TTrackSegment.SetWidth(const Value: Single);
begin
  inherited;
//  Selection.Width := Value;
end;

procedure TTrackSegment.VisualPropertyUpdated;
begin
  InvalidateBitmap;
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

procedure TTrackSegmentTurnout.LoadFromStream(Stream: TStream);
begin
  inherited;
  Stream.Read(FRouting, SizeOf(FRouting));
end;

procedure TTrackSegmentTurnout.LoadFromXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode);
var
  ChildNode: TMustangpeakXmlNode;
  ReadStr: string;
begin
  inherited;
  ChildNode := XmlFindChildNode(Node, 'routing');
  if Assigned(ChildNode) then
  begin
    ReadStr := XmlNodeTextContent(ChildNode);
    if ReadStr = 'diverging' then Routing := Diverging;
  end;
end;

procedure TTrackSegmentTurnout.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FRouting, SizeOf(Routing))
end;

procedure TTrackSegmentTurnout.SaveToXML(XmlDoc: TMustangpeakXmlDocument; Node: TMustangpeakXmlNode);
begin
  inherited;
  case Routing of
     Straight  : XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'routing', 'straight');
     Diverging : XmlNodeSetTextContentForceCreate(XmlDoc, Node, 'routing', 'diverging')
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

constructor TTrackSegmentManager.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TTrackSegmentManager.Destroy;
begin
  inherited;
end;

initialization
  RegisterClass(TTrackSegmentStraight);
  RegisterClass(TTrackSegmentTurnout);

finalization

end.
