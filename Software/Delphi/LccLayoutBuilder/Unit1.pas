unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, System.UIConsts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Platform,
  System.Generics.Collections, System.ImageList, FMX.ImgList, FMX.Gestures;

type
  TTrackSegment = class(TRectangle)
  private
    FSelected: Boolean;
    FSelection: TSelection;
    FSegmentDragStart: TPointF;
    procedure SetSelected(const Value: Boolean);
  protected
    procedure SetSize(const AValue: TControlSize); override;
    procedure SetWidth(const Value: Single); override;
    procedure SetHeight(const Value: Single); override;
    property Selection: TSelection read FSelection write FSelection;
    property SegmentDragStart: TPointF read FSegmentDragStart write FSegmentDragStart;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SegmentDragBegin;
    procedure SegmentDragMove(DeltaXLocal, DeltaYLocal: single; SnapX, SnapY: Integer);
    procedure SetSnappedPosition(X, Y: single; SnapX, SnapY: Integer);
    property Selected: Boolean read FSelected write SetSelected;
  end;

type
  TDragState = (dsNone, dsDragging, dsSelecting);

type
  TFormLayoutBuilder = class(TForm)
    RectangleDragSelect: TRectangle;
    LayoutBkgnd: TLayout;
    StatusBar: TStatusBar;
    TextStatusMousePos: TText;
    ScrollBoxPanel: TScrollBox;
    LabelStatusSelection: TLabel;
    ImageList: TImageList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    GestureManager: TGestureManager;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBoxPanelDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure ScrollBoxPanelDragEnter(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure ScrollBoxPanelMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
    procedure ScrollBoxPanelDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure ScrollBoxPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ScrollBoxPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button4Click(Sender: TObject);
  private
    FSelections: TObjectList<TControl>;
    FDragStartPointLocal: TPointF;
    FDragStartPointScreen: TPointF;
    FDragStartPointAbsolute: TPointF;
    FDragCurrentPointScreen: TPointF;
    FDragCurrentPointAbsolute: TPointF;
    FDragCurrentPointLocal: TPointF;
    FDragState: TDragState;
    FDragMouseButton: TMouseButton;
    FDragShift: TShiftState;
    FTrackSegments: TObjectList<TTrackSegment>;
  protected
    property DragMouseButton: TMouseButton read FDragMouseButton write FDragMouseButton;
    property DragStartPointScreen: TPointF read FDragStartPointScreen write FDragStartPointScreen;
    property DragStartPointAbsolute: TPointF read FDragStartPointAbsolute write FDragStartPointAbsolute;
    property DragStartPointLocal: TPointF read FDragStartPointLocal write FDragStartPointLocal;
    property DragCurrentPointScreen: TPointF read FDragCurrentPointScreen write FDragCurrentPointScreen;
    property DragCurrentPointAbsolute: TPointF read FDragCurrentPointAbsolute write FDragCurrentPointAbsolute;
    property DragCurrentPointLocal: TPointF read FDragCurrentPointLocal write FDragCurrentPointLocal;
    property DragState: TDragState read FDragState write FDragState;
    property DragShift: TShiftState read FDragShift write FDragShift;

    function CreateSegment(ImageIndex: Integer; PosX, PosY: single; cx, cy: Integer): TTrackSegment;
    function FindTrackSegmentByPt(X, Y: single): TTrackSegment;
    procedure TrackSegmentsDeselectAll;
    procedure TrackSegmentsSelectAll;
    procedure TrackSegmentsSelectionByPt(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TrackSegmentsSelectionByRect(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure UpdateStatusBar;
  public
    { Public declarations }
    WindowService: IFMXWindowService;
    ScreenService: IFMXScreenService;

    property Selections: TObjectList<TControl> read FSelections write FSelections;
    property TrackSegments: TObjectList<TTrackSegment> read FTrackSegments write FTrackSegments;
    function CreateTurnoutRHSegment(PosX, PosY: single): TTrackSegment;
    function CreateTurnoutLHSegment(PosX, PosY: single): TTrackSegment;
    function CreateStraightSegment(PosX, PosY: single): TTrackSegment;
  end;

var
  FormLayoutBuilder: TFormLayoutBuilder;

implementation

{$R *.fmx}


procedure TFormLayoutBuilder.Button1Click(Sender: TObject);
begin
  CreateTurnoutRHSegment(Random(Round( ScrollBoxPanel.Width)) - 32, Random(Round( ScrollBoxPanel.Height)) - 32)
end;

procedure TFormLayoutBuilder.Button2Click(Sender: TObject);
begin
  CreateTurnoutLHSegment(Random(Round( ScrollBoxPanel.Width)) - 32, Random(Round( ScrollBoxPanel.Height)) - 32)
end;

procedure TFormLayoutBuilder.Button3Click(Sender: TObject);
begin
  CreateStraightSegment(Random(Round( ScrollBoxPanel.Width)) - 32, Random(Round( ScrollBoxPanel.Height)) - 32)
end;

procedure TFormLayoutBuilder.Button4Click(Sender: TObject);
var
  Pt: TPointF;
  Scale: single;
  Orientation: TScreenOrientation;
begin
  Pt := ScreenService.GetScreenSize;
  Scale := ScreenService.GetScreenScale;
  Orientation := ScreenService.GetScreenOrientation;
end;

function TFormLayoutBuilder.CreateSegment(ImageIndex: Integer; PosX, PosY: single; cx, cy: Integer): TTrackSegment;
var
  BitSize: TSizeF;
  Bitmap: TBitmap;
begin
  Result := TTrackSegment.Create(nil);
  BitSize.cx := cx;
  BitSize.cy := cy;
  Bitmap := ImageList.Bitmap(BitSize, ImageIndex);
  Result.Fill.Bitmap.Bitmap.Width := cx;
  Result.Fill.Bitmap.Bitmap.Height := cy;
  Result.Fill.Bitmap.Bitmap.CopyFromBitmap(Bitmap);
  Result.Fill.Kind := TBrushKind.Bitmap;
 // Result.Position.X := PosX;
 // Result.Position.Y := PosY;
  Result.SetSnappedPosition(PosX, PosY, cx, cy);
  Result.Width := cx;
  Result.Height := cy;
  Result.HitTest := False;
  Result.Stroke.Kind := TBrushKind.None;
  Result.Parent := ScrollBoxPanel;
  TrackSegments.Add(Result);
end;

function TFormLayoutBuilder.CreateStraightSegment(PosX, PosY: single): TTrackSegment;
begin
  Result := CreateSegment(2, PosX, PosY, 39, 9);
end;

function TFormLayoutBuilder.CreateTurnoutLHSegment(PosX, PosY: single): TTrackSegment;
begin
  Result := CreateSegment(1, PosX, PosY,39, 32);
end;

function TFormLayoutBuilder.CreateTurnoutRHSegment(PosX, PosY: single): TTrackSegment;
begin
   Result := CreateSegment(0, PosX, PosY, 39, 32);
end;

function TFormLayoutBuilder.FindTrackSegmentByPt(X, Y: single): TTrackSegment;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to TrackSegments.Count - 1 do
  begin
    if TrackSegments[i].PointInObject(X, Y) then
    begin
      Result := TrackSegments[i];
      Break;
    end;
  end;
end;

procedure TFormLayoutBuilder.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Selections.Clear;
end;

procedure TFormLayoutBuilder.FormCreate(Sender: TObject);
var
  Segment: TTrackSegment;
  BitSize: TSizeF;
  Bitmap: TBitmap;
begin
  FSelections := TObjectList<TControl>.Create;
  Selections.OwnsObjects := False;
  FTrackSegments := TObjectList<TTrackSegment>.Create;
  TrackSegments.OwnsObjects := True;
  RectangleDragSelect.Parent := nil;
  TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService);
  TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, WindowService);

 // ScrollBoxPanel.AniCalculations.Animation := False;
  ScrollBoxPanel.AniCalculations.BoundsAnimation := False;
 // ScrollBoxPanel.AniCalculations.TouchTracking := [];
end;

procedure TFormLayoutBuilder.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSelections);
end;

procedure TFormLayoutBuilder.ScrollBoxPanelDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(Point.X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(Point.Y, ffFixed, 4, 4);
end;

procedure TFormLayoutBuilder.ScrollBoxPanelDragEnter(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(Point.X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(Point.Y, ffFixed, 4, 4);
end;

procedure TFormLayoutBuilder.ScrollBoxPanelDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(Point.X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(Point.Y, ffFixed, 4, 4);
  Operation := TDragOperation.None
end;

procedure TFormLayoutBuilder.ScrollBoxPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  i: Integer;
begin
  case Button of
    TMouseButton.mbLeft :
      begin
        DragMouseButton := Button;

        FDragStartPointLocal.X := X;
        FDragStartPointLocal.Y := Y;
        DragStartPointScreen := ClientToScreen(FDragStartPointLocal);
        DragStartPointAbsolute := ScrollBoxPanel.LocalToAbsolute(DragStartPointLocal);

        DragShift := Shift;
        TrackSegmentsSelectionByPt(Button, Shift, X, Y);

        case DragState of
          dsNone :
            begin

            end;
          dsDragging :
            begin
              for i := 0 to Selections.Count - 1 do
                (Selections[i] as TTrackSegment).SegmentDragBegin;
            end;
          dsSelecting :
            begin
              RectangleDragSelect.Position.X := X;
              RectangleDragSelect.Position.Y := Y;
              RectangleDragSelect.Width := 0;
              RectangleDragSelect.Height := 0;
              RectangleDragSelect.Parent := ScrollBoxPanel;
              ScrollBoxPanel.Root.Captured := ScrollBoxPanel;
            end;
        end;
        UpdateStatusBar;
      end;
    TMouseButton.mbRight :
      begin

      end;
    TMouseButton.mbMiddle :
      begin

      end;
  end;
end;

procedure TFormLayoutBuilder.ScrollBoxPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  i: Integer;
begin
  case DragMouseButton of
    TMouseButton.mbLeft :
      begin
        FDragCurrentPointLocal.X := X;
        FDragCurrentPointLocal.Y := Y;
        DragCurrentPointScreen := ClientToScreen(DragCurrentPointLocal);
        DragCurrentPointAbsolute := ScrollBoxPanel.LocalToAbsolute(DragCurrentPointLocal);

        TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(Y, ffFixed, 4, 4);

        case DragState of
          dsNone :
            begin

            end;
          dsDragging :
            begin
              for i := 0 to Selections.Count - 1 do
                (Selections[i] as TTrackSegment).SegmentDragMove(DragCurrentPointLocal.X - DragStartPointLocal.X, DragCurrentPointLocal.Y - DragStartPointLocal.Y, Trunc((Selections[i] as TTrackSegment).Width), Trunc((Selections[i] as TTrackSegment).Height));
            end;
          dsSelecting :
            begin
              if X < 0 then
                X := 0;
              if X > ScrollBoxPanel.Width then
                X := ScrollBoxPanel.Width;
              if Y < 0 then
                Y := 0;
              if Y > ScrollBoxPanel.Height then
                Y := ScrollBoxPanel.Height;

              if X - DragStartPointLocal.X < 0 then
              begin
                RectangleDragSelect.Position.X := X;
                RectangleDragSelect.Width := DragStartPointLocal.X - X;
              end else
              begin
                RectangleDragSelect.Position.X := DragStartPointLocal.X;
                RectangleDragSelect.Width := X - DragStartPointLocal.X;
              end;
              if Y - DragStartPointLocal.Y < 0 then
              begin
                RectangleDragSelect.Position.Y := Y;
                RectangleDragSelect.Height := DragStartPointLocal.Y - Y;
              end else
              begin
                RectangleDragSelect.Position.Y := DragStartPointLocal.Y;
                RectangleDragSelect.Height := Y - DragStartPointLocal.Y;
              end;
              TrackSegmentsSelectionByRect(DragMouseButton, Shift, X, Y);
              UpdateStatusBar;
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
end;

procedure TFormLayoutBuilder.ScrollBoxPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  RectangleDragSelect.Parent := nil;
  DragState := dsNone;
  UpdateStatusBar;
end;

procedure TFormLayoutBuilder.TrackSegmentsDeselectAll;
var
  I: Integer;
begin
  for I := 0 to Selections.Count - 1 do
    (Selections[i] as TTrackSegment).Selected := False;
  Selections.Clear;
end;

procedure TFormLayoutBuilder.TrackSegmentsSelectAll;
var
  I: Integer;
begin
  Selections.Clear;
  for I := 0 to TrackSegments.Count - 1 do
  begin
    TrackSegments[i].Selected := True;
    Selections.Add(TrackSegments[i])
  end;
end;

procedure TFormLayoutBuilder.TrackSegmentsSelectionByPt(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  HitSegment: TTrackSegment;
  InSelection: Boolean;
  i: Integer;
begin
  HitSegment := FindTrackSegmentByPt(DragStartPointAbsolute.X, DragStartPointAbsolute.Y);
  InSelection := Selections.IndexOf(HitSegment) > -1;

  if Assigned(HitSegment) then
  begin
    DragState := dsDragging;

    if not InSelection and not (ssShift in Shift) then
      TrackSegmentsDeselectAll;

    if not InSelection then
    begin
      Selections.Add(HitSegment);
      HitSegment.Selected := True;
    end;
  end else
  begin
    if not (ssShift in Shift) then
      TrackSegmentsDeselectAll;
    DragState := dsSelecting
  end;
end;

procedure TFormLayoutBuilder.TrackSegmentsSelectionByRect(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  i: Integer;
begin
  if not (ssShift in Shift) then
    TrackSegmentsDeselectAll;

  if (RectangleDragSelect.Width > 0) and (RectangleDragSelect.Height > 0) then
  begin
    for i := 0 to TrackSegments.Count - 1 do
    begin
      if not IsRectEmpty( TRectF.Intersect(TrackSegments[i].AbsoluteRect, RectangleDragSelect.AbsoluteRect)) then
      if Selections.IndexOf(TrackSegments[i]) < 0 then
      begin
        Selections.Add(TrackSegments[i]);
        (TrackSegments[i] as TTrackSegment).Selected := True;
      end;
    end;
  end;
end;

procedure TFormLayoutBuilder.UpdateStatusBar;
begin
  LabelStatusSelection.Text :=  'Selections: ' + IntToStr(Selections.Count);
end;

{ TTrackSegment }

constructor TTrackSegment.Create(AOwner: TComponent);
begin
  inherited;
  FSelection := TSelection.Create(Self);
  Selection.HitTest := False;
  Selection.Parent := nil;
end;

destructor TTrackSegment.Destroy;
begin
  FreeAndNil(FSelection);
  inherited;
end;

procedure TTrackSegment.SegmentDragBegin;
begin
  SegmentDragStart := BoundsRect.TopLeft;
end;

procedure TTrackSegment.SegmentDragMove(DeltaXLocal, DeltaYLocal: single; SnapX, SnapY: Integer);
begin
  SetSnappedPosition(SegmentDragStart.X + DeltaXLocal, SegmentDragStart.Y + DeltaYLocal, SnapX, SnapY);
end;

procedure TTrackSegment.SetHeight(const Value: Single);
begin
  inherited;
  Selection.Height := Value;
end;

procedure TTrackSegment.SetSelected(const Value: Boolean);
begin
  if Value <> FSelected then
  begin
    FSelected := Value;
    if Value then
      Selection.Parent := Self
    else
      Selection.Parent := nil;
  end;
end;

procedure TTrackSegment.SetSize(const AValue: TControlSize);
begin
  inherited;
end;

procedure TTrackSegment.SetSnappedPosition(X, Y: single; SnapX, SnapY: Integer);
var
  TruncPoint: TPoint;
begin

SnapX := 39;
SnapY := 32;

  TruncPoint.X := Trunc(X);
  TruncPoint.Y := Trunc(Y);
  if TruncPoint.X mod SnapX <> 0 then
  begin
    Position.X := ((TruncPoint.X div SnapX) * SnapX);
    if TruncPoint.X mod SnapX > (SnapX div 2) then
      Position.X := Position.X + SnapX
  end;
  if TruncPoint.Y mod SnapY <> 0 then
  begin
    Position.Y := ((TruncPoint.Y div SnapY) * SnapY);
    if TruncPoint.Y mod SnapY > (SnapY div 2) then
      Position.Y := Position.Y + SnapY
  end;
end;

procedure TTrackSegment.SetWidth(const Value: Single);
begin
  inherited;
  Selection.Width := Value;
end;

end.
