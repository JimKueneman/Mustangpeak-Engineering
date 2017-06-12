unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, System.UIConsts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Platform,
  System.Generics.Collections, System.ImageList, FMX.ImgList, FMX.Gestures, mustangpeak.tracksegment,
  System.Actions, FMX.ActnList, FMX.MultiView, FMX.Ani, Math;

type
  TDragState = (dsNone, dsDragging, dsSelectRect, dsDragPending, dsSelectRectPending);

type
  TDragManager = class(TPersistent)
  private
    FDragState: TDragState;
    FDragStartViewportPoint: TPointF;
    FDragMouseButton: TMouseButton;
    FDragCurrentViewportPoint: TPointF;
    FDragShift: TShiftState;
    FEditMode: Boolean;
    procedure SetEditMode(const Value: Boolean);
  protected
    property DragMouseButton: TMouseButton read FDragMouseButton write FDragMouseButton;
    property DragStartViewportPoint: TPointF read FDragStartViewportPoint write FDragStartViewportPoint;
    property DragCurrentViewportPoint: TPointF read FDragCurrentViewportPoint write FDragCurrentViewportPoint;
    property DragState: TDragState read FDragState write FDragState;
    property DragShift: TShiftState read FDragShift write FDragShift;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; ViewportX, ViewportY: Single);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; ViewportX, ViewportY: Single);
    procedure MouseMove(Button: TMouseButton; Shift: TShiftState; ViewportX, ViewportY: Single);

    property EditMode: Boolean read FEditMode write SetEditMode;
  end;


type
  TFormLayoutBuilder = class(TForm)
    RectangleDragSelect: TRectangle;
    LayoutBkgnd: TLayout;
    StatusBar: TStatusBar;
    TextStatusMousePos: TText;
    ScrollBoxPanel: TScrollBox;
    LabelStatusSelection: TLabel;
    ImageList: TImageList;
    GestureManager: TGestureManager;
    CheckBoxEditMode: TCheckBox;
    MultiView1: TMultiView;
    LayoutMainDrawer: TLayout;
    ActionListMain: TActionList;
    ActionNewStraightSegment: TAction;
    ActionNewTurnoutSegment: TAction;
    RectangleMainMenu: TRectangle;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Button4: TButton;
    LabelTrackSegmentCount: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBoxPanelDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure ScrollBoxPanelDragEnter(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure ScrollBoxPanelMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
    procedure ScrollBoxPanelDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure ScrollBoxPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ScrollBoxPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBoxEditModeChange(Sender: TObject);
    procedure ActionNewStraightSegmentExecute(Sender: TObject);
    procedure ActionNewTurnoutSegmentExecute(Sender: TObject);
  private
    FDragManager: TDragManager;
    FTrackSegmentManager: TTrackSegmentManager;
  protected
    property DragManager: TDragManager read FDragManager write FDragManager;
    procedure ScrollWindowClientToViewport(var ClientX, ClientY: single);
    procedure UpdateStatusBar;
  public
    { Public declarations }
    WindowService: IFMXWindowService;
    ScreenService: IFMXScreenService;

    property TrackSegmentManager: TTrackSegmentManager read FTrackSegmentManager write FTrackSegmentManager;
  end;

var
  FormLayoutBuilder: TFormLayoutBuilder;

implementation

{$R *.fmx}


procedure TFormLayoutBuilder.ActionNewStraightSegmentExecute(Sender: TObject);
var
  Segment: TTrackSegment;
  FinalX, FinalY: Single;
begin
  Segment := TrackSegmentManager.NewSegment(TTrackSegmentStraight, ScrollBoxPanel);
  FinalX := Max(0, TrackSegmentManager.CalculateSnapX(Random(Round( ScrollBoxPanel.Width)) - BASE_SEGMENT_WIDTH, BASE_SEGMENT_WIDTH));
  FinalY := Max(0, TrackSegmentManager.CalculateSnapY(Random(Round( ScrollBoxPanel.Height)) - BASE_SEGMENT_HEIGHT, BASE_SEGMENT_HEIGHT));
  Segment.AnimateFloat('Position.X', FinalX , 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
  Segment.AnimateFloat('Position.Y', FinalY, 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
end;

procedure TFormLayoutBuilder.ActionNewTurnoutSegmentExecute(Sender: TObject);
var
  Segment: TTrackSegment;
  FinalX, FinalY: Single;
begin
  Segment := TrackSegmentManager.NewSegment(TTrackSegmentTurnout, ScrollBoxPanel);
  FinalX := Max(0, TrackSegmentManager.CalculateSnapX(Random(Round( ScrollBoxPanel.Width)) - BASE_SEGMENT_WIDTH, BASE_SEGMENT_WIDTH));
  FinalY := Max(0, TrackSegmentManager.CalculateSnapY(Random(Round( ScrollBoxPanel.Height)) - BASE_SEGMENT_HEIGHT, BASE_SEGMENT_HEIGHT));
  Segment.AnimateFloat('Position.X', FinalX , 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
  Segment.AnimateFloat('Position.Y', FinalY, 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
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

procedure TFormLayoutBuilder.CheckBoxEditModeChange(Sender: TObject);
begin
  DragManager.EditMode := CheckBoxEditMode.IsChecked;
end;

procedure TFormLayoutBuilder.FormCreate(Sender: TObject);
var
  Segment: TTrackSegment;
  BitSize: TSizeF;
  Bitmap: TBitmap;
begin
  FDragManager := TDragManager.Create;
  FTrackSegmentManager := TTrackSegmentManager.Create;

  RectangleDragSelect.Parent := nil;
  TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService);
  TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, WindowService);

 // ScrollBoxPanel.AniCalculations.Animation := False;
  ScrollBoxPanel.AniCalculations.BoundsAnimation := False;
 // ScrollBoxPanel.AniCalculations.TouchTracking := [];
end;

procedure TFormLayoutBuilder.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrackSegmentManager);
  FreeAndNil(FDragManager);
end;

procedure TFormLayoutBuilder.ScrollBoxPanelDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
var
  ViewportX, ViewportY: single;
begin
  ViewportX := Point.X;
  ViewportY := Point.Y;
  ScrollWindowClientToViewport(ViewportX, ViewportY);

  TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(ViewportX, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(ViewportX, ffFixed, 4, 4);
end;

procedure TFormLayoutBuilder.ScrollBoxPanelDragEnter(Sender: TObject; const Data: TDragObject; const Point: TPointF);
var
  ViewportX, ViewportY: single;
begin
  ViewportX := Point.X;
  ViewportY := Point.Y;
  ScrollWindowClientToViewport(ViewportX, ViewportY);

  TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(ViewportX, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(ViewportY, ffFixed, 4, 4);
end;

procedure TFormLayoutBuilder.ScrollBoxPanelDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
var
  ViewportX, ViewportY: single;
begin
  ViewportX := Point.X;
  ViewportY := Point.Y;
  ScrollWindowClientToViewport(ViewportX, ViewportY);

  TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(Point.X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(Point.Y, ffFixed, 4, 4);
  Operation := TDragOperation.None
end;

procedure TFormLayoutBuilder.ScrollBoxPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  i: Integer;
  AbsolutePt: TPointF;
  ViewportX, ViewportY: single;
  TrackSelected: Boolean;
begin
 (*
  // Convert the Client Coordinates into Viewport of the ScrollWindow
  ViewportX := X;
  ViewportY := Y;
 // ScrollWindowClientToViewport(ViewportX, ViewportY);

  DragManager.MouseDown(Button, Shift, ViewportX, ViewportY);
   *)
  case Button of
    TMouseButton.mbLeft :
      begin
        // Only allow selection and dragging when in Edit Mode;
        if DragManager.EditMode then
        begin
          // Check for multiselect else unselect anthing that is selected
          if not ((ssCtrl in Shift) or (ssShift in Shift)) then  // Mac uses Ctrl to force a RightButton click on the track pad
            TrackSegmentManager.UnSelectAll;

          // Try to find the items the user clicked on
          TrackSelected := False;
          i := 0;
          while (i < TrackSegmentManager.Segment.Count) and not TrackSelected do
          begin
            AbsolutePt := ScrollBoxPanel.LocalToAbsolute(TPointF.Create(X, Y)); // Magically works with the scroll and client mouse coordinates
            if TrackSegmentManager.Segment[i].PointInObject(AbsolutePt.X, AbsolutePt.Y) then
            begin
              TrackSegmentManager.Segment[i].Selected := True;
              TrackSegmentManager.Segment[i].Selection.MouseDown(Button, Shift, X, Y);
              DragManager.DragState := TDragState.dsDragPending;
              TrackSelected := True;
            end;
            Inc(i);
          end;

          // The user did not click on anything so unselect everything
          if not TrackSelected then
          begin
            TrackSegmentManager.UnSelectAll;
            DragManager.DragState := TDragState.dsSelectRectPending;
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

  function DragThresholdMet(ViewX, ViewY: single): Boolean;
  begin
    Result := (Abs(DragManager.DragStartViewportPoint.X - ViewX) > 4) or (Abs(DragManager.DragStartViewportPoint.Y - ViewY) > 4);
  end;

var
  ViewportX, ViewportY, ViewportDeltaX, ViewportDeltaY: single;
begin
  // Convert to the Window Viewport and not the Client
  ViewportX := X;
  ViewportY := Y;
  ScrollWindowClientToViewport(ViewportX, ViewportY);

  DragManager.FDragCurrentViewportPoint.X := ViewportX;
  DragManager.FDragCurrentViewportPoint.Y := ViewportY;

  case DragManager.DragMouseButton of
    TMouseButton.mbLeft :
      begin
        TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(DragManager.DragCurrentViewportPoint.X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(DragManager.DragCurrentViewportPoint.Y, ffFixed, 4, 4);

        case DragManager.DragState of
          dsNone :
            begin

            end;
          dsDragPending :
            begin
              if DragThresholdMet(ViewportX, ViewportY) then
              begin
         //       for i := 0 to DragManager.Selections.Count - 1 do
         //         DragManager.Selections[i].DragBegin;
                DragManager.DragState := TDragState.dsDragging;
              end;
            end;
          dsDragging :
            begin
              if DragManager.DragCurrentViewportPoint.X <= 0 then
                ViewportDeltaX := 0
              else
                ViewportDeltaX := DragManager.DragCurrentViewportPoint.X - DragManager.DragStartViewportPoint.X;

              if DragManager.DragCurrentViewportPoint.Y <= 0 then
                ViewportDeltaY := 0
              else
                ViewportDeltaY := DragManager.DragCurrentViewportPoint.Y - DragManager.DragStartViewportPoint.Y;

         //     for i := 0 to DragManager.Selections.Count - 1 do
         //       DragManager.Selections[i].DragMove(ViewportDeltaX, ViewportDeltaY, BASE_SEGMENT_WIDTH, BASE_SEGMENT_HEIGHT);
            end;
          dsSelectRectPending :
            begin
              if DragThresholdMet(ViewportX, ViewportY) then
              begin
                RectangleDragSelect.Position.X := X;
                RectangleDragSelect.Position.Y := Y;
                RectangleDragSelect.Width := 0;
                RectangleDragSelect.Height := 0;
                RectangleDragSelect.Parent := ScrollBoxPanel;
                ScrollBoxPanel.Root.Captured := ScrollBoxPanel;
                DragManager.DragState := TDragState.dsSelectRect;
              end;
            end;
          dsSelectRect :
            begin
              if X < 0 then
                X := 0;
              if X > ScrollBoxPanel.Width then
                X := ScrollBoxPanel.Width;
              if Y < 0 then
                Y := 0;
              if Y > ScrollBoxPanel.Height then
                Y := ScrollBoxPanel.Height;

              if X - DragManager.DragStartViewportPoint.X < 0 then
              begin
                RectangleDragSelect.Position.X := X;
                RectangleDragSelect.Width := DragManager.DragStartViewportPoint.X - X;
              end else
              begin
                RectangleDragSelect.Position.X := DragManager.DragStartViewportPoint.X;
                RectangleDragSelect.Width := X - DragManager.DragStartViewportPoint.X;
              end;
              if Y - DragManager.DragStartViewportPoint.Y < 0 then
              begin
                RectangleDragSelect.Position.Y := Y;
                RectangleDragSelect.Height := DragManager.DragStartViewportPoint.Y - Y;
              end else
              begin
                RectangleDragSelect.Position.Y := DragManager.DragStartViewportPoint.Y;
                RectangleDragSelect.Height := Y - DragManager.DragStartViewportPoint.Y;
              end;
       //       TrackSegmentsSelectionByRect(DragManager.DragMouseButton, Shift, X, Y);
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
var
//  HitSegment: TTrackSegment;
  ViewportX, ViewportY: single;
begin
  ViewportX := X;
  ViewportY := Y;
  ScrollWindowClientToViewport(ViewportX, ViewportY);

  RectangleDragSelect.Parent := nil;
  if not DragManager.EditMode then
  begin
 //   HitSegment := FindTrackSegmentByPt(ViewportX, ViewportY);
 //   if Assigned(HitSegment) then
 //     HitSegment.Click;
  end;
  DragManager.DragState := dsNone;
  UpdateStatusBar;
end;

procedure TFormLayoutBuilder.ScrollWindowClientToViewport(var ClientX, ClientY: single);
begin
  ClientX := ClientX + ScrollBoxPanel.ViewportPosition.X;
  ClientY := ClientY + ScrollBoxPanel.ViewportPosition.Y;
end;

procedure TFormLayoutBuilder.UpdateStatusBar;
begin
  LabelStatusSelection.Text :=  'Selections: ' + IntToStr(TrackSegmentManager.Selection.Count);
  LabelTrackSegmentCount.Text := 'Segments: ' + IntToStr(TrackSegmentManager.Segment.Count);
end;

{ TDragManager }

constructor TDragManager.Create;
begin
  inherited;
end;

destructor TDragManager.Destroy;
begin
  inherited;
end;

procedure TDragManager.MouseDown(Button: TMouseButton; Shift: TShiftState; ViewportX, ViewportY: Single);
begin
  DragMouseButton := Button;

  FDragStartViewportPoint.X := ViewportX;
  FDragStartViewportPoint.Y := ViewportY;

  DragShift := Shift;
end;

procedure TDragManager.MouseMove(Button: TMouseButton; Shift: TShiftState; ViewportX, ViewportY: Single);
begin

end;

procedure TDragManager.MouseUp(Button: TMouseButton; Shift: TShiftState; ViewportX, ViewportY: Single);
begin

end;

procedure TDragManager.SetEditMode(const Value: Boolean);
begin
  if FEditMode <> Value then
  begin
    FEditMode := Value;
  end;
end;

end.
