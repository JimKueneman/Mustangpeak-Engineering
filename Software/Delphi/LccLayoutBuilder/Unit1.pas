unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, System.UIConsts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Platform,
  System.Generics.Collections, System.ImageList, FMX.ImgList, FMX.Gestures, mustangpeak.tracksegment,
  System.Actions, FMX.ActnList, FMX.MultiView, FMX.Ani, Math, FMX.ListBox,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Header, FMX.ListView, FMX.MultiView.Presentations;

type
  TDragState = (dsNone, dsDragging, dsSelectRect, dsDragPending, dsSelectRectPending);

type
  TDragManager = class(TPersistent)
  private
    FState: TDragState;
    FStartViewportPoint: TPointF;
    FPreviousVewportPoint: TPointF;
    FCurrentViewportPoint: TPointF;
    FShift: TShiftState;
    FMouseButton: TMouseButton;
    FEditMode: Boolean;
    procedure SetEditMode(const Value: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property CurrentViewportPoint: TPointF read FCurrentViewportPoint write FCurrentViewportPoint;
    property EditMode: Boolean read FEditMode write SetEditMode;
    property MouseButton: TMouseButton read FMouseButton write FMouseButton;
    property PreviousVewportPoint: TPointF read FPreviousVewportPoint write FPreviousVewportPoint;
    property Shift: TShiftState read FShift write FShift;
    property StartViewportPoint: TPointF read FStartViewportPoint write FStartViewportPoint;
    property State: TDragState read FState write FState;
  end;


type
  TFormLayoutBuilder = class(TForm)
    StatusBar: TStatusBar;
    TextStatusMousePos: TText;
    LabelStatusSelection: TLabel;
    ImageList: TImageList;
    GestureManager: TGestureManager;
    MultiViewMain: TMultiView;
    ActionListMain: TActionList;
    ActionNewStraightSegment: TAction;
    ActionNewTurnoutSegment: TAction;
    LabelTrackSegmentCount: TLabel;
    Rectangle1: TRectangle;
    Button4: TButton;
    CheckBoxEditMode: TCheckBox;
    ScrollBoxPanel: TScrollBox;
    RectangleDragSelect: TRectangle;
    ComboBoxMultiViewMode: TComboBox;
    PanelMain: TPanel;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ImageListMain: TImageList;
    SpeedButtonMasterButton: TSpeedButton;
    ToolBar1: TToolBar;
    SpeedButtonMasterButton2: TSpeedButton;
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
    procedure ComboBoxMultiViewModeChange(Sender: TObject);
    procedure MultiViewMainPresenterChanging(Sender: TObject; var PresenterClass: TMultiViewPresentationClass);
    procedure MultiViewMainStartShowing(Sender: TObject);
    procedure MultiViewMainStartHiding(Sender: TObject);
  private
    FDragManager: TDragManager;
    FTrackSegmentManager: TTrackSegmentManager;
  protected
    property DragManager: TDragManager read FDragManager write FDragManager;
    function ScrollWindowClientToViewport(var ClientX, ClientY: single): TPointF;
    function ScrollWindowClientToViewportRect(ClientRect: TRectF): TRectF;
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

procedure TFormLayoutBuilder.ComboBoxMultiViewModeChange(Sender: TObject);
begin
  if ComboBoxMultiViewMode.ItemIndex > -1 then
    MultiViewMain.Mode := TMultiViewMode( ComboBoxMultiViewMode.ItemIndex);
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

 // Broken in OSX, won't show
  ScrollBoxPanel.AniCalculations.AutoShowing := False;
end;

procedure TFormLayoutBuilder.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrackSegmentManager);
  FreeAndNil(FDragManager);
end;

procedure TFormLayoutBuilder.MultiViewMainPresenterChanging(Sender: TObject;
  var PresenterClass: TMultiViewPresentationClass);
begin
  if PresenterClass = TMultiViewDockedPanelPresentation then
  begin
    SpeedButtonMasterButton.Visible := False;
    SpeedButtonMasterButton2.Visible := False;
    MultiViewMain.MasterButton := nil;
  end else
  if (PresenterClass = TMultiViewDrawerOverlapPresentation) or
     (PresenterClass = TMultiViewDrawerPushingPresentation) or
     (PresenterClass = TMultiViewPopoverPresentation) then
  begin
    SpeedButtonMasterButton.Visible := False;
    SpeedButtonMasterButton2.Visible := True;
    MultiViewMain.MasterButton := SpeedButtonMasterButton2;
  end;
  if PresenterClass = TMultiViewNavigationPanePresentation then
  begin
    SpeedButtonMasterButton.Visible := True;
    SpeedButtonMasterButton2.Visible := False;
    MultiViewMain.MasterButton := SpeedButtonMasterButton;
  end;
end;

procedure TFormLayoutBuilder.MultiViewMainStartHiding(Sender: TObject);
begin
  if MultiViewMain.Mode = TMultiViewMode.Drawer then
  begin
    SpeedButtonMasterButton.Visible := False;
    SpeedButtonMasterButton2.Visible := True;
    MultiViewMain.MasterButton := SpeedButtonMasterButton2;
  end;
end;

procedure TFormLayoutBuilder.MultiViewMainStartShowing(Sender: TObject);
begin
  if MultiViewMain.Mode = TMultiViewMode.Drawer then
  begin
    SpeedButtonMasterButton.Visible := True;
    SpeedButtonMasterButton2.Visible := False;
    MultiViewMain.MasterButton := SpeedButtonMasterButton;
  end;
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
  TrackSelected: Boolean;
begin

  // Convert the Client Coordinates into Viewport of the ScrollWindow
  DragManager.FStartViewportPoint := ScrollWindowClientToViewport(X, Y);
  DragManager.FCurrentViewportPoint := DragManager.StartViewportPoint;
  DragManager.FPreviousVewportPoint := DragManager.StartViewportPoint;
  DragManager.MouseButton := Button;
  DragManager.Shift := Shift;


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
          // Needs to be called on all segments so they get the position of the click
          for i := 0 to TrackSegmentManager.Segment.Count - 1 do
          begin
            if PtInRect(TrackSegmentManager.Segment[i].BoundsRect, DragManager.StartViewportPoint) then
            begin
              TrackSegmentManager.Segment[i].Selected := True;
              TrackSelected := True;
            end;
            if TrackSegmentManager.Segment[i].Selected then
              TrackSegmentManager.Segment[i].DragBegin(DragManager.CurrentViewportPoint);
          end;

          // The user did not click on anything so unselect everything
          if not TrackSelected then
          begin
            if not ((ssCtrl in Shift) or (ssShift in Shift)) then
              TrackSegmentManager.UnSelectAll;
            DragManager.State := TDragState.dsSelectRectPending;
          end else
            DragManager.State := TDragState.dsDragPending;

          TrackSegmentManager.RefreshSelectionBounds;
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

  function DragThresholdMet(ViewportPt: TPointF): Boolean;
  const DRAG_THRESHOLD = 5;
  begin
    Result := (Abs(DragManager.StartViewportPoint.X - ViewportPt.X) > DRAG_THRESHOLD) or (Abs(DragManager.StartViewportPoint.Y - ViewportPt.Y) > DRAG_THRESHOLD);
  end;

var
  i: Integer;
  DeltaPt: TPointF;
  TempSelectRect: TRectF;
begin
  DragManager.FCurrentViewportPoint := ScrollWindowClientToViewport(X, Y);
  // Calulate the difference in the last point and this point
  DeltaPt.X := DragManager.CurrentViewportPoint.X - DragManager.PreviousVewportPoint.X;
  DeltaPt.Y := DragManager.CurrentViewportPoint.Y - DragManager.PreviousVewportPoint.Y;

  case DragManager.MouseButton of
    TMouseButton.mbLeft :
      begin
        TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(DragManager.CurrentViewportPoint.X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(DragManager.CurrentViewportPoint.Y, ffFixed, 4, 4);

        case DragManager.State of
          dsNone :
            begin

            end;
          dsDragPending :
            begin
              if DragThresholdMet(DragManager.CurrentViewportPoint) then
              begin
                DragManager.State := TDragState.dsDragging;

              end;
            end;
          dsDragging :
            begin
              // Calculate where the selection rectangle would be with this movement.
              TempSelectRect := TrackSegmentManager.CurrentSelectionBounds;
              TempSelectRect.Offset(DeltaPt.X, DeltaPt.Y);
              if TempSelectRect.Left < 0 then
                DragManager.FCurrentViewportPoint.X := DragManager.PreviousVewportPoint.X;
              if TempSelectRect.Top < 0 then
                DragManager.FCurrentViewportPoint.Y := DragManager.PreviousVewportPoint.Y;

              for i := 0 to TrackSegmentManager.Selection.Count - 1 do
                TrackSegmentManager.Selection[i].Drag(DragManager.CurrentViewportPoint, BASE_SEGMENT_WIDTH, BASE_SEGMENT_HEIGHT);

              TrackSegmentManager.RefreshSelectionBounds;
            end;
          dsSelectRectPending :
            begin
              if DragThresholdMet(DragManager.CurrentViewportPoint) then
              begin
                RectangleDragSelect.Position.X := X;
                RectangleDragSelect.Position.Y := Y;
                RectangleDragSelect.Width := 0;
                RectangleDragSelect.Height := 0;
                RectangleDragSelect.Parent := ScrollBoxPanel;
                ScrollBoxPanel.Root.Captured := ScrollBoxPanel;
                DragManager.State := TDragState.dsSelectRect;
              end;
            end;
          dsSelectRect :
            begin
              if DragManager.CurrentViewportPoint.X < 0 then
                DragManager.FCurrentViewportPoint.X := 0;
              if DragManager.CurrentViewportPoint.X > ScrollBoxPanel.Width then
                DragManager.FCurrentViewportPoint.X := ScrollBoxPanel.Width;
              if DragManager.CurrentViewportPoint.Y < 0 then
                DragManager.FCurrentViewportPoint.Y := 0;
              if DragManager.CurrentViewportPoint.Y > ScrollBoxPanel.Height then
                DragManager.FCurrentViewportPoint.Y := ScrollBoxPanel.Height;

              if DragManager.CurrentViewportPoint.X - DragManager.StartViewportPoint.X < 0 then
              begin
                RectangleDragSelect.Position.X := DragManager.CurrentViewportPoint.X;
                RectangleDragSelect.Width := DragManager.StartViewportPoint.X - DragManager.CurrentViewportPoint.X;
              end else
              begin
                RectangleDragSelect.Position.X := DragManager.StartViewportPoint.X;
                RectangleDragSelect.Width := DragManager.CurrentViewportPoint.X - DragManager.StartViewportPoint.X;
              end;
              if DragManager.CurrentViewportPoint.Y - DragManager.StartViewportPoint.Y < 0 then
              begin
                RectangleDragSelect.Position.Y := DragManager.CurrentViewportPoint.Y;
                RectangleDragSelect.Height := DragManager.StartViewportPoint.Y - DragManager.CurrentViewportPoint.Y;
              end else
              begin
                RectangleDragSelect.Position.Y := DragManager.StartViewportPoint.Y;
                RectangleDragSelect.Height := DragManager.CurrentViewportPoint.Y - DragManager.StartViewportPoint.Y;
              end;

              TempSelectRect := ScrollWindowClientToViewportRect(RectangleDragSelect.BoundsRect);
              TrackSegmentManager.SelectByRect(TempSelectRect, ((ssCtrl in Shift) or (ssShift in Shift)));
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
  // Update previous to current for next mouse move call
  DragManager.FPreviousVewportPoint := DragManager.CurrentViewportPoint;
end;

procedure TFormLayoutBuilder.ScrollBoxPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  HitSegment: TTrackSegment;
begin
  DragManager.FCurrentViewportPoint := ScrollWindowClientToViewport(X, Y);

  RectangleDragSelect.Parent := nil;
  if not DragManager.EditMode then
  begin
    HitSegment := TrackSegmentManager.FindSegmentByPt(DragManager.CurrentViewportPoint.X, DragManager.CurrentViewportPoint.Y);
    if Assigned(HitSegment) then
      HitSegment.Click;
  end;
  DragManager.State := dsNone;
  UpdateStatusBar;
end;

function TFormLayoutBuilder.ScrollWindowClientToViewport(var ClientX, ClientY: single): TPointF;
begin
  Result.X := ClientX + ScrollBoxPanel.ViewportPosition.X;
  Result.Y := ClientY + ScrollBoxPanel.ViewportPosition.Y;
end;

function TFormLayoutBuilder.ScrollWindowClientToViewportRect(ClientRect: TRectF): TRectF;
begin
  Result.TopLeft := ScrollWindowClientToViewport(ClientRect.Left, ClientRect.Top);
  Result.BottomRight := ScrollWindowClientToViewport(ClientRect.Right, ClientRect.Bottom)
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


procedure TDragManager.SetEditMode(const Value: Boolean);
begin
  if FEditMode <> Value then
  begin
    FEditMode := Value;
  end;
end;

end.
