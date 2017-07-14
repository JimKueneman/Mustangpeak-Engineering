unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, System.UIConsts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Platform,
  System.Generics.Collections, System.ImageList, FMX.ImgList, FMX.Gestures, mustangpeak.tracksegment,
  System.Actions, FMX.ActnList, FMX.MultiView, FMX.Ani, Math, FMX.ListBox,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Header, FMX.ListView, FMX.MultiView.Presentations,
  FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.TabControl, system.diagnostics;

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

    function MouseSelectRectOffset: TPointF;
  end;


type
  TFormLayoutBuilder = class(TForm)
    StatusBar: TStatusBar;
    TextStatusMousePos: TText;
    LabelStatusSelection: TLabel;
    GestureManager: TGestureManager;
    MultiViewMain: TMultiView;
    ActionListMain: TActionList;
    ActionNewStraightSegment: TAction;
    ActionNewTurnoutSegment: TAction;
    LabelTrackSegmentCount: TLabel;
    RectangleHeader: TRectangle;
    Button4: TButton;
    ComboBoxMultiViewMode: TComboBox;
    PanelMain: TPanel;
    ListBox1: TListBox;
    ListBoxItemStraight: TListBoxItem;
    ListBoxItemTurnout: TListBoxItem;
    ListBoxItemCrossover: TListBoxItem;
    ListBoxItemScissor: TListBoxItem;
    ImageListMain: TImageList;
    SpeedButtonMasterButton: TSpeedButton;
    ToolBar1: TToolBar;
    SpeedButtonMasterButton2: TSpeedButton;
    ListBoxItemSignal: TListBoxItem;
    ListBoxGroupHeaderMultiViewSignals: TListBoxGroupHeader;
    ListBoxGroupHeaderMultiviewTracks: TListBoxGroupHeader;
    ListBoxProperites: TListBox;
    ListBoxItemPropertiesPanel: TListBoxItem;
    ListBoxItemProperitesDimHeight: TListBoxItem;
    ListBoxItemHeaderPropertiesHeader: TListBoxItem;
    SpeedButtonPanelProperties: TSpeedButton;
    Rectangle2: TRectangle;
    Label1: TLabel;
    NumberBoxPanelWidth: TNumberBox;
    Label2: TLabel;
    NumberBoxPanelHeight: TNumberBox;
    ColorAnimation1: TColorAnimation;
    RectanglePanelContainer: TRectangle;
    SpeedButtonProperitesMaster: TSpeedButton;
    CheckBoxEditMode: TCheckBox;
    CheckBoxMultiSelectMode: TCheckBox;
    ScrollBoxSketchpad: TScrollBox;
    RectangleSketchpad: TRectangle;
    TextSnapMousePos: TText;
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBoxEditModeChange(Sender: TObject);
    procedure ActionNewStraightSegmentExecute(Sender: TObject);
    procedure ActionNewTurnoutSegmentExecute(Sender: TObject);
    procedure ComboBoxMultiViewModeChange(Sender: TObject);
    procedure MultiViewMainPresenterChanging(Sender: TObject; var PresenterClass: TMultiViewPresentationClass);
    procedure MultiViewMainStartShowing(Sender: TObject);
    procedure MultiViewMainStartHiding(Sender: TObject);
    procedure ListBoxItemStraightClick(Sender: TObject);
    procedure ListBoxItemTurnoutClick(Sender: TObject);
    procedure ListBoxGroupHeaderProperitesPanelClick(Sender: TObject);
    procedure ListBoxItemHeaderPropertiesHeaderClick(Sender: TObject);
    procedure NumberBoxPanelWidthChange(Sender: TObject);
    procedure NumberBoxPanelHeightChange(Sender: TObject);
    procedure SpeedButtonProperitesMasterClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ScrollBoxSketchpadMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ScrollBoxSketchpadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ScrollBoxSketchpadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ScrollBoxSketchpadTap(Sender: TObject; const Point: TPointF);
  private
    FDragManager: TDragManager;
    FTrackSegmentManager: TTrackSegmentManager;
    FTapDuration: TStopwatch;     // Record not an object
  protected
    property DragManager: TDragManager read FDragManager write FDragManager;

    function SketchPadClientToViewport(var ClientX, ClientY: single): TPointF;
    function SketchPadClientToViewportRect(ClientRect: TRectF): TRectF;

    procedure UpdateStatusBar;
  public
    { Public declarations }
    WindowService: IFMXWindowService;
    ScreenService: IFMXScreenService;

    property TapDuration: TStopwatch read FTapDuration write FTapDuration;
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
  Segment := TrackSegmentManager.NewSegment(TTrackSegmentStraight,  ScrollBoxSketchpad);
  FinalX := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( RectangleSketchpad.Width)) - BASE_SEGMENT_WIDTH, BASE_SEGMENT_WIDTH));
  FinalY := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( RectangleSketchpad.Height)) - BASE_SEGMENT_HEIGHT, BASE_SEGMENT_HEIGHT));
  TAnimator.AnimateFloat(Segment, 'Position.X', FinalX , 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
  TAnimator.AnimateFloat(Segment, 'Position.Y', FinalY, 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
end;

procedure TFormLayoutBuilder.ActionNewTurnoutSegmentExecute(Sender: TObject);
var
  Segment: TTrackSegment;
  FinalX, FinalY: Single;
begin
  Segment := TrackSegmentManager.NewSegment(TTrackSegmentTurnout, ScrollBoxSketchpad);
  FinalX := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( RectangleSketchpad.Width)) - BASE_SEGMENT_WIDTH, BASE_SEGMENT_WIDTH));
  FinalY := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( RectangleSketchpad.Height)) - BASE_SEGMENT_HEIGHT, BASE_SEGMENT_HEIGHT));
  TAnimator.AnimateFloat(Segment, 'Position.X', FinalX , 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
  TAnimator.AnimateFloat(Segment, 'Position.Y', FinalY, 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
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
  begin
    // Bug in popover, won't resize the MasterPane when it hides the MultiView
    if TMultiViewMode( ComboBoxMultiViewMode.ItemIndex) = TMultiViewMode.Popover then
      MultiViewMain.Mode := TMultiViewMode.Drawer;
    MultiViewMain.Mode := TMultiViewMode( ComboBoxMultiViewMode.ItemIndex);
  end;
end;

procedure TFormLayoutBuilder.FormCreate(Sender: TObject);
begin

  // Broken in OSX, won't show
  ScrollBoxSketchpad.AniCalculations.AutoShowing := False;

  TapDuration := TStopwatch.Create;

  ComboBoxMultiViewMode.ItemIndex := 0; // Set to platform, assumes at design time it is -1
  ListBoxProperites.Width := 0;         // Mainly for mobile devices

  FDragManager := TDragManager.Create;
  FTrackSegmentManager := TTrackSegmentManager.Create;

  TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService);
  TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, WindowService);

end;

procedure TFormLayoutBuilder.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrackSegmentManager);
  FreeAndNil(FDragManager);
end;

procedure TFormLayoutBuilder.FormResize(Sender: TObject);
begin
  {$IF Defined(IOS) or Defined(ANDROID)}
  if Assigned(ScreenService) then
  begin
    if Width <> Trunc( ScreenService.GetScreenSize.X * ScreenService.GetScreenScale) then
      Width := Trunc( ScreenService.GetScreenSize.X * ScreenService.GetScreenScale);
    if Height <> Trunc( ScreenService.GetScreenSize.Y * ScreenService.GetScreenScale) then
      Height := Trunc( ScreenService.GetScreenSize.Y * ScreenService.GetScreenScale);
  end;
  {$ENDIF}
end;

procedure TFormLayoutBuilder.FormShow(Sender: TObject);
begin
  NumberBoxPanelWidth.Value := RectangleSketchpad.Width;
  NumberBoxPanelHeight.Value := RectangleSketchpad.Height;
end;

procedure TFormLayoutBuilder.ListBoxGroupHeaderProperitesPanelClick(
  Sender: TObject);
begin
  if SpeedButtonPanelProperties.ImageIndex = 33 then
  begin
    SpeedButtonPanelProperties.ImageIndex := 34;
  end else
  begin
    SpeedButtonPanelProperties.ImageIndex := 33;
  end;
end;

procedure TFormLayoutBuilder.ListBoxItemHeaderPropertiesHeaderClick(Sender: TObject);
begin
  if SpeedButtonPanelProperties.ImageIndex = 33 then
  begin
    SpeedButtonPanelProperties.ImageIndex := 34;
    TAnimator.AnimateFloat(ListBoxItemPropertiesPanel, 'Height', 0, 0.3, TAnimationType.In, TInterpolationType.Quadratic);
  end else
  begin
    SpeedButtonPanelProperties.ImageIndex := 33;
    TAnimator.AnimateFloat(ListBoxItemPropertiesPanel, 'Height', 125, 0.3, TAnimationType.Out, TInterpolationType.Quadratic);
  end;

end;

procedure TFormLayoutBuilder.ListBoxItemStraightClick(Sender: TObject);
begin
  ActionNewStraightSegment.Execute
end;

procedure TFormLayoutBuilder.ListBoxItemTurnoutClick(Sender: TObject);
begin
  ActionNewTurnoutSegment.Execute
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
  if PresenterClass = TMultiViewPopoverPresentation then
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
  end else
  if (PresenterClass = TMultiViewDrawerOverlapPresentation) or
     (PresenterClass = TMultiViewDrawerPushingPresentation) then
  begin
    SpeedButtonMasterButton.Visible := True;
    SpeedButtonMasterButton2.Visible := True;
    MultiViewMain.MasterButton := SpeedButtonMasterButton2;
  end;
end;

procedure TFormLayoutBuilder.MultiViewMainStartHiding(Sender: TObject);
begin
  if MultiViewMain.Mode = TMultiViewMode.Drawer then
    MultiViewMain.MasterButton := SpeedButtonMasterButton2;
end;

procedure TFormLayoutBuilder.MultiViewMainStartShowing(Sender: TObject);
begin
  if MultiViewMain.Mode = TMultiViewMode.Drawer then
    MultiViewMain.MasterButton := SpeedButtonMasterButton;
end;

procedure TFormLayoutBuilder.NumberBoxPanelHeightChange(Sender: TObject);
begin
  RectangleSketchpad.Height := NumberBoxPanelHeight.Value;
end;

procedure TFormLayoutBuilder.NumberBoxPanelWidthChange(Sender: TObject);
begin
  RectangleSketchpad.Width := NumberBoxPanelWidth.Value
end;

procedure TFormLayoutBuilder.ScrollBoxSketchpadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  i, StartingCount: Integer;
  IsMultiSelect, ToggleSelection, HitOne: Boolean;
begin
  RectangleSketchpad.Root.Captured := RectangleSketchpad;
  TapDuration.Start;

  // Convert the Client Coordinates into Viewport of the ScrollWindow
  DragManager.FMouseDownViewportPoint := SketchPadClientToViewport(X, Y);
  DragManager.FMouseCurrentViewportPoint := DragManager.MouseDownViewportPoint;
  DragManager.FMousePreviousVewportPoint := DragManager.MouseDownViewportPoint;
  DragManager.MouseButton := Button;
  DragManager.Shift := Shift;

  case Button of
    TMouseButton.mbLeft :
      begin
        // Only allow selection and dragging when in Edit Mode;
        if DragManager.EditMode then
        begin
          IsMultiSelect := (ssCtrl in Shift) or (ssShift in Shift);
          ToggleSelection := (ssCtrl in Shift) or (ssShift in Shift);
          HitOne := False;
          StartingCount := TrackSegmentManager.Selection.Count;

          // Needs to be called on all segments so they get the position of the click
          for i := 0 to TrackSegmentManager.Segment.Count - 1 do
          begin
            if PtInRect(TrackSegmentManager.Segment[i].BoundsRect, DragManager.MouseDownViewportPoint) then
            begin
              if ToggleSelection then
                TrackSegmentManager.Segment[i].Selected := not TrackSegmentManager.Segment[i].Selected
              else
                TrackSegmentManager.Segment[i].Selected := True;
              HitOne := True;
            end else
            begin
              if not IsMultiSelect and (StartingCount < 2) then
                TrackSegmentManager.Segment[i].Selected := False;
            end;
          end;

          if not HitOne and not IsMultiSelect then
            TrackSegmentManager.UnSelectAll;

          DragManager.DragSelectStartRect := TrackSegmentManager.SelectionBounds;
          DragManager.DragSelectCurrentRect := TrackSegmentManager.SelectionBounds;

          if (TrackSegmentManager.Selection.Count = 0) or IsMultiSelect then
            DragManager.State := TDragState.dsSelectRectPending
          else
            DragManager.State := TDragState.dsDragPending;
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

procedure TFormLayoutBuilder.ScrollBoxSketchpadMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);

  function LocalDragThresholdMet(ViewportPt: TPointF): Boolean;
  const DRAG_THRESHOLD = 5;
  begin
    Result := (Abs(DragManager.MouseDownViewportPoint.X - ViewportPt.X) > DRAG_THRESHOLD) or (Abs(DragManager.MouseDownViewportPoint.Y - ViewportPt.Y) > DRAG_THRESHOLD);
  end;

  procedure LocalDoDragSelect;
  var
    IsMultiSelect: Boolean;
  begin
    if DragManager.MouseCurrentViewportPoint.X < 0 then
      DragManager.FMouseCurrentViewportPoint.X := 0;
    if DragManager.MouseCurrentViewportPoint.Y < 0 then
      DragManager.FMouseCurrentViewportPoint.Y := 0;

    if DragManager.MouseCurrentViewportPoint.X - DragManager.MouseDownViewportPoint.X < 0 then
    begin
      DragManager.RectangleDragSelect.Position.X := DragManager.MouseCurrentViewportPoint.X;
      DragManager.RectangleDragSelect.Width := DragManager.MouseDownViewportPoint.X - DragManager.MouseCurrentViewportPoint.X;
    end else
    begin
      DragManager.RectangleDragSelect.Position.X := DragManager.MouseDownViewportPoint.X;
      DragManager.RectangleDragSelect.Width := DragManager.MouseCurrentViewportPoint.X - DragManager.MouseDownViewportPoint.X;
    end;
    if DragManager.MouseCurrentViewportPoint.Y - DragManager.MouseDownViewportPoint.Y < 0 then
    begin
      DragManager.RectangleDragSelect.Position.Y := DragManager.MouseCurrentViewportPoint.Y;
      DragManager.RectangleDragSelect.Height := DragManager.MouseDownViewportPoint.Y - DragManager.MouseCurrentViewportPoint.Y;
    end else
    begin
      DragManager.RectangleDragSelect.Position.Y := DragManager.MouseDownViewportPoint.Y;
      DragManager.RectangleDragSelect.Height := DragManager.MouseCurrentViewportPoint.Y - DragManager.MouseDownViewportPoint.Y;
    end;

    IsMultiSelect := (ssCtrl in Shift) or (ssShift in Shift);
    TrackSegmentManager.SelectByRect(DragManager.RectangleDragSelect.BoundsRect, IsMultiSelect);
    UpdateStatusBar;

  end;

  procedure LocalDoDrag;
  var
    SelectRectSnapPt, DeltaPt: TPointF;
    DeltaX, DeltaY: single;
  begin
    SelectRectSnapPt := TPointF.Create(DragManager.MouseCurrentViewportPoint.X - DragManager.MouseSelectRectOffset.X, DragManager.MouseCurrentViewportPoint.Y - DragManager.MouseSelectRectOffset.Y);

    SelectRectSnapPt := TrackSegmentManager.CalculateSnapPt(SelectRectSnapPt, BASE_SEGMENT_WIDTH, BASE_SEGMENT_HEIGHT);

    if SelectRectSnapPt.X < 0 then
      DeltaPt.X := 0
    else
      DeltaPt.X := SelectRectSnapPt.X - DragManager.DragSelectCurrentRect.Left;
    if SelectRectSnapPt.Y < 0 then
      DeltaPt.Y := 0
    else
      DeltaPt.Y := SelectRectSnapPt.Y - DragManager.DragSelectCurrentRect.Top;

    TrackSegmentManager.MoveSelectedBy(DeltaPt);

    DragManager.DragSelectCurrentRect := TrackSegmentManager.SelectionBounds;

    if DragManager.DragSelectCurrentRect.Left < ScrollboxSketchpad.ViewportPosition.X then
      ScrollboxSketchpad.ScrollBy(+(DragManager.DragSelectCurrentRect.Left - ScrollboxSketchpad.ViewportPosition.X), 0)
    else
      if DragManager.DragSelectCurrentRect.Right > (ScrollboxSketchpad.ViewportPosition.X + ScrollboxSketchpad.Width) then
      ScrollboxSketchpad.ScrollBy(-(DragManager.DragSelectCurrentRect.Right - (ScrollboxSketchpad.ViewportPosition.X + ScrollboxSketchpad.Width)), 0);


    if DragManager.DragSelectCurrentRect.Bottom < ScrollboxSketchpad.ViewportPosition.Y then
      ScrollboxSketchpad.ScrollBy(0, +(DragManager.DragSelectCurrentRect.Bottom - ScrollboxSketchpad.ViewportPosition.Y))
    else
    if DragManager.DragSelectCurrentRect.Bottom > (ScrollboxSketchpad.ViewportPosition.Y + ScrollboxSketchpad.Height) then
      ScrollboxSketchpad.ScrollBy(0, -(DragManager.DragSelectCurrentRect.Bottom - (ScrollboxSketchpad.ViewportPosition.Y + ScrollboxSketchpad.Height)));
  end;

var
  i: Integer;
  DeltaPt: TPointF;
  TempSelectRect: TRectF;
begin
  DragManager.FMouseCurrentViewportPoint := SketchPadClientToViewport(X, Y);

  case DragManager.MouseButton of
    TMouseButton.mbLeft :
      begin
        TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(DragManager.MouseCurrentViewportPoint.X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(DragManager.MouseCurrentViewportPoint.Y, ffFixed, 4, 4);

        case DragManager.State of
          dsNone :
            begin

            end;
          dsDragPending :
            begin
              if LocalDragThresholdMet(DragManager.MouseCurrentViewportPoint) then
              begin
                DragManager.State := TDragState.dsDragging;
                LocalDoDrag;
              end;
            end;
          dsDragging :
            begin
              LocalDoDrag;
            end;
          dsSelectRectPending :
            begin
              if LocalDragThresholdMet(DragManager.MouseCurrentViewportPoint) then
              begin
                DragManager.RectangleDragSelect.Position.Point := DragManager.MouseDownViewportPoint;
                DragManager.RectangleDragSelect.Width := 0;
                DragManager.RectangleDragSelect.Height := 0;
                DragManager.RectangleDragSelect.Parent := ScrollBoxSketchpad;
                DragManager.State := TDragState.dsSelectRect;
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
  DragManager.FMousePreviousVewportPoint := DragManager.MouseCurrentViewportPoint;
end;

procedure TFormLayoutBuilder.ScrollBoxSketchpadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  HitSegment: TTrackSegment;
begin
  RectangleSketchpad.Root.Captured := nil;
  TapDuration.Stop;
//  TapDuration.ElapsedMilliseconds

  DragManager.FMouseCurrentViewportPoint := SketchPadClientToViewport(X, Y);

  DragManager.RectangleDragSelect.Parent := nil;
  if not DragManager.EditMode then
  begin
    HitSegment := TrackSegmentManager.FindSegmentByPt(DragManager.MouseCurrentViewportPoint.X, DragManager.MouseCurrentViewportPoint.Y);
    if Assigned(HitSegment) then
      HitSegment.Click;
  end;
  DragManager.State := dsNone;
  UpdateStatusBar;
end;

procedure TFormLayoutBuilder.ScrollBoxSketchpadTap(Sender: TObject; const Point: TPointF);
begin
  beep;
end;

function TFormLayoutBuilder.SketchPadClientToViewport(var ClientX, ClientY: single): TPointF;
begin
  Result.X := ClientX + ScrollBoxSketchpad.ViewportPosition.X;
  Result.Y := ClientY + ScrollBoxSketchpad.ViewportPosition.Y;
end;

function TFormLayoutBuilder.SketchPadClientToViewportRect(ClientRect: TRectF): TRectF;
begin
  Result.TopLeft := SketchPadClientToViewport(ClientRect.Left, ClientRect.Top);
  Result.BottomRight := SketchPadClientToViewport(ClientRect.Right, ClientRect.Bottom)
end;

procedure TFormLayoutBuilder.SpeedButtonProperitesMasterClick(Sender: TObject);
begin
  if ListBoxProperites.Width > 0  then
    TAnimator.AnimateFloat(ListBoxProperites, 'Width', 0, 0.2, TAnimationType.&In, TInterpolationType.Exponential)
  else
    TAnimator.AnimateFloat(ListBoxProperites, 'Width', 175, 0.2, TAnimationType.&In, TInterpolationType.Exponential);
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
