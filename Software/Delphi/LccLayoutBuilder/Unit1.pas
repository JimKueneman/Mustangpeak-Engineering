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
  FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.TabControl, system.diagnostics,
  mustangpeak.dragmanager, mustangpeak.xmlutilities;

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
    ColorAnimation1: TColorAnimation;
    RectanglePanelContainer: TRectangle;
    SpeedButtonProperitesMaster: TSpeedButton;
    ScrollBoxSketchpad: TScrollBox;
    TextSnapMousePos: TText;
    CheckBoxEditMode: TCheckBox;
    CheckBoxMultiSelectMode: TCheckBox;
    ActionSaveToXml: TAction;
    ButtonSaveToXml: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ActionLoadFromXml: TAction;
    ButtonLoadFromXml: TButton;
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
    procedure SpeedButtonProperitesMasterClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ScrollBoxSketchpadMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ScrollBoxSketchpadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ScrollBoxSketchpadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ScrollBoxSketchpadTap(Sender: TObject; const Point: TPointF);
    procedure ActionSaveToXmlExecute(Sender: TObject);
    procedure ActionLoadFromXmlExecute(Sender: TObject);
  private
    FTrackSegmentManager: TTrackSegmentManager;
    FTapDuration: TStopwatch;     // Record not an object
  protected
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


procedure TFormLayoutBuilder.ActionLoadFromXmlExecute(Sender: TObject);
var
  XmlDoc: TMustangpeakXmlDocument;
  Root, Layout: TMustangpeakXmlNode;
begin
  if OpenDialog.Execute then
  begin
    XmlDoc := XmlLoadFromFile(OpenDialog.FileName);
    Root := XmlFindRootNode(XmlDoc, 'mustangpeak');
    if Assigned(Root) then
    begin
      Layout := XmlFindChildNode(Root, 'layout');
      if Assigned(Layout) then
        TrackSegmentManager.LoadFromXML(XmlDoc, Layout, ScrollBoxSketchpad);
    end;
  end;
end;

procedure TFormLayoutBuilder.ActionNewStraightSegmentExecute(Sender: TObject);
var
  Segment: TTrackSegment;
  FinalX, FinalY: Single;
begin
  Segment := TrackSegmentManager.NewSegment(TTrackSegmentStraight,  ScrollBoxSketchpad);
  FinalX := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( ScrollBoxSketchpad.Width)) - BASE_SEGMENT_WIDTH, BASE_SEGMENT_WIDTH));
  FinalY := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( ScrollBoxSketchpad.Height)) - BASE_SEGMENT_HEIGHT, BASE_SEGMENT_HEIGHT));
  TAnimator.AnimateFloat(Segment, 'Position.X', FinalX , 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
  TAnimator.AnimateFloat(Segment, 'Position.Y', FinalY, 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
end;

procedure TFormLayoutBuilder.ActionNewTurnoutSegmentExecute(Sender: TObject);
var
  Segment: TTrackSegment;
  FinalX, FinalY: Single;
begin
  Segment := TrackSegmentManager.NewSegment(TTrackSegmentTurnout, ScrollBoxSketchpad);
  FinalX := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( ScrollBoxSketchpad.Width)) - BASE_SEGMENT_WIDTH, BASE_SEGMENT_WIDTH));
  FinalY := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( ScrollBoxSketchpad.Height)) - BASE_SEGMENT_HEIGHT, BASE_SEGMENT_HEIGHT));
  TAnimator.AnimateFloat(Segment, 'Position.X', FinalX , 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
  TAnimator.AnimateFloat(Segment, 'Position.Y', FinalY, 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
end;

procedure TFormLayoutBuilder.ActionSaveToXmlExecute(Sender: TObject);
var
  XmlDoc: TMustangpeakXmlDocument;
  Root, Layout: TMustangpeakXmlNode;
begin
 // SaveDialog.
  if SaveDialog.Execute then
  begin
    XmlDoc := XmlCreateEmptyDocument;
    Root := XmlCreateRootNode(XmlDoc, 'mustangpeak', '');
    Layout := XmlCreateChildNode(XmlDoc, Root, 'layout', '');
    TrackSegmentManager.SaveToXML(XmlDoc, Layout);
    XmlWriteToFile(SaveDialog.FileName, XmlDoc);
  end;
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
  TrackSegmentManager.EditMode := CheckBoxEditMode.IsChecked;
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

  FTrackSegmentManager := TTrackSegmentManager.Create;

  TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService);
  TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, WindowService);

end;

procedure TFormLayoutBuilder.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrackSegmentManager);
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

procedure TFormLayoutBuilder.ScrollBoxSketchpadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  i, StartingCount: Integer;
  IsMultiSelect, ToggleSelection, HitOne: Boolean;
begin
  ScrollBoxSketchpad.Root.Captured := ScrollBoxSketchpad;
  TapDuration.Start;

  // Convert the Client Coordinates into Viewport of the ScrollWindow
  TrackSegmentManager.MouseDownViewportPoint := SketchPadClientToViewport(X, Y);
  TrackSegmentManager.MouseCurrentViewportPoint := TrackSegmentManager.MouseDownViewportPoint;
  TrackSegmentManager.MousePreviousVewportPoint := TrackSegmentManager.MouseDownViewportPoint;
  TrackSegmentManager.MouseButton := Button;
  TrackSegmentManager.Shift := Shift;

  case Button of
    TMouseButton.mbLeft :
      begin
        // Only allow selection and dragging when in Edit Mode;
        if TrackSegmentManager.EditMode then
        begin
          IsMultiSelect := (ssCtrl in Shift) or (ssShift in Shift);
          ToggleSelection := (ssCtrl in Shift) or (ssShift in Shift);
          HitOne := False;
          StartingCount := TrackSegmentManager.Selection.Count;

          // Needs to be called on all segments so they get the position of the click
          for i := 0 to TrackSegmentManager.Segment.Count - 1 do
          begin
            if PtInRect(TrackSegmentManager.Segment[i].BoundsRect, TrackSegmentManager.MouseDownViewportPoint) then
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

          TrackSegmentManager.DragSelectStartRect := TrackSegmentManager.SelectionBounds;
          TrackSegmentManager.DragSelectCurrentRect := TrackSegmentManager.SelectionBounds;

          if (TrackSegmentManager.Selection.Count = 0) or IsMultiSelect then
            TrackSegmentManager.State := TDragState.dsSelectRectPending
          else
            TrackSegmentManager.State := TDragState.dsDragPending;
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
    Result := (Abs(TrackSegmentManager.MouseDownViewportPoint.X - ViewportPt.X) > DRAG_THRESHOLD) or (Abs(TrackSegmentManager.MouseDownViewportPoint.Y - ViewportPt.Y) > DRAG_THRESHOLD);
  end;

  procedure LocalDoDragSelect;
  var
    IsMultiSelect: Boolean;
  begin
    if TrackSegmentManager.MouseCurrentViewportPoint.X < 0 then
      TrackSegmentManager.MouseCurrentViewportPoint.Create(0, TrackSegmentManager.MouseCurrentViewportPoint.Y);
    if TrackSegmentManager.MouseCurrentViewportPoint.Y < 0 then
       TrackSegmentManager.MouseCurrentViewportPoint.Create(TrackSegmentManager.MouseCurrentViewportPoint.X, 0);

    if TrackSegmentManager.MouseCurrentViewportPoint.X - TrackSegmentManager.MouseDownViewportPoint.X < 0 then
    begin
      TrackSegmentManager.RectangleDragSelect.Position.X := TrackSegmentManager.MouseCurrentViewportPoint.X;
      TrackSegmentManager.RectangleDragSelect.Width := TrackSegmentManager.MouseDownViewportPoint.X - TrackSegmentManager.MouseCurrentViewportPoint.X;
    end else
    begin
      TrackSegmentManager.RectangleDragSelect.Position.X := TrackSegmentManager.MouseDownViewportPoint.X;
      TrackSegmentManager.RectangleDragSelect.Width := TrackSegmentManager.MouseCurrentViewportPoint.X - TrackSegmentManager.MouseDownViewportPoint.X;
    end;
    if TrackSegmentManager.MouseCurrentViewportPoint.Y - TrackSegmentManager.MouseDownViewportPoint.Y < 0 then
    begin
      TrackSegmentManager.RectangleDragSelect.Position.Y := TrackSegmentManager.MouseCurrentViewportPoint.Y;
      TrackSegmentManager.RectangleDragSelect.Height := TrackSegmentManager.MouseDownViewportPoint.Y - TrackSegmentManager.MouseCurrentViewportPoint.Y;
    end else
    begin
      TrackSegmentManager.RectangleDragSelect.Position.Y := TrackSegmentManager.MouseDownViewportPoint.Y;
      TrackSegmentManager.RectangleDragSelect.Height := TrackSegmentManager.MouseCurrentViewportPoint.Y - TrackSegmentManager.MouseDownViewportPoint.Y;
    end;

    IsMultiSelect := (ssCtrl in Shift) or (ssShift in Shift);
    TrackSegmentManager.SelectByRect(TrackSegmentManager.RectangleDragSelect.BoundsRect, IsMultiSelect);
    UpdateStatusBar;

  end;

  procedure LocalDoDrag;
  var
    SelectRectSnapPt, DeltaPt: TPointF;
    DeltaX, DeltaY: single;
  begin
    SelectRectSnapPt := TPointF.Create(TrackSegmentManager.MouseCurrentViewportPoint.X - TrackSegmentManager.MouseSelectRectOffset.X, TrackSegmentManager.MouseCurrentViewportPoint.Y - TrackSegmentManager.MouseSelectRectOffset.Y);

    SelectRectSnapPt := TrackSegmentManager.CalculateSnapPt(SelectRectSnapPt, BASE_SEGMENT_WIDTH, BASE_SEGMENT_HEIGHT);

    if SelectRectSnapPt.X < 0 then
      DeltaPt.X := 0
    else
      DeltaPt.X := SelectRectSnapPt.X - TrackSegmentManager.DragSelectCurrentRect.Left;
    if SelectRectSnapPt.Y < 0 then
      DeltaPt.Y := 0
    else
      DeltaPt.Y := SelectRectSnapPt.Y - TrackSegmentManager.DragSelectCurrentRect.Top;

    TrackSegmentManager.MoveSelectedBy(DeltaPt);

    TrackSegmentManager.DragSelectCurrentRect := TrackSegmentManager.SelectionBounds;

    if TrackSegmentManager.DragSelectCurrentRect.Left < ScrollboxSketchpad.ViewportPosition.X then
      ScrollboxSketchpad.ScrollBy(+(TrackSegmentManager.DragSelectCurrentRect.Left - ScrollboxSketchpad.ViewportPosition.X), 0)
    else
      if TrackSegmentManager.DragSelectCurrentRect.Right > (ScrollboxSketchpad.ViewportPosition.X + ScrollboxSketchpad.Width) then
      ScrollboxSketchpad.ScrollBy(-(TrackSegmentManager.DragSelectCurrentRect.Right - (ScrollboxSketchpad.ViewportPosition.X + ScrollboxSketchpad.Width)), 0);


    if TrackSegmentManager.DragSelectCurrentRect.Bottom < ScrollboxSketchpad.ViewportPosition.Y then
      ScrollboxSketchpad.ScrollBy(0, +(TrackSegmentManager.DragSelectCurrentRect.Bottom - ScrollboxSketchpad.ViewportPosition.Y))
    else
    if TrackSegmentManager.DragSelectCurrentRect.Bottom > (ScrollboxSketchpad.ViewportPosition.Y + ScrollboxSketchpad.Height) then
      ScrollboxSketchpad.ScrollBy(0, -(TrackSegmentManager.DragSelectCurrentRect.Bottom - (ScrollboxSketchpad.ViewportPosition.Y + ScrollboxSketchpad.Height)));
  end;

var
  i: Integer;
  DeltaPt: TPointF;
  TempSelectRect: TRectF;
begin
  TrackSegmentManager.MouseCurrentViewportPoint := SketchPadClientToViewport(X, Y);

  case TrackSegmentManager.MouseButton of
    TMouseButton.mbLeft :
      begin
        TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(TrackSegmentManager.MouseCurrentViewportPoint.X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(TrackSegmentManager.MouseCurrentViewportPoint.Y, ffFixed, 4, 4);

        case TrackSegmentManager.State of
          dsNone :
            begin

            end;
          dsDragPending :
            begin
              if LocalDragThresholdMet(TrackSegmentManager.MouseCurrentViewportPoint) then
              begin
                TrackSegmentManager.State := TDragState.dsDragging;
                LocalDoDrag;
              end;
            end;
          dsDragging :
            begin
              LocalDoDrag;
            end;
          dsSelectRectPending :
            begin
              if LocalDragThresholdMet(TrackSegmentManager.MouseCurrentViewportPoint) then
              begin
                TrackSegmentManager.RectangleDragSelect.Position.Point := TrackSegmentManager.MouseDownViewportPoint;
                TrackSegmentManager.RectangleDragSelect.Width := 0;
                TrackSegmentManager.RectangleDragSelect.Height := 0;
                TrackSegmentManager.RectangleDragSelect.Parent := ScrollBoxSketchpad;
                TrackSegmentManager.State := TDragState.dsSelectRect;
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
  TrackSegmentManager.MousePreviousVewportPoint := TrackSegmentManager.MouseCurrentViewportPoint;
end;

procedure TFormLayoutBuilder.ScrollBoxSketchpadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  HitSegment: TTrackSegment;
begin
  ScrollBoxSketchpad.Root.Captured := nil;
  TapDuration.Stop;
//  TapDuration.ElapsedMilliseconds

  TrackSegmentManager.MouseCurrentViewportPoint := SketchPadClientToViewport(X, Y);

  TrackSegmentManager.RectangleDragSelect.Parent := nil;
  if not TrackSegmentManager.EditMode then
  begin
    HitSegment := TrackSegmentManager.FindSegmentByPt(TrackSegmentManager.MouseCurrentViewportPoint.X, TrackSegmentManager.MouseCurrentViewportPoint.Y);
    if Assigned(HitSegment) then
      HitSegment.Click;
  end;
  TrackSegmentManager.State := dsNone;
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

end.
