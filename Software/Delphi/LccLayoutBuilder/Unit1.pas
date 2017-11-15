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
    ListBoxItemEditPropertiesPanel: TListBoxItem;
    ListBoxItemEditPropertiesHeader: TListBoxItem;
    SpeedButtonEditProperties: TSpeedButton;
    RectangleEditProperties: TRectangle;
    ColorAnimation1: TColorAnimation;
    RectanglePanelContainer: TRectangle;
    SpeedButtonProperitesMaster: TSpeedButton;
    ScrollBoxSketchpad: TScrollBox;
    CheckBoxEditMode: TCheckBox;
    CheckBoxSelectModifier: TCheckBox;
    ActionSaveToXml: TAction;
    ButtonSaveToXml: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ActionLoadFromXml: TAction;
    ButtonLoadFromXml: TButton;
    SpeedButtonSelectAll: TSpeedButton;
    ActionSelectAll: TAction;
    ActionUnSelectAll: TAction;
    SpeedButtonUnSelectAll: TSpeedButton;
    ListBoxItemItemProperitesHeader: TListBoxItem;
    SpeedButtonItemProperties: TSpeedButton;
    ListBoxItemItemProperitesPanel: TListBoxItem;
    RectangleItemProperites: TRectangle;
    ColorAnimation2: TColorAnimation;
    LabelNone: TLabel;
    SpeedButtonDeleteAll: TSpeedButton;
    ActionDelete: TAction;
    ActionCopy: TAction;
    ActionPaste: TAction;
    SpeedButtonCopy: TSpeedButton;
    SpeedButtonPaste: TSpeedButton;
    Rectangle1: TRectangle;
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionLoadFromXmlExecute(Sender: TObject);
    procedure ActionNewStraightSegmentExecute(Sender: TObject);
    procedure ActionNewTurnoutSegmentExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionSaveToXmlExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionUnSelectAllExecute(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CheckBoxEditModeChange(Sender: TObject);
    procedure CheckBoxSelectModifierChange(Sender: TObject);
    procedure ComboBoxMultiViewModeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListBoxItemEditPropertiesHeaderClick(Sender: TObject);
    procedure ListBoxItemItemProperitesHeaderClick(Sender: TObject);
    procedure ListBoxItemStraightClick(Sender: TObject);
    procedure ListBoxItemTurnoutClick(Sender: TObject);
    procedure MultiViewMainPresenterChanging(Sender: TObject; var PresenterClass: TMultiViewPresentationClass);
    procedure MultiViewMainStartShowing(Sender: TObject);
    procedure MultiViewMainStartHiding(Sender: TObject);
    procedure ScrollBoxSketchpadMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ScrollBoxSketchpadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ScrollBoxSketchpadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SpeedButtonProperitesMasterClick(Sender: TObject);

  private
    FTrackSegmentManager: TTrackSegmentManager;
    FTapDuration: TStopwatch;
  protected
    procedure OnSelectionChange(Sender: TObject; SelectableObject: TSelectableObject; Selected: Boolean);
    procedure OnSelectableObjectCreate(Sender: TObject; SelectableObject: TSelectableObject);
    procedure OnSelectableObjectDestroy(Sender: TObject; SelectableObject: TSelectableObject);
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


procedure TFormLayoutBuilder.ActionCopyExecute(Sender: TObject);
begin
  TrackSegmentManager.CopySelected;
end;

procedure TFormLayoutBuilder.ActionDeleteExecute(Sender: TObject);
begin
  TrackSegmentManager.DeleteSelected;
end;

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
  Segment: TSelectableObject;
  FinalX, FinalY: Single;
begin
  Segment := TrackSegmentManager.NewSelectableObject(TTrackSegmentStraight,  ScrollBoxSketchpad);
  FinalX := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( ScrollBoxSketchpad.Width)) - BASE_SEGMENT_WIDTH, BASE_SEGMENT_WIDTH));
  FinalY := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( ScrollBoxSketchpad.Height)) - BASE_SEGMENT_HEIGHT, BASE_SEGMENT_HEIGHT));
  TAnimator.AnimateFloat(Segment, 'Position.X', FinalX , 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
  TAnimator.AnimateFloat(Segment, 'Position.Y', FinalY, 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
end;

procedure TFormLayoutBuilder.ActionNewTurnoutSegmentExecute(Sender: TObject);
var
  Segment: TSelectableObject;
  FinalX, FinalY: Single;
begin
  Segment := TrackSegmentManager.NewSelectableObject(TTrackSegmentTurnout, ScrollBoxSketchpad);
  FinalX := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( ScrollBoxSketchpad.Width)) - BASE_SEGMENT_WIDTH, BASE_SEGMENT_WIDTH));
  FinalY := Max(0, TrackSegmentManager.CalculateSnap(Random(Round( ScrollBoxSketchpad.Height)) - BASE_SEGMENT_HEIGHT, BASE_SEGMENT_HEIGHT));
  TAnimator.AnimateFloat(Segment, 'Position.X', FinalX , 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
  TAnimator.AnimateFloat(Segment, 'Position.Y', FinalY, 0.25, TAnimationType.&In, TInterpolationType.Quadratic);
end;

procedure TFormLayoutBuilder.ActionPasteExecute(Sender: TObject);
begin
  TrackSegmentManager.PasteTo(ScrollBoxSketchpad);
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

procedure TFormLayoutBuilder.ActionSelectAllExecute(Sender: TObject);
begin
  TrackSegmentManager.SelectAll;
end;

procedure TFormLayoutBuilder.ActionUnSelectAllExecute(Sender: TObject);
begin
  TrackSegmentManager.UnSelectAll;
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

procedure TFormLayoutBuilder.CheckBoxSelectModifierChange(Sender: TObject);
begin
  TrackSegmentManager.SelectionModifier := CheckBoxSelectModifier.IsChecked
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
  // Don't allow the scroll to "pull" past 0, it make dragging/selecting impossible
  ScrollBoxSketchpad.AniCalculations.BoundsAnimation := False;

  TapDuration := TStopwatch.Create;

  ComboBoxMultiViewMode.ItemIndex := 0; // Set to platform, assumes at design time it is -1
  ListBoxProperites.Width := 0;         // Mainly for mobile devices

  FTrackSegmentManager := TTrackSegmentManager.Create(nil);

  TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService);
  TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, WindowService);

  TrackSegmentManager.OnSelectionChange := OnSelectionChange;
  TrackSegmentManager.OnSelectableObjectCreate := OnSelectableObjectCreate;
  TrackSegmentManager.OnSelectableObjectDestroy := OnSelectableObjectDestroy;

end;

procedure TFormLayoutBuilder.FormDestroy(Sender: TObject);
begin
  TrackSegmentManager.SelectableObject.Clear;
  TrackSegmentManager.DisposeOf;
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

procedure TFormLayoutBuilder.ListBoxItemItemProperitesHeaderClick(Sender: TObject);
var
  TotalH: single;
  i: Integer;
begin
  if SpeedButtonItemProperties.ImageIndex = 33 then
  begin
    SpeedButtonItemProperties.ImageIndex := 34;
    TAnimator.AnimateFloat(ListBoxItemItemProperitesPanel, 'Height', 0, 0.3, TAnimationType.In, TInterpolationType.Quadratic);
  end else
  begin
    SpeedButtonItemProperties.ImageIndex := 33;
    TotalH := -1;
    for i := 0 to RectangleItemProperites.ControlsCount - 1 do
    begin
      if RectangleItemProperites.Controls[i].Visible then
        if RectangleItemProperites.Controls[i].Position.Y > TotalH then
          TotalH := RectangleItemProperites.Controls[i].Position.Y + RectangleItemProperites.Controls[i].Height;
    end;
    TAnimator.AnimateFloat(ListBoxItemItemProperitesPanel, 'Height', TotalH, 0.3, TAnimationType.Out, TInterpolationType.Quadratic);
  end;
end;

procedure TFormLayoutBuilder.ListBoxItemEditPropertiesHeaderClick(Sender: TObject);
var
  TotalH: single;
  i: Integer;
begin
  if SpeedButtonEditProperties.ImageIndex = 33 then
  begin
    SpeedButtonEditProperties.ImageIndex := 34;
    TAnimator.AnimateFloat(ListBoxItemEditPropertiesPanel, 'Height', 0, 0.3, TAnimationType.In, TInterpolationType.Quadratic);
  end else
  begin
    SpeedButtonEditProperties.ImageIndex := 33;
    TotalH := -1;
    for i := 0 to RectangleEditProperties.ControlsCount - 1 do
    begin
      if RectangleEditProperties.Controls[i].Visible then
        if RectangleEditProperties.Controls[i].Position.Y > TotalH then
          TotalH := RectangleEditProperties.Controls[i].Position.Y + RectangleEditProperties.Controls[i].Height;
    end;
    TAnimator.AnimateFloat(ListBoxItemEditPropertiesPanel, 'Height', TotalH, 0.3, TAnimationType.Out, TInterpolationType.Quadratic);
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

procedure TFormLayoutBuilder.OnSelectableObjectCreate(Sender: TObject; SelectableObject: TSelectableObject);
begin
  LabelTrackSegmentCount.Text := 'Segments: ' + IntToStr(TrackSegmentManager.SelectableObject.Count);
end;

procedure TFormLayoutBuilder.OnSelectableObjectDestroy(Sender: TObject; SelectableObject: TSelectableObject);
begin
  LabelTrackSegmentCount.Text := 'Segments: ' + IntToStr(TrackSegmentManager.SelectableObject.Count);
end;

procedure TFormLayoutBuilder.OnSelectionChange(Sender: TObject; SelectableObject: TSelectableObject; Selected: Boolean);
begin
  LabelStatusSelection.Text :=  'Selections: ' + IntToStr(TrackSegmentManager.Selection.Count);
end;

procedure TFormLayoutBuilder.ScrollBoxSketchpadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  TrackSegmentManager.MouseDownScrollBox(ScrollBoxSketchpad, Sender, Button, Shift, X, Y);
  TapDuration.Start;
end;

procedure TFormLayoutBuilder.ScrollBoxSketchpadMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  TrackSegmentManager.MouseMoveScrollBox(ScrollBoxSketchpad, Sender, Shift, X, Y, BASE_SEGMENT_WIDTH, BASE_SEGMENT_HEIGHT);
  TextStatusMousePos.Text := 'Mouse Pos X:' + FloatToStrF(TrackSegmentManager.MouseCurrentViewportPoint.X, ffFixed, 4, 4) + ' Y: ' + FloatToStrF(TrackSegmentManager.MouseCurrentViewportPoint.Y, ffFixed, 4, 4);
end;

procedure TFormLayoutBuilder.ScrollBoxSketchpadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  TrackSegmentManager.MouseUpScrollBox(ScrollBoxSketchpad, Sender, Button, Shift, X, Y);
  TapDuration.Stop;
//  TapDuration.ElapsedMilliseconds
end;

procedure TFormLayoutBuilder.SpeedButtonProperitesMasterClick(Sender: TObject);
begin
  if ListBoxProperites.Width > 0  then
    TAnimator.AnimateFloat(ListBoxProperites, 'Width', 0, 0.2, TAnimationType.&In, TInterpolationType.Exponential)
  else
    TAnimator.AnimateFloat(ListBoxProperites, 'Width', 175, 0.2, TAnimationType.&In, TInterpolationType.Exponential);
end;

end.
