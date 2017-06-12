unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects, FMX.TabControl, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Actions, FMX.ActnList, FMX.MultiView,
  FMX.Layouts, FMX.ListBox, FMX.Colors, FMX.Edit, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.ComboEdit, mustangpeak.vkbdhelper;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    SpeedButtonMenu: TSpeedButton;
    MultiViewNavigator: TMultiView;
    TabControlMain: TTabControl;
    TabItemThrottle: TTabItem;
    TabItemSelectThrottle: TTabItem;
    TabItemSettings: TTabItem;
    TabItemRoute: TTabItem;
    LabelNewThrottle: TLabel;
    LabelExistingThrottle: TLabel;
    LabelRoute: TLabel;
    RectangleNavigatorBkGnd: TRectangle;
    LabelSettings: TLabel;
    ListBoxSettings: TListBox;
    ListBoxItemSettingsServer: TListBoxItem;
    ListBoxItemSettingsPort: TListBoxItem;
    LabelSettingsServer: TLabel;
    EditSettingsServer: TEdit;
    EditSettingsPort: TEdit;
    LabelSettingsPort: TLabel;
    ListViewRoute: TListView;
    RectangleRouteHeader: TRectangle;
    LayoutNavigatorNewThrottle: TLayout;
    LayoutNavigatorExistingThrottle: TLayout;
    LayoutNavigatorRoute: TLayout;
    LayoutNavigatorSettings: TLayout;
    LayoutMenu: TLayout;
    RectangleThrottleBkGnd: TRectangle;
    ArcDialThrottle: TArcDial;
    LabelThrottleSpeed: TLabel;
    FlowLayout1: TFlowLayout;
    Layout1: TLayout;
    Label4: TLabel;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ColorButton4: TColorButton;
    ColorButton5: TColorButton;
    ColorButton6: TColorButton;
    ColorButton7: TColorButton;
    ColorButton8: TColorButton;
    ColorButton10: TColorButton;
    LabelF00: TLabel;
    LabelF01: TLabel;
    LabelF03: TLabel;
    LabelF04: TLabel;
    LabelF05: TLabel;
    LabelF06: TLabel;
    LabelF07: TLabel;
    LabelF08: TLabel;
    LabelF09: TLabel;
    Layout2: TLayout;
    ListBoxItemLefty: TListBoxItem;
    Label5: TLabel;
    SwitchLefty: TSwitch;
    Layout3: TLayout;
    LabelReverse: TLabel;
    SwitchReverse: TSwitch;
    LabelAddress: TLabel;
    LabelStatusBar: TLabel;
    EditAddress: TEdit;
    LabelTrainName: TLabel;
    Layout4: TLayout;
    Circle1: TCircle;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LayoutNavigatorExistingThrottleClick(Sender: TObject);
    procedure LayoutNavigatorNewThrottleClick(Sender: TObject);
    procedure LayoutNavigatorRouteClick(Sender: TObject);
    procedure LayoutNavigatorSettingsClick(Sender: TObject);
    procedure ArcDialThrottleChange(Sender: TObject);
    procedure LabelReverseClick(Sender: TObject);
    procedure SwitchLeftySwitch(Sender: TObject);
    procedure EditAddressExit(Sender: TObject);
    procedure EditAddressKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditAddressEnter(Sender: TObject);
    procedure ArcDialThrottleEnter(Sender: TObject);
    procedure SpeedButtonMenuClick(Sender: TObject);
  private
    FArcDialLastAngle: single;
    { Private declarations }
  public
    { Public declarations }
    procedure LoadDccAddress;
    property ArcDialLastAngle: single read FArcDialLastAngle write FArcDialLastAngle;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.ArcDialThrottleChange(Sender: TObject);
var
  Delta: single;
  Angle: single;
begin
  if ArcDialThrottle.Value < 0 then
    Angle := 180.0 + (180.0 + ArcDialThrottle.Value)
  else
    Angle := ArcDialThrottle.Value;

  Delta := ArcDialLastAngle - Angle;

  if Angle <> ArcDialLastAngle then
  begin
 //   if abs(ArcDialLastAngle - Angle) <>  then

  end;
  LabelThrottleSpeed.Text := FloatToStrF(Angle, ffGeneral, 0, 0, FormatSettings)
end;

procedure TForm1.ArcDialThrottleEnter(Sender: TObject);
begin
  if ArcDialThrottle.Value < 0 then
    ArcDialLastAngle := 180.0 + (180.0 + ArcDialThrottle.Value)
  else
    ArcDialLastAngle := ArcDialThrottle.Value;
end;

procedure TForm1.EditAddressEnter(Sender: TObject);
begin
  LabelStatusBar.Text := 'Enter a DCC Address';
end;

procedure TForm1.EditAddressExit(Sender: TObject);
begin
  LoadDccAddress
end;

procedure TForm1.EditAddressKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkReturn then
    LoadDccAddress;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  MultiViewNavigator.Mode := TMultiViewMode.Drawer;
  ArcDialThrottle.ValueRange.Min := 0;
  ArcDialThrottle.ValueRange.Max := 127;
 // ArcDialThrottle.ValueRange.Frequency := 127;
end;

procedure TForm1.LabelReverseClick(Sender: TObject);
begin
  SwitchReverse.IsChecked := not SwitchReverse.IsChecked;
end;

procedure TForm1.LayoutNavigatorExistingThrottleClick(Sender: TObject);
begin
  MultiViewNavigator.HideMaster;
  TabControlMain.ActiveTab := TabItemSelectThrottle;
end;

procedure TForm1.LayoutNavigatorNewThrottleClick(Sender: TObject);
begin
  MultiViewNavigator.HideMaster;
  TabControlMain.ActiveTab := TabItemThrottle;
end;

procedure TForm1.LayoutNavigatorRouteClick(Sender: TObject);
begin
  MultiViewNavigator.HideMaster;
  TabControlMain.ActiveTab := TabItemRoute;
end;

procedure TForm1.LayoutNavigatorSettingsClick(Sender: TObject);
begin
  MultiViewNavigator.HideMaster;
  TabControlMain.ActiveTab := TabItemSettings;
end;

procedure TForm1.LoadDccAddress;
begin
  LabelStatusBar.Text := 'Loading Address';
end;

procedure TForm1.SpeedButtonMenuClick(Sender: TObject);
begin
  MultiViewNavigator.ShowMaster;
end;

procedure TForm1.SwitchLeftySwitch(Sender: TObject);
begin
  if not SwitchLefty.IsChecked then
  begin
    LabelAddress.Align := TAlignLayout.Right;
    LabelReverse.Align := TAlignLayout.Left;
  end else
  begin
    LabelAddress.Align := TAlignLayout.Left;
    LabelReverse.Align := TAlignLayout.Right;
  end;
end;

end.
