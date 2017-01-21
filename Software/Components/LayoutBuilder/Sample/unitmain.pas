unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Graphics, StdCtrls,
  Spin, Dialogs, FPCanvas, layoutbuilderframe;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonAddTextDoodle: TButton;
    ButtonAddGraphicDoodle: TButton;
    CheckBox1: TCheckBox;
    CheckBoxDoodlesVisible: TCheckBox;
    ColorDialog: TColorDialog;
    ComboBoxGridStyle: TComboBox;
    FloatSpinEditScale: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelGridColor: TPanel;
    PanelLayoutColor: TPanel;
    SpinEditLayoutWidth: TSpinEdit;
    SpinEditLayoutHeight: TSpinEdit;
    SpinEditGridX: TSpinEdit;
    SpinEditGridY: TSpinEdit;
    procedure ButtonAddGraphicDoodleClick(Sender: TObject);
    procedure ButtonAddTextDoodleClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBoxDoodlesVisibleChange(Sender: TObject);
    procedure ComboBoxGridStyleChange(Sender: TObject);
    procedure FloatSpinEditScaleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PanelGridColorClick(Sender: TObject);
    procedure PanelLayoutColorClick(Sender: TObject);
    procedure SpinEditGridXChange(Sender: TObject);
    procedure SpinEditGridYChange(Sender: TObject);
    procedure SpinEditLayoutHeightChange(Sender: TObject);
    procedure SpinEditLayoutWidthChange(Sender: TObject);
  private
    FBuilderPaintBox: TLayoutBuilderPaintBox;
    { private declarations }
  public
    { public declarations }
    property BuilderPaintBox: TLayoutBuilderPaintBox read FBuilderPaintBox write FBuilderPaintBox;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BuilderPaintBox := TLayoutBuilderPaintBox.Create(Self);
  BuilderPaintBox.Parent := Panel1;
  BuilderPaintBox.Align := alClient;
  BuilderPaintBox.Font.Size := 12;
  BuilderPaintBox.Sketchpad.Color := clBlue;
  BuilderPaintBox.BorderWidth := 10;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  PanelGridColor.Color := BuilderPaintBox.Sketchpad.Grid.Color;
  PanelLayoutColor.Color := BuilderPaintBox.Sketchpad.Color;
  SpinEditLayoutWidth.Value := BuilderPaintBox.Sketchpad.LayoutWidth;
  SpinEditLayoutHeight.Value := BuilderPaintBox.Sketchpad.LayoutHeight;
end;

procedure TForm1.ComboBoxGridStyleChange(Sender: TObject);
begin
  if ComboBoxGridStyle.ItemIndex < 5 then
    BuilderPaintBox.Sketchpad.Grid.Style := TFPPenStyle(ComboBoxGridStyle.ItemIndex)
  else
    BuilderPaintBox.Sketchpad.Grid.Style := psClear;
end;

procedure TForm1.ButtonAddTextDoodleClick(Sender: TObject);
var
  Doodle: TMustangpeakSketchPadTextDoodle;
  i: Integer;
begin
  BuilderPaintBox.LockUpdate;
 // for i := 0 to 1000 do
    Doodle := BuilderPaintBox.Sketchpad.AddTextDoodle(Random(Trunc(BuilderPaintBox.Sketchpad.LayoutWidth)), Random(Trunc(BuilderPaintBox.Sketchpad.LayoutHeight)), 'Testing', clRed, Font);
  BuilderPaintBox.UnLockUpdate;
end;

procedure TForm1.ButtonAddGraphicDoodleClick(Sender: TObject);
var
  Doodle: TMustangpeakSketchPadGraphicDoodle;
  i: Integer;
begin
  BuilderPaintBox.LockUpdate;
 // for i := 0 to 1000 do
    Doodle := BuilderPaintBox.Sketchpad.AddGraphicDoodle(Random(Trunc(BuilderPaintBox.Sketchpad.LayoutWidth)), Random(Trunc(BuilderPaintBox.Sketchpad.LayoutHeight)), 20, 20, 'Testing', clGreen, Font);
  BuilderPaintBox.UnLockUpdate;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
    BuilderPaintBox.Sketchpad.SelectAll
  else
    BuilderPaintBox.Sketchpad.UnSelectAll;
end;

procedure TForm1.CheckBoxDoodlesVisibleChange(Sender: TObject);
begin
  if CheckBoxDoodlesVisible.Checked then
    BuilderPaintBox.Sketchpad.ShowAllDoodles
  else
    BuilderPaintBox.Sketchpad.HideAllDoodles
end;

procedure TForm1.FloatSpinEditScaleChange(Sender: TObject);
begin
  BuilderPaintBox.Sketchpad.Scale := FloatSpinEditScale.Value;
end;

procedure TForm1.PanelGridColorClick(Sender: TObject);
begin
  ColorDialog.Color := BuilderPaintBox.Sketchpad.Grid.Color;
  if ColorDialog.Execute then
  begin
    PanelGridColor.Color := ColorDialog.Color;
    BuilderPaintBox.Sketchpad.Grid.Color := ColorDialog.Color;
  end;
end;

procedure TForm1.PanelLayoutColorClick(Sender: TObject);
begin
  ColorDialog.Color := BuilderPaintBox.Sketchpad.Color;
  if ColorDialog.Execute then
  begin
    BuilderPaintBox.Sketchpad.Color := ColorDialog.Color;
    PanelLayoutColor.Color := ColorDialog.Color;
  end;
end;

procedure TForm1.SpinEditGridXChange(Sender: TObject);
begin
   BuilderPaintBox.Sketchpad.Grid.X := SpinEditGridX.Value;
end;

procedure TForm1.SpinEditGridYChange(Sender: TObject);
begin
  BuilderPaintBox.Sketchpad.Grid.Y := SpinEditGridY.Value;
end;

procedure TForm1.SpinEditLayoutHeightChange(Sender: TObject);
begin
  BuilderPaintBox.Sketchpad.LayoutHeight := SpinEditLayoutHeight.Value;
end;

procedure TForm1.SpinEditLayoutWidthChange(Sender: TObject);
begin
  BuilderPaintBox.Sketchpad.LayoutWidth := SpinEditLayoutWidth.Value;
end;

end.

