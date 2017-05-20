unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ActnList, StdCtrls, Spin, virtuallistview;

type

  { TFormEventTower }

  TFormEventTower = class(TForm)
    ActionEditEvent: TAction;
    ActionDeleteEvent: TAction;
    ActionAddEvent: TAction;
    ActionListMain: TActionList;
    ButtonCaptionFont: TButton;
    ButtonDetailsFont: TButton;
    CheckBoxUseImages: TCheckBox;
    ComboBoxAlign: TComboBox;
    ComboBoxVAlign: TComboBox;
    FontDialog: TFontDialog;
    ImageListListview: TImageList;
    ImageListMain: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ListViewEvents: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpinEditCellHeight: TSpinEdit;
    SpinEditCaptionLineCount: TSpinEdit;
    SpinEditItemCount: TSpinEdit;
    SpinEditDetailsIndent: TSpinEdit;
    SpinEditCaptionIndent: TSpinEdit;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBarEvents: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButtonAddEvent: TToolButton;
    VirtualListview1: TVirtualListview;
    procedure ActionAddEventExecute(Sender: TObject);
    procedure ButtonCaptionFontClick(Sender: TObject);
    procedure ButtonDetailsFontClick(Sender: TObject);
    procedure CheckBoxUseImagesChange(Sender: TObject);
    procedure ComboBoxAlignChange(Sender: TObject);
    procedure ComboBoxVAlignChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewEventsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure SpinEditCaptionIndentChange(Sender: TObject);
    procedure SpinEditCaptionLineCountChange(Sender: TObject);
    procedure SpinEditCellHeightChange(Sender: TObject);
    procedure SpinEditDetailsIndentChange(Sender: TObject);
    procedure SpinEditItemCountChange(Sender: TObject);
    procedure VirtualListview1DebugEvent(Sender: TObject);
  private
    FShownOnce: Boolean;
    { private declarations }
    procedure UpdateUI;
  public
    { public declarations }
    property ShownOnce: Boolean read FShownOnce;

    procedure RebuildItems;
  end;

var
  FormEventTower: TFormEventTower;

implementation

{$R *.lfm}

{ TFormEventTower }

procedure TFormEventTower.ActionAddEventExecute(Sender: TObject);
var
  ListItem: TListItem;
begin
  ListItem := ListViewEvents.Items.Add;
  ListItem.ImageIndex := 0;
  ListItem.StateIndex := 1;
  ListItem.Caption := 'New Event';
  ListItem.SubItems.Add('[unknown]');
  ListItem.SubItems.Add('[unknown]');
  ListItem.SubItems.Add('[unknown]');
end;

procedure TFormEventTower.ButtonCaptionFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(VirtualListview1.Font);
  if FontDialog.Execute then
    VirtualListview1.Font.Assign(FontDialog.Font);
end;

procedure TFormEventTower.ButtonDetailsFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(VirtualListview1.DetailsFont);
  if FontDialog.Execute then
    VirtualListview1.DetailsFont.Assign(FontDialog.Font);
end;

procedure TFormEventTower.CheckBoxUseImagesChange(Sender: TObject);
begin
  if CheckBoxUseImages.Checked then
    VirtualListview1.Images := ImageListListview
  else
    VirtualListview1.Images := nil;
  RebuildItems;
end;

procedure TFormEventTower.ComboBoxAlignChange(Sender: TObject);
begin
  case ComboBoxAlign.ItemIndex of
    0: VirtualListview1.Alignment := taLeftJustify;
    1: VirtualListview1.Alignment := taCenter;
    2: VirtualListview1.Alignment := taRightJustify;
  end;
end;

procedure TFormEventTower.ComboBoxVAlignChange(Sender: TObject);
begin
  case ComboBoxVAlign.ItemIndex of
    0: VirtualListview1.TextLayout := vtlTop;
    1: VirtualListview1.TextLayout := vtlCenter;
    2: VirtualListview1.TextLayout := vtlBottom;
  end;
end;

procedure TFormEventTower.FormShow(Sender: TObject);
var
  Item: TVirtualListviewItem;
begin
  UpdateUI;
  if not ShownOnce then
  begin
    FShownOnce := True;
  {  VirtualListview.BeginUpdate;

    Item := VirtualListview.Items.Add('New Event');
    Item.Captions.Add('Type: Block');
    Item.ImageIndex := 2;

    Item := VirtualListview.Items.Add('New Event');
    Item.Captions.Add('Type: Block');
    Item.ImageIndex := 2;

    Item := VirtualListview.Items.Add('New Event');
    Item.Captions.Add('Type: Block');
    Item.ImageIndex := 2;

    Item := VirtualListview.Items.Add('New Event');
    Item.Captions.Add('Type: Block');
    Item.ImageIndex := 2;

    Item := VirtualListview.Items.Add('New Event');
    Item.Captions.Add('Type: Block');
    Item.ImageIndex := 2;

    Item := VirtualListview.Items.Add('New Event');
    Item.Captions.Add('Type: Block');
    Item.ImageIndex := 2;

    VirtualListview.TextLayout := vtlCenter;
    VirtualListview.Alignment := taCenter;
    VirtualListview.CaptionLineCount := 2;
    VirtualListview.DetailsIndent := 0;
    VirtualListview.CaptionIndent := 0;
    VirtualListview.EndUpdate;  }

    SpinEditCellHeight.Value := VirtualListview1.DefaultItemHeight;
    SpinEditCaptionIndent.Value := VirtualListview1.CaptionIndent;
    SpinEditDetailsIndent.Value := VirtualListview1.DetailsIndent;
    SpinEditCaptionLineCount.Value := VirtualListview1.CaptionLineCount;
  end;
end;

procedure TFormEventTower.ListViewEventsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  UpdateUI
end;

procedure TFormEventTower.SpinEditCaptionIndentChange(Sender: TObject);
begin
  VirtualListview1.CaptionIndent:= SpinEditCaptionIndent.Value;
end;

procedure TFormEventTower.SpinEditCaptionLineCountChange(Sender: TObject);
begin
  VirtualListview1.CaptionLineCount := SpinEditCaptionLineCount.Value;
  RebuildItems;
end;

procedure TFormEventTower.SpinEditCellHeightChange(Sender: TObject);
begin
  VirtualListview1.DefaultItemHeight := SpinEditCellHeight.Value;
end;

procedure TFormEventTower.SpinEditDetailsIndentChange(Sender: TObject);
begin
  VirtualListview1.DetailsIndent := SpinEditDetailsIndent.Value;
end;

procedure TFormEventTower.SpinEditItemCountChange(Sender: TObject);
begin
  RebuildItems
end;

procedure TFormEventTower.VirtualListview1DebugEvent(Sender: TObject);
begin
  StatusBar1.Panels[2].Text := 'Position: ' + IntToStr(VirtualListview1.VertScrollbar.Position);
end;

procedure TFormEventTower.UpdateUI;
begin
  ActionEditEvent.Enabled := ListViewEvents.SelCount = 1;
  ActionDeleteEvent.Enabled := ListViewEvents.SelCount > 0;
end;

procedure TFormEventTower.RebuildItems;
var
  i, j: Integer;
  Item: TVirtualListviewItem;
begin
  VirtualListview1.BeginUpdate;
  VirtualListview1.Items.Clear;
  for i := 0 to SpinEditItemCount.Value-1 do
  begin
    Item := VirtualListview1.Items.Add('Item: ' + IntToStr(i), 0);
    for j := 1 to VirtualListview1.CaptionLineCount - 1 do
      Item.Captions.Add('Detail Line: ' + IntToStr(j-1));
  end;
  VirtualListview1.EndUpdate;
end;

end.

