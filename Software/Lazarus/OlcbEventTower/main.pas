unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ActnList, virtuallistview;

type

  { TFormEventTower }

  TFormEventTower = class(TForm)
    ActionEditEvent: TAction;
    ActionDeleteEvent: TAction;
    ActionAddEvent: TAction;
    ActionListMain: TActionList;
    ImageListListview: TImageList;
    ImageListMain: TImageList;
    ListViewEvents: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBarEvents: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButtonAddEvent: TToolButton;
    procedure ActionAddEventExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewEventsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  private
    FVirtualListview: TVirtualListview;
    FShownOnce: Boolean;
    { private declarations }
    procedure UpdateUI;
  protected
    property VirtualListview: TVirtualListview read FVirtualListview write FVirtualListview;
  public
    { public declarations }
    property ShownOnce: Boolean read FShownOnce;
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

procedure TFormEventTower.FormShow(Sender: TObject);
var
  Item: TVirtualListviewItem;
begin
  UpdateUI;
  if not ShownOnce then
  begin
    FShownOnce := True;
    VirtualListview := TVirtualListview.Create(Self);
    VirtualListview.Parent := Panel2;
    VirtualListview.Align := alClient;
    VirtualListview.Visible := True;
    VirtualListview.Images := ImageListListview;

    VirtualListview.BeginUpdate;

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
    VirtualListview.EndUpdate;
  end;
end;

procedure TFormEventTower.ListViewEventsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateUI
end;

procedure TFormEventTower.UpdateUI;
begin
  ActionEditEvent.Enabled := ListViewEvents.SelCount = 1;
  ActionDeleteEvent.Enabled := ListViewEvents.SelCount > 0;
end;

end.

