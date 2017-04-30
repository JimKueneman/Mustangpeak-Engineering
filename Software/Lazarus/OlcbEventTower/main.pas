unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ActnList, EasyListview;

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
    FFirstShow: Boolean;
    { private declarations }
    procedure UpdateUI;
  public
    { public declarations }
    property FirstShow: Boolean read FFirstShow;
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
begin
  UpdateUI;
  if FirstShow then
  begin
    FFirstShow := True;;
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

