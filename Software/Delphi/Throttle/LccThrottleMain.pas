unit LccThrottleMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList, lcc.transfer.tcp.client, lcc.transfer.gridconnect.wire.synapse,
  lcc.node, FMX.Edit, System.IOUtils, mustangpeak.fileutilities, System.StartupCopy,
  FMX.Colors, FMX.Layouts, FMX.Objects, FMX.Header;

type
  TTabbedwithNavigationForm = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabControl2: TTabControl;
    TabItem5: TTabItem;
    ToolBar1: TToolBar;
    lblTitle1: TLabel;
    btnNext: TSpeedButton;
    TabItem6: TTabItem;
    ToolBar2: TToolBar;
    lblTitle2: TLabel;
    btnBack: TSpeedButton;
    TabItem2: TTabItem;
    ToolBar3: TToolBar;
    lblTitle3: TLabel;
    TabItem3: TTabItem;
    ToolBar4: TToolBar;
    lblTitle4: TLabel;
    TabItem4: TTabItem;
    ToolBar5: TToolBar;
    lblTitle5: TLabel;
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    CheckBoxTCP: TCheckBox;
    Panel2: TPanel;
    LabelNodeInfoPath: TLabel;
    LabelCdiPath: TLabel;
    FramedVertScrollBox1: TFramedVertScrollBox;
    Layout1: TLayout;
    Label3: TLabel;
    BWTrackBar1: TBWTrackBar;
    Layout2: TLayout;
    BWTrackBar2: TBWTrackBar;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    Label4: TLabel;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    Label5: TLabel;
    Label6: TLabel;
    Rectangle1: TRectangle;
    ArcDial1: TArcDial;
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FEthernetClient: TLccTransferManagerTcpClient;
    FNode: TLccNode;
    { Private declarations }
  public
    { Public declarations }
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
    property EthernetClient: TLccTransferManagerTcpClient read FEthernetClient write FEthernetClient;
    property Node: TLccNode read FNode write FNode;
  end;

var
  TabbedwithNavigationForm: TTabbedwithNavigationForm;

implementation

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}
{$R *.iPad.fmx IOS}

procedure TTabbedwithNavigationForm.Button1Click(Sender: TObject);
begin
  FEthernetClient := TLccTransferManagerTcpClient.Create;
  EthernetClient.Start('10.0.3.164', 12021, False, TGridConnectSendTcpThread, TGridConnectReceiveTcpThread);

  if EthernetClient.Connected then
  begin
    {$IFDEF IOS}
    Node := TLccNode.Create( System.IOUtils.TPath.Combine(GetFolder_Bundle, 'ExampleNodeDefinitionFile.xml'));
   {$ELSE}
    Node := TLccNode.Create(System.IOUtils.TPath.GetDocumentsPath + PathDelim + 'ExampleNodeDefinitionFile.xml');
    {$ENDIF}
    GlobalNodeList.Add(Node);
    Node.AliasIDEngine.Enabled := True;
    Node.Start;
    Application.OnIdle := OnAppIdle;
  end;
end;

procedure TTabbedwithNavigationForm.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControl1.ActiveTab := TabItem1;
end;

procedure TTabbedwithNavigationForm.FormDestroy(Sender: TObject);
begin
  if Assigned(EthernetClient) then
   EthernetClient.Close;
  FreeAndNil(FEthernetClient);
end;

procedure TTabbedwithNavigationForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    if (TabControl1.ActiveTab = TabItem1) and (TabControl2.ActiveTab = TabItem6) then
    begin
      TabControl2.Previous;
      Key := 0;
    end;
  end;
end;

procedure TTabbedwithNavigationForm.FormShow(Sender: TObject);
begin
  {$IFDEF IOS}
   LabelNodeInfoPath.Text := System.IOUtils.TPath.Combine(GetFolder_Bundle, 'ExampleNodeDefinitionFile.xml');
   if FileExists(LabelNodeInfoPath.Text) then
     LabelNodeInfoPath.Text := 'Node XML: ' + LabelNodeInfoPath.Text + ': Found'
   else
     LabelNodeInfoPath.Text := 'Node XML: ' + LabelNodeInfoPath.Text + ': Missing';

   LabelCdiPath.Text := System.IOUtils.TPath.Combine( GetFolder_Bundle, 'ExampleCDI.xml');
   if FileExists(LabelCdiPath.Text) then
     LabelCdiPath.Text := 'Node CDI: ' + LabelCdiPath.Text + ': Found'
   else
     LabelCdiPath.Text := 'Node CDI: ' + LabelCdiPath.Text + ': Missing';
  {$ENDIF}
  {$IFDEF ANDROID}
   LabelNodeInfoPath.Text := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath, 'ExampleNodeDefinitionFile.xml');
   if FileExists(LabelNodeInfoPath.Text) then
     LabelNodeInfoPath.Text := 'Node XML: ' + LabelNodeInfoPath.Text + ': Found'
   else
     LabelNodeInfoPath.Text := 'Node XML: ' + LabelNodeInfoPath.Text + ': Missing';

   LabelCdiPath.Text := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath, 'ExampleCDI.xml');
   if FileExists(LabelCdiPath.Text) then
     LabelCdiPath.Text := 'Node CDI: ' + LabelCdiPath.Text + ': Found'
   else
     LabelCdiPath.Text := 'Node CDI: ' + LabelCdiPath.Text + ': Missing';
  {$ENDIF}
end;

procedure TTabbedwithNavigationForm.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[TabControl1.TabCount - 1] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex + 1];
        Handled := True;
      end;

    sgiRight:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[0] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex - 1];
        Handled := True;
      end;
  end;
end;

procedure TTabbedwithNavigationForm.OnAppIdle(Sender: TObject; var Done: Boolean);
begin
  if Assigned(Node) then
    Node.ProcessMessages;
end;

end.

