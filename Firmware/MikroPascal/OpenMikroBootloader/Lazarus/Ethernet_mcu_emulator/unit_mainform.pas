unit unit_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, connection_threads, mcu_bootloader,
  bootloader_driver_lazarus, threadedstringlist;

type

  { TMcuThread }

  TMcuThread = class(TThread)
  private
    FEnded: Boolean;
    FLatched: Boolean;
  protected
    procedure Execute; override;
    property Latched: Boolean read FLatched write FLatched;
  public
    property Ended: Boolean read FEnded;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    ButtonMemoClear: TButton;
    ButtonReset: TButton;
    ButtonRunApplication: TButton;
    CheckBoxOverrideMCU: TCheckBox;
    EditBootloaderAddress: TEdit;
    EditBootloaderSize: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo: TMemo;
    PageControlMain: TPageControl;
    RadioGroupEraseSize: TRadioGroup;
    RadioGroupFlashSize: TRadioGroup;
    RadioGroupWriteSize: TRadioGroup;
    RadioGroupMCU: TRadioGroup;
    StatusBar1: TStatusBar;
    TabSheetMain: TTabSheet;
    TabSheetMcu: TTabSheet;
    TimerStrings: TTimer;
    ToggleBoxEthernetConnect: TToggleBox;
    procedure ButtonMemoClearClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure ButtonRunApplicationClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerStringsTimer(Sender: TObject);
    procedure ToggleBoxEthernetConnectChange(Sender: TObject);
  private
    FEthernet: TEthernetComponent;
    FMcuLoopThread: TMcuThread;
    FThreadedStrings: TThreadStringList;
    procedure SetEthernet(AValue: TEthernetComponent);
    { private declarations }
  protected
    procedure ConnectionReceive(Sender: TConnectionBase; AByte: Byte);
    procedure ConnectionError(Sender: TConnectionBase; ErrorCode: Integer; ErrorMsg: string);
    procedure ConnectionChange(Sender: TConnectionBase; Thread: TThread; AConnectionString: string);
  public
    { public declarations }
    procedure MemoMsg(AMsg: string);
    procedure MemoMsgThreadContext(AMsg: string);
    property McuLoopThread: TMcuThread read FMcuLoopThread write FMcuLoopThread;
    property Ethernet: TEthernetComponent read FEthernet write SetEthernet;
    property ThreadedStrings: TThreadStringList read FThreadedStrings write FThreadedStrings;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TMcuThread }

procedure TMcuThread.Execute;
begin
  FEnded := False;
  while not Terminated do
  begin
    ThreadSwitch;
    Main;
  end;

  FEnded := True;

  while Latched do
    ThreadSwitch;
end;

{ TForm1 }

procedure TForm1.ButtonMemoClearClick(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TForm1.ButtonResetClick(Sender: TObject);
begin

  if Assigned(McuLoopThread) then
  begin
    Memo.Lines.Add('Terminating Mcu Loop Thread');
    McuLoopThread.Latched := True;
    BootloaderEnabled := False;
    McuLoopThread.Terminate;
    while not McuLoopThread.Ended do
    begin
      ThreadSwitch;
      Application.ProcessMessages;
    end;
    McuLoopThread.Latched := False;
    ThreadSwitch;
    FreeAndNil(FMcuLoopThread);
    Memo.Lines.Add('Mcu Loop Thread Terminated');
  end;


  Memo.Lines.Add('Creating new Mcu Loop Thread');
  BootloaderEnabled := True;
  McuLoopThread := TMcuThread.Create(True);
  McuInfoLoaded := False;
  McuLoopThread.Suspended := False;

  while not McuInfoLoaded do
    ThreadSwitch;

  if CheckBoxOverrideMCU.Checked then
  begin
    Memo.Lines.Add('Loding Overridden BootInfo');
    BootInfo.EraseBlockSize := $00000001 shl (RadioGroupEraseSize.ItemIndex + 6);
    BootInfo.WriteBlockSize := $00000001 shl (RadioGroupWriteSize.ItemIndex + 6);
    BootInfo.ProgramFlashSize := $0000001 shl (RadioGroupFlashSize.ItemIndex + 12);
    BootInfo.BootloaderAddress := StrToInt('$' + EditBootloaderAddress.Text);
    BootInfo.BootloaderSize := StrToInt('$' + EditBootloaderSize.Text);
    BootInfo.McuFamily := RadioGroupMCU.ItemIndex;
  end;
  AddMessageCallback := @MemoMsgThreadContext;

  Memo.Lines.Add('New Mcu Loop Thread created');
end;

procedure TForm1.ButtonRunApplicationClick(Sender: TObject);
begin
  WaitForBootLinkDetectedTimout := True;
end;

procedure TForm1.ConnectionChange(Sender: TConnectionBase; Thread: TThread;
  AConnectionString: string);
begin
  StatusBar1.Panels[0].Text := 'Status: ' + AConnectionString;
end;

procedure TForm1.ConnectionError(Sender: TConnectionBase; ErrorCode: Integer; ErrorMsg: string);
begin
  ShowMessage('Connection Error: Error Code = ' + IntToStr(ErrorCode) + ', ' + ErrorMsg);
end;

procedure TForm1.ConnectionReceive(Sender: TConnectionBase; AByte: Byte);
begin
//  Memo.Lines.Add('Byte: 0x' + IntToHex(AByte, 2) + ': Char: ' + IntToStr(AByte));

  EnterCriticalsection(CriticalSection);
  try
    if Assigned(ReceivedByteStream) then
    begin
      ReceivedByteStream.Position := ReceivedByteStream.Size;
      ReceivedByteStream.WriteByte(AByte);
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

  if Assigned(McuLoopThread) then
  begin
    McuLoopThread.Latched := True;
    BootloaderEnabled := False;
    McuLoopThread.Terminate;
    while not McuLoopThread.Ended do
    begin
      ThreadSwitch;
      Application.ProcessMessages;
    end;
    McuLoopThread.Latched := False;
    ThreadSwitch;
    FreeAndNil(FMcuLoopThread);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ThreadedStrings := TThreadStringList.Create;
  Ethernet := TEthernetComponent.Create(Self);
  Ethernet.OnConnectionChange := @ConnectionChange;
  Ethernet.OnConnectionError := @ConnectionError;
  Ethernet.OnConnectionReceive := @ConnectionReceive;
  Ethernet.IpAddress := '127.0.0.1';
  Ethernet.Port := 12334;
  Ethernet.RemoteIpAddress := '127.0.0.1';
  Ethernet.RemotePort := 12333;
  Ethernet.Listener := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  LocalThread: TEthernetComponent;
begin
  LocalThread := Ethernet;
  Ethernet := nil;
  LocalThread.Enabled := False;
  FreeAndNil(LocalThread) ;
  FreeAndNil(FThreadedStrings);
end;


procedure TForm1.MemoMsg(AMsg: string);
begin
  Memo.Lines.BeginUpdate;
  try
    Memo.Lines.Add(AMsg);
  finally
    Memo.Lines.EndUpdate;
  end;
end;

procedure TForm1.MemoMsgThreadContext(AMsg: string);
begin
  ThreadedStrings.Add(AMsg);
end;

procedure TForm1.SetEthernet(AValue: TEthernetComponent);
begin
  if FEthernet <> AValue then
  begin
    FEthernet := AValue;
    bootloader_driver_lazerus_Ethernet := FEthernet;
  end;
end;

procedure TForm1.TimerStringsTimer(Sender: TObject);
var
  Strings: TStringList;
  i: Integer;
begin
  Strings := ThreadedStrings.LockList;
  try
    if Strings.Count > 0 then
    begin
      for i := 0 to Strings.Count - 1 do
        MemoMsg(Strings[i]);
    end;
  finally
    Strings.Clear;
    ThreadedStrings.UnlockList;
  end;
end;

procedure TForm1.ToggleBoxEthernetConnectChange(Sender: TObject);
begin
  if ToggleBoxEthernetConnect.Checked then
  begin
    MemoMsg('Connecting to Ethernet');
    Ethernet.Enabled := True;
  end else
  begin
    MemoMsg('Disconnecting from Ethernet');
    Ethernet.Enabled := False;
  end;
end;

end.

