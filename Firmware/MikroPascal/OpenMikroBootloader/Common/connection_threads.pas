unit connection_threads;

// Version 0.1
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The initial developer of this code is Jim Kueneman <jimkueneman@yahoo.com> and Vcc
//

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, synaser, synsock, blcksock, Forms;

const
  PATH_LINUX_DEV = '/dev/';
  PATH_OSX_DEV = 'dev/';

const
  TParityChar: array[0..4] of Char = ('N', 'O', 'E', 'M', 'S');
  TComBitsValue: array[0..4] of Integer = (5, 6, 7, 8, 9);
  TBaudRateValue: array[0..8] of Integer = (9600, 19200, 28800, 57600, 115200, 230400, 256000, 333333, 500000);

type
  TComportComponent = class;
  TComportReadThread = class;
  TConnectionBase = class;
  TEthernetComponent = class;

  TCommonConnectionState = (ccs_Disconnected, ccs_Connecting, ccs_Connected, ccs_Disconnecting);
  TParity = (parity_None, parity_Odd, parity_Even, parity_Mark, parity_Space);

  TOnConnectionReceive = procedure(Sender: TConnectionBase; AByte: Byte) of object;
  TOnConnectionError = procedure(Sender: TConnectionBase; ErrorCode: Integer; ErrorMsg: string) of object;
  TOnConnectionChange = procedure(Sender: TConnectionBase; Thread: TThread; AConnectionString: string) of object;

    { TConnectionBase }

  TConnectionBase = class(TComponent)
  private
    FConnectionState: TCommonConnectionState;
    FConnectionString: string;
    FConnectionThread: TThread;
    FErrorID: Integer;
    FErrorMessage: string;
    FOnConnectionChange: TOnConnectionChange;
    FOnConnectionError: TOnConnectionError;
    FOnConnectionReceive: TOnConnectionReceive;
    FReceivedByte: Byte;
    procedure SetEnabled(AValue: Boolean); virtual;
  protected
    FEnabled: Boolean;
    procedure DoConnectionReceive;
    procedure DoConnectionError;
    procedure DoConnectionChange;
    property ErrorID: Integer read FErrorID write FErrorID;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property ReceivedByte: Byte read FReceivedByte write FReceivedByte;
    property ConnectionString: string read FConnectionString write FConnectionString;
    property ConnectionThread: TThread read FConnectionThread write FConnectionThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMessage(AMessage: AnsiString); virtual;
    procedure SendByte(AByte: Byte); virtual;
  published
    property ConnectionState: TCommonConnectionState read FConnectionState;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnConnectionChange: TOnConnectionChange read FOnConnectionChange write FOnConnectionChange;
    property OnConnectionError: TOnConnectionError read FOnConnectionError write FOnConnectionError;
    property OnConnectionReceive: TOnConnectionReceive read FOnConnectionReceive write FOnConnectionReceive;
  end;

  { TComportReadThread }

  TComportReadThread = class(TThread)
  private
    FComport: TBlockSerial;
    FCriticalSection: TRTLCriticalSection;
    FEnded: Boolean;
    FOwner: TComportComponent;
    FReceivedByte: Byte;
  protected
    procedure Execute; override;
    property Comport: TBlockSerial read FComport write FComport;
    property CritialSection: TRTLCriticalSection read FCriticalSection write FCriticalSection;
    property Ended: Boolean read FEnded write FEnded;
    property Owner: TComportComponent read FOwner;
    property ReceivedByte: Byte read FReceivedByte write FReceivedByte;
  public
    constructor Create(AnOwner: TComportComponent); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;


  { TComportComponent }

  TComportComponent = class(TConnectionBase)
  private
    FBaudRate: Integer;
    FComBits: Byte;
    FComportName: string;
    FHardwareHandshake: Boolean;
    FParity: TParity;
    FReadThread: TComportReadThread;
    FSoftwareHandshake: Boolean;
    FStopBits: Integer;
    procedure SetEnabled(AValue: Boolean) override;
  protected
    property ReadThread: TComportReadThread read FReadThread write FReadThread;
    procedure CreateThreads;
    procedure TerminateThreads;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMessage(AMessage: AnsiString); override;
    procedure SendByte(AByte: Byte); override;
  published
    property ComportName: string read FComportName write FComportName;
    property BaudRate: Integer read FBaudRate write FBaudRate;
    property ComBits: Byte read FComBits write FComBits;
    property StopBits: Integer read FStopBits write FStopBits;
    property Parity: TParity read FParity write FParity;
    property SoftwareHandshake: Boolean read FSoftwareHandshake write FSoftwareHandshake;
    property HardwareHandshake: Boolean read FHardwareHandshake write FHardwareHandshake;
  end;

  { TEthernetTCPThread }

  TEthernetTCPThread = class(TThread)
  private
    FCriticalSection: TRTLCriticalSection;
    FEnded: Boolean;
    FInheritedSocketHandle: THandle;
    FLatched: Boolean;
    FOwner: TEthernetComponent;
    FSocket: TTCPBlockSocket;
  protected
    procedure Execute; override;
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    property CritialSection: TRTLCriticalSection read FCriticalSection write FCriticalSection;
    property Ended: Boolean read FEnded write FEnded;
    property Owner: TEthernetComponent read FOwner;
  public
    constructor Create(AnOwner: TEthernetComponent); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    property InheritedSocketHandle: THandle read FInheritedSocketHandle write FInheritedSocketHandle;
    property Latched: Boolean read FLatched write FLatched;
  end;

  { TEthernetListenerThread }

  TEthernetListenerThread = class(TEthernetTCPThread)
  private
  protected
    procedure Execute; override;
  public
    constructor Create(AnOwner: TEthernetComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

  { TConnectionThreadList }

  TConnectionThreadList = class
  private
    FList: TList;
    FLock: TRTLCriticalSection;
    function GetConnectionThread(Index: Integer): TEthernetTCPThread;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function  LockList: TList;
    procedure Remove(Item: Pointer);
    procedure UnlockList;
    property ConnectionThread[Index: Integer]: TEthernetTCPThread read GetConnectionThread; default;
    property Count: Integer read GetCount;
  end;

  { TEthernetComponent }

  TEthernetComponent = class(TConnectionBase)
  private
    FConnectionList: TConnectionThreadList;
    FIpAddress: string;
    FListener: Boolean;
    FPort: Integer;
    FRemoteIpAddress: string;
    FRemotePort: Integer;
    procedure SetEnabled(AValue: Boolean); override;
    procedure SetListener(AValue: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMessage(AMessage: AnsiString); override;
    procedure SendByte(AByte: Byte); override;
  published
    property Listener: Boolean read FListener write SetListener;
    property IpAddress: string read FIpAddress write FIpAddress;
    property Port: Integer read FPort write FPort;
    property RemoteIpAddress: string read FRemoteIpAddress write FRemoteIpAddress;
    property RemotePort: Integer read FRemotePort write FRemotePort;
    property ConnectionThreads: TConnectionThreadList read FConnectionList;
  end;

  { TUSBComponent }

  TUSBComponent = class(TConnectionBase)
  private
    procedure SetEnabled(AValue: Boolean); override;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMessage(AMessage: AnsiString); override;
    procedure SendByte(AByte: Byte); override;
  published
  end;

implementation

{ TConnectionThreadList }

procedure TConnectionThreadList.Add(Item: Pointer);
begin
  LockList;
  try
    // make sure it's not already in the list
    if FList.IndexOf(Item)= -1 then
       FList.Add(Item)
  finally
    UnLockList;
  end;
end;

procedure TConnectionThreadList.Clear;
var
  i: Integer;
begin
  Locklist;
  try
    for i := FList.Count - 1 downto 0  do
    begin
      ConnectionThread[i].Latched := True;
      ConnectionThread[i].Terminate;
      if Assigned(ConnectionThread[i].Socket) then
        ConnectionThread[i].Socket.AbortSocket;      // Kick it out a wait forever
      ConnectionThread[i].Latched := False;
    end;
  finally
    UnLockList;
  end;
  while Count > 0 do
  begin
    Application.ProcessMessages; // Yuck but...
    ThreadSwitch;
  end;
end;

constructor TConnectionThreadList.Create;
begin
  inherited Create;
  InitCriticalSection(FLock);
  FList:=TList.Create;
end;

destructor TConnectionThreadList.Destroy;
begin
  LockList;
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DoneCriticalSection(FLock);
  end;
end;

function TConnectionThreadList.GetConnectionThread(Index: Integer): TEthernetTCPThread;
begin
  Result := TEthernetTCPThread( FList[Index]);
end;

function TConnectionThreadList.GetCount: Integer;
begin
  LockList;
  try
    Result := FList.Count
  finally
    UnlockList;
  end;
end;

function TConnectionThreadList.LockList: TList;
begin
  Result:=FList;
  System.EnterCriticalSection(FLock);
end;

procedure TConnectionThreadList.Remove(Item: Pointer);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TConnectionThreadList.UnlockList;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TEthernetListenerThread }

procedure TEthernetListenerThread.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TEthernetListenerThread.Execute;
var
  TCPThread: TEthernetTCPThread;
begin
  Owner.FConnectionState := ccs_Connecting;
  Owner.ConnectionString := '';
  Owner.ConnectionThread := Self;
  Synchronize(@Owner.DoConnectionChange);
  Socket := TTCPBlockSocket.Create;
  try
    Socket.CreateSocket;
    // Make the socket a listener
    Socket.Bind(Owner.IpAddress, IntToStr(Owner.Port));
    if Socket.LastError = 0 then
    begin
      Socket.SetLinger(True, 1000);
      if Socket.LastError = 0 then
      begin
        Socket.Listen;
        if Socket.LastError = 0 then
        begin
          Owner.ConnectionString := 'Listening on: ' + Owner.IpAddress + ':' + IntToStr(Owner.Port);
              Owner.FConnectionState := ccs_Connected;
          Owner.ConnectionThread := Self;
          Synchronize(@Owner.DoConnectionChange);
        end;
      end;
    end;

    if Socket.LastError <> 0 then
    begin
      Owner.ConnectionString := '';
      Owner.FConnectionState := ccs_Disconnected;
      Owner.ConnectionThread := Self;
      Synchronize(@Owner.DoConnectionChange);

      Owner.ErrorID := Socket.LastError;
      Owner.ErrorMessage := Socket.GetErrorDesc(Owner.ErrorID);
      Synchronize(@Owner.DoConnectionError);
    end else
    begin
      while not Terminated do
      begin
        ThreadSwitch;
        if Socket.CanRead(1000) then
          if Socket.LastError = 0 then
          begin
            TCPThread := TEthernetTCPThread.Create(Owner);
            TCPThread.InheritedSocketHandle := Socket.Accept;
            if Socket.LastError = 0 then
            begin
              Owner.ConnectionThreads.Add(TCPThread);
              TCPThread.Suspended := False;
            end else
            begin
              FreeAndNil(TCPThread);
              Owner.ErrorID := Socket.LastError;
              Owner.ErrorMessage := Socket.GetErrorDesc(Owner.ErrorID);
              Synchronize(@Owner.DoConnectionError);
            end;
          end else
          begin
            Owner.ErrorID := Socket.LastError;
            Owner.ErrorMessage := Socket.GetErrorDesc(Owner.ErrorID);
            Synchronize(@Owner.DoConnectionError);
          end;
      end;
    end;
    Owner.ConnectionString := '';
    Owner.FConnectionState := ccs_Disconnecting;
    Owner.ConnectionThread := Self;
    Synchronize(@Owner.DoConnectionChange);

    while Latched do
      ThreadSwitch;

    Socket.CloseSocket;
  finally
    Owner.ConnectionThreads.Remove(Self);
    FreeAndNil(FSocket);
    Owner.FConnectionState := ccs_Disconnected;
    Owner.ConnectionThread := Self;
    Owner.ConnectionString := '';
    Synchronize(@Owner.DoConnectionChange);
    FEnded := True;
  end
end;

constructor TEthernetListenerThread.Create(AnOwner: TEthernetComponent);
begin
  inherited Create(AnOwner);
end;

destructor TEthernetListenerThread.Destroy;
begin
  inherited Destroy;
end;

{ TEthernetTCPThread }

procedure TEthernetTCPThread.AfterConstruction;
begin
  inherited AfterConstruction;
  InitCriticalSection(FCriticalSection);
  FEnded := False;
  FSocket := nil;
  FInheritedSocketHandle := 0;
end;

procedure TEthernetTCPThread.Execute;
var
  PortOffset: Word;
  ReceivedCount: Integer;
begin
  Owner.FConnectionState := ccs_Connecting;
  Owner.ConnectionString := '';
  Owner.ConnectionThread := Self;
  Synchronize(@Owner.DoConnectionChange);

  Socket := TTCPBlockSocket.Create;
  try
    Socket.SetLinger(False, 0);
    Socket.SetSendTimeout(0);
    Socket.SetRecvTimeout(0);
    Socket.SetTimeout(0);

    if InheritedSocketHandle <> 0 then    //
    begin
      Socket.Socket := InheritedSocketHandle;
      if Socket.LastError = 0 then
      begin
        Socket.GetSins;                     // Back load the IP's / Ports information from the handle
        if Socket.LastError = 0 then
        begin
          // Inherited this handle, need to read back what we can from the socket
          Owner.ConnectionString :=  Socket.GetLocalSinIP + ':' + IntToStr(Socket.GetLocalSinPort) + ' Connected to ' + Socket.GetRemoteSinIP + ':' + IntToStr(Socket.GetRemoteSinPort);
          Owner.FConnectionState := ccs_Connected
        end
        else
          Owner.FConnectionState := ccs_Disconnected;
      end
    end else
    begin
      PortOffset := 0;
      repeat
        Socket.Bind(Owner.IpAddress, IntToStr( Owner.Port + PortOffset));
        Inc(PortOffset)
      until (Socket.LastError <> WSAEADDRINUSE) or (PortOffset > 100) or (PortOffset = $FFFF);

      if Socket.LastError = 0 then
      begin
        Socket.Connect(Owner.RemoteIpAddress, IntToStr(Owner.RemotePort));
        if Socket.LastError = 0 then
        begin
          Owner.ConnectionString := Owner.IpAddress + ':' + IntToStr(Owner.Port) + ' Connected to ' + Owner.RemoteIpAddress + ':' + IntToStr(Owner.RemotePort);
          Owner.FConnectionState := ccs_Connected;
        end else
          Owner.FConnectionState := ccs_Disconnected;
      end
    end;
    // It is either connected or disconnected by here

    Owner.ConnectionThread := Self;
    Synchronize(@Owner.DoConnectionChange);

    if Socket.LastError <> 0 then
    begin
      Owner.ErrorID := Socket.LastError;
      Owner.ErrorMessage := Socket.GetErrorDesc(Owner.ErrorID);
      Synchronize(@Owner.DoConnectionError);
    end else
    begin
      while not Terminated do
      begin
        ThreadSwitch;
        if not Terminated then
        begin
          ReceivedCount := Socket.RecvBufferEx(@Owner.FReceivedByte, 1, -1);
          case Socket.LastError of
            S_OK            : begin
                                Synchronize(@Owner.DoConnectionReceive);
                               end;
            WSAETIMEDOUT    : begin end;               // Normal if nothing to read
            WSAECONNRESET,
            WSAECONNABORTED : begin
                                Terminate
                              end;     // This is normal if the other app is shut down so don't waste time with errors and shutting us down
          else begin
              Owner.ErrorID := Socket.LastError;
              Owner.ErrorMessage := Socket.GetErrorDesc(Owner.ErrorID);
              Synchronize(@Owner.DoConnectionError);
              Terminate;
            end;
          end;
        end;
      end;

      while Latched do
        ThreadSwitch;

      // Is terminated
      Owner.FConnectionState := ccs_Disconnecting;
      Owner.ConnectionString := '';
      Owner.ConnectionThread := Self;
      Synchronize(@Owner.DoConnectionChange);

      Socket.CloseSocket;
    end;
  finally
    FreeAndNil(FSocket);
    Owner.ConnectionThreads.Remove(Self);
    Owner.FConnectionState := ccs_Disconnected;
    Owner.ConnectionString := '';
    Owner.ConnectionThread := Self;
    Synchronize(@Owner.DoConnectionChange);
    FEnded := True;
  end;
end;

constructor TEthernetTCPThread.Create(AnOwner: TEthernetComponent);
begin
  inherited Create(True);
  FOwner := AnOwner;
end;

destructor TEthernetTCPThread.Destroy;
begin
  DoneCriticalsection(FCriticalSection);
  inherited Destroy;
end;

{ TUSBComponent }

constructor TUSBComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TUSBComponent.Destroy;
begin
  inherited Destroy;
end;

procedure TUSBComponent.SendByte(AByte: Byte);
begin
  inherited SendByte(AByte);
end;

procedure TUSBComponent.SendMessage(AMessage: AnsiString);
begin
  inherited SendMessage(AMessage);
end;

procedure TUSBComponent.SetEnabled(AValue: Boolean);
begin
  inherited SetEnabled(AValue);
end;

{ TEthernetComponent }

constructor TEthernetComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectionList := TConnectionThreadList.Create;
end;

destructor TEthernetComponent.Destroy;
begin
  ConnectionThreads.Clear;
  FreeAndNil(FConnectionList);
  inherited Destroy;
end;

procedure TEthernetComponent.SendByte(AByte: Byte);
var
  List: TList;
  i: Integer;
begin
  List := ConnectionThreads.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      if ConnectionThreads[i] is TEthernetTCPThread then
        ConnectionThreads[i].Socket.SendByte(AByte);
    end;
  finally
    ConnectionThreads.UnlockList;
  end;
end;

procedure TEthernetComponent.SendMessage(AMessage: AnsiString);
var
  List: TList;
  i: Integer;
begin
  List := ConnectionThreads.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      if ConnectionThreads[i] is TEthernetTCPThread then
        ConnectionThreads[i].Socket.SendString(AMessage);
    end;
  finally
    ConnectionThreads.UnlockList;
  end;
end;

procedure TEthernetComponent.SetEnabled(AValue: Boolean);
var
  EthernetThread: TEthernetTCPThread;
begin
  if FEnabled <> AValue then
  begin
    if AValue and not (csDesigning in ComponentState) then
    begin
      if Listener then
      begin
        EthernetThread := TEthernetListenerThread.Create(Self);
        ConnectionThreads.Add(EthernetThread);
        EthernetThread.Suspended := False;
      end else
      begin
        EthernetThread := TEthernetTCPThread.Create(Self);
        ConnectionThreads.Add(EthernetThread);
        EthernetThread.Suspended := False;
      end;
    end else
    begin
      ConnectionThreads.Clear;
    end;
    FEnabled := AValue
  end
end;

procedure TEthernetComponent.SetListener(AValue: Boolean);
begin
  if FListener=AValue then Exit;
  FListener:=AValue;
end;

{ TConnectionBase }

constructor TConnectionBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TConnectionBase.Destroy;
begin
  inherited Destroy;
end;

procedure TConnectionBase.DoConnectionChange;
begin
  if Assigned(FOnConnectionChange) then
    OnConnectionChange(Self, ConnectionThread, ConnectionString)
end;

procedure TConnectionBase.DoConnectionError;
begin
  if Assigned(FOnConnectionError) then
    OnConnectionError(Self, ErrorID, ErrorMessage)
end;

procedure TConnectionBase.DoConnectionReceive;
begin
  if Assigned(FOnConnectionReceive) then
    OnConnectionReceive(Self, ReceivedByte)
end;

procedure TConnectionBase.SendByte(AByte: Byte);
begin
   // Override
end;

procedure TConnectionBase.SendMessage(AMessage: AnsiString);
begin
  // Override
end;

procedure TConnectionBase.SetEnabled(AValue: Boolean);
begin
  // Override this
end;

{ TComportReadThread }

procedure TComportReadThread.AfterConstruction;
begin
  inherited AfterConstruction;
  InitCriticalSection(FCriticalSection);
  FEnded := False;
  FComport := nil;
end;

constructor TComportReadThread.Create(AnOwner: TComportComponent);
begin
  inherited Create(False);
  FOwner := AnOwner;
end;

destructor TComportReadThread.Destroy;
begin
  DoneCriticalsection(FCriticalSection);
  inherited Destroy;
end;

procedure TComportReadThread.Execute;
begin
  Owner.FConnectionState := ccs_Connecting;
  Owner.ConnectionString := '';
  Owner.ConnectionThread := Self;
  Synchronize(@Owner.DoConnectionChange);
  Comport := TBlockSerial.Create;
  try
    Comport.LinuxLock:=False;
    {$IFDEF MSWINDOWS}
    Comport.Connect(Owner.ComportName);
    {$ELSE}
      {$IFDEF DARWIN}
      Comport.Connect(PATH_OSX_DEV + Owner.ComportName);
      {$ELSE}
      Comport.Connect(PATH_LINUX_DEV + Owner.ComportName);
      {$ENDIF}
    {$ENDIF}
    if Comport.LastError <> 0 then
    begin
      Owner.ErrorID := Comport.LastError;
      Owner.ErrorMessage := Comport.GetErrorDesc(Owner.ErrorID);
      Synchronize(@Owner.DoConnectionError);
    end else
    begin
      Comport.Config(Owner.BaudRate, Owner.ComBits, TParityChar[Integer( Owner.Parity)], Owner.StopBits, Owner.SoftwareHandshake, Owner.HardwareHandshake);
      if Comport.LastError <> 0 then
      begin
        Owner.ErrorID := Comport.LastError;
        Owner.ErrorMessage := Comport.GetErrorDesc(Owner.ErrorID);
        Synchronize(@Owner.DoConnectionError);
        Terminate
      end
    end;
    if Comport.Handle <> INVALID_HANDLE_VALUE then
    begin
      Owner.FConnectionState := ccs_Connected;
      Owner.ConnectionString := Owner.ComportName;
      Owner.ConnectionThread := Self;
      Synchronize(@Owner.DoConnectionChange);
      while not Terminated do
      begin
        {$IFDEF UNIX}
        ReceivedByte := Comport.RecvByte(500);     // wait until a string comes in   Windows won't wait forever with -1
        {$ELSE}
        ThreadSwitch;
        ReceivedByte := Comport.RecvByte(500);
        {$ENDIF}
        if not Terminated then
        begin
          case Comport.LastError of
            ErrTimeout :
              begin
                { Do nothing }
              end;
            0 :  // Success
              begin
                Owner.ReceivedByte := ReceivedByte;
                Synchronize(@Owner.DoConnectionReceive);
              end;
          else
            if Comport.LastError <> 0 then
            begin
              Owner.ErrorID := Comport.LastError;
              Owner.ErrorMessage := Comport.GetErrorDesc(Owner.ErrorID);
              Synchronize(@Owner.DoConnectionError);
            end;
          end;
        end;
      end;
      // Is terminated
      Owner.FConnectionState := ccs_Disconnecting;
      Owner.ConnectionString := '';
      Owner.ConnectionThread := Self;
      Synchronize(@Owner.DoConnectionChange);
    end;
  finally
    FreeAndNil(FComport);
    Owner.FConnectionState := ccs_Disconnected;
    Owner.ConnectionString := '';
    Owner.ConnectionThread := Self;
    Synchronize(@Owner.DoConnectionChange);
    FEnded := True;
  end;
end;

{ TComportComponent }

constructor TComportComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComportName := '';
end;

destructor TComportComponent.Destroy;
begin
  TerminateThreads;
  inherited Destroy;
end;

procedure TComportComponent.CreateThreads;
begin
  FReadThread := TComportReadThread.Create(Self);
end;

procedure TComportComponent.SendMessage(AMessage: AnsiString);
begin
  if Assigned(ReadThread) then
  begin
    if Assigned(ReadThread.Comport) then
    begin
      EnterCriticalsection(ReadThread.FCriticalSection);
      try
        ReadThread.Comport.SendString(AMessage);
      finally
        LeaveCriticalsection(ReadThread.FCriticalSection);
      end;
    end;
  end;
end;

procedure TComportComponent.SendByte(AByte: Byte);
begin
  if Assigned(ReadThread) then
  begin
    if Assigned(ReadThread.Comport) then
    begin
      EnterCriticalsection(ReadThread.FCriticalSection);
      try
        ReadThread.Comport.SendByte(AByte);
      finally
        LeaveCriticalsection(ReadThread.FCriticalSection);
      end;
    end;
  end;
end;

procedure TComportComponent.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    if AValue and not (csDesigning in ComponentState) then
      CreateThreads
    else
      TerminateThreads;
    FEnabled := AValue
  end
end;

procedure TComportComponent.TerminateThreads;
begin
  if Assigned(ReadThread) then
  begin
    if Assigned(ReadThread) then
    begin
      ReadThread.Terminate;
      if Assigned(ReadThread.Comport) then
        ReadThread.Comport.CloseSocket;
    end;
    while not ReadThread.Ended do
    begin
      Application.ProcessMessages;
      ThreadSwitch;
    end;
  end;
end;

end.

