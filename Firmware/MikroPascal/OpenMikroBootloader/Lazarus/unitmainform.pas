unit unitmainform;

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

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  {$IFDEF FPC}
  FileUtil,
  LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  StdCtrls, Menus, ActnList, Spin, ExtCtrls, file_utilities, common_settings,
  synaser, connection_threads, ConnectionDefines, intel_hex_parser, SynMemo,
  mcu_loader_base, KGrids, KEdits, types, mapform;

const
  BUNDLENAME             = 'Open mikroBootloader';
  PATH_LINUX_APP_FOLDER  = 'openmikrobootloader/';
  PATH_SETTINGS_FILE     = 'settings.ini';

const
  STATE_RECEIVE_NEW_MESSAGE    = 0;
  STATE_RECEIVE_FLASH_INFO     = 1;

type
  TFlashInfoRawBuffer = array[0..SizeOf(TMcuInfo)-1] of Byte;

type

  TFormBootloader = class;

  { TConnectionManager }

  TConnectionManager = class(TComponent)
  private
    FActiveConnection: TConnectionBase;
    FComport: TComportComponent;
    FEthernet: TEthernetComponent;
    FOwner: TFormBootloader;
    FUSB: TUSBComponent;
  protected
    procedure OnConnectionError(Sender: TConnectionBase; ErrorCode: Integer; ErrorMsg: string);
    procedure OnConnectionReceive(Sender: TConnectionBase; AByte: Byte);
    procedure OnConnectionChange(Sender: TConnectionBase; Thread: TThread; AConnectionString: string);
    property Owner: TFormBootloader read FOwner;
  public
    constructor Create(AnOwner: TFormBootloader); reintroduce; virtual;
    destructor Destroy; override;
    procedure CloseAllConnections;
    property ActiveConnection: TConnectionBase read FActiveConnection write FActiveConnection;
    property Comport: TComportComponent read FComport write FComport;
    property USB: TUSBComponent read FUSB write FUSB;
    property Ethernet: TEthernetComponent read FEthernet write FEthernet;
  end;

  { TFormBootloader }

  TFormBootloader = class(TForm)
    ActionToolsSettingsShowWin: TAction;
    ActionToolsPreferenceShowMac: TAction;
    ActionHelpAboutShow: TAction;
    ActionList: TActionList;
    Button1: TButton;
    ButtonLoadFromFileErase: TButton;
    ButtonSaveToFileErase: TButton;
    ButtonShowMap: TButton;
    ButtonMemoClear: TButton;
    ButtonRescanComport: TButton;
    Button3: TButton;
    ButtonBootloaderConnect: TButton;
    ButtonBootloaderWrite: TButton;
    ButtonLocalHost: TButton;
    ButtonRemoteLocalHost: TButton;
    ComboBoxBaud: TComboBox;
    ComboBoxComPort: TComboBox;
    ComboBoxDataBits: TComboBox;
    ComboBoxFlowControl: TComboBox;
    ComboBoxParity: TComboBox;
    ComboBoxStopBits: TComboBox;
    EditEthernetLocalIP: TEdit;
    EditEthernetRemoteIP: TEdit;
    KFileNameEdit: TKFileNameEdit;
    Label10: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelBaud: TLabel;
    LabelBuild1: TLabel;
    LabelBuildDate: TLabel;
    LabelComPort: TLabel;
    LabelCPU: TLabel;
    LabelDataBits: TLabel;
    LabelFlowControl: TLabel;
    LabelMyName: TLabel;
    LabelNodeExplorer: TLabel;
    LabelParity: TLabel;
    LabelStopBits: TLabel;
    LabelTargetCPU: TLabel;
    LabelTargetOperatingSystem: TLabel;
    LabelTargetOS: TLabel;
    LabelURLFreePascal: TLabel;
    LabelURLLazarus: TLabel;
    LabelWrittenIn: TLabel;
    MainMenu: TMainMenu;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar: TProgressBar;
    RadioGroupBootloader: TRadioGroup;
    SaveDialog: TSaveDialog;
    ShapeLink: TShape;
    SpinEditEtherneRemotePort: TSpinEdit;
    SpinEditEthernetLocalPort: TSpinEdit;
    StatusBar: TStatusBar;
    SynMemo: TSynMemo;
    TabSheetCDC: TTabSheet;
    TabSheetSD: TTabSheet;
    TabSheetCAN: TTabSheet;
    TabSheetHexMap: TTabSheet;
    TabSheetAbout: TTabSheet;
    TabSheetBoot: TTabSheet;
    TabSheetComPort: TTabSheet;
    TabSheetComPort1: TTabSheet;
    TabSheetComPort2: TTabSheet;
    TabSheetEthernet: TTabSheet;
    TabSheetUSB: TTabSheet;
    TreeViewHexMap: TTreeView;
    procedure ButtonBootloaderWriteClick(Sender: TObject);
    procedure ButtonLoadFromFileEraseClick(Sender: TObject);
    procedure ButtonMemoClearClick(Sender: TObject);
    procedure ButtonRescanComportClick(Sender: TObject);
    procedure ButtonBootloaderConnectClick(Sender: TObject);
    procedure ButtonSaveToFileEraseClick(Sender: TObject);
    procedure ButtonShowMapClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure KFileNameEditChange(Sender: TObject);
    procedure RadioGroupBootloaderClick(Sender: TObject);
    procedure TabSheetComPortHide(Sender: TObject);
    procedure TabSheetComPortShow(Sender: TObject);
    procedure TabSheetEthernetHide(Sender: TObject);
    procedure TabSheetEthernetShow(Sender: TObject);
    procedure TabSheetHexMapShow(Sender: TObject);
    procedure TabSheetUSBHide(Sender: TObject);
    procedure TabSheetUSBShow(Sender: TObject);
  private
    FAppAboutCmd: TMenuItem;
    FConnectionManager: TConnectionManager;
    FConnectionState: TCommonConnectionState;
    FConnectionString: string;
    FFlashInfoRawBufferIndex: Byte;
    FIntelHexParser: TIntelHexParser;
    FiReceiveState: Byte;
    FBootloading: Boolean;
    FConnectionEstablished: Boolean;
    FMcuInfo: TMcuInfo;
    FMcuLoader: TMcuLoaderBase;
    {$IFDEF DARWIN}
    FOSXMenu: TMenuItem;
    FOSXPrefCmd: TMenuItem;
    FOSXSep1Cmd: TMenuItem;
    {$ENDIF}
    FSettingsFilePath: string;
    FSettingsLocked: Boolean;
    FShownOnce: Boolean;
    FFlashInfoRawBuffer: TFlashInfoRawBuffer;
    FState_EraseBlockIndex: Integer;
    FState_WriteBlockIndex: Integer;
    procedure SetMcuLoader(AValue: TMcuLoaderBase);
  protected
    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
    property iReceiveState: Byte read FiReceiveState write FiReceiveState;
    property FlashInfoRawBuffer: TFlashInfoRawBuffer read FFlashInfoRawBuffer write FFlashInfoRawBuffer;
    property FlashInfoRawBufferIndex: Byte read FFlashInfoRawBufferIndex write FFlashInfoRawBufferIndex;
    procedure AddLine(Line: string);
    procedure CustomAddress(BlockList: TPhysicalAddressBlockList);
    procedure InsertNewBaudrateValueIntoComboBox(Value: LongWord);
    procedure LoadSettings(SettingType: TLoadSettingType);
    procedure OnConnectionError(Sender: TConnectionBase; ErrorCode: Integer; ErrorMsg: string);
    procedure OnConnectionReceive(Sender: TConnectionBase; AByte: Byte);
    procedure OnConnectionChange(Sender: TConnectionBase; Thread: TThread; AConnectionString: string);
    function McuToLoader(Mcu: Byte): TMcuLoaderBase;
    function McuToHexInfo(Mcu: Byte): TIntelHexInfo;
    procedure ProcessIntelHexFile;
    procedure StoreSettings(SettingType: TLoadSettingType);
    procedure ScanComPorts;
    procedure State_EraseCurrentBlock;
    procedure State_WriteCurrentBlock;
    procedure UpdateTime;
  public
    { public declarations }
    property ConnectionManager: TConnectionManager read FConnectionManager write FConnectionManager;
    property ConnectionState: TCommonConnectionState read FConnectionState write FConnectionState;
    property ConnectionString: string read FConnectionString write FConnectionString;
    property ConnectionEstablished: Boolean read FConnectionEstablished write FConnectionEstablished;
    property Bootloading: Boolean read FBootloading write FBootloading;
    property McuInfo: TMcuInfo read FMcuInfo write FMcuInfo;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property SettingsFilePath: string read FSettingsFilePath write FSettingsFilePath;
    property SettingsLocked: Boolean read FSettingsLocked write FSettingsLocked;
    property IntelHexParser: TIntelHexParser read FIntelHexParser write FIntelHexParser;
    property McuLoader: TMcuLoaderBase read FMcuLoader write SetMcuLoader;
    property State_EraseBlockIndex: Integer read FState_EraseBlockIndex;
    property State_WriteBlockIndex: Integer read FState_WriteBlockIndex;

    procedure UpdateUI;
  end;

var
  FormBootloader: TFormBootloader;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

//{$L hid.o}

{ TConnectionManager }

procedure TConnectionManager.CloseAllConnections;
begin
  ActiveConnection := nil;
  USB.Enabled := False;
  Ethernet.Enabled := False;
  Comport.Enabled := False;
end;

constructor TConnectionManager.Create(AnOwner: TFormBootloader);
begin
  inherited Create(AnOwner);
  FActiveConnection := nil;
  FOwner := AnOwner;
  FUSB := TUSBComponent.Create(Self);
  FEthernet := TEthernetComponent.Create(Self);
  FComport := TComportComponent.Create(Self);
  Comport.OnConnectionError := @OnConnectionError;
  Comport.OnConnectionChange := @OnConnectionChange;
  Comport.OnConnectionReceive := @OnConnectionReceive;
  Ethernet.OnConnectionError := @OnConnectionError;
  Ethernet.OnConnectionChange := @OnConnectionChange;
  Ethernet.OnConnectionReceive := @OnConnectionReceive;
  USB.OnConnectionError := @OnConnectionError;
  USB.OnConnectionChange := @OnConnectionChange;
  USB.OnConnectionReceive := @OnConnectionReceive;
end;

destructor TConnectionManager.Destroy;
begin
  CloseAllConnections;
  FreeAndNil(FComport);
  FreeAndNil(FUSB);
  FreeAndNil(FEthernet);
  inherited Destroy;
end;

procedure TConnectionManager.OnConnectionChange(Sender: TConnectionBase; Thread: TThread;
  AConnectionString: string);
begin
  Owner.OnConnectionChange(Sender, Thread, AConnectionString);
end;

procedure TConnectionManager.OnConnectionError(Sender: TConnectionBase; ErrorCode: Integer; ErrorMsg: string);
begin
  Owner.OnConnectionError(Sender, ErrorCode, ErrorMsg);
end;

procedure TConnectionManager.OnConnectionReceive(Sender: TConnectionBase; AByte: Byte);
begin
  Owner.OnConnectionReceive(Sender, AByte);
end;


{ TFormBootloader }

procedure TFormBootloader.ButtonMemoClearClick(Sender: TObject);
begin
  SynMemo.Clear;
end;

procedure TFormBootloader.ButtonRescanComportClick(Sender: TObject);
begin
  ScanComPorts
end;

procedure TFormBootloader.ButtonBootloaderConnectClick(Sender: TObject);
begin
  AddLine('Sending Sync to connect to Bootloader');
  ProgressBar.Position := 0;
  FState_EraseBlockIndex := 0;
  FState_WriteBlockIndex := 0;
  if Assigned(ConnectionManager.ActiveConnection) then
    ConnectionManager.ActiveConnection.SendByte(CMD_SYNC);
end;

procedure TFormBootloader.ButtonSaveToFileEraseClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if SaveDialog.Execute then
  begin
    Stream := TFileStream.Create(SaveDialog.FileName, fmCreate);
    TreeViewHexMap.SaveToStream(Stream);
    Stream.Free;
  end;
end;

procedure TFormBootloader.ButtonShowMapClick(Sender: TObject);
begin
  FormMemoryMap.HexParser := IntelHexParser;
  FormMemoryMap.Show
end;

procedure TFormBootloader.ButtonBootloaderWriteClick(Sender: TObject);
begin
  if IntelHexParser.PhysicalEraseBlockList.Count > 0 then
  begin
    AddLine('Erasing MCU program area.....');
    State_EraseCurrentBlock;
  end else
    AddLine('Nothing to Erase');
end;

procedure TFormBootloader.ButtonLoadFromFileEraseClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if OpenDialog.Execute then
  begin
    Stream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    TreeViewHexMap.LoadFromStream(Stream);
    TreeViewHexMap.FullExpand;
    Stream.Free;
  end;
end;

procedure TFormBootloader.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FormMemoryMap.HexParser := nil;
  FreeAndNil(FConnectionManager);
  FreeAndNil(FIntelHexParser);
end;

procedure TFormBootloader.FormCreate(Sender: TObject);
begin
  FConnectionManager := TConnectionManager.Create(Self);
  FConnectionEstablished := False;
  FBootloading := False;
  FiReceiveState := 0;
  FIntelHexParser := TIntelHexParser.Create;
  FState_WriteBlockIndex := 0;
  FState_EraseBlockIndex := 0;
  IntelHexParser.CustomAddressFunc := @CustomAddress;
end;

procedure TFormBootloader.FormShow(Sender: TObject);
begin
  if not ShownOnce then
  begin
    {$IFDEF DARWIN}
    OSXMenu := TMenuItem.Create(Self);  {Application menu}
    OSXMenu.Caption := #$EF#$A3#$BF;  {Unicode Apple logo char}
    MainMenu.Items.Insert(0, OSXMenu);

    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    AppAboutCmd.Caption := 'About ' + BUNDLENAME;
    OSXMenu.Add(AppAboutCmd);  {Add About as item in application menu}

    OSXSep1Cmd := TMenuItem.Create(Self);
    OSXSep1Cmd.Caption := '-';
    OSXMenu.Add(OSXSep1Cmd);

    ActionToolsPreferenceShowMac.ShortCut := ShortCut(VK_OEM_COMMA, [ssMeta]);
    OSXPrefCmd := TMenuItem.Create(Self);
    OSXPrefCmd.Action := ActionToolsPreferenceShowMac;
    OSXMenu.Add(OSXPrefCmd);
    ActionToolsSettingsShowWin.Visible := False;
    {$ELSE}
    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
  //  MenuItemHelp.Add(AppAboutCmd);
    {$ENDIF}
    {$IFDEF Linux}
    SettingsFilePath:= GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE;
    GlobalSettings.LoadFromFile(UTF8ToSys( GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE));
    {$ELSE}
    SettingsFilePath := GetSettingsPath + PATH_SETTINGS_FILE;
      {$IFDEF FPC}
      GlobalSettings.LoadFromFile(UTF8ToSys( GetSettingsPath + PATH_SETTINGS_FILE));
      {$ELSE}
      GlobalSettings.LoadFromFile(GetSettingsPath + PATH_SETTINGS_FILE);
      {$ENDIF}
    {$ENDIF}

    {$IFDEF FPC}
    LabelBuildDate.Caption := {$I %DATE%} + ': ' + {$I %TIME%};
    LabelTargetOS.Caption := {$I %FPCTARGETOS%};
    LabelTargetCPU.Caption := {$I %FPCTARGETCPU%};
    {$ENDIF}

    LoadSettings(lstCom);
    LoadSettings(lstEthernet);
    LoadSettings(lstGeneral);

    ShapeLink.Brush.Color := clBtnShadow;

    ShownOnce := True;
  end;
  UpdateUI
end;

procedure TFormBootloader.KFileNameEditChange(Sender: TObject);
begin
  UpdateUI;
  FormMemoryMap.BuildGrids
end;

procedure TFormBootloader.InsertNewBaudrateValueIntoComboBox(Value: LongWord);
var
  i: Integer;
  Found: Boolean; //Found an item where to insert the new value
begin
  Found := False;
  for i := 0 to ComboBoxBaud.Items.Count - 1 do
    if StrToIntDef(ComboBoxBaud.Items.Strings[i], 0) > Value then
    begin
      Found := True;
      ComboBoxBaud.Items.Insert(i, IntToStr(Value));
      Break;
    end;

  if not Found then //all values found are smaller than what is to be added
    ComboBoxBaud.Items.Add(IntToStr(Value));
end;

procedure TFormBootloader.LoadSettings(SettingType: TLoadSettingType);
begin
  if not SettingsLocked then
  begin
    SettingsLocked := True;
    try
      case SettingType of
        lstCom:
          begin
            ScanComPorts;
            ComboBoxComPort.ItemIndex := ComboBoxComPort.Items.IndexOf(GlobalSettings.ComPort.Port);
            if (ComboBoxComPort.ItemIndex < 0) and (ComboBoxComPort.Items.Count > 0) then
              ComboBoxComPort.ItemIndex := 0;
            ComboBoxBaud.ItemIndex := ComboBoxBaud.Items.IndexOf(IntToStr(GlobalSettings.ComPort.BaudRate));
            if ComboBoxBaud.ItemIndex = -1 then //loaded value is not on the list
            begin
              InsertNewBaudrateValueIntoComboBox(GlobalSettings.ComPort.BaudRate); //add the new value
              ComboBoxBaud.ItemIndex := ComboBoxBaud.Items.IndexOf(IntToStr(GlobalSettings.ComPort.BaudRate));  //select it now
            end;
            ComboBoxDataBits.ItemIndex := ComboBoxDataBits.Items.IndexOf(IntToStr( GlobalSettings.ComPort.DataBits));
            ComboBoxStopBits.ItemIndex := GlobalSettings.ComPort.StopBits;  // Problem because of the 1.5 Bits
            ComboBoxParity.ItemIndex := Integer( GlobalSettings.ComPort.Parity);
            ComboBoxFlowControl.ItemIndex := Integer( GlobalSettings.ComPort.FlowControl);
          end;
        lstEthernet:
          begin
            EditEthernetLocalIP.Text := GlobalSettings.Ethernet.LocalIP;
            EditEthernetRemoteIP.Text := GlobalSettings.Ethernet.RemoteIP;
            SpinEditEthernetLocalPort.Value := GlobalSettings.Ethernet.ListenPort;
            SpinEditEtherneRemotePort.Value := GlobalSettings.Ethernet.ClientPort;
          end;
        lstGeneral:
          begin

          end;
      end;
    finally
      SettingsLocked := False;
    end;
  end;
end;

function TFormBootloader.McuToHexInfo(Mcu: Byte): TIntelHexInfo;
begin
  case Mcu of
    FAMILY_PIC16F : begin
          Result.HexType := iht_INHX8M;    // PIC16
          Result.BytesPerInstruction := 2;
          Result.AddressIncrement := 1;
          Result.DoubleAddress := True;
        end;
    FAMILY_PIC16F_ENHANCED : begin
          Result.HexType := iht_INHX8M;    // PIC16 enhanced
          Result.BytesPerInstruction := 2;
          Result.AddressIncrement := 1;
          Result.DoubleAddress := True;
        end;
    FAMILY_PIC18F : begin
          Result.HexType := iht_INHX8M;    // PIC18
          Result.BytesPerInstruction := 2;
          Result.AddressIncrement := 2;
          Result.DoubleAddress := False;
        end;
    FAMILY_dsPIC30 : begin                       // dsPIC30
          Result.HexType := iht_INH24_dsPIC30;
          Result.BytesPerInstruction := 3;
          Result.AddressIncrement := 2;
          Result.DoubleAddress := True;
        end;
    FAMILY_PIC24,                                // PIC24
    FAMILY_dsPIC33 : begin                       // dsPIC33
          Result.HexType := iht_INH24_dsPIC33;
          Result.BytesPerInstruction := 3;
          Result.AddressIncrement := 2;
          Result.DoubleAddress := True;
        end;
    FAMILY_PIC32 : begin
          Result.HexType := iht_INHX32;          // PIC32
          Result.BytesPerInstruction := 4;
          Result.AddressIncrement := 4;
          Result.DoubleAddress := False;
        end;
  end;
end;

function TFormBootloader.McuToLoader(Mcu: Byte): TMcuLoaderBase;
begin
 case Mcu of
    FAMILY_PIC16F          : Result := TPic16FLoader.Create;
    FAMILY_PIC16F_ENHANCED : Result := TPic16FEnhanced.Create;
    FAMILY_PIC18F          : Result := TPic18FLoader.Create;
    FAMILY_PIC24           : Result := TPic24Loader.Create;
    FAMILY_dsPIC30         : Result := TPic30Loader.Create;
    FAMILY_dsPIC33         : Result := TPic33Loader.Create;
    FAMILY_PIC32           : Result := TPic32Loader.Create;
  end;
end;

procedure TFormBootloader.OnConnectionError(Sender: TConnectionBase;
  ErrorCode: Integer; ErrorMsg: string);
begin
  if Sender is TComportComponent then
  begin
    {$IFDEF MSWINDOWS}
    ShowMessage('Could not connect to Comport [' + ComboBoxComPort.Text + '] : ErrorCode=' + IntToStr(ErrorCode) + ', ' + ErrorMsg);
    {$ELSE}
      {$IFDEF DARWIN}
      ShowMessage('Could not connect to Comport [' + PATH_OSX_DEV + ComboBoxComPort.Text + '] : ErrorCode=' + IntToStr(ErrorCode) + ', ' + ErrorMsg);
      {$ELSE}
      ShowMessage('Could not connect to Comport [' + PATH_LINUX_DEV + ComboBoxComPort.Text + '] : ErrorCode=' + IntToStr(ErrorCode) + ', ' + ErrorMsg);
      {$ENDIF}
    {$ENDIF}
  end;
  ConnectionEstablished := False;
  Bootloading := False;
  UpdateUI
end;

procedure TFormBootloader.OnConnectionReceive(Sender: TConnectionBase; AByte: Byte);
var
  i: Integer;
begin
  case iReceiveState of
    STATE_RECEIVE_NEW_MESSAGE :
        begin
          case AByte of
            CMD_LINK :
                begin
                  AddLine('Link Received');
                  ShapeLink.Brush.Color := clGreen;
                  ConnectionEstablished := True;
                  UpdateUI;
                end;
            CMD_UNLINK :
                begin
                  AddLine('UnLink Received');
                  ShapeLink.Brush.Color := clBtnShadow;
                  Bootloading := False;
                  ConnectionEstablished := False;
                  UpdateUI
                end;
            CMD_SYNC :
                begin
                  // Sync Complete
                  AddLine('Sync Received: Entering Bootloader Mode');
                  Bootloading := True;
                  AddLine('Requesting Flash Info');
                  if Assigned(ConnectionManager.ActiveConnection) then
                    ConnectionManager.ActiveConnection.SendByte(CMD_REQEUST_FLASH_DATA);
                end;
            CMD_REQEUST_FLASH_DATA :
                begin
                  // Requested Data
                  AddLine('Flash Info Request Received');
                  FillChar( FFlashInfoRawBuffer, SizeOf(FFlashInfoRawBuffer), $00);
                  FillChar( FMcuInfo, SizeOf(FMcuInfo), $00);
                  FiReceiveState := STATE_RECEIVE_FLASH_INFO;
                  FlashInfoRawBufferIndex := 0;
                end;
            CMD_ERASE_BLOCKS :
                begin
                 // Erase Complete
                  AddLine('Erase Blocks Done Received');
                  Inc(FState_EraseBlockIndex);
                  if State_EraseBlockIndex < IntelHexParser.PhysicalEraseBlockList.Count then
                    State_EraseCurrentBlock
                  else begin
                    AddLine('Writing Blocks..............');
                    State_WriteCurrentBlock;
                  end;
                end;
            CMD_WRITE,
            CMD_WRITE_BLOCK :
                begin
                  // Write Complete
                  AddLine('Write Block Done Received');
                  Inc(FState_WriteBlockIndex);
                  if State_WriteBlockIndex < IntelHexParser.PhysicalWriteBlockList.Count then
                    State_WriteCurrentBlock
                  else begin
                    AddLine('Resetting MCU');
                    ConnectionManager.ActiveConnection.SendByte(CMD_RESET);
                  end;
                end
          else
            AddLine('Unknown Message Received');
          end
        end;
      STATE_RECEIVE_FLASH_INFO :
        begin
          FFlashInfoRawBuffer[FlashInfoRawBufferIndex] := AByte;
          Inc(FFlashInfoRawBufferIndex);
          if FlashInfoRawBufferIndex = 2 then
            FMcuInfo.StructSize := PWord( @FFlashInfoRawBuffer[0])^;
          if FlashInfoRawBufferIndex > 2 then
          begin
            if FlashInfoRawBufferIndex >= McuInfo.StructSize then
            begin
              FMcuInfo.EraseBlockSize := PDWord( @FFlashInfoRawBuffer[2])^;
              FMcuInfo.WriteBlockSize := PDWord( @FFlashInfoRawBuffer[6])^;
              FMcuInfo.ProgramFlashSize := PDWord( @FFlashInfoRawBuffer[10])^;
              FMcuInfo.BootloaderAddress := PDWord( @FFlashInfoRawBuffer[14])^;
              FMcuInfo.BootloaderSize := PDWord( @FFlashInfoRawBuffer[18])^;
              FMcuInfo.McuFamily := PByte( @FFlashInfoRawBuffer[22])^;
              FMcuInfo.Revision[0] := FFlashInfoRawBuffer[23];
              FMcuInfo.Revision[1] := FFlashInfoRawBuffer[24];
              for i := 0 to (32-1) do
                FMcuInfo.ApplicationName[i] := Char( FlashInfoRawBuffer[i+25]);
              ProcessIntelHexFile;
              FiReceiveState := STATE_RECEIVE_NEW_MESSAGE;
              UpdateUI;
            end
          end;
        end else
          AddLine('Lost State' + LF);
    end;
end;

procedure TFormBootloader.ProcessIntelHexFile;
var
  IgnoreList: TIgnoreBoundsGroups;
  HexInfo: TIntelHexInfo;
  AlignWriteBlockOnEraseSize: Boolean;
begin
  IgnoreList := TIgnoreBoundsGroups.Create;
  try
    AddLine('Creating Loader');
    McuLoader := McuToLoader(McuInfo.McuFamily);
    AddLine('Parsing HEX file');
    HexInfo := McuToHexInfo(McuInfo.McuFamily);
    McuLoader.IgnoreAddress(McuInfo, HexInfo, IgnoreList);
    AlignWriteBlockOnEraseSize := (McuInfo.McuFamily = FAMILY_PIC16F);
    if IntelHexParser.ParseHex(KFileNameEdit.Text, HexInfo, McuInfo.EraseBlockSize, McuInfo.WriteBlockSize, 1, IgnoreList, AlignWriteBlockOnEraseSize) then
    begin
      AddLine('Creating Write Block List');
      IntelHexParser.BuildWriteBlockList;
      AddLine('Creating Erase Block List');
      IntelHexParser.BuildEraseBlockList;
      AddLine('Fixing up Jump Addresses');
      McuLoader.FixupBootloader(McuInfo, IntelHexParser);
      ProgressBar.Max := IntelHexParser.PhysicalEraseBlockList.Count + IntelHexParser.PhysicalWriteBlockList.Count - 1;
      FormMemoryMap.BuildGrids;
    end else
    begin
      ShowMessage('Error parsing the HEX file');
    end;
  finally
    FreeAndNil(IgnoreList)
  end;
end;

procedure TFormBootloader.OnConnectionChange(Sender: TConnectionBase; Thread: TThread;
  AConnectionString: string);
var
  ThreadCount: Integer;
begin
  if not Assigned(ConnectionManager) then
    Exit;

  ConnectionState := Sender.ConnectionState;
  ConnectionString := AConnectionString;

  ThreadCount := ConnectionManager.Ethernet.ConnectionThreads.Count;
  if ThreadCount > 0 then
    Dec(ThreadCount);
  case ConnectionState of
    ccs_Disconnected :
      begin
        if Thread is TEthernetListenerThread then
          Statusbar.Panels[0].Text := 'Status: Disconnected'
        else
        if Thread is TEthernetTCPThread then
          Statusbar.Panels[1].Text := ' Client Count: ' + IntToStr(ThreadCount)
        else
          Statusbar.Panels[0].Text := 'Status: Disconnected'
      end;
    ccs_Connecting :
      begin
        if Thread is TEthernetListenerThread then
          Statusbar.Panels[0].Text := 'Status: Connecting'
        else
        if Thread is TEthernetTCPThread then
          Statusbar.Panels[1].Text := ' Client Count: ' + IntToStr(ThreadCount)
        else
          Statusbar.Panels[0].Text := 'Status: Connecting'
      end;
    ccs_Connected :
      begin
        if Thread is TEthernetListenerThread then
          Statusbar.Panels[0].Text := ConnectionString
        else
        if Thread is TEthernetTCPThread then
          Statusbar.Panels[1].Text := ' Client Count: ' + IntToStr(ThreadCount)
        else
          Statusbar.Panels[0].Text := ConnectionString;
      end;
    ccs_Disconnecting :
      begin
        if Thread is TEthernetListenerThread then
          Statusbar.Panels[0].Text := 'Status: Disconnecting'
        else
        if Thread is TEthernetTCPThread then
          Statusbar.Panels[1].Text := ' Client Count: ' + IntToStr(ThreadCount)
        else
          Statusbar.Panels[0].Text := 'Status: Disconnecting'
      end;
  end;
end;

procedure TFormBootloader.RadioGroupBootloaderClick(Sender: TObject);
begin
  case RadioGroupBootloader.ItemIndex of
    0 : ConnectionManager.CloseAllConnections;
    1 : begin
          ConnectionManager.CloseAllConnections;
          ConnectionManager.Comport.ComportName := ComboBoxComPort.Caption;
          ConnectionManager.Comport.BaudRate := StrToInt(ComboBoxBaud.Caption);
          ConnectionManager.Comport.ComBits := StrToInt(ComboBoxDataBits.Caption);
          case ComboBoxStopBits.ItemIndex of
            0 : ConnectionManager.Comport.StopBits := SB1;
            1 : ConnectionManager.Comport.StopBits := SB1andHalf;
            2 : ConnectionManager.Comport.StopBits := SB2;
          end;
          ConnectionManager.Comport.SoftwareHandshake := ComboBoxFlowControl.ItemIndex = 3;
          ConnectionManager.Comport.HardwareHandshake := ComboBoxFlowControl.ItemIndex = 1;
          ConnectionManager.Comport.Enabled := True;
          ConnectionManager.ActiveConnection := ConnectionManager.Comport;
        end;
    2 : begin
          ConnectionManager.CloseAllConnections;
          ConnectionManager.Ethernet.IpAddress := EditEthernetLocalIP.Text;
          ConnectionManager.Ethernet.Port := SpinEditEthernetLocalPort.Value;
          ConnectionManager.Ethernet.RemoteIpAddress := EditEthernetRemoteIP.Text;
          ConnectionManager.Ethernet.RemotePort := SpinEditEtherneRemotePort.Value;
          ConnectionManager.Ethernet.Listener := True;
          ConnectionManager.Ethernet.Enabled := True;
          ConnectionManager.ActiveConnection := ConnectionManager.Ethernet;
        end;
    3 : begin
          ConnectionManager.CloseAllConnections;
          ConnectionManager.USB.Enabled := True;
          ConnectionManager.ActiveConnection := ConnectionManager.USB;
        end;
  end;
  UpdateUI
end;

procedure TFormBootloader.ScanComPorts;
begin
  ComboBoxComPort.Items.Delimiter := ';';
  ComboBoxComPort.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  if ComboBoxComPort.Items.Count > 0 then
    ComboBoxComPort.ItemIndex := 0;
end;

procedure TFormBootloader.SetMcuLoader(AValue: TMcuLoaderBase);
begin
  if FMcuLoader=AValue then Exit;
  FreeAndNil(FMcuLoader);
  FMcuLoader:=AValue;
end;

procedure TFormBootloader.State_EraseCurrentBlock;
begin
  // Always set the address as contigious blocks are handled in the Count parameter sent
  AddLine('Setting Erase Address: ' + IntToHex(IntelHexParser.PhysicalEraseBlockList[State_EraseBlockIndex].AddressStart, 8));
  ConnectionManager.ActiveConnection.SendByte(CMD_SET_ADDRESS);
  ConnectionManager.ActiveConnection.SendByte(CMD_SET_ADDRESS_ERASE);
  ConnectionManager.ActiveConnection.SendByte( Highest(IntelHexParser.PhysicalEraseBlockList[State_EraseBlockIndex].AddressStart));
  ConnectionManager.ActiveConnection.SendByte( Higher(IntelHexParser.PhysicalEraseBlockList[State_EraseBlockIndex].AddressStart));
  ConnectionManager.ActiveConnection.SendByte( High(IntelHexParser.PhysicalEraseBlockList[State_EraseBlockIndex].AddressStart));
  ConnectionManager.ActiveConnection.SendByte( Low(IntelHexParser.PhysicalEraseBlockList[State_EraseBlockIndex].AddressStart));

  AddLine('Erasing: ' + IntToStr(IntelHexParser.PhysicalEraseBlockList[State_EraseBlockIndex].ByteCount) + ' byte block');
  ConnectionManager.ActiveConnection.SendByte(CMD_ERASE_BLOCKS);
  ConnectionManager.ActiveConnection.SendByte( Highest(IntelHexParser.PhysicalEraseBlockList[State_EraseBlockIndex].BlockCount));
  ConnectionManager.ActiveConnection.SendByte( Higher(IntelHexParser.PhysicalEraseBlockList[State_EraseBlockIndex].BlockCount));
  ConnectionManager.ActiveConnection.SendByte( High(IntelHexParser.PhysicalEraseBlockList[State_EraseBlockIndex].BlockCount));
  ConnectionManager.ActiveConnection.SendByte( Low(IntelHexParser.PhysicalEraseBlockList[State_EraseBlockIndex].BlockCount));

  AddLine('Waiting for response..');

  ProgressBar.Position := State_EraseBlockIndex + State_WriteBlockIndex;
end;

procedure TFormBootloader.State_WriteCurrentBlock;
var
  i: Integer;
  WriteBlock: TPhysicalWriteBlock;
  Str: string;
begin
  WriteBlock := IntelHexParser.PhysicalWriteBlockList[State_WriteBlockIndex];
  AddLine('Setting Write Address: ' + IntToHex(WriteBlock.AddressStart, 8));
  ConnectionManager.ActiveConnection.SendByte(CMD_SET_ADDRESS);
  ConnectionManager.ActiveConnection.SendByte(CMD_SET_ADDRESS_WRITE);
  ConnectionManager.ActiveConnection.SendByte( Highest(WriteBlock.AddressStart));
  ConnectionManager.ActiveConnection.SendByte( Higher(WriteBlock.AddressStart));
  ConnectionManager.ActiveConnection.SendByte( High(WriteBlock.AddressStart));
  ConnectionManager.ActiveConnection.SendByte( Low(WriteBlock.AddressStart));

  AddLine('Writing Block: ' + IntToStr(WriteBlock.ByteCount) + ' byte block');
  if WriteBlock.ByteCount = McuInfo.WriteBlockSize then
  begin
    Str := '0x';
    ConnectionManager.ActiveConnection.SendByte(CMD_WRITE_BLOCK);
    for i := 0 to McuInfo.WriteBlockSize - 1 do
    begin
      Str := Str + IntToHex(WriteBlock.DataBlock[i], 2);
      ConnectionManager.ActiveConnection.SendByte(WriteBlock.DataBlock[i]);
    end;
    AddLine(Str);
    AddLine('Waiting for response..');
  end else
  begin
    ConnectionManager.ActiveConnection.SendByte(CMD_WRITE);
    ConnectionManager.ActiveConnection.SendByte( Highest(WriteBlock.ByteCount));
    ConnectionManager.ActiveConnection.SendByte( Higher(WriteBlock.ByteCount));
    ConnectionManager.ActiveConnection.SendByte( High(WriteBlock.ByteCount));
    ConnectionManager.ActiveConnection.SendByte( Low(WriteBlock.ByteCount));
    for i := 0 to WriteBlock.ByteCount - 1 do
      ConnectionManager.ActiveConnection.SendByte(WriteBlock.DataBlock[i]);
  end;
  ProgressBar.Position := State_EraseBlockIndex + State_WriteBlockIndex;
end;

procedure TFormBootloader.UpdateTime;
begin

end;

procedure TFormBootloader.StoreSettings(SettingType: TLoadSettingType);
begin
  if not SettingsLocked then
  begin
    case SettingType of
      lstCom:
        begin
          GlobalSettings.ComPort.Port := ComboBoxComPort.Text;
          if (ComboBoxBaud.Text > '') and
             (StrToIntDef( ComboBoxBaud.Text, -1) <> -1) and
             (ComboBoxBaud.Text <> '-1') and
             (StrToIntDef( ComboBoxBaud.Text, -1) > 0) then //check for valid integer
            GlobalSettings.ComPort.BaudRate := StrToInt( ComboBoxBaud.Text)
          else
          begin
            ShowMessage('Please select or fill in a positive integer for Baud Rate.');
            PageControl.ActivePage := TabSheetComPort;
            if Visible then //don't focus an invisible component! Perhaps parents also need to be checked.
              ComboBoxBaud.SetFocus;
          end;

          GlobalSettings.ComPort.DataBits := StrToInt( ComboBoxDataBits.Text);
          GlobalSettings.ComPort.StopBits := ComboBoxStopBits.ItemIndex;  // Problem because of the 1.5 Bits
          GlobalSettings.ComPort.Parity := TComPortParity( ComboBoxParity.ItemIndex);
          GlobalSettings.ComPort.FlowControl := TComPortFlowControl( ComboBoxFlowControl.ItemIndex);
        end;
      lstEthernet:
        begin
          GlobalSettings.Ethernet.LocalIP := EditEthernetLocalIP.Text;      // Should validate this
          GlobalSettings.Ethernet.RemoteIP := EditEthernetRemoteIP.Text;      // Should validate this
          GlobalSettings.Ethernet.ListenPort := SpinEditEthernetLocalPort.Value;
          GlobalSettings.Ethernet.ClientPort := SpinEditEtherneRemotePort.Value;
        end;
      lstGeneral:
        begin

        end;
    end;
    {$IFDEF FPC}
    GlobalSettings.SaveToFile(UTF8ToSys( SettingsFilePath));
    {$ELSE}
    GlobalSettings.SaveToFile(SettingsFilePath);
    {$ENDIF}
  end;
end;

procedure TFormBootloader.TabSheetHexMapShow(Sender: TObject);
var
  MCUNode, EraseNode, EraseNodeChild: TTreeNode;
  EraseBlock: TPhysicalEraseBlock;
  i: Integer;
begin
  TreeViewHexMap.BeginUpdate;
  try
    TreeViewHexMap.Items.Clear;
  finally
    TreeViewHexMap.EndUpdate;
  end;

  if Assigned(McuLoader) then
  begin
    TreeViewHexMap.BeginUpdate;
    try
      TreeViewHexMap.Items.Clear;
      MCUNode := TreeViewHexMap.Items.AddChild(nil, 'MCU Information');
      TreeViewHexMap.Items.AddChild(MCUNode, 'EraseBlock Size: 0x' + IntToHex(McuInfo.EraseBlockSize, 8) + ' [' + IntToStr(McuInfo.EraseBlockSize) + ']');
      TreeViewHexMap.Items.AddChild(MCUNode, 'WriteBlock Size: 0x' + IntToHex(McuInfo.WriteBlockSize, 8) + ' [' + IntToStr(McuInfo.WriteBlockSize) + ']');
      TreeViewHexMap.Items.AddChild(MCUNode, 'Program Flash Size: 0x' + IntToHex(McuInfo.ProgramFlashSize, 8) + ' [' + IntToStr(McuInfo.ProgramFlashSize) + ']');
      TreeViewHexMap.Items.AddChild(MCUNode, 'Bootloader Address: 0x' + IntToHex(McuInfo.BootloaderAddress, 8) + ' [' + IntToStr(McuInfo.BootloaderAddress) + ']');
      TreeViewHexMap.Items.AddChild(MCUNode, 'Bootloader Size: 0x' + IntToHex(McuInfo.BootloaderSize, 8) + ' [' + IntToStr(McuInfo.BootloaderSize) + ']');
      TreeViewHexMap.Items.AddChild(MCUNode, 'MCU Family: ' + McuFamilyToStr(McuInfo.McuFamily));
      TreeViewHexMap.Items.AddChild(MCUNode, 'Revision: ' + IntToStr(McuInfo.Revision[0]) + '.' + IntToStr(McuInfo.Revision[1]) );
      TreeViewHexMap.Items.AddChild(MCUNode, 'Application Name: ' + McuInfo.ApplicationName);

      EraseNode := TreeViewHexMap.Items.AddChild(nil, 'Erase Information');
      for i := 0 to IntelHexParser.PhysicalEraseBlockList.Count - 1 do
      begin
        EraseBlock := IntelHexParser.PhysicalEraseBlockList[i];
        EraseNodeChild := TreeViewHexMap.Items.AddChild(EraseNode, 'Block ' + IntToStr(i) + ': Block Count = ' + IntToStr(EraseBlock.ByteCount));
        TreeViewHexMap.Items.AddChild(EraseNodeChild, 'From: 0x' + IntToHex(EraseBlock.AddressStart, 8));
        TreeViewHexMap.Items.AddChild(EraseNodeChild, 'To:   0x' + IntToHex(EraseBlock.AddressLast, 8));
      end;
      McuLoader.LoadHexMapInUI(McuInfo, IntelHexParser, TreeViewHexMap);
    finally
      TreeViewHexMap.FullExpand;
      TreeViewHexMap.EndUpdate;
    end;
  end;
end;

procedure TFormBootloader.TabSheetComPortHide(Sender: TObject);
begin
  StoreSettings(lstCom)
end;

procedure TFormBootloader.TabSheetComPortShow(Sender: TObject);
begin
  LoadSettings(lstCom)
end;

procedure TFormBootloader.TabSheetEthernetHide(Sender: TObject);
begin
  StoreSettings(lstEthernet)
end;

procedure TFormBootloader.TabSheetEthernetShow(Sender: TObject);
begin
  LoadSettings(lstEthernet)
end;

procedure TFormBootloader.TabSheetUSBHide(Sender: TObject);
begin
  StoreSettings(lstUSB)
end;

procedure TFormBootloader.TabSheetUSBShow(Sender: TObject);
begin
  LoadSettings(lstUSB)
end;

procedure TFormBootloader.AddLine(Line: string);
begin
  SynMemo.Lines.BeginUpdate;
  try
    SynMemo.Lines.Add(Line);
    SynMemo.CaretY := SynMemo.LineHeight * SynMemo.Lines.Count;
    SynMemo.Invalidate;
  finally
    SynMemo.Lines.EndUpdate;
  end;
end;

procedure TFormBootloader.CustomAddress(BlockList: TPhysicalAddressBlockList);
begin
  McuLoader.CustomAddress(McuInfo, BlockList);
end;

procedure TFormBootloader.UpdateUI;
begin
  if ConnectionEstablished then
    ShapeLink.Brush.Color := clGreen
  else
    ShapeLink.Brush.Color := clBtnShadow;
  ButtonBootloaderConnect.Enabled := ConnectionEstablished and FileExistsUTF8(KFileNameEdit.Text);
  ButtonBootloaderWrite.Enabled := Bootloading;
end;

end.

