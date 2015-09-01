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
  StdCtrls, Menus, ActnList, Spin, ExtCtrls, file_utilities, common_settings,
  synaser, bootloader;

const
  BUNDLENAME             = 'Open mikroBootloader';
  PATH_LINUX_APP_FOLDER  = 'openmikrobootloader/';
  PATH_SETTINGS_FILE     = 'settings.ini';

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionToolsSettingsShowWin: TAction;
    ActionToolsPreferenceShowMac: TAction;
    ActionHelpAboutShow: TAction;
    ActionList: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ButtonBootloaderConnect: TButton;
    ButtonLoadHex: TButton;
    ButtonReloadHex: TButton;
    ButtonBootloaderWrite: TButton;
    ButtonLocalHost: TButton;
    ButtonRemoteLocalHost: TButton;
    ComboBoxBaud: TComboBox;
    ComboBoxComPort: TComboBox;
    ComboBoxDataBits: TComboBox;
    ComboBoxFlowControl: TComboBox;
    ComboBoxParity: TComboBox;
    ComboBoxStopBits: TComboBox;
    EditHex: TEdit;
    EditEthernetLocalIP: TEdit;
    EditEthernetRemoteIP: TEdit;
    Label1: TLabel;
    LabelHexStatus: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
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
    RadioGroupBootloader: TRadioGroup;
    SpinEditEtherneRemotePort: TSpinEdit;
    SpinEditEthernetLocalPort: TSpinEdit;
    TabSheetAbout: TTabSheet;
    TabSheetBoot: TTabSheet;
    TabSheetComPort: TTabSheet;
    TabSheetComPort1: TTabSheet;
    TabSheetComPort2: TTabSheet;
    TabSheetEthernet: TTabSheet;
    TabSheetUSB: TTabSheet;
    procedure ButtonLoadHexClick(Sender: TObject);
    procedure ButtonReloadHexClick(Sender: TObject);
    procedure EditHexExit(Sender: TObject);
    procedure EditHexKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormShow(Sender: TObject);
    procedure TabSheetComPortHide(Sender: TObject);
    procedure TabSheetComPortShow(Sender: TObject);
    procedure TabSheetEthernetHide(Sender: TObject);
    procedure TabSheetEthernetShow(Sender: TObject);
    procedure TabSheetUSBHide(Sender: TObject);
    procedure TabSheetUSBShow(Sender: TObject);
  private
    FAppAboutCmd: TMenuItem;
    FHexValid: Boolean;
    {$IFDEF DARWIN}
    FOSXMenu: TMenuItem;
    FOSXPrefCmd: TMenuItem;
    FOSXSep1Cmd: TMenuItem;
    {$ENDIF}
    FSettingsFilePath: string;
    FSettingsLocked: Boolean;
    FShownOnce: Boolean;
  protected
    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
    procedure LoadSettings(SettingType: TLoadSettingType);
    procedure StoreSettings(SettingType: TLoadSettingType);
    procedure ScanComPorts;
    procedure ValidateHexFile;
  public
    { public declarations }
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property SettingsFilePath: string read FSettingsFilePath write FSettingsFilePath;
    property SettingsLocked: Boolean read FSettingsLocked write FSettingsLocked;
    property HexValid: Boolean read FHexValid write FHexValid;

    procedure UpdateUI;
  end;

var
  Form1: TForm1;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TForm1 }

procedure TForm1.ButtonLoadHexClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    EditHex.Text := OpenDialog.FileName;
    ValidateHexFile
  end;
end;

procedure TForm1.ButtonReloadHexClick(Sender: TObject);
begin
  ValidateHexFile
end;

procedure TForm1.EditHexExit(Sender: TObject);
begin
  ValidateHexFile;
end;

procedure TForm1.EditHexKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ValidateHexFile
end;

procedure TForm1.FormShow(Sender: TObject);
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

    ShownOnce := True;
  end;
  UpdateUI
end;

procedure TForm1.LoadSettings(SettingType: TLoadSettingType);
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
            ComboBoxDataBits.ItemIndex := ComboBoxDataBits.Items.IndexOf(IntToStr( GlobalSettings.ComPort.DataBits));
            ComboBoxStopBits.ItemIndex := ComboBoxStopBits.Items.IndexOf(IntToStr( GlobalSettings.ComPort.StopBits));
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

procedure TForm1.ScanComPorts;
begin
  ComboBoxComPort.Items.Delimiter := ';';
  ComboBoxComPort.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  if ComboBoxComPort.Items.Count > 0 then
    ComboBoxComPort.ItemIndex := 0;
end;

procedure TForm1.StoreSettings(SettingType: TLoadSettingType);
begin
  if not SettingsLocked then
  begin
    case SettingType of
      lstCom:
        begin
          GlobalSettings.ComPort.Port := ComboBoxComPort.Text;
          GlobalSettings.ComPort.BaudRate := StrToInt( ComboBoxBaud.Text);
          GlobalSettings.ComPort.DataBits := StrToInt( ComboBoxDataBits.Text);
          GlobalSettings.ComPort.StopBits := StrToInt( ComboBoxStopBits.Text);
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

procedure TForm1.TabSheetComPortHide(Sender: TObject);
begin
  StoreSettings(lstCom)
end;

procedure TForm1.TabSheetComPortShow(Sender: TObject);
begin
  LoadSettings(lstCom)
end;

procedure TForm1.TabSheetEthernetHide(Sender: TObject);
begin
  StoreSettings(lstEthernet)
end;

procedure TForm1.TabSheetEthernetShow(Sender: TObject);
begin
  LoadSettings(lstEthernet)
end;

procedure TForm1.TabSheetUSBHide(Sender: TObject);
begin
  StoreSettings(lstUSB)
end;

procedure TForm1.TabSheetUSBShow(Sender: TObject);
begin
  LoadSettings(lstUSB)
end;

procedure TForm1.UpdateUI;
begin
  ButtonBootloaderConnect.Enabled := HexValid;
  ButtonBootloaderWrite.Enabled := HexValid;
  ButtonReloadHex.Enabled := HexValid;
end;

procedure TForm1.ValidateHexFile;
begin
  {$IFDEF FPC}
  if FileExistsUTF8(EditHex.Text) then
  {$ELSE}
  if FileExists(EditHex.Text) then
  {$ENDIF}
  begin
    HexValid := BootloaderParseAndValidate(EditHex.Text);
    if not HexValid then
    begin
      LabelHexStatus.Font.Color := clRed;
      LabelHexStatus.Caption := 'Invalid HEX file';
    end;
    LabelHexStatus.Font.Color := clGreen;
    LabelHexStatus.Caption := 'HEX File Validated';
  end else
  begin
    HexValid := False;
    LabelHexStatus.Font.Color := clRed;
    LabelHexStatus.Caption := 'Unable to open selected file';
  end;
  UpdateUI
end;

end.

