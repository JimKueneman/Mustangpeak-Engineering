object Form1: TForm1
  Left = 373
  Height = 374
  Top = 208
  Width = 507
  Caption = 'Open mikroBootloader'
  ClientHeight = 374
  ClientWidth = 507
  Menu = MainMenu
  OnShow = FormShow
  object PageControl: TPageControl
    Left = 0
    Height = 374
    Top = 0
    Width = 507
    ActivePage = TabSheetBoot
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object TabSheetBoot: TTabSheet
      Caption = 'BootLoader'
      object TabSheetComPort2: TTabSheet
        Caption = 'ComPort'
        object Button3: TButton
          Left = 253
          Height = 22
          Top = 28
          Width = 187
          Caption = 'ReScan ComPorts'
          TabOrder = 0
        end
      end
      object ButtonBootloaderConnect: TButton
        Left = 13
        Height = 53
        Top = 196
        Width = 474
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Connect to Bootloader'
        TabOrder = 1
      end
      object ButtonLoadHex: TButton
        Left = 13
        Height = 53
        Top = 12
        Width = 232
        Caption = 'Load Hex...'
        OnClick = ButtonLoadHexClick
        TabOrder = 2
      end
      object EditHex: TEdit
        Left = 13
        Height = 22
        Top = 79
        Width = 471
        Anchors = [akTop, akLeft, akRight]
        OnExit = EditHexExit
        OnKeyDown = EditHexKeyDown
        TabOrder = 3
      end
      object ButtonReloadHex: TButton
        Left = 252
        Height = 53
        Top = 12
        Width = 232
        Caption = 'Reload Hex'
        OnClick = ButtonReloadHexClick
        TabOrder = 4
      end
      object ButtonBootloaderWrite: TButton
        Left = 13
        Height = 53
        Top = 260
        Width = 474
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Write Hex'
        TabOrder = 5
      end
      object RadioGroupBootloader: TRadioGroup
        Left = 13
        Height = 48
        Top = 140
        Width = 474
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Bootloader Connection'
        Columns = 3
        Items.Strings = (
          'ComPort'
          'Ethernet'
          'USB'
        )
        TabOrder = 6
        TabStop = True
      end
      object LabelHexStatus: TLabel
        Left = 13
        Height = 16
        Top = 108
        Width = 474
        Anchors = [akTop, akLeft, akRight]
        AutoSize = False
        Caption = 'Select HEX File'
        ParentColor = False
      end
    end
    object TabSheetComPort: TTabSheet
      Caption = 'ComPort'
      OnHide = TabSheetComPortHide
      OnShow = TabSheetComPortShow
      object Button1: TButton
        Left = 277
        Height = 22
        Top = 30
        Width = 144
        Caption = 'ReScan ComPorts'
        TabOrder = 0
      end
      object TabSheetComPort1: TTabSheet
        Caption = 'ComPort'
        object Button2: TButton
          Left = 253
          Height = 22
          Top = 28
          Width = 187
          Caption = 'ReScan ComPorts'
          TabOrder = 0
        end
      end
      object LabelComPort: TLabel
        Left = 11
        Height = 16
        Top = 16
        Width = 60
        Caption = 'COM Port'
        ParentColor = False
      end
      object ComboBoxComPort: TComboBox
        Left = 22
        Height = 20
        Top = 32
        Width = 226
        ItemHeight = 0
        Style = csDropDownList
        TabOrder = 2
      end
      object LabelBaud: TLabel
        Left = 11
        Height = 16
        Top = 64
        Width = 63
        Caption = 'Baud Rate'
        ParentColor = False
      end
      object ComboBoxBaud: TComboBox
        Left = 22
        Height = 21
        Top = 80
        Width = 132
        ItemHeight = 0
        ItemIndex = 0
        Items.Strings = (
          '19200'
          '38400'
          '57600'
          '115200'
          '230400'
          '333333'
          '500000'
        )
        TabOrder = 3
        Text = '19200'
      end
      object LabelDataBits: TLabel
        Left = 11
        Height = 16
        Top = 112
        Width = 56
        Caption = 'Data Bits'
        ParentColor = False
      end
      object ComboBoxDataBits: TComboBox
        Left = 22
        Height = 20
        Top = 128
        Width = 132
        ItemHeight = 0
        ItemIndex = 3
        Items.Strings = (
          '5'
          '6'
          '7'
          '8'
          '9'
        )
        Style = csDropDownList
        TabOrder = 4
        Text = '8'
      end
      object LabelStopBits: TLabel
        Left = 11
        Height = 16
        Top = 160
        Width = 55
        Caption = 'Stop Bits'
        ParentColor = False
      end
      object ComboBoxStopBits: TComboBox
        Left = 22
        Height = 20
        Top = 176
        Width = 132
        ItemHeight = 0
        ItemIndex = 0
        Items.Strings = (
          '0'
          '1'
          '2'
        )
        Style = csDropDownList
        TabOrder = 5
        Text = '0'
      end
      object LabelParity: TLabel
        Left = 11
        Height = 16
        Top = 208
        Width = 35
        Caption = 'Parity'
        ParentColor = False
      end
      object ComboBoxParity: TComboBox
        Left = 22
        Height = 20
        Top = 228
        Width = 132
        ItemHeight = 0
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Even'
          'Odd'
          'Mark'
          'Space'
        )
        Style = csDropDownList
        TabOrder = 6
        Text = 'None'
      end
      object LabelFlowControl: TLabel
        Left = 11
        Height = 16
        Top = 256
        Width = 80
        Caption = 'Flow Control'
        ParentColor = False
      end
      object ComboBoxFlowControl: TComboBox
        Left = 22
        Height = 20
        Top = 276
        Width = 132
        ItemHeight = 0
        ItemIndex = 0
        Items.Strings = (
          'None'
          'RTS/CTS'
          'DTR/DSR'
          'XON/XOFF'
        )
        Style = csDropDownList
        TabOrder = 7
        Text = 'None'
      end
    end
    object TabSheetEthernet: TTabSheet
      Caption = 'Ethernet'
      OnHide = TabSheetEthernetHide
      OnShow = TabSheetEthernetShow
      object Label1: TLabel
        Left = 133
        Height = 43
        Top = 108
        Width = 277
        Caption = 'Unimplemented'
        Font.Color = clRed
        Font.Height = -36
        Font.Pitch = fpFixed
        ParentColor = False
        ParentFont = False
      end
      object SpinEditEtherneRemotePort: TSpinEdit
        Left = 37
        Height = 16
        Top = 236
        Width = 122
        MaxValue = 65535
        ReadOnly = True
        TabOrder = 0
      end
      object Label7: TLabel
        Left = 16
        Height = 16
        Top = 212
        Width = 118
        Caption = 'Remote Client Port'
        ParentColor = False
      end
      object EditEthernetRemoteIP: TEdit
        Left = 37
        Height = 22
        Top = 172
        Width = 256
        TabOrder = 1
      end
      object Label8: TLabel
        Left = 16
        Height = 16
        Top = 148
        Width = 315
        Caption = 'Remote IP Address (0.0.0.0  is default machine IP)'
        ParentColor = False
      end
      object SpinEditEthernetLocalPort: TSpinEdit
        Left = 37
        Height = 16
        Top = 95
        Width = 122
        MaxValue = 65535
        ReadOnly = True
        TabOrder = 2
      end
      object Label6: TLabel
        Left = 16
        Height = 16
        Top = 72
        Width = 267
        Caption = 'Local Port (JMRI uses 12021 for a Listener)'
        ParentColor = False
      end
      object EditEthernetLocalIP: TEdit
        Left = 37
        Height = 22
        Top = 40
        Width = 256
        TabOrder = 3
      end
      object Label5: TLabel
        Left = 16
        Height = 16
        Top = 16
        Width = 300
        Caption = 'Local IP Address (0.0.0.0  is default machine IP)'
        ParentColor = False
      end
      object ButtonLocalHost: TButton
        Left = 317
        Height = 22
        Top = 40
        Width = 104
        Caption = 'Local Host'
        TabOrder = 4
      end
      object ButtonRemoteLocalHost: TButton
        Left = 317
        Height = 22
        Top = 172
        Width = 104
        Caption = 'Local Host'
        TabOrder = 5
      end
    end
    object TabSheetUSB: TTabSheet
      Caption = 'USB'
      OnHide = TabSheetUSBHide
      OnShow = TabSheetUSBShow
      object Label3: TLabel
        Left = 125
        Height = 43
        Top = 76
        Width = 277
        Caption = 'Unimplemented'
        Font.Color = clRed
        Font.Height = -36
        Font.Pitch = fpFixed
        ParentColor = False
        ParentFont = False
      end
    end
    object TabSheetAbout: TTabSheet
      Caption = 'About'
      object LabelNodeExplorer: TLabel
        Left = 0
        Height = 28
        Top = 0
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'Open mikroBootloader'
        Font.Height = -24
        Font.Name = 'Lucida Grande'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object LabelMyName: TLabel
        Left = 0
        Height = 17
        Top = 28
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'written by Jim Kueneman'
        Font.Height = -14
        ParentColor = False
        ParentFont = False
      end
      object LabelBuild1: TLabel
        Left = 0
        Height = 17
        Top = 55
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'Build: '
        Font.Height = -14
        ParentColor = False
        ParentFont = False
      end
      object LabelBuildDate: TLabel
        Left = 0
        Height = 17
        Top = 72
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'LabelBuildDate'
        Font.Height = -14
        ParentColor = False
        ParentFont = False
      end
      object LabelCPU: TLabel
        Left = 0
        Height = 15
        Top = 99
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'Target CPU: '
        Font.Height = -12
        ParentColor = False
        ParentFont = False
      end
      object LabelTargetCPU: TLabel
        Left = 0
        Height = 15
        Top = 114
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'LabelTargetCPU'
        Font.Height = -12
        ParentColor = False
        ParentFont = False
      end
      object LabelTargetOperatingSystem: TLabel
        Left = 0
        Height = 15
        Top = 139
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'Target OS: '
        Font.Height = -12
        ParentColor = False
        ParentFont = False
      end
      object LabelTargetOS: TLabel
        Left = 0
        Height = 15
        Top = 154
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'LabelTargetOS'
        Font.Height = -12
        ParentColor = False
        ParentFont = False
      end
      object LabelWrittenIn: TLabel
        Left = 0
        Height = 15
        Top = 189
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'Written in Free Pascal and Lazarus'
        Font.Height = -12
        ParentColor = False
        ParentFont = False
      end
      object LabelURLFreePascal: TLabel
        Cursor = crHandPoint
        Left = 0
        Height = 15
        Top = 204
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'www.freepascal.org'
        Font.Color = clBlue
        Font.Height = -12
        ParentColor = False
        ParentFont = False
      end
      object LabelURLLazarus: TLabel
        Cursor = crHandPoint
        Left = 0
        Height = 15
        Top = 219
        Width = 503
        Align = alTop
        Alignment = taCenter
        Caption = 'www.lazarus.freepascal.org'
        Font.Color = clBlue
        Font.Height = -12
        ParentColor = False
        ParentFont = False
      end
    end
  end
  object MainMenu: TMainMenu
    left = 184
    top = 104
  end
  object ActionList: TActionList
    left = 184
    top = 240
    object ActionHelpAboutShow: TAction
      Category = 'Help'
      Caption = 'Help'
    end
    object ActionToolsPreferenceShowMac: TAction
      Category = 'Tools'
      Caption = 'Preferences...'
    end
    object ActionToolsSettingsShowWin: TAction
      Category = 'Tools'
      Caption = 'Settings'
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.hex'
    Filter = 'Intel Hex File|*.hex'
    left = 208
    top = 176
  end
end
