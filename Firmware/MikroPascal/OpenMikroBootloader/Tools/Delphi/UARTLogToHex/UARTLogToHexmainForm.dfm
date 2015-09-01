object frmUARTLogToHex: TfrmUARTLogToHex
  Left = 0
  Top = 0
  Caption = 'UART Log To Hex'
  ClientHeight = 604
  ClientWidth = 960
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 961
    Height = 596
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Variant 1'
      ExplicitWidth = 281
      ExplicitHeight = 165
      object lblExampleCode: TLabel
        Left = 440
        Top = 16
        Width = 248
        Height = 13
        Caption = 'mikroPascal PIC32 Code used to generate UART log'
      end
      object lblFormatInfo: TLabel
        Left = 8
        Top = 552
        Width = 451
        Height = 13
        Caption = 
          'Decodes to PIC32Hex Format from http://www.mikroe.com/forum/down' +
          'load/file.php?id=9145'
      end
      object memPIC32UARTCode: TMemo
        Left = 440
        Top = 35
        Width = 497
        Height = 262
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'var'
          '  AText: string[10];'
          ''
          
            'procedure Do_FLASH_Write(Address: dword; var Data_: array[FLASH_' +
            'WRITE_BLOCK] of byte);'
          'type TDWordPtr = ^dword;'
          'begin'
          '  LongWordToHex(Address, AText);'
          '  UART1_Write_Text('#39'FshWrWord addr = '#39' + AText + #13 + #10);'
          '  '
          '  LongWordToHex(TDWordPtr(@Data_)^, AText);'
          '  UART1_Write_Text('#39'FshWrWord data = '#39' + AText + #13 + #10);'
          '  '
          '  UART1_Write_Text('#39'...'#39' + #13 + #10);'
          '  '
          '  Delay_ms(1000);'
          '  Flash_Write_Word(Address, TDWordPtr(@Data_)^);'
          'end;')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object lstUARTLogVariant1: TListBox
        Left = 8
        Top = 8
        Width = 409
        Height = 289
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 16
        ParentFont = False
        TabOrder = 1
      end
      object lstDecodedVariant1: TListBox
        Left = 8
        Top = 303
        Width = 609
        Height = 242
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 16
        ParentFont = False
        TabOrder = 2
      end
      object btnLoadLogVariant1: TButton
        Left = 672
        Top = 320
        Width = 129
        Height = 25
        Caption = 'Load Log Variant1'
        TabOrder = 3
        OnClick = btnLoadLogVariant1Click
      end
      object btnDecodeVariant1: TButton
        Left = 672
        Top = 351
        Width = 129
        Height = 25
        Hint = 'Calls btnLoadLogVariant1Click if not loaded.'
        Caption = 'Decode'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = btnDecodeVariant1Click
      end
      object btnSaveDecodedToFileVariant1: TButton
        Left = 672
        Top = 400
        Width = 129
        Height = 25
        Caption = 'Save Decoded To File'
        TabOrder = 5
        OnClick = btnSaveDecodedToFileVariant1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Variant 2'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object lblExplanationVariant2: TLabel
        Left = 16
        Top = 32
        Width = 292
        Height = 13
        Caption = 'This variant takes a different UART log to transform it to hex'
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Left = 816
    Top = 336
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Left = 816
    Top = 424
  end
end
