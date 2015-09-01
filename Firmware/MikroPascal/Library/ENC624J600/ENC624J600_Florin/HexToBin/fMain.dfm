object Form1: TForm1
  Left = 219
  Top = 117
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'HexToBinPIC24FJ'
  ClientHeight = 150
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button3: TButton
    Left = 104
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Convert'
    TabOrder = 0
    OnClick = Button3Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 56
    Width = 177
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 81
    Height = 25
    Caption = 'File Search'
    TabOrder = 2
    OnClick = Button1Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 127
    Width = 263
    Height = 23
    Panels = <>
    SimplePanel = True
  end
  object CheckBox1: TCheckBox
    Left = 200
    Top = 8
    Width = 49
    Height = 17
    Caption = '32Gx'
    TabOrder = 4
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 200
    Top = 32
    Width = 49
    Height = 17
    Caption = '64Gx'
    TabOrder = 5
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 200
    Top = 56
    Width = 49
    Height = 17
    Caption = '96Gx'
    TabOrder = 6
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 200
    Top = 80
    Width = 57
    Height = 17
    Caption = '128Gx'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = CheckBox4Click
  end
  object CheckBox5: TCheckBox
    Left = 200
    Top = 104
    Width = 57
    Height = 17
    Caption = '256Gx'
    TabOrder = 8
    OnClick = CheckBox5Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 104
    Width = 177
    Height = 16
    Min = 0
    Max = 100
    TabOrder = 9
  end
  object OpenDialog1: TOpenDialog
    Left = 80
    Top = 24
  end
end
