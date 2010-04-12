object FrmFilterDialog: TFrmFilterDialog
  Left = 590
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Time series data filters'
  ClientHeight = 124
  ClientWidth = 348
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 48
    Width = 9
    Height = 13
    Caption = 'or'
  end
  object LblFilterPrompt: TLabel
    Left = 8
    Top = 8
    Width = 144
    Height = 13
    Caption = 'Only display records such that:'
  end
  object RbtnValue: TRadioButton
    Left = 40
    Top = 24
    Width = 81
    Height = 17
    Caption = 'The value'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = SetControlStatus
  end
  object RbtnFlag: TRadioButton
    Left = 40
    Top = 64
    Width = 81
    Height = 17
    Caption = 'The flag'
    TabOrder = 3
    OnClick = SetControlStatus
  end
  object EdtValue: TEdit
    Left = 272
    Top = 24
    Width = 65
    Height = 21
    TabOrder = 2
    OnChange = SetControlStatus
  end
  object CboValueCondition: TComboBox
    Left = 136
    Top = 24
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = SetControlStatus
    Items.Strings = (
      'is greater than'
      'is less than'
      'is null'
      'is not null'
      'is unmodified'
      'is modified'
      'is new')
  end
  object CboFlag: TComboBox
    Left = 136
    Top = 64
    Width = 121
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    Sorted = True
    TabOrder = 4
  end
  object CboFlagCondition: TComboBox
    Left = 272
    Top = 64
    Width = 65
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 5
    Items.Strings = (
      'is on'
      'is off')
  end
  object BtnOK: TButton
    Left = 8
    Top = 96
    Width = 89
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 248
    Top = 96
    Width = 89
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
end
