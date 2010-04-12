object FrmRangeCheck: TFrmRangeCheck
  Left = 367
  Top = 302
  BorderStyle = bsDialog
  Caption = 'Range check'
  ClientHeight = 133
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = IFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblLowLimit: TLabel
    Left = 8
    Top = 8
    Width = 44
    Height = 13
    Caption = 'Low limit:'
  end
  object lblHighLimit: TLabel
    Left = 8
    Top = 32
    Width = 46
    Height = 13
    Caption = 'High limit:'
  end
  object lblMarkOut: TLabel
    Left = 8
    Top = 56
    Width = 168
    Height = 13
    Caption = 'Mark out of range values with flag:'
  end
  object lblProbabilityLevel: TLabel
    Left = 10
    Top = 80
    Width = 105
    Height = 13
    Caption = 'Confidence level (%):'
  end
  object EdtLowLimit: TEdit
    Left = 288
    Top = 8
    Width = 57
    Height = 21
    TabOrder = 0
  end
  object EdtHighLimit: TEdit
    Left = 288
    Top = 32
    Width = 57
    Height = 21
    TabOrder = 1
  end
  object BtnOk: TButton
    Left = 8
    Top = 104
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object BtnCancel: TButton
    Left = 304
    Top = 104
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object CmbRangeFlag: TComboBox
    Left = 288
    Top = 56
    Width = 97
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = 'CmbRangeFlag'
  end
  object chkAutoLow: TCheckBox
    Left = 183
    Top = 8
    Width = 98
    Height = 17
    Caption = 'Auto'
    TabOrder = 5
    OnClick = chkAutoLowClick
  end
  object chkAutoHigh: TCheckBox
    Left = 183
    Top = 32
    Width = 90
    Height = 17
    Caption = 'Auto'
    TabOrder = 6
    OnClick = chkAutoLowClick
  end
  object edtProbabilityLevel: TEdit
    Left = 192
    Top = 80
    Width = 57
    Height = 21
    TabOrder = 7
    Text = '99'
  end
end
