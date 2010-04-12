object FrmRegression: TFrmRegression
  Left = 268
  Top = 207
  BorderStyle = bsDialog
  Caption = 'Time series regression'
  ClientHeight = 258
  ClientWidth = 334
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
  object Label1: TLabel
    Left = 7
    Top = 3
    Width = 117
    Height = 13
    Caption = 'Order of auto-correlation:'
  end
  object EdtLag: TEdit
    Left = 137
    Top = 2
    Width = 41
    Height = 21
    TabOrder = 0
    Text = '0'
  end
  object ChkCrossesZero: TCheckBox
    Left = 8
    Top = 27
    Width = 153
    Height = 17
    Caption = 'Zero constant term'
    TabOrder = 1
  end
  object ChkOrganic: TCheckBox
    Left = 8
    Top = 43
    Width = 153
    Height = 17
    Caption = 'Organic'
    TabOrder = 2
    OnClick = ChkOrganicClick
  end
  object ChkSeasonal: TCheckBox
    Left = 8
    Top = 59
    Width = 153
    Height = 17
    Caption = 'Seasonal'
    TabOrder = 3
  end
  object BtnOK: TButton
    Left = 8
    Top = 226
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object BtnCancel: TButton
    Left = 253
    Top = 226
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object chkDoFilling: TCheckBox
    Left = 8
    Top = 125
    Width = 158
    Height = 17
    Caption = 'Do filling on null records'
    TabOrder = 6
    OnClick = ChkOrganicClick
  end
  object chkRandomTerm: TCheckBox
    Left = 8
    Top = 141
    Width = 97
    Height = 17
    Caption = 'Random term'
    TabOrder = 7
    OnClick = ChkOrganicClick
  end
  object chkTruncToZero: TCheckBox
    Left = 8
    Top = 157
    Width = 273
    Height = 17
    Caption = 'Truncate negative values to zero'
    TabOrder = 8
  end
  object chkRandomSeed: TCheckBox
    Left = 176
    Top = 141
    Width = 207
    Height = 17
    Caption = 'Random seed'
    TabOrder = 9
  end
  object chkDoExtendBefore: TCheckBox
    Left = 8
    Top = 173
    Width = 161
    Height = 17
    Caption = 'Extend values to the past'
    TabOrder = 10
  end
  object chkDoExtendAfter: TCheckBox
    Left = 176
    Top = 173
    Width = 153
    Height = 17
    Caption = 'Extend values to the future'
    TabOrder = 11
  end
  object chkDonotFillInnerValues: TCheckBox
    Left = 8
    Top = 189
    Width = 167
    Height = 17
    Caption = 'Do not fill inner missing values'
    TabOrder = 12
  end
  object chkMeanValue: TCheckBox
    Left = 8
    Top = 75
    Width = 257
    Height = 17
    Caption = 'Do not regress, just use mean values'
    TabOrder = 13
    OnClick = ChkOrganicClick
  end
  object chkOptimize: TCheckBox
    Left = 8
    Top = 91
    Width = 241
    Height = 17
    Caption = 'Optimize for multi regression'
    TabOrder = 14
  end
end
