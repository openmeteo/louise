object FrmAggregation: TFrmAggregation
  Left = 356
  Top = 189
  BorderStyle = bsDialog
  Caption = 'Aggregation parameters'
  ClientHeight = 174
  ClientWidth = 454
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
  object LblTimeOffset: TLabel
    Left = 144
    Top = 11
    Width = 58
    Height = 13
    Caption = 'Time offset:'
  end
  object Label1: TLabel
    Left = 144
    Top = 35
    Width = 173
    Height = 13
    Caption = 'Number of missing values to accept:'
  end
  object Label2: TLabel
    Left = 144
    Top = 59
    Width = 176
    Height = 26
    Caption = 'For records derived from incomplete source, raise flag:'
    WordWrap = True
  end
  object lblFrom: TLabel
    Left = 352
    Top = 90
    Width = 26
    Height = 13
    Caption = 'from:'
  end
  object lblTo: TLabel
    Left = 354
    Top = 115
    Width = 14
    Height = 13
    Caption = 'to:'
  end
  object RgrpMethod: TRadioGroup
    Left = 8
    Top = 8
    Width = 129
    Height = 129
    Caption = 'Aggregation method'
    ItemIndex = 0
    Items.Strings = (
      'Sum'
      'Average'
      'Maximum'
      'Minimum'
      'Vector'
      'First Instant.')
    TabOrder = 0
    TabStop = True
  end
  object EdtTimeOffset: TEdit
    Left = 408
    Top = 11
    Width = 41
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object BtnOk: TButton
    Left = 8
    Top = 144
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object BtnCancel: TButton
    Left = 368
    Top = 144
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object ChkHydrologicalYear: TCheckBox
    Left = 144
    Top = 91
    Width = 153
    Height = 17
    Caption = 'Use hydrological year'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = ChkHydrologicalYearClick
  end
  object EdtMissingAllowed: TEdit
    Left = 408
    Top = 35
    Width = 41
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object CmbMissingFlag: TComboBox
    Left = 326
    Top = 59
    Width = 123
    Height = 21
    ItemHeight = 13
    TabOrder = 3
  end
  object chkSeasonal: TCheckBox
    Left = 143
    Top = 110
    Width = 162
    Height = 17
    Caption = 'Seasonal aggregation'
    TabOrder = 7
    OnClick = ChkHydrologicalYearClick
  end
  object cmbFrom: TComboBox
    Left = 378
    Top = 90
    Width = 70
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 8
  end
  object cmbTo: TComboBox
    Left = 378
    Top = 115
    Width = 70
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 9
  end
end
