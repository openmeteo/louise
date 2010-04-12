object FrmIDFEval: TFrmIDFEval
  Left = 388
  Top = 213
  BorderStyle = bsDialog
  Caption = 'Evaluate IDF'
  ClientHeight = 260
  ClientWidth = 302
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = IFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 5
    Width = 46
    Height = 13
    Caption = 'Multiplier:'
  end
  object Label2: TLabel
    Left = 10
    Top = 172
    Width = 145
    Height = 13
    Caption = 'Raise flag when acc. missings:'
  end
  object Label3: TLabel
    Left = 10
    Top = 202
    Width = 145
    Height = 13
    Caption = 'Raise flag for marginal values:'
  end
  object btnOK: TButton
    Left = 5
    Top = 229
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 225
    Top = 229
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cmbMarginalFlags: TComboBox
    Left = 180
    Top = 202
    Width = 121
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = 'cmbMarginalFlags'
  end
  object cmbMissingFlags: TComboBox
    Left = 180
    Top = 172
    Width = 121
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    Text = 'cmbMissingFlags'
  end
  object edtMultiplier: TEdit
    Left = 165
    Top = 5
    Width = 66
    Height = 21
    TabOrder = 4
    Text = 'edtMultiplier'
  end
  object chkHydrologicalYear: TCheckBox
    Left = 15
    Top = 95
    Width = 231
    Height = 17
    Caption = 'Hydrological Year'
    TabOrder = 5
  end
  object rgrpAnalysisTimeStep: TRadioGroup
    Left = 5
    Top = 35
    Width = 131
    Height = 56
    Caption = 'IDF timestep'
    ItemIndex = 0
    Items.Strings = (
      'Yearly'
      'Monthly')
    TabOrder = 6
    OnClick = rgrpAnalysisTimeStepClick
  end
  object chkAllowMissingValues: TCheckBox
    Left = 15
    Top = 112
    Width = 226
    Height = 17
    Caption = 'Accumulate Missing values'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object rgrpIDFVariable: TRadioGroup
    Left = 165
    Top = 35
    Width = 131
    Height = 56
    Caption = 'IDF variable'
    ItemIndex = 0
    Items.Strings = (
      'Height'
      'Intensity (mm/h)')
    TabOrder = 8
  end
  object chkCalculatetMissing: TCheckBox
    Left = 15
    Top = 129
    Width = 265
    Height = 17
    Caption = 'Calculate missing values percent, timeseries (%)'
    TabOrder = 9
  end
  object chkDayNumber: TCheckBox
    Left = 16
    Top = 146
    Width = 257
    Height = 17
    Caption = 'Extreme days time series output'
    TabOrder = 10
  end
end
