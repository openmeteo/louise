object FrmRegularizeStep: TFrmRegularizeStep
  Left = 229
  Top = 111
  BorderStyle = bsDialog
  Caption = 'Regularize Step'
  ClientHeight = 362
  ClientWidth = 534
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 133
    Height = 13
    Caption = 'Offset (nominal) in minutes:'
  end
  object Label1: TLabel
    Left = 8
    Top = 176
    Width = 320
    Height = 13
    Caption = 
      'In new records with dates which did not previously exist raise f' +
      'lag:'
  end
  object EdtTimeOffset: TEdit
    Left = 184
    Top = 8
    Width = 49
    Height = 21
    TabOrder = 0
    Text = '0'
  end
  object BtnOk: TButton
    Left = 5
    Top = 330
    Width = 105
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object BtnCancel: TButton
    Left = 429
    Top = 330
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object RgrpMethod: TRadioGroup
    Left = 8
    Top = 32
    Width = 518
    Height = 138
    Caption = 'Source variable type'
    ItemIndex = 0
    Items.Strings = (
      'Normal instantaneous'
      'Vector instantaneous'
      
        'Cumulative: each measurement is for a period equal to the time s' +
        'tep (constant intervals)'
      
        'Cumulative: each measurement is for the period beginning on prev' +
        'ious measurement (variable intervals)'
      
        'Change time: move each record as it is, so that its position is ' +
        'appropriate')
    TabOrder = 1
    WordWrap = True
  end
  object CmbNewDateFlag: TComboBox
    Left = 400
    Top = 176
    Width = 126
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = 'CmbNewDateFlag'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 203
    Width = 518
    Height = 110
    Caption = 'Time step'
    TabOrder = 5
    object lblDefaultStep: TLabel
      Left = 359
      Top = 16
      Width = 16
      Height = 13
    end
    object rbDefaultTimestep: TRadioButton
      Left = 6
      Top = 15
      Width = 347
      Height = 17
      Caption = 'Use default time step specified by time series properties:'
      TabOrder = 0
      OnClick = rbDefaultTimestepClick
    end
    object rbTimestepFromList: TRadioButton
      Left = 6
      Top = 43
      Width = 155
      Height = 17
      Caption = 'Select a time step from list:'
      TabOrder = 1
      OnClick = rbDefaultTimestepClick
    end
    object rbOtherTimeStep: TRadioButton
      Left = 6
      Top = 71
      Width = 251
      Height = 17
      Caption = 'Other (custom) time step. Specify minutes:'
      TabOrder = 2
      OnClick = rbDefaultTimestepClick
    end
    object cmbTimeSteps: TComboBox
      Left = 264
      Top = 43
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 1
      TabOrder = 3
      Text = 'Ten (10) minute'
      Items.Strings = (
        'Five (5) minute'
        'Ten (10) minute'
        'Fithteen (15) minute'
        'Twenty (20) minute'
        'Thirty (30) minute'
        'Hourly'
        'Two (2) hour'
        'Three (3) hour'
        'Four (4) hour'
        'Six (6) hour'
        'Eight (8) hour'
        'Twelve (12) hour'
        'Daily')
    end
    object spinCustomStep: TSpinEdit
      Left = 320
      Top = 71
      Width = 121
      Height = 22
      MaxValue = 100000
      MinValue = 1
      TabOrder = 4
      Value = 2
    end
  end
end
