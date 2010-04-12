object FrmTimeseriesProperties: TFrmTimeseriesProperties
  Left = 233
  Top = 136
  BorderStyle = bsDialog
  Caption = 'Time series properties'
  ClientHeight = 181
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 50
    Height = 13
    Caption = 'Time step:'
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 67
    Height = 13
    Caption = 'Variable type:'
  end
  object LblDateOffset: TLabel
    Left = 8
    Top = 72
    Width = 86
    Height = 13
    Caption = 'Date offset (min):'
  end
  object Label3: TLabel
    Left = 272
    Top = 48
    Width = 84
    Height = 13
    Caption = 'Display comment:'
  end
  object Label4: TLabel
    Left = 272
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Display title:'
  end
  object Label5: TLabel
    Left = 8
    Top = 120
    Width = 23
    Height = 13
    Caption = 'Unit:'
  end
  object Label6: TLabel
    Left = 273
    Top = 125
    Width = 46
    Height = 13
    Caption = 'Precision:'
  end
  object ChkHydrologicalYear: TCheckBox
    Left = 104
    Top = 48
    Width = 161
    Height = 17
    Caption = 'Uses hydrological year'
    TabOrder = 2
  end
  object CmbTimeStep: TComboBox
    Left = 104
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = CmbTimeStepChange
    Items.Strings = (
      'Five-minute'
      'Ten-minute'
      'Hourly'
      'Daily'
      'Monthly'
      'Annual'
      'Variable'
      'Unspecified'
      'Two hours'
      'Two months')
  end
  object ChkStrictTimeStep: TCheckBox
    Left = 104
    Top = 32
    Width = 145
    Height = 17
    Caption = 'Time step is strict'
    TabOrder = 1
  end
  object CmbVariableType: TComboBox
    Left = 136
    Top = 96
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'Unknown'
      'Instantaneous'
      'Cumulative'
      'Average'
      'Maximum'
      'Minimum'
      'Stdev')
  end
  object EdtDateOffset: TEdit
    Left = 136
    Top = 72
    Width = 89
    Height = 21
    TabOrder = 3
    Text = 'EdtDateOffset'
  end
  object BtnOk: TButton
    Left = 8
    Top = 152
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
    OnClick = BtnOkClick
  end
  object BtnCancel: TButton
    Left = 440
    Top = 152
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object MemComment: TMemo
    Left = 368
    Top = 48
    Width = 153
    Height = 69
    Lines.Strings = (
      'MemComment')
    ScrollBars = ssHorizontal
    TabOrder = 7
    WordWrap = False
  end
  object EdtTitle: TEdit
    Left = 368
    Top = 8
    Width = 153
    Height = 21
    TabOrder = 6
    Text = 'EdtTitle'
  end
  object EdtMUnit: TEdit
    Left = 136
    Top = 120
    Width = 81
    Height = 21
    TabOrder = 5
    Text = 'EdtMUnit'
  end
  object EdtPrecision: TEdit
    Left = 368
    Top = 120
    Width = 121
    Height = 21
    TabOrder = 10
    Text = 'EdtPrecision'
  end
end
