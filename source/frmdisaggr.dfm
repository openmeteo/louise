object FrmDissagregate: TFrmDissagregate
  BorderStyle = bsDialog
  Caption = 'Disaggregation options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = LFormShow
  Top = 292
  Left = 442
  ClientWidth = 309
  ClientHeight = 198
  PixelsPerInch = 96
  TextHeight = 13
  object rgrpVariableType: TRadioGroup
    Left = 8
    Top = 8
    Width = 297
    Height = 81
    Caption = 'Variable type'
    ItemIndex = 0
    Items.Strings = (
      'Cumulative: constant disaggregated values'
      'Cumulative: random disaggregated values'
      'Average: constant disaggregated values'
      'Average: random disaggregated values')
    TabOrder = 0
    OnClick = LFormShow
  end
  object btnOK: TButton
    Left = 8
    Top = 168
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 224
    Top = 168
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object rgrpRandomModel: TRadioGroup
    Left = 8
    Top = 96
    Width = 297
    Height = 65
    Caption = 'Random model'
    Columns = 2
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Uniform'
      'Exponential'
      'Logarithmic'
      'Quadric'
      'HighOrder')
    TabOrder = 3
  end
end
