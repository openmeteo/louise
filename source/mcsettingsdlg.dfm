object FrmConfIntSettings: TFrmConfIntSettings
  Left = 461
  Top = 221
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Confidence interval settings'
  ClientHeight = 162
  ClientWidth = 201
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object rgrpMCCount: TRadioGroup
    Left = 0
    Top = 0
    Width = 111
    Height = 71
    Caption = 'Simulation iterations'
    ItemIndex = 1
    Items.Strings = (
      '30 000'
      '60 000'
      '120 000')
    TabOrder = 2
  end
  object rgrpMCPointsCount: TRadioGroup
    Left = 115
    Top = 0
    Width = 86
    Height = 71
    Caption = 'Ploted points'
    ItemIndex = 1
    Items.Strings = (
      '5 points'
      '10 points'
      '20 points')
    TabOrder = 3
  end
  object rgrpMCConfidence: TRadioGroup
    Left = 0
    Top = 75
    Width = 111
    Height = 86
    Caption = 'Confidence interval'
    ItemIndex = 1
    Items.Strings = (
      '90 %'
      '95 %'
      '99 %'
      '99.9 %')
    TabOrder = 4
  end
  object btnOK: TButton
    Left = 125
    Top = 105
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 125
    Top = 135
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
