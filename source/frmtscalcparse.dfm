object FrmTimeseriesParser: TFrmTimeseriesParser
  Left = 320
  Top = 350
  Caption = 'Timeseries complex calculations'
  ClientHeight = 381
  ClientWidth = 507
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = LFormCreate
  OnDestroy = LFormDestroy
  OnShow = LFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblExpression: TLabel
    Left = 18
    Top = 249
    Width = 56
    Height = 26
    Caption = 'Write expression:'
    WordWrap = True
  end
  object lblVariables: TLabel
    Left = 16
    Top = 8
    Width = 47
    Height = 26
    Caption = 'Available Variables:'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 16
    Top = 127
    Width = 56
    Height = 65
    Caption = 'Available operators, constants and special variables:'
    WordWrap = True
  end
  object btnCalculate: TButton
    Left = 8
    Top = 349
    Width = 75
    Height = 25
    Caption = 'Calculate'
    TabOrder = 0
    OnClick = btnCalculateClick
  end
  object btnCancel: TButton
    Left = 424
    Top = 349
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object memoExpression: TMemo
    Left = 88
    Top = 246
    Width = 409
    Height = 97
    Lines.Strings = (
      'x1*2')
    TabOrder = 2
  end
  object listVariables: TListBox
    Left = 88
    Top = 8
    Width = 409
    Height = 113
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 88
    Top = 127
    Width = 409
    Height = 113
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      '     Operators:'
      '+   -   *   /   (   )   ^ '
      '     Functions:'
      
        'sqr(), sqrt(), sin(), cos(), tan(), arcsin(), arccos(), arctan()' +
        ', abs(), exp(), '
      'log(), log10(), random(), nrnd(), trunc(), int(), round()'
      '     Standard constants and variables:'
      'pi, index, year, month, day, dayinyear, hour, minute')
    ParentFont = False
    TabOrder = 4
  end
end
