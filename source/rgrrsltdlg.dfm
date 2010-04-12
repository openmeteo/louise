object FrmRegressionResults: TFrmRegressionResults
  Left = 271
  Top = 161
  Caption = 'Regression results'
  ClientHeight = 313
  ClientWidth = 480
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
  object SgrdResults: TStringGrid
    Left = 0
    Top = 0
    Width = 480
    Height = 281
    Align = alClient
    ColCount = 6
    DefaultRowHeight = 18
    RowCount = 13
    PopupMenu = PMnu
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 281
    Width = 480
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ChkHydrologicalYear: TCheckBox
      Left = 0
      Top = 8
      Width = 121
      Height = 17
      Caption = 'Hydrological year'
      TabOrder = 0
      OnClick = ChkHydrologicalYearClick
    end
    object btnShowDeviationChart: TButton
      Left = 320
      Top = 4
      Width = 147
      Height = 25
      Caption = 'Show deviation chart'
      TabOrder = 1
      OnClick = btnShowDeviationChartClick
    end
  end
  object PMnu: TPopupMenu
    Left = 272
    Top = 136
    object MnuCopy: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = MnuCopyClick
    end
  end
end
