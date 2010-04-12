object FrmLinearComb: TFrmLinearComb
  Left = 328
  Top = 159
  ActiveControl = SgrdCoefficients
  BorderStyle = bsDialog
  Caption = 'Linear Combinations'
  ClientHeight = 209
  ClientWidth = 273
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
  object SgrdCoefficients: TOdStringGrid
    Left = 0
    Top = 0
    Width = 273
    Height = 175
    Align = alClient
    Color = clWhite
    ColCount = 2
    DefaultRowHeight = 18
    FixedColor = 14540253
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 0
    SelectedCellColor = 8404992
    SelectedFontColor = clWhite
    FitColToWidth = -1
    goPasteNonEditable = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 175
    Width = 273
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 184
      Top = 0
      Width = 89
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      Caption = 'Panel2'
      TabOrder = 0
      object BtnCancel: TButton
        Left = 8
        Top = 5
        Width = 81
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 0
      end
    end
    object BtnOK: TButton
      Left = 0
      Top = 5
      Width = 81
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
end
