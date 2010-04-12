object FrmDeviationChart: TFrmDeviationChart
  Left = 0
  Top = 0
  Caption = 'Deviation chart'
  ClientHeight = 404
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  DesignSize = (
    587
    404)
  PixelsPerInch = 96
  TextHeight = 13
  object Chart: TChart
    Left = 127
    Top = 8
    Width = 452
    Height = 388
    Legend.Visible = False
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.LabelsOnAxis = False
    BottomAxis.LabelStyle = talValue
    BottomAxis.TickOnLabelsOnly = False
    BottomAxis.Title.Caption = 'Independent variable'
    LeftAxis.Title.Caption = 'Dependent variable'
    RightAxis.Visible = False
    TopAxis.Visible = False
    View3D = False
    Zoom.Pen.Color = clMedGray
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    Anchors = [akLeft, akTop, akRight, akBottom]
    object seriesLine: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = False
      SeriesColor = clBlack
      Title = 'Line'
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loNone
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object seriesPoints: TPointSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = False
      SeriesColor = clTeal
      Title = 'Points'
      ClickableLine = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = True
      XValues.Name = 'X'
      XValues.Order = loNone
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object rgrpMarks: TRadioGroup
    Left = 8
    Top = 8
    Width = 113
    Height = 197
    Anchors = [akLeft, akBottom]
    Caption = 'Point marks'
    ItemIndex = 0
    Items.Strings = (
      'Square'
      'Circle'
      'Triangle'
      'Down Triangle'
      'Cross'
      'Diaglonal Cross'
      'Star')
    TabOrder = 1
    OnClick = lstVariablesClick
  end
  object chkShowMarks: TCheckBox
    Left = 8
    Top = 227
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Show marks'
    TabOrder = 2
    OnClick = lstVariablesClick
  end
  object MainMenu1: TMainMenu
    Left = 216
    Top = 344
    object mnuFile: TMenuItem
      Caption = 'File'
      object mnuPrintChart: TMenuItem
        Caption = 'Print Chart'
        ShortCut = 16464
        OnClick = mnuPrintChartClick
      end
      object mnuPrintSetup: TMenuItem
        Caption = 'Print Setup'
        OnClick = mnuPrintSetupClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuExit: TMenuItem
        Caption = 'Exit'
        OnClick = mnuExitClick
      end
    end
    object mnuEdit: TMenuItem
      Caption = 'Edit'
      object mnuCopyChart: TMenuItem
        Caption = 'Copy chart'
        ShortCut = 16451
        OnClick = mnuCopyChartClick
      end
    end
  end
  object PrintDialog: TPrintDialog
    Left = 440
    Top = 8
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 520
    Top = 8
  end
end
