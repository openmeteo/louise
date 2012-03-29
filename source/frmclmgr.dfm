object FrmClimacogram: TFrmClimacogram
  Left = 0
  Top = 0
  Caption = 'Climacogram of time series'
  ClientHeight = 465
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    641
    465)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 493
    Top = 8
    Width = 128
    Height = 52
    Anchors = [akTop, akRight]
    Caption = 
      'Select one or multiple time series to display by clicking, dragg' +
      'ing and / or using Ctrl and Shift keys:'
    WordWrap = True
    ExplicitLeft = 495
  end
  object Chart: TChart
    Left = 8
    Top = 8
    Width = 479
    Height = 449
    Legend.Alignment = laTop
    Legend.LegendStyle = lsSeries
    MarginTop = 5
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.Logarithmic = True
    BottomAxis.MinorGrid.Color = clSilver
    BottomAxis.MinorGrid.Visible = True
    BottomAxis.MinorTickCount = 8
    BottomAxis.Title.Caption = 'scale (days)'
    DepthAxis.Automatic = False
    DepthAxis.AutomaticMaximum = False
    DepthAxis.AutomaticMinimum = False
    DepthAxis.Maximum = 0.319999999999999800
    DepthAxis.Minimum = -0.680000000000000200
    DepthTopAxis.Automatic = False
    DepthTopAxis.AutomaticMaximum = False
    DepthTopAxis.AutomaticMinimum = False
    DepthTopAxis.Maximum = 0.319999999999999800
    DepthTopAxis.Minimum = -0.680000000000000200
    LeftAxis.Logarithmic = True
    LeftAxis.MinorGrid.Color = clSilver
    LeftAxis.MinorGrid.Visible = True
    LeftAxis.MinorTickCount = 8
    LeftAxis.PositionUnits = muPixels
    LeftAxis.Title.Caption = 'stdev'
    RightAxis.Automatic = False
    RightAxis.AutomaticMaximum = False
    RightAxis.AutomaticMinimum = False
    TopAxis.Automatic = False
    TopAxis.AutomaticMaximum = False
    TopAxis.AutomaticMinimum = False
    TopAxis.Grid.Color = 16744576
    TopAxis.Grid.Style = psDashDot
    TopAxis.LabelStyle = talText
    TopAxis.Logarithmic = True
    View3D = False
    View3DOptions.Orthogonal = False
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object lstSeries: TListBox
    Left = 493
    Top = 66
    Width = 146
    Height = 391
    Anchors = [akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
    OnClick = lstSeriesClick
  end
  object MainMenu: TMainMenu
    Left = 472
    Top = 64
    object File1: TMenuItem
      Caption = 'File'
      object mnuPrintChart: TMenuItem
        Caption = 'Print chart'
        ShortCut = 16464
        OnClick = mnuPrintChartClick
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object mnuCopyChart: TMenuItem
        Caption = 'Copy chart'
        ShortCut = 16451
        OnClick = mnuCopyChartClick
      end
      object mnuCopyTabularData: TMenuItem
        Caption = 'Copy tabular data'
        ShortCut = 24643
      end
    end
    object Methods1: TMenuItem
      Caption = 'Method'
      object First1: TMenuItem
        Caption = 'Simple - single pass'
        Checked = True
        RadioItem = True
        OnClick = First1Click
      end
      object Second1: TMenuItem
        Caption = 'Multi pass'
        RadioItem = True
        OnClick = First1Click
      end
      object Both1: TMenuItem
        Caption = 'Both'
        RadioItem = True
        OnClick = First1Click
      end
    end
    object Span1: TMenuItem
      Caption = 'Span'
      object mnuOneToTen: TMenuItem
        Caption = '1/10 (default)'
        Checked = True
        RadioItem = True
        OnClick = mnuOneToTenClick
      end
      object mnuOneToFive: TMenuItem
        Caption = '1/5'
        RadioItem = True
        OnClick = mnuOneToTenClick
      end
      object mnuOneToTwo: TMenuItem
        Caption = '1/2'
        RadioItem = True
        OnClick = mnuOneToTenClick
      end
    end
  end
  object PrintDialog1: TPrintDialog
    Left = 472
    Top = 120
  end
end
