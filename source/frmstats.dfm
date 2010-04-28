object FrmStatistics: TFrmStatistics
  Left = 242
  Top = 100
  Caption = 'Statistics'
  ClientHeight = 487
  ClientWidth = 701
  Color = clBtnFace
  Constraints.MinHeight = 445
  Constraints.MinWidth = 630
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmnuMainMenu
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = IFormCreate
  OnDestroy = IFormDestroy
  OnShow = FormShow
  DesignSize = (
    701
    487)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 550
    Top = 18
    Width = 146
    Height = 39
    Anchors = [akTop, akRight]
    Caption = 
      'Select distributions to display. Use shift and/or ctrl key or dr' +
      'ag to select many at once:'
    WordWrap = True
  end
  object btnLog: TCheckBox
    Left = 555
    Top = 468
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Logarithmic'
    TabOrder = 0
    OnClick = btnLogClick
  end
  object grpEmpiricalDistribution: TGroupBox
    Left = 550
    Top = 362
    Width = 149
    Height = 97
    Anchors = [akRight, akBottom]
    Caption = 'Empirical Distributions'
    TabOrder = 1
    object btnWeibullPoints: TCheckBox
      Left = 10
      Top = 17
      Width = 97
      Height = 17
      Caption = 'Weibull Points'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = btnWeibullPointsClick
    end
    object btnBlomPoints: TCheckBox
      Left = 10
      Top = 35
      Width = 97
      Height = 17
      Caption = 'Blom Points'
      TabOrder = 1
      OnClick = btnWeibullPointsClick
    end
    object btnCunnanePoints: TCheckBox
      Left = 10
      Top = 53
      Width = 97
      Height = 17
      Caption = 'Cunnane Points'
      TabOrder = 2
      OnClick = btnWeibullPointsClick
    end
    object btnGringortenPoints: TCheckBox
      Left = 9
      Top = 72
      Width = 106
      Height = 17
      Caption = 'Gringorten Points'
      TabOrder = 3
      OnClick = btnWeibullPointsClick
    end
  end
  object pgcPages: TPageControl
    Left = 0
    Top = 0
    Width = 548
    Height = 484
    ActivePage = tbcPlots
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object tbcPlots: TTabSheet
      Caption = 'Distribution functions'
      DesignSize = (
        540
        456)
      object tbcMonths: TTabControl
        Left = 0
        Top = 3
        Width = 538
        Height = 451
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabPosition = tpBottom
        Tabs.Strings = (
          'All Data'
          'January'
          'February'
          'March'
          'April'
          'May'
          'June'
          'Jully'
          'August'
          'September'
          'October'
          'November'
          'December')
        TabIndex = 0
        TabWidth = 41
        OnChange = tbcMonthsChange
        DesignSize = (
          538
          451)
        object Chart: TChart
          Left = 5
          Top = 5
          Width = 526
          Height = 419
          AllowPanning = pmNone
          BackWall.Brush.Color = clWhite
          BackWall.Brush.Style = bsClear
          Legend.Alignment = laTop
          Legend.LegendStyle = lsSeries
          MarginBottom = 3
          MarginTop = 3
          Title.Text.Strings = (
            'TChart')
          Title.Visible = False
          BottomAxis.Automatic = False
          BottomAxis.AutomaticMaximum = False
          BottomAxis.AutomaticMinimum = False
          BottomAxis.Grid.Visible = False
          BottomAxis.Labels = False
          BottomAxis.LabelsAngle = 90
          BottomAxis.LabelStyle = talMark
          BottomAxis.Maximum = 24.000000000000000000
          BottomAxis.MinorTickCount = 2
          BottomAxis.MinorTickLength = 0
          BottomAxis.MinorTicks.Visible = False
          LeftAxis.Automatic = False
          LeftAxis.AutomaticMaximum = False
          LeftAxis.AutomaticMinimum = False
          LeftAxis.Grid.Color = clBlack
          LeftAxis.Maximum = 100.000000000000000000
          LeftAxis.Minimum = 537.500000000000000000
          LeftAxis.MinorGrid.Color = clGray
          LeftAxis.MinorGrid.Style = psDashDotDot
          LeftAxis.MinorGrid.SmallDots = True
          LeftAxis.MinorGrid.Visible = True
          LeftAxis.MinorTickCount = 1
          LeftAxis.Title.Font.Height = -9
          LeftAxis.TitleSize = 2
          RightAxis.Automatic = False
          RightAxis.AutomaticMaximum = False
          RightAxis.AutomaticMinimum = False
          RightAxis.Grid.Visible = False
          RightAxis.Labels = False
          RightAxis.MinorTickCount = 2
          RightAxis.MinorTicks.Visible = False
          TopAxis.Automatic = False
          TopAxis.AutomaticMaximum = False
          TopAxis.AutomaticMinimum = False
          TopAxis.Grid.Color = clBlack
          TopAxis.LabelsAngle = 90
          TopAxis.LabelStyle = talMark
          TopAxis.Maximum = 10.000000000000000000
          TopAxis.Minimum = -10.000000000000000000
          TopAxis.MinorTickCount = 2
          TopAxis.MinorTickLength = 0
          TopAxis.MinorTicks.Visible = False
          TopAxis.Title.Caption = 'Exceedance Probability (%)'
          TopAxis.Title.Font.Height = -9
          TopAxis.TitleSize = 3
          View3D = False
          View3DWalls = False
          Zoom.Allow = False
          Zoom.Pen.Color = clGray
          BevelOuter = bvNone
          Color = clWhite
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Anchors = [akLeft, akTop, akRight, akBottom]
          PrintMargins = (
            15
            10
            15
            10)
          object GridPoint: TPointSeries
            HorizAxis = aTopAxis
            Marks.Arrow.Visible = False
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = False
            Marks.Callout.Length = 5
            Marks.BackColor = clWhite
            Marks.Clip = True
            Marks.Color = clWhite
            Marks.Frame.Visible = False
            Marks.Visible = True
            SeriesColor = clTeal
            ShowInLegend = False
            Title = 'GridPoint'
            ClickableLine = False
            Pointer.Brush.Color = clBlack
            Pointer.HorizSize = 1
            Pointer.InflateMargins = True
            Pointer.Pen.Visible = False
            Pointer.Style = psRectangle
            Pointer.VertSize = 1
            Pointer.Visible = True
            XValues.Name = 'X'
            XValues.Order = loAscending
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object WeibullPoints: TPointSeries
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Shadow.Color = 8553090
            Marks.Visible = False
            SeriesColor = clBlack
            Title = 'Weibull'
            OnClick = WeibullPointsClick
            ClickableLine = False
            Pointer.Brush.Color = clBlue
            Pointer.HorizSize = 3
            Pointer.InflateMargins = True
            Pointer.Style = psCircle
            Pointer.VertSize = 3
            Pointer.Visible = True
            XValues.Name = 'X'
            XValues.Order = loAscending
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object BlomPoints: TPointSeries
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clGreen
            Title = 'Blom'
            OnClick = WeibullPointsClick
            ClickableLine = False
            Pointer.HorizSize = 3
            Pointer.InflateMargins = True
            Pointer.Style = psTriangle
            Pointer.VertSize = 3
            Pointer.Visible = True
            XValues.Name = 'X'
            XValues.Order = loAscending
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object CunnanePoints: TPointSeries
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.BackColor = clBlack
            Marks.Color = clBlack
            Marks.Visible = False
            SeriesColor = clBlack
            Title = 'Cunnane'
            OnClick = WeibullPointsClick
            ClickableLine = False
            Pointer.Brush.Color = clBlack
            Pointer.HorizSize = 3
            Pointer.InflateMargins = True
            Pointer.Style = psCross
            Pointer.VertSize = 3
            Pointer.Visible = True
            XValues.Name = 'X'
            XValues.Order = loAscending
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GringortenPoints: TPointSeries
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clBlue
            Title = 'Gringorten'
            OnClick = WeibullPointsClick
            ClickableLine = False
            Pointer.Brush.Color = clBlack
            Pointer.HorizSize = 3
            Pointer.InflateMargins = True
            Pointer.Style = psDiagCross
            Pointer.VertSize = 3
            Pointer.Visible = True
            XValues.Name = 'X'
            XValues.Order = loAscending
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object NormalLine: TLineSeries
            Tag = 101
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 16384
            Title = 'Normal'
            LinePen.Style = psDash
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object NormalLLine: TLineSeries
            Tag = 115
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 4210816
            Title = 'L-Moments Normal'
            LinePen.Style = psDash
            LinePen.SmallDots = True
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object LogNormalLine: TLineSeries
            Tag = 102
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clFuchsia
            Title = 'LogNormal'
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GaltonLine: TLineSeries
            Tag = 103
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clNavy
            Title = 'Galton'
            LinePen.Style = psDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object ExponentialLine: TLineSeries
            Tag = 104
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 16512
            Title = 'Exponential'
            LinePen.Style = psDashDotDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object ExponentialLLine: TLineSeries
            Tag = 116
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 12615935
            Title = 'L-Moments Exponential'
            LinePen.Style = psDashDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GammaLine: TLineSeries
            Tag = 105
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clPurple
            Title = 'Gamma'
            LinePen.Style = psDash
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object PearsonIIILine: TLineSeries
            Tag = 106
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 64
            Title = 'PearsonIII'
            LinePen.Style = psDashDotDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object LogPearsonIIILine: TLineSeries
            Tag = 107
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 8404992
            Title = 'LogPearsonIII'
            LinePen.Style = psDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GumbelMAXLine: TLineSeries
            Tag = 108
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 8388672
            Title = 'Gumbel Max'
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object EV2MAXLine: TLineSeries
            Tag = 109
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 16744448
            Title = 'EV2-Max'
            LinePen.Style = psDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GumbelMINLine: TLineSeries
            Tag = 110
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 16384
            Title = 'Gumbel Min'
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object WeibullLine: TLineSeries
            Tag = 111
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clBlue
            Title = 'Weibull'
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GEVMAXLine: TLineSeries
            Tag = 112
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clMaroon
            Title = 'GEV Max'
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GEVMINLine: TLineSeries
            Tag = 113
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 4227072
            Title = 'GEV Min'
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object ParetoLine: TLineSeries
            Tag = 114
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 4227072
            Title = 'Pareto'
            LinePen.SmallDots = True
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GEVMAXLLine: TLineSeries
            Tag = 121
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 4210816
            Title = 'L-Moments GEV Max'
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GEVMINLLine: TLineSeries
            Tag = 122
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 8404992
            Title = 'L-Moments GEV Min'
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object EV1MAXLLine: TLineSeries
            Tag = 117
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clLime
            Title = 'L-Moments EV1-Max'
            LinePen.SmallDots = True
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object EV2MAXLLine: TLineSeries
            Tag = 118
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clPurple
            Title = 'L-Moments EV2-Max'
            LinePen.SmallDots = True
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object EV1MINLLine: TLineSeries
            Tag = 119
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 4227327
            Title = 'L-Moments EV1-Min'
            LinePen.SmallDots = True
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object EV3MINLLine: TLineSeries
            Tag = 120
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 4227200
            Title = 'L-Moments EV3-Min'
            LinePen.SmallDots = True
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object ParetoLLine: TLineSeries
            Tag = 123
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 8404992
            Title = 'L-Moments Pareto'
            LinePen.Style = psDashDotDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GEVMAXKLine: TLineSeries
            Tag = 124
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 16711808
            Title = 'GEV-Max (k spec.)'
            LinePen.Style = psDashDotDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GEVMINKLine: TLineSeries
            Tag = 125
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 12615808
            Title = 'GEV-Min (k spec.)'
            LinePen.Style = psDashDotDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GEVMAXKLLine: TLineSeries
            Tag = 126
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 33023
            Title = 'L-Moments GEV-Max (k. spec.)'
            LinePen.Style = psDashDotDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object GEVMINKLLine: TLineSeries
            Tag = 127
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = 16744703
            Title = 'L-Moments GEV-Min (k. spec.)'
            LinePen.Style = psDashDotDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object LowSampleLimitLine: TLineSeries
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clBlack
            Title = 'Low sample limit'
            LinePen.Style = psDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object HighSampleLimitLine: TLineSeries
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clBlack
            ShowInLegend = False
            Title = 'High sample limit'
            LinePen.Style = psDot
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object HighConfidenceLimitLine: TLineSeries
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clMaroon
            ShowInLegend = False
            Title = 'High confidence limit'
            LinePen.Style = psDash
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
          object LowConfidenceLimitLine: TLineSeries
            Active = False
            Marks.Arrow.Visible = True
            Marks.Callout.Brush.Color = clBlack
            Marks.Callout.Arrow.Visible = True
            Marks.Visible = False
            SeriesColor = clMaroon
            Title = 'Low confidence limit'
            LinePen.Style = psDash
            Pointer.InflateMargins = True
            Pointer.Style = psRectangle
            Pointer.Visible = False
            XValues.Name = 'X'
            XValues.Order = loNone
            YValues.Name = 'Y'
            YValues.Order = loNone
          end
        end
      end
    end
    object tbcHistogram: TTabSheet
      Caption = 'Histogram - Density functions'
      ImageIndex = 2
      object chartPDF: TChart
        Left = 3
        Top = 3
        Width = 502
        Height = 424
        Legend.Alignment = laTop
        Legend.LegendStyle = lsSeries
        MarginBottom = 3
        MarginTop = 3
        Title.Font.Charset = GREEK_CHARSET
        Title.Font.Color = clBlack
        Title.Font.Name = 'Tahoma'
        Title.Text.Strings = (
          'Probability Density Functions (PDF) - Histogram')
        BottomAxis.MinorGrid.Color = clSilver
        BottomAxis.MinorGrid.Style = psDash
        BottomAxis.MinorGrid.SmallDots = True
        BottomAxis.MinorGrid.Visible = True
        LeftAxis.Labels = False
        Shadow.Visible = False
        View3D = False
        View3DWalls = False
        Zoom.Pen.Color = clSilver
        BevelOuter = bvNone
        Color = clWhite
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        PrintMargins = (
          15
          12
          15
          12)
        object seriesHistogram: TBarSeries
          BarBrush.Color = clWhite
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Visible = False
          SeriesColor = 14803696
          ShowInLegend = False
          Title = 'seriesHistogram'
          BarStyle = bsRectGradient
          BarWidthPercent = 50
          Gradient.Direction = gdTopBottom
          Gradient.StartColor = 11053311
          Shadow.HorizSize = 1
          Shadow.Visible = False
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Bar'
          YValues.Order = loNone
        end
      end
    end
    object tbcParameters: TTabSheet
      Caption = 'Parameter evaluation - Forecasts'
      ImageIndex = 1
      DesignSize = (
        540
        456)
      object sgrdData: TOdStringGrid
        Left = 0
        Top = 0
        Width = 538
        Height = 454
        Anchors = [akLeft, akTop, akRight, akBottom]
        Color = clWhite
        DefaultColWidth = 60
        DefaultRowHeight = 18
        FixedColor = 14540253
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goRowMoving, goColMoving]
        TabOrder = 0
        SelectedCellColor = 8404992
        SelectedFontColor = clWhite
        FitColToWidth = -1
        goPasteNonEditable = False
        RowHeights = (
          18
          18
          18
          18
          18)
      end
    end
  end
  object Memo: TMemo
    Left = 346
    Top = 154
    Width = 185
    Height = 101
    Lines.Strings = (
      'Data count'
      'Mean value'
      'Standard deviation'
      'Third central moment'
      'Skewness'
      'Kurtosis'
      'Mean value of y=Ln(x)'
      'Std. dev. of y=Ln(x)'
      'Third c. moment -//-'
      'Skewness r -//-'
      'LogNormal my'
      'LogNormal sy'
      'Galton my'
      'Galton sy'
      'Galton c'
      'Exponential c'
      'Exponential Lambda'
      'Gamma Lambda'
      'Gamma Kappa'
      'Pearson III Kappa'
      'Pearson III Lambda'
      'Pearson III c'
      'Log Pearson III Kappa'
      'Log Pearson III Lambda'
      'Log Pearson III c'
      'EV-1 (Gumbel) Max Lambda'
      'EV-1 (Gumbel) Max Psi'
      'EV-2 Max Kappa'
      'EV-2 Max Lambda'
      'EV-1 (Gumbel) Min Lambda'
      'EV-1 (Gumbel) Min Psi'
      'EV-3 (Weibull) Min Kappa'
      'EV-3 (Weibull) Min Lambda'
      'GEV Max Kappa'
      'GEV Max Lambda'
      'GEV Max Psi '
      'GEV Min Kappa'
      'GEV Min Lambda'
      'GEV Min Psi '
      'Pareto Kappa'
      'Pareto Lambda'
      'Pareto Psi'
      'LMoment: L1'
      'LMoment: L2'
      'LMoment: L3'
      'LMoment: L4'
      'L-Skewness (tau-3)'
      'L-Kurtosis (tau-4)'
      'LM-Normal mx'
      'LM-Normal sx'
      'LM-Exponential c'
      'LM-Exponential Lambda'
      'LM-EV-1 (Gumbel) Max Lambda'
      'LM-EV-1 (Gumbel) Max Psi'
      'LM-EV-2 Max Kappa'
      'LM-EV-2 Max Lambda'
      'LM-EV-1 (Gumbel) Min Lambda'
      'LM-EV-1 (Gumbel) Min Psi'
      'LM-EV-3 (Weibull) Min Kappa'
      'LM-EV-3 (Weibull) Min Lambda'
      'LM-GEV Max Kappa'
      'LM-GEV Max Lambda'
      'LM-GEV Max Psi '
      'LM-GEV Min Kappa'
      'LM-GEV Min Lambda'
      'LM-GEV Min Psi '
      'LM-Pareto Kappa'
      'LM-Pareto Lambda'
      'LM-Pareto Psi'
      'GEV Max (k spc.) Kappa'
      'GEV Max (k spc.) Lambda'
      'GEV Max (k spc.) Psi '
      'GEV Min (k spc.) Kappa'
      'GEV Min (k spc.) Lambda'
      'GEV Min (k spc.) Psi '
      'LM-GEV Max (k spc.) Kappa'
      'LM-GEV Max (k spc.) Lambda'
      'LM-GEV Max (k spc.) Psi '
      'LM-GEV Min (k spc.) Kappa'
      'LM-GEV Min (k spc.) Lambda'
      'LM-GEV Min (k spc.) Psi ')
    TabOrder = 2
    Visible = False
  end
  object btnReset: TButton
    Left = 554
    Top = 335
    Width = 66
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Reset'
    TabOrder = 4
    OnClick = btnResetClick
  end
  object lstDistributions: TListBox
    Left = 550
    Top = 72
    Width = 149
    Height = 257
    Anchors = [akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 5
    OnClick = btnNormalLineClick
  end
  object mmnuMainMenu: TMainMenu
    Left = 335
    Top = 65531
    object mnuFile: TMenuItem
      Caption = 'File'
      object mnuPrintChart: TMenuItem
        Caption = 'Print Chart'
        ShortCut = 16464
        OnClick = mnuPrintChartClick
      end
      object mnuPrintHistogram: TMenuItem
        Caption = 'Print Histogram'
        OnClick = mnuPrintHistogramClick
      end
      object mnuPrinterSetup: TMenuItem
        Caption = 'Print Setup'
        OnClick = mnuPrinterSetupClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuExit: TMenuItem
        Caption = 'Exit'
        ShortCut = 32883
        OnClick = mnuExitClick
      end
    end
    object mnuEdit: TMenuItem
      Caption = 'Edit'
      object mnuCopyChart: TMenuItem
        Caption = 'Copy chart to clipboard'
        ShortCut = 16451
        OnClick = mnuCopyChartClick
      end
      object mnuCopyChartData: TMenuItem
        Caption = 'Copy all chart data as text'
        OnClick = mnuCopyChartDataClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuCopyGridClipboardAll: TMenuItem
        Caption = 'Copy entire grid to clipbrd'
        ShortCut = 24643
        OnClick = mnuCopyGridClipboardAllClick
      end
      object MnuCopyGridSelection: TMenuItem
        Caption = 'Copy grid selection'
        ShortCut = 24641
        OnClick = MnuCopyGridSelectionClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object mnuCopyHistogram: TMenuItem
        Caption = 'Copy PDF && histogram to clipboard'
        OnClick = mnuCopyHistogramClick
      end
    end
    object mnuView: TMenuItem
      Caption = 'View'
      object mnuPaperType: TMenuItem
        Caption = 'CDF Paper Type:'
        Enabled = False
      end
      object mnuLinear: TMenuItem
        Caption = 'Linear'
        GroupIndex = 3
        RadioItem = True
        OnClick = mnuLinearClick
      end
      object mnuNormal: TMenuItem
        Caption = 'Normal'
        Checked = True
        GroupIndex = 3
        RadioItem = True
        OnClick = mnuNormalClick
      end
      object mnuLogNormal: TMenuItem
        Caption = 'Log Normal'
        GroupIndex = 3
        RadioItem = True
        OnClick = mnuLogNormalClick
      end
      object mnuGumbelMax: TMenuItem
        Caption = 'Gumbel Max'
        GroupIndex = 3
        RadioItem = True
        OnClick = mnuGumbelMaxClick
      end
      object mnuGumbelMin: TMenuItem
        Caption = 'Gumbel Min'
        GroupIndex = 3
        RadioItem = True
        OnClick = mnuGumbelMinClick
      end
      object mnuWeibull: TMenuItem
        Caption = 'Weibull (Min)'
        GroupIndex = 3
        RadioItem = True
        OnClick = mnuWeibullClick
      end
      object mnuGEV: TMenuItem
        Caption = 'GEV Max'
        GroupIndex = 3
        RadioItem = True
        OnClick = mnuGEVClick
      end
      object mnuGEVMin: TMenuItem
        Caption = 'GEV Min'
        GroupIndex = 3
        RadioItem = True
        OnClick = mnuGEVMinClick
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 5
      end
      object HorizAxisis1: TMenuItem
        Caption = 'CDF Horiz. Axis is:'
        Enabled = False
        GroupIndex = 5
      end
      object mnuExceedanceProbability: TMenuItem
        Caption = 'Exceedance probability'
        Checked = True
        GroupIndex = 15
        RadioItem = True
        OnClick = mnuExceedanceProbabilityClick
      end
      object mnuProbabilityFunction: TMenuItem
        Caption = 'Probability function'
        GroupIndex = 15
        RadioItem = True
        OnClick = mnuProbabilityFunctionClick
      end
      object mnuReturnPeriodMax: TMenuItem
        Caption = 'Return Period (Max)'
        GroupIndex = 15
        RadioItem = True
        OnClick = mnuReturnPeriodMaxClick
      end
      object mnuReturnPeriodMin: TMenuItem
        Caption = 'Return period (Min)'
        GroupIndex = 15
        RadioItem = True
        OnClick = mnuReturnPeriodMinClick
      end
      object N7: TMenuItem
        Caption = '-'
        GroupIndex = 15
      end
      object mnuRefreshParametersTable: TMenuItem
        Caption = 'Refresh parameters table'
        GroupIndex = 15
        ShortCut = 116
        OnClick = mnuHideForecastsClick
      end
    end
    object mnuOptions: TMenuItem
      Caption = 'Options'
      object mnuUnbiased: TMenuItem
        Caption = 'Unbiased evaluation'
        OnClick = mnuUnbiasedClick
      end
      object mnuSetPaperMaxX: TMenuItem
        Caption = 'Set Max X Value'
        OnClick = mnuSetPaperMaxXClick
      end
      object SetGEVShapeparameter1: TMenuItem
        Caption = 'Specify K (GEV shape parm.)'
        OnClick = SetGEVShapeparameter1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mnuNegativeValues: TMenuItem
        Caption = 'Negative values are:'
        Enabled = False
      end
      object mnuTruncateToZero: TMenuItem
        Caption = 'Truncated to zero (0) for statistics'
        Checked = True
        GroupIndex = 5
        OnClick = mnuTruncateToZeroClick
      end
      object N9: TMenuItem
        Caption = '-'
        GroupIndex = 5
      end
      object mnuAutoLeftAxis: TMenuItem
        Caption = 'Auto left axis - Display negative values'
        GroupIndex = 5
        OnClick = mnuAutoLeftAxisClick
      end
      object mnuAllowZoomAndPan: TMenuItem
        Caption = 'Allow Zoom and Pan'
        GroupIndex = 5
        OnClick = mnuAllowZoomAndPanClick
      end
      object Showhistogram1: TMenuItem
        AutoCheck = True
        Caption = 'Show histogram alongside the PDFs'
        Checked = True
        GroupIndex = 5
        OnClick = Showhistogram1Click
      end
    end
    object mnuForecasts: TMenuItem
      Caption = 'Forecasts'
      object mnuToAProbability: TMenuItem
        Caption = 'To a probability value'
        OnClick = mnuToAProbabilityClick
      end
      object mnuToAnExceedanceProbability: TMenuItem
        Tag = 1
        Caption = 'To an exceedance probability value'
        OnClick = mnuToAProbabilityClick
      end
      object mnuToAReturnPeriodMax: TMenuItem
        Tag = 2
        Caption = 'To a return period (Max)'
        OnClick = mnuToAProbabilityClick
      end
      object mnuToAReturnPeriodMin: TMenuItem
        Tag = 3
        Caption = 'To a return period (Min)'
        OnClick = mnuToAProbabilityClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnuToAValue: TMenuItem
        Tag = 4
        Caption = 'To a value'
        OnClick = mnuToAProbabilityClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnuHideForecasts: TMenuItem
        Caption = 'Hide forecasts'
        OnClick = mnuHideForecastsClick
      end
    end
    object mnuConfidence: TMenuItem
      Tag = 1
      Caption = 'Confidence'
      object mnuNormalFamily: TMenuItem
        Tag = 2
        Caption = 'Normal family'
        object mnuConfidenceNormal: TMenuItem
          Tag = 1
          Caption = 'Normal'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceLogNormal: TMenuItem
          Tag = 2
          Caption = 'Log Normal'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGalton: TMenuItem
          Tag = 3
          Caption = 'Galton (LN 3p)'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceLMomentsNormal: TMenuItem
          Tag = 15
          Caption = 'L-Moments Normal'
          OnClick = mnuConfidenceClick
        end
      end
      object mnuGammaFamily: TMenuItem
        Caption = 'Gamma family'
        object mnuConfidenceExponential: TMenuItem
          Tag = 4
          Caption = 'Exponential'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGamma: TMenuItem
          Tag = 5
          Caption = 'Gamma (2p)'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidencePearsonIII: TMenuItem
          Tag = 6
          Caption = 'PearsonIII'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceLogPearsonIII: TMenuItem
          Tag = 7
          Caption = 'LogPearsonIII'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceLMomentExponential: TMenuItem
          Tag = 16
          Caption = 'L-Moments Exponential'
          OnClick = mnuConfidenceClick
        end
      end
      object mnuEVFamily: TMenuItem
        Caption = 'EV family (Moment)'
        object mnuConfidenceGumbelMax: TMenuItem
          Tag = 8
          Caption = 'Gumbel Max'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceEV2MAX: TMenuItem
          Tag = 9
          Caption = 'EV2 Max'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGumbelMin: TMenuItem
          Tag = 10
          Caption = 'Gumbel Min'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceWeibull: TMenuItem
          Tag = 11
          Caption = 'Weibull'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGEVMax: TMenuItem
          Tag = 12
          Caption = 'GEV Max'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGEVMin: TMenuItem
          Tag = 13
          Caption = 'GEV Min'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGEVMaxK: TMenuItem
          Tag = 24
          Caption = 'GEV Max (k spec.)'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGEVMinK: TMenuItem
          Tag = 25
          Caption = 'GEV Min (k spec.)'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidencePareto: TMenuItem
          Tag = 14
          Caption = 'Pareto'
          OnClick = mnuConfidenceClick
        end
      end
      object mnuEVfamilyLMoments: TMenuItem
        Caption = 'EV family (L-Moments)'
        object mnuConfidenceGumbelLMax: TMenuItem
          Tag = 17
          Caption = 'Gumbel Max'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceEV2LMAX: TMenuItem
          Tag = 18
          Caption = 'EV2 Max'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGumbelLMin: TMenuItem
          Tag = 19
          Caption = 'Gumbel Min'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceWeibullL: TMenuItem
          Tag = 20
          Caption = 'Weibull'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGEVLMax: TMenuItem
          Tag = 21
          Caption = 'GEV Max'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGEVLMin: TMenuItem
          Tag = 22
          Caption = 'GEV Min'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGEVLMaxK: TMenuItem
          Tag = 26
          Caption = 'GEV Max (k spec.)'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceGEVLMinK: TMenuItem
          Tag = 27
          Caption = 'GEV Min (k spec.)'
          OnClick = mnuConfidenceClick
        end
        object mnuConfidenceParetoL: TMenuItem
          Tag = 23
          Caption = 'Pareto'
          OnClick = mnuConfidenceClick
        end
      end
      object mnuMCSettings: TMenuItem
        Caption = 'Settings'
        OnClick = mnuMCSettingsClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object mnuConfidenceForecasts: TMenuItem
        Caption = 'Forecasts'
        OnClick = mnuConfidenceForecastsClick
      end
      object mnuHideConfidenceForcasts: TMenuItem
        Caption = 'Hide forecasts'
        OnClick = mnuHideForecastsClick
      end
    end
    object mnuTests: TMenuItem
      Caption = 'Tests'
      object mnuXSquareTest: TMenuItem
        Caption = 'X-Square test'
        OnClick = mnuXSquareTestClick
      end
      object mnuKolmogorovSminovTest: TMenuItem
        Caption = 'Kolmogorov - Smirnov'
        OnClick = mnuKolmogorovSminovTestClick
      end
    end
  end
  object PrintDialog: TPrintDialog
    Left = 360
    Top = 65531
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 385
    Top = 65531
  end
end
