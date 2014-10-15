object FrmIDFCurves: TFrmIDFCurves
  Left = 281
  Top = 149
  Caption = 'IDF Curves'
  ClientHeight = 424
  ClientWidth = 622
  Color = clBtnFace
  Constraints.MinHeight = 445
  Constraints.MinWidth = 620
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mnuMainMenu
  OldCreateOrder = False
  OnCreate = IFormCreate
  OnDestroy = IFormDestroy
  OnHide = IFormHide
  OnShow = IFormShow
  DesignSize = (
    622
    424)
  PixelsPerInch = 96
  TextHeight = 13
  object pgcTabs: TPageControl
    Left = 0
    Top = 0
    Width = 621
    Height = 421
    ActivePage = tabMulti
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabMulti: TTabSheet
      Caption = 'Multiple curves'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        613
        393)
      object GroupBox1: TGroupBox
        Left = 507
        Top = 0
        Width = 106
        Height = 246
        Anchors = [akTop, akRight]
        Caption = 'Return periods'
        TabOrder = 0
        object chkMulti1000: TCheckBox
          Left = 5
          Top = 60
          Width = 97
          Height = 17
          Caption = '1000 years'
          TabOrder = 0
          OnClick = chkMulti1000Click
        end
        object chkMulti500: TCheckBox
          Left = 5
          Top = 80
          Width = 97
          Height = 17
          Caption = '500 years'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = chkMulti1000Click
        end
        object chkMulti100: TCheckBox
          Left = 5
          Top = 120
          Width = 97
          Height = 17
          Caption = '100 years'
          TabOrder = 2
          OnClick = chkMulti1000Click
        end
        object chkMulti50: TCheckBox
          Left = 5
          Top = 140
          Width = 97
          Height = 17
          Caption = '50 years'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = chkMulti1000Click
        end
        object chkMulti20: TCheckBox
          Left = 5
          Top = 160
          Width = 97
          Height = 17
          Caption = '20 years'
          TabOrder = 4
          OnClick = chkMulti1000Click
        end
        object chkMulti10: TCheckBox
          Left = 5
          Top = 180
          Width = 97
          Height = 17
          Caption = '10 years'
          Checked = True
          State = cbChecked
          TabOrder = 5
          OnClick = chkMulti1000Click
        end
        object chkMulti5: TCheckBox
          Left = 5
          Top = 200
          Width = 97
          Height = 17
          Caption = '5 years'
          Checked = True
          State = cbChecked
          TabOrder = 6
          OnClick = chkMulti1000Click
        end
        object chkMulti2: TCheckBox
          Left = 5
          Top = 220
          Width = 97
          Height = 17
          Caption = '2 years'
          TabOrder = 7
          OnClick = chkMulti1000Click
        end
        object chkMulti10000: TCheckBox
          Left = 5
          Top = 40
          Width = 97
          Height = 17
          Caption = '10000 years'
          TabOrder = 8
          OnClick = chkMulti1000Click
        end
        object chkMulti100000: TCheckBox
          Left = 5
          Top = 20
          Width = 97
          Height = 17
          Caption = '100000 years'
          TabOrder = 9
          OnClick = chkMulti1000Click
        end
        object chkMulti200: TCheckBox
          Left = 4
          Top = 100
          Width = 97
          Height = 17
          Caption = '200 years'
          TabOrder = 10
          OnClick = chkMulti1000Click
        end
      end
      object chartMulti: TChart
        Left = 5
        Top = 2
        Width = 496
        Height = 381
        AllowPanning = pmNone
        BackWall.Brush.Style = bsClear
        Legend.LegendStyle = lsSeries
        Title.Font.Color = clBlack
        Title.Font.Height = -12
        Title.Font.Style = [fsBold]
        Title.Text.Strings = (
          'TChart')
        BottomAxis.Automatic = False
        BottomAxis.AutomaticMaximum = False
        BottomAxis.AutomaticMinimum = False
        BottomAxis.LabelsFormat.TextAlignment = taCenter
        BottomAxis.Logarithmic = True
        BottomAxis.Maximum = 100.000000000000000000
        BottomAxis.Minimum = 0.010000000000000000
        BottomAxis.MinorGrid.Color = clSilver
        BottomAxis.MinorGrid.Visible = True
        BottomAxis.MinorTickCount = 8
        BottomAxis.Title.Caption = 'd (h)'
        BottomAxis.TitleSize = 1
        DepthAxis.LabelsFormat.TextAlignment = taCenter
        DepthTopAxis.LabelsFormat.TextAlignment = taCenter
        LeftAxis.Automatic = False
        LeftAxis.AutomaticMaximum = False
        LeftAxis.AutomaticMinimum = False
        LeftAxis.LabelsFormat.TextAlignment = taCenter
        LeftAxis.Logarithmic = True
        LeftAxis.Maximum = 1000.000000000000000000
        LeftAxis.Minimum = 1.000000000000000000
        LeftAxis.MinorGrid.Color = clSilver
        LeftAxis.MinorGrid.Visible = True
        LeftAxis.MinorTickCount = 8
        LeftAxis.Title.Caption = 'i (mm/h)'
        LeftAxis.TitleSize = 1
        RightAxis.LabelsFormat.TextAlignment = taCenter
        TopAxis.LabelsFormat.TextAlignment = taCenter
        View3D = False
        View3DWalls = False
        Zoom.Allow = False
        Zoom.Pen.Mode = pmNotXor
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 1
        Anchors = [akLeft, akTop, akRight, akBottom]
        DefaultCanvas = 'TGDIPlusCanvas'
        ColorPaletteIndex = 13
        object seriesMulti100000: TLineSeries
          Marks.Visible = False
          SeriesColor = 64
          Title = '100000'
          Brush.BackColor = clDefault
          LinePen.Style = psDot
          Pointer.InflateMargins = True
          Pointer.Style = psSmallDot
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesMulti10000: TLineSeries
          Marks.Visible = False
          SeriesColor = 4210688
          Title = '10000'
          Brush.BackColor = clDefault
          LinePen.Style = psDash
          Pointer.InflateMargins = True
          Pointer.Style = psSmallDot
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesMulti1000: TLineSeries
          Marks.Visible = False
          SeriesColor = clNavy
          Title = '1000'
          Brush.BackColor = clDefault
          Pointer.InflateMargins = True
          Pointer.Style = psSmallDot
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesMulti500: TLineSeries
          Marks.Visible = False
          SeriesColor = clTeal
          Title = '500'
          Brush.BackColor = clDefault
          Pointer.HorizSize = 3
          Pointer.InflateMargins = True
          Pointer.Style = psDiamond
          Pointer.VertSize = 3
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesMulti200: TLineSeries
          Marks.Visible = False
          SeriesColor = clPurple
          Title = '200'
          Brush.BackColor = clDefault
          Pointer.Brush.Color = clPurple
          Pointer.HorizSize = 3
          Pointer.InflateMargins = True
          Pointer.Style = psCircle
          Pointer.VertSize = 3
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesMulti100: TLineSeries
          Marks.Visible = False
          SeriesColor = 8388672
          Title = '100'
          Brush.BackColor = clDefault
          Pointer.InflateMargins = True
          Pointer.Style = psStar
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesMulti50: TLineSeries
          Marks.Visible = False
          SeriesColor = clGray
          Title = '50'
          Brush.BackColor = clDefault
          Pointer.HorizSize = 3
          Pointer.InflateMargins = True
          Pointer.Style = psCross
          Pointer.VertSize = 3
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesMulti20: TLineSeries
          Marks.Visible = False
          SeriesColor = clBlue
          Title = '20'
          Brush.BackColor = clDefault
          Pointer.HorizSize = 3
          Pointer.InflateMargins = True
          Pointer.Style = psDownTriangle
          Pointer.VertSize = 3
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesMulti10: TLineSeries
          Marks.Visible = False
          SeriesColor = 16512
          Title = '10'
          Brush.BackColor = clDefault
          Pointer.HorizSize = 3
          Pointer.InflateMargins = True
          Pointer.Style = psTriangle
          Pointer.VertSize = 3
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesMulti5: TLineSeries
          Marks.Visible = False
          SeriesColor = clGreen
          Title = '5'
          Brush.BackColor = clDefault
          Pointer.HorizSize = 3
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          Pointer.VertSize = 3
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesMulti2: TLineSeries
          Marks.Visible = False
          SeriesColor = clMaroon
          Title = '2'
          Brush.BackColor = clDefault
          Pointer.InflateMargins = True
          Pointer.Style = psCircle
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
      end
    end
    object tabSingle: TTabSheet
      Caption = 'Single curve'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        613
        393)
      object Label21: TLabel
        Left = 5
        Top = 5
        Width = 120
        Height = 13
        Caption = 'Return period T (years): '
      end
      object Label22: TLabel
        Left = 70
        Top = 115
        Width = 24
        Height = 18
        Caption = 'a ='
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object GroupBox4: TGroupBox
        Left = 5
        Top = 35
        Width = 241
        Height = 76
        Caption = 'IDF curve for specified T'
        Color = clMenu
        ParentColor = False
        TabOrder = 0
        object Label17: TLabel
          Left = 25
          Top = 25
          Width = 75
          Height = 18
          Caption = 'i (mm/h) ='
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Shape1: TShape
          Left = 85
          Top = 35
          Width = 116
          Height = 1
        end
        object Label18: TLabel
          Left = 140
          Top = 15
          Width = 8
          Height = 18
          Caption = 'a'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label19: TLabel
          Left = 90
          Top = 45
          Width = 40
          Height = 18
          Caption = '(  d +'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblDenTheta: TLabel
          Left = 130
          Top = 45
          Width = 37
          Height = 18
          Caption = '0.999'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object labelFoufots: TLabel
          Left = 170
          Top = 45
          Width = 6
          Height = 18
          Caption = ')'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblDenEta: TLabel
          Left = 180
          Top = 40
          Width = 28
          Height = 13
          Caption = '0.777'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
      end
      object chartSingle: TChart
        Left = 250
        Top = -3
        Width = 361
        Height = 284
        AllowPanning = pmNone
        BackWall.Brush.Style = bsClear
        Legend.Alignment = laTop
        Legend.LegendStyle = lsSeries
        Title.Font.Color = clBlack
        Title.Font.Height = -12
        Title.Font.Style = [fsBold]
        Title.Text.Strings = (
          'TChart')
        BottomAxis.Automatic = False
        BottomAxis.AutomaticMaximum = False
        BottomAxis.AutomaticMinimum = False
        BottomAxis.LabelsFormat.TextAlignment = taCenter
        BottomAxis.Logarithmic = True
        BottomAxis.Maximum = 100.000000000000000000
        BottomAxis.Minimum = 0.010000000000000000
        BottomAxis.MinorGrid.Color = clSilver
        BottomAxis.MinorGrid.Visible = True
        BottomAxis.MinorTickCount = 8
        BottomAxis.Title.Caption = 'd (h)'
        BottomAxis.TitleSize = 1
        DepthAxis.LabelsFormat.TextAlignment = taCenter
        DepthTopAxis.LabelsFormat.TextAlignment = taCenter
        LeftAxis.Automatic = False
        LeftAxis.AutomaticMaximum = False
        LeftAxis.AutomaticMinimum = False
        LeftAxis.LabelsFormat.TextAlignment = taCenter
        LeftAxis.Logarithmic = True
        LeftAxis.Maximum = 1000.000000000000000000
        LeftAxis.Minimum = 1.000000000000000000
        LeftAxis.MinorGrid.Color = clSilver
        LeftAxis.MinorGrid.Visible = True
        LeftAxis.MinorTickCount = 8
        LeftAxis.Title.Caption = 'i (mm/h)'
        LeftAxis.TitleSize = 1
        RightAxis.LabelsFormat.TextAlignment = taCenter
        TopAxis.LabelsFormat.TextAlignment = taCenter
        View3D = False
        View3DWalls = False
        Zoom.Allow = False
        Zoom.Pen.Mode = pmNotXor
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 1
        Anchors = [akLeft, akTop, akRight, akBottom]
        DefaultCanvas = 'TGDIPlusCanvas'
        ColorPaletteIndex = 13
        object seriesSingleCentral: TLineSeries
          Marks.Visible = False
          SeriesColor = clBlack
          Title = 'Rainfall intensity'
          Brush.BackColor = clDefault
          Pointer.HorizSize = 3
          Pointer.InflateMargins = True
          Pointer.Style = psDiamond
          Pointer.VertSize = 3
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesUpperSample: TLineSeries
          Active = False
          Marks.Visible = False
          SeriesColor = 10485760
          Title = 'Upper Sample'
          Brush.BackColor = clDefault
          LinePen.Style = psDash
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          Pointer.Visible = False
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesLowerSample: TLineSeries
          Active = False
          Marks.Visible = False
          SeriesColor = 10485760
          Title = 'Lower Sample'
          Brush.BackColor = clDefault
          LinePen.Style = psDash
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          Pointer.Visible = False
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesUpperConfidence: TLineSeries
          Active = False
          Marks.Visible = False
          SeriesColor = 64
          Title = 'Upper Confidence'
          Brush.BackColor = clDefault
          LinePen.Style = psDashDot
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          Pointer.Visible = False
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesLowerConfidence: TLineSeries
          Active = False
          Marks.Visible = False
          SeriesColor = 64
          Title = 'Lower Confidence'
          Brush.BackColor = clDefault
          LinePen.Style = psDashDot
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          Pointer.Visible = False
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
      end
      object edtReturnPeriod: TEdit
        Left = 130
        Top = 5
        Width = 46
        Height = 21
        TabOrder = 2
        Text = '200'
        OnChange = edtReturnPeriodChange
      end
      object btnCalculate: TButton
        Left = 185
        Top = 5
        Width = 61
        Height = 20
        Caption = 'Caclulate'
        TabOrder = 3
        OnClick = btnCalculateClick
      end
      object edtSingleAlpha: TEdit
        Left = 105
        Top = 115
        Width = 61
        Height = 21
        ReadOnly = True
        TabOrder = 4
      end
      object grpConfidence: TGroupBox
        Left = 10
        Top = 140
        Width = 231
        Height = 141
        Caption = 'Confidence interval for a'
        TabOrder = 5
        object Label23: TLabel
          Left = 10
          Top = 40
          Width = 90
          Height = 13
          Caption = 'Lower sample limit:'
        end
        object Label24: TLabel
          Left = 10
          Top = 65
          Width = 90
          Height = 13
          Caption = 'Upper sample limit:'
        end
        object Label25: TLabel
          Left = 10
          Top = 90
          Width = 109
          Height = 13
          Caption = 'Lower confidence limit:'
        end
        object Label26: TLabel
          Left = 10
          Top = 115
          Width = 109
          Height = 13
          Caption = 'Upper confidence limit:'
        end
        object edtLowerSampleLimit: TEdit
          Left = 155
          Top = 35
          Width = 61
          Height = 21
          ReadOnly = True
          TabOrder = 0
        end
        object edtUpperSampleLimit: TEdit
          Left = 155
          Top = 60
          Width = 61
          Height = 21
          ReadOnly = True
          TabOrder = 1
        end
        object edtLowerConfidenceLimit: TEdit
          Left = 155
          Top = 85
          Width = 61
          Height = 21
          ReadOnly = True
          TabOrder = 2
        end
        object edtUpperConfidenceLimit: TEdit
          Left = 155
          Top = 110
          Width = 61
          Height = 21
          ReadOnly = True
          TabOrder = 3
        end
      end
      object btnConfidenceCalculate: TButton
        Left = 165
        Top = 150
        Width = 61
        Height = 20
        Caption = 'Calculate'
        TabOrder = 7
        OnClick = btnConfidenceCalculateClick
      end
      object rgrpConfidenceInterval: TRadioGroup
        Left = 10
        Top = 285
        Width = 231
        Height = 61
        Caption = 'Confidence interval of:'
        ItemIndex = 1
        Items.Strings = (
          '90%'
          '95%'
          '99%')
        TabOrder = 6
        OnClick = rgrpConfidenceIntervalClick
      end
    end
    object tabData: TTabSheet
      Caption = 'Data'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Image: TImage
        Left = 155
        Top = 5
        Width = 291
        Height = 123
        AutoSize = True
        Stretch = True
      end
      object Label1: TLabel
        Left = 35
        Top = 5
        Width = 101
        Height = 13
        Caption = 'IDF curves equation:'
      end
      object Label2: TLabel
        Left = 30
        Top = 25
        Width = 89
        Height = 13
        Caption = 'i: Intensity (mm/h)'
      end
      object Label3: TLabel
        Left = 30
        Top = 45
        Width = 70
        Height = 13
        Caption = 'd: duration (h)'
      end
      object Label4: TLabel
        Left = 324
        Top = 192
        Width = 27
        Height = 13
        Caption = 'Eta ='
      end
      object Label5: TLabel
        Left = 315
        Top = 216
        Width = 39
        Height = 13
        Caption = 'Theta ='
      end
      object Label6: TLabel
        Left = 10
        Top = 145
        Width = 157
        Height = 13
        Caption = 'Statistical distribution function F:'
      end
      object edtEtaValue: TEdit
        Left = 356
        Top = 191
        Width = 77
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object edtThetaValue: TEdit
        Left = 356
        Top = 217
        Width = 77
        Height = 21
        ReadOnly = True
        TabOrder = 1
      end
      object edtDistributionName: TEdit
        Left = 180
        Top = 145
        Width = 251
        Height = 21
        ReadOnly = True
        TabOrder = 2
      end
      object GroupBox2: TGroupBox
        Left = 10
        Top = 170
        Width = 221
        Height = 101
        Caption = 'Statistical distribution parameters'
        TabOrder = 3
        object Label7: TLabel
          Left = 85
          Top = 25
          Width = 8
          Height = 13
          Caption = '='
        end
        object Label8: TLabel
          Left = 85
          Top = 50
          Width = 8
          Height = 13
          Caption = '='
        end
        object Label9: TLabel
          Left = 85
          Top = 75
          Width = 8
          Height = 13
          Caption = '='
        end
        object edtStatParam1Name: TEdit
          Left = 5
          Top = 20
          Width = 76
          Height = 21
          BiDiMode = bdLeftToRight
          ParentBiDiMode = False
          ReadOnly = True
          TabOrder = 0
        end
        object edtStatParam2Name: TEdit
          Left = 5
          Top = 45
          Width = 76
          Height = 21
          ReadOnly = True
          TabOrder = 1
        end
        object edtStatParam3Name: TEdit
          Left = 5
          Top = 70
          Width = 76
          Height = 21
          ReadOnly = True
          TabOrder = 2
        end
        object edtStatParam1Value: TEdit
          Left = 110
          Top = 20
          Width = 101
          Height = 21
          ReadOnly = True
          TabOrder = 3
        end
        object edtStatParam2Value: TEdit
          Left = 110
          Top = 45
          Width = 101
          Height = 21
          ReadOnly = True
          TabOrder = 4
        end
        object edtStatParam3Value: TEdit
          Left = 110
          Top = 70
          Width = 101
          Height = 21
          ReadOnly = True
          TabOrder = 5
        end
      end
      object GroupBox3: TGroupBox
        Left = 10
        Top = 275
        Width = 601
        Height = 116
        Caption = 'Unified sample statistical properties'
        TabOrder = 4
        object Label10: TLabel
          Left = 10
          Top = 20
          Width = 59
          Height = 13
          Caption = 'Mean value:'
        end
        object Label11: TLabel
          Left = 10
          Top = 50
          Width = 95
          Height = 13
          Caption = 'Standard deviation:'
        end
        object Label12: TLabel
          Left = 10
          Top = 80
          Width = 51
          Height = 13
          Caption = 'Skewness:'
        end
        object Label13: TLabel
          Left = 350
          Top = 20
          Width = 60
          Height = 13
          Caption = 'L-Moment 1:'
        end
        object Label14: TLabel
          Left = 350
          Top = 50
          Width = 60
          Height = 13
          Caption = 'L-Moment 2:'
        end
        object Label15: TLabel
          Left = 350
          Top = 80
          Width = 60
          Height = 13
          Caption = 'L-Moment 3:'
        end
        object edtMeanValue: TEdit
          Left = 110
          Top = 20
          Width = 101
          Height = 21
          ReadOnly = True
          TabOrder = 0
        end
        object edtStandardDeviation: TEdit
          Left = 110
          Top = 50
          Width = 101
          Height = 21
          ReadOnly = True
          TabOrder = 1
        end
        object edtSkewness: TEdit
          Left = 110
          Top = 80
          Width = 101
          Height = 21
          ReadOnly = True
          TabOrder = 2
        end
        object edtLMoment1: TEdit
          Left = 425
          Top = 20
          Width = 101
          Height = 21
          ReadOnly = True
          TabOrder = 3
        end
        object edtLMoment2: TEdit
          Left = 425
          Top = 50
          Width = 101
          Height = 21
          ReadOnly = True
          TabOrder = 4
        end
        object edtLMoment3: TEdit
          Left = 425
          Top = 80
          Width = 101
          Height = 21
          ReadOnly = True
          TabOrder = 5
        end
      end
    end
    object tabDistribution: TTabSheet
      Caption = 'Distribution plot'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        613
        393)
      object Label16: TLabel
        Left = 490
        Top = 5
        Width = 98
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Displayed durations:'
      end
      object Chart: TChart
        Left = 5
        Top = 5
        Width = 481
        Height = 386
        AllowPanning = pmNone
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
        BottomAxis.LabelsAngle = 90
        BottomAxis.LabelsFormat.TextAlignment = taCenter
        BottomAxis.LabelStyle = talMark
        BottomAxis.Maximum = 10.000000000000000000
        BottomAxis.Minimum = -10.000000000000000000
        BottomAxis.MinorTickCount = 2
        BottomAxis.MinorTickLength = 0
        BottomAxis.MinorTicks.Visible = False
        BottomAxis.Visible = False
        DepthAxis.LabelsFormat.TextAlignment = taCenter
        DepthTopAxis.LabelsFormat.TextAlignment = taCenter
        LeftAxis.Grid.Color = clBlack
        LeftAxis.LabelsFormat.TextAlignment = taCenter
        LeftAxis.LabelsSize = 20
        LeftAxis.MinorGrid.Color = clGray
        LeftAxis.MinorGrid.Style = psDashDotDot
        LeftAxis.MinorGrid.SmallDots = True
        LeftAxis.MinorGrid.Visible = True
        LeftAxis.MinorTickCount = 1
        LeftAxis.Title.Font.Height = -9
        LeftAxis.TitleSize = 2
        RightAxis.LabelsFormat.TextAlignment = taCenter
        TopAxis.Automatic = False
        TopAxis.AutomaticMaximum = False
        TopAxis.AutomaticMinimum = False
        TopAxis.Grid.Color = clBlack
        TopAxis.LabelsAngle = 90
        TopAxis.LabelsFormat.TextAlignment = taCenter
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
        Zoom.Pen.Mode = pmNotXor
        Color = clWhite
        TabOrder = 0
        Anchors = [akLeft, akTop, akRight, akBottom]
        DefaultCanvas = 'TGDIPlusCanvas'
        ColorPaletteIndex = 13
        object GridPoint: TPointSeries
          HorizAxis = aTopAxis
          Marks.Frame.Visible = False
          Marks.Visible = True
          Marks.Arrow.Visible = False
          Marks.BackColor = clWhite
          Marks.Callout.Arrow.Visible = False
          Marks.Callout.Length = 5
          Marks.Clip = True
          Marks.Color = clWhite
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
          Marks.Visible = False
          SeriesColor = clBlack
          Title = 'Weibull points'
          ClickableLine = False
          Pointer.Brush.Color = clOlive
          Pointer.InflateMargins = True
          Pointer.Pen.Color = clLime
          Pointer.Pen.Visible = False
          Pointer.Style = psCircle
          Pointer.Visible = True
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object seriesDistribution: TLineSeries
          Active = False
          Marks.Visible = False
          SeriesColor = 64
          ShowInLegend = False
          Title = 'Distribution function'
          Brush.BackColor = clDefault
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          Pointer.Visible = False
          XValues.Name = 'X'
          XValues.Order = loNone
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
      end
      object lstDistributionLines: TListBox
        Left = 490
        Top = 25
        Width = 121
        Height = 241
        Anchors = [akTop, akRight]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
        OnClick = lstDistributionLinesClick
      end
      object chkLogY: TCheckBox
        Left = 496
        Top = 376
        Width = 97
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'Logarithmic Y'
        TabOrder = 2
        OnClick = chkMulti1000Click
      end
    end
  end
  object mnuMainMenu: TMainMenu
    Left = 650
    Top = 405
    object mnuFile: TMenuItem
      Caption = 'File'
      object mnuPrintMulti: TMenuItem
        Caption = 'Print multi chart'
        OnClick = mnuPrintMultiClick
      end
      object mnuPrintDistribution: TMenuItem
        Caption = 'Print distribution plot'
        OnClick = mnuPrintDistributionClick
      end
      object mnuPrintSingle: TMenuItem
        Caption = 'Print single curve'
        OnClick = mnuPrintSingleClick
      end
      object mnuPrinterSetup: TMenuItem
        Caption = 'Printer setup'
        OnClick = mnuPrinterSetupClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuSingleIDFEvaluation: TMenuItem
        Caption = 'Single IDF curve evaluation'
        OnClick = mnuSingleIDFEvaluationClick
      end
      object N3: TMenuItem
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
      object mnuCopyMulti: TMenuItem
        Caption = 'Copy multi curves to clipboard'
        OnClick = mnuCopyMultiClick
      end
      object mnuCopyDistribution: TMenuItem
        Caption = 'Copy distribution plot to clipboard'
        OnClick = mnuCopyDistributionClick
      end
      object mnuCopySingle: TMenuItem
        Caption = 'Copy single curve to clipboard'
        OnClick = mnuCopySingleClick
      end
      object mnuCopyGrid: TMenuItem
        Caption = 'Copy grid to clipboard'
        OnClick = mnuCopyGridClick
      end
    end
    object mnuOptions: TMenuItem
      Caption = 'Options'
      object mnuPaper: TMenuItem
        Caption = 'Paper (distribution plot)'
        object mnuNormalPaper: TMenuItem
          Tag = 601
          Caption = 'Normal'
          GroupIndex = 205
          RadioItem = True
          OnClick = mnuNormalPaperClick
        end
        object mnuGumbelPaper: TMenuItem
          Tag = 602
          Caption = 'Gumbel'
          GroupIndex = 205
          RadioItem = True
          OnClick = mnuNormalPaperClick
        end
        object mnuGEVMaxPaper: TMenuItem
          Tag = 603
          Caption = 'GEV-Max (with specified k)'
          Checked = True
          GroupIndex = 205
          RadioItem = True
          OnClick = mnuNormalPaperClick
        end
        object mnuLogLogPaper: TMenuItem
          Tag = 604
          Caption = 'Log - Log'
          GroupIndex = 205
          RadioItem = True
          OnClick = mnuNormalPaperClick
        end
      end
      object mnuTimeResolution: TMenuItem
        Caption = 'Consider time resolution effect'
        Checked = True
        OnClick = mnuTimeResolutionClick
      end
      object Convertreturnperiodsforabovethresholdtimeseries1: TMenuItem
        Caption = 'Convert return periods for above threshold time series'
        OnClick = Convertreturnperiodsforabovethresholdtimeseries1Click
      end
    end
    object mnuDistribution: TMenuItem
      Caption = 'Distribution'
      object mnuExponential: TMenuItem
        Tag = 500
        Caption = 'Exponential'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuLExponential: TMenuItem
        Tag = 501
        Caption = 'Exponential (L-Moments)'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuGamma: TMenuItem
        Tag = 502
        Caption = 'Gamma'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuLogPearsonIII: TMenuItem
        Tag = 503
        Caption = 'LogPearsonIII'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuGumbel: TMenuItem
        Tag = 504
        Caption = 'Gumbel'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuLGumbel: TMenuItem
        Tag = 505
        Caption = 'Gumbel (L-Moments)'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuEV2Max: TMenuItem
        Tag = 506
        Caption = 'EV2-Max'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuLEV2Max: TMenuItem
        Tag = 507
        Caption = 'EV2-Max (L-Moments)'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuGEVMax: TMenuItem
        Tag = 508
        Caption = 'GEV-Max'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuLGEVMax: TMenuItem
        Tag = 509
        Caption = 'GEV-Max (L-Moments)'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuGEVMaxK: TMenuItem
        Tag = 510
        Caption = 'GEV-Max (kappa specified)'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuLGEVMaxK: TMenuItem
        Tag = 511
        Caption = 'GEV-Max (kappa specified, L-Moments)'
        Checked = True
        Default = True
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuPareto: TMenuItem
        Tag = 512
        Caption = 'Pareto'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object mnuLPareto: TMenuItem
        Tag = 513
        Caption = 'Pareto (L-Moments)'
        GroupIndex = 200
        RadioItem = True
        OnClick = mnuGammaClick
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 205
      end
      object mnuSpecifyKappa: TMenuItem
        Caption = 'Specify GEV kappa (shape)'
        GroupIndex = 205
        OnClick = mnuSpecifyKappaClick
      end
    end
  end
  object PrintDialog: TPrintDialog
    Left = 554
    Top = 299
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 554
    Top = 324
  end
end
