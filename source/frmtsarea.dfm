object FrmArealIntegration: TFrmArealIntegration
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 
    'Surface rainfall estimation - spatial integration of time series' +
    '.'
  ClientHeight = 488
  ClientWidth = 655
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grdParameters: TStringGrid
    Left = 4
    Top = 5
    Width = 646
    Height = 145
    Color = clWhite
    DefaultColWidth = 80
    DefaultRowHeight = 18
    FixedColor = 14540253
    RowCount = 6
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    PopupMenu = PopupMenu
    TabOrder = 0
    ColWidths = (
      135
      80
      80
      80
      80)
  end
  object btnCalculate: TButton
    Left = 4
    Top = 456
    Width = 141
    Height = 25
    Caption = 'Calculate surface rainfall'
    TabOrder = 1
    OnClick = btnCalculateClick
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 340
    Width = 269
    Height = 105
    Caption = 'Estimation of weights from station coordinates'
    TabOrder = 2
    object btnCalcStationMean: TButton
      Left = 8
      Top = 32
      Width = 146
      Height = 25
      Caption = 'Estimate center'
      TabOrder = 0
      OnClick = btnCalcStationMeanClick
    end
    object edtMeanX: TLabeledEdit
      Left = 160
      Top = 31
      Width = 97
      Height = 21
      EditLabel.Width = 65
      EditLabel.Height = 13
      EditLabel.Caption = 'Center X (m):'
      TabOrder = 1
      Text = '0'
    end
    object edtMeanY: TLabeledEdit
      Left = 160
      Top = 73
      Width = 97
      Height = 21
      EditLabel.Width = 65
      EditLabel.Height = 13
      EditLabel.Caption = 'Center Y (m):'
      TabOrder = 2
      Text = '0'
    end
    object btnCalcPosWeights: TButton
      Left = 8
      Top = 72
      Width = 146
      Height = 25
      Caption = 'Estimate weights'
      TabOrder = 3
      OnClick = btnCalcPosWeightsClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 156
    Width = 269
    Height = 178
    Caption = 'Altitude gradient'
    TabOrder = 3
    object edtMeanAltitude: TLabeledEdit
      Left = 8
      Top = 32
      Width = 97
      Height = 21
      EditLabel.Width = 116
      EditLabel.Height = 13
      EditLabel.Caption = 'Mean basin altitude (m):'
      EditLabel.Font.Charset = DEFAULT_CHARSET
      EditLabel.Font.Color = clWindowText
      EditLabel.Font.Height = -11
      EditLabel.Font.Name = 'Tahoma'
      EditLabel.Font.Style = [fsItalic]
      EditLabel.ParentFont = False
      TabOrder = 0
      Text = '0'
    end
    object btnCalcGradients: TButton
      Left = 160
      Top = 20
      Width = 97
      Height = 25
      Caption = 'Calculate gradient'
      TabOrder = 1
      OnClick = btnCalcGradientsClick
    end
    object edtMeanStationAltitude: TLabeledEdit
      Left = 8
      Top = 69
      Width = 97
      Height = 21
      EditLabel.Width = 131
      EditLabel.Height = 13
      EditLabel.Caption = 'Mean Stations Altitude (m):'
      TabOrder = 2
      Text = '0'
    end
    object edtGradient: TLabeledEdit
      Left = 160
      Top = 69
      Width = 97
      Height = 21
      EditLabel.Width = 84
      EditLabel.Height = 13
      EditLabel.Caption = 'Gradient (mm/m):'
      TabOrder = 3
      Text = '0'
    end
    object edtAltitudeFactor: TLabeledEdit
      Left = 8
      Top = 146
      Width = 97
      Height = 21
      Hint = 
        'You can alter the value of the altitude reduction factor. Then t' +
        'his value will be used to the calculations...'
      EditLabel.Width = 142
      EditLabel.Height = 13
      EditLabel.Caption = 'Altitude reduction factor:'
      EditLabel.Font.Charset = DEFAULT_CHARSET
      EditLabel.Font.Color = clWindowText
      EditLabel.Font.Height = -11
      EditLabel.Font.Name = 'Tahoma'
      EditLabel.Font.Style = [fsBold]
      EditLabel.ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = '1'
    end
    object edtCorrelation: TLabeledEdit
      Left = 8
      Top = 108
      Width = 97
      Height = 21
      EditLabel.Width = 88
      EditLabel.Height = 13
      EditLabel.Caption = 'Correlation (-1..1)'
      TabOrder = 5
      Text = '0'
    end
    object edtConstantTerm: TLabeledEdit
      Left = 160
      Top = 112
      Width = 97
      Height = 21
      EditLabel.Width = 96
      EditLabel.Height = 13
      EditLabel.Caption = 'Constant term (mm)'
      TabOrder = 6
      Text = '0'
    end
    object chkAltitudeReduce: TCheckBox
      Left = 160
      Top = 151
      Width = 89
      Height = 16
      Hint = 'Check to apply reduction factor to final calculations...'
      Caption = 'Apply factor'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 7
    end
  end
  object Chart: TChart
    Left = 277
    Top = 162
    Width = 373
    Height = 283
    Legend.Alignment = laTop
    Legend.VertSpacing = -2
    MarginBottom = 0
    MarginLeft = 0
    MarginRight = 0
    MarginTop = 0
    Title.Text.Strings = (
      '')
    BottomAxis.LabelsFormat.TextAlignment = taCenter
    BottomAxis.Title.Caption = 'Rainfall (mm)'
    DepthAxis.LabelsFormat.TextAlignment = taCenter
    DepthTopAxis.LabelsFormat.TextAlignment = taCenter
    LeftAxis.LabelsFormat.TextAlignment = taCenter
    LeftAxis.Title.Caption = 'Altitude (m)'
    RightAxis.LabelsFormat.TextAlignment = taCenter
    TopAxis.LabelsFormat.TextAlignment = taCenter
    View3D = False
    Zoom.Pen.Color = clGray
    Zoom.Pen.Mode = pmNotXor
    Color = clWhite
    TabOrder = 4
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object seriesMeasures: TPointSeries
      Marks.Visible = False
      SeriesColor = clBlue
      ShowInLegend = False
      Title = 'Points'
      ClickableLine = False
      Pointer.InflateMargins = True
      Pointer.Style = psDiamond
      Pointer.Visible = True
      XValues.Name = 'X'
      XValues.Order = loNone
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object seriesSlope: TLineSeries
      Marks.Visible = False
      SeriesColor = clMaroon
      ShowInLegend = False
      Title = 'Slope'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loNone
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object seriesMeanBasinAltitude: TLineSeries
      Marks.Shadow.Color = 8487297
      Marks.Visible = False
      Title = 'Mean basin altitude'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loNone
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object seriesMeanStationAltitude: TLineSeries
      Marks.Visible = False
      Title = 'Mean station altitude'
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
  object btnCopyChart: TButton
    Left = 277
    Top = 456
    Width = 124
    Height = 25
    Caption = 'Copy chart to clipboard'
    TabOrder = 5
    OnClick = btnCopyChartClick
  end
  object PopupMenu: TPopupMenu
    Left = 528
    Top = 453
    object Copygridtoclipboard1: TMenuItem
      Caption = 'Copy grid values to clipboard'
      OnClick = Copygridtoclipboard1Click
    end
    object Pastetogrid1: TMenuItem
      Caption = 'Paste to grid'
      OnClick = Pastetogrid1Click
    end
  end
end
