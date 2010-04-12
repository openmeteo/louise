object FrmHydrometry: TFrmHydrometry
  Left = 44
  Top = 47
  BorderStyle = bsSingle
  Caption = 'Hydrometry calculations'
  ClientHeight = 642
  ClientWidth = 790
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tbsSections: TTabSet
    Left = 176
    Top = 353
    Width = 537
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    SoftTop = True
    Style = tsModernPopout
    Tabs.Strings = (
      'Section 1'
      'Section 2'
      'Section 3'
      'Section 4'
      'Section 5'
      'Section 6'
      'Section 7'
      'Section 8'
      'Section 9'
      'Section 10')
    TabIndex = 0
    OnClick = tbsSectionsClick
  end
  object pnlSections: TPanel
    Left = 176
    Top = 6
    Width = 611
    Height = 347
    TabOrder = 1
    object chartSection: TChart
      Left = 1
      Top = 1
      Width = 609
      Height = 345
      Legend.Visible = False
      Title.Text.Strings = (
        'TChart')
      Title.Visible = False
      BottomAxis.Automatic = False
      BottomAxis.AutomaticMaximum = False
      BottomAxis.Grid.Color = 16744576
      BottomAxis.Grid.Style = psSolid
      BottomAxis.Maximum = 100.000000000000000000
      LeftAxis.Automatic = False
      LeftAxis.AutomaticMaximum = False
      LeftAxis.AutomaticMinimum = False
      LeftAxis.Grid.Color = 16744576
      LeftAxis.Grid.Style = psSolid
      LeftAxis.Maximum = 100.000000000000000000
      Shadow.Visible = False
      View3D = False
      View3DWalls = False
      Zoom.Pen.Color = clGray
      Align = alClient
      Color = clWhite
      TabOrder = 0
      object seriesGround: TLineSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        SeriesColor = clGreen
        Title = 'seriesGround'
        LinePen.Width = 2
        Pointer.HorizSize = 1
        Pointer.InflateMargins = True
        Pointer.Style = psDiamond
        Pointer.VertSize = 1
        Pointer.Visible = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
      object seriesCutSection: TLineSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        SeriesColor = clBlue
        Title = 'seriesCutSection'
        LinePen.Width = 2
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.Visible = False
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
      object seriesMeasurePoints: TPointSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        Title = 'seriesMeasurePoints'
        ClickableLine = False
        Pointer.Brush.Color = 8404992
        Pointer.HorizSize = 5
        Pointer.InflateMargins = True
        Pointer.Style = psCircle
        Pointer.VertSize = 5
        Pointer.Visible = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
      object seriesSlices: TLineSeries
        ColorEachLine = False
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        SeriesColor = 12615935
        Title = 'seriesSlices'
        Dark3D = False
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.Visible = False
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
      object seriesSectionMeasurements: TPointSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        Title = 'seriesSectionMeasurements'
        ClickableLine = False
        Pointer.InflateMargins = True
        Pointer.Style = psDiagCross
        Pointer.Visible = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
  end
  object btnEditSection: TButton
    Left = 712
    Top = 348
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 2
    OnClick = btnEditSectionClick
  end
  object grpSessions: TGroupBox
    Left = 0
    Top = 3
    Width = 170
    Height = 371
    Caption = 'Sessions'
    TabOrder = 3
    object lstSessions: TListBox
      Left = 3
      Top = 20
      Width = 164
      Height = 314
      ItemHeight = 13
      TabOrder = 0
      OnClick = lstSessionsClick
    end
    object btnAddSession: TButton
      Left = 3
      Top = 340
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 1
      OnClick = btnAddSessionClick
    end
    object btnRemove: TButton
      Left = 92
      Top = 340
      Width = 75
      Height = 25
      Caption = 'Remove'
      TabOrder = 2
      OnClick = btnRemoveClick
    end
  end
  object grpProperties: TGroupBox
    Left = 0
    Top = 374
    Width = 217
    Height = 187
    Caption = 'Session properties'
    TabOrder = 4
    object lblStage: TLabel
      Left = 16
      Top = 64
      Width = 51
      Height = 13
      Caption = 'Stage (m):'
    end
    object lblDate: TLabel
      Left = 16
      Top = 32
      Width = 27
      Height = 13
      Caption = 'Date:'
    end
    object edtStage: TEdit
      Left = 119
      Top = 59
      Width = 87
      Height = 21
      TabOrder = 0
    end
    object edtDate: TMaskEdit
      Left = 105
      Top = 32
      Width = 101
      Height = 21
      EditMask = '!0000/99/99 !90:00;1;_'
      MaxLength = 16
      TabOrder = 1
      Text = '    /  /     :  '
    end
    object btnChangeSessionSection: TButton
      Left = 103
      Top = 118
      Width = 103
      Height = 25
      Caption = 'Change XSection'
      TabOrder = 2
      OnClick = btnChangeSessionSectionClick
    end
    object btnSaveStageDateValidate: TButton
      Left = 16
      Top = 149
      Width = 190
      Height = 25
      Caption = 'Save Stage, Date and validate'
      TabOrder = 3
      OnClick = btnSaveStageDateValidateClick
    end
    object chkFromCut: TCheckBox
      Left = 16
      Top = 95
      Width = 190
      Height = 17
      Caption = 'X is measured from water section'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object grpSlices: TGroupBox
    Left = 223
    Top = 374
    Width = 250
    Height = 265
    Caption = 'Slices'
    TabOrder = 5
    object lblSlicePosition: TLabel
      Left = 13
      Top = 174
      Width = 69
      Height = 13
      Caption = 'X Position (m):'
    end
    object lstSlices: TListBox
      Left = 3
      Top = 16
      Width = 238
      Height = 121
      ItemHeight = 13
      TabOrder = 0
      OnClick = lstSlicesClick
    end
    object btnAddSlice: TButton
      Left = 3
      Top = 143
      Width = 70
      Height = 25
      Caption = 'Add'
      TabOrder = 1
      OnClick = btnAddSliceClick
    end
    object btnRemoveSlice: TButton
      Left = 79
      Top = 143
      Width = 69
      Height = 25
      Caption = 'Remove'
      TabOrder = 2
      OnClick = btnRemoveSliceClick
    end
    object btnChangeSlice: TButton
      Left = 167
      Top = 143
      Width = 74
      Height = 25
      Caption = 'Change'
      TabOrder = 3
      OnClick = btnChangeSliceClick
    end
    object edtSlicePosition: TEdit
      Left = 104
      Top = 174
      Width = 73
      Height = 21
      TabOrder = 4
    end
    object cmbSliceMode: TComboBox
      Left = 3
      Top = 201
      Width = 238
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 5
      Text = '1 measurement at 0.6xdepth'
      Items.Strings = (
        '1 measurement at 0.6xdepth'
        '2 measurements at 0.2 and 0.8'
        '3 measurements at 0.2, 0.6, 0.8'
        'n measur. turbulent flow, log curve'
        'n measur. laminar flow, parab. curve')
    end
    object btnValidateSlices: TButton
      Left = 3
      Top = 228
      Width = 166
      Height = 25
      Caption = 'Validate and draw slices'
      TabOrder = 6
      OnClick = btnValidateSlicesClick
    end
  end
  object rgrpOutputTimeseries: TRadioGroup
    Left = 0
    Top = 567
    Width = 217
    Height = 71
    Caption = 'Output time series'
    ItemIndex = 0
    Items.Strings = (
      'H (m) - Q (m3/s)'
      'H (m) - Vm (m/s)')
    TabOrder = 6
    OnClick = rgrpOutputTimeseriesClick
  end
  object btnCalculateTimeseries: TButton
    Left = 119
    Top = 602
    Width = 87
    Height = 25
    Caption = 'Calculate'
    TabOrder = 7
    OnClick = btnCalculateTimeseriesClick
  end
  object grpMeasurements: TGroupBox
    Left = 479
    Top = 374
    Width = 308
    Height = 265
    Caption = 'Measurements'
    TabOrder = 8
    object grdMeasurements: TStringGrid
      Left = 3
      Top = 16
      Width = 182
      Height = 169
      ColCount = 2
      DefaultColWidth = 80
      DefaultRowHeight = 18
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
      TabOrder = 0
      ColWidths = (
        80
        74)
    end
    object chartMeasurements: TChart
      Left = 184
      Top = 16
      Width = 121
      Height = 170
      Legend.Visible = False
      Title.Text.Strings = (
        'TChart')
      Title.Visible = False
      BottomAxis.Automatic = False
      BottomAxis.AutomaticMinimum = False
      BottomAxis.LabelsFont.Height = -9
      BottomAxis.Minimum = 0.100000000000000000
      BottomAxis.MinorGrid.SmallDots = True
      BottomAxis.MinorGrid.SmallSpace = 1
      BottomAxis.MinorTickCount = 8
      LeftAxis.Automatic = False
      LeftAxis.AutomaticMaximum = False
      LeftAxis.AutomaticMinimum = False
      LeftAxis.ExactDateTime = False
      LeftAxis.Increment = 0.050000000000000000
      LeftAxis.Inverted = True
      LeftAxis.LabelsFont.Height = -9
      LeftAxis.LabelsFont.Shadow.Visible = False
      LeftAxis.Maximum = 1.000000000000000000
      LeftAxis.Title.Visible = False
      View3D = False
      View3DWalls = False
      Zoom.Pen.Color = clGray
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Color = clWhite
      TabOrder = 1
      object seriesVelocityCurve: TLineSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        SeriesColor = clNavy
        Title = 'seriesVelocityCurve'
        LinePen.Width = 2
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.Visible = False
        XValues.Name = 'X'
        XValues.Order = loNone
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
      object seriesVelocitiesMeasurements: TPointSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        Title = 'seriesVelocitiesMeasurements'
        ClickableLine = False
        Pointer.InflateMargins = True
        Pointer.Style = psDiagCross
        Pointer.Visible = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
    object btnSaveMeasurements: TButton
      Left = 3
      Top = 228
      Width = 110
      Height = 25
      Caption = 'Update - save'
      TabOrder = 2
      OnClick = btnSaveMeasurementsClick
    end
    object grpMeasurementsNumber: TGroupBox
      Left = 159
      Top = 191
      Width = 146
      Height = 62
      Caption = 'Meas. number'
      Enabled = False
      TabOrder = 3
      object edtMeasurementsNum: TEdit
        Left = 3
        Top = 14
        Width = 41
        Height = 21
        TabOrder = 0
        Text = '3'
      end
      object spnMeasurementsNum: TUpDown
        Left = 44
        Top = 14
        Width = 16
        Height = 21
        Associate = edtMeasurementsNum
        Min = 3
        Orientation = udHorizontal
        Position = 3
        TabOrder = 1
        OnClick = spnMeasurementsNumClick
      end
      object chkActualDepth: TCheckBox
        Left = 3
        Top = 39
        Width = 97
        Height = 17
        Caption = 'Actual Depth'
        TabOrder = 2
        OnClick = chkActualDepthClick
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 548
    Top = 41
    object mnuFile: TMenuItem
      Caption = 'File'
      object mnuNew: TMenuItem
        Caption = 'New'
        OnClick = mnuNewClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuOpen: TMenuItem
        Caption = 'Open ...'
        ShortCut = 16463
        OnClick = mnuOpenClick
      end
      object mnuSave: TMenuItem
        Caption = 'Save...'
        ShortCut = 16467
        OnClick = mnuSaveClick
      end
      object mnuSaveAs: TMenuItem
        Caption = 'Save as ...'
        ShortCut = 123
        OnClick = mnuSaveAsClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnuPrintSection: TMenuItem
        Caption = 'Print X-Section...'
        ShortCut = 16464
        OnClick = mnuPrintSectionClick
      end
      object mnuPrintVelocityProfile: TMenuItem
        Caption = 'Print velocity profile...'
        OnClick = mnuPrintVelocityProfileClick
      end
      object mnuPrinterSetup: TMenuItem
        Caption = 'Printer setup...'
        OnClick = mnuPrinterSetupClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exitwithoutcalculations1: TMenuItem
        Caption = 'Exit without calculations'
        ShortCut = 32883
        OnClick = Exitwithoutcalculations1Click
      end
    end
    object mnuEdit: TMenuItem
      Caption = 'Edit'
      object mnuCopyChart: TMenuItem
        Caption = 'Copy X-Section'
        ShortCut = 16451
        OnClick = mnuCopyChartClick
      end
      object mnuCopyvelocityprofile: TMenuItem
        Caption = 'Copy velocity profile'
        OnClick = mnuCopyvelocityprofileClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnuCopymeasurementstable: TMenuItem
        Caption = 'Copy measurements table'
        OnClick = mnuCopymeasurementstableClick
      end
    end
    object mnuProcess: TMenuItem
      Caption = 'Process'
      object mnuCalculate: TMenuItem
        Caption = 'Calculate, creating time series, then close'
        OnClick = btnCalculateTimeseriesClick
      end
      object mnuCalculateOnly: TMenuItem
        Caption = 'Run without output, for validation'
        OnClick = btnCalculateTimeseriesClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'hdm'
    Filter = 'Hydrometry data files (*.hdm)|*.hdm|All files (*.*)|*.*'
    Title = 'Open file...'
    Left = 609
    Top = 40
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'hdm'
    Filter = 'Hydrometry data files (*.hdm)|*.hdm|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save as...'
    Left = 665
    Top = 40
  end
  object PrintDialog: TPrintDialog
    Left = 552
    Top = 104
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 608
    Top = 104
  end
end
