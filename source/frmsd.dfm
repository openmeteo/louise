object FrmStageDischarge: TFrmStageDischarge
  Left = 199
  Top = 104
  HelpContext = 300
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderStyle = bsDialog
  Caption = 'Interpolations (Stage-Discharge)'
  ClientHeight = 500
  ClientWidth = 689
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  HelpFile = 'Interpolations.hlp'
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = IFormCreate
  OnDestroy = IFormDestroy
  OnHide = IFormHide
  OnShow = IFormShow
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 195
    Top = 160
    Width = 14
    Height = 13
    Caption = 'log'
  end
  object Label4: TLabel
    Left = 186
    Top = 195
    Width = 26
    Height = 13
    Caption = 'YAxis'
  end
  object pnlLeakage: TPanel
    Left = 215
    Top = 415
    Width = 291
    Height = 81
    BevelOuter = bvNone
    TabOrder = 17
    Visible = False
    object rgrpLeakage: TRadioGroup
      Left = 5
      Top = 0
      Width = 261
      Height = 81
      Caption = 'Leakage Calculations'
      ItemIndex = 0
      Items.Strings = (
        'Calculate instantaneous values'
        'Calculate inst. montly averages')
      TabOrder = 0
    end
  end
  object pnlVolume: TPanel
    Left = 215
    Top = 415
    Width = 291
    Height = 81
    BevelOuter = bvNone
    TabOrder = 16
    Visible = False
    object rgrpVolume: TRadioGroup
      Left = 5
      Top = 0
      Width = 266
      Height = 81
      Caption = 'Area, Volume Calculations'
      ItemIndex = 0
      Items.Strings = (
        'Calculate instantaneous values'
        'Calculate inst., monthly averages')
      TabOrder = 0
    end
  end
  object pnlStageDischarge: TPanel
    Left = 220
    Top = 415
    Width = 281
    Height = 81
    BevelOuter = bvNone
    TabOrder = 14
    object rgrpStageDischarge: TRadioGroup
      Left = 0
      Top = 0
      Width = 266
      Height = 81
      Caption = 'Stage-Discharge Calculations'
      ItemIndex = 0
      Items.Strings = (
        'Calculate instantaneous values'
        'Calculate inst., monthly && yearly cumulative values'
        'Calculate inst., monthly && yearly averages')
      TabOrder = 0
    end
  end
  object grpOptions: TGroupBox
    Left = 5
    Top = 375
    Width = 206
    Height = 121
    Caption = 'Options'
    TabOrder = 13
  end
  object grpLoadCurves: TGroupBox
    Left = 100
    Top = 293
    Width = 86
    Height = 66
    Caption = 'Load Curves'
    TabOrder = 5
    Visible = False
    object btnLoadFromDatabase: TButton
      Left = 5
      Top = 40
      Width = 76
      Height = 21
      Caption = 'from DB'
      Enabled = False
      TabOrder = 0
      OnClick = btnLoadFromDatabaseClick
    end
  end
  object Chart1: TChart
    Left = 218
    Top = -7
    Width = 471
    Height = 416
    AllowPanning = pmNone
    BackWall.Brush.Style = bsClear
    Legend.Alignment = laTop
    Legend.LegendStyle = lsSeries
    PrintProportional = False
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.Automatic = False
    BottomAxis.AutomaticMaximum = False
    BottomAxis.AutomaticMinimum = False
    BottomAxis.LabelsFormat.TextAlignment = taCenter
    BottomAxis.Logarithmic = True
    BottomAxis.MinorGrid.Color = 14737346
    BottomAxis.MinorGrid.Visible = True
    BottomAxis.MinorTickCount = 4
    BottomAxis.MinorTicks.Color = 14802881
    BottomAxis.Ticks.Color = clSilver
    BottomAxis.TicksInner.Color = clSilver
    BottomAxis.Title.Caption = 'Discharge (m^3/s)'
    DepthAxis.LabelsFormat.TextAlignment = taCenter
    DepthTopAxis.LabelsFormat.TextAlignment = taCenter
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMaximum = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.LabelsFormat.TextAlignment = taCenter
    LeftAxis.Logarithmic = True
    LeftAxis.MinorGrid.Color = 15263961
    LeftAxis.MinorGrid.Visible = True
    LeftAxis.MinorTickCount = 4
    LeftAxis.MinorTicks.Color = 15132870
    LeftAxis.Ticks.Color = clSilver
    LeftAxis.TicksInner.Color = clSilver
    LeftAxis.Title.Caption = 'Stage (m)'
    RightAxis.LabelsFormat.TextAlignment = taCenter
    RightAxis.Logarithmic = True
    RightAxis.Visible = False
    TopAxis.LabelsFormat.TextAlignment = taCenter
    TopAxis.Logarithmic = True
    TopAxis.Visible = False
    View3D = False
    View3DWalls = False
    Zoom.Allow = False
    Zoom.Pen.Mode = pmNotXor
    Color = clWhite
    TabOrder = 0
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
  end
  object chkLogYAxis: TCheckBox
    Left = 195
    Top = 175
    Width = 16
    Height = 21
    Alignment = taLeftJustify
    TabOrder = 1
    OnClick = chkLogYAxisClick
  end
  object chkLogXAxis: TCheckBox
    Left = 620
    Top = 425
    Width = 66
    Height = 16
    Caption = 'log XAxis'
    TabOrder = 2
    OnClick = chkLogXAxisClick
  end
  object btnShowMeasurements: TButton
    Left = 8
    Top = 215
    Width = 113
    Height = 26
    Caption = 'Display H-Q Points'
    TabOrder = 3
    OnClick = btnShowMeasurementsClick
  end
  object btnCalculate: TButton
    Left = 515
    Top = 468
    Width = 81
    Height = 26
    Caption = 'Calculate '
    TabOrder = 4
    OnClick = btnCalculateClick
  end
  object chkH1Correction: TCheckBox
    Left = 10
    Top = 393
    Width = 196
    Height = 21
    Caption = 'Make H1 Correction (from Accu. TS)'
    TabOrder = 6
  end
  object chkMergeData: TCheckBox
    Left = 10
    Top = 413
    Width = 191
    Height = 21
    Caption = 'Merge Sparse + Dense Stage Data'
    TabOrder = 7
  end
  object chkH2Correction: TCheckBox
    Left = 10
    Top = 434
    Width = 196
    Height = 21
    Caption = 'Make H2 Correction (from H-Q)'
    TabOrder = 8
  end
  object chkStout: TCheckBox
    Left = 10
    Top = 454
    Width = 151
    Height = 21
    Caption = 'Make Stout Correction'
    TabOrder = 9
  end
  object chkCalculateDischarge: TCheckBox
    Left = 10
    Top = 475
    Width = 136
    Height = 16
    Caption = 'Calculate Discharge'
    Checked = True
    State = cbChecked
    TabOrder = 10
  end
  object grpSaveCurves: TGroupBox
    Left = 13
    Top = 293
    Width = 86
    Height = 66
    Caption = 'Save Curves'
    TabOrder = 11
    Visible = False
    object btnSaveToDatabase: TButton
      Left = 5
      Top = 40
      Width = 76
      Height = 21
      Caption = 'to DB'
      Enabled = False
      TabOrder = 0
      OnClick = btnSaveToDatabaseClick
    end
  end
  object rgrpTypeOfCalculations: TRadioGroup
    Left = 8
    Top = 8
    Width = 186
    Height = 121
    Caption = 'Type of Calculations'
    ItemIndex = 0
    Items.Strings = (
      'Stage - Discharge'
      'Stage - Area, Volume'
      'Stage - Leakage'
      'Spillway Stage - Discharge'
      'Discharge - Sediment Discharge')
    TabOrder = 12
    OnClick = rgrpTypeOfCalculationsClick
  end
  object btnCancel: TButton
    Left = 614
    Top = 468
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 18
  end
  object btnEdit: TButton
    Left = 8
    Top = 250
    Width = 113
    Height = 26
    Caption = 'Edit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 19
    OnClick = btnEditClick
  end
  object btnLoad: TButton
    Left = 8
    Top = 146
    Width = 113
    Height = 26
    Hint = 'Load curves from file'
    Caption = 'Open curves from File'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 20
    OnClick = btnLoadClick
  end
  object btnSave: TButton
    Left = 8
    Top = 180
    Width = 113
    Height = 26
    Hint = 'Load curves from file'
    Caption = 'Write curves to File'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 21
    OnClick = btnSaveClick
  end
  object pnlSpillway: TPanel
    Left = 220
    Top = 415
    Width = 291
    Height = 86
    BevelOuter = bvNone
    TabOrder = 15
    Visible = False
    object rgrpSpillway: TRadioGroup
      Left = 0
      Top = 0
      Width = 266
      Height = 81
      Caption = 'Spillway Discharge Calculations'
      ItemIndex = 0
      Items.Strings = (
        'Calculate Instantaneous values'
        'Calculate inst., monthly && yearly cumulative values')
      TabOrder = 0
    end
  end
  object OpenDialog: TOpenDialog
    Left = 520
    Top = 424
  end
  object SaveDialog: TSaveDialog
    Left = 560
    Top = 424
  end
end
