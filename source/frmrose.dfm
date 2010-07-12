object FrmRoseDiagram: TFrmRoseDiagram
  Left = 0
  Top = 0
  Caption = 'Rose diagram'
  ClientHeight = 528
  ClientWidth = 684
  Color = clBtnFace
  Constraints.MinHeight = 460
  Constraints.MinWidth = 630
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    684
    528)
  PixelsPerInch = 96
  TextHeight = 13
  object lblCalmRatio: TLabel
    Left = 532
    Top = 404
    Width = 58
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'lblCalmRatio'
  end
  object Chart: TChart
    Left = 0
    Top = 0
    Width = 518
    Height = 518
    AllowPanning = pmNone
    BackWall.Visible = False
    BottomWall.Visible = False
    LeftWall.Visible = False
    Legend.Alignment = laLeft
    Legend.Inverted = True
    Legend.LegendStyle = lsSeries
    Legend.ResizeChart = False
    Legend.Shadow.Visible = False
    Legend.Symbol.Shadow.Visible = False
    Legend.Symbol.Squared = True
    Legend.TopPos = 1
    Legend.Visible = False
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    AxisVisible = False
    BottomAxis.Automatic = False
    BottomAxis.AutomaticMaximum = False
    BottomAxis.AutomaticMinimum = False
    BottomAxis.Grid.Visible = False
    BottomAxis.Maximum = 10.000000000000000000
    BottomAxis.Minimum = -10.000000000000000000
    BottomAxis.PositionPercent = 50.000000000000000000
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMaximum = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.Grid.Visible = False
    LeftAxis.Labels = False
    LeftAxis.Maximum = 10.000000000000000000
    LeftAxis.Minimum = -10.000000000000000000
    LeftAxis.PositionPercent = 50.000000000000000000
    RightAxis.Automatic = False
    RightAxis.AutomaticMinimum = False
    View3D = False
    View3DWalls = False
    Zoom.Allow = False
    OnAfterDraw = ChartBeforeDrawSeries
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    PrintMargins = (
      25
      15
      25
      15)
    object Series5: TBarSeries
      Active = False
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = True
      Gradient.Direction = gdTopBottom
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
  end
  object rgrpSectionCount: TRadioGroup
    Left = 524
    Top = 8
    Width = 152
    Height = 65
    Anchors = [akTop, akRight]
    Caption = 'Sections count'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      '8'
      '16'
      '24'
      '32'
      '36'
      '72')
    TabOrder = 1
    OnClick = rgrpSectionCountClick
  end
  object btnAlterBrushColor: TButton
    Tag = 1
    Left = 524
    Top = 164
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Alter brush color'
    TabOrder = 2
    OnClick = btnAlterPenColorClick
  end
  object btnAlterPenColor: TButton
    Left = 524
    Top = 133
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Alter pen color'
    TabOrder = 3
    OnClick = btnAlterPenColorClick
  end
  object chkAxesOverRose: TCheckBox
    Left = 524
    Top = 110
    Width = 152
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Axes over rose diagram'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = chkAxesOverRoseClick
  end
  object btnChangeStyle: TButton
    Left = 524
    Top = 79
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '>> Change style >>'
    TabOrder = 5
    OnClick = btnChangeStyleClick
  end
  object btnCopyClipboard: TButton
    Left = 524
    Top = 464
    Width = 125
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Copy to clipboard...'
    TabOrder = 6
    OnClick = btnCopyClipboardClick
  end
  object btnPrint: TButton
    Left = 524
    Top = 495
    Width = 78
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Print...'
    TabOrder = 7
    OnClick = btnPrintClick
  end
  object grpSpeedDistribution: TGroupBox
    Left = 524
    Top = 218
    Width = 152
    Height = 157
    Anchors = [akTop, akRight]
    Caption = 'Speed distribution'
    TabOrder = 8
    DesignSize = (
      152
      157)
    object chkLogScales: TCheckBox
      Tag = 1
      Left = 8
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Log scales'
      TabOrder = 0
      OnClick = chkLogScalesClick
    end
    object rgrpSpeedClasses: TRadioGroup
      Left = 8
      Top = 76
      Width = 132
      Height = 76
      Caption = 'Speed classes'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        '5'
        '8'
        '12'
        '16'
        '20'
        '25')
      TabOrder = 1
      OnClick = rgrpSpeedClassesClick
    end
    object chkShowLegend: TCheckBox
      Left = 8
      Top = 18
      Width = 141
      Height = 39
      Anchors = [akTop, akRight]
      Caption = 'Show legend / Changes position in each click'
      TabOrder = 2
      WordWrap = True
      OnClick = chkAxesOverRoseClick
    end
  end
  object chkPenColorSameToBrush: TCheckBox
    Left = 524
    Top = 195
    Width = 152
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Pen color same to brush'
    TabOrder = 9
    OnClick = chkAxesOverRoseClick
  end
  object chkDisplayCalmRatio: TCheckBox
    Left = 532
    Top = 381
    Width = 144
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Display calm conditions'
    TabOrder = 10
    OnClick = chkAxesOverRoseClick
  end
  object edtCalmRatio: TEdit
    Left = 553
    Top = 427
    Width = 76
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 11
    Text = '5'
    OnChange = edtCalmRatioChange
    OnKeyPress = edtCalmThresholdKeyPress
  end
  object edtCalmThreshold: TEdit
    Left = 553
    Top = 427
    Width = 76
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 12
    Text = '1'
    Visible = False
    OnChange = edtCalmThresholdChange
    OnKeyPress = edtCalmThresholdKeyPress
  end
  object btnSave: TButton
    Left = 608
    Top = 495
    Width = 68
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save...'
    TabOrder = 13
    OnClick = btnSaveClick
  end
  object ColorDialog: TColorDialog
    Left = 464
    Top = 16
  end
  object PrintDialog: TPrintDialog
    Left = 464
    Top = 72
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'Windows bitmap file (*.bmp)|*.bmp|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 464
    Top = 128
  end
end
