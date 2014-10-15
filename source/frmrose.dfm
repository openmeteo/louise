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
  Menu = MainMenu
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
    Left = 524
    Top = 358
    Width = 58
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'lblCalmRatio'
  end
  object lblMarkSectors: TLabel
    Left = 524
    Top = 404
    Width = 98
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Mark sectors (drag):'
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
    BottomAxis.LabelsFormat.TextAlignment = taCenter
    BottomAxis.Maximum = 10.000000000000000000
    BottomAxis.Minimum = -10.000000000000000000
    BottomAxis.PositionPercent = 50.000000000000000000
    DepthAxis.LabelsFormat.TextAlignment = taCenter
    DepthTopAxis.LabelsFormat.TextAlignment = taCenter
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMaximum = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.Grid.Visible = False
    LeftAxis.Labels = False
    LeftAxis.LabelsFormat.Visible = False
    LeftAxis.LabelsFormat.TextAlignment = taCenter
    LeftAxis.Maximum = 10.000000000000000000
    LeftAxis.Minimum = -10.000000000000000000
    LeftAxis.PositionPercent = 50.000000000000000000
    RightAxis.Automatic = False
    RightAxis.AutomaticMinimum = False
    RightAxis.LabelsFormat.TextAlignment = taCenter
    TopAxis.LabelsFormat.TextAlignment = taCenter
    View3D = False
    View3DWalls = False
    Zoom.Allow = False
    Zoom.Pen.Mode = pmNotXor
    OnAfterDraw = ChartBeforeDrawSeries
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    DefaultCanvas = 'TGDIPlusCanvas'
    PrintMargins = (
      25
      15
      25
      15)
    ColorPaletteIndex = 13
    object Series5: TBarSeries
      Active = False
      Marks.Visible = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
  end
  object rgrpSectionCount: TRadioGroup
    Left = 524
    Top = 2
    Width = 152
    Height = 55
    Anchors = [akTop, akRight]
    Caption = 'Sections count'
    Columns = 3
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
    Top = 144
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Alter brush color'
    TabOrder = 2
    OnClick = btnAlterPenColorClick
  end
  object btnAlterPenColor: TButton
    Left = 524
    Top = 113
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Alter pen color'
    TabOrder = 3
    OnClick = btnAlterPenColorClick
  end
  object chkAxesOverRose: TCheckBox
    Left = 524
    Top = 92
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
    Top = 61
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '>> Change style >>'
    TabOrder = 5
    OnClick = btnChangeStyleClick
  end
  object grpSpeedDistribution: TGroupBox
    Left = 524
    Top = 196
    Width = 152
    Height = 133
    Anchors = [akTop, akRight]
    Caption = 'Speed distribution'
    TabOrder = 6
    DesignSize = (
      152
      133)
    object chkLogScales: TCheckBox
      Tag = 1
      Left = 8
      Top = 50
      Width = 97
      Height = 17
      Caption = 'Log scales'
      TabOrder = 0
      OnClick = chkLogScalesClick
    end
    object rgrpSpeedClasses: TRadioGroup
      Left = 8
      Top = 69
      Width = 141
      Height = 60
      Caption = 'Speed classes'
      Columns = 3
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
      Top = 12
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
    Top = 175
    Width = 152
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Pen color same to brush'
    TabOrder = 7
    OnClick = chkAxesOverRoseClick
  end
  object chkDisplayCalmRatio: TCheckBox
    Left = 524
    Top = 335
    Width = 144
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Display calm conditions'
    TabOrder = 8
    OnClick = chkAxesOverRoseClick
  end
  object edtCalmRatio: TEdit
    Left = 553
    Top = 377
    Width = 76
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 9
    Text = '5'
    OnChange = edtCalmRatioChange
    OnKeyPress = edtCalmThresholdKeyPress
  end
  object edtCalmThreshold: TEdit
    Left = 553
    Top = 377
    Width = 76
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 10
    Text = '1'
    Visible = False
    OnChange = edtCalmThresholdChange
    OnKeyPress = edtCalmThresholdKeyPress
  end
  object lstMarkSection: TListBox
    Left = 524
    Top = 423
    Width = 152
    Height = 69
    Anchors = [akTop, akRight]
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 11
    OnClick = lstMarkSectionClick
    OnKeyDown = lstMarkSectionKeyDown
  end
  object btnSpeedStats: TButton
    Left = 524
    Top = 495
    Width = 152
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Speed stats on marked...'
    TabOrder = 12
    OnClick = btnSpeedStatsClick
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
  object MainMenu: TMainMenu
    Left = 464
    Top = 192
    object mnuFile: TMenuItem
      Caption = 'File'
      object mnuSaveBitmap: TMenuItem
        Caption = 'Save as bitmap...'
        OnClick = btnSaveClick
      end
      object mnuPrint: TMenuItem
        Caption = 'Print diagram...'
        OnClick = btnPrintClick
      end
    end
    object mnuEdit: TMenuItem
      Caption = 'Edit'
      object mnuCopyClipboard: TMenuItem
        Caption = 'Copy to clipboard...'
        OnClick = btnCopyClipboardClick
      end
    end
    object mnuOptions: TMenuItem
      Caption = 'Options'
      object mnuBellowThresholdStats: TMenuItem
        Caption = 'Use bellow threshold speeds for marked sectors stats'
        Checked = True
        OnClick = mnuBellowThresholdStatsClick
      end
    end
  end
end
