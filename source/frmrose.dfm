object FrmRoseDiagram: TFrmRoseDiagram
  Left = 0
  Top = 0
  Caption = 'Rose diagram'
  ClientHeight = 528
  ClientWidth = 684
  Color = clBtnFace
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
  object Chart: TChart
    Left = 0
    Top = 0
    Width = 518
    Height = 518
    AllowPanning = pmNone
    BackWall.Visible = False
    BottomWall.Visible = False
    LeftWall.Visible = False
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
    object Series2: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object rgrpSectionCount: TRadioGroup
    Left = 524
    Top = 8
    Width = 152
    Height = 73
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
    Top = 254
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Alter brush color'
    TabOrder = 2
    OnClick = btnAlterPenColorClick
  end
  object btnAlterPenColor: TButton
    Left = 524
    Top = 223
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Alter pen color'
    TabOrder = 3
    OnClick = btnAlterPenColorClick
  end
  object chkAxesOverRose: TCheckBox
    Left = 524
    Top = 136
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
    Top = 95
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Change style'
    TabOrder = 5
    OnClick = btnChangeStyleClick
  end
  object btnCopyClipboard: TButton
    Left = 524
    Top = 448
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Copy to clipboard'
    TabOrder = 6
    OnClick = btnCopyClipboardClick
  end
  object btnPrint: TButton
    Left = 524
    Top = 479
    Width = 109
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Print'
    TabOrder = 7
    OnClick = btnPrintClick
  end
  object ColorDialog: TColorDialog
    Left = 464
    Top = 16
  end
  object PrintDialog: TPrintDialog
    Left = 464
    Top = 72
  end
end
