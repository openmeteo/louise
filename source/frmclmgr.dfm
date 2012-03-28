object FrmClimacogram: TFrmClimacogram
  Left = 0
  Top = 0
  Caption = 'FrmClimacogram'
  ClientHeight = 410
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    518
    410)
  PixelsPerInch = 96
  TextHeight = 13
  object Chart: TChart
    Left = 0
    Top = 0
    Width = 521
    Height = 409
    MarginTop = 5
    Title.Text.Strings = (
      'TChart')
    BottomAxis.Logarithmic = True
    BottomAxis.MinorGrid.Color = clSilver
    BottomAxis.MinorGrid.Visible = True
    BottomAxis.MinorTickCount = 8
    LeftAxis.Logarithmic = True
    LeftAxis.MinorGrid.Color = clSilver
    LeftAxis.MinorGrid.Visible = True
    LeftAxis.MinorTickCount = 8
    LeftAxis.PositionUnits = muPixels
    View3D = False
    View3DOptions.Orthogonal = False
    Color = clWhite
    TabOrder = 0
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
end
