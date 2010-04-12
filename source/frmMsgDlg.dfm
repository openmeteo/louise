object FrmMessageDlg: TFrmMessageDlg
  BorderStyle = bsDialog
  Caption = 'Messages'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Top = 200
  Left = 200
  ClientWidth = 500
  ClientHeight = 180
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 144
    Width = 500
    Height = 36
    Align = alBottom
    TabOrder = 0
    object OKBtn: TButton
      Left = 220
      Top = 7
      Width = 60
      Height = 23
      Caption = 'OK'
      TabOrder = 0
      OnClick = OKBtnClick
    end
  end
  object MemoMessages: TMemo
    Left = 0
    Top = 0
    Width = 500
    Height = 144
    Align = alClient
    Lines.Strings = (
      'MemoMessages')
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
