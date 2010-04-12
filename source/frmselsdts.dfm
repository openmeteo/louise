object SelectSDTimeseries: TSelectSDTimeseries
  BorderStyle = bsDialog
  Caption = 'Select time series for stage - discharge'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnShow = FormShow
  Top = 220
  Left = 280
  ClientWidth = 370
  ClientHeight = 205
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 0
    Width = 114
    Height = 13
    Caption = 'Select stage time series:'
  end
  object Label2: TLabel
    Left = 216
    Top = 0
    Width = 134
    Height = 13
    Caption = 'Select discharge time series:'
  end
  object btnOK: TButton
    Left = 8
    Top = 176
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 288
    Top = 176
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object lstStageTS: TListBox
    Left = 0
    Top = 16
    Width = 177
    Height = 97
    ItemHeight = 13
    TabOrder = 2
    OnClick = lstStageTSClick
  end
  object lstDischargeTS: TListBox
    Left = 192
    Top = 16
    Width = 177
    Height = 97
    ItemHeight = 13
    TabOrder = 3
    OnClick = lstStageTSClick
  end
  object memoStageTS: TMemo
    Left = 0
    Top = 120
    Width = 177
    Height = 49
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object memoDischargeTS: TMemo
    Left = 192
    Top = 120
    Width = 177
    Height = 49
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 5
  end
end
