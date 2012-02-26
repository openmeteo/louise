object FrmProcessingDialog: TFrmProcessingDialog
  Left = 390
  Top = 306
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Processing'
  ClientHeight = 149
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblProcessing: TLabel
    Left = 206
    Top = 5
    Width = 51
    Height = 13
    Caption = 'Processing'
  end
  object Label1: TLabel
    Left = 16
    Top = 64
    Width = 57
    Height = 13
    Caption = 'Completion:'
  end
  object lblPercent: TLabel
    Left = 88
    Top = 64
    Width = 47
    Height = 13
    Caption = 'lblPercent'
  end
  object Label2: TLabel
    Left = 153
    Top = 64
    Width = 4
    Height = 13
    Caption = '('
  end
  object lblCompleted: TLabel
    Left = 161
    Top = 64
    Width = 61
    Height = 13
    Caption = 'lblCompleted'
  end
  object Label3: TLabel
    Left = 226
    Top = 64
    Width = 66
    Height = 13
    Caption = 'steps done of'
  end
  object lblTotal: TLabel
    Left = 312
    Top = 64
    Width = 34
    Height = 13
    Caption = 'lblTotal'
  end
  object Label4: TLabel
    Left = 352
    Top = 64
    Width = 9
    Height = 13
    Caption = ')'
  end
  object Label5: TLabel
    Left = 16
    Top = 83
    Width = 121
    Height = 13
    Caption = 'Elapsed time (hh:mm:ss):'
  end
  object lblElapsed: TLabel
    Left = 206
    Top = 83
    Width = 47
    Height = 13
    Caption = 'lblElapsed'
  end
  object Label7: TLabel
    Left = 16
    Top = 102
    Width = 183
    Height = 13
    Caption = 'Estimated Remaining time (hh:mm:ss):'
  end
  object lblRemaining: TLabel
    Left = 206
    Top = 102
    Width = 59
    Height = 13
    Caption = 'lblRemaining'
  end
  object ProgressBar: TProgressBar
    Left = 5
    Top = 30
    Width = 454
    Height = 16
    Smooth = True
    TabOrder = 0
  end
  object bntStop: TButton
    Left = 195
    Top = 121
    Width = 75
    Height = 21
    Caption = 'Stop'
    TabOrder = 1
    OnClick = bntStopClick
  end
end
