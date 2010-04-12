object FrmMain: TFrmMain
  Left = 422
  Top = 225
  BorderStyle = bsDialog
  Caption = 'louise installer'
  ClientHeight = 148
  ClientWidth = 218
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LblWillInstall: TLabel
    Left = 8
    Top = 8
    Width = 122
    Height = 13
    Caption = 'This program will install LoUISE'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 200
    Height = 13
    Caption = 'The following components will be installed:'
  end
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 147
    Height = 13
    Caption = 'Close Delphi before continuing.'
  end
  object ChkInstallLibrary: TCheckBox
    Left = 16
    Top = 72
    Width = 97
    Height = 17
    Caption = 'LoUISE library'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 0
  end
  object ChkInstallHelp: TCheckBox
    Left = 16
    Top = 96
    Width = 97
    Height = 17
    Caption = 'Help files'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object BtnInstall: TButton
    Left = 56
    Top = 120
    Width = 97
    Height = 25
    Caption = 'Install'
    TabOrder = 2
    OnClick = BtnInstallClick
  end
end
