object FrmSetFlagsDialog: TFrmSetFlagsDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Set flags'
  ClientHeight = 289
  ClientWidth = 479
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
  DesignSize = (
    479
    289)
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 481
    Height = 250
    HorzScrollBar.Style = ssHotTrack
    HorzScrollBar.Tracking = True
    HorzScrollBar.Visible = False
    TabOrder = 0
    object FlowPanel: TFlowPanel
      Left = -1
      Top = -1
      Width = 477
      Height = 246
      Caption = 'FlowPanel'
      ShowCaption = False
      TabOrder = 0
    end
  end
  object btnOK: TButton
    Left = 16
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 112
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnAddNewFlag: TButton
    Left = 240
    Top = 256
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add new...'
    TabOrder = 3
    OnClick = btnAddNewFlagClick
  end
  object btnUncheckAll: TButton
    Left = 335
    Top = 256
    Width = 136
    Height = 25
    Caption = 'Uncheck all / reset'
    TabOrder = 4
    OnClick = btnUncheckAllClick
  end
end
