object FrmMLEDialog: TFrmMLEDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'MLE Estimation parameters'
  ClientHeight = 415
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    451
    415)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 54
    Height = 13
    Caption = 'Distribution'
  end
  object edtDistributionName: TEdit
    Left = 88
    Top = 8
    Width = 185
    Height = 21
    ReadOnly = True
    TabOrder = 0
    Text = 'distribution'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 35
    Width = 435
    Height = 54
    Caption = 'Moments method parameters'
    TabOrder = 1
    object lblLMomentsParam1: TLabel
      Tag = 1
      Left = 16
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Param1'
    end
    object lblLMomentsParam2: TLabel
      Tag = 2
      Left = 152
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Param2'
    end
    object lblLMomentsParam3: TLabel
      Tag = 3
      Left = 280
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Param3'
    end
    object edtMomentsParam1: TEdit
      Tag = 1
      Left = 58
      Top = 16
      Width = 71
      Height = 21
      NumbersOnly = True
      ReadOnly = True
      TabOrder = 0
      Text = '0'
    end
    object edtMomentsParam2: TEdit
      Tag = 2
      Left = 194
      Top = 16
      Width = 71
      Height = 21
      NumbersOnly = True
      ReadOnly = True
      TabOrder = 1
      Text = '0'
    end
    object edtMomentsParam3: TEdit
      Tag = 3
      Left = 322
      Top = 16
      Width = 71
      Height = 21
      NumbersOnly = True
      ReadOnly = True
      TabOrder = 2
      Text = '0'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 167
    Width = 435
    Height = 50
    Caption = 'Optimization initial values'
    TabOrder = 2
    object lblOptimParam1: TLabel
      Tag = 1
      Left = 16
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Param1'
    end
    object lblOptimParam2: TLabel
      Tag = 2
      Left = 152
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Param2'
    end
    object lblOptimParam3: TLabel
      Tag = 3
      Left = 280
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Param3'
    end
    object edtOptimizationParam1: TEdit
      Tag = 1
      Left = 58
      Top = 16
      Width = 71
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object edtOptimizationParam2: TEdit
      Tag = 2
      Left = 194
      Top = 16
      Width = 71
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object edtOptimizationParam3: TEdit
      Tag = 3
      Left = 322
      Top = 16
      Width = 71
      Height = 21
      TabOrder = 2
      Text = '0'
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 95
    Width = 435
    Height = 50
    Caption = 'Sample bounds'
    TabOrder = 3
    object Label8: TLabel
      Left = 12
      Top = 16
      Width = 40
      Height = 13
      Caption = 'Minimum'
    end
    object Label9: TLabel
      Left = 144
      Top = 16
      Width = 44
      Height = 13
      Caption = 'Maximum'
    end
    object edtSampleMin: TEdit
      Left = 58
      Top = 16
      Width = 71
      Height = 21
      NumbersOnly = True
      ReadOnly = True
      TabOrder = 0
      Text = '0'
    end
    object edtSampleMax: TEdit
      Left = 194
      Top = 16
      Width = 71
      Height = 21
      NumbersOnly = True
      ReadOnly = True
      TabOrder = 1
      Text = '0'
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 223
    Width = 435
    Height = 98
    Caption = 'Optimization, limits of parameters'
    TabOrder = 4
    object lblLimitsParam1: TLabel
      Tag = 1
      Left = 72
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Param1'
    end
    object lblLimitsParam2: TLabel
      Tag = 2
      Left = 208
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Param2'
    end
    object lblLimitsParam3: TLabel
      Tag = 3
      Left = 336
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Param3'
    end
    object Label13: TLabel
      Left = 12
      Top = 32
      Width = 40
      Height = 13
      Caption = 'Minimum'
    end
    object Label14: TLabel
      Left = 8
      Top = 62
      Width = 44
      Height = 13
      Caption = 'Maximum'
    end
    object edtParam1Min: TEdit
      Tag = 1
      Left = 58
      Top = 35
      Width = 71
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edtParam1MinChange
    end
    object edtParam2Min: TEdit
      Tag = 2
      Left = 194
      Top = 35
      Width = 71
      Height = 21
      TabOrder = 1
      Text = '0'
      OnChange = edtParam1MinChange
    end
    object edtParam3Min: TEdit
      Tag = 3
      Left = 322
      Top = 35
      Width = 71
      Height = 21
      TabOrder = 2
      Text = '0'
      OnChange = edtParam1MinChange
    end
    object edtParam1Max: TEdit
      Tag = 1
      Left = 58
      Top = 62
      Width = 71
      Height = 21
      TabOrder = 3
      Text = '0'
      OnChange = edtParam1MinChange
    end
    object edtParam2Max: TEdit
      Tag = 2
      Left = 194
      Top = 62
      Width = 71
      Height = 21
      TabOrder = 4
      Text = '0'
      OnChange = edtParam1MinChange
    end
    object edtParam3Max: TEdit
      Tag = 3
      Left = 322
      Top = 62
      Width = 71
      Height = 21
      TabOrder = 5
      Text = '0'
      OnChange = edtParam1MinChange
    end
  end
  object btnCalculate: TButton
    Left = 8
    Top = 382
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Calculate'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 368
    Top = 382
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 327
    Width = 435
    Height = 50
    Caption = 
      'Expected random value bounds according to the optimization limit' +
      's of parameters'
    TabOrder = 7
    object Label2: TLabel
      Left = 12
      Top = 16
      Width = 40
      Height = 13
      Caption = 'Minimum'
    end
    object Label3: TLabel
      Left = 144
      Top = 16
      Width = 44
      Height = 13
      Caption = 'Maximum'
    end
    object edtOptMin: TEdit
      Left = 58
      Top = 16
      Width = 71
      Height = 21
      NumbersOnly = True
      ReadOnly = True
      TabOrder = 0
      Text = '0'
    end
    object edtOptMax: TEdit
      Left = 194
      Top = 16
      Width = 71
      Height = 21
      NumbersOnly = True
      ReadOnly = True
      TabOrder = 1
      Text = '0'
    end
  end
end
