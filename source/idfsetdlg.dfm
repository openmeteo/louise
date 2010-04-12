object FrmIDFSet: TFrmIDFSet
  Left = 256
  Top = 197
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Set IDF Timeseries'
  ClientHeight = 423
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = IFormCreate
  OnDestroy = IFormDestroy
  OnShow = IFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 5
    Width = 102
    Height = 13
    Caption = 'Available Time series:'
  end
  object Label2: TLabel
    Left = 400
    Top = 5
    Width = 120
    Height = 13
    Caption = 'IDF Time series (intensity)'
  end
  object Label3: TLabel
    Left = 5
    Top = 260
    Width = 107
    Height = 13
    Caption = 'Time series comments:'
  end
  object Label4: TLabel
    Left = 235
    Top = 260
    Width = 94
    Height = 13
    Caption = 'Time series records:'
  end
  object lblExplicitEta: TLabel
    Left = 200
    Top = 237
    Width = 16
    Height = 13
    Caption = 'Eta'
  end
  object lblExplicitTheta: TLabel
    Left = 296
    Top = 237
    Width = 28
    Height = 13
    Caption = 'Theta'
  end
  object lstTimeseries: TListBox
    Left = 5
    Top = 25
    Width = 221
    Height = 168
    ItemHeight = 13
    TabOrder = 0
    OnClick = lstTimeseriesClick
  end
  object memoTSComments: TMemo
    Left = 5
    Top = 275
    Width = 226
    Height = 111
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object memoTSRecords: TMemo
    Left = 235
    Top = 275
    Width = 166
    Height = 111
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object rgrpTimeseriesValues: TRadioGroup
    Left = 5
    Top = 197
    Width = 141
    Height = 56
    Caption = 'Timesereries Values are:'
    ItemIndex = 0
    Items.Strings = (
      'Height (mm)'
      'Intensity (mm/h)')
    TabOrder = 3
  end
  object lstIDFTimeseries: TListBox
    Left = 400
    Top = 25
    Width = 216
    Height = 236
    ItemHeight = 13
    TabOrder = 4
    OnClick = lstIDFTimeseriesClick
  end
  object btnLoad: TButton
    Left = 5
    Top = 395
    Width = 85
    Height = 25
    Caption = 'Load...'
    Enabled = False
    TabOrder = 5
  end
  object btnSave: TButton
    Left = 105
    Top = 395
    Width = 85
    Height = 25
    Caption = 'Save...'
    Enabled = False
    TabOrder = 6
  end
  object btnClose: TButton
    Left = 435
    Top = 395
    Width = 85
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 7
  end
  object btnAnalysis: TButton
    Left = 535
    Top = 395
    Width = 85
    Height = 25
    Caption = 'IDF Analysis...'
    TabOrder = 8
    OnClick = btnAnalysisClick
  end
  object rgrpAmount: TRadioGroup
    Left = 410
    Top = 275
    Width = 207
    Height = 110
    Caption = 'Desired amount of records used'
    Items.Strings = (
      '10%'
      '20%'
      '1/3'
      '50%'
      '2/3'
      '100%')
    TabOrder = 9
    OnClick = rgrpAmountClick
  end
  object grpControls: TGroupBox
    Left = 231
    Top = 16
    Width = 166
    Height = 193
    Caption = 'Options'
    TabOrder = 10
    object lblDuration: TLabel
      Left = 10
      Top = 17
      Width = 43
      Height = 13
      Caption = 'Duration:'
    end
    object btnAdd: TButton
      Left = 10
      Top = 165
      Width = 65
      Height = 20
      Caption = 'Add'
      TabOrder = 0
      OnClick = btnAddClick
    end
    object btnRemove: TButton
      Left = 90
      Top = 165
      Width = 65
      Height = 20
      Caption = 'Remove'
      TabOrder = 1
      OnClick = btnRemoveClick
    end
    object edtDuration: TEdit
      Left = 18
      Top = 35
      Width = 46
      Height = 21
      TabOrder = 2
    end
    object Panel1: TPanel
      Left = 73
      Top = 35
      Width = 66
      Height = 31
      BevelOuter = bvNone
      TabOrder = 3
      object rdbDurationMinutes: TRadioButton
        Left = 5
        Top = 0
        Width = 113
        Height = 17
        Caption = 'Minutes'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rdbDurationHours: TRadioButton
        Left = 5
        Top = 15
        Width = 113
        Height = 17
        Caption = 'Hours'
        TabOrder = 1
      end
    end
    object chkTimeResolutionUnknown: TCheckBox
      Left = 8
      Top = 68
      Width = 156
      Height = 17
      Caption = 'Time ressolution unknown'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = chkTimeResolutionUnknownClick
    end
    object pnlTimeResolution: TPanel
      Left = 8
      Top = 84
      Width = 151
      Height = 51
      BevelOuter = bvNone
      TabOrder = 5
      object lblTimeResolution: TLabel
        Left = 2
        Top = 0
        Width = 74
        Height = 13
        Caption = 'Time resolution:'
      end
      object Panel2: TPanel
        Left = 65
        Top = 15
        Width = 66
        Height = 31
        BevelOuter = bvNone
        TabOrder = 0
        object rdbResolutionMinutes: TRadioButton
          Left = 5
          Top = 0
          Width = 113
          Height = 17
          Caption = 'Minutes'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rdbResolutionHours: TRadioButton
          Left = 5
          Top = 15
          Width = 113
          Height = 17
          Caption = 'Hours'
          TabOrder = 1
        end
      end
      object edtTimeResolution: TEdit
        Left = 15
        Top = 15
        Width = 46
        Height = 21
        TabOrder = 1
      end
    end
    object chkEtaTheta: TCheckBox
      Left = 8
      Top = 129
      Width = 145
      Height = 17
      Caption = 'Use for Eta, Theta'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object chkDataList: TCheckBox
      Left = 9
      Top = 146
      Width = 144
      Height = 17
      Caption = 'Use for Distrib. fit'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
  end
  object chkExplicitSetEtaTheta: TCheckBox
    Left = 264
    Top = 216
    Width = 81
    Height = 17
    Caption = 'Explicit set:'
    TabOrder = 11
    OnClick = chkTimeResolutionUnknownClick
  end
  object edtExplicitEta: TEdit
    Left = 224
    Top = 237
    Width = 57
    Height = 21
    TabOrder = 12
    Text = '0.5'
  end
  object edtExplicitTheta: TEdit
    Left = 328
    Top = 237
    Width = 57
    Height = 21
    TabOrder = 13
    Text = '0.5'
  end
end
