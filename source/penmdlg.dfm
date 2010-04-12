object FrmPenman: TFrmPenman
  Left = 204
  Top = 148
  BorderStyle = bsDialog
  Caption = 'Evapotranspiration calculations'
  ClientHeight = 284
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = IFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 13
    Caption = 'Latitude:'
  end
  object Label12: TLabel
    Left = 107
    Top = 8
    Width = 4
    Height = 13
    Caption = #176
  end
  object Label13: TLabel
    Left = 140
    Top = 8
    Width = 2
    Height = 13
    Caption = #39
  end
  object Label14: TLabel
    Left = 184
    Top = 8
    Width = 5
    Height = 13
    Caption = '"'
  end
  object lblDeterminationFactor: TLabel
    Left = 375
    Top = 208
    Width = 98
    Height = 13
    Alignment = taRightJustify
    Caption = 'Determination factor:'
  end
  object grpParams: TGroupBox
    Left = 0
    Top = 128
    Width = 141
    Height = 105
    Caption = 'Parameters'
    TabOrder = 8
    Visible = False
    object lblAlpha: TLabel
      Left = 55
      Top = 15
      Width = 9
      Height = 13
      Caption = 'a:'
    end
    object Label16: TLabel
      Left = 55
      Top = 40
      Width = 9
      Height = 13
      Caption = 'b:'
    end
    object Label17: TLabel
      Left = 55
      Top = 65
      Width = 9
      Height = 13
      Caption = 'c:'
    end
    object edtAlphaParam: TEdit
      Left = 70
      Top = 15
      Width = 61
      Height = 21
      TabOrder = 0
    end
    object edtBetaParam: TEdit
      Left = 70
      Top = 40
      Width = 61
      Height = 21
      TabOrder = 1
    end
    object edtCeParam: TEdit
      Left = 70
      Top = 65
      Width = 61
      Height = 21
      TabOrder = 2
    end
  end
  object BtnOK: TButton
    Left = 0
    Top = 256
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object BtnCancel: TButton
    Left = 232
    Top = 256
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object EdtLatDeg: TEdit
    Left = 82
    Top = 8
    Width = 25
    Height = 21
    TabOrder = 0
    Text = 'EdtLatDeg'
  end
  object EdtLatMin: TEdit
    Left = 114
    Top = 8
    Width = 25
    Height = 21
    TabOrder = 1
    Text = 'EdtLatMin'
  end
  object EdtLatSec: TEdit
    Left = 146
    Top = 8
    Width = 39
    Height = 21
    TabOrder = 2
    Text = 'EdtLatSec'
  end
  object rgrpCalculationType: TRadioGroup
    Left = 192
    Top = 8
    Width = 121
    Height = 121
    Caption = 'Calculation'
    ItemIndex = 0
    Items.Strings = (
      'Penman'
      'Penman - Monteith'
      'Thornthwaite'
      'Blaney-Criddle'
      'Hargreaves'
      'Parametric')
    TabOrder = 6
    OnClick = rgrpCalculationTypeClick
  end
  object pnlPenman: TPanel
    Left = 5
    Top = 30
    Width = 180
    Height = 51
    BevelOuter = bvNone
    TabOrder = 7
    object Label15: TLabel
      Left = 128
      Top = 2
      Width = 8
      Height = 13
      Caption = 'm'
    end
    object Label10: TLabel
      Left = 3
      Top = 2
      Width = 38
      Height = 13
      Caption = 'Altitude:'
    end
    object Label11: TLabel
      Left = 3
      Top = 26
      Width = 36
      Height = 13
      Caption = 'Albedo:'
    end
    object EdtAltitude: TEdit
      Left = 77
      Top = 2
      Width = 41
      Height = 21
      TabOrder = 0
      Text = 'EdtAltitude'
    end
    object EdtAlbedo: TEdit
      Left = 78
      Top = 26
      Width = 41
      Height = 21
      TabOrder = 1
      Text = 'EdtAlbedo'
    end
  end
  object rgrpSunshine: TRadioGroup
    Left = 320
    Top = 8
    Width = 159
    Height = 121
    Caption = 'Sunshine Timeseries'
    ItemIndex = 0
    Items.Strings = (
      'Sunshine duration (min)'
      'Sunshine Percent (0-1)'
      'Radiation (kJ/sqm/d)')
    TabOrder = 10
  end
  object btnCalcParams: TButton
    Left = 392
    Top = 256
    Width = 83
    Height = 25
    Caption = 'Calc Params'
    TabOrder = 11
    OnClick = btnCalcParamsClick
  end
  object edtDetermination: TEdit
    Left = 416
    Top = 228
    Width = 57
    Height = 21
    ReadOnly = True
    TabOrder = 12
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 128
    Width = 313
    Height = 121
    Caption = 'Parameters'
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 65
      Height = 13
      Caption = 'Brunt formula:'
    end
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 60
      Height = 13
      Caption = 'Cloud effect:'
    end
    object Label3: TLabel
      Left = 8
      Top = 64
      Width = 97
      Height = 13
      Caption = 'Prescott co-efficient:'
    end
    object Label4: TLabel
      Left = 160
      Top = 16
      Width = 16
      Height = 13
      Caption = 'Ae:'
    end
    object Label5: TLabel
      Left = 240
      Top = 16
      Width = 16
      Height = 13
      Caption = 'Be:'
    end
    object Label6: TLabel
      Left = 160
      Top = 40
      Width = 15
      Height = 13
      Caption = 'aL:'
    end
    object Label7: TLabel
      Left = 240
      Top = 40
      Width = 15
      Height = 13
      Caption = 'bL:'
    end
    object Label8: TLabel
      Left = 160
      Top = 64
      Width = 15
      Height = 13
      Caption = 'As:'
    end
    object Label9: TLabel
      Left = 240
      Top = 64
      Width = 15
      Height = 13
      Caption = 'Bs:'
    end
    object EdtA_e: TEdit
      Left = 184
      Top = 16
      Width = 41
      Height = 21
      TabOrder = 0
      Text = 'EdtA_e'
    end
    object EdtA_L: TEdit
      Left = 184
      Top = 40
      Width = 41
      Height = 21
      TabOrder = 2
      Text = 'EdtA_L'
    end
    object EdtA_s: TEdit
      Left = 184
      Top = 64
      Width = 41
      Height = 21
      TabOrder = 4
      Text = 'EdtA_s'
    end
    object EdtB_e: TEdit
      Left = 264
      Top = 16
      Width = 41
      Height = 21
      TabOrder = 1
      Text = 'EdtB_e'
    end
    object EdtB_L: TEdit
      Left = 264
      Top = 40
      Width = 41
      Height = 21
      TabOrder = 3
      Text = 'EdtB_L'
    end
    object EdtB_s: TEdit
      Left = 264
      Top = 64
      Width = 41
      Height = 21
      TabOrder = 5
      Text = 'EdtB_s'
    end
    object BtnDefaultParms: TButton
      Left = 240
      Top = 88
      Width = 65
      Height = 25
      Caption = 'Defaults'
      TabOrder = 6
      OnClick = BtnDefaultParmsClick
    end
  end
  object grpBlaneyCriddle: TGroupBox
    Left = 0
    Top = 128
    Width = 141
    Height = 105
    Caption = 'Parameters'
    TabOrder = 9
    Visible = False
    object Label18: TLabel
      Left = 8
      Top = 16
      Width = 15
      Height = 13
      Caption = 'kc:'
    end
    object edtKc: TEdit
      Left = 32
      Top = 16
      Width = 81
      Height = 21
      TabOrder = 0
      Text = '0.50'
    end
  end
  object rgrpMonthDay: TRadioGroup
    Left = 320
    Top = 128
    Width = 159
    Height = 72
    Caption = 'Day for Penman / Monthly'
    ItemIndex = 0
    Items.Strings = (
      'Representive day for month'
      'Month middle')
    TabOrder = 13
    WordWrap = True
  end
end
