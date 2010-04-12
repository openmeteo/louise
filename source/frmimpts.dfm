object FrmImportDataToTimeseries: TFrmImportDataToTimeseries
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Import data to time series'
  ClientHeight = 558
  ClientWidth = 709
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnImportData: TButton
    Left = 8
    Top = 525
    Width = 97
    Height = 25
    Caption = 'Import data'
    ModalResult = 1
    TabOrder = 0
  end
  object memoFileContents: TMemo
    Left = 8
    Top = 9
    Width = 410
    Height = 200
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object grpTestParsing: TGroupBox
    Left = 424
    Top = 8
    Width = 281
    Height = 201
    Caption = 'Parsing test'
    TabOrder = 2
    object sgrdParseTest: TStringGrid
      Left = 3
      Top = 16
      Width = 275
      Height = 145
      ColCount = 3
      DefaultRowHeight = 18
      RowCount = 10
      FixedRows = 0
      ScrollBars = ssVertical
      TabOrder = 0
      ColWidths = (
        129
        66
        64)
      RowHeights = (
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18)
    end
    object btnTest: TButton
      Left = 64
      Top = 167
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 1
      OnClick = btnTestClick
    end
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 215
    Width = 697
    Height = 304
    Caption = 'Options'
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 54
      Width = 47
      Height = 13
      Caption = 'Encoding:'
    end
    object Label2: TLabel
      Left = 480
      Top = 21
      Width = 63
      Height = 13
      Caption = 'Data column:'
    end
    object Label3: TLabel
      Left = 8
      Top = 79
      Width = 161
      Height = 13
      Caption = 'Missing data character sequence:'
    end
    object Label4: TLabel
      Left = 487
      Top = 79
      Width = 100
      Height = 13
      Caption = 'Other data delimiter:'
    end
    object Bevel1: TBevel
      Left = 471
      Top = 58
      Width = 202
      Height = 9
      Shape = bsBottomLine
    end
    object Bevel2: TBevel
      Left = 471
      Top = 134
      Width = 202
      Height = 9
      Shape = bsBottomLine
    end
    object Label10: TLabel
      Left = 8
      Top = 21
      Width = 87
      Height = 13
      Caption = 'Line to start read:'
    end
    object Label11: TLabel
      Left = 471
      Top = 192
      Width = 65
      Height = 13
      Caption = 'Flags column:'
    end
    object Label12: TLabel
      Left = 471
      Top = 277
      Width = 101
      Height = 13
      Caption = 'Other flags delimiter:'
    end
    object cmbEncoding: TComboBox
      Left = 96
      Top = 54
      Width = 113
      Height = 19
      Style = csOwnerDrawFixed
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbEncodingChange
      Items.Strings = (
        'UTF-8'
        'ASCII'
        'Default System ANSI code'
        'UTF-7'
        'UTF-16'
        'UTF-16 Big Endian')
    end
    object chkTrimSpaces: TCheckBox
      Left = 8
      Top = 123
      Width = 153
      Height = 17
      Caption = 'Trim spaces'
      TabOrder = 1
      OnClick = chkTrimSpacesClick
    end
    object chkOverwrite: TCheckBox
      Left = 8
      Top = 146
      Width = 177
      Height = 17
      Caption = 'Overwrite existing records'
      TabOrder = 2
      OnClick = chkOverwriteClick
    end
    object chkHaltOnErrors: TCheckBox
      Left = 8
      Top = 169
      Width = 153
      Height = 17
      Caption = 'Halt on errors'
      TabOrder = 3
      OnClick = chkHaltOnErrorsClick
    end
    object spinDataColumn: TSpinEdit
      Left = 592
      Top = 21
      Width = 65
      Height = 22
      EditorEnabled = False
      MaxValue = 100000
      MinValue = 1
      TabOrder = 4
      Value = 1
      OnChange = spinDataColumnChange
    end
    object edtNullValueString: TEdit
      Left = 96
      Top = 98
      Width = 113
      Height = 21
      TabOrder = 5
      OnChange = edtNullValueStringChange
    end
    object rgrpDecimalSymbol: TRadioGroup
      Left = 229
      Top = 21
      Width = 220
      Height = 49
      Caption = 'Decimal symbol'
      Columns = 2
      Items.Strings = (
        'Dot (.)'
        'Comma (,)')
      TabOrder = 6
      OnClick = rgrpDecimalSymbolClick
    end
    object rgrpDataDelimiter: TRadioGroup
      Left = 229
      Top = 79
      Width = 220
      Height = 74
      Caption = 'Data delimiter'
      Columns = 2
      Items.Strings = (
        'Comma (,)'
        'Semicolon (;)'
        'White space'
        'Tab character'
        'Dollar sign ($)'
        'Other (specify)')
      TabOrder = 7
      OnClick = rgrpDataDelimiterClick
    end
    object edtOtherDelimiter: TEdit
      Left = 592
      Top = 98
      Width = 65
      Height = 21
      TabOrder = 8
      Text = '%'
      OnChange = edtOtherDelimiterChange
    end
    object chkParseDates: TCheckBox
      Left = 8
      Top = 233
      Width = 153
      Height = 17
      Caption = 'Parse dates'
      TabOrder = 9
      OnClick = chkParseDatesClick
    end
    object grpParseDatesOptions: TGroupBox
      Left = 133
      Top = 169
      Width = 316
      Height = 132
      Caption = 'Parse dates options'
      TabOrder = 10
      Visible = False
      object Label5: TLabel
        Left = 179
        Top = 20
        Width = 63
        Height = 13
        Caption = 'Date column:'
      end
      object Label6: TLabel
        Left = 179
        Top = 89
        Width = 92
        Height = 13
        Caption = 'Other date format:'
      end
      object spinDateColumn: TSpinEdit
        Left = 248
        Top = 20
        Width = 65
        Height = 22
        EditorEnabled = False
        MaxValue = 100000
        MinValue = 1
        TabOrder = 0
        Value = 1
        OnChange = spinDateColumnChange
      end
      object rgrpDateFormat: TRadioGroup
        Left = 16
        Top = 16
        Width = 157
        Height = 113
        Caption = 'Date format'
        Items.Strings = (
          'Auto detect'
          'dd/mm/yyyy'
          'yyyy-mm'
          'yyyy-mm-ddThh:nn'
          'Other (specify)')
        TabOrder = 1
        OnClick = rgrpDateFormatClick
      end
      object edtOtherDateFormat: TEdit
        Left = 192
        Top = 108
        Width = 121
        Height = 21
        TabOrder = 2
        Text = 'yyyy-mm-dd'
        OnChange = edtOtherDateFormatChange
      end
    end
    object grpSpecifyDates: TGroupBox
      Left = 178
      Top = 169
      Width = 226
      Height = 132
      Caption = 'Specify dates'
      TabOrder = 11
      Visible = False
      object Label7: TLabel
        Left = 16
        Top = 16
        Width = 156
        Height = 13
        Caption = 'Start Date (yyyy-mm-dd hh:nn):'
      end
      object Label8: TLabel
        Left = 16
        Top = 71
        Width = 89
        Height = 13
        Caption = 'Months increment:'
      end
      object Label9: TLabel
        Left = 16
        Top = 97
        Width = 91
        Height = 13
        Caption = 'Minutes increment:'
      end
      object edtStartDate: TEdit
        Left = 16
        Top = 35
        Width = 161
        Height = 21
        TabOrder = 0
        OnChange = edtStartDateChange
        OnExit = edtStartDateExit
      end
      object spinMonthsIncrement: TSpinEdit
        Left = 143
        Top = 65
        Width = 65
        Height = 22
        MaxValue = 100000
        MinValue = 0
        TabOrder = 1
        Value = 1
        OnChange = spinMonthsIncrementChange
      end
      object spinMinutesIncrement: TSpinEdit
        Left = 143
        Top = 93
        Width = 65
        Height = 22
        MaxValue = 100000
        MinValue = 0
        TabOrder = 2
        Value = 1
        OnChange = spinMinutesIncrementChange
      end
    end
    object chkIgnoreEmptyLines: TCheckBox
      Left = 8
      Top = 192
      Width = 119
      Height = 17
      Caption = 'Ignore empty lines'
      TabOrder = 12
      OnClick = chkIgnoreEmptyLinesClick
    end
    object spinFirstLine: TSpinEdit
      Left = 144
      Top = 21
      Width = 65
      Height = 22
      MaxValue = 100000
      MinValue = 1
      TabOrder = 13
      Value = 1
      OnChange = spinFirstLineChange
    end
    object chkParseFlags: TCheckBox
      Left = 471
      Top = 169
      Width = 153
      Height = 17
      Caption = 'Parse flags'
      TabOrder = 14
      OnClick = chkParseFlagsClick
    end
    object spinFlagsColumn: TSpinEdit
      Left = 592
      Top = 189
      Width = 65
      Height = 22
      EditorEnabled = False
      MaxValue = 100000
      MinValue = 1
      TabOrder = 15
      Value = 1
      OnChange = spinFlagsColumnChange
    end
    object rgrpFlagsDelimiter: TRadioGroup
      Left = 471
      Top = 211
      Width = 220
      Height = 54
      Caption = 'Flags delimiter'
      Columns = 2
      Items.Strings = (
        'Semicolon (;)'
        'White space'
        'Tab character'
        'Other (specify)')
      TabOrder = 16
      OnClick = rgrpFlagsDelimiterClick
    end
    object edtOtherFlagsDelimiter: TEdit
      Left = 592
      Top = 277
      Width = 65
      Height = 21
      TabOrder = 17
      Text = '%'
      OnChange = edtOtherFlagsDelimiterChange
    end
  end
end
