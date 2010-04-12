unit frmimpts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, istrutils, StdCtrls, Grids, Spin, ExtCtrls;

type
  TFrmImportDataToTimeseries = class(TForm)
    btnImportData: TButton;
    memoFileContents: TMemo;
    grpTestParsing: TGroupBox;
    sgrdParseTest: TStringGrid;
    btnTest: TButton;
    grpOptions: TGroupBox;
    cmbEncoding: TComboBox;
    Label1: TLabel;
    chkTrimSpaces: TCheckBox;
    chkOverwrite: TCheckBox;
    chkHaltOnErrors: TCheckBox;
    spinDataColumn: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtNullValueString: TEdit;
    rgrpDecimalSymbol: TRadioGroup;
    rgrpDataDelimiter: TRadioGroup;
    Label4: TLabel;
    edtOtherDelimiter: TEdit;
    chkParseDates: TCheckBox;
    grpParseDatesOptions: TGroupBox;
    Label5: TLabel;
    spinDateColumn: TSpinEdit;
    rgrpDateFormat: TRadioGroup;
    Label6: TLabel;
    edtOtherDateFormat: TEdit;
    grpSpecifyDates: TGroupBox;
    Label7: TLabel;
    edtStartDate: TEdit;
    Label8: TLabel;
    spinMonthsIncrement: TSpinEdit;
    Label9: TLabel;
    spinMinutesIncrement: TSpinEdit;
    chkIgnoreEmptyLines: TCheckBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label10: TLabel;
    spinFirstLine: TSpinEdit;
    chkParseFlags: TCheckBox;
    Label11: TLabel;
    spinFlagsColumn: TSpinEdit;
    rgrpFlagsDelimiter: TRadioGroup;
    Label12: TLabel;
    edtOtherFlagsDelimiter: TEdit;
    procedure FormShow(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure cmbEncodingChange(Sender: TObject);
    procedure chkTrimSpacesClick(Sender: TObject);
    procedure chkOverwriteClick(Sender: TObject);
    procedure chkHaltOnErrorsClick(Sender: TObject);
    procedure spinDataColumnChange(Sender: TObject);
    procedure edtNullValueStringChange(Sender: TObject);
    procedure rgrpDecimalSymbolClick(Sender: TObject);
    procedure rgrpDataDelimiterClick(Sender: TObject);
    procedure edtOtherDelimiterChange(Sender: TObject);
    procedure chkParseDatesClick(Sender: TObject);
    procedure spinDateColumnChange(Sender: TObject);
    procedure rgrpDateFormatClick(Sender: TObject);
    procedure edtOtherDateFormatChange(Sender: TObject);
    procedure edtStartDateChange(Sender: TObject);
    procedure edtStartDateExit(Sender: TObject);
    procedure spinMonthsIncrementChange(Sender: TObject);
    procedure spinMinutesIncrementChange(Sender: TObject);
    procedure chkIgnoreEmptyLinesClick(Sender: TObject);
    procedure spinFirstLineChange(Sender: TObject);
    procedure chkParseFlagsClick(Sender: TObject);
    procedure spinFlagsColumnChange(Sender: TObject);
    procedure rgrpFlagsDelimiterClick(Sender: TObject);
    procedure edtOtherFlagsDelimiterChange(Sender: TObject);
  private
    FFileName: string;
    FOptions: TImportDataToTimeseriesOptions;
    procedure SetControlStatus;
    procedure ReadFileToMemo;
    procedure ClearGrid;
  public
    property Options: TImportDataToTimeseriesOptions read FOptions
      write FOptions;
    property FileName: string read FFileName write FFileName;
  end;

implementation

{$R *.dfm}

uses Math, Dates;

procedure TFrmImportDataToTimeseries.btnTestClick(Sender: TObject);
var
  AInStream, AOutStream: TMemoryStream;
  AStreamReader: TStreamReader;
  s: string;
  i, SavedFirstLine: Integer;
begin
  AInStream := nil;
  AOutStream := nil;
  AStreamReader := nil;
  SavedFirstLine := FOptions.FirstLine;
  try
    FOptions.FirstLine := 1;
    AInStream := TMemoryStream.Create;
    AOutStream := TMemoryStream.Create;
    memoFileContents.Lines.SaveToStream(AInStream, FOptions.Encoding);
    AInStream.Seek(0,0);
    ImportDataToTimeseries(AInStream, AOutStream, FOptions);
    AOutStream.Seek(0,0);
    AStreamReader := TStreamReader.Create(AOutStream, FOptions.Encoding);
    sgrdParseTest.RowCount := memoFileContents.Lines.Count;
    i := 0;
    while not AStreamReader.EndOfStream do
    begin
      s := AStreamReader.ReadLine;
      sgrdParseTest.Cells[0, i] :=
        DelimitedStringItem(s, 1, ',');
      sgrdParseTest.Cells[1, i] :=
        DelimitedStringItem(s, 2, ',');
      sgrdParseTest.Cells[2, i] :=
        DelimitedStringItem(s, 3, ',');
      Inc(i);
    end;
  finally
    FOptions.FirstLine := SavedFirstLine;
    AInStream.Free;
    AOutStream.Free;
  end;
end;

procedure TFrmImportDataToTimeseries.chkHaltOnErrorsClick(Sender: TObject);
begin
  with FOptions do
    with (Sender as TCheckBox) do
      HaltOnError := Checked;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.chkIgnoreEmptyLinesClick(Sender: TObject);
begin
  with FOptions do
    with (Sender as TCheckBox) do
      IgnoreEmptyLines := Checked;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.chkOverwriteClick(Sender: TObject);
begin
  with FOptions do
    with (Sender as TCheckBox) do
      Overwrite := Checked;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.chkParseDatesClick(Sender: TObject);
begin
  with FOptions do
    with Sender as TCheckBox do
      if not Checked then
        DateColumn := 0 else
        DateColumn := spinDateColumn.Value;
  SetControlStatus;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.chkParseFlagsClick(Sender: TObject);
begin
  with FOptions do
    with Sender as TCheckBox do
      if not Checked then
        FlagsColumn := 0 else
        FlagsColumn := spinFlagsColumn.Value;
  SetControlStatus;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.chkTrimSpacesClick(Sender: TObject);
begin
  with FOptions do
    with (Sender as TCheckBox) do
      TrimSpaces := Checked;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.ClearGrid;
var
  i: Integer;
begin
  for i := 0 to 9 do
  begin
    sgrdParseTest.Cells[0, i] := '';
    sgrdParseTest.Cells[1, i] := '';
    sgrdParseTest.Cells[2, i] := '';
  end;
end;

procedure TFrmImportDataToTimeseries.cmbEncodingChange(Sender: TObject);
begin
  with FOptions do
    case cmbEncoding.ItemIndex of
      0: Encoding := TEncoding.UTF8;
      1: Encoding := TEncoding.ASCII;
      2: Encoding := TEncoding.Default;
      3: Encoding := TEncoding.UTF7;
      4: Encoding := TEncoding.Unicode;
      5: Encoding := TEncoding.BigEndianUnicode;
    else
      Assert(False);
    end;
  ReadFileToMemo;
  sgrdParseTest.RowCount := 10;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.edtNullValueStringChange(Sender: TObject);
begin
  with FOptions do
    NullValueString := edtNullValueString.Text;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.edtOtherDateFormatChange(Sender: TObject);
begin
  with Sender as TEdit do
    with FOptions do
      if Text <> '' then
        DateFormat := Text;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.edtOtherDelimiterChange(Sender: TObject);
begin
  with Sender as TEdit do
    with FOptions do
      if Text <> '' then
        Delimiter := PChar(Text)[0];
end;

procedure TFrmImportDataToTimeseries.edtOtherFlagsDelimiterChange(
  Sender: TObject);
begin
  with Sender as TEdit do
    with FOptions do
      if Text <> '' then
        FlagsDelimiter := PChar(Text)[0];
end;

procedure TFrmImportDataToTimeseries.edtStartDateChange(Sender: TObject);
begin
  with Sender as TEdit do
    with FOptions do
      if Text <> '' then
        try
          StartDate := FormatStrToDateTime('yyyy-mm-dd hh:nn',PChar(Text));
        except
        end;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.edtStartDateExit(Sender: TObject);
begin
  with Sender as TEdit do
    with FOptions do
      if Text <> '' then
        try
          StartDate := FormatStrToDateTime('yyyy-mm-dd hh:nn',PChar(Text));
        except
          SetFocus;
          raise;
        end;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.FormShow(Sender: TObject);
begin
  sgrdParseTest.DefaultRowHeight :=
    sgrdParseTest.DefaultRowHeight * Screen.PixelsPerInch div 96;
  grpSpecifyDates.Left := grpParseDatesOptions.Left;
  grpSpecifyDates.Top := grpParseDatesOptions.Top;
  ReadFileToMemo;
end;

procedure TFrmImportDataToTimeseries.ReadFileToMemo;
var
  AStreamReader: TStreamReader;
  AFileStream: TFileStream;
  i: Integer;
  s: string;
begin
  AStreamReader := nil;
  AFileStream := nil;
  try
    memoFileContents.Visible := False;
    memoFileContents.Clear;
    AFileStream := TFileStream.Create(FFileName,
      fmOpenRead or fmShareDenyWrite);
    AStreamReader := TStreamReader.Create(AFileStream, FOptions.Encoding);
    i := 0;
    while not AStreamReader.EndOfStream do
    begin
      s := AStreamReader.ReadLine;
      if (i+1)>=FOptions.FirstLine then
        memoFileContents.Lines.Add(s);
      Inc(i);
      if i>=199+FOptions.FirstLine then Break;
    end;
  finally
    SetControlStatus;
    memoFileContents.Visible := True;
    if AStreamReader<>nil then AStreamReader.Close;
    AStreamReader.Free;
    AFileStream.Free;
  end;
end;

procedure TFrmImportDataToTimeseries.rgrpDataDelimiterClick(Sender: TObject);
begin
  edtOtherDelimiter.Enabled := False;
  with FOptions do
    with Sender as TRadioGroup do
      case ItemIndex of
        0: Delimiter := ',';
        1: Delimiter := ';';
        2: Delimiter := ' ';
        3: Delimiter := #9;
        4: Delimiter := '$';
        5: begin
             edtOtherDelimiter.Enabled := True;
             Delimiter := PChar(edtOtherDelimiter.Text)[0];
        end;
        else
          Assert(False);
      end;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.rgrpDateFormatClick(Sender: TObject);
begin
  edtOtherDateFormat.Enabled := False;
  with FOptions do
    with Sender as TRadioGroup do
      case ItemIndex of
        0: DateFormat := '';
        1: DateFormat := 'dd/mm/yyyy';
        2: DateFormat := 'yyyy-mm';
        3: DateFormat := 'yyyy-mm-ddThh:nn';
        4: begin
          edtOtherDateFormat.Enabled := True;
          DateFormat := edtOtherDateFormat.Text;
        end;
        else
          Assert(False);
      end;
end;

procedure TFrmImportDataToTimeseries.rgrpDecimalSymbolClick(Sender: TObject);
begin
  with FOptions do
    with Sender as TRadioGroup do
      case ItemIndex of
        0: DecimalSymbol := '.';
        1: DecimalSymbol := ',';
      else
        Assert(False);
      end;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.rgrpFlagsDelimiterClick(Sender: TObject);
begin
  edtOtherFlagsDelimiter.Enabled := False;
  with FOptions do
    with Sender as TRadioGroup do
      case ItemIndex of
        0: FlagsDelimiter := ';';
        1: FlagsDelimiter := ' ';
        2: FlagsDelimiter := #9;
        3: begin
             edtOtherFlagsDelimiter.Enabled := True;
             FlagsDelimiter := PChar(edtOtherFlagsDelimiter.Text)[0];
        end;
        else
          Assert(False);
      end;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.SetControlStatus;
begin
  with FOptions do
  begin
    if Encoding = TEncoding.UTF8 then cmbEncoding.ItemIndex := 0
    else if Encoding = TEncoding.ASCII then cmbEncoding.ItemIndex := 1
    else if Encoding = TEncoding.Default then cmbEncoding.ItemIndex := 2
    else if Encoding = TEncoding.UTF7 then cmbEncoding.ItemIndex := 3
    else if Encoding = TEncoding.Unicode then cmbEncoding.ItemIndex := 4
    else if Encoding = TEncoding.BigEndianUnicode then
      cmbEncoding.ItemIndex := 5
    else Assert(False);
    if DecimalSymbol = '.' then rgrpDecimalSymbol.ItemIndex := 0 else
    if DecimalSymbol = ',' then rgrpDecimalSymbol.ItemIndex := 1 else
      Assert(False);
    edtOtherDelimiter.Enabled := False;
    if Delimiter = ',' then rgrpDataDelimiter.ItemIndex := 0 else
    if Delimiter = ';' then rgrpDataDelimiter.ItemIndex := 1 else
    if Delimiter = ' ' then rgrpDataDelimiter.ItemIndex := 2 else
    if Delimiter = #9 then rgrpDataDelimiter.ItemIndex := 3 else
    if Delimiter = '$' then rgrpDataDelimiter.ItemIndex := 4 else
    begin
      rgrpDataDelimiter.ItemIndex := 5;
      edtOtherDelimiter.Enabled := True;
    end;
    chkTrimSpaces.Checked := TrimSpaces;
    chkOverwrite.Checked := Overwrite;
    chkHaltOnErrors.Checked := HaltOnError;
    chkIgnoreEmptyLines.Checked := IgnoreEmptyLines;
    spinFirstLine.Value := FirstLine;
    spinDataColumn.Value := DataColumn;
    if DateColumn<>spinDateColumn.Value then
      spinDateColumn.Value := Min(DateColumn,1);
    if spinFlagsColumn.Value<>FlagsColumn then
      spinFlagsColumn.Value := Min(FlagsColumn,1);
    spinMonthsIncrement.Value := DateIncrementMonths;
    spinMinutesIncrement.Value := DateIncrementMinutes;
    edtNullValueString.Text := NullValueString;
    chkParseDates.Checked := (DateColumn>0);
    chkParseFlags.Checked := (FlagsColumn>0);
    rgrpFlagsDelimiter.Enabled := chkParseFlags.Checked;
    spinFlagsColumn.Enabled := chkParseFlags.Checked;
    edtOtherFlagsDelimiter.Enabled := False;
    if FlagsDelimiter = ';' then rgrpFlagsDelimiter.ItemIndex := 0 else
    if FlagsDelimiter = ' ' then rgrpFlagsDelimiter.ItemIndex := 1 else
    if FlagsDelimiter = #9 then rgrpFlagsDelimiter.ItemIndex := 2 else
    begin
      rgrpFlagsDelimiter.ItemIndex := 3;
      edtOtherFlagsDelimiter.Enabled := rgrpFlagsDelimiter.Enabled;
    end;
    grpParseDatesOptions.Visible := chkParseDates.Checked;
    grpSpecifyDates.Visible := not grpParseDatesOptions.Visible;
    edtOtherDateFormat.Enabled := False;
    if DateFormat = '' then rgrpDateFormat.ItemIndex := 0 else
    if DateFormat = 'dd/mm/yyyy' then rgrpDateFormat.ItemIndex := 1 else
    if DateFormat = 'yyyy-mm' then rgrpDateFormat.ItemIndex := 2 else
    if DateFormat = 'yyyy-mm-ddThh:nn' then rgrpDateFormat.ItemIndex := 3 else
    begin
      rgrpDateFormat.ItemIndex := 4;
      edtOtherDateFormat.Enabled := True;
    end;
    edtStartDate.Text :=
      FormatDateTime('yyyy-mm-dd hh:nn', StartDate);
  end;
end;

procedure TFrmImportDataToTimeseries.spinDataColumnChange(Sender: TObject);
begin
  with FOptions do
    DataColumn := spinDataColumn.Value;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.spinDateColumnChange(Sender: TObject);
begin
  if not chkParseDates.Checked then Exit;
  with FOptions do
    DateColumn := spinDateColumn.Value;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.spinFirstLineChange(Sender: TObject);
begin
  with FOptions do
    FirstLine := spinFirstLine.Value;
  ReadFileToMemo;
  sgrdParseTest.RowCount := 10;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.spinFlagsColumnChange(Sender: TObject);
begin
  if not chkParseFlags.Checked then Exit;
  with FOptions do
    FlagsColumn := spinFlagsColumn.Value;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.spinMinutesIncrementChange(
  Sender: TObject);
begin
  with FOptions do
    DateIncrementMinutes := spinMinutesIncrement.Value;
  ClearGrid;
end;

procedure TFrmImportDataToTimeseries.spinMonthsIncrementChange(Sender: TObject);
begin
  with FOptions do
    DateIncrementMonths := spinMonthsIncrement.Value;
  ClearGrid;
end;

end.
