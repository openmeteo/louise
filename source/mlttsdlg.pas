{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit mlttsdlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ts, StdCtrls, Grids, tsgrid, Menus,
  Dates, icomponent, tsdialogs, ImgList, ComCtrls, ToolWin;

type
  TFrmMultiTimeseries = class(TForm)
    TimeseriesGrid: TTimeseriesGrid;
    PopupMenu: TPopupMenu;
    MainMenu: TMainMenu;
    mnuSeries: TMenuItem;
    mnuEdit: TMenuItem;
    mnuCopyToClipboard: TMenuItem;
    mnuCopyWithDates: TMenuItem;
    mnuPaste: TMenuItem;
    N1: TMenuItem;
    mnuInsertSection: TMenuItem;
    mnuAddSection: TMenuItem;
    mnuDeleteSection: TMenuItem;
    mnuAddRecordsEnd: TMenuItem;
    mnuDeleteRecords: TMenuItem;
    mnuAddRecordsBefore: TMenuItem;
    N2: TMenuItem;
    mnuLoadFromFile: TMenuItem;
    mnuWriteToFile: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    mnuClose: TMenuItem;
    pmnuAddRecordsStart: TMenuItem;
    pmnuAddRecordsEnd: TMenuItem;
    pmnuDeleteRecords: TMenuItem;
    pmnuInsertSections: TMenuItem;
    pmnuRemoveSections: TMenuItem;
    pmnuAddSections: TMenuItem;
    pmnuCopy: TMenuItem;
    pmnuCopyDates: TMenuItem;
    pmnuPaste: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    mnuResetData: TMenuItem;
    mnuView: TMenuItem;
    mnuShowStatistics: TMenuItem;
    mnuGraphs: TMenuItem;
    mnuAddTimeseriesGraph: TMenuItem;
    mnuRemoveTimeseriesFromGraph: TMenuItem;
    mnuClearGraph: TMenuItem;
    TimeSeriesGraphForm: TTimeSeriesGraphForm;
    StatisticsForm: TStatisticsForm;
    mnuTools: TMenuItem;
    mnuStatistics: TMenuItem;
    N6: TMenuItem;
    mnuSelectAll: TMenuItem;
    ToolBar: TToolBar;
    tbtnCopy: TToolButton;
    tbtnPaste: TToolButton;
    ToolButton3: TToolButton;
    tbtnGraph: TToolButton;
    tbtnStats: TToolButton;
    ImageList: TImageList;
    mnuRowStatistics: TMenuItem;
    ToolButton1: TToolButton;
    tbtnTableView: TToolButton;
    N7: TMenuItem;
    mnuSeriesProperties: TMenuItem;
    N3: TMenuItem;
    tbtnUndo: TToolButton;
    tbtnRedo: TToolButton;
    ToolButton5: TToolButton;
    N8: TMenuItem;
    mnuUndo: TMenuItem;
    mnuRedo: TMenuItem;
    TimeseriesWizard: TTimeseriesWizard;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAddRecordEndClick(Sender: TObject);
    procedure btnDeleteRecordsClick(Sender: TObject);
    procedure btnCopyClipboardClick(Sender: TObject);
    procedure btnCopyDatesClick(Sender: TObject);
    procedure btnPasteClipboardClick(Sender: TObject);
    procedure btnDeleteSectionsClick(Sender: TObject);
    procedure btnInsertSectionClick(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuLoadFromFileClick(Sender: TObject);
    procedure mnuWriteToFileClick(Sender: TObject);
    procedure TimeseriesGridClick(Sender: TObject);
    procedure TimeseriesGridColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure TimeseriesGridContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure mnuCloseClick(Sender: TObject);
    procedure LFormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuResetDataClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure mnuShowStatisticsClick(Sender: TObject);
    procedure mnuAddTimeseriesGraphClick(Sender: TObject);
    procedure mnuRemoveTimeseriesFromGraphClick(Sender: TObject);
    procedure mnuClearGraphClick(Sender: TObject);
    procedure mnuStatisticsClick(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuRowStatisticsClick(Sender: TObject);
    procedure tbtnTableViewClick(Sender: TObject);
    procedure mnuSeriesPropertiesClick(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuRedoClick(Sender: TObject);
  private
    FMultiTimeseries: TMultiTimeseries;
    FTargetTimestep: TTimeStep;
    FModifiedFlag: Boolean;
    FIsHydrologicalYear: Boolean;
    FReadOnly: Boolean;
    FShowStatistics: Boolean;
    FStatisticsVisible: Boolean;
    FCaptionProperty: string;
    FTimeseriesTitle: string;
    procedure SetControlStatus;
    procedure ReformatStatistics;
    procedure ReadMultiTimeseries(AMultiTimeseries: TMultiTimeseries);
    procedure WriteMultiTimeseries(AMultiTimeseries: TMultiTimeseries);
  public
    property MultiTimeseries: TMultiTimeseries read FMultiTimeseries write
      FMultiTimeseries;
    property TargetTimestep: TTimeStep read FTargetTimestep
      write FTargetTimestep;
    property IsHydrologicalYear: Boolean read FIsHydrologicalYear write
      FIsHydrologicalYear;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property CaptionProperty: string read FCaptionProperty write
      FCaptionProperty;
  end;

implementation

{$R *.dfm}

uses uiutils, clipbrd, DateUtils, Math, GenUtils;

const
  StatsColsCount = 14;

resourcestring
  rsSection = 'Section: ';
  rsMultiTimeseriesView = 'Multi timeseries view';
  rsModified = ' (Modified)';
  rsUndo = 'Undo ';
  rsRedo = 'Redo ';

procedure TFrmMultiTimeseries.SetControlStatus;
var
  i: Integer;
  StatsCols: Integer;
  ReadWrite: Boolean;
begin
  if FStatisticsVisible then StatsCols := StatsColsCount else StatsCols := 0;
  ReadWrite := (not FReadOnly) and (not FShowStatistics);
  TimeseriesGrid.ReadOnly := (not ReadWrite);
  for i := 0 to TimeseriesGrid.Count-1-StatsCols do
    TimeseriesGrid.Data[i].Title := rsSection + IntToStr(i+1);
  for i := 0 to TimeseriesGrid.Count-1-StatsCols do
    if TimeseriesGrid.Data[i].Modified then FModifiedFlag := True;
  if FCaptionProperty<>'' then
    Caption := FCaptionProperty else
    if FMultiTimeseries<>nil then
      Caption := FTimeseriesTitle;
  if FModifiedFlag then
    Caption := Caption+rsModified else
  mnuCopyToClipboard.Enabled := (TimeseriesGrid.Count>0);
  mnuCopyWithDates.Enabled := (TimeseriesGrid.Count>0);
  pmnuCopy.Enabled := mnuCopyToClipboard.Enabled;
  pmnuCopyDates.Enabled := mnuCopyToClipboard.Enabled;
  mnuPaste.Enabled := (TimeseriesGrid.Count>0) and ReadWrite;
  pmnuPaste.Enabled := mnuPaste.Enabled;
  mnuSelectAll.Enabled := (TimeseriesGrid.Count>0);
  mnuDeleteRecords.Enabled := (TimeseriesGrid.Count>0) and
    (TimeseriesGrid.Data[0].Count>0) and ReadWrite;
  pmnuDeleteRecords.Enabled := mnuDeleteRecords.Enabled;
  mnuDeleteSection.Enabled := (TimeseriesGrid.Count>1) and ReadWrite;
  pmnuRemoveSections.Enabled := mnuDeleteSection.Enabled;
  mnuAddRecordsEnd.Enabled := (TimeseriesGrid.Count>0) and ReadWrite;
  pmnuAddRecordsStart.Enabled := mnuAddRecordsEnd.Enabled;
  pmnuAddRecordsEnd.Enabled := mnuAddRecordsEnd.Enabled;
  mnuAddRecordsBefore.Enabled := (TimeseriesGrid.Count>0) and ReadWrite;
  mnuWriteToFile.Enabled := (TimeseriesGrid.Count>0);
  mnuAddSection.Enabled := ReadWrite;
  mnuInsertSection.Enabled := mnuAddSection.Enabled;
  pmnuInsertSections.Enabled := mnuAddSection.Enabled;
  pmnuAddSections.Enabled := mnuAddSection.Enabled;
  mnuResetData.Enabled := ReadWrite;
  mnuLoadFromFile.Enabled := ReadWrite;
  mnuGraphs.Enabled := (TimeseriesGrid.Count>0);
  tbtnGraph.Enabled := mnuGraphs.Enabled;
  mnuTools.Enabled := (TimeseriesGrid.Count>0);
  mnuShowStatistics.Checked := FShowStatistics;
  if TimeseriesGrid.Count<1 then
    tbtnTableView.Enabled := False else
    tbtnTableView.Enabled := (TimeseriesGrid.Data[0].Count>0) and
      (TimeseriesGrid.Data[0].TimeStep = tstMonthly);
  if tbtnTableView.Down and tbtnTableView.Enabled then
    TimeseriesGrid.DisplayFormat := dfTable else
    TimeseriesGrid.DisplayFormat := dfSimple;
  if TimeseriesGrid.DisplayFormat = dfTable then
     TimeseriesGrid.StatisticsVisible := True else
     TimeseriesGrid.StatisticsVisible := False;
  mnuAddSection.Enabled := mnuAddSection.Enabled and
    (TimeseriesGrid.DisplayFormat = dfSimple);
  pmnuAddSections.Enabled := mnuAddSection.Enabled;
  mnuInsertSection.Enabled := mnuAddSection.Enabled;
  pmnuInsertSections.Enabled := mnuAddSection.Enabled;
  mnuDeleteSection.Enabled := mnuDeleteSection.Enabled and
    (TimeseriesGrid.DisplayFormat = dfSimple);
  pmnuRemoveSections.Enabled := mnuDeleteSection.Enabled;
  mnuShowStatistics.Enabled := (TimeseriesGrid.DisplayFormat = dfSimple);
  tbtnStats.Enabled := (TimeseriesGrid.DisplayFormat = dfSimple);
  if FStatisticsVisible<>FShowStatistics then
    ReformatStatistics;
  mnuResetData.Enabled := mnuResetData.Enabled and
    (TimeseriesGrid.DisplayFormat = dfSimple);
  { Undo - redo controls }
  mnuUndo.Enabled := (TimeseriesGrid.Count>0) and
    (TimeseriesGrid.DisplayFormat = dfSimple) and
      (TimeseriesGrid.CanUndo(TimeseriesGrid.ActiveIndex));
  if mnuUndo.Enabled then
    mnuUndo.Caption := rsUndo +
      TimeseriesGrid.UndoCaption(TimeseriesGrid.ActiveIndex)+'...'
  else
    mnuUndo.Caption := rsUndo;
  mnuRedo.Enabled := (TimeseriesGrid.Count>0) and
    (TimeseriesGrid.DisplayFormat = dfSimple) and
      (TimeseriesGrid.CanRedo(TimeseriesGrid.ActiveIndex));
  if mnuRedo.Enabled then
    mnuRedo.Caption := rsRedo +
      TimeseriesGrid.RedoCaption(TimeseriesGrid.ActiveIndex)+'...'
  else
    mnuRedo.Caption := rsRedo;
  tbtnUndo.Enabled := mnuUndo.Enabled;
  tbtnUndo.Hint := mnuUndo.Caption;
  tbtnRedo.Enabled := mnuRedo.Enabled;
  tbtnRedo.Hint := mnuRedo.Caption;
end;

procedure TFrmMultiTimeseries.ReadMultiTimeseries(
  AMultiTimeseries: TMultiTimeseries);
var
  i: Integer;
  ATimeseries: TTimeseries;
begin
  for i := TimeseriesGrid.Count-1 downto 0 do TimeseriesGrid.Delete(i);
  ATimeseries  := nil;
  for i := 1 to AMultiTimeseries.SectionCount do
  begin
    try
      ATimeseries := TTimeseries.Create;
      ATimeseries.Assign(AMultiTimeseries.Sections[i]);
      TimeseriesGrid.Add(ATimeseries);
      ATimeseries := nil;
    except
      ATimeseries.Free;
      raise;
    end;
  end;
  if TimeseriesGrid.Count>0 then
  begin
    TimeseriesGrid.Update; // Needs to redisplay itself to register the change
                           // of number of columns before changing ActiveIndex;
                           // possibly a tsgrid bug.
    TimeseriesGrid.ActiveIndex := 0;
    SetFocusedControl(TimeseriesGrid);
    SetFocus;
  end;
  FTimeseriesTitle := AMultiTimeseries.Name;
end;

procedure TFrmMultiTimeseries.WriteMultiTimeseries(
  AMultiTimeseries: TMultiTimeseries);
var
  i: Integer;
  ATimeseries: TTimeseries;
  StatsCols: Integer;
begin
  if FStatisticsVisible then StatsCols := StatsColsCount else StatsCols := 0;
  AMultiTimeseries.ClearSections;
  ATimeseries := nil;
  for i := 0 to TimeseriesGrid.Count-1-StatsCols do
  begin
    try
      ATimeseries := TTimeseries.Create;
      ATimeseries.Assign(TimeseriesGrid.Data[i]);
      AMultiTimeseries.AddSection(ATimeseries);
      ATimeseries := nil;
      TimeseriesGrid.Data[i].Modified := False;
    except
      ATimeseries.Free;
      raise;
    end;
  end;
  AMultiTimeseries.Name := FTimeseriesTitle;
end;

procedure TFrmMultiTimeseries.FormCreate(Sender: TObject);
begin
  Caption := rsMultiTimeseriesView;
  FMultiTimeseries := nil;
  FTargetTimestep := tstMonthly;
  FModifiedFlag := False;
  FIsHydrologicalYear := False;
  FReadOnly := False;
  FShowStatistics := False;
  FStatisticsVisible := False;
  FTimeseriesTitle := '';
end;

procedure TFrmMultiTimeseries.FormShow(Sender: TObject);
begin
  Assert(FMultiTimeseries<>nil);
    ReadMultiTimeseries(FMultiTimeseries);
  SetControlStatus;
end;

resourcestring
  rsEnterNumberOfNewRecords =
    'Enter the number of the new records';
  rsNoDateEnter =
    'No records are existing, specify starting date...';
  rsInsertRecords = 'Insert records';

procedure TFrmMultiTimeseries.btnAddRecordEndClick(Sender: TObject);
var
  i, j, ACount, AStep: Integer;
  s: string;
  ADate: TDateTime;
  ADateList: TDateTimeList;
  DateFormat: string;
  Options: TGetDateFormatOptions;
  StartFromScratchFlag: Boolean;
begin
 if TimeseriesGrid.Count<1 then
    Exit;
  StartFromScratchFlag := False;
  AStep := 1; {dummy}
  case TMenuItem(Sender).Tag of
    0: AStep := -1;
    1: AStep := 1;
  else
    Assert(False);
  end;
  s := '1';
  if not InputQuery(rsEnterNumberOfNewRecords, rsEnterNumberOfNewRecords,
    s) then Exit;
  ACount := StrToInt(s);
  if TimeseriesGrid.Data[0].Count<1 then
  begin
    s := '';
    if not InputQuery(rsNoDateEnter, rsNoDateEnter, s) then Exit;
    Options := [gdfRaiseOnFail];
    if (TimeseriesGrid.Data[0].TimeStep = tstAnnual) and
      (TimeseriesGrid.Data[0].HydrologicalYear) then
      Options := Options + [gdfAllowHydrologicalYear];
    DateFormat := GetDateFormat(s, Options);
    ADate := FormatStrToDateTime(DateFormat, s);
    StartFromScratchFlag := True;
  end else
    if AStep = 1 then
      ADate := TimeseriesGrid.Data[0].Last.Date
    else
      ADate := TimeseriesGrid.Data[0].First.Date;
  ADateList := nil;
  TimeseriesGrid.IncUndoIDPointer;
  try try
    ADateList := TDateTimeList.Create;
    for j := 0 to ACount-1 do
    begin
      if not StartFromScratchFlag then
        ADate:= TimeseriesGrid.Data[0].TimeStep.IncStep(ADate, AStep);
      StartFromScratchFlag := False;
      ADateList.Add(ADate);
    end;
    with TimeseriesGrid do
      for i := 0 to Count-1 do
        PrepareBuffer(i, Min(ADateList.First, ADateList.Last),
          Max(ADateList.First, ADateList.Last), rsInsertRecords, UndoIDPointer);
    for j := 0 to ADateList.Count-1 do
      for i := 0 to TimeseriesGrid.Count-1 do
        with TimeseriesGrid.Data[i] do
          Insert(ADateList[j], True, 0, '', msNew);
    for i := 0 to TimeseriesGrid.Count-1 do
      TimeseriesGrid.FinalizeBuffer(i);          
    FModifiedFlag := True;
  finally
    ADateList.Free;
  end;
  except
    for i := 0 to TimeseriesGrid.Count-1 do
      TimeseriesGrid.RevertBuffer(i);
    raise;
  end;
  SetControlStatus;
end;

resourcestring
  rsDeleteRecords = 'Delete records';

procedure TFrmMultiTimeseries.btnDeleteRecordsClick(Sender: TObject);
var
  i, j: Integer;
begin
  if TimeseriesGrid.Count<1 then Exit;
  if TimeseriesGrid.Data[0].Count<1 then Exit;
  TimeseriesGrid.IncUndoIDPointer;
  for i := 0 to TimeseriesGrid.Count-1 do
    with TimeseriesGrid do
      PrepareBuffer(i, Data[i].Items[Selection.Top-1].Date,
        Data[i].Items[Selection.Bottom-1].Date, rsDeleteRecords, UndoIDPointer);
  try
    for j := TimeseriesGrid.Selection.Bottom-1 downto
      TimeseriesGrid.Selection.Top-1 do
      for i := 0 to TimeseriesGrid.Count-1 do
        TimeseriesGrid.Data[i].Delete(j);
    for i := 0 to TimeseriesGrid.Count-1 do
      TimeseriesGrid.FinalizeBuffer(i);
  except
    for i := 0 to TimeseriesGrid.Count-1 do
      TimeseriesGrid.RevertBuffer(i);
    raise;
  end;
  FModifiedFlag := True;  
  SetControlStatus;
end;

procedure TFrmMultiTimeseries.btnCopyClipboardClick(Sender: TObject);
begin
  TimeseriesGrid.CopyToClipboard(TimeseriesGrid.Selection);
end;

procedure TFrmMultiTimeseries.btnCopyDatesClick(Sender: TObject);
begin
  TimeseriesGrid.CopyToClipboardWithDates(TimeseriesGrid.Selection);
end;

procedure TFrmMultiTimeseries.btnPasteClipboardClick(Sender: TObject);
begin
  TimeseriesGrid.PasteFromClipboard(TimeseriesGrid.Selection);
  FModifiedFlag := True;
  SetControlStatus;
end;

procedure TFrmMultiTimeseries.btnDeleteSectionsClick(Sender: TObject);

  procedure CloseSingleTimeseries(AIndex: Integer);
  begin
{Remove from TSGraph}
    if TimeSeriesGraphForm.Visible then
      TimeSeriesGraphForm.Remove(TimeseriesGrid.Data[AIndex]);
    TimeseriesGrid.Delete(AIndex);
  end;

var
  i: Integer;
begin
  for i := TimeseriesGrid.Selection.Right downto
    TimeseriesGrid.Selection.Left do
      CloseSingleTimeseries(i-1);
  FModifiedFlag := True;
  SetControlStatus;
end;

resourcestring
  rsEnterNumberOfSections = 'Enter the number of new sections';

procedure TFrmMultiTimeseries.btnInsertSectionClick(Sender: TObject);
var
  ATimeseries: TTimeseries;
  i, j, ACount: Integer;
  s: string;
begin
  s := '1';
  if not InputQuery(rsEnterNumberOfSections, rsEnterNumberOfSections, s) then
    Exit;
  ACount := StrToInt(s);
  ATimeseries := nil;
  for j := 0 to ACount-1 do
  begin
    try
      ATimeseries := TTimeseries.Create;
      if TimeseriesGrid.Count>0 then
        ATimeseries.Assign(TimeseriesGrid.Data[0])
      else begin
        ATimeseries.TimeStep := FTargetTimestep;
        if ATimeseries.TimeStep = tstAnnual then
          ATimeseries.SetHydrologicalYear(FIsHydrologicalYear);
      end;
      for i := 0 to ATimeseries.Count-1 do
        ATimeseries[i].SetNull;
      if (TimeseriesGrid.Count< 1) or (TMenuItem(Sender).Tag = 0) then
        TimeseriesGrid.Add(ATimeseries)
      else
        TimeseriesGrid.Insert(TimeseriesGrid.ActiveIndex, ATimeseries);
      ATimeseries := nil;
      TimeseriesGrid.Update; // Needs to redisplay itself to register the change
                             // of number of columns before changing ActiveIndex;
                             // possibly a tsgrid bug.
      SetFocusedControl(TimeseriesGrid);
      SetFocus;
    except
      ATimeseries.Free;
      raise;
    end;
  end;
  FModifiedFlag := True;
  SetControlStatus;
end;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}

procedure TFrmMultiTimeseries.mnuEditClick(Sender: TObject);
begin
  if TimeSeriesGrid.Count > 0 then
    if (StrLen(PChar((Clipboard.AsText)))) <1 then
      mnuPaste.Enabled := False
    else
      mnuPaste.Enabled := True and (not FReadOnly) and (not FShowStatistics);
end;

procedure TFrmMultiTimeseries.PopupMenuPopup(Sender: TObject);
begin
  if TimeSeriesGrid.Count > 0 then
    if (StrLen(PChar((Clipboard.AsText)))) <1 then
      pmnuPaste.Enabled := False
    else
      pmnuPaste.Enabled := True and (not FReadOnly) and (not FShowStatistics);
end;

{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CODE ON}

procedure TFrmMultiTimeseries.mnuLoadFromFileClick(Sender: TObject);
var
  AMultiTimeseries: TMultiTimeseries;
  ACursor: TCursor;
begin
  if not OpenDialog.Execute then Exit;
  ACursor := Screen.Cursor;
  AMultiTimeseries := nil;
  try
    AMultiTimeseries := TMultiTimeseries.Create;
    Screen.Cursor := crHourGlass;
    AMultiTimeseries.LoadFromFile(OpenDialog.FileName);
    FTimeseriesTitle := AMultiTimeseries.Name;
    ReadMultiTimeseries(AMultiTimeseries);
  finally
    Screen.Cursor := ACursor;
    AMultiTimeseries.Free;
  end;
  FModifiedFlag := True;
  SetControlStatus;
end;

resourcestring
  rsOvewriteExistingFile = 'File already exists. Do you want to overwrite the '+
    'existing file?';

procedure TFrmMultiTimeseries.mnuWriteToFileClick(Sender: TObject);
var
  AMultiTimeseries: TMultiTimeseries;
  ACursor: TCursor;
  AVer: Integer;
begin
  if not SaveDialog.Execute then Exit;
  if FileExists(SaveDialog.FileName) then
    if MessageDlg(rsOvewriteExistingFile, mtConfirmation, [mbOK,mbCancel], 0) =
      mrCancel then
        Exit;
  AVer := 2;
  if SaveDialog.FilterIndex =2 then AVer := 1;
  ACursor := Screen.Cursor;
  AMultiTimeseries := nil;
  try
    AMultiTimeseries := TMultiTimeseries.Create;
    WriteMultiTimeseries(AMultiTimeseries);
    Screen.Cursor := crHourGlass;
    AMultiTimeseries.WriteToFile(SaveDialog.FileName, nil, AVer);
  finally
    Screen.Cursor := ACursor;
    AMultiTimeseries.Free;
  end;
end;

procedure TFrmMultiTimeseries.TimeseriesGridClick(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmMultiTimeseries.TimeseriesGridColumnMoved(Sender: TObject;
  FromIndex, ToIndex: Integer);
begin
  SetControlStatus;
end;

procedure TFrmMultiTimeseries.TimeseriesGridContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var ARect: TGridRect;
begin
  Handled := False;

  { If more than a cell selected, exit immediately. }
  with TimeseriesGrid.Selection do
    if (Right-Left>0) or (Bottom-Top>0) then Exit;

  { Otherwise, select cell at mouse coordinates. }
  ARect.TopLeft := TimeseriesGrid.MouseCoord(MousePos.X, MousePos.Y);
  ARect.BottomRight := ARect.TopLeft;
  if (ARect.Top>0) and (ARect.Left>0) then
    TimeseriesGrid.Selection := ARect;
end;

procedure TFrmMultiTimeseries.mnuCloseClick(Sender: TObject);
begin
  Close;
end;

resourcestring
  rsSaveChanges = 'Change have been made to the timeseries. Keep changes?';

procedure TFrmMultiTimeseries.LFormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;
  ModalResult := mrNo;
  if not FModifiedFlag then Exit;
  case MessageDlg(rsSaveChanges, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
    mrYes:
    begin
      WriteMultiTimeseries(FMultiTimeseries);
      ModalResult := mrYes;
    end;
    mrCancel: CanClose := False;
  end;
end;

procedure TFrmMultiTimeseries.mnuResetDataClick(Sender: TObject);
var
  i: Integer;
begin
  for i := TimeseriesGrid.Count-1 downto 0 do TimeseriesGrid.Delete(i);
  FModifiedFlag := True;
  SetControlStatus;
end;

procedure TFrmMultiTimeseries.mnuShowStatisticsClick(Sender: TObject);
begin
  FShowStatistics := not FShowStatistics;
  if Sender is TToolButton then
    mnuShowStatistics.Checked := tbtnStats.Down;
  if tbtnStats.Down <> FShowStatistics then
    tbtnStats.Down := FShowStatistics;
  SetControlStatus;
end;

{A function to interpolate between the list of values
derived from simulations}
function InterpolListItem (const l: TFloatList; alpha: Real): Real;
var
  k: Integer;
  m: Real;
  n: Integer;
begin
  n := l.Count;
  m := alpha*n;
  k := trunc (m);
  m := m - k;
  if k <= 0 then
    Result := l.Items[0]
  else if k >= n - 1 then
    Result := l.Items[n - 1]
  else
    Result := l.Items[k] + (l.Items[k + 1] - l.Items[k])* m;
end;

resourcestring
  rsMeanValue = 'Mean value';
  rsCount = 'Count';
  rsStandardDeviation = 'StdDev';
  rsMin = 'Min';
  rsMax = 'Max';

procedure TFrmMultiTimeseries.ReformatStatistics;

  procedure ShowStatistics;
  var
    i, j, k, ACount: Integer;
    ASum, ASum2, AMin, AMax: Real;
    ATimeseriesList: array[1..StatsColsCount] of TTimeseries;
    ASample: TFloatList;
  begin
    if TimeseriesGrid.Count<1 then Exit;
    for k := 1 to StatsColsCount do ATimeseriesList[k] := nil;
    ASample := nil;
    try
      ASample := TFloatList.Create;
      for k := 1 to StatsColsCount do
      begin
        ATimeseriesList[k] := TTimeseries.Create;
        ATimeseriesList[k].Assign(TimeseriesGrid.Data[0]);
      end;
      ATimeseriesList[1].Title := rsCount;
      ATimeseriesList[1].Precision := 0;
      ATimeseriesList[2].Title := rsMeanValue;
      ATimeseriesList[3].Title := rsStandardDeviation;
      ATimeseriesList[4].Title := rsMin;
      ATimeseriesList[5].Title := rsMax;
      for k := 6 to 14 do
        ATimeseriesList[k].Title := IntToStr((k-5)*10)+'%';
      for i := 0 to TimeseriesGrid.Data[0].Count-1 do
      begin
        ASum := 0;
        ASum2 := 0;
        ACount := 0;
        AMin := 1e37;
        AMax := -1e37;
        ASample.Clear;
        for k := 1 to StatsColsCount do ATimeseriesList[k][i].SetNull;
        for j := 0 to TimeseriesGrid.Count-1 do
          if not TimeseriesGrid.Data[j].Items[i].IsNull then
          begin
            ASum := ASum + TimeseriesGrid.Data[j].Items[i].AsFloat;
            ASum2 := ASum2 + Sqr(TimeseriesGrid.Data[j].Items[i].AsFloat);
            AMax := Max(AMax, TimeseriesGrid.Data[j].Items[i].AsFloat);
            AMin := Min(AMin, TimeseriesGrid.Data[j].Items[i].AsFloat);
            ASample.Add(TimeseriesGrid.Data[j].Items[i].AsFloat);
            Inc(ACount);
          end;
        ATimeseriesList[1][i].AsInteger := ACount;
        if ACount>19 then ASample.Sort;
        if ACount>0 then
        begin
          ATimeseriesList[2][i].AsFloat := ASum/ACount;
          ATimeseriesList[4][i].AsFloat := AMin;
          ATimeseriesList[5][i].AsFloat := AMax;
        end;
        if ACount>1 then
          ATimeseriesList[3][i].AsFloat := Sqrt((ASum2-(1/ACount)*Sqr(ASum))/
            (ACount-1));
        if ACount>19 then
        begin
          for k := 6 to 14 do
            ATimeseriesList[k][i].AsFloat :=
              InterpolListItem(ASample, (k-5)*0.1);
        end;
      end;
      for k := 1 to StatsColsCount do
      begin
        ATimeseriesList[k].Modified := False;
        ATimeseriesList[k].StatisticsBgColor := True;
        TimeseriesGrid.Add(ATimeseriesList[k]);
        ATimeseriesList[k] := nil;
      end;
      if TimeseriesGrid.Count>0 then
        TimeseriesGrid.ActiveIndex := TimeseriesGrid.Count-StatsColsCount-1;
      TimeseriesGrid.Update;
      SetFocusedControl(TimeseriesGrid);
      SetFocus;
      if TimeseriesGrid.Count>0 then
        TimeseriesGrid.ActiveIndex := TimeseriesGrid.Count-StatsColsCount;
    finally
      for k := 1 to StatsColsCount do ATimeseriesList[k].Free;
      ASample.Free;
    end;
  end;

  procedure HideStatistics;
  var
    i: Integer;
  begin
    for i := TimeseriesGrid.Count-1 downto
      TimeseriesGrid.Count-StatsColsCount do
        if i>=0 then
          TimeseriesGrid.Delete(i);
  end;

var
  ACursor: TCursor;
begin
  if not mnuShowStatistics.Enabled then Exit;
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    FStatisticsVisible := FShowStatistics;
    if FStatisticsVisible then
      ShowStatistics
    else
      HideStatistics;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmMultiTimeseries.mnuAddTimeseriesGraphClick(Sender: TObject);
var
  ARange: TGridRect;
  i: Integer;
begin
  if not TimeSeriesGraphForm.Visible then
  begin
    TimeSeriesGraphForm.Show;
  end else
  begin
    if TimeSeriesGraphForm.WindowState = wsMinimized then
      TimeSeriesGraphForm.WindowState := wsNormal;
    TimeSeriesGraphForm.BringToFront;
  end;
  if TimeseriesGrid.DisplayFormat = dfSimple then
  begin
    ARange := TimeSeriesGrid.Selection;
    for i := ARange.Left to ARange.Right do
      TimeSeriesGraphForm.Add(TimeSeriesGrid.Data[i-1]);
  end else
    TimeSeriesGraphForm.Add(TimeSeriesGrid.ActiveTimeseries);
end;

procedure TFrmMultiTimeseries.mnuRemoveTimeseriesFromGraphClick(
  Sender: TObject);
var
  ARange: TGridRect;
  i: Integer;
begin
  ARange := TimeSeriesGrid.Selection;
  for i := ARange.Left to ARange.Right do
    TimeSeriesGraphForm.Remove(TimeSeriesGrid.Data[i-1]);
  TimeSeriesGraphForm.BringToFront;
end;

procedure TFrmMultiTimeseries.mnuClearGraphClick(Sender: TObject);
begin
  if TimeSeriesGraphForm.Visible then
  begin
    TimeSeriesGraphForm.ClearArea;
    TimeSeriesGraphForm.BringToFront;
  end;
end;

procedure TFrmMultiTimeseries.mnuStatisticsClick(Sender: TObject);
begin
  StatisticsForm.Timeseries := TimeseriesGrid.ActiveTimeseries;
  StatisticsForm.Execute;
end;

procedure TFrmMultiTimeseries.mnuSelectAllClick(Sender: TObject);
var AGridRect: TGridRect;
begin
  AGridRect.Top := Min(1, TimeseriesGrid.RowCount-1);
  AGridRect.Bottom := TimeseriesGrid.RowCount-1;
  if not TimeseriesGrid.FlagsVisible then
  begin
    AGridRect.Left := TimeseriesGrid.ActiveIndex+1;
    AGridRect.Right := AGridRect.Left;
  end else
  begin
    AGridRect.Left := TimeseriesGrid.ActiveIndex*2+1;
    AGridRect.Right := AGridRect.Left+1;
  end;
  TimeseriesGrid.Selection := AGridRect;
end;

procedure TFrmMultiTimeseries.mnuRowStatisticsClick(Sender: TObject);
var
  ATimeseries: TTimeseries;
  ADate: TDateTime;
  i, ARow: Integer;
begin
  if TimeseriesGrid.Count<1 then Exit;
  if TimeseriesGrid.Data[0].Count<1 then Exit;
  ARow := TimeseriesGrid.Selection.Top-1;
  ATimeseries := nil;
  try
    ATimeseries := TTimeseries.Create;
    ATimeseries.TimeStep := tstAnnual;
    ADate := EncodeDate(YearOf(TimeseriesGrid.Data[0].Items[0].Date),1,1);
    for i := 0 to TimeseriesGrid.Count-1 do
    begin
      if TimeseriesGrid.Data[i].Items[ARow].IsNull then
        ATimeseries.Insert(ADate, True, 0, '', msNew)
      else
        ATimeseries.Insert(ADate, False,
          TimeseriesGrid.Data[i].Items[ARow].AsFloat, '', msNew);
      ADate := IncYear(ADate);
    end;
    StatisticsForm.Timeseries := ATimeseries;
    StatisticsForm.Execute;
  finally
    ATimeseries.Free;
  end;
end;

procedure TFrmMultiTimeseries.tbtnTableViewClick(Sender: TObject);
begin
  SetControlStatus;
end;

resourcestring
  rsTimeseriesPropertiesCaption = 'Time series properties';

procedure TFrmMultiTimeseries.mnuSeriesPropertiesClick(Sender: TObject);
var
  i: Integer;
  SavedTitle: string;
begin
  with TimeseriesWizard do
  begin
    Caption := rsTimeseriesPropertiesCaption;
    HYearOrigin := 9;
    if TimeseriesGrid.Count>0 then
      MainTimeseries := TimeseriesGrid.Data[0]
    else
      Exit;
    try
      SavedTitle := TimeseriesGrid.Data[0].Title;
      MainTimeseries.Title := FTimeseriesTitle;
      NewTimeseriesMode := False;
      if not Execute then
        Exit;
      FTimeseriesTitle := MainTimeseries.Title;
      for i := 1 to TimeseriesGrid.Count-1 do
        TimeseriesGrid.Data[i].AssignMeta(TimeseriesGrid.Data[0]);
    finally
      TimeseriesGrid.Data[0].Title := SavedTitle;
    end;
  end;
  TimeseriesGrid.Reformat;
  TimeseriesGrid.Refresh;
  SetControlStatus;
end;

procedure TFrmMultiTimeseries.mnuUndoClick(Sender: TObject);
var
  ACursor: TCursor;
  AID, i: Integer;
begin
  ACursor := Screen.Cursor;
  AID := TimeseriesGrid.UndoID(TimeseriesGrid.ActiveIndex);
  try
    Screen.Cursor := crAppStart;
    with TimeseriesGrid do
      for i := 0 to Count-1 do
        if UndoID(i) = AID then
          Undo(i);
  finally
    Screen.Cursor := ACursor;
  end;
  TimeseriesGrid.Refresh;
  SetControlStatus;
end;

procedure TFrmMultiTimeseries.mnuRedoClick(Sender: TObject);
var
  ACursor: TCursor;
  AID, i: Integer;
begin
  ACursor := Screen.Cursor;
  AID := TimeseriesGrid.RedoID(TimeseriesGrid.ActiveIndex);
  try
    Screen.Cursor := crAppStart;
    with TimeseriesGrid do
      for i := 0 to Count-1 do
        if RedoID(i) = AID then
          Redo(i);
  finally
    Screen.Cursor := ACursor;
  end;
  TimeseriesGrid.Refresh;
  SetControlStatus;
end;

end.
