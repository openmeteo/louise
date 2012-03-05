{******************************************************************}
{                                                                  }
{  Itia library                                                    }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Stage-discharge curves construction GUI}
unit frmsdcon;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ToolWin, ComCtrls, ExtCtrls, TeeProcs, TeEngine, Chart,
  interpol, Series, contnrs, ImgList, StdCtrls, Grids, GanttCh, tsgrid,
  StrGrdOd, genopts;

const Colors: array [0..7] of TColor = (clBlue, clRed, clGreen, clPurple,
  clDkGray, clMaroon, clNavy, clOlive);

function GetColor(Index: Integer): TColor;

type
  EMyError = class(Exception);
  TSingleAction = class(TTransientCurveList)
  public
    Caption: string;
    procedure Assign(Source: TPersistent); override;
  end;

  TCurvesActionList = class(TObjectList)
  end;

  TFrmSDConstruction = class(TForm)
    ToolBar: TToolBar;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuEdit: TMenuItem;
    chartPeriods: TChart;
    mnuUndo: TMenuItem;
    mnuRedo: TMenuItem;
    tbtnUndo: TToolButton;
    tbtnRedo: TToolButton;
    ImageList: TImageList;
    chartMain: TChart;
    lstCurves: TListBox;
    grpCurvesProperties: TGroupBox;
    tbtnShowAll: TToolButton;
    tbtnShowCurves: TToolButton;
    tbtnShowPoints: TToolButton;
    ToolButton1: TToolButton;
    btnInsert: TButton;
    btnRemove: TButton;
    btnAdd: TButton;
    btnPropertyApply: TButton;
    btnPropertyCancel: TButton;
    edtOffsetSet: TEdit;
    lblOffsetSet: TLabel;
    chkExtensionSet: TCheckBox;
    spinSegmentsSet: TUpDown;
    edtSegmentsSet: TEdit;
    lblSegmentsSet: TLabel;
    btnFitSet: TButton;
    dtpStartDate: TDateTimePicker;
    dtpEndDate: TDateTimePicker;
    lblStartDateSet: TLabel;
    Label1: TLabel;
    dtpStartTime: TDateTimePicker;
    dtpEndTime: TDateTimePicker;
    sgrdCurvePoints: TOdStringGrid;
    mnuTools: TMenuItem;
    mnuChangeDefaultOffset: TMenuItem;
    mnuChangeOffsetToAll: TMenuItem;
    N1: TMenuItem;
    mnuCopyCurve: TMenuItem;
    mnuPasteCurve: TMenuItem;
    N2: TMenuItem;
    mnuCopyChart: TMenuItem;
    mnuLoad: TMenuItem;
    mnuSave: TMenuItem;
    N3: TMenuItem;
    mnuNew: TMenuItem;
    N4: TMenuItem;
    mnuPrintChart: TMenuItem;
    mnuPrinterSetup: TMenuItem;
    N5: TMenuItem;
    mnuExit: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    seriesPeriods: TGanttSeries;
    trackStart: TTrackBar;
    trackEnd: TTrackBar;
    recColor: TShape;
    ToolButton2: TToolButton;
    tbtnMarkOutlier: TToolButton;
    N6: TMenuItem;
    mnuFixOverlapingDates: TMenuItem;
    N7: TMenuItem;
    mnuCalcDetCoef: TMenuItem;
    DisabledImageList: TImageList;
    btnAddExtension: TButton;
    ToolButton3: TToolButton;
    btnHydraulic: TToolButton;
    mnuTimeseries: TMenuItem;
    mnuExportTimeseries: TMenuItem;
    mnuImportTimeseries: TMenuItem;
    mnuWetPeriod: TMenuItem;
    mnuDryPeriod: TMenuItem;
    procedure LFormShow(Sender: TObject);
    procedure LFormCreate(Sender: TObject);
    procedure LFormDestroy(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuRedoClick(Sender: TObject);
    procedure tbtnShowAllClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnPropertyCancelClick(Sender: TObject);
    procedure edtOffsetSetChange(Sender: TObject);
    procedure btnPropertyApplyClick(Sender: TObject);
    procedure spinSegmentsSetClick(Sender: TObject; Button: TUDBtnType);
    procedure btnFitSetClick(Sender: TObject);
    procedure dtpStartDateChange(Sender: TObject);
    procedure sgrdCurvePointsSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: String);
    procedure btnAddClick(Sender: TObject);
    procedure mnuChangeDefaultOffsetClick(Sender: TObject);
    procedure mnuChangeOffsetToAllClick(Sender: TObject);
    procedure mnuCopyCurveClick(Sender: TObject);
    procedure mnuPasteCurveClick(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuPrintChartClick(Sender: TObject);
    procedure mnuPrinterSetupClick(Sender: TObject);
    procedure mnuCopyChartClick(Sender: TObject);
    procedure mnuLoadClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure trackStartChange(Sender: TObject);
    procedure trackEndChange(Sender: TObject);
    procedure tbtnMarkOutlierClick(Sender: TObject);
    procedure chartMainClickSeries(Sender: TCustomChart;
      Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chartPeriodsClickSeries(Sender: TCustomChart;
      Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuFixOverlapingDatesClick(Sender: TObject);
    procedure mnuCalcDetCoefClick(Sender: TObject);
    procedure LFormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnAddExtensionClick(Sender: TObject);
    procedure btnHydraulicClick(Sender: TObject);
    procedure mnuExportTimeseriesClick(Sender: TObject);
    procedure mnuImportTimeseriesClick(Sender: TObject);
    procedure lstCurvesClick(Sender: TObject);
  private
    CurrentIndex: Integer;
    FFixesDates: Boolean;
    FDefaultOffset: Real;
    ActionList: TCurvesActionList;
    CurrentPointer: TSingleAction;
    FMode: TInterpolatingCurveEditingMode;
    DryPeriodStartingMonth, DryPeriodEndingMonth: Integer;
    WetPeriodStartingMonth, WetPeriodEndingMonth: Integer;
    procedure RedrawChartPeriods;
    function GetNewAction(OldAction: TSingleAction;
      MessageStr: string): TSingleAction;
    procedure FinilizeNewAction(AAction: TSingleAction);
    procedure Undo;
    procedure Redo;
    procedure SetCurveList(ACurveList: TTransientCurveList);
    function GetCurveList: TTransientCurveList;
    procedure SetControlStatus;
    procedure SetWetDryMenus;
    procedure PopulateList;
    procedure RedrawChartMain;
    procedure RemoveAndFreeAllSeries;
    procedure SetGrid;
    procedure CheckForChanges;
    procedure SetAxisLimits;
    procedure SetTrackBars;
    procedure SetDatesConsistent(AAction: TSingleAction);
    function CheckForNegatives: Boolean;
    procedure SetWetDryPeriodsClick(Sender: TObject);
    procedure FixWetDry(AAction: TSingleAction);
  public
    TimeseriesGrid: TTimeseriesGrid;
    property Mode: TInterpolatingCurveEditingMode read FMode write FMode;
    property CurveList: TTransientCurveList read GetCurveList
      write SetCurveList;
    procedure ReturnCurveList(ReturnValue: TTransientCurveList);
  end;

implementation

uses Math, Dates, uiUtils, Clipbrd, frmhydxs, frmselsdts;

{$R *.DFM}

function GetColor(Index: Integer): TColor;
begin
  while Index>7 do
    Index := Index-8;
  Result := Colors[Index];
end;

procedure TSingleAction.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSingleAction then
    Caption := TSingleAction(Source).Caption;
end;

procedure TFrmSDConstruction.LFormCreate(Sender: TObject);
begin
  ActionList := nil;
  try
    ActionList := TCurvesActionList.Create(True);
    FDefaultOffset := 1.0;
    FFixesDates := True;
    CurrentIndex := -1;
    CurrentPointer := nil;
    FMode := icemStageDischarge;
    DryPeriodStartingMonth := 4;
    DryPeriodEndingMonth := 9;
    WetPeriodStartingMonth := 10;
    WetPeriodEndingMonth := 3;
  except
    ActionList.Free;
    raise;
  end;
end;

procedure TFrmSDConstruction.LFormDestroy(Sender: TObject);
begin
  ActionList.Free;
  RemoveAndFreeAllSeries;
end;

resourcestring
  rsGridStage = 'Stage (m)';
  rsGridDischarge = 'Discharge (m3/s)';
  rsGridSedimentDischarge = 'Sediment Discharge (kg/s)';

procedure TFrmSDConstruction.LFormShow(Sender: TObject);
begin
  sgrdCurvePoints.DefaultRowHeight :=
    sgrdCurvePoints.DefaultRowHeight * Screen.PixelsPerInch div 96;
  Assert(CurveList<>nil);
  PopulateList;
  if lstCurves.Items.Count >0 then
    lstCurves.ItemIndex := 0;
  SetControlStatus;
{A work-arround: SetControlStatus fails to set these properties to False,
 on first appearance}
  btnPropertyApply.Enabled := False;
  btnPropertyCancel.Enabled := False;
  chartMain.BottomAxis.Title.Caption := rsGridDischarge;
  chartMain.LeftAxis.Title.Caption := rsGridStage;
  if FMode = icemDischargeSedimentDischarge then
  begin
    btnAddExtension.Visible := False;
    btnHydraulic.Visible := False;
    btnInsert.Visible := False;
    btnAdd.Left := btnInsert.Left;
    FDefaultOffset := 0;
    edtOffsetSet.Text := '0';
    lblOffsetSet.Visible := False;
    edtOffsetSet.Visible := False;
    chkExtensionSet.Visible := False;
    chartMain.BottomAxis.Title.Caption := rsGridDischarge;
    chartMain.LeftAxis.Title.Caption := rsGridSedimentDischarge;
    mnuWetPeriod.Visible := True;
    mnuDryPeriod.Visible := True;
    mnuChangeDefaultOffset.Enabled := False;
    mnuChangeOffsetToAll.Enabled := False;
  end;
end;

procedure TFrmSDConstruction.lstCurvesClick(Sender: TObject);
begin
  trackStart.OnChange := nil;
  trackEnd.OnChange := nil;
  try
    SetControlStatus;
  finally
    trackStart.OnChange := trackStartChange;
    trackEnd.OnChange := trackEndChange;
  end;
end;

procedure TFrmSDConstruction.SetCurveList(ACurveList: TTransientCurveList);
var
  TmpCurveList: TSingleAction;
begin
  TmpCurveList := nil;
  try
    ActionList.Clear;
    TmpCurveList := TSingleAction.Create;
    TmpCurveList.Assign(ACurveList);
    TmpCurveList.Caption := '';
    ActionList.Add(TmpCurveList);
    CurrentPointer := TmpCurveList;
    if Mode = icemDischargeSedimentDischarge then FixWetDry(CurrentPointer);
    CurrentIndex := 0;
    TmpCurveList := nil;
  except
    TmpCurveList.Free;
    raise;
  end;
end;

function TFrmSDConstruction.GetCurveList: TTransientCurveList;
begin
  Assert(CurrentIndex<ActionList.Count);
  Result := TTransientCurveList(CurrentPointer);
end;

procedure TFrmSDConstruction.ReturnCurveList(ReturnValue: TTransientCurveList);
begin
  Assert(CurrentIndex<ActionList.Count);
  ReturnValue.Assign(TTransientCurveList(CurrentPointer));
end;

function TFrmSdConstruction.GetNewAction(OldAction: TSingleAction;
  MessageStr: string): TSingleAction;
begin
  Result := nil;
  try
    Result := TSingleAction.Create;
    Result.Assign(OldAction);
    Result.Caption := MessageStr;
  except
    Result.Free;
    raise;
  end;
end;

procedure TFrmSDConstruction.FinilizeNewAction(AAction: TSingleAction);
var
  i: Integer;
begin
  for i := ActionList.Count-1 downto CurrentIndex+1 do
    ActionList.Delete(i);
  Inc(CurrentIndex);
  ActionList.Add(AAction);
  CurrentPointer := AAction;
end;

procedure TFrmSDConstruction.Undo;
begin
  if CurrentIndex>0 then
    Dec(CurrentIndex);
  CurrentPointer := TSingleAction(ActionList.Items[CurrentIndex]);
end;

procedure TFrmSDConstruction.Redo;
begin
  if CurrentIndex<ActionList.Count-1 then
    Inc(CurrentIndex);
  CurrentPointer := TSingleAction(ActionList.Items[CurrentIndex]);    
end;

resourcestring
  rsUndo = 'Undo';
  rsRedo = 'Redo';

procedure TFrmSDConstruction.SetControlStatus;
begin
{Undo - Redo}
  mnuUndo.Caption := rsUndo;
  if (CurrentIndex>0) and (CurrentIndex<ActionList.Count) then
    mnuUndo.Caption := mnuUndo.Caption+' '+
      TSingleAction(ActionList.Items[CurrentIndex]).Caption;
  mnuRedo.Caption := rsRedo;
  if (CurrentIndex>=0) and (CurrentIndex<ActionList.Count-1) then
    mnuRedo.Caption := mnuRedo.Caption+' '+
      TSingleAction(ActionList.Items[CurrentIndex+1]).Caption;
  mnuUndo.Enabled := (CurrentIndex>0);
  tbtnUndo.Enabled := mnuUndo.Enabled;
  mnuRedo.Enabled := (CurrentIndex<ActionList.Count-1);
  tbtnRedo.Enabled := mnuRedo.Enabled;
  tbtnUndo.Hint := mnuUndo.Caption;
  tbtnRedo.Hint := mnuRedo.Caption;
{Poputlate Lists}
  PopulateList;
{Insert button}
  btnInsert.Enabled := (lstCurves.ItemIndex>-1);
  btnRemove.Enabled := btnInsert.Enabled;
{Redraw Charts}
  chartMain.OriginalCursor := crDefault;
  RedrawChartPeriods;
  RedrawChartMain;
  SetAxisLimits;
{Property Group}
  btnPropertyApply.Enabled := False;
  btnPropertyCancel.Enabled := False;
  btnFitSet.Enabled := False;
  if lstCurves.ItemIndex>-1 then
  begin
    recColor.Brush.Color := GetColor(lstCurves.ItemIndex);
    with CurrentPointer[lstCurves.ItemIndex] do
    begin
      edtOffsetSet.Text := FloatToStr(Offset.Independent);
      chkExtensionSet.Checked := Extension;
      spinSegmentsSet.Enabled := not chkExtensionSet.Checked;
      spinSegmentsSet.Position := Max(0, Count-1);
      dtpStartDate.DateTime := StartDate;
      dtpStartTime.DateTime := StartDate;
      dtpEndDate.DateTime := EndDate;
      dtpEndTime.DateTime := EndDate;
    end;
  end else
  begin
    edtOffsetSet.Text := '';
    chkExtensionSet.Checked := False;
    spinSegmentsSet.Position := 0;
  end;
  SetGrid;
  {Copy - paste menu items}
  mnuCopyCurve.Enabled := (CurrentPointer.Count>0) and
    (sgrdCurvePoints.RowCount>1);
  mnuPasteCurve.Enabled := (CurrentPointer.Count>0) and
    (lstCurves.ItemIndex>-1) and (StrLen(PChar((Clipboard.AsText)))>0);
  {Other menus}
  mnuFixOverlapingDates.Checked := FFixesDates;
  mnuCalcDetCoef.Enabled := (CurrentPointer.Count>0);
  {Set trackbars}
  SetTrackBars;
  {Mark points}
  tbtnMarkOutlier.Down := False;
  {Hydraulic button}
  btnHydraulic.Enabled := (lstCurves.ItemIndex>-1);
  {Timeseries menu}
  mnuExportTimeseries.Enabled := (CurrentPointer.HydrometricPointsCount>0);
  {Runtime created menus}
  if Mode = icemDischargeSedimentDischarge then
    SetWetDryMenus;
end;

procedure TFrmSDConstruction.SetTrackBars;
begin
  if lstCurves.ItemIndex<0 then
  begin
    trackStart.Position := 0;
    trackEnd.Position := 4095;
    Exit;
  end;
  try
    trackStart.Position := Floor(4096*
      (CurrentPointer[lstCurves.ItemIndex].StartDate-seriesPeriods.MinXValue)/
         (seriesPeriods.MaxXValue-seriesPeriods.MinXValue));
    trackEnd.Position := Floor(4096*
      (CurrentPointer[lstCurves.ItemIndex].EndDate-
        seriesPeriods.MinXValue)/
         (seriesPeriods.MaxXValue-seriesPeriods.MinXValue));
    trackStart.SelStart := trackStart.Position;
    trackEnd.SelStart := trackStart.Position;
    trackStart.SelEnd := trackEnd.Position;
    trackEnd.SelEnd := trackEnd.Position;
  except
    on EMathError do
      Exit;
    else
      raise;
  end;
end;

procedure TFrmSDConstruction.SetAxisLimits;
var
  xmin, xmax, ymin, ymax: Real;
begin
  xmin := chartMain.MinXValue(chartMain.BottomAxis);
  xmax := chartMain.MaxXValue(chartMain.BottomAxis);
  xmin := Max(xmin, 0.001);
  xmax := Max(xmax, 0.001);
  chartMain.BottomAxis.Maximum := 10e37;
  chartMain.BottomAxis.Minimum := -10e37;
  chartMain.BottomAxis.Maximum := Power(10.000,(Floor(Log10(xmax))+1));
  chartMain.BottomAxis.Minimum := Power(10.000,(Floor(Log10(xmin))));
  ymin := chartMain.MinYValue(chartMain.LeftAxis);
  ymax := chartMain.MaxYValue(chartMain.LeftAxis);
  ymin := Max(ymin, 0.001);
  ymax := Max(ymax, 0.001);
  chartMain.LeftAxis.Maximum := 10e37;
  chartMain.LeftAxis.Minimum := -10e37;
  chartMain.LeftAxis.Maximum := Power(10.000,(Floor(Log10(ymax))+1));
  chartMain.LeftAxis.Minimum := Power(10.000,(Floor(Log10(ymin))));
end;

procedure TFrmSDConstruction.SetGrid;
var
  i, j: Integer;
begin
  sgrdCurvePoints.Cells[0,0] := '';
  if Mode = icemStageDischarge then
  begin
    sgrdCurvePoints.Cells[1,0] := rsGridStage;
    sgrdCurvePoints.Cells[2,0] := rsGridDischarge;
  end else
  begin
    sgrdCurvePoints.Cells[1,0] := rsGridDischarge;
    sgrdCurvePoints.Cells[2,0] := rsGridSedimentDischarge;
  end;
  if lstCurves.ItemIndex<0 then
  begin
    for i := 1 to sgrdCurvePoints.RowCount-1 do
      for j := 1 to sgrdCurvePoints.ColCount-1 do
        sgrdCurvePoints.Cells[i,j] := '';
  end else begin
    sgrdCurvePoints.RowCount := 1 + CurrentPointer[lstCurves.ItemIndex].Count;
    if sgrdCurvePoints.RowCount>1 then
      sgrdCurvePoints.FixedRows := 1;
    for i := 0 to CurrentPointer[lstCurves.ItemIndex].Count-1 do
    begin
      sgrdCurvePoints.Cells[0, i+1] := IntToStr(i+1);
      sgrdCurvePoints.Cells[1, i+1] := FormatFloat('0.000',
        CurrentPointer[lstCurves.ItemIndex].Points[i].Independent);
      sgrdCurvePoints.Cells[2, i+1] := FormatFloat('0.000',
        CurrentPointer[lstCurves.ItemIndex].Points[i].Dependent);
    end;
  end;
end;

procedure TFrmSDConstruction.SetWetDryMenus;
var
  AMenuItem: TMenuItem;
  Menus: array[0..1] of TMenuItem;
  i, j: Integer;
begin
  Menus[0] := mnuWetPeriod;
  Menus[1] := mnuDryPeriod;
  for i := 0 to 1 do
    with Menus[i] do
    begin
      Clear;
      for j := 0 to 11 do
      begin
        AMenuItem := nil;
        try
          AMenuItem := TMenuItem.Create(Menus[i]);
          AMenuItem.Caption := LongMonthNames[j+1];
          AMenuItem.Tag := j+1;
          AMenuItem.RadioItem := True;
          AMenuItem.GroupIndex := 151+i;
          AMenuItem.Name := 'mnu'+IntToStr(i)+IntToStr(j);
          AMenuItem.OnClick := SetWetDryPeriodsClick;
          if (i=0) and (j+1=WetPeriodStartingMonth) then
            AMenuItem.Checked := True;
          if (i=1) and (j+1=DryPeriodStartingMonth) then
            AMenuItem.Checked := True;
          Add(AMenuItem);
          AMenuItem := nil;
        except
          AMenuItem.Free;
          raise;
        end;
      end;
    end;
end;

resourcestring
  rsChangeWetDryPeriods =
    'Change wet and dry periods';
  rsWetAndDryCannot =
    'Origin of wet and dry periods could not be the same';
  rsSureToProceedToPeriodChange =
    'Are you sure to proceed to change of the origin of wet and dry periods?';

procedure TFrmSDConstruction.SetWetDryPeriodsClick(Sender: TObject);
var
  i, AMonth: Integer;
  ANewAction: TSingleAction;
  IsWet: Boolean;
begin
  AMonth := (Sender as TMenuItem).Tag;
  IsWet := (Sender as TMenuItem).GroupIndex = 151;
  if IsWet then
  begin
    if AMonth = DryPeriodStartingMonth then
      raise Exception.Create(rsWetAndDryCannot);
  end else begin
    if AMonth = WetPeriodStartingMonth then
      raise Exception.Create(rsWetAndDryCannot);
  end;
  if MessageDlg(rsSureToProceedToPeriodChange, mtConfirmation, mbYesNo, 0) =
    mrNo then Exit;
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsChangeWetDryPeriods);
    if IsWet then
    begin
      WetPeriodStartingMonth := AMonth;
      if AMonth > 1 then
        DryPeriodEndingMonth := AMonth -1 else
        DryPeriodEndingMonth := 12;
    end else begin
      DryPeriodStartingMonth := AMonth;
      if AMonth > 1 then
        WetPeriodEndingMonth := AMonth -1 else
        WetPeriodEndingMonth := 12;
    end;
    for i := 0 to ANewAction.Count-1 do
      with ANewAction.Items[i] do
      begin
        if PrimaryCurve then
        begin
          StartMonth := WetPeriodStartingMonth;
          EndMonth := WetPeriodEndingMonth;
        end else begin
          StartMonth := DryPeriodStartingMonth;
          EndMonth := DryPeriodEndingMonth;
        end;
      end;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  (Sender as TMenuItem).Checked := True;
  SetControlStatus;
end;

procedure TFrmSDConstruction.mnuUndoClick(Sender: TObject);
begin
  Undo;
  SetControlStatus;
end;

procedure TFrmSDConstruction.mnuRedoClick(Sender: TObject);
begin
  Redo;
  SetControlStatus;
end;

procedure TFrmSDConstruction.tbtnShowAllClick(Sender: TObject);
begin
  SetControlStatus;
end;

resourcestring
  rsSimpleCurve = 'Stage-Discharge curve';
  rsExtensionCurve = 'Extension curve';
  rsSedimentDry = 'Sediment Discharge curve for dry period';
  rsSedimentWet = 'Sediment Discharge curve for wet period';

procedure TFrmSDConstruction.PopulateList;
var
  i, SavedIndex, SavedCount: Integer;
  s: string;
begin
  SavedIndex := lstCurves.ItemIndex;
  SavedCount := lstCurves.Items.Count;
  lstCurves.Clear;
  for i := 0 to CurrentPointer.Count-1 do
  begin
    s := IntToStr(i+1)+'. ';
    if Mode = icemStageDischarge then
    begin
      if CurrentPointer[i].Extension then
        s := s+rsExtensionCurve else
        s := s+rsSimpleCurve;
    end else begin
      if CurrentPointer[i].PrimaryCurve then
        s := s + rsSedimentWet else
        s := s + rsSedimentDry;
    end;
    lstCurves.Items.Add(s);
  end;
  if (SavedIndex>-1) and (SavedIndex<lstCurves.Items.Count) then
    if SavedCount = lstCurves.Items.Count then
      lstCurves.ItemIndex := SavedIndex
    else if SavedCount < lstCurves.Items.Count then
      lstCurves.ItemIndex := Min(lstCurves.Items.Count-1, SavedIndex+1)
    else
      lstCurves.ItemIndex := Min(lstCurves.Items.Count-1, SavedIndex);
end;

procedure TFrmSDConstruction.RedrawChartPeriods;
var
  i: Integer;
begin
  seriesPeriods.Clear;
  for i := 0 to CurrentPointer.Count-1 do
  begin
    with CurrentPointer.Items[i] do
    begin
      if Extension then Continue;
      GraphIndex :=  seriesPeriods.AddGanttColor(StartDate, EndDate, i, '',
        GetColor(i));
    end;
  end;
end;

procedure TfrmSDConstruction.RemoveAndFreeAllSeries;
var
  AChartSeries: TChartSeries;
begin
  while ChartMain.SeriesList.Count>0 do
  begin
    AChartSeries := ChartMain.SeriesList[0];
    ChartMain.RemoveSeries(AChartSeries);
    AChartSeries.Free;
  end;
end;

procedure TfrmSDConstruction.RedrawChartMain;
var
  i, j: Integer;
  APointSeries: TPointSeries;
  AOutlierSeries: TPointSeries;
  ALineSeries: TLineSeries;
  Year, Month, Day: Word;
  ATempXY: Real;
begin
  RemoveAndFreeAllSeries;
  for i := 0 to CurrentPointer.Count -1 do
  begin
    if not tbtnShowAll.Down then
      if i<>lstCurves.ItemIndex then
        Continue;
    if tbtnShowCurves.Down then
    begin
      ALineSeries := nil;
      try
        ALineSeries := TLineSeries.Create(chartMain);
        ALineSeries.ParentChart := chartMain;
        ALineSeries.Color := GetColor(i);
        ALineSeries.XValues.Order := loNone;
        ALineSeries.YValues.Order := loNone;
        for j := 0 to CurrentPointer[i].Count-1 do
        begin
          ALineSeries.AddXY(CurrentPointer[i].Points[j].Dependent,
            CurrentPointer[i].Points[j].Independent+
              CurrentPointer[i].Offset.Independent,'',GetColor(i));
        end;
        ALineSeries := nil;
      except
        ALineSeries.Free;
        raise;
      end;
    end;
  end;
  if tbtnShowPoints.Down then
  begin
    APointSeries := nil;
    AOutlierSeries := nil;
    try
      APointSeries := TPointSeries.Create(chartMain);
      APointSeries.ParentChart := chartMain;
      APointSeries.Pointer.Style := psTriangle;
      AOutlierSeries := TPointSeries.Create(chartMain);
      AOutlierSeries.ParentChart := chartMain;
      AOutlierSeries.Pointer.Style := psDiagCross;
      for i := 0 to CurrentPointer.Count -1 do
      begin
        if not tbtnShowAll.Down then
          if i<>lstCurves.ItemIndex then
            Continue;
        if CurrentPointer[i].Extension then
          Continue;
        for j := 0 to CurrentPointer.HydrometricPointsCount-1 do
        begin
          if DiffInSecs(CurrentPointer.HydrometricPoints[j].Date,
            CurrentPointer[i].StartDate)<-1 then
            Continue;
          if DiffInSecs(CurrentPointer.HydrometricPoints[j].Date,
            CurrentPointer[i].EndDate)>1 then
            Continue;
          DecodeDate(CurrentPointer.HydrometricPoints[j].Date,
            Year, Month, Day);
          if CurrentPointer[i].EndMonth > CurrentPointer[i].StartMonth then
          begin
            if (Month<CurrentPointer[i].StartMonth) or
              (Month>CurrentPointer[i].EndMonth) then Continue;
          end else begin
            if (Month<CurrentPointer[i].StartMonth) and
              (Month>CurrentPointer[i].EndMonth) then Continue;
          end;
          if not CurrentPointer.HydrometricPoints[j].Outlier then
            APointSeries.AddXY(CurrentPointer.HydrometricPoints[j].Discharge,
              CurrentPointer.HydrometricPoints[j].Stage+
                CurrentPointer[i].Offset.Independent,'',GetColor(i))
          else
            AOutlierSeries.AddXY(CurrentPointer.HydrometricPoints[j].Discharge,
              CurrentPointer.HydrometricPoints[j].Stage+
                CurrentPointer[i].Offset.Independent,'',GetColor(i));
        end;
      end;
      if CurrentPointer.Count<1 then
      begin
        for j := 0 to CurrentPointer.HydrometricPointsCount-1 do
        begin
          if (CurrentPointer.HydrometricPoints[j].Stage+FDefaultOffset)<0 then
            Continue;
          if not CurrentPointer.HydrometricPoints[j].Outlier then
            APointSeries.AddXY(CurrentPointer.HydrometricPoints[j].Discharge,
              CurrentPointer.HydrometricPoints[j].Stage+
                FDefaultOffset,'',GetColor(0)) else
            AOutlierSeries.AddXY(CurrentPointer.HydrometricPoints[j].Discharge,
              CurrentPointer.HydrometricPoints[j].Stage+
                FDefaultOffset,'',GetColor(0));
        end;
      end;
      APointSeries := nil;
      AOutlierSeries := nil;
    except
      APointSeries.Free;
      AOutlierSeries.Free;
      raise;
    end;
  end;
  if Mode = icemDischargeSedimentDischarge then
    for i := 0 to chartMain.SeriesCount-1 do
      with chartMain.Series[i] do
        for j := 0 to Count-1 do
        begin
          ATempXY := XValue[j];
          XValue[j] := YValue[j];
          YValue[j] := ATempXY;
        end;
end;

resourcestring
  rsSelectToDelete = 'You must select a curve in order to remove it';
  rsRemoveCurve = 'remove curve';

procedure TFrmSDConstruction.btnRemoveClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  AIndex: Integer;
begin
  if lstCurves.Items.Count>0 then
  begin
    if lstCurves.ItemIndex<0 then
      raise Exception.Create(rsSelectToDelete);
  end else
    Exit;
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsRemoveCurve);
    if Mode = icemDischargeSedimentDischarge then
    begin
      AIndex := lstCurves.ItemIndex;
      if not ANewAction.Items[AIndex].PrimaryCurve then
        Dec(AIndex);
      lstCurves.ItemIndex := AIndex;
      Assert(ANewAction.Items[AIndex].LinkedTo =  ANewAction.Items[AIndex+1]);
      ANewAction.Delete(AIndex+1);
    end;
    if not ANewAction[lstCurves.ItemIndex].Extension then
    begin
      AIndex := ANewAction.IndexNotExtension(lstCurves.ItemIndex);
      if AIndex < ANewAction.CountNotExtension-1 then
        ANewAction.ItemsNotExtension[AIndex+1].StartDate :=
          ANewAction.ItemsNotExtension[AIndex].StartDate;
      if (AIndex = ANewAction.CountNotExtension-1) and
        (ANewAction.CountNotExtension>1) then
          ANewAction.ItemsNotExtension[AIndex-1].EndDate :=
            ANewAction.ItemsNotExtension[AIndex].EndDate;
    end;
    ANewAction.Delete(lstCurves.ItemIndex);
    SetDatesConsistent(ANewAction);
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

resourcestring
  rsInsertCurve = 'insert curve';
  rsSelectToInsert = 'You must select a curve in order to insert a new one '+
    'on it';
  rsYouCannotInsertOnExtension = 'You cannot insert a new curve on an '+
    'extension line';

procedure TFrmSDConstruction.btnInsertClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  ANewCurve: TTransientCurve;
  ANewDate: TDateTime;
begin
  if lstCurves.Items.Count>0 then
  begin
    if lstCurves.ItemIndex<0 then
      raise Exception.Create(rsSelectToInsert);
    if CurrentPointer[lstCurves.ItemIndex].Extension then
      raise Exception.Create(rsYouCannotInsertOnExtension);
  end else
    Exit;
  ANewAction := nil;
  ANewCurve := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsInsertCurve);
    ANewDate := 0.5*(CurrentPointer[lstCurves.ItemIndex].StartDate+
      CurrentPointer[lstCurves.ItemIndex].EndDate);
    ANewCurve := TTransientCurve.Create(True);
    with ANewAction do
    begin
      ANewCurve.Offset := Items[lstCurves.ItemIndex].Offset;
      ANewCurve.StartDate := Items[lstCurves.ItemIndex].StartDate;
      ANewCurve.EndDate := ANewDate;
      Items[lstCurves.ItemIndex].StartDate := ANewDate;
      Insert(lstCurves.ItemIndex, ANewCurve);
      ANewCurve := nil;
    end;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    ANewCurve.Free;
    raise;
  end;
  SetControlStatus;
end;

procedure TFrmSDConstruction.btnAddClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  ANewCurve, ALastCurve: TTransientCurve;
  AStartDate: TDateTime;
  AEndDate: TDateTime;
  AOffset: TCurvePoint;

  procedure AddForSeason(StartMonth, EndMonth: Integer; LinkedCurve:
    TTransientCurve);
  begin
    with ANewAction do
    begin
      ANewCurve := TTransientCurve.Create(True);
      AOffset.Independent := FDefaultOffset;
      AOffset.Dependent := 0;
      ANewCurve.Offset := AOffset;
      ANewCurve.StartDate := AStartDate;
      ANewCurve.EndDate := AEndDate;
      ANewCurve.StartMonth := StartMonth;
      ANewCurve.EndMonth := EndMonth;
      ANewCurve.LinkedTo := LinkedCurve;
      if LinkedCurve = nil then ANewCurve.PrimaryCurve := True else
      ANewCurve.PrimaryCurve := False;
      Add(ANewCurve);
    end;
  end;

begin
  if CurrentPointer.CountNotExtension=0 then
  begin
    AStartDate := CurrentPointer.HydrometricPointStartDate;
    AEndDate := CurrentPointer.HydrometricPointEndDate;
  end else begin
    AStartDate := (CurrentPointer.LastNotExtension.StartDate+
      CurrentPointer.LastNotExtension.EndDate)*0.50;
    AEndDate := CurrentPointer.LastNotExtension.EndDate;
  end;
  ANewAction := nil;
  ANewCurve := nil;
  ALastCurve := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsInsertCurve);
    if ANewAction.CountNotExtension>0 then ALastCurve :=
      ANewAction.LastNotExtension;
    if Mode = icemStageDischarge then
      AddForSeason(0, 0, nil)
    else
    begin
      AddForSeason(WetPeriodStartingMonth, WetPeriodEndingMonth, nil);
      AddForSeason(DryPeriodStartingMonth, DryPeriodEndingMonth, ANewCurve);
    end;
    ANewCurve := nil;
    if ALastCurve<> nil then
    begin
      ALastCurve.EndDate := AStartDate;
      if ALastCurve.LinkedTo<>nil then
        (ALastCurve.LinkedTo as TTransientCurve).EndDate := AStartDate;
    end;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    ANewCurve.Free;
    raise;
  end;
  SetControlStatus;
end;

procedure TFrmSDConstruction.btnPropertyCancelClick(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmSDConstruction.edtOffsetSetChange(Sender: TObject);
begin
  if lstCurves.ItemIndex<0 then Exit;
  btnPropertyApply.Enabled := True;
  btnPropertyCancel.Enabled := True;
  CheckForChanges;  
end;

resourcestring
  rsChangePropertiesCurve = 'change curve properties';
  rsCurveValuesShouldIncrease =
    'Curve values should be increasing for stage as well as for discharge';
  rsDatesShouldBeConsistent =
    'Dates should be consistent (Start date not greater than end date)';

procedure TFrmSDConstruction.SetDatesConsistent(AAction: TSingleAction);
var
  i, AIndex: Integer;
  ADifference, AReference: Real;
begin
  if AAction.Count<1 then
    Exit;
  while lstCurves.ItemIndex > AAction.Count -1 do
    lstCurves.ItemIndex := lstCurves.ItemIndex -1;
  if DiffInSecs(AAction[lstCurves.ItemIndex].StartDate,
    AAction[lstCurves.ItemIndex].EndDate)>=0 then
      raise Exception.Create(rsDatesShouldBeConsistent);
  if not FFixesDates then
    Exit;
{Stage I}
  for i := 0 to AAction.CountNotExtension-1 do
  begin
    if AAction.IndexNotExtension(lstCurves.ItemIndex) = i then
      Continue;
    if not AAction.ItemsNotExtension[i].PrimaryCurve then Continue;
    if DiffInSecs(AAction[lstCurves.ItemIndex].StartDate,
      AAction.ItemsNotExtension[i].StartDate)>0 then
        if DiffInSecs(AAction[lstCurves.ItemIndex].StartDate,
          AAction.ItemsNotExtension[i].EndDate)<=0 then
            AAction.ItemsNotExtension[i].EndDate :=
              AddDateTime(AAction[lstCurves.ItemIndex].StartDate,-1/86400);
    if DiffInSecs(AAction[lstCurves.ItemIndex].EndDate,
      AAction.ItemsNotExtension[i].EndDate)<0 then
        if DiffInSecs(AAction[lstCurves.ItemIndex].EndDate,
          AAction.ItemsNotExtension[i].StartDate)>=0 then
            AAction.ItemsNotExtension[i].StartDate :=
              AddDateTime(AAction[lstCurves.ItemIndex].EndDate,1/86400);
  end;
{Stage II}
  AReference := 1e35;
  AIndex := -1;
  for i := 0 to AAction.CountNotExtension-1 do
  begin
    if AAction.IndexNotExtension(lstCurves.ItemIndex) = i then
      Continue;
    if not AAction.ItemsNotExtension[i].PrimaryCurve then Continue;
    ADifference := DiffInSecs(AAction.ItemsNotExtension[i].StartDate,
      AAction[lstCurves.ItemIndex].EndDate);
    if ADifference >0 then
    begin
      if ADifference < AReference then
      begin
        AIndex := i;
        AReference := ADifference;
      end;
    end;
  end;
  if (AIndex<>-1) and (AReference>1) then
  begin
    AAction.ItemsNotExtension[AIndex].StartDate :=
      AddDateTime(AAction[lstCurves.ItemIndex].EndDate, 1/86400);    
  end;
{Stage III}
  AReference := 1e35;
  AIndex := -1;
  for i := 0 to AAction.CountNotExtension-1 do
  begin
    if AAction.IndexNotExtension(lstCurves.ItemIndex) = i then
      Continue;
    if not AAction.ItemsNotExtension[i].PrimaryCurve then Continue;
    ADifference := DiffInSecs(AAction[lstCurves.ItemIndex].StartDate,
      AAction.ItemsNotExtension[i].EndDate);
    if ADifference >0 then
    begin
      if ADifference < AReference then
      begin
        AIndex := i;
        AReference := ADifference;
      end;
    end;
  end;
  if (AIndex<>-1) and (AReference>1) then
  begin
    AAction.ItemsNotExtension[AIndex].EndDate :=
      AddDateTime(AAction[lstCurves.ItemIndex].StartDate, -1/86400);
  end;
  for i := 0 to AAction.CountNotExtension-1 do
  begin
    if not AAction.ItemsNotExtension[i].PrimaryCurve then
      if AAction.ItemsNotExtension[i].LinkedTo<>nil then
        with (AAction.ItemsNotExtension[i].LinkedTo as TTransientCurve) do
        begin
          AAction.ItemsNotExtension[i].StartDate := StartDate;
          AAction.ItemsNotExtension[i].EndDate := EndDate;
        end;
  end;
end;


procedure TFrmSDConstruction.btnPropertyApplyClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  AOffset: TCurvePoint;
  i: Integer;
begin
  if lstCurves.ItemIndex<0 then
    Exit;
  ANewAction := nil;
  try try
    for i := 0 to CurrentPointer[lstCurves.ItemIndex].Count-2 do
    begin
      if StrToFloat(sgrdCurvePoints.Cells[1,i+1]) >
        StrToFloat(sgrdCurvePoints.Cells[1,i+2]) then
          raise EMyError.Create(rsCurveValuesShouldIncrease);
      if StrToFloat(sgrdCurvePoints.Cells[2,i+1]) >
        StrToFloat(sgrdCurvePoints.Cells[2,i+2]) then
          raise EMyError.Create(rsCurveValuesShouldIncrease);
    end;
    ANewAction := GetNewAction(CurrentPointer, rsChangePropertiesCurve);
    AOffset.Independent := StrToFloat(edtOffsetSet.Text);
    AOffset.Dependent := 0;
    ANewAction[lstCurves.ItemIndex].Offset := AOffset;
    ANewAction[lstCurves.ItemIndex].StartDate :=
      dtpStartDate.DateTime;
    ANewAction[lstCurves.ItemIndex].EndDate :=
      dtpEndDate.DateTime;
    if ANewAction[lstCurves.ItemIndex].LinkedTo <> nil then
    begin
      (ANewAction[lstCurves.ItemIndex].LinkedTo as TTransientCurve).StartDate :=
        ANewAction[lstCurves.ItemIndex].StartDate;
      (ANewAction[lstCurves.ItemIndex].LinkedTo as TTransientCurve).EndDate :=
        ANewAction[lstCurves.ItemIndex].EndDate;
    end;
    if not ANewAction[lstCurves.ItemIndex].Extension then
      SetDatesConsistent(ANewAction);
    for i := 0 to CurrentPointer[lstCurves.ItemIndex].Count-1 do
    begin
      AOffset.Independent := StrToFloat(sgrdCurvePoints.Cells[1,i+1]);
      AOffset.Dependent := StrToFloat(sgrdCurvePoints.Cells[2,i+1]);
      ANewAction[lstCurves.ItemIndex].Points[i] := AOffset;
    end;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  finally
    SetControlStatus;
  end;
end;

procedure TFrmSDConstruction.spinSegmentsSetClick(Sender: TObject;
  Button: TUDBtnType);
begin
  if lstCurves.ItemIndex<0 then Exit;
  btnFitSet.Enabled := True;
  btnPropertyCancel.Enabled := True;
end;

resourcestring
  rsChangeCurveSegments = 'change curve segments';
  rsCoefficientOfDetForCurve =
    'Coefficient of determination for the curve is: ';
  rsOperationMayTakeLong =
    'This operation may take a long to complete, depending on the number of '+
      'the points. Press OK to proceed.';

procedure TFrmSDConstruction.btnFitSetClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  ACursor: TCursor;
  AFloat: Real;
begin
  if lstCurves.ItemIndex<0 then
    Exit;
  ANewAction := nil;
    if spinSegmentsSet.Position>4 then
      if MessageDlg(rsOperationMayTakeLong, mtWarning, mbOKCancel,0)
        = mrCancel then
          Exit;
  try
    ANewAction := GetNewAction(CurrentPointer, rsChangeCurveSegments);
    if spinSegmentsSet.Position >0 then
    begin
      ACursor := Screen.Cursor;
      try
        Screen.Cursor := crHourGlass;
        AFloat :=
          ANewAction.FitPolyline(lstCurves.ItemIndex, spinSegmentsSet.Position);
      finally
        Screen.Cursor := ACursor;
      end;
      ShowMessage(rsCoefficientOfDetForCurve+FormatFloat('0.000', AFloat));
    end else
      ANewAction[lstCurves.ItemIndex].Clear;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

procedure TFrmSDConstruction.dtpStartDateChange(Sender: TObject);
begin
  if lstCurves.ItemIndex<0 then Exit;
  if TDateTimePicker(Sender).Kind = dtkDate then
  begin
    dtpStartTime.DateTime := dtpStartDate.DateTime;
    dtpEndTime.DateTime := dtpEndDate.DateTime;
  end else begin
    dtpStartDate.DateTime := dtpStartTime.DateTime;
    dtpEndDate.DateTime := dtpEndTime.DateTime;
  end;
  btnPropertyApply.Enabled := True;
  btnPropertyCancel.Enabled := True;
  CheckForChanges;
end;

procedure TFrmSDConstruction.sgrdCurvePointsSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: String);
begin
  btnPropertyApply.Enabled := True;
  btnPropertyCancel.Enabled := True;
  CheckForChanges;
end;

procedure TFrmSDConstruction.CheckForChanges;
var
  AFlag: Boolean;
  i: Integer;
begin
  if lstCurves.ItemIndex<0 then
    Exit;
  AFlag := False;
  try
    if Abs(StrToFloat(edtOffsetSet.Text)-
      CurrentPointer[lstCurves.ItemIndex].Offset.Independent)>0.0015 then
        AFlag := True;
    if Abs(DiffInSecs(CurrentPointer[lstCurves.ItemIndex].StartDate,
      dtpStartDate.DateTime))>1 then
        AFlag := True;
    if Abs(DiffInSecs(CurrentPointer[lstCurves.ItemIndex].EndDate,
      dtpEndDate.DateTime))>1 then
        AFlag := True;
    for i := 0 to CurrentPointer[lstCurves.ItemIndex].Count-1 do
    begin
      if Abs(CurrentPointer[lstCurves.ItemIndex].Points[i].Independent -
        StrToFloat(sgrdCurvePoints.Cells[1,i+1]))>0.0015 then
        AFlag := True;
      if Abs(CurrentPointer[lstCurves.ItemIndex].Points[i].Dependent -
        StrToFloat(sgrdCurvePoints.Cells[2,i+1]))>0.0015 then
        AFlag := True;
    end;
  except
    on EConvertError do
      AFlag := True;
    else
      raise;
  end;
  if AFlag=False then
  begin
    btnPropertyApply.Enabled := False;
    btnPropertyCancel.Enabled := False;
  end;
end;

resourcestring
  rsChangeDefaultOffset = 'Change the default offset for new created curves';
  rsWithAddButton = ' with add button';

procedure TFrmSDConstruction.mnuChangeDefaultOffsetClick(Sender: TObject);
begin
  FDefaultOffset := StrToFloat(InputBox(rsChangeDefaultOffset,
    rsChangeDefaultOffset+rsWithAddButton, FloatToStr(FDefaultOffset)));
  SetControlStatus;
end;

resourcestring
  rsChangeOffsetToAllCurves = 'Change the offset to all curves to the value';
  rsChangeOffsetToAll = 'mass-change offset';

procedure TFrmSDConstruction.mnuChangeOffsetToAllClick(Sender: TObject);
var
  i: Integer;
  ANewAction: TSingleAction;
  AOffset: TCurvePoint;
  s: string;
begin
  s := FloatToStr(FDefaultOffset);
  if InputQuery(rsChangeOffsetToAllCurves, rsChangeOffsetToAllCurves, s) then
  begin
    AOffset.Independent := StrToFloat(s);
    AOffset.Dependent := 0;
    ANewAction := nil;
    try
      ANewAction := GetNewAction(CurrentPointer, rsChangeOffsetToAll);
      for i := 0 to ANewAction.Count-1 do
        ANewAction[i].Offset := AOffset;
      FinilizeNewAction(ANewAction);
    except
      ANewAction.Free;
      raise;
    end;
  end;
  SetControlStatus;
end;

procedure TFrmSDConstruction.mnuCopyCurveClick(Sender: TObject);
begin
  if lstCurves.ItemIndex<0 then
    Exit;
  StringGridToClipboard(sgrdCurvePoints, 1, 2, 1, sgrdCurvePoints.RowCount-1);
end;

resourcestring
  rsPasteCurveFromClipboard ='paste curve from clipboard';
  rsTooManyColumns = 'Too many columns of pasted text';  

procedure TFrmSDConstruction.mnuPasteCurveClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  Cliptext, s: string;
  p: PChar;
  j: Integer;
  ACurvePoint: TCurvePoint;
begin
  if lstCurves.ItemIndex<0 then
    Exit;
  Cliptext := Clipboard.AsText;
  j := 0;
  p := PChar(Cliptext);
  s := '';
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsPasteCurveFromClipboard);
    ANewAction[lstCurves.ItemIndex].Clear;
    while p^<>#0 do
    begin
      case p^ of
        #13:
          begin
            if j=0 then
              ACurvePoint.Independent := StrToFloat(s)
            else
              ACurvePoint.Dependent := StrToFloat(s);
            if ANewAction[lstCurves.ItemIndex].Count>1 then
            begin
              if ANewAction[lstCurves.ItemIndex].Last.Independent >
                ACurvePoint.Independent then
                  raise Exception.Create(rsCurveValuesShouldIncrease);
{              if ANewAction[lstCurves.ItemIndex].Last.Dependent >
                ACurvePoint.Dependent then
                  raise Exception.Create(rsCurveValuesShouldIncrease);}
            end;
            ANewAction[lstCurves.ItemIndex].Add(ACurvePoint);
            s := '';
            Inc(p); { Skip #10 as well }
            j := 0;
          end;
        #9:
          begin
            if j=0 then
              ACurvePoint.Independent := StrToFloat(s)
            else
              ACurvePoint.Dependent := StrToFloat(s);
            s := '';
            Inc(j);
            if j>1 then
              raise Exception.Create(rsTooManyColumns);
          end;
      else
        s := s+p^;
      end;
      Inc(p);
    end;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

procedure TFrmSDConstruction.mnuEditClick(Sender: TObject);
begin
  mnuPasteCurve.Enabled := (CurrentPointer.Count>0) and
    (lstCurves.ItemIndex>-1) and (StrLen(PChar((Clipboard.AsText)))>0);
end;

resourcestring
  rsResetAll = 'reset all';

procedure TFrmSDConstruction.mnuNewClick(Sender: TObject);
var
  ANewAction: TSingleAction;
begin
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsResetAll);
    ANewAction.Clear;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

procedure TFrmSDConstruction.mnuPrintChartClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    chartMain.Print;
end;

procedure TFrmSDConstruction.mnuPrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TFrmSDConstruction.mnuCopyChartClick(Sender: TObject);
begin
  chartMain.CopyToClipboardMetafile(True);
end;

resourcestring
  rsLoadFromFile = 'load from file';

procedure TFrmSDConstruction.FixWetDry(AAction: TSingleAction);
var
  i: Integer;
begin
  if AAction.Count<2 then Exit;
  Assert(AAction.Count mod 2 = 0);
  WetPeriodStartingMonth := AAction.Items[0].StartMonth;
  WetPeriodEndingMonth := AAction.Items[0].EndMonth;
  DryPeriodStartingMonth := AAction.Items[1].StartMonth;
  DryPeriodEndingMonth := AAction.Items[1].EndMonth;
  i := 0;
  while i < AAction.Count do
  begin
    AAction.Items[i].PrimaryCurve := True;
    AAction.Items[i+1].PrimaryCurve := False;
    AAction.Items[i].LinkedTo := AAction.Items[i+1];
    Inc(i, 2);
  end;
end;

procedure TFrmSDConstruction.mnuLoadClick(Sender: TObject);
var
  ANewAction: TSingleAction;
begin
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsLoadFromFile);
    if OpenDialog.Execute then
      ANewAction.LoadFromFile(OpenDialog.FileName);
    if Mode = icemDischargeSedimentDischarge then FixWetDry(ANewAction);
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

procedure TFrmSDConstruction.mnuSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    CurrentPointer.WriteToFile(SaveDialog.FileName, 'yyyy-mm-dd hh:nn',
      ',','.');
end;

procedure TFrmSDConstruction.mnuExitClick(Sender: TObject);
begin
  Close;
end;

resourcestring
  rsChangeCurveStartDate = 'change curve start date';

procedure TFrmSDConstruction.trackStartChange(Sender: TObject);
var
  ANewAction: TSingleAction;
  ADate: TDateTime;
  ADrift: Real;
begin
  if lstCurves.ItemIndex<0 then
    Exit;
  if not CurrentPointer[lstCurves.ItemIndex].PrimaryCurve then
    if lstCurves.ItemIndex <> lstCurves.ItemIndex -1 then
      lstCurves.ItemIndex := lstCurves.ItemIndex -1;
  ADate := AddDateTime((trackStart.Position/4095)*
    (seriesPeriods.MaxXValue-seriesPeriods.MinXValue),
      seriesPeriods.MinXValue);
  ADrift := (1.5/4096)*(seriesPeriods.MaxXValue-seriesPeriods.MinXValue)*
    86400;
  if Abs(DiffInSecs(ADate, CurrentPointer[lstCurves.ItemIndex].StartDate))<
    ADrift then
      Exit;
  if Abs(DiffInSecs(seriesPeriods.MinXValue,
    CurrentPointer[lstCurves.ItemIndex].StartDate))< ADrift then
  begin
    trackStart.Position := trackStart.Min;
    Exit;
  end;
  ANewAction := nil;
  try
    if CurrentPointer.Caption<>rsChangeCurveStartDate then
      ANewAction := GetNewAction(CurrentPointer, rsChangeCurveStartDate)
    else
      ANewAction := CurrentPointer;
    ANewAction[lstCurves.ItemIndex].StartDate := ADate;
    if not ANewAction[lstCurves.ItemIndex].Extension then
      SetDatesConsistent(ANewAction);
    if ANewAction<>CurrentPointer then
      FinilizeNewAction(ANewAction);
  except
    if ANewAction<>CurrentPointer then
      ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

resourcestring
  rsChangeCurveEndDate = 'change curve end date';

procedure TFrmSDConstruction.trackEndChange(Sender: TObject);
var
  ANewAction: TSingleAction;
  ADate: TDateTime;
  ADrift: Real;
begin
  if lstCurves.ItemIndex<0 then
    Exit;
  if not CurrentPointer[lstCurves.ItemIndex].PrimaryCurve then
    if lstCurves.ItemIndex <> lstCurves.ItemIndex -1 then
      lstCurves.ItemIndex := lstCurves.ItemIndex -1;
  ADate := AddDateTime((trackEnd.Position/4095)*
    (seriesPeriods.MaxXValue-seriesPeriods.MinXValue),
      seriesPeriods.MinXValue);
  ADrift := (1.5/4096)*(seriesPeriods.MaxXValue-seriesPeriods.MinXValue)*
    86400;
  if Abs(DiffInSecs(ADate, CurrentPointer[lstCurves.ItemIndex].EndDate))<
    ADrift then
      Exit;
  if Abs(DiffInSecs(seriesPeriods.MaxXValue,
    CurrentPointer[lstCurves.ItemIndex].EndDate))< ADrift then
  begin
    trackEnd.Position := trackEnd.Max;
    Exit;
  end;
  ANewAction := nil;
  try
    if CurrentPointer.Caption<>rsChangeCurveEndDate then
      ANewAction := GetNewAction(CurrentPointer, rsChangeCurveEndDate)
    else
      ANewAction := CurrentPointer;
    ANewAction[lstCurves.ItemIndex].EndDate := ADate;
    if not ANewAction[lstCurves.ItemIndex].Extension then
      SetDatesConsistent(ANewAction);
    if ANewAction<>CurrentPointer then
      FinilizeNewAction(ANewAction);
  except
    if ANewAction<>CurrentPointer then
      ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

procedure TFrmSDConstruction.tbtnMarkOutlierClick(Sender: TObject);
var
  i: Integer;
begin
  if tbtnMarkOutlier.Down then
  begin
    chartMain.OriginalCursor := crCross;
    for i := 0 to chartMain.SeriesCount-1 do
    begin
      if chartMain.Series[i] is TPointSeries then
        TPointSeries(chartMain.Series[i]).Cursor := crHandPoint;
    end;
  end else
  begin
    SetControlStatus;
  end;
end;

resourcestring
  rsMarkPointAsOutlier = 'mark point as outlier';
  rsUnmarkPointAsOutlier = 'unmark point as outlier';

procedure TFrmSDConstruction.chartMainClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  function ExtractOffset(ADate: TDateTime): Real;
  var
    i: Integer;
  begin
    Result := FDefaultOffset;
    if CurrentPointer.Count>0 then
    begin
      for i := 0 to CurrentPointer.Count-1 do
      begin
        if CurrentPointer[i].Extension then
          Continue;
        if DiffInSecs(ADate, CurrentPointer[i].StartDate)<-1 then
          Continue;
        if DiffInSecs(ADate, CurrentPointer[i].EndDate)>1 then
          Continue;
        Result := CurrentPointer[i].Offset.Independent;
        Break;
      end;
    end;
  end;

var
  AHydrometricPoint: THydrometricPoint;
  i: Integer;
  ANewAction: TSingleAction;
  s: string;
  AXValue, AYValue: Real;
begin
  if not tbtnMarkOutlier.Down then
    Exit;
  if Series is TPointSeries then
  begin
    for i := 0 to CurrentPointer.HydrometricPointsCount-1 do
    begin
      with TPointSeries(Series) do
      begin
        AHydrometricPoint := CurrentPointer.HydrometricPoints[i];
        if Mode = icemStageDischarge then
        begin
          AXValue := XValues[ValueIndex];
          AYValue := YValues[ValueIndex];
        end else begin
          AXValue := YValues[ValueIndex];
          AYValue := XValues[ValueIndex];
        end;
        if (Abs(AXValue -
          CurrentPointer.HydrometricPoints[i].Discharge)<0.0001) and
          (Abs(AYValue -
           CurrentPointer.HydrometricPoints[i].Stage-
           ExtractOffset(CurrentPointer.HydrometricPoints[i].Date))<0.0001) then
        begin
          AHydrometricPoint.Outlier := not AHydrometricPoint.Outlier;
          ANewAction := nil;
          try
            if AHydrometricPoint.Outlier then
              s := rsMarkPointAsOutlier else
              s := rsUnmarkPointAsOutlier;
            ANewAction := GetNewAction(CurrentPointer, s);
            ANewAction.HydrometricPoints[i] := AHydrometricPoint;
            FinilizeNewAction(ANewAction);
          except
            ANewAction.Free;
            raise;
          end;
          Break;
        end;
      end;
    end;
  end;
  SetControlStatus;
end;

procedure TFrmSDConstruction.chartPeriodsClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, ACurveIndex: Integer;
begin
  ACurveIndex := -1;
  for i := 0 to CurrentPointer.Count-1 do
    if CurrentPointer[i].GraphIndex = ValueIndex then
    begin
      ACurveIndex := i;
      Break;
    end;
  if ACurveIndex<0 then Exit;
  lstCurves.ItemIndex := ACurveIndex;
  SetControlStatus;
end;

procedure TFrmSDConstruction.mnuFixOverlapingDatesClick(Sender: TObject);
begin
  FFixesDates := not FFixesDates;
  SetControlStatus;
end;

resourcestring
  rsDeterminationCoefForAllIs =
    'Determination coefficient, calculated for all curves is: ';

procedure TFrmSDConstruction.mnuCalcDetCoefClick(Sender: TObject);
begin
  ShowMessage(rsDeterminationCoefForAllIs+
    FormatFloat('0.000',CurrentPointer.GetDeterminationCoefficient));
end;

resourcestring
  rsAbortChanges =
    'It seems that negative values of curve points are existing, considering '+
      'the effect of Offset. This may lead to several calculation problems. '+
        'Press OK to proceed or Cancel to return, then try to change offset';

procedure TFrmSDConstruction.LFormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
  if CheckForNegatives then
  begin
    if MessageDlg(rsAbortChanges, mtConfirmation, mbOKCancel, 0) = mrOK then
      CanClose := True;
  end else
    CanClose := True;
end;

function TFrmSDConstruction.CheckForNegatives: Boolean;
var
  i,j : Integer;
begin
  Result := False;
  for i := 0 to CurrentPointer.Count-1 do
  begin
    for j := 0 to CurrentPointer[i].Count-1 do
    begin
      with CurrentPointer[i] do
      begin
        if Points[j].Dependent<=0 then
          Result := True;
        if Points[j].Independent+Offset.Independent<=0 then
          Result := True;
      end;
    end;
    if CurrentPointer[i].Extension then Continue;
    for j := 0 to CurrentPointer.HydrometricPointsCount-1 do
    begin
      if DiffInSecs(CurrentPointer.HydrometricPoints[j].Date,
        CurrentPointer[i].StartDate)<-1 then
        Continue;
      if DiffInSecs(CurrentPointer.HydrometricPoints[j].Date,
        CurrentPointer[i].EndDate)>1 then
        Continue;
      if CurrentPointer.HydrometricPoints[j].Outlier then
        Continue;
      if CurrentPointer.HydrometricPoints[j].Stage+
        CurrentPointer[i].Offset.Independent<=0 then
        Result := True;
    end;
    if Result = True then
      Break;
  end;
end;

resourcestring
  rsAddExtension = 'add extension';
  rsOnlyOneExtensionAllowed = 'Only one extension curve is allowed';

procedure TFrmSDConstruction.btnAddExtensionClick(Sender: TObject);
var
  i: Integer;
  ANewAction: TSingleAction;
  ACurve: TTransientCurve;
  AOffset: TCurvePoint;
begin
  for i := 0 to CurrentPointer.Count-1 do
    if CurrentPointer[i].Extension then
      raise Exception.Create(rsOnlyOneExtensionAllowed);
  ANewAction := nil;
  ACurve := nil;
  try
    ANewAction := GetNewAction(CurrentPointer,  rsAddExtension);
    ACurve := TTransientCurve.Create(True);
    ACurve.Extension := True;
    ACurve.StartDate := seriesPeriods.MinXValue;
    ACurve.EndDate := seriesPeriods.MaxXValue;
    AOffset.Independent := FDefaultOffset;
    ACurve.Offset := AOffset;
    ANewAction.Add(ACurve);
    ACurve := nil;
    FinilizeNewAction(ANewAction);
  except
    ACurve.Free;
    ANewAction.Free;
  end;
  SetControlStatus;
end;

resourcestring
  rsInsertHydraulicCurve =
    'construct curve with hydraulic calculations';

procedure TFrmSDConstruction.btnHydraulicClick(Sender: TObject);
var
  FrmHydraulics: TFrmHydrXSections;
  ANewAction: TSingleAction;
begin
  if lstCurves.ItemIndex<0 then
    Exit;
  FrmHydraulics := nil;
  ANewAction := nil;
  try try
    ANewAction := GetNewAction(CurrentPointer, rsInsertHydraulicCurve);
    FrmHydraulics := TFrmHydrXSections.Create(Self);
    FrmHydraulics.TransientCurveList := TTransientCurveList(ANewAction);
    FrmHydraulics.CurveNo := lstCurves.ItemIndex;
    if FrmHydraulics.ShowModal = mrOK then
      FinilizeNewAction(ANewAction) else
      ANewAction.Free;
  except
    ANewAction.Free;
    raise;
  end;
  finally
    FrmHydraulics.Free;
  end;
  SetControlStatus;
end;

procedure TFrmSDConstruction.mnuExportTimeseriesClick(Sender: TObject);
begin
  TimeseriesGrid.Add(CurrentPointer.ExtractStageTS);
  TimeseriesGrid.Add(CurrentPointer.ExtractDischargeTS);
end;

resourcestring
  rsImportTimeseries = 'import time series';

procedure TFrmSDConstruction.mnuImportTimeseriesClick(Sender: TObject);
var
  AForm: TSelectSDTimeseries;
  ANewAction: TSingleAction;
begin
  AForm := nil;
  ANewAction := nil;
  try try
    ANewAction := GetNewAction(CurrentPointer, rsImportTimeseries);
    AForm := TSelectSDTimeseries.Create(Self);
    AForm.TimeseriesGrid := TimeseriesGrid;
    AForm.TransientCurveList := ANewAction as TTransientCurveList;
    if AForm.ShowModal = mrOK then
      FinilizeNewAction(ANewAction) else
      ANewAction.Free;
  except
    ANewAction.Free;
    raise
  end;
  finally
    AForm.Free;
  end;
  SetControlStatus;
end;

end.
