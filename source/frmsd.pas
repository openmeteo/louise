{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2005   National Technical University of Athens    }
{                                                                  }
{******************************************************************}

{** A Form for stage-discharge analysis}

unit frmsd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tsgrid, Ts, interpol, StdCtrls, ExtCtrls, VclTee.TeeProcs, VclTee.TeEngine,
  VclTee.Series, VclTee.Chart, Dates, Math, Menus, tsprocess, Contnrs,
  VclTee.TeeGDIPlus;

type
  TFrmStageDischarge = class(TForm)
    Chart1: TChart;
    chkLogYAxis: TCheckBox;
    chkLogXAxis: TCheckBox;
    btnShowMeasurements: TButton;
    btnCalculate: TButton;
    grpLoadCurves: TGroupBox;
    Label3: TLabel;
    chkH1Correction: TCheckBox;
    chkMergeData: TCheckBox;
    chkH2Correction: TCheckBox;
    chkStout: TCheckBox;
    chkCalculateDischarge: TCheckBox;
    Label4: TLabel;
    btnLoadFromDatabase: TButton;
    grpSaveCurves: TGroupBox;
    btnSaveToDatabase: TButton;
    rgrpTypeOfCalculations: TRadioGroup;
    grpOptions: TGroupBox;
    pnlStageDischarge: TPanel;
    rgrpStageDischarge: TRadioGroup;
    pnlSpillway: TPanel;
    rgrpSpillway: TRadioGroup;
    pnlVolume: TPanel;
    rgrpVolume: TRadioGroup;
    pnlLeakage: TPanel;
    rgrpLeakage: TRadioGroup;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnCancel: TButton;
    btnEdit: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    procedure btnLoadClick(Sender: TObject);
    procedure IFormCreate(Sender: TObject);
    procedure IFormDestroy(Sender: TObject);
    procedure IFormShow(Sender: TObject);
    procedure IFormHide(Sender: TObject);
    procedure chkLogYAxisClick(Sender: TObject);
    procedure chkLogXAxisClick(Sender: TObject);
    procedure btnShowMeasurementsClick(Sender: TObject);
    procedure btnCalculateClick(Sender: TObject);
    procedure btnLoadFromDatabaseClick(Sender: TObject);
    procedure btnSaveToDatabaseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure rgrpTypeOfCalculationsClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  private
    FCurvesLoaded: Boolean;
    FSeries: array of TLineSeries;
    FPSeries: array of TPointSeries;
    FColor: array [1..8] of TColor;
    FSeriesCount: Integer;
    FColorIndex: Integer;
    FTransientCurveList: TTransientCurveList;
    FVolumeCurve: TInterpolatingCurve;
    FAreaCurve: TInterpolatingCurve;
    FSpillway: TInterpolatingCurve;
    FLeakage: TLeakageInterpolation;
    FTSGridReq: array [0..3] of Integer;
    FCalculationType: Integer;
    procedure SetAxisLimits;
    procedure RedrawChart;
    procedure DoCalculate;
    procedure SetControlStatus;
  public
    TimeSeriesGrid: TTimeSeriesGrid;
    DBSession: TComponent;
    DenseTSIndex, SparseTSIndex, HQStageTSIndex, HQDischTSIndex: Integer;
  end;

implementation

{$R *.DFM}

uses
  istrutils, {interpoldb,} frmsdcon{, frmsdsel}, genopts;

procedure TFrmStageDischarge.IFormCreate(Sender: TObject);
begin
  FTransientCurveList := TTransientCurveList.Create;
  FVolumeCurve := TInterpolatingCurve.Create(True);
  FAreaCurve := TInterpolatingCurve.Create(True);
  FLeakage := TLeakageInterpolation.Create;
  FSpillway := TInterpolatingCurve.Create(True);
end;

procedure TFrmStageDischarge.IFormDestroy(Sender: TObject);
begin
  FTransientCurveList.Free;
  FVolumeCurve.Free;
  FAreaCurve.Free;
  FLeakage.Free;
  FSpillway.Free;
end;

procedure TFrmStageDischarge.IFormShow(Sender: TObject);
begin
{Colours for different periods of TransientCurveList
 limited to 8 colour since this is the separative
 capacity of the human eye}
  FColor[1] := clBlack;
  FColor[2] := clGreen;
  FColor[3] := clBlue;
  FColor[4] := clRed;
  FColor[5] := clOlive;
  FColor[6] := clYellow;
  FColor[7] := clPurple;
  FColor[8] := clNavy;
  FSeriesCount := 0;
  chkLogYAxis.Checked := Chart1.LeftAxis.Logarithmic;
  chkLogXAxis.Checked := Chart1.BottomAxis.Logarithmic;
{The array FTSGridReq keeps the required time series
 for calculations and corrections}
  FTSGridReq[0] := DenseTSIndex;
  FTSGridReq[1] := SparseTSIndex;
  FTSGridReq[2] := HQStageTSIndex;
  FTSGridReq[3] := HQDischTSIndex;
  FCurvesLoaded := False;
  SetControlStatus;
end;

resourcestring
  rsCalculateDischarge = 'Calculate discharge';
  rsCalculateSedimentDischarge = 'Calculate sediment discharge';
  rsCaptionStageDischarge = 'Interpolations (Stage-Discharge)';
  rsCaptionDischargeSedimentDischarge =
    'Interpolations (Discharge-Sediment Discharge)';
  rsStage = 'Stage (m)';
  rsDischarge = 'Discharge (m^3/s)';
  rsSedimentDischarge = 'Sediment discharge (kg/s)';
  rsLeakage = 'Leakage (hm^3/month)';
  rsVolumeArea = 'Volume/Area (hm^3, km^2)';
  rsCaptionVolume = 'Interpolations (Stage-Area, Volume)';
  rsCalculateVolume = 'Calculate Area, Volume';
  rsCaptionStageLeakage = 'Interpolations (Stage-Leakage)';
  rsCalculateLeakage = 'Calculate Leakage';
  rsCaptionSpillway = 'Interpolations (Spillway Stage-Discharge)';  

procedure TFrmStageDischarge.SetControlStatus;
begin
  btnCalculate.Enabled := (FTSGridReq[0]>-1);

  chkCalculateDischarge.Enabled := (FTSGridReq[0]>-1) and FCurvesLoaded;

  chkH1Correction.Enabled := (FTSGridReq[1]>-1) and
    (rgrpTypeOfCalculations.ItemIndex<4);
  chkMergeData.Enabled := chkH1Correction.Enabled;

  chkH2Correction.Enabled := (FTSGridReq[2]>-1) and (FTSGridReq[3]>-1) and
    (rgrpTypeOfCalculations.ItemIndex=0);
  chkStout.Enabled := chkH2Correction.Enabled;

  btnShowMeasurements.Enabled := ((rgrpTypeOfCalculations.ItemIndex = 0) or
    (rgrpTypeOfCalculations.ItemIndex = 4)) and
    (FTSGridReq[2]>-1) and (FTSGridReq[3]>-1);

  case rgrpTypeOfCalculations.ItemIndex of
    0:
    begin
      Caption := rsCaptionStageDischarge;
      chkCalculateDischarge.Caption := rsCalculateDischarge;
      btnLoad.Enabled := True;
      btnEdit.Enabled := True;
      btnSaveToDatabase.Enabled := True;
      btnSave.Enabled := True;
      chkLogXAxis.Checked := True;
      chkLogYAxis.Checked := True;
      pnlStageDischarge.Visible := True;
      pnlVolume.Visible := False;
      pnlLeakage.Visible := False;
      pnlSpillway.Visible := False;
      Chart1.LeftAxis.Title.Caption := rsStage;
      Chart1.BottomAxis.Title.Caption := rsDischarge;
    end;
    1:
    begin
      Caption := rsCaptionVolume;
      chkCalculateDischarge.Caption := rsCalculateVolume;
      btnLoad.Enabled := True;
      btnEdit.Enabled := False;
      btnSaveToDatabase.Enabled := True;
      btnSave.Enabled := False;
      chkLogXAxis.Checked := True;
      chkLogYAxis.Checked := False;
      pnlStageDischarge.Visible := False;
      pnlVolume.Visible := True;
      pnlLeakage.Visible := False;
      pnlSpillway.Visible := False;
      Chart1.LeftAxis.Title.Caption := rsStage;
      Chart1.BottomAxis.Title.Caption := rsVolumeArea;
    end;
    2:
    begin
      Caption := rsCaptionStageLeakage;
      chkCalculateDischarge.Caption := rsCalculateLeakage;
      btnLoad.Enabled := True;
      btnEdit.Enabled := False;
      btnSaveToDatabase.Enabled := False;
      btnSave.Enabled := False;
      chkLogXAxis.Checked := False;
      chkLogYAxis.Checked := False;
      pnlStageDischarge.Visible := False;
      pnlVolume.Visible := False;
      pnlLeakage.Visible := True;
      pnlSpillway.Visible := False;
      Chart1.LeftAxis.Title.Caption := rsLeakage;
      Chart1.BottomAxis.Title.Caption := rsStage;
    end;
    3:
    begin
      Caption := rsCaptionSpillway;
      chkCalculateDischarge.Caption := rsCalculateDischarge;
      btnLoad.Enabled := True;
      btnEdit.Enabled := False;
      btnSaveToDatabase.Enabled := False;
      btnSave.Enabled := False;
      chkLogXAxis.Checked := True;
      chkLogYAxis.Checked := True;
      pnlStageDischarge.Visible := False;
      pnlVolume.Visible := False;
      pnlLeakage.Visible := False;
      pnlSpillway.Visible := True;
      Chart1.LeftAxis.Title.Caption := rsStage;
      Chart1.BottomAxis.Title.Caption := rsDischarge;
    end;
    4:
    begin
      Caption := rsCaptionDischargeSedimentDischarge;
      chkCalculateDischarge.Caption := rsCalculateSedimentDischarge;
      btnLoad.Enabled := True;
      btnEdit.Enabled := True;
      btnSaveToDatabase.Enabled := True;
      btnSave.Enabled := True;
      chkLogXAxis.Checked := True;
      chkLogYAxis.Checked := True;
      pnlStageDischarge.Visible := False;
      pnlVolume.Visible := False;
      pnlLeakage.Visible := False;
      pnlSpillway.Visible := False;
      Chart1.LeftAxis.Title.Caption := rsSedimentDischarge;
      Chart1.BottomAxis.Title.Caption := rsDischarge;
    end;
  end;
end;

procedure TFrmStageDischarge.SetAxisLimits;
var
  xmin, xmax, ymin, ymax, hundreds: Real;
begin
  xmin := Chart1.MinXValue(Chart1.BottomAxis);
  xmax := Chart1.MaxXValue(Chart1.BottomAxis);
  if chkLogXAxis.Checked then
  begin
    if xmin <= 0 then xmin := 0.001;
    if xmax <= 0 then xmax := 0.001;
    Chart1.BottomAxis.Minimum := Power(10.000,(Floor(Log10(xmin))));
    Chart1.BottomAxis.Maximum := Power(10.000,(Floor(Log10(xmax))+1));
  end
  else begin
    hundreds := Power(10.000,(Floor(Log10(abs(xmin)+0.0005))));
    Chart1.BottomAxis.Minimum := Floor(xmin/hundreds)*hundreds;
    hundreds := Power(10.000,(Floor(Log10(abs(xmax)+0.0005))));
    Chart1.BottomAxis.Maximum := (Floor(xmax/hundreds)+1)*hundreds;
  end;
  ymin := Chart1.MinYValue(Chart1.LeftAxis);
  ymax := Chart1.MaxYValue(Chart1.LeftAxis);
  if chkLogYAxis.Checked then
  begin
    if (ymin <= 0) then ymin := 0.001;
    if (ymax <= 0) then ymax := 0.001;
    Chart1.LeftAxis.Minimum := Power(10.000,(Floor(Log10(ymin))));
    Chart1.LeftAxis.Maximum := Power(10.000,(Floor(Log10(ymax))+1));
  end
  else begin
    hundreds := Power(10.000,(Floor(Log10(abs(ymin)+0.0005))));
    Chart1.LeftAxis.Minimum := Floor(ymin/hundreds)*hundreds;
    hundreds := Power(10.000,(Floor(Log10(abs(ymax)+0.0005))));
    Chart1.LeftAxis.Maximum := (Floor(ymax/hundreds)+1)*hundreds;
  end;
end;

procedure TFrmStageDischarge.RedrawChart;
var
  i,j: Integer;
  x,y,CoefSum,CoefSumOld: Real;
begin
  Chart1.RemoveAllSeries;
  SetLength(FPSeries,0);
  FColorIndex := 0;
  x := 0; //Initialization to avoid silly compiler warnings
  y := 0;
{Start with some conventional axis limits derived from
 the first point of TransientCurveList}
  if FCalculationType = 0 then
  begin
    if FTransientCurveList.Count<1 then
      Exit;
    if FTransientCurveList[0].Count<1 then
      Exit;
    y := FTransientCurveList[0].Points[0].Independent+
      FTransientCurveList[0].Offset.Independent;
    x := FTransientCurveList[0].Points[0].Dependent;
  end else if FCalculationType = 1 then
  begin
    if FVolumeCurve.Count<1 then
      Exit;
    y := FVolumeCurve.Points[0].Independent;
    x := FVolumeCurve.Points[0].Dependent;
  end else if FCalculationType = 2 then
  begin
    x := 0.5*(FLeakage.Hmax+FLeakage.Hmin);
    y := 0.5*(FLeakage.Interpolate(1,x)+FLeakage.Interpolate(7,x));
  end else if FCalculationType = 3 then
  begin
    if FSpillway.Count<1 then
      Exit;
    y := FSpillway.Points[0].Independent;
    x := FSpillway.Points[0].Dependent;
  end else if FCalculationType = 4 then
  begin
    if FTransientCurveList.Count<1 then
      Exit;
    if FTransientCurveList[0].Count<1 then
      Exit;
    x := FTransientCurveList[0].Points[0].Independent+
      FTransientCurveList[0].Offset.Independent;
    y := FTransientCurveList[0].Points[0].Dependent;
  end;
  if Chart1.BottomAxis.Maximum > x+0.001 then
    Chart1.BottomAxis.Minimum := x;
  Chart1.BottomAxis.Maximum := x+0.001;
  Chart1.BottomAxis.Minimum := x;
  if Chart1.LeftAxis.Maximum > y+0.001 then
    Chart1.LeftAxis.Minimum := y;
  Chart1.LeftAxis.Maximum := y+0.001;
  Chart1.LeftAxis.Minimum := y;
  if FCalculationType = 0 then
  begin
  for i := 0 to FTransientCurveList.Count-1 do
  begin
    Inc(FSeriesCount);
    SetLength(FSeries,FSeriesCount);
    FSeries[FSeriesCount-1] := TLineSeries.Create(self);
    FSeries[FSeriesCount-1].ParentChart := Chart1;
    Inc(FColorIndex);
    FSeries[FSeriesCount-1].SeriesColor := FColor[FColorIndex];
    FSeries[FSeriesCount-1].XValues.Order := loNone;
    FSeries[FSeriesCount-1].YValues.Order := loNone;
    FSeries[FSeriesCount-1].Title :=
    DateToStr(FTransientCurveList[i].StartDate)+' - '+
    DateToStr(FTransientCurveList[i].EndDate);
    if FTransientCurveList[i].Extension then
      FSeries[FSeriesCount-1].Title := FSeries[FSeriesCount-1].Title+' (Ext.)';
{Recycle colours after colorindex8}
    if FColorIndex>8 then FColorIndex := 1;
    for j := 0 to TTransientCurve(FTransientCurveList.Items[i]).Count-1 do
    begin
      With FSeries[FSeriesCount-1] do
      begin
        y := FTransientCurveList[i].Points[j].Independent+
          FTransientCurveList[i].Offset.Independent;
        x := FTransientCurveList[i].Points[j].Dependent;
        AddXY(x,y,'',FColor[FColorIndex]);
      end;
    end;
  end;
  end else if FCalculationType = 1 then
  begin
    Inc(FSeriesCount);
    SetLength(FSeries,FSeriesCount);
    FSeries[FSeriesCount-1] := TLineSeries.Create(self);
    FSeries[FSeriesCount-1].ParentChart := Chart1;
    Inc(FColorIndex);
    FSeries[FSeriesCount-1].SeriesColor := FColor[FColorIndex];
    FSeries[FSeriesCount-1].Title :='Volume';
    if FColorIndex>8 then FColorIndex := 1;
    for j := 0 to FVolumeCurve.Count-1 do
    begin
      With FSeries[FSeriesCount-1] do
      begin
        y := FVolumeCurve.Points[j].Independent;
        x := FVolumeCurve.Points[j].Dependent;
        AddXY(x,y,'',FColor[FColorIndex]);
      end;
    end;
    Inc(FSeriesCount);
    SetLength(FSeries,FSeriesCount);
    FSeries[FSeriesCount-1] := TLineSeries.Create(self);
    FSeries[FSeriesCount-1].ParentChart := Chart1;
    Inc(FColorIndex);
    FSeries[FSeriesCount-1].SeriesColor := FColor[FColorIndex];
    FSeries[FSeriesCount-1].Title :='Area';
    if FColorIndex>8 then FColorIndex := 1;
    for j := 0 to FAreaCurve.Count-1 do
    begin
      With FSeries[FSeriesCount-1] do
      begin
        y := FAreaCurve.Points[j].Independent;
        x := FAreaCurve.Points[j].Dependent;
        AddXY(x,y,'',FColor[FColorIndex]);
      end;
    end;
  end else if FCalculationType = 2 then
  begin
    CoefSumOld := 0.000;
    for i := 1 to 12 do
    begin
      CoefSum := Fleakage.Interpolate(i,Fleakage.Hmin)+
        Fleakage.Interpolate(i,Fleakage.Hmax)+
        Fleakage.Interpolate(i,0.5*(Fleakage.Hmax+Fleakage.Hmin));
      if CoefSum<>CoefSumOld then
      begin
        CoefSumOld := CoefSum;
        Inc(FSeriesCount);
        SetLength(FSeries,FSeriesCount);
        FSeries[FSeriesCount-1] := TLineSeries.Create(self);
        FSeries[FSeriesCount-1].ParentChart := Chart1;
        Inc(FColorIndex);
        FSeries[FSeriesCount-1].SeriesColor := FColor[FColorIndex];
        FSeries[FSeriesCount-1].Title := FormatSettings.ShortMonthNames[i]+'- ';
        if FColorIndex>8 then FColorIndex := 1;
        for j := 0 to 20 do
        begin
          With FSeries[FSeriesCount-1] do
          begin
            x := (FLeakage.Hmax-FLeakage.Hmin)*(j/20)+FLeakage.Hmin;
            y := FLeakage.Interpolate(i,x);
            AddXY(x,y,'',FColor[FColorIndex]);
          end;
        end;
      end;
    end;
  end else if FCalculationType = 3 then
  begin
    Inc(FSeriesCount);
    SetLength(FSeries,FSeriesCount);
    FSeries[FSeriesCount-1] := TLineSeries.Create(self);
    FSeries[FSeriesCount-1].ParentChart := Chart1;
    Inc(FColorIndex);
    FSeries[FSeriesCount-1].SeriesColor := FColor[FColorIndex];
    FSeries[FSeriesCount-1].Title :='Spillway Discharge';
    if FColorIndex>8 then FColorIndex := 1;
    for j := 0 to FSpillway.Count-1 do
    begin
      With FSeries[FSeriesCount-1] do
      begin
        y := FSpillway.Points[j].Independent;
        x := FSpillway.Points[j].Dependent;
        AddXY(x,y,'',FColor[FColorIndex]);
      end;
    end;
  end else if FCalculationType = 4 then
  begin
  for i := 0 to FTransientCurveList.Count-1 do
  begin
    Inc(FSeriesCount);
    SetLength(FSeries,FSeriesCount);
    FSeries[FSeriesCount-1] := TLineSeries.Create(self);
    FSeries[FSeriesCount-1].ParentChart := Chart1;
    Inc(FColorIndex);
    FSeries[FSeriesCount-1].SeriesColor := FColor[FColorIndex];
    FSeries[FSeriesCount-1].XValues.Order := loNone;
    FSeries[FSeriesCount-1].YValues.Order := loNone;
    FSeries[FSeriesCount-1].Title :=
    DateToStr(FTransientCurveList[i].StartDate)+' - '+
    DateToStr(FTransientCurveList[i].EndDate);
    if FTransientCurveList[i].PrimaryCurve then
      FSeries[FSeriesCount-1].Title := FSeries[FSeriesCount-1].Title +
      ' (Wet Period)' else
      FSeries[FSeriesCount-1].Title := FSeries[FSeriesCount-1].Title +
      ' (Dry Period)';
    if FTransientCurveList[i].Extension then
      FSeries[FSeriesCount-1].Title := FSeries[FSeriesCount-1].Title+' (Ext.)';
{Recycle colours after colorindex8}
    if FColorIndex>8 then FColorIndex := 1;
    for j := 0 to TTransientCurve(FTransientCurveList.Items[i]).Count-1 do
    begin
      With FSeries[FSeriesCount-1] do
      begin
        x := FTransientCurveList[i].Points[j].Independent+
          FTransientCurveList[i].Offset.Independent;
        y := FTransientCurveList[i].Points[j].Dependent;
        AddXY(x,y,'',FColor[FColorIndex]);
      end;
    end;
  end;
  end;
  {Set the right min and max for the axes}
  if FSeriesCount>0 then
    SetAxisLimits;
end;

procedure TFrmStageDischarge.IFormHide(Sender: TObject);
begin
  Chart1.RemoveAllSeries;
  SetLength(FSeries,0);
  SetLength(FPSeries,0);
  FTransientCurveList.Clear;
  FVolumeCurve.Clear;
  FAreaCurve.Clear;
end;

procedure TFrmStageDischarge.chkLogYAxisClick(Sender: TObject);
begin
  Chart1.LeftAxis.Logarithmic := chkLogYAxis.Checked;
  if FSeriesCount > 0 then
    SetAxisLimits;
end;

procedure TFrmStageDischarge.chkLogXAxisClick(Sender: TObject);
begin
  Chart1.BottomAxis.Logarithmic := chkLogXAxis.Checked;
  if FSeriesCount > 0 then
    SetAxisLimits;
end;

procedure TFrmStageDischarge.btnShowMeasurementsClick(Sender: TObject);
var
  i,j: Integer;
  x,y: Real;
  CommonPeriod: TDateTimeList;
  AList: TObjectList;
begin
  CommonPeriod := nil;
  AList := nil;
  if (FTransientCurveList.Count > 0) and (FTSGridReq[2]>-1)
    and (FTSGridReq[3]>-1) then
  begin
    try
      AList := TObjectList.Create(False);
      AList.Add(TimeSeriesGrid.Data[FTSGridReq[3]]);
      AList.Add(TimeSeriesGrid.Data[FTSGridReq[2]]);
      CommonPeriod := GetCommonPeriod(AList,0);
      SetLength(FPSeries,FTransientCurveList.Count);
      FColorIndex := 0;
{Consider all curves, then check if a measurement
 point belogs to curve's time period, then plot
 this point with the same colour as the curve}
      for i := 0 to FTransientCurveList.Count-1 do
      begin
{Uses one pointseries but the same colours used by
 corresponding curves}
        Inc(FColorIndex);
        if FColorIndex>8 then FColorIndex := 1;
        FPSeries[i] := TPointSeries.Create(self);
        FPSeries[i].ParentChart := Chart1;
        FPSeries[i].Pointer.Style := psTriangle;
        FPSeries[i].SeriesColor := FColor[FColorIndex];
        FPSeries[i].Title := '';
        FPSeries[i].ShowInLegend := False;
        for j := 0 to CommonPeriod.Count-1 do
        begin
          if (DiffInSecs(CommonPeriod[j], FTransientCurveList[i].StartDate)>=0) and
             (DiffInSecs(CommonPeriod[j], FTransientCurveList[i].EndDate)<0) and
              (not FTransientCurveList[i].Extension) then
          begin
            with TimeSeriesGrid.Data[FTSGridReq[3]] do
              x := Items[IndexOf(CommonPeriod[j])].AsFloat;
            with TimeSeriesGrid.Data[FTSGridReq[2]] do
              y := Items[IndexOf(CommonPeriod[j])].AsFloat+
                FTransientCurveList[i].Offset.Independent;
            if FCalculationType = 4 then
              FPSeries[i].AddXY(y,x,'',FColor[FColorIndex]) else
              FPSeries[i].AddXY(x,y,'',FColor[FColorIndex]);
          end;
        end;
      end;
{Update axes limits as usual...}
      SetAxisLimits;
    finally
      AList.Free;
      CommonPeriod.Free;
    end;
  end;
  SetControlStatus;
end;

procedure TFrmStageDischarge.DoCalculate;
var
  ATimeSeries: TTimeSeries;
  NewTsIndex, H1TSIndex, H2TSIndex: Integer;
  i: Integer;
  ATimeStep: TTimestep;
  ATimeStepStrict: Boolean;
begin
{If no TS present, do not make any calculations}
  if TimeSeriesGrid.Count < 1 then
    exit;
{Some initializations for the case where no
 corrections are consedered}
  NewTSIndex := FTSGridReq[0];
  H1TSIndex := FTSGridReq[0];
  ATimeStep := TimeSeriesGrid.Data[FTSGridReq[0]].TimeStep;
  ATimeStepStrict := TimeSeriesGrid.Data[FTSGridReq[0]].TimeStepStrict;
{}
  if ((chkH1Correction.Checked) and (chkH1Correction.Enabled))
    or ((chkMergeData.Checked) and (chkMergeData.Enabled))
      and (FTSGridReq[0]>-1) and (FTSGridReq[1]>-1) then
  begin
{First, interpolate diffs (corrections) from sparse to dense timeseries }
    if (chkH1Correction.Checked) and (chkH1Correction.Enabled) then
    begin
      ATimeSeries := TTimeSeries.Create;
      ATimeSeries.MUnit := 'm';
      ATimeSeries.Title := 'DeltaH1';
      ATimeSeries.VariableType := vtInstantaneous;
      ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
      ATimeSeries.TimeStep := ATimeStep;
      InterpolateDiffs(TimeSeriesGrid.Data[FTSGridReq[1]],
                     TimeSeriesGrid.Data[FTSGridReq[0]],
                     ATimeSeries);
      NewTsIndex := TimeSeriesGrid.Add(ATimeSeries);
    end;
{Now, add diffs and merge sparse and dense timeseries }
    ATimeSeries := TTimeSeries.Create;
    ATimeSeries.Assign(TimeSeriesGrid.Data[FTSGridReq[0]]);
    ATimeSeries.MUnit := 'm';
    ATimeSeries.Title := 'Corrected H1';
    ATimeSeries.VariableType := vtInstantaneous;
    ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
    ATimeSeries.TimeStep := ATimeStep;
{}
    if (chkH1Correction.Checked) and (chkH1Correction.Enabled) then
    begin
      for i := 0 to ATimeSeries.Count-1 do
      begin
        if not TimeSeriesGrid.Data[NewTsIndex].Items[i].IsNull then
          ATimeSeries.Items[i].AsFloat := ATimeSeries.Items[i].AsFloat+
          TimeSeriesGrid.Data[NewTsIndex].Items[i].AsFloat;
      end;
    end;
{And merge sparse and corrected dense data on demand}
    if (chkMergeData.Checked) and (chkMergeData.Enabled)
      and (FTSGridReq[0]>-1) and (FTSGridReq[1]>-1) then
    begin
      if TimeSeriesGrid.Data[FTSGridReq[1]].TimeStep = tstVariable then
        ATimeStep := TimeSeriesGrid.Data[FTSGridReq[1]].TimeStep;
      if ATimeStep = tstVariable then
        ATimeStepStrict := False;
      ATimeSeries.TimeStepStrict := ATimeStepStrict;
      ATimeSeries.Merge(TimeSeriesGrid.Data[FTSGridReq[1]]);
    end;
{Making all tsrecords new...}
    for i := 0 to ATimeSeries.Count-1 do
      ATimeSeries.Items[i].MStatus := msNew;
    H1TSIndex := TimeSeriesGrid.Add(ATimeSeries);
  end;
{}
  H2TSIndex := H1TSIndex;
{}
  if (chkH2Correction.Checked) and (chkH2Correction.Enabled)
    and (FCalculationType = 0) and (FTSGridReq[2]>-1) then
  begin
{Again, interpolate diffs from sparse to dense timeseries for
 stage from discharge, stage measurments}
    ATimeSeries := TTimeSeries.Create;
    ATimeSeries.MUnit := 'm';
    ATimeSeries.Title := 'DeltaH2';
    ATimeSeries.VariableType := vtInstantaneous;
    ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
    ATimeSeries.TimeStep := TimeSeriesGrid.Data[FTSGridReq[2]].TimeStep;
    InterpolateDiffs(TimeSeriesGrid.Data[FTSGridReq[2]],
                     TimeSeriesGrid.Data[H1TSIndex],
                     ATimeSeries);
    NewTsIndex := TimeSeriesGrid.Add(ATimeSeries);
{Now, add diffs but do NOT merge sparse and dense timeseries }
    ATimeSeries := TTimeSeries.Create;
    ATimeSeries.Assign(TimeSeriesGrid.Data[H1TSIndex]);
    ATimeSeries.MUnit := 'm';
    ATimeSeries.Title := 'Corrected H2';
    ATimeSeries.TimeStep := ATimeStep;
    ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
    ATimeSeries.VariableType := vtInstantaneous;
    for i := 0 to ATimeSeries.Count-1 do
    begin
      if not TimeSeriesGrid.Data[NewTsIndex].Items[i].IsNull then
        ATimeSeries.Items[i].AsFloat := ATimeSeries.Items[i].AsFloat+
        TimeSeriesGrid.Data[NewTsIndex].Items[i].AsFloat;
    end;
    for i := 0 to ATimeSeries.Count-1 do
      ATimeSeries.Items[i].MStatus := msNew;
    H2TSIndex := TimeSeriesGrid.Add(ATimeSeries);
  end;
{}
  if (chkStout.Checked) and (chkStout.Enabled)
    and (FCurvesLoaded) and (FCalculationType = 0)
      and (FTSGridReq[2]>-1) and (FTSGridReq[3]>-1) then
  begin
{Make Stout Correction on demand, fist and interpolate
 diffs}
    ATimeSeries := TTimeSeries.Create;
    ATimeSeries.Assign(TimeSeriesGrid.Data[H1TSIndex]);
    ATimeSeries.MUnit := 'm';
    ATimeSeries.Title := 'Stout DeltaH';
    ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
    ATimeSeries.VariableType := vtInstantaneous;
    ATimeSeries.TimeStep := ATimeStep;
    StageCorrect(TimeSeriesGrid.Data[H2TSIndex],
      TimeSeriesGrid.Data[FTSGridReq[2]],TimeSeriesGrid.Data[FTSGridReq[3]],
        FTransientCurveList, ATimeSeries);
    DecreaseStageCorrect(FTransientCurveList, TimeSeriesGrid.Data[H2TSIndex],
      ATimeSeries);
    for i := 0 to ATimeSeries.Count-1 do
      ATimeSeries.Items[i].MStatus := msNew;
    NewTSIndex := TimeSeriesGrid.Add(ATimeSeries);
{Add diffs...}
    ATimeSeries := TTimeSeries.Create;
    ATimeSeries.Assign(TimeSeriesGrid.Data[H2TSIndex]);
    ATimeSeries.MUnit := 'm';
    ATimeSeries.Title := 'Corrected H';
    ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
    ATimeSeries.VariableType := vtInstantaneous;
    ATimeSeries.TimeStep := ATimeStep;
    for i := 0 to ATimeSeries.Count-1 do
    begin
      if not TimeSeriesGrid.Data[NewTsIndex].Items[i].IsNull then
        ATimeSeries.Items[i].AsFloat := ATimeSeries.Items[i].AsFloat+
        TimeSeriesGrid.Data[NewTsIndex].Items[i].AsFloat;
    end;
    for i := 0 to ATimeSeries.Count-1 do
      ATimeSeries.Items[i].MStatus := msNew;
    NewTSIndex := TimeSeriesGrid.Add(ATimeSeries);
  end;
{Finally, calculate discharge, only when a curve list is loaded}
  if (chkCalculateDischarge.Checked) and (chkCalculateDischarge.Enabled)
    and FCurvesLoaded then
  begin
    if FCalculationType = 0 then
    begin
      ATimeSeries := TTimeSeries.Create;
      ATimeSeries.MUnit := 'm^3/s';
      ATimeSeries.Title := 'Calculated Discharge';
      ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
      ATimeSeries.VariableType := vtInstantaneous;
      ATimeSeries.TimeStep := ATimeStep;
      ATimeSeries.TimeStepStrict := ATimeStepStrict;
      TsInterpolate(TimeSeriesGrid.Data[NewTSIndex], ATimeSeries,
        FTransientCurveList);
      for i := 0 to ATimeSeries.Count-1 do
        ATimeSeries.Items[i].MStatus := msNew;
      NewTSIndex := TimeSeriesGrid.Add(ATimeSeries);
{Calculate cumulative Monthly and Annual Discharge on demand}
      if rgrpStageDischarge.ItemIndex = 1 then
      begin
{  Monthly...}
        ATimeSeries := TTimeSeries.Create;
        ATimeSeries.MUnit := 'hm^3/month';
        ATimeSeries.Title := 'Cumulative Discharge';
        ATimeSeries.VariableType := vtCumulative;
        ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
        ATimeSeries.TimeStep := tstMonthly;
        ATimeSeries.TimeStepStrict := True;
        TimeseriesIntegrate(TimeSeriesGrid.Data[NewTsIndex],ATimeSeries,1,
          tstMonthly,1000000.000,False);
        TimeSeriesGrid.Add(ATimeSeries);
{  Yearly...}
        ATimeSeries := TTimeSeries.Create;
        ATimeSeries.MUnit := 'hm^3/year';
        ATimeSeries.Title := 'Cumulative Discharge';
        ATimeSeries.VariableType := vtCumulative;
        ATimeSeries.TimeStep := tstYearly;
        ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
        ATimeSeries.TimeStepStrict := True;
        TimeseriesIntegrate(TimeSeriesGrid.Data[NewTsIndex],ATimeSeries,1,
          tstYearly,1000000.000,False);
        TimeSeriesGrid.Add(ATimeSeries);
      end;
{Calculate Average Monthly and Annual Discharge on demand}
      if rgrpStageDischarge.ItemIndex = 2 then
      begin
{  Monthly...}
        ATimeSeries := TTimeSeries.Create;
        ATimeSeries.MUnit := 'm^3/s';
        ATimeSeries.Title := 'Average Discharge';
        ATimeSeries.VariableType := vtInstantaneous;
        ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
        ATimeSeries.TimeStep := tstMonthly;
        ATimeSeries.TimeStepStrict := True;
        TimeseriesIntegrate(TimeSeriesGrid.Data[NewTsIndex],ATimeSeries,1,
          tstMonthly,1,True);
        TimeSeriesGrid.Add(ATimeSeries);
{  Yearly...}
        ATimeSeries := TTimeSeries.Create;
        ATimeSeries.MUnit := 'm^3/s';
        ATimeSeries.Title := 'Average Discharge';
        ATimeSeries.VariableType := vtInstantaneous;
        ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
        ATimeSeries.TimeStep := tstYearly;
        ATimeSeries.TimeStepStrict := True;
        TimeseriesIntegrate(TimeSeriesGrid.Data[NewTsIndex],ATimeSeries,1,
          tstYearly,1,True);
        TimeSeriesGrid.Add(ATimeSeries);
      end;
    end else if FCalculationType = 1 then begin
{Area Calculation}
      ATimeSeries := TTimeSeries.Create;
      ATimeSeries.MUnit := 'km^2';
      ATimeSeries.Title := 'Area';
      ATimeSeries.VariableType := vtInstantaneous;
      ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
      ATimeSeries.TimeStep := ATimeStep;
      ATimeSeries.TimeStepStrict := ATimeStepStrict;
      TsInterpolate(TimeSeriesGrid.Data[NewTSIndex],ATimeSeries, FAreaCurve);
      for i := 0 to ATimeSeries.Count-1 do
        ATimeSeries.Items[i].MStatus := msNew;
      H2TSIndex := TimeSeriesGrid.Add(ATimeSeries);
{Volume Calculation}
      ATimeSeries := TTimeSeries.Create;
      ATimeSeries.MUnit := 'hm^3';
      ATimeSeries.Title := 'Volume';
      ATimeSeries.VariableType := vtInstantaneous;
      ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
      ATimeSeries.TimeStep := ATimeStep;
      ATimeSeries.TimeStepStrict := ATimeStepStrict;
      TsInterpolate(TimeSeriesGrid.Data[NewTSIndex],ATimeSeries, FVolumeCurve);
      for i := 0 to ATimeSeries.Count-1 do
        ATimeSeries.Items[i].MStatus := msNew;
      NewTsIndex := TimeSeriesGrid.Add(ATimeSeries);
{Calculate Average Monthly Area and Volume on demand}
      if rgrpVolume.ItemIndex = 1 then
      begin
        ATimeSeries := TTimeSeries.Create;
        ATimeSeries.MUnit := 'km^2';
        ATimeSeries.Title := 'Average Area';
        ATimeSeries.VariableType := vtInstantaneous;
        ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
        ATimeSeries.TimeStep := tstMonthly;

        ATimeSeries.TimeStepStrict := True;
        TimeseriesIntegrate(TimeSeriesGrid.Data[H2TSIndex],ATimeSeries,1,
          tstMonthly,1,True);
        TimeSeriesGrid.Add(ATimeSeries);
        ATimeSeries := TTimeSeries.Create;
        ATimeSeries.MUnit := 'hm^3';
        ATimeSeries.Title := 'Average Volume';
        ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
        ATimeSeries.VariableType := vtInstantaneous;
        ATimeSeries.TimeStep := tstMonthly;
        ATimeSeries.TimeStepStrict := True;
        TimeseriesIntegrate(TimeSeriesGrid.Data[NewTsIndex],ATimeSeries,1,
          tstMonthly,1,True);
        TimeSeriesGrid.Add(ATimeSeries);
      end;
    end else if FCalculationType = 2 then begin
{Leakage Calculation}
      ATimeSeries := TTimeSeries.Create;
      ATimeSeries.MUnit := 'hm^3/month';
      ATimeSeries.Title := 'Leakage';
      ATimeSeries.VariableType := vtInstantaneous;
      ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
      ATimeSeries.TimeStep := ATimeStep;
      ATimeSeries.TimeStepStrict := ATimeStepStrict;
      FLeakage.InterpolateTS(TimeSeriesGrid.Data[NewTSIndex],ATimeSeries);
      for i := 0 to ATimeSeries.Count-1 do
        ATimeSeries.Items[i].MStatus := msNew;
      NewTsIndex := TimeSeriesGrid.Add(ATimeSeries);
{Calculate Average Monthly Volume on demand}
      if rgrpLeakage.ItemIndex = 1 then
      begin
        ATimeSeries := TTimeSeries.Create;
        ATimeSeries.MUnit := 'hm^3/month';
        ATimeSeries.Title := 'Average Leakage';
        ATimeSeries.VariableType := vtInstantaneous;
        ATimeSeries.TimeStep := tstMonthly;
        ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
        ATimeSeries.TimeStepStrict := True;
        TimeseriesIntegrate(TimeSeriesGrid.Data[NewTsIndex],ATimeSeries,1,
          tstMonthly,1,True);
        TimeSeriesGrid.Add(ATimeSeries);
      end;
    end else if FCalculationType = 3 then begin
{Spillway Stage-Discharge Calculation}
      ATimeSeries := TTimeSeries.Create;
      ATimeSeries.MUnit := 'm^3/s';
      ATimeSeries.Title := 'Spillway Discharge';
      ATimeSeries.VariableType := vtInstantaneous;
      ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
      ATimeSeries.TimeStep := ATimeStep;
      ATimeSeries.TimeStepStrict := ATimeStepStrict;
      TsInterpolate(TimeSeriesGrid.Data[NewTSIndex],ATimeSeries, FSpillway);
      for i := 0 to ATimeSeries.Count-1 do
        ATimeSeries.Items[i].MStatus := msNew;
      NewTsIndex := TimeSeriesGrid.Add(ATimeSeries);
{Calculate cumulative Monthly and Annual Spill. Discharge on demand}
      if rgrpSpillway.ItemIndex = 1 then
      begin
{  Monthly...}
        ATimeSeries := TTimeSeries.Create;
        ATimeSeries.MUnit := 'hm^3/month';
        ATimeSeries.Title := 'Cumulative Spill. Discharge';
        ATimeSeries.VariableType := vtCumulative;
        ATimeSeries.TimeStep := tstMonthly;
        ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
        ATimeSeries.TimeStepStrict := True;
        TimeseriesIntegrate(TimeSeriesGrid.Data[NewTsIndex],ATimeSeries,1,
          tstMonthly,1000000.000,False);
        TimeSeriesGrid.Add(ATimeSeries);
{  Yearly...}
        ATimeSeries := TTimeSeries.Create;
        ATimeSeries.MUnit := 'hm^3/year';
        ATimeSeries.Title := 'Cumulative Spill. Discharge';
        ATimeSeries.VariableType := vtCumulative;
        ATimeSeries.TimeStep := tstYearly;
        ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
        ATimeSeries.TimeStepStrict := True;
        TimeseriesIntegrate(TimeSeriesGrid.Data[NewTsIndex],ATimeSeries,1,
          tstYearly,1000000.000,False);
        TimeSeriesGrid.Add(ATimeSeries);
      end;
    end else if FCalculationType = 4 then begin
{Spillway Stage-Discharge Calculation}
      ATimeSeries := TTimeSeries.Create;
      ATimeSeries.MUnit := 'kg/s';
      ATimeSeries.Title := 'Sediment discharge';
      ATimeSeries.VariableType := vtInstantaneous;
      ATimeSeries.TimeStep := ATimeStep;
      ATimeseries.TimeZone := TimeSeriesGrid.Data[FTSGridReq[0]].TimeZone;
      ATimeSeries.TimeStepStrict := ATimeStepStrict;
      TsInterpolate(TimeSeriesGrid.Data[NewTSIndex], ATimeSeries,
        FTransientCurveList);
      for i := 0 to ATimeSeries.Count-1 do
        ATimeSeries.Items[i].MStatus := msNew;
    end;
  end;
end;

procedure TFrmStageDischarge.btnCalculateClick(Sender: TObject);
begin
  DoCalculate;
  ModalResult := mrOK;
end;

resourcestring
  rsLoadfrom = 'Load from';
  rsLoadfromDB = 'Load from DB with id:';

procedure TFrmStageDischarge.btnLoadFromDatabaseClick(Sender: TObject);
{var
  AID, i: Integer;
  ACurvePoint: TCurvePoint;
  ASavedCursor: TCursor;}
begin
  Assert(False, 'not implemented.. under destruction');
{  AID := GetCurveID(False);
  if AID = -1 then Exit;
  ASavedCursor := Screen.Cursor;
  try
    Screen.Cursor := crSQLWait;
    if FCalculationType = 0 then
    begin
      LoadSDFromDatabase(FTransientCurveList, AID, DBSession);
      if FTransientCurveList.Count > 0 then
       begin
         FCurvesLoaded := True;
         RedrawChart;
       end;
    end else if FCalculationType = 1 then
    begin
      LoadInterpolFromDataBase(FVolumeCurve, AID, DBSession,
        'reservoir_hsvb','h','v');
      LoadInterpolFromDatabase(FAreaCurve, AID, DBSession,
        'reservoir_hsvb','h','s');
      if FVolumeCurve.Count > 0 then
      begin
        for i := 0 to FVolumeCurve.Count-1 do
        begin
          if FVolumeCurve.Points[i].Dependent <= 0 then
          begin
            ACurvePoint.Dependent := 0.001;
            ACurvePoint.Independent := FVolumeCurve.Points[i].Independent;
            FVolumeCurve.Points[i] := ACurvePoint;
          end;
        end;
        for i := 0 to FAreaCurve.Count-1 do
        begin
          if FAreaCurve.Points[i].Dependent <= 0 then
          begin
            ACurvePoint.Dependent := 0.001;
            ACurvePoint.Independent := FAreaCurve.Points[i].Independent;
            FAreaCurve.Points[i] := ACurvePoint;
          end;
        end;
        FCurvesLoaded := True;
        RedrawChart;
      end;
    end else if FCalculationType =2 then
    begin
      LoadLeakageFromDatabase(Fleakage, AID, DBSession);
      FCurvesLoaded := True;
      RedrawChart;
    end else if FCalculationType =3 then
    begin
      LoadInterpolFromDataBase(FSpillway, AID, DBSession,
        'reservoir_spill','h','q');
      if FSpillway.Count > 0 then
      begin
        for i := 0 to FSpillway.Count-1 do
        begin
          if FSpillway.Points[i].Dependent <= 0 then
          begin
            ACurvePoint.Dependent := 0.001;
            ACurvePoint.Independent := FSpillway.Points[i].Independent;
            FSpillway.Points[i] := ACurvePoint;
          end;
        end;
        FCurvesLoaded := True;
        RedrawChart;
      end;
    end;
  finally
    Screen.Cursor := ASavedCursor;
  end;
  SetControlStatus;}
end;

resourcestring
  rsInputDelimiterSymbol = 'Input delimiter symbol';
  rsInputDecimalSeparator = 'Input decimal separator';

procedure TFrmStageDischarge.btnLoadClick(Sender: TObject);
var
  ACurvePoint: TCurvePoint;
  s: string;
  ADelimiter, ADecimalSeparator, ASavedDecimalSeparator: Char;
  F: TextFile;
  i, SavedFilemode: Integer;
  AStage, AVolume, AArea: Real;
begin
  SavedFilemode := FileMode;
  try
  FileMode := fmOpenRead or fmShareDenyWrite;
  if OpenDialog.Execute then
  begin
{Stage - Discharge, Stage - Sed. Discharge}
    if (FCalculationType = 0) or ((FCalculationType = 4)) then
    begin
      FTransientCurveList.LoadFromFile(OpenDialog.FileName);
      if FTransientCurveList.Count > 0 then
      begin
        FCurvesLoaded := True;
        RedrawChart;
      end;
{Reservoir stage - area, volume}
    end else if FCalculationType = 1 then
    begin
      if not InputQuery(rsInputDelimiterSymbol,rsInputDelimiterSymbol,s) then
        Exit;
      if Length(s)=1 then
        ADelimiter := s[1]
      else Exit;
      s := SysUtils.FormatSettings.DecimalSeparator;
      if not InputQuery(rsInputDecimalSeparator,rsInputDecimalSeparator,s) then
        Exit;
      if Length(s)=1 then
        ADecimalSeparator := s[1]
      else Exit;
      AssignFile(F, OpenDialog.FileName);
      ASavedDecimalSeparator := SysUtils.FormatSettings.DecimalSeparator;
      try
        SysUtils.FormatSettings.DecimalSeparator := ADecimalSeparator;
        Reset(F);
        FVolumeCurve.Clear;
        FAreaCurve.Clear;
        while not eof(F) do
        begin
          ReadLn(F,s);
          if s='' then Continue;
          AStage := StrToFloat(DelimitedStringItem(s, 1, ADelimiter));
          AArea := StrToFloat(DelimitedStringItem(s, 2, ADelimiter));
          AVolume := StrToFloat(DelimitedStringItem(s, 3, ADelimiter));
          if AArea <=0 then AArea := 0.001;
          if AVolume <=0 then AVolume := 0.001;
          ACurvePoint.Independent := AStage;
          ACurvePoint.Dependent := AArea;
          FAreaCurve.Add(ACurvePoint);
          ACurvePoint.Independent := AStage;
          ACurvePoint.Dependent := AVolume;
          FVolumeCurve.Add(ACurvePoint);
        end;
      finally
        SysUtils.FormatSettings.DecimalSeparator := ASavedDecimalSeparator;
        CloseFile(F);
        if FVolumeCurve.Count > 0 then
        begin
          FCurvesLoaded := True;
          RedrawChart;
        end;
      end;
{Spillway}
    end else if FCalculationType =3 then
    begin
      if not InputQuery(rsInputDelimiterSymbol,rsInputDelimiterSymbol,s) then
        Exit;
      if Length(s)=1 then
        ADelimiter := s[1]
      else Exit;
      s := SysUtils.FormatSettings.DecimalSeparator;
      if not InputQuery(rsInputDecimalSeparator,rsInputDecimalSeparator,s) then
        Exit;
      if Length(s)=1 then
        ADecimalSeparator := s[1]
      else Exit;
      AssignFile(F, OpenDialog.FileName);
      ASavedDecimalSeparator := SysUtils.FormatSettings.DecimalSeparator;
      try
        SysUtils.FormatSettings.DecimalSeparator := ADecimalSeparator;
        Reset(F);
        FSpillway.Clear;
        while not eof(F) do
        begin
          ReadLn(F,s);
          if s='' then Continue;
          AStage := StrToFloat(DelimitedStringItem(s, 1, ADelimiter));
          AArea := StrToFloat(DelimitedStringItem(s, 2, ADelimiter));
          if AArea <=0 then AArea := 0.001;
          ACurvePoint.Independent := AStage;
          ACurvePoint.Dependent := AArea;
          FSpillway.Add(ACurvePoint);
        end;
      finally
        SysUtils.FormatSettings.DecimalSeparator := ASavedDecimalSeparator;
        CloseFile(F);
        if FSpillway.Count > 0 then
        begin
          FCurvesLoaded := True;
          RedrawChart;
        end;
      end;
{Leakage}
    end else if FCalculationType =2 then
    begin
      if not InputQuery(rsInputDelimiterSymbol,rsInputDelimiterSymbol,s) then
        Exit;
      if Length(s)=1 then
        ADelimiter := s[1]
      else Exit;
      s := SysUtils.FormatSettings.DecimalSeparator;
      if not InputQuery(rsInputDecimalSeparator,rsInputDecimalSeparator,s) then
        Exit;
      if Length(s)=1 then
        ADecimalSeparator := s[1]
      else Exit;
      AssignFile(F, OpenDialog.FileName);
      ASavedDecimalSeparator := SysUtils.FormatSettings.DecimalSeparator;
      try
        SysUtils.FormatSettings.DecimalSeparator := ADecimalSeparator;
        Reset(F);
        ReadLn(F,s);
        FLeakage.FHmin := StrToFloat(s);
        ReadLn(F,s);
        FLeakage.FHmax := StrToFloat(s);
        for i := 1 to 12 do
        begin
          ReadLn(F,s);
          if s='' then Continue;
          FLeakage.FACoef[i] := StrToFloat(DelimitedStringItem(s, 1, ADelimiter));
          FLeakage.FBCoef[i] := StrToFloat(DelimitedStringItem(s, 2, ADelimiter));
          FLeakage.FCCoef[i] := StrToFloat(DelimitedStringItem(s, 3, ADelimiter));
          FLeakage.FECoef[i] := StrToFloat(DelimitedStringItem(s, 4, ADelimiter));
        end;
      finally
        SysUtils.FormatSettings.DecimalSeparator := ASavedDecimalSeparator;
        CloseFile(F);
        FCurvesLoaded := True;
        RedrawChart;
      end;
    end;
  end;
  finally
    FileMode := SavedFilemode;
  end;
  SetControlStatus;
end;

resourcestring
  rsWriteto = 'Write to';
  rsWritetoDB = 'Write to DB with id:';

procedure TFrmStageDischarge.btnSaveToDatabaseClick(Sender: TObject);
{var
  AID: Integer;
  ASavedCursor: TCursor;}
begin
  Assert(False, 'Not implemented... under destruction');
{  AID := GetCurveID(True);
  if AID = -1 then Exit;
  ASavedCursor := Screen.Cursor;
  try
    Screen.Cursor := crSQLWait;
    if FCalculationType=0 then
    begin
      if FTransientCurveList.Count > 0 then
        WriteSDToDatabase(FTransientCurveList, AID, DBSession);
    end else if FCalculationType =1 then
    begin
      if FAreaCurve.Count > 0 then
        WriteReservoirHSVBToDB(FAreaCurve, FVolumeCurve, AID, DBSession);
    end;
  finally
    Screen.Cursor := ASavedCursor;
  end;}
end;

procedure TFrmStageDischarge.btnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    if FTransientCurveList.Count > 0 then
      FTransientCurveList.WriteToFile(SaveDialog.FileName,'yyyy-mm-dd hh:nn',
      ',','.');
  end;
end;

resourcestring
  rsUnloadingCurves = 'Changing calculation type results to curve unloading. '+
      'Continue Operation?';

procedure TFrmStageDischarge.rgrpTypeOfCalculationsClick(Sender: TObject);
begin
  if FCalculationType <> rgrpTypeOfCalculations.ItemIndex then
  begin
    if MessageDlg(rsUnloadingCurves, mtWarning, [mbYes, mbNo],0) = mrNo then
    begin
      rgrpTypeOfCalculations.ItemIndex := FCalculationType;
      Exit;
    end;
    FCalculationType := rgrpTypeOfCalculations.ItemIndex;
    Chart1.RemoveAllSeries;
    SetLength(FSeries,0);
    SetLength(FPSeries,0);
    Chart1.LeftAxis.Maximum := Abs(Chart1.LeftAxis.Minimum)+1;
    Chart1.LeftAxis.Minimum := Abs(Chart1.LeftAxis.Minimum)+0.001;
    Chart1.BottomAxis.Maximum := Abs(Chart1.BottomAxis.Minimum)+1;
    Chart1.BottomAxis.Minimum := Abs(Chart1.BottomAxis.Minimum)+0.001;
    FColorIndex := 0;
    FSeriesCount := 0;
    FTransientCurveList.Clear;
    FVolumeCurve.Clear;
    FAreaCurve.Clear;
    FCurvesLoaded := False;
  end;
  SetControlStatus;
end;

procedure TFrmStageDischarge.btnEditClick(Sender: TObject);
var
  AForm: TFrmSDConstruction;
begin
  AForm := nil;
  try
    AForm := TFrmSDConstruction.Create(Self);
    if (FTSGridReq[2]>-1) and (FTSGridReq[3]>-1) then
      FTransientCurveList.SetHydrometricPoints(
        TimeSeriesGrid.Data[FTSGridReq[2]], TimeSeriesGrid.Data[FTSGridReq[3]]);
    AForm.TimeseriesGrid := TimeseriesGrid;
    AForm.CurveList := FTransientCurveList;
    if FCalculationType = 4 then
      AForm.Mode := icemDischargeSedimentDischarge else
      AForm.Mode := icemStageDischarge;
    AForm.ShowModal;
    AForm.ReturnCurveList(FTransientCurveList);
    FCurvesLoaded := True;
    RedrawChart;
  finally
    AForm.Release;
  end;
  SetControlStatus;
end;

end.


