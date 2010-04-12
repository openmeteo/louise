{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit rgrrsltdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, Menus, contnrs, ts, frmdevchart;

type
  TFrmRegressionResults = class(TForm)
    SgrdResults: TStringGrid;
    Panel1: TPanel;
    ChkHydrologicalYear: TCheckBox;
    PMnu: TPopupMenu;
    MnuCopy: TMenuItem;
    btnShowDeviationChart: TButton;
    procedure ChkHydrologicalYearClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    procedure btnShowDeviationChartClick(Sender: TObject);
  private
    FHYearOrigin: Integer;
  public
    RegressionResults: TObject;
    DependentTimeseries: TTimeseries;
    IndependentTimeseries: TObjectList;
    procedure UpdateGrid;
    property HYearOrigin: Integer read FHYearOrigin write FHYearOrigin;
  end;

implementation

{$R *.DFM}

uses TsProcess, uiutils, Dates;

procedure TFrmRegressionResults.btnShowDeviationChartClick(Sender: TObject);
var
  AFrmDevChart: TFrmDeviationChart;
  ARegressionResults: TRegressionResults;
  ARegressionResultsList: TObjectList;
  ATimeseriesList: TObjectList;
  i, j, k, AMonth: Integer;
  ACommonPeriod: TDateTimeList;
  AValue: Real;
begin
  i := SgrdResults.Selection.Top-1;
  if (i<0) or (i>11) then Exit;
  if RegressionResults is TObjectList then
  begin
    if not ChkHydrologicalYear.Checked then
      AMonth := i+1 else
      begin
        if i in [0..(12-FHYearOrigin)] then
          AMonth := FHYearOrigin + i else
          AMonth := i - (12-FHYearOrigin);
      end;
    ARegressionResultsList := TObjectList(RegressionResults);
    ARegressionResults := TRegressionResults(ARegressionResultsList[AMonth-1]);
  end else
  begin
    ARegressionResults := TRegressionResults(RegressionResults);
    Amonth := 0;
  end;
  AFrmDevChart := nil;
  ATimeseriesList := nil;
  ACommonPeriod := nil;
  try
    ATimeseriesList := TObjectList.Create(False);
    AFrmDevChart := TFrmDeviationChart.Create(Self);
    AFrmDevChart.Data.RegressionResults := ARegressionResults;
    AFrmDevChart.Data.ChartPoints := nil;
    ATimeseriesList.Add(DependentTimeseries);
    for i := 0 to IndependentTimeseries.Count-1 do
      ATimeseriesList.Add(IndependentTimeseries.Items[i]);
    ACommonPeriod := GetCommonPeriod(ATimeseriesList, AMonth);
    SetLength(AFrmDevChart.Data.ChartPoints, ACommonPeriod.Count);
    for i := 0 to ACommonPeriod.Count-1 do
    begin
      AValue := 0;
      k := 0;
      for j := 0 to IndependentTimeseries.Count-1 do
      begin
        if ARegressionResults[j+2]<> 0 then
        begin
          Inc(k);
          with TTimeseries(IndependentTimeseries[j]) do
            AValue := AValue + Items[IndexOf(ACommonPeriod[i])].AsFloat;
        end;
      end;
      if k>0 then AValue := AValue/k;
      with AFrmDevChart.Data.ChartPoints[i] do
      begin
        x := AValue;
        with DependentTimeseries do
        y := Items[IndexOf(ACommonPeriod[i])].AsFloat;
        Date := ACommonPeriod[i];
      end;
    end;
    AFrmDevChart.ShowModal;
  finally
    ACommonPeriod.Free;
    ATimeseriesList.Free;
    AFrmDevChart.Free;
  end;
end;

procedure TFrmRegressionResults.ChkHydrologicalYearClick(Sender: TObject);
begin
  UpdateGrid;
end;

procedure TFrmRegressionResults.FormShow(Sender: TObject);
begin
  SgrdResults.DefaultRowHeight := SgrdResults.DefaultRowHeight *
    Screen.PixelsPerInch div 96;
  ChkHydrologicalYear.Enabled := RegressionResults is TObjectList;
  UpdateGrid;
end;

resourcestring
  rsCommonPeriodCardinality = 'N';
  rsCoefficientOfDetermination = 'D=R^2';
  rsCriticalCoefficientOfDetermination ='Dcr.';
  rsCoefficientOfCorrelation = 'R';

procedure TFrmRegressionResults.UpdateGrid;
var
  ARegressionResults: TRegressionResults;
  ARegressionResultsList: TObjectList;
  i: Integer;

  procedure WriteRowContents;
  var j: Integer;
  begin
    SgrdResults.Cells[1,i] := IntToStr(ARegressionResults.CommonPeriodCardinality);
    SgrdResults.Cells[2,i] := Format('%.3f', [ARegressionResults.CoefficientOfDetermination]);
    SgrdResults.Cells[3,i] := Format('%.3f', [ARegressionResults.CoefficientOfCorrelation]);
    SgrdResults.Cells[4,i] := Format('%.3f', [ARegressionResults.CriticalCoefficientOfDetermination]);
    for j := 1 to ARegressionResults.Size do
      if ARegressionResults[j]<>0 then
        SgrdResults.Cells[j+4,i] := Format('%.3f', [ARegressionResults[j]])
      else
        SgrdResults.Cells[j+4,i] := '';
  end;

begin
  ARegressionResultsList := nil;

  { Determine grid row and column count. }
  if RegressionResults is TObjectList then
  begin
    ARegressionResultsList := TObjectList(RegressionResults);
    ARegressionResults := TRegressionResults(ARegressionResultsList[0]);
    SgrdResults.RowCount := 13;
    SgrdResults.ColCount := 5+ARegressionResults.Size;
  end else
  begin
    ARegressionResults := TRegressionResults(RegressionResults);
    SgrdResults.RowCount := 2;
    SgrdResults.ColCount := 5+ARegressionResults.Size;
  end;

  { Write column headings. }
  SgrdResults.Cells[0,0] := '';
  SgrdResults.Cells[1,0] := rsCommonPeriodCardinality;
  SgrdResults.Cells[2,0] := rsCoefficientOfDetermination;
  SgrdResults.Cells[3,0] := rsCoefficientOfCorrelation;
  SgrdResults.Cells[4,0] := rsCriticalCoefficientOfDetermination;
  for i := 0 to ARegressionResults.Size-1 do
    SgrdResults.Cells[i+5,0] := 'a'+IntToStr(i);

  { Write row headings. }
  if not (RegressionResults is TObjectList) then
    SgrdResults.Cells[0,1] := ''
  else
    for i := 1 to 12 do
      if not ChkHydrologicalYear.Checked then
        SgrdResults.Cells[0,i] := IntToStr(i)
      else
        if i<=(12-FHYearOrigin+1) then
          SgrdResults.Cells[0,i] := IntToStr(i+(FHYearOrigin-1))
        else
          SgrdResults.Cells[0,i] := IntToStr(i-(12-FHYearOrigin+1));

  { Write contents. }
  if not (RegressionResults is TObjectList) then
  begin
    i := 1;
    WriteRowContents;
  end else
    for i := 1 to 12 do
    begin
      if not ChkHydrologicalYear.Checked then
        ARegressionResults := TRegressionResults(ARegressionResultsList[i-1])
      else
        if i<=(12-FHYearOrigin+1) then
          ARegressionResults := TRegressionResults(ARegressionResultsList[i+
            (FHYearOrigin-1)-1])
        else
          ARegressionResults := TRegressionResults(ARegressionResultsList[i-
            (12-FHYearOrigin+1)-1]);
      WriteRowContents;
    end;
end;

procedure TFrmRegressionResults.MnuCopyClick(Sender: TObject);
begin
  with SgrdResults.Selection do
    StringGridToClipboard(SgrdResults, Left, Right, Top, Bottom);
end;

end.
