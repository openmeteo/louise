{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-09 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit frmtsarea;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, ts, tsarea, Grids, StdCtrls, ExtCtrls, VclTee.Series,
  VclTee.TeEngine, VclTee.TeeProcs, VclTee.Chart, Menus, uiutils,
  VclTee.TeeGDIPlus;

type
  TFrmArealIntegration = class(TForm)
    grdParameters: TStringGrid;
    btnCalculate: TButton;
    GroupBox1: TGroupBox;
    btnCalcStationMean: TButton;
    edtMeanX: TLabeledEdit;
    edtMeanY: TLabeledEdit;
    btnCalcPosWeights: TButton;
    GroupBox2: TGroupBox;
    edtMeanAltitude: TLabeledEdit;
    btnCalcGradients: TButton;
    edtMeanStationAltitude: TLabeledEdit;
    edtGradient: TLabeledEdit;
    edtAltitudeFactor: TLabeledEdit;
    edtCorrelation: TLabeledEdit;
    edtConstantTerm: TLabeledEdit;
    chkAltitudeReduce: TCheckBox;
    Chart: TChart;
    seriesMeasures: TPointSeries;
    seriesSlope: TLineSeries;
    seriesMeanBasinAltitude: TLineSeries;
    seriesMeanStationAltitude: TLineSeries;
    btnCopyChart: TButton;
    PopupMenu: TPopupMenu;
    Copygridtoclipboard1: TMenuItem;
    Pastetogrid1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCalcGradientsClick(Sender: TObject);
    procedure btnCalculateClick(Sender: TObject);
    procedure btnCalcStationMeanClick(Sender: TObject);
    procedure btnCalcPosWeightsClick(Sender: TObject);
    procedure btnCopyChartClick(Sender: TObject);
    procedure Copygridtoclipboard1Click(Sender: TObject);
    procedure Pastetogrid1Click(Sender: TObject);
  private
    FMeasures: TArrayOfPointMeanRains;
    FMeanAltitude, FMeanStationAltitude: Real;
    FGradient, FReductionFactor: Real;
    FConstantTerm, FCorrelation: Real;
    procedure ReadGrid;
    procedure WriteGrid;
    procedure RedrawChart;
  public
    TimeseriesList: TObjectList;
    Dest: TTimeseries;
  end;

  const
    rowTitle = 0;
    rowWeight = 1;
    rowAltitude = 2;
    rowCoordX = 3;
    rowCoordY = 4;
    rowMeanValue = 5;

    rowtitles: array[0..5] of string = ('Title', 'Weight', 'Altitude (m)',
      'X coordinate (m)', 'Y coordinate (m)', 'Mean value (mm)');

    GridRowCount = 6;

implementation

{$R *.dfm}

procedure TFrmArealIntegration.btnCalcGradientsClick(Sender: TObject);
begin
  ReadGrid;
  FMeanStationAltitude := CalcStationsMeanAltitude(FMeasures);
  FMeanAltitude := StrToFloat(edtMeanAltitude.Text);
  FGradient := RainfallGradient(TimeseriesList, FMeanAltitude, FMeasures,
    FConstantTerm, FCorrelation, FReductionFactor);
  edtGradient.Text := FormatFloat('0.0000', FGradient);
  edtMeanStationAltitude.Text := FormatFloat('0.000', FMeanStationAltitude);
  edtCorrelation.Text := FormatFloat('0.000', FCorrelation);
  edtConstantTerm.Text := FormatFloat('0.000', FConstantTerm);
  edtAltitudeFactor.Text := FormatFloat('0.000', FReductionFactor);
  WriteGrid;
  RedrawChart;
end;

procedure TFrmArealIntegration.btnCalcPosWeightsClick(Sender: TObject);
begin
  ReadGrid;
  CalcWeightsFromCoords(StrToFloat(edtMeanX.Text), StrToFloat(edtMeanY.Text),
    2, FMeasures);
  WriteGrid;
end;

procedure TFrmArealIntegration.btnCalcStationMeanClick(Sender: TObject);
var
  AX, AY: Real;
begin
  ReadGrid;
  FindStationCenter(FMeasures, AX, AY);
  edtMeanX.Text := FormatFloat('0.000', AX);
  edtMeanY.Text := FormatFloat('0.000', AY);
end;

procedure TFrmArealIntegration.btnCalculateClick(Sender: TObject);
begin
  ReadGrid;
  CalcArealRainfall(TimeseriesList, Dest, FMeasures,
    StrToFloat(edtAltitudeFactor.Text), chkAltitudeReduce.Checked);
  ModalResult := mrOk;
end;

procedure TFrmArealIntegration.btnCopyChartClick(Sender: TObject);
begin
  Chart.CopyToClipboardMetafile(True);
end;

procedure TFrmArealIntegration.Copygridtoclipboard1Click(Sender: TObject);
begin
  StringGridToClipboard(grdParameters, 1, grdParameters.ColCount-1,
    1, grdParameters.RowCount-1)
end;

procedure TFrmArealIntegration.FormDestroy(Sender: TObject);
begin
  FMeasures := nil;
end;

procedure TFrmArealIntegration.FormShow(Sender: TObject);
var
  i: Integer;
begin
  grdParameters.DefaultRowHeight := grdParameters.DefaultRowHeight *
    Screen.PixelsPerInch div 96;
  Assert(TimeseriesList<>nil);
  Assert(Dest<>nil);
  grdParameters.ColCount := 1 + TimeseriesList.Count;
  grdParameters.RowCount := GridRowCount;
  for i := 0 to TimeseriesList.Count-1 do
  begin
    grdParameters.Cells[i+1, 0] := TTimeseries(TimeseriesList[i]).Title;
    grdParameters.Cells[i+1, rowAltitude] := '0';
    grdParameters.Cells[i+1, rowMeanValue] := '';
    grdParameters.Cells[i+1, rowCoordX] := '0';
    grdParameters.Cells[i+1, rowCoordY] := '0';
    grdParameters.Cells[i+1, rowWeight] :=
      FormatFloat('0.000', 1/TimeseriesList.Count);
  end;
  for i := 0 to grdParameters.RowCount-1 do
    grdParameters.Cells[0, i] := rowtitles[i];
  SetLength(FMeasures, TimeseriesList.Count);
end;

procedure TFrmArealIntegration.Pastetogrid1Click(Sender: TObject);
begin
  ClipboardToStringGrid(grdParameters, 1, 1, True);
end;

procedure TFrmArealIntegration.WriteGrid;
var
  i: Integer;
begin
  for i := 0 to TimeseriesList.Count-1 do
  begin
    grdParameters.Cells[i+1, rowMeanValue] :=
      FormatFloat('0.000', FMeasures[i].Value);
    grdParameters.Cells[i+1, rowWeight] :=
      FormatFloat('0.000', FMeasures[i].Weight);
  end;
end;

procedure TFrmArealIntegration.ReadGrid;
var
  i: Integer;
begin
  for i := 0 to TimeseriesList.Count-1 do
    with FMeasures[i] do
    begin
      Altitude := StrToFloat(grdParameters.Cells[i+1, rowAltitude]);
      Weight := StrToFloat(grdParameters.Cells[i+1, rowWeight]);
      CoordX := StrToFloat(grdParameters.Cells[i+1, rowCoordX]);
      CoordY := StrToFloat(grdParameters.Cells[i+1, rowCoordY]);
    end;
end;

procedure TFrmArealIntegration.RedrawChart;
var
  i: Integer;
  MinX, MaxX, a, b: Real;
begin
  for i := 0 to Chart.SeriesCount-1 do
    Chart.Series[i].Clear;
  for i := 0 to Length(FMeasures)-1 do
    with FMeasures[i] do
      seriesMeasures.AddXY(Value, Altitude, '', clDefault);
  MinX := 0.9*seriesMeasures.MinXValue;
  MaxX := 1.1*seriesMeasures.MaxXValue;
  a := FConstantTerm;
  b := FGradient;
  seriesSlope.AddXY(MinX, (MinX-a)/b, '', clDefault);
  seriesSlope.AddXY(MaxX, (MaxX-a)/b, '', clDefault);
  seriesMeanBasinAltitude.AddXY(MinX, FMeanAltitude, '', clDefault);
  seriesMeanBasinAltitude.AddXY(MaxX, FMeanAltitude, '', clDefault);
  seriesMeanStationAltitude.AddXY(MinX, FMeanStationAltitude, '', clDefault);
  seriesMeanStationAltitude.AddXY(MaxX, FMeanStationAltitude, '', clDefault);
end;

end.
