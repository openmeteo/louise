{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit idfcurvesdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, tsidf, statprocesses, VclTee.TeEngine, VclTee.Series, ExtCtrls,
  VclTee.TeeProcs, VclTee.Chart, ComCtrls, StdCtrls, Grids, StrGrdOd,
  frmprcsdlg, VclTee.TeeGDIPlus, Vcl.ImgList;

type
  TFrmIDFCurves = class(TForm)
    mnuMainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuPrintMulti: TMenuItem;
    mnuDistribution: TMenuItem;
    mnuExponential: TMenuItem;
    mnuLExponential: TMenuItem;
    mnuGamma: TMenuItem;
    mnuLogPearsonIII: TMenuItem;
    mnuGumbel: TMenuItem;
    mnuLGumbel: TMenuItem;
    mnuEV2Max: TMenuItem;
    mnuLEV2Max: TMenuItem;
    mnuGEVMax: TMenuItem;
    mnuLGEVMax: TMenuItem;
    mnuGEVMaxK: TMenuItem;
    mnuLGEVMaxK: TMenuItem;
    mnuPareto: TMenuItem;
    mnuLPareto: TMenuItem;
    N1: TMenuItem;
    mnuSpecifyKappa: TMenuItem;
    mnuEdit: TMenuItem;
    mnuCopyMulti: TMenuItem;
    mnuOptions: TMenuItem;
    mnuTimeResolution: TMenuItem;
    pgcTabs: TPageControl;
    tabMulti: TTabSheet;
    GroupBox1: TGroupBox;
    chkMulti1000: TCheckBox;
    chkMulti500: TCheckBox;
    chkMulti100: TCheckBox;
    chkMulti50: TCheckBox;
    chkMulti20: TCheckBox;
    chkMulti10: TCheckBox;
    chkMulti5: TCheckBox;
    chkMulti2: TCheckBox;
    chkMulti10000: TCheckBox;
    chkMulti100000: TCheckBox;
    chkMulti200: TCheckBox;
    chartMulti: TChart;
    seriesMulti100000: TLineSeries;
    seriesMulti10000: TLineSeries;
    seriesMulti1000: TLineSeries;
    seriesMulti500: TLineSeries;
    seriesMulti200: TLineSeries;
    seriesMulti100: TLineSeries;
    seriesMulti50: TLineSeries;
    seriesMulti20: TLineSeries;
    seriesMulti10: TLineSeries;
    seriesMulti5: TLineSeries;
    seriesMulti2: TLineSeries;
    tabSingle: TTabSheet;
    tabData: TTabSheet;
    tabDistribution: TTabSheet;
    Image: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtEtaValue: TEdit;
    Label5: TLabel;
    edtThetaValue: TEdit;
    Label6: TLabel;
    edtDistributionName: TEdit;
    GroupBox2: TGroupBox;
    edtStatParam1Name: TEdit;
    edtStatParam2Name: TEdit;
    edtStatParam3Name: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    edtStatParam1Value: TEdit;
    edtStatParam2Value: TEdit;
    edtStatParam3Value: TEdit;
    GroupBox3: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    edtMeanValue: TEdit;
    edtStandardDeviation: TEdit;
    edtSkewness: TEdit;
    edtLMoment1: TEdit;
    edtLMoment2: TEdit;
    edtLMoment3: TEdit;
    Chart: TChart;
    GridPoint: TPointSeries;
    WeibullPoints: TPointSeries;
    seriesDistribution: TLineSeries;
    lstDistributionLines: TListBox;
    mnuPrintDistribution: TMenuItem;
    mnuCopyDistribution: TMenuItem;
    mnuPaper: TMenuItem;
    mnuNormalPaper: TMenuItem;
    mnuGumbelPaper: TMenuItem;
    mnuGEVMaxPaper: TMenuItem;
    Label16: TLabel;
    GroupBox4: TGroupBox;
    Label17: TLabel;
    chartSingle: TChart;
    seriesSingleCentral: TLineSeries;
    Shape1: TShape;
    Label18: TLabel;
    Label19: TLabel;
    lblDenTheta: TLabel;
    labelFoufots: TLabel;
    lblDenEta: TLabel;
    edtReturnPeriod: TEdit;
    Label21: TLabel;
    btnCalculate: TButton;
    Label22: TLabel;
    edtSingleAlpha: TEdit;
    grpConfidence: TGroupBox;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    edtLowerSampleLimit: TEdit;
    edtUpperSampleLimit: TEdit;
    edtLowerConfidenceLimit: TEdit;
    edtUpperConfidenceLimit: TEdit;
    seriesLowerSample: TLineSeries;
    seriesUpperSample: TLineSeries;
    seriesLowerConfidence: TLineSeries;
    seriesUpperConfidence: TLineSeries;
    sgrdSingle: TOdStringGrid;
    mnuCopySingle: TMenuItem;
    mnuCopyGrid: TMenuItem;
    mnuPrintSingle: TMenuItem;
    btnConfidenceCalculate: TButton;
    rgrpConfidenceInterval: TRadioGroup;
    mnuSingleIDFEvaluation: TMenuItem;
    mnuExit: TMenuItem;
    mnuPrinterSetup: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    chkLogY: TCheckBox;
    mnuLogLogPaper: TMenuItem;
    Convertreturnperiodsforabovethresholdtimeseries1: TMenuItem;
    ImglstIdfEquations: TImageList;
    procedure IFormCreate(Sender: TObject);
    procedure IFormDestroy(Sender: TObject);
    procedure IFormShow(Sender: TObject);
    procedure mnuPrintMultiClick(Sender: TObject);
    procedure chkMulti1000Click(Sender: TObject);
    procedure mnuGammaClick(Sender: TObject);
    procedure mnuSpecifyKappaClick(Sender: TObject);
    procedure mnuCopyMultiClick(Sender: TObject);
    procedure mnuTimeResolutionClick(Sender: TObject);
    procedure IFormHide(Sender: TObject);
    procedure lstDistributionLinesClick(Sender: TObject);
    procedure mnuPrintDistributionClick(Sender: TObject);
    procedure mnuCopyDistributionClick(Sender: TObject);
    procedure mnuNormalPaperClick(Sender: TObject);
    procedure btnCalculateClick(Sender: TObject);
    procedure edtReturnPeriodChange(Sender: TObject);
    procedure mnuPrintSingleClick(Sender: TObject);
    procedure mnuCopySingleClick(Sender: TObject);
    procedure mnuCopyGridClick(Sender: TObject);
    procedure btnConfidenceCalculateClick(Sender: TObject);
    procedure rgrpConfidenceIntervalClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuPrinterSetupClick(Sender: TObject);
    procedure mnuSingleIDFEvaluationClick(Sender: TObject);
    procedure Convertreturnperiodsforabovethresholdtimeseries1Click(
      Sender: TObject);
  private
    FExplicitEtaTheta: Boolean;
    FProbabilityList: array[0..22] of Real;
    FPaperMinZ: array[TProbabilityPaperType] of Real;
    FPaperMaxZ: array[TProbabilityPaperType] of Real;
    FPaperType: TProbabilityPaperType;
    FIDFTimeseriesCollection: TIDFTimeseriesCollection;
    FDataList: TDataList;
    FGEVParameter: Real;
    FEta: Real;
    FTheta: Real;
    FSingleIDFReturnPeriod: Real;
    FConsiderTimeResolution: Boolean;
    FStatisticalDistributionType: TStatisticalDistributionType;
    FWeibullSeries: array of TPointSeries;
    FDistributionSeries: array of TLineSeries;
    FAboveThreshold: Boolean;
    FrmProcessingDialog: TFrmProcessingDialog;
    procedure SetControlStatus;
    procedure Refresh;
    procedure DrawEquation;
    procedure DrawMulti;
    procedure DrawData;
    procedure DrawDistribution;
    procedure DrawDistributionLines;
    procedure DrawDistributionPoints;
    procedure DrawGrid;
    procedure SetDistributionChartBounds;
    function GetColorSequence(Index: Integer): TColor;
    function GetMarkSequence(Index: Integer): TSeriesPointerStyle;
    procedure ResetSinglePage;
    procedure DrawSingle;
    procedure DrawSingleMC;
    procedure WriteGrid;
    function ConvertReturnPeriod(AReturnPeriod: Real): Real;
    procedure MCLimitsProgress(var Stop: Boolean);
  public
    property IDFTimeseriesCollection: TIDFTimeseriesCollection read
      FIDFTimeseriesCollection write FIDFTimeseriesCollection;
    property ExplicitEtaTheta: Boolean write FExplicitEtaTheta default False;
    property ExplicitEta: Real write FEta;
    property ExplicitTheta: Real write FTheta;
  end;

implementation

uses
  uiutils, montecarlo;

{$R *.DFM}
(*
  At this point the file originally contained the directive
  {$RESOURCE idfeqs.res}
  However, in Delphi XE5 we do away with .res files from the repository; they
  are now purely "binary" files. At this stage it is not clear what idfeqs.res
  contained and what was the purpose of the above directive (and Delphi has
  discontinued the resource editor since version 2009). Until we investigate
  further, we remove the directive so that the library can compile.
    A.X., 2014-10-16
*)

const
  DurationList: array[0..12] of Real =
    (0.08333333333, 0.1666666667, 0.3333333333, 0.5,
      1, 2, 6, 8, 12, 18, 24, 36, 48);
  PresetReturnPeriod: array[0..10] of Real =
    (100000, 10000, 1000, 500, 200, 100, 50, 20, 10, 5, 2);
  pl_ProbabilityList: array [0..22] of Real =
    (0.0005,0.001,0.002,0.005,0.01,0.02,0.05,0.10,0.20,0.30,0.40,0.50,0.60,0.70,
    0.80,0.90,0.95,0.98,0.99,0.995,0.998,0.999,0.9995);

procedure TFrmIDFCurves.IFormCreate(Sender: TObject);
var
  i: Integer;
begin
  FDataList := TDataList.Create;
  FStatisticalDistributionType := sdtLGEVMaxK;
  FGEVParameter := 0.15;
  FConsiderTimeResolution := True;
  FAboveThreshold := False;
  FPaperType := pdptGEVMax;
  FPaperMinZ[pdptNormal] := pmz_PaperMinZ[pdptNormal];
  FPaperMaxZ[pdptNormal] := pmz_PaperMaxZ[pdptNormal];
  FPaperMinZ[pdptGumbelMax] := pmz_PaperMinZ[pdptGumbelMax];
  FPaperMaxZ[pdptGumbelMax] := pmz_PaperMaxZ[pdptGumbelMax];
  FPaperMinZ[pdptGEVMax] := pmz_PaperMinZ[pdptGEVMax];
  FPaperMaxZ[pdptGEVMax] := pmz_PaperMaxZ[pdptGEVMax];
  FPaperMinZ[pdptLinear] := 0.01;
  FPaperMaxZ[pdptLinear] := 0.999;
  FSingleIDFReturnPeriod := 200;
  for i := 0 to 22 do
    FProbabilityList[i] := pl_ProbabilityList[i];
  FEta := 0.5;
  FTheta := 0.5;
  FrmProcessingDialog := TFrmProcessingDialog.Create(self);
end;

procedure TFrmIDFCurves.IFormDestroy(Sender: TObject);
begin
  FrmProcessingDialog.Free;
  FDataList.Free;
  SetLength(FDistributionSeries,0);
  SetLength(FWeibullSeries,0);
end;

procedure TFrmIDFCurves.IFormShow(Sender: TObject);
var
  i: Integer;
  s: string;
begin
  sgrdSingle.DefaultRowHeight :=
    sgrdSingle.DefaultRowHeight * Screen.PixelsPerInch div 96;
  SetLength(FDistributionSeries,FIDFTimeseriesCollection.Count);
  SetLength(FWeibullSeries,FIDFTimeseriesCollection.Count);
  lstDistributionLines.Clear;
  for i := 0 to Length(FWeibullSeries)-1 do
  begin
    FWeibullSeries[i] := TPointSeries.Create(self);
    FDistributionSeries[i] := TLineSeries.Create(self);
    FWeibullSeries[i].ParentChart := Chart;
    FDistributionSeries[i].ParentChart := Chart;
    if FIDFTimeseriesCollection[i].Duration <1 then
      s := FloatToStr(60*FIDFTimeseriesCollection[i].Duration)+' min.'
    else
      s := FloatToStr(FIDFTimeseriesCollection[i].Duration)+' hrs.';
    lstDistributionLines.Items.Add(s);
  end;
  for i := 0 to lstDistributionLines.Items.Count-1 do
    lstDistributionLines.Selected[i] := True;
  Refresh;
end;

procedure TFrmIDFCurves.IFormHide(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Length(FWeibullSeries)-1 do
  begin
    FWeibullSeries[i].Free;
    FDistributionSeries[i].Free;
  end;
end;

procedure TFrmIDFCurves.mnuGammaClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    500: FStatisticalDistributionType := sdtExponential;
    501: FStatisticalDistributionType := sdtLExponential;
    502: FStatisticalDistributionType := sdtGamma;
    503: FStatisticalDistributionType := sdtLogPearsonIII;
    504: FStatisticalDistributionType := sdtEV1Max;
    505: FStatisticalDistributionType := sdtLEV1Max;
    506: FStatisticalDistributionType := sdtEV2Max;
    507: FStatisticalDistributionType := sdtLEV2Max;
    508: FStatisticalDistributionType := sdtGEVMax;
    509: FStatisticalDistributionType := sdtLGEVMax;
    510: FStatisticalDistributionType := sdtGEVMaxK;
    511: FStatisticalDistributionType := sdtLGEVMaxK;
    512: FStatisticalDistributionType := sdtPareto;
    513: FStatisticalDistributionType := sdtLPareto;
  else
    Assert(False);
  end;
  TMenuItem(Sender).Checked := True;
  Refresh;
end;

procedure TFrmIDFCurves.mnuNormalPaperClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    601: FPaperType := pdptNormal;
    602: FPaperType := pdptGumbelMax;
    603: FPaperType := pdptGEVMax;
    604: FPaperType := pdptLinear;
  else
    Assert(False);
  end;
  TMenuItem(Sender).Checked := True;
  if TMenuItem(Sender).Tag = 604 then
    chkLogY.Checked := True else
    chkLogY.Checked := False;
  DrawDistribution;
  SetControlStatus;
end;

procedure TFrmIDFCurves.mnuPrintDistributionClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    Chart.Print;
end;

procedure TFrmIDFCurves.mnuCopyDistributionClick(Sender: TObject);
begin
  Chart.CopyToClipboardMetafile(True);
end;

procedure TFrmIDFCurves.mnuPrintMultiClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    chartMulti.Print;
end;

procedure TFrmIDFCurves.chkMulti1000Click(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmIDFCurves.lstDistributionLinesClick(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmIDFCurves.mnuCopyMultiClick(Sender: TObject);
begin
  chartMulti.CopyToClipboardMetafile(True);
end;

resourcestring
  rsEnterGEVCaption = 'Enter GEV Parameter';
  rsEnterGEVPrompt = 'Enter a GEV Parameter (0.001-0.5), '+
  '[default value is 0.15]';

procedure TFrmIDFCurves.mnuSpecifyKappaClick(Sender: TObject);
begin
  FGEVParameter := StrToFloat( InputBox(rsEnterGEVCaption,rsEnterGEVPrompt,
      FloatToStr(FGEVParameter)) );
  if FGEVParameter < 0.001 then
    FGEVParameter := 0.001;
  if FGEVParameter > 0.50 then
    FGEVParameter := 0.50;
  Refresh;
end;

procedure TFrmIDFCurves.mnuTimeResolutionClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  FConsiderTimeResolution := TMenuItem(Sender).Checked;
  Refresh;
end;

procedure TFrmIDFCurves.mnuPrintSingleClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    chartSingle.Print;
end;

procedure TFrmIDFCurves.mnuPrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

resourcestring
  rsSingleIDFEvaluationByCurveFit = 'Single IDF evaluation by curve fitting';
  rsEnterReturnPeriodInYears = 'Enter a return period T in years';
  rsCorrelFactorEq = 'd in hours. Correlation factor = ';
  rsInvalidValueNegative =
    'Return period should be a number greater than 1 year';

procedure TFrmIDFCurves.mnuSingleIDFEvaluationClick(Sender: TObject);
var
  s: string;
  AOmega, AEta, ACorrel: Real;
begin
  s := FormatFloat('0.0', FSingleIDFReturnPeriod);
  if not InputQuery(rsSingleIDFEvaluationByCurveFit,
    rsEnterReturnPeriodInYears, s)
  then Exit;
  FSingleIDFReturnPeriod := ConvertReturnPeriod(StrToFloat(s));
  if FSingleIDFReturnPeriod <=1 then
    raise Exception.Create(rsInvalidValueNegative);
  FIDFTimeseriesCollection.SingleIDFEvaluation(FStatisticalDistributionType,
    FSingleIDFReturnPeriod, FConsiderTimeResolution, FGEVParameter,
      AOmega, AEta, ACorrel);
  ShowMessage('i (mm/h) := '+FormatFloat('0.00',AOmega)+' / d^'+
    FormatFloat('0.000',AEta)+#13#10+
      rsCorrelFactorEq+ FormatFloat('0.000', ACorrel));
end;

procedure TFrmIDFCurves.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmIDFCurves.mnuCopySingleClick(Sender: TObject);
begin
  chartSingle.CopyToClipboardMetafile(True);
end;

procedure TFrmIDFCurves.mnuCopyGridClick(Sender: TObject);
begin
  StringGridToClipboard(sgrdSingle, 0, 5, 0, 13);
end;

procedure TFrmIDFCurves.btnCalculateClick(Sender: TObject);
var
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    ResetSinglePage;
    DrawSingle;
    WriteGrid;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmIDFCurves.btnConfidenceCalculateClick(Sender: TObject);
var
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    ResetSinglePage;
    DrawSingle;
    DrawSingleMC;
    WriteGrid;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmIDFCurves.edtReturnPeriodChange(Sender: TObject);
begin
  ResetSinglePage;
end;

procedure TFrmIDFCurves.rgrpConfidenceIntervalClick(Sender: TObject);
begin
  ResetSinglePage;
end;

procedure TFrmIDFCurves.Refresh;
var
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    IDFEtaThetaEvaluation(FIDFTimeseriesCollection, FConsiderTimeResolution,
      FEta, FTheta, FExplicitEtaTheta);
    IDFDataList(FIDFTimeseriesCollection, FConsiderTimeResolution,
      FEta, FTheta, FDataList);
    FDataList.PrepareData(True, True);
    DrawMulti;
    DrawData;
    DrawDistribution;
    ResetSinglePage;
    DrawEquation;
    SetControlStatus;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmIDFCurves.DrawEquation;
var
  i: Integer;
begin
  case FStatisticalDistributionType of
    sdtExponential, sdtLExponential: i := 3;
    sdtGamma: i := 4;
    sdtLogPearsonIII: i := 6;
    sdtEV1Max, sdtLEV1Max: i := 1;
    sdtEV2Max, sdtLEV2Max: i := 2;
    sdtGEVMax, sdtLGEVMax, sdtGEVMaxK, sdtLGEVMaxK: i := 5;
    sdtPareto, sdtLPareto: i := 7;
  else
    raise Exception.Create('Internal error');
  end;
  ImglstIdfEquations.GetBitmap(i, Image.Picture.Bitmap);
  Image.Repaint;
end;

procedure TFrmIDFCurves.SetControlStatus;
var
  i: Integer;
begin
  {Multi curves activation}
  seriesMulti100000.Active := chkMulti100000.Checked;
  seriesMulti10000.Active := chkMulti10000.Checked;
  seriesMulti1000.Active := chkMulti1000.Checked;
  seriesMulti500.Active := chkMulti500.Checked;
  seriesMulti200.Active := chkMulti200.Checked;
  seriesMulti100.Active := chkMulti100.Checked;
  seriesMulti50.Active := chkMulti50.Checked;
  seriesMulti20.Active := chkMulti20.Checked;
  seriesMulti10.Active := chkMulti10.Checked;
  seriesMulti5.Active := chkMulti5.Checked;
  seriesMulti2.Active := chkMulti2.Checked;
  {Set distribution menu}
  case FStatisticalDistributionType of
    sdtExponential: mnuExponential.Checked := True;
    sdtLExponential: mnuLExponential.Checked := True;
    sdtGamma: mnuGamma.Checked := True;
    sdtLogPearsonIII: mnuLogPearsonIII.Checked := True;
    sdtEV1Max: mnuGumbel.Checked := True;
    sdtLEV1Max: mnuLGumbel.Checked := True;
    sdtEV2Max: mnuEV2Max.Checked := True;
    sdtLEV2Max: mnuLEV2Max.Checked := True;
    sdtGEVMax: mnuGEVMax.Checked := True;
    sdtLGEVMax: mnuLGEVMax.Checked := True;
    sdtGEVMaxK: mnuGEVMaxK.Checked := True;
    sdtLGEVMaxK: mnuLGEVMaxK.Checked := True;
    sdtPareto: mnuPareto.Checked := True;
    sdtLPareto: mnuLPareto.Checked := True;
  else
    Assert(False);
  end;
  case FPaperType of
    pdptNormal: mnuNormalPaper.Checked := True;
    pdptGumbelMax: mnuGumbelPaper.Checked := True;
    pdptGEVMax: mnuGEVMaxPaper.Checked := True;
    pdptLinear: mnuLogLogPaper.Checked := True;
  else
    Assert(False);
  end;
  {other menus}
  mnuTimeResolution.Checked := FConsiderTimeResolution;
  {distriubion lines list}
  for i := 0 to lstDistributionLines.Items.Count-1 do
  begin
    FWeibullSeries[i].Active := False;
    FDistributionSeries[i].Active := False;
    if lstDistributionLines.Selected[i] then
    begin
      FWeibullSeries[i].Active := True;
      FDistributionSeries[i].Active := True;
    end;
  end;
  Chart.LeftAxis.Logarithmic := chkLogY.Checked;
end;

resourcestring
  rsIDFCurves = 'IDF Curves';
  rsDistribution = 'Distribution: ';

procedure TFrmIDFCurves.DrawMulti;
var
  AStatisticalDistribution: TStatisticalDistribution;
  i, j: Integer;
  AFloat, ADuration: Real;
begin
  AStatisticalDistribution := nil;
  try
    AStatisticalDistribution := TStatisticalDistribution.Create(
      FStatisticalDistributionType, FDataList, FGEVParameter);
    chartMulti.Title.Text.Clear;
    chartMulti.Title.Text.Add(rsIDFCurves+ ' - '+
      rsDistribution+AStatisticalDistribution.Name);
    for j := 0 to chartMulti.SeriesCount-1 do
    begin
      chartMulti.Series[j].Clear;
     AFloat := AStatisticalDistribution.InvcdfValue(1-1/
       ConvertReturnPeriod(PresetReturnPeriod[j]));
      for i := 0 to 12 do
      begin
        ADuration := DurationList[i];
        chartMulti.Series[j].AddXY(ADuration,
          AFloat/IDFDurationToIntensity(ADuration, FEta, FTheta),'',
            chartMulti.SeriesList[j].SeriesColor);
      end;
    end;
  finally
    AStatisticalDistribution.Free;
  end;
end;

procedure TFrmIDFCurves.DrawData;
var
  AStatisticalDistribution: TStatisticalDistribution;
begin
  AStatisticalDistribution := nil;
  try
    AStatisticalDistribution := TStatisticalDistribution.Create(
      FStatisticalDistributionType, FDataList, FGEVParameter);
    edtEtaValue.Text := FormatFloat('#.###',FEta);
    edtThetaValue.Text := FormatFloat('#.###',FTheta);
    edtDistributionName.Text := AStatisticalDistribution.Name;
    edtStatParam1Name.Text := AStatisticalDistribution.Param1Name;
    edtStatParam2Name.Text := AStatisticalDistribution.Param2Name;
    edtStatParam3Name.Text := AStatisticalDistribution.Param3Name;
    edtStatParam1Value.Text :=
      FormatFloat('#.##',AStatisticalDistribution.Parameter1);
    edtStatParam2Value.Text :=
      FormatFloat('#.##',AStatisticalDistribution.Parameter2);
    edtStatParam3Value.Text :=
      FormatFloat('#.##',AStatisticalDistribution.Parameter3);
    edtMeanValue.Text := FormatFloat('#.##',FDataList.MeanValue);
    edtStandardDeviation.Text := FormatFloat('#.##',FDataList.StandardDeviation);
    edtSkewness.Text := FormatFloat('#.##',FDataList.Skewness);
    edtLMoment1.Text := FormatFloat('#.##',FDataList.LMoment1);
    edtLMoment2.Text := FormatFloat('#.##',FDataList.LMoment2);
    edtLMoment3.Text := FormatFloat('#.##',FDataList.LMoment3);
  finally
    AStatisticalDistribution.Free;
  end;
end;

procedure TFrmIDFCurves.SetDistributionChartBounds;
begin
  try
    FPaperMinZ[FPaperType] := ProbabilityPaper(0.991,FPaperType,FGEVParameter);
    FPaperMaxZ[FPaperType] := ProbabilityPaper(0.004,FPaperType,FGEVParameter);
    if FPaperType = pdptLinear then
    begin
      FPaperMinZ[FPaperType] := 0.01;
      FPaperMaxZ[FPaperType] := 0.9995;
    end;
  except
    on EMathError do
    begin
      FPaperMinZ[FPaperType] := -2.0;
      FPaperMaxZ[FPaperType] := 8.0;
    end;
    else
      raise;
  end;
  Chart.BottomAxis.Minimum := -100000000;
  Chart.BottomAxis.Maximum := 1000000000;
  Chart.TopAxis.Minimum := -100000000;
  Chart.TopAxis.Maximum := 1000000000;
  Chart.BottomAxis.Minimum := FPaperMinZ[FPaperType];
  Chart.BottomAxis.Maximum := FPaperMaxZ[FPaperType];
  Chart.TopAxis.Minimum := FPaperMinZ[FPaperType];
  Chart.TopAxis.Maximum := FPaperMaxZ[FPaperType];
end;

procedure TFrmIDFCurves.DrawDistributionLines;
var
  AStatisticalDistribution: TStatisticalDistribution;
  i,j: Integer;
  AFloat, AValue: Real;
begin
  AStatisticalDistribution := nil;
  try
    AStatisticalDistribution := TStatisticalDistribution.Create(
      FStatisticalDistributionType, FDataList, FGEVParameter);
    for i := 0 to FIDFTimeseriesCollection.Count-1 do
    begin
      FDistributionSeries[i].Assign(seriesDistribution);
      FDistributionSeries[i].SeriesColor := GetColorSequence(i);
      FDistributionSeries[i].Clear;
      for j := 1 to 119 do
      begin
        try
          AValue := (FPaperMaxZ[FPaperType] - FPaperMinZ[FPaperType])*j/120+
            FPaperMinZ[FPaperType];
          AFloat := AStatisticalDistribution.InvcdfValue(
            InvProbabilityPaper(AValue, FPaperType, FGEVParameter))/
              IDFDurationToIntensity(FIDFTimeseriesCollection[i].Duration,
                FEta, FTheta);
          FDistributionSeries[i].AddXY(AValue, AFloat, '',
            FDistributionSeries[i].SeriesColor);
        except
          on EMathError do
            Continue;
          else
            raise;
        end;
      end;
    end;
  finally
    AStatisticalDistribution.Free;
  end;
end;

procedure TFrmIDFCurves.DrawDistributionPoints;
var
  AFullDataList: TFullDataList;
  i,j: Integer;
  s: string;
  AFloat, AValue: Real;
begin
  AFullDataList := nil;
  for i := 0 to FIDFTimeseriesCollection.Count-1 do
    try
      AFullDataList := TFullDataList.Create;
      AFullDataList.SetTS(FIDFTimeseriesCollection[i].Timeseries,True);
      AFullDataList.PrepareData(True, True);
      FWeibullSeries[i].Assign(WeibullPoints);
      FWeibullSeries[i].SeriesColor := GetColorSequence(i);
      FWeibullSeries[i].Pointer.Brush.Color := FWeibullSeries[i].SeriesColor;
      FWeibullSeries[i].Pointer.Style := GetMarkSequence(i);
      FWeibullSeries[i].Clear;
      if FIDFTimeseriesCollection[i].Duration <1 then
        s := FloatToStr(60*FIDFTimeseriesCollection[i].Duration)+' min.'
      else
        s := FloatToStr(FIDFTimeseriesCollection[i].Duration)+' hrs.';
      FWeibullSeries[i].Title := s;
      for j := 0 to AFullDataList.Count -1 do
      begin
        AFloat := AFullDataList[j].WeibullProbability;
        AValue := AFullDataList[j].Value;
        if FConsiderTimeResolution then
          AValue := AValue * FIDFTimeseriesCollection[i].TimeResolutionFactor;
        FWeibullSeries[i].AddXY(
          ProbabilityPaper( AFloat ,FPaperType, FGEVParameter),
            AValue, '', FWeibullSeries[i].SeriesColor);
      end;
    finally
      FreeAndNil(AFullDataList);
    end;
end;

procedure TFrmIDFCurves.DrawDistribution;
begin
  SetDistributionChartBounds;
  DrawGrid;
  DrawDistributionPoints;
  DrawDistributionLines;
end;

resourcestring
  rsReturnPeriodMax = 'Return period (T) in years';
  rsPaperNormal = ' - scale: Normal distribution';
  rsPaperGumbelMax = ' - scale: Gumbel (Max) distribution';
  rsPaperGEV = ' - scale: GEV (Max) distribution';
  rsPaperLogLog = ' - Logarithmic scale';

procedure TFrmIDFCurves.DrawGrid;
var
  i: Integer;
  ALabel: string;
begin
  Chart.LeftAxis.Title.Caption := 'i (mm/h)';
  Chart.TopAxis.Title.Caption := rsReturnPeriodMax;
  case FPaperType of
    pdptNormal:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperNormal;
    pdptGumbelMax:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperGumbelMax;
    pdptGEVMax:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperGEV;
    pdptLinear:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperLogLog;
    else
      Assert(False);
  end;
  GridPoint.Clear;
  for i:=0 to 22 do
  begin
    try
      if (ProbabilityPaper(FProbabilityList[i],FPaperType,FGEVParameter)<
        FPaperMaxZ[FPaperType]) and
        (ProbabilityPaper(FProbabilityList[i],FPaperType,FGEVParameter)>
        FPaperMinZ[FPaperType]) then
      begin
        ALabel := FormatFloat('#.##',1/FProbabilityList[i]);
        GridPoint.AddXY(ProbabilityPaper(FProbabilityList[i],
          FPapertype,FGEVParameter), 10, ALabel, clNone);
      end;
    except
      on EMathError do
        Continue;
      else
        raise;
    end;
  end;
  if FPaperType = pdptLinear then
    Chart.BottomAxis.Logarithmic := True else
    Chart.BottomAxis.Logarithmic := False;  
end;

function TFrmIDFCurves.GetColorSequence(Index: Integer): TColor;
begin
  Result := clBlack;
  while Index>=12 do
    Index := Index-12;
  case Index of
    0: Result := clBlack;
    1: Result := clBlue;
    2: Result := clOlive;
    3: Result := clMaroon;
    4: Result := clGreen;
    5: Result := clDkGray;
    6: Result := clFuchsia;
    7: Result := clPurple;
    8: Result := clTeal;
    9: Result := clLime;
    10: Result := clGray;
    11: Result := clAqua;
  else
    Assert(False);
  end;
end;

function TFrmIDFCurves.GetMarkSequence(Index: Integer): TSeriesPointerStyle;
begin
  Result := psCircle;
  while Index>=4 do
    Index := Index-4;
  case Index of
    0: Result := psCircle;
    1: Result := psRectangle;
    2: Result := psTriangle;
    3: Result := psDiamond;
  else
    Assert(False);
  end;
end;

procedure TFrmIDFCurves.ResetSinglePage;
var
  i,j: Integer;
begin
  for i := 0 to chartSingle.SeriesCount -1 do
  begin
    chartSingle.Series[i].Clear;
    chartSingle.Series[i].Active := False;
  end;
  chartSingle.Title.Text.Clear;
  edtSingleAlpha.Text := '';
  edtLowerSampleLimit.Text := '';
  edtUpperSampleLimit.Text := '';
  edtLowerConfidenceLimit.Text := '';
  edtUpperConfidenceLimit.Text := '';
  lblDenTheta.Caption := FormatFloat('0.000', FTheta);
  lblDenEta.Caption := FormatFloat('0.000', FEta);
  for i := 0 to 5 do
    for j := 0 to 14 do
      sgrdSingle.Cells[i, j] := '';
end;

resourcestring
  rsReturnPeriodYears =
    'Return period (in years) = ';

procedure TFrmIDFCurves.DrawSingle;
var
  AStatisticalDistribution: TStatisticalDistribution;
  i: Integer;
  AFloat, ADuration, AReturnPeriod: Real;
begin
  AReturnPeriod := StrToFloat( edtReturnPeriod.Text);
  AReturnPeriod := ConvertReturnPeriod(AReturnPeriod);
  if AReturnPeriod <= 1 then
    raise Exception.Create(rsInvalidValueNegative);
  AStatisticalDistribution := nil;
  try
    AStatisticalDistribution := TStatisticalDistribution.Create(
      FStatisticalDistributionType, FDataList, FGEVParameter);
    chartSingle.Title.Text.Clear;
    chartSingle.Title.Text.Add(rsIDFCurves+ ' - '+
    rsDistribution+AStatisticalDistribution.Name);
    chartSingle.Title.Text.Add(rsReturnPeriodYears+FormatFloat('#.##',
      AReturnPeriod));
    seriesSingleCentral.Active := True;
    AFloat := AStatisticalDistribution.InvcdfValue(1-1/AReturnPeriod);
    edtSingleAlpha.Text := FormatFloat('0.00', AFloat);
    for i := 0 to 12 do
    begin
      ADuration := DurationList[i];
      seriesSingleCentral.AddXY(ADuration,
        AFloat/IDFDurationToIntensity(ADuration, FEta, FTheta),'',
          seriesSingleCentral.SeriesColor);
    end;
  finally
    AStatisticalDistribution.Free;
  end;
end;

resourcestring
  rsConfidenceLevelOf =
    'Confidence intervals';

procedure TFrmIDFCurves.DrawSingleMC;
var
  AStatisticalDistribution: TStatisticalDistribution;
  i, j: Integer;
  ADuration, AReturnPeriod, AConfidenceLevel: Real;
  AConfidenceIntervals: array[1..4] of Real;
begin
  AReturnPeriod := StrToFloat( edtReturnPeriod.Text);
  AReturnPeriod := ConvertReturnPeriod(AReturnPeriod);
  if AReturnPeriod <= 1 then
    raise Exception.Create(rsInvalidValueNegative);
  AConfidenceLevel := 0.950;
  RandSeed := 0;
  case rgrpConfidenceInterval.ItemIndex of
    0: AConfidenceLevel := 0.800;
    1: AConfidenceLevel := 0.900;
    2: AConfidenceLevel := 0.950;
    3: AConfidenceLevel := 0.990;
  else
    Assert(False);
  end;
  AStatisticalDistribution := nil;
  try
    AStatisticalDistribution := TStatisticalDistribution.Create(
      FStatisticalDistributionType, FDataList, FGEVParameter);
    Enabled := False;
    FrmProcessingDialog.Initialize(600*
      (AStatisticalDistribution.ParameterCount+1));
    FrmProcessingDialog.Show;
    MonteCarloLimits(1-1/AReturnPeriod, AStatisticalDistribution, 60000,
      FIDFTimeseriesCollection.MCSampleCount, AConfidenceLevel,
        AConfidenceIntervals[1], AConfidenceIntervals[2],
          AConfidenceIntervals[3], AConfidenceIntervals[4], MCLimitsProgress);
    edtUpperSampleLimit.Text := FormatFloat('0.00', AConfidenceIntervals[1]);
    edtLowerSampleLimit.Text := FormatFloat('0.00', AConfidenceIntervals[2]);
    edtUpperConfidenceLimit.Text := FormatFloat('0.00',
      AConfidenceIntervals[3]);
    edtLowerConfidenceLimit.Text := FormatFloat('0.00',
      AConfidenceIntervals[4]);
    chartSingle.Title.Text.Add(FormatFloat('0.0', AConfidenceLevel*100)+'% '+
      rsConfidenceLevelOf);
    for j := 1 to 4 do
    begin
      chartSingle.Series[j].Active := True;
      for i := 0 to 12 do
      begin
        ADuration := DurationList[i];
        chartSingle.Series[j].AddXY(ADuration, AConfidenceIntervals[j]/
          IDFDurationToIntensity(ADuration, FEta, FTheta),'',
            chartSingle.Series[j].SeriesColor);
      end;
    end;
  finally
    Enabled := True;
    FrmProcessingDialog.Hide;
    AStatisticalDistribution.Free;
  end;
end;

procedure TFrmIDFCurves.MCLimitsProgress(var Stop: Boolean);
begin
  Stop := FrmProcessingDialog.ToStopPlease;
  FrmProcessingDialog.StepOne;
end;

procedure TFrmIDFCurves.WriteGrid;
var
  i, j, ACount: Integer;
begin
  for i := 0 to 5 do
    for j := 0 to 14 do
      sgrdSingle.Cells[i, j] := '';
  for i := 0 to 5 do
  begin
    if i = 0 then ACount := 14 else
      ACount := chartSingle.Series[i-1].Count+1;
    for j := 0 to ACount-1 do
    begin
      if (i=0) and (j>0) then
      begin
        if DurationList[j-1]<1 then
          sgrdSingle.Cells[i,j] := FormatFloat('#.##',
            DurationList[j-1]*60)+' min.' else
          sgrdSingle.Cells[i,j] := FormatFloat('#.##',
            DurationList[j-1])+' hrs.';
      end;
      if (j=0) and (i>0) then
      begin
        sgrdSingle.Cells[i,j] := chartSingle.Series[i-1].Title;
      end;
      if (i=0) or (j=0) then
        Continue;
      sgrdSingle.Cells[i,j] :=
        FormatFloat('0.00',chartSingle.Series[i-1].YValue[j-1]);
    end;
  end;
end;

function TFrmIDFCurves.ConvertReturnPeriod(AReturnPeriod: Real): Real;
begin
  if not FAboveThreshold then
    Result := AReturnPeriod else
    Result := 1 / ( 1 - Exp(-1 / AReturnPeriod) );
end;

procedure TFrmIDFCurves.Convertreturnperiodsforabovethresholdtimeseries1Click(
  Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  FAboveThreshold := TMenuItem(Sender).Checked;
  Refresh;
end;

end.
