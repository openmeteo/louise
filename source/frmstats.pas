{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2004   National Technical University of Athens    }
{                                                                  }
{******************************************************************}

{** A Form to show statistics - distribution curves for a Timeseries.}
unit frmstats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  iform, ts, Dates, Math, TeEngine, Series, ExtCtrls, TeeProcs, Chart,
  ComCtrls, Menus, StdCtrls, Dialogs, Grids, frmprcsdlg, mcsettingsdlg,
  statprocesses, StrGrdOd;

type
  TArrayOfReal = array of Real;
type
  {** ProbabilityRecord Class, holding a value, the Weibull
      probability and the Date.
  }
  TFrmStatistics = class(TForm)
    mmnuMainMenu: TMainMenu;
    mnuFile: TMenuItem;
    btnLog: TCheckBox;
    mnuView: TMenuItem;
    mnuPaperType: TMenuItem;
    mnuLinear: TMenuItem;
    mnuNormal: TMenuItem;
    mnuGumbelMax: TMenuItem;
    mnuGumbelMin: TMenuItem;
    mnuWeibull: TMenuItem;
    mnuLogNormal: TMenuItem;
    grpEmpiricalDistribution: TGroupBox;
    btnWeibullPoints: TCheckBox;
    btnBlomPoints: TCheckBox;
    btnCunnanePoints: TCheckBox;
    btnGringortenPoints: TCheckBox;
    mnuPrintChart: TMenuItem;
    mnuExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuCopyChart: TMenuItem;
    mnuGEV: TMenuItem;
    mnuOptions: TMenuItem;
    mnuUnbiased: TMenuItem;
    mnuPrinterSetup: TMenuItem;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    N1: TMenuItem;
    N3: TMenuItem;
    HorizAxisis1: TMenuItem;
    mnuExceedanceProbability: TMenuItem;
    mnuProbabilityFunction: TMenuItem;
    mnuReturnPeriodMax: TMenuItem;
    mnuReturnPeriodMin: TMenuItem;
    mnuSetPaperMaxX: TMenuItem;
    Memo: TMemo;
    mnuCopyGridClipboardAll: TMenuItem;
    N2: TMenuItem;
    MnuCopyGridSelection: TMenuItem;
    mnuGEVMin: TMenuItem;
    mnuForecasts: TMenuItem;
    mnuToAProbability: TMenuItem;
    mnuToAnExceedanceProbability: TMenuItem;
    mnuToAReturnPeriodMax: TMenuItem;
    mnuToAReturnPeriodMin: TMenuItem;
    N4: TMenuItem;
    mnuToAValue: TMenuItem;
    N5: TMenuItem;
    mnuHideForecasts: TMenuItem;
    pgcPages: TPageControl;
    tbcPlots: TTabSheet;
    tbcParameters: TTabSheet;
    sgrdData: TOdStringGrid;
    tbcMonths: TTabControl;
    Chart: TChart;
    GridPoint: TPointSeries;
    WeibullPoints: TPointSeries;
    BlomPoints: TPointSeries;
    CunnanePoints: TPointSeries;
    GringortenPoints: TPointSeries;
    NormalLine: TLineSeries;
    LogNormalLine: TLineSeries;
    GaltonLine: TLineSeries;
    ExponentialLine: TLineSeries;
    GammaLine: TLineSeries;
    PearsonIIILine: TLineSeries;
    LogPearsonIIILine: TLineSeries;
    GumbelMAXLine: TLineSeries;
    GumbelMINLine: TLineSeries;
    WeibullLine: TLineSeries;
    GEVMAXLine: TLineSeries;
    GEVMINLine: TLineSeries;
    GEVMAXLLine: TLineSeries;
    GEVMINLLine: TLineSeries;
    EV1MAXLLine: TLineSeries;
    EV2MAXLLine: TLineSeries;
    EV1MINLLine: TLineSeries;
    EV3MINLLine: TLineSeries;
    EV2MAXLine: TLineSeries;
    NormalLLine: TLineSeries;
    ExponentialLLine: TLineSeries;
    ParetoLine: TLineSeries;
    ParetoLLine: TLineSeries;
    btnReset: TButton;
    N6: TMenuItem;
    mnuNegativeValues: TMenuItem;
    mnuTruncateToZero: TMenuItem;
    mnuConfidence: TMenuItem;
    mnuNormalFamily: TMenuItem;
    mnuGammaFamily: TMenuItem;
    mnuEVFamily: TMenuItem;
    mnuConfidenceNormal: TMenuItem;
    LowSampleLimitLine: TLineSeries;
    HighSampleLimitLine: TLineSeries;
    HighConfidenceLimitLine: TLineSeries;
    LowConfidenceLimitLine: TLineSeries;
    mnuConfidenceExponential: TMenuItem;
    mnuConfidenceGamma: TMenuItem;
    mnuConfidenceLogNormal: TMenuItem;
    mnuConfidenceGumbelMax: TMenuItem;
    mnuConfidenceEV2MAX: TMenuItem;
    mnuConfidenceGumbelMin: TMenuItem;
    mnuConfidenceWeibull: TMenuItem;
    mnuMCSettings: TMenuItem;
    mnuConfidenceGalton: TMenuItem;
    mnuConfidenceLMomentsNormal: TMenuItem;
    mnuConfidencePearsonIII: TMenuItem;
    mnuConfidenceLogPearsonIII: TMenuItem;
    mnuConfidenceLMomentExponential: TMenuItem;
    mnuEVfamilyLMoments: TMenuItem;
    mnuConfidenceGumbelLMax: TMenuItem;
    mnuConfidenceEV2LMAX: TMenuItem;
    mnuConfidenceGumbelLMin: TMenuItem;
    mnuConfidenceWeibullL: TMenuItem;
    mnuConfidenceGEVMax: TMenuItem;
    mnuConfidenceGEVMin: TMenuItem;
    mnuConfidenceGEVLMax: TMenuItem;
    mnuConfidenceGEVLMin: TMenuItem;
    mnuConfidencePareto: TMenuItem;
    mnuConfidenceParetoL: TMenuItem;
    N7: TMenuItem;
    mnuRefreshParametersTable: TMenuItem;
    N8: TMenuItem;
    mnuConfidenceForecasts: TMenuItem;
    mnuHideConfidenceForcasts: TMenuItem;
    GEVMINKLLine: TLineSeries;
    GEVMAXKLine: TLineSeries;
    GEVMINKLine: TLineSeries;
    GEVMAXKLLine: TLineSeries;
    mnuConfidenceGEVMaxK: TMenuItem;
    mnuConfidenceGEVMinK: TMenuItem;
    mnuConfidenceGEVLMaxK: TMenuItem;
    mnuConfidenceGEVLMinK: TMenuItem;
    SetGEVShapeparameter1: TMenuItem;
    mnuTests: TMenuItem;
    mnuXSquareTest: TMenuItem;
    mnuKolmogorovSminovTest: TMenuItem;
    lstDistributions: TListBox;
    Label1: TLabel;
    mnuCopyChartData: TMenuItem;
    N9: TMenuItem;
    mnuAutoLeftAxis: TMenuItem;
    mnuAllowZoomAndPan: TMenuItem;
    N10: TMenuItem;
    mnuCopyHistogram: TMenuItem;
    mnuPrintHistogram: TMenuItem;
    Showhistogram1: TMenuItem;
    tbcHistogram: TTabSheet;
    chartPDF: TChart;
    seriesHistogram: TBarSeries;
    procedure IFormCreate(Sender: TObject);
    procedure IFormDestroy(Sender: TObject);
    procedure btnLogClick(Sender: TObject);
    procedure mnuLinearClick(Sender: TObject);
    procedure mnuNormalClick(Sender: TObject);
    procedure mnuGumbelMaxClick(Sender: TObject);
    procedure mnuGumbelMinClick(Sender: TObject);
    procedure mnuWeibullClick(Sender: TObject);
    procedure mnuLogNormalClick(Sender: TObject);
    procedure btnWeibullPointsClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuPrintChartClick(Sender: TObject);
    procedure mnuCopyChartClick(Sender: TObject);
    procedure tbcMonthsChange(Sender: TObject);
    procedure mnuGEVClick(Sender: TObject);
    procedure btnNormalLineClick(Sender: TObject);
    procedure mnuUnbiasedClick(Sender: TObject);
    procedure mnuPrinterSetupClick(Sender: TObject);
    procedure mnuExceedanceProbabilityClick(Sender: TObject);
    procedure mnuProbabilityFunctionClick(Sender: TObject);
    procedure mnuReturnPeriodMaxClick(Sender: TObject);
    procedure mnuReturnPeriodMinClick(Sender: TObject);
    procedure mnuSetPaperMaxXClick(Sender: TObject);
    procedure mnuCopyGridClipboardAllClick(Sender: TObject);
    procedure MnuCopyGridSelectionClick(Sender: TObject);
    procedure mnuGEVMinClick(Sender: TObject);
    procedure mnuToAProbabilityClick(Sender: TObject);
    procedure mnuHideForecastsClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure mnuTruncateToZeroClick(Sender: TObject);
    procedure mnuConfidenceClick(Sender: TObject);
    procedure mnuMCSettingsClick(Sender: TObject);
    procedure mnuConfidenceForecastsClick(Sender: TObject);
    procedure SetGEVShapeparameter1Click(Sender: TObject);
    procedure mnuXSquareTestClick(Sender: TObject);
    procedure mnuKolmogorovSminovTestClick(Sender: TObject);
    procedure MCLimitsProgress(var Stop: Boolean);
    procedure mnuCopyChartDataClick(Sender: TObject);
    procedure mnuAutoLeftAxisClick(Sender: TObject);
    procedure WeibullPointsClick(Sender: TChartSeries; ValueIndex: Integer;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mnuAllowZoomAndPanClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuCopyHistogramClick(Sender: TObject);
    procedure mnuPrintHistogramClick(Sender: TObject);
    procedure Showhistogram1Click(Sender: TObject);
  private
    FPaperType: TProbabilityPaperType;
    FTimeStep: TTimeStep;
    FMonthShowed: Integer;
    FUnbiased: Boolean;
    FProbabilityList: array[0..22] of Real;
    FPaperMinZ: array[TProbabilityPaperType] of Real;
    FPaperMaxZ: array[TProbabilityPaperType] of Real;
    FPaperMinX: Real;
    FPaperMaxX: Real;
    FSetPaperMaxX: Real;
    FMinProbability: Real;
    FMaxProbability: Real;
    FGEVParameter: Real;
    FFullDataList: TFullDataList;
    FMonthlyDataList: TMonthlyDataList;
    FrmProcessingDialog: TFrmProcessingDialog;
    FrmConfIntSettings: TFrmConfIntSettings;
    FAutoLeftAxisMin: Boolean;
    FAllowZoomAndPan: Boolean;
    SettingMCCount: Integer;
    SettingMCPointsCount: Integer;
    SettingMCConfidence: Integer;
    FHYearOrigin: Integer;
    procedure ResetAllButtons;
    procedure Refresh(RefreshStatus: Boolean);
    procedure FillTheGrid;
    procedure DrawGraph(DrawStatus: Boolean);
    procedure PrepareGraphSeries;
    procedure PrepareLineSeries;
    procedure PreparePointSeries;
    procedure PrepareHistogram;
    procedure DrawGrid;
    procedure ShowSeries;
    procedure DrawLines;
    procedure DrawPoints;
    procedure DoMonteCarloSimul(ADistributionType: TStatisticalDistributionType);
    procedure DoXSquareTest;
    procedure DoKolmogorovSmirnovTest;
  public
    ParentForm: TForm;
    procedure SetTS(Timeseries: TTimeseries);
    property HYearOrigin: Integer read FHYearOrigin write FHYearOrigin;
  end;

  const pl_ProbabilityList: array [0..22] of Real =
    (0.0005,0.001,0.002,0.005,0.01,0.02,0.05,0.10,0.20,0.30,0.40,0.50,0.60,0.70,
    0.80,0.90,0.95,0.98,0.99,0.995,0.998,0.999,0.9995);

  const pl_Distributions: array [0..26] of TStatisticalDistributionType =
    (sdtNormal, sdtLNormal, sdtLogNormal, sdtGalton, sdtExponential,
      sdtLExponential, sdtGamma, sdtPearsonIII, sdtLogPearsonIII,
      sdtEV1Max, sdtEV2Max, sdtEV1Min, sdtEV3Min, sdtGEVMax, sdtGEVMin,
      sdtPareto, sdtLGEVMax, sdtLGEVMin, sdtLEV1Max, sdtLEV2Max,
      sdtLEV1Min, sdtLEV3Min, sdtLPareto, sdtGEVMaxK, sdtGEVMinK,
      sdtLGEVMaxK, sdtLGEVMinK);

implementation

{$R *.DFM}

uses montecarlo, prob, uiutils, Clipbrd;

procedure TFrmStatistics.IFormCreate(Sender: TObject);
var
  i, j: Integer;
  ALineSeries: TLineSeries;
begin
  lstDistributions.Clear;
  for i := 101 to 200 do
    for j := 0 to Chart.SeriesCount-1 do
      if Chart.Series[j].Tag=i then
        lstDistributions.AddItem(Chart.Series[j].Title, Chart.Series[j]);
  lstDistributions.Selected[0] := True;
  lstDistributions.Selected[4] := True;
  FFullDataList := TFullDataList.Create;
  FMonthlyDataList := TMonthlyDataList.Create;
  FrmProcessingDialog := TFrmProcessingDialog.Create(self);
  FrmConfIntSettings := TFrmConfIntSettings.Create(self);
{  for i := 0 to 6 do
  begin
    FPaperMinZ[i] := pmz_PaperMinZ[i];
    FPaperMaxZ[i] := pmz_PaperMaxZ[i];
  end;}
  FPaperMinZ[pdptLinear] := pmz_PaperMinZ[pdptLinear];
  FPaperMaxZ[pdptLinear] := pmz_PaperMaxZ[pdptLinear];
  FPaperMinZ[pdptNormal] := pmz_PaperMinZ[pdptNormal];
  FPaperMaxZ[pdptNormal] := pmz_PaperMaxZ[pdptNormal];
  FPaperMinZ[pdptGumbelMax] := pmz_PaperMinZ[pdptGumbelMax];
  FPaperMaxZ[pdptGumbelMax] := pmz_PaperMaxZ[pdptGumbelMax];
  FPaperMinZ[pdptGumbelMin] := pmz_PaperMinZ[pdptGumbelMin];
  FPaperMaxZ[pdptGumbelMin] := pmz_PaperMaxZ[pdptGumbelMin];
  FPaperMinZ[pdptWeibull] := pmz_PaperMinZ[pdptWeibull];
  FPaperMaxZ[pdptWeibull] := pmz_PaperMaxZ[pdptWeibull];
  FPaperMinZ[pdptGEVMax] := pmz_PaperMinZ[pdptGEVMax];
  FPaperMaxZ[pdptGEVMax] := pmz_PaperMaxZ[pdptGEVMax];
  FPaperMinZ[pdptGEVMin] := pmz_PaperMinZ[pdptGEVMin];
  FPaperMaxZ[pdptGEVMin] := pmz_PaperMaxZ[pdptGEVMin];

  FPaperType := pdptNormal;
  FMonthShowed := 0;
  FUnbiased := True;
  FGEVParameter := 0.15;
  for i := 0 to 22 do
    FProbabilityList[i] := pl_ProbabilityList[i];
{These for the drawing of the distribution curves}
  FMinProbability := 0.0002;
  FMaxProbability := 0.9998;
  SettingMCCount := 1;
  SettingMCPointsCount := 1;
  SettingMCConfidence := 1;
  FAutoLeftAxisMin := False;
  FAllowZoomAndPan := False;
  for i := 5 to Chart.SeriesCount-1 do
  begin
    ALineSeries := TLineSeries.Create(nil);
    ALineSeries.Assign(Chart.Series[i]);
    chartPDF.AddSeries(ALineSeries);
  end;
  ChartPDF.Align := alClient;
end;

procedure TFrmStatistics.IFormDestroy(Sender: TObject);
var
  i: Integer;
  ALineSeries: TChartSeries;
begin
  FrmProcessingDialog.Free;
  FrmConfIntSettings.Free;
  FFullDataList.Free;
  FMonthlyDataList.Free;
  for i := ChartPdf.SeriesCount-1 downto 1 do
  begin
    ALineSeries := ChartPdf.Series[i];
    ChartPdf.RemoveSeries(ALineSeries);
    ALineSeries.Free;
  end;
end;

procedure TFrmStatistics.mnuExitClick(Sender: TObject);
begin
  Hide;
end;

procedure TFrmStatistics.mnuPrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TFrmStatistics.mnuPrintHistogramClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    ChartPDF.Print;
end;

procedure TFrmStatistics.mnuPrintChartClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    Chart.Print;
end;

procedure TFrmStatistics.mnuCopyChartClick(Sender: TObject);
begin
  Chart.CopyToClipboardMetafile(False);
end;

procedure TFrmStatistics.mnuCopyChartDataClick(Sender: TObject);
var
  s: String;
  i, j: Integer;
  ToContinue: Boolean;
begin
  s := '';
  i := 0;
{ j=0 is for grid point, do not copy it }
  for j := 1 to Chart.SeriesCount-1 do
  begin
    if not Chart.Series[j].Visible then Continue;
    s := s + Chart.Series[j].Title+ '(Fx)'+#9;
    s := s + '(X)';
    if j < Chart.SeriesCount-1 then s := s+#9
  end;
  s := s+#13;
  ToContinue := True;
  while ToContinue do
  begin
    ToContinue := False;
    for j := 1 to Chart.SeriesCount-1 do
    begin
      if not Chart.Series[j].Visible then Continue;
      if i<Chart.Series[j].Count then
      begin
        ToContinue := True;
        s := s + FloatToStr(InvProbabilityPaper(Chart.Series[j].XValue[i],
          FPaperType, FGEVParameter));
        s := s + #9;
        s := s + FloatToStr(Chart.Series[j].YValue[i]);
        if j< Chart.SeriesCount-1 then s := s+#9;
      end else begin
        s := s+#9;
        if j< Chart.SeriesCount-1 then s := s+#9;
      end;
    end;
    if ToContinue then
      s := s+#13;
    Inc(i);
  end;
  Clipboard.AsText := s;
end;

procedure TFrmStatistics.mnuCopyGridClipboardAllClick(Sender: TObject);
begin
  StringGridToClipboard(sgrdData, 0, sgrdData.ColCount-1,
    0, sgrdData.RowCount-1);
end;

procedure TFrmStatistics.MnuCopyGridSelectionClick(Sender: TObject);
begin
  with sgrdData.Selection do
    StringGridToClipboard(sgrdData,Left,Right,Top,Bottom);
end;

procedure TFrmStatistics.mnuLinearClick(Sender: TObject);
begin
{I made this to avoid the Event handler dispach, after the Checked
 proberty of the CheckBox is set. It is a little hack...}
  try
    btnLog.OnClick := nil;
    btnLog.Checked := False;
  finally
    btnLog.OnClick := btnLogClick;
  end;
  FPaperType := pdptLinear;
  mnuLinear.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuNormalClick(Sender: TObject);
begin
  try
    btnLog.OnClick := nil;
    btnLog.Checked := False;
  finally
    btnLog.OnClick := btnLogClick;
  end;
  FPaperType := pdptNormal;
  mnuNormal.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuLogNormalClick(Sender: TObject);
begin
  try
    btnLog.OnClick := nil;
    btnLog.Checked := True;
  finally
    btnLog.OnClick := btnLogClick;
  end;
  FPaperType := pdptNormal;
  mnuLogNormal.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuGumbelMaxClick(Sender: TObject);
begin
  try
    btnLog.OnClick := nil;
    btnLog.Checked := False;
  finally
    btnLog.Checked := False;
  end;
  btnLog.OnClick := btnLogClick;
  FPaperType := pdptGumbelMax;
  mnuGumbelMax.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuGumbelMinClick(Sender: TObject);
begin
  try
    btnLog.OnClick := nil;
    btnLog.Checked := False;
  finally
    btnLog.OnClick := btnLogClick;
  end;
  FPaperType := pdptGumbelMin;
  mnuGumbelMin.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuWeibullClick(Sender: TObject);
begin
  try
    btnLog.OnClick := nil;
    btnLog.Checked := True;
  finally
    btnLog.OnClick := btnLogClick;
  end;
  FPaperType := pdptWeibull;
  mnuWeibull.Checked := True;
  DrawGraph(False);
end;

resourcestring
  rsEnterGEVCaption = 'Enter GEV Parameter';
  rsEnterGEVPrompt = 'Enter a GEV Parameter (0.001-0.5), '+
  '[default value is 0.15]';

procedure TFrmStatistics.mnuGEVClick(Sender: TObject);
begin
{This is a workarround in order to avoid running Onclick
 method when Checked is set to False.}
  try
    btnLog.OnClick := nil;
    btnLog.Checked := False;
  finally
    btnLog.OnClick := btnLogClick;
  end;
  FPaperType := pdptGEVMax;
  mnuGEV.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuGEVMinClick(Sender: TObject);
begin
{This is a workarround in order to avoid running Onclick
 method when Checked is set to False.}
  try
    btnLog.OnClick := nil;
    btnLog.Checked := False;
  finally
    btnLog.OnClick := btnLogClick;
  end;
  FPaperType := pdptGEVMin;
  mnuGEVMin.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.SetGEVShapeparameter1Click(Sender: TObject);
begin
  FGEVParameter := StrToFloat( InputBox(rsEnterGEVCaption,rsEnterGEVPrompt,
      FloatToStr(FGEVParameter)) );
  if FGEVParameter < 0.001 then
    FGEVParameter := 0.001;
  if FGEVParameter > 0.50 then
    FGEVParameter := 0.50;
  Refresh(False);    
end;

procedure TFrmStatistics.mnuCopyHistogramClick(Sender: TObject);
begin
  chartPDF.CopyToClipboardMetafile(True);
end;

procedure TFrmStatistics.mnuExceedanceProbabilityClick(Sender: TObject);
begin
  mnuExceedanceProbability.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuProbabilityFunctionClick(Sender: TObject);
begin
  mnuProbabilityFunction.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuReturnPeriodMaxClick(Sender: TObject);
begin
  mnuReturnPeriodMax.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuReturnPeriodMinClick(Sender: TObject);
begin
  mnuReturnPeriodMin.Checked := True;
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuUnbiasedClick(Sender: TObject);
begin
  mnuUnbiased.Checked := not mnuUnbiased.Checked;
  FUnbiased := mnuUnbiased.Checked;
  Refresh(False);
end;

resourcestring
  rsEnterMaxXCaption = 'Enter Maximum X Value';
  rsEnterMaxXPrompt = 'Enter a value for the maximum value to the X axis';

procedure TFrmStatistics.mnuSetPaperMaxXClick(Sender: TObject);
begin
  FSetPaperMaxX := StrToFloat( InputBox(rsEnterMaxxCaption,rsEnterMaxXPrompt,
      FloatToStr(FPaperMaxX)) );
  DrawGraph(False);
end;

procedure TFrmStatistics.mnuTruncateToZeroClick(Sender: TObject);
var
  i: Integer;
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    mnuTruncateToZero.Checked := not mnuTruncateToZero.Checked;
    FFullDataList.PrepareData(True, mnuTruncateToZero.Checked);
    if FTimeStep < tstAnnual then
    begin
      for i := 1 to 12 do
      begin
        FMonthlyDataList.Months[i].PrepareData(False, mnuTruncateToZero.Checked);
      end;
    end;
    Refresh(False);
  finally
    Screen.Cursor := ACursor;
  end;
end;

{Forecasts menus interaction}

resourcestring
  rsEnterProbabilityValue = 'Enter probability value F(%)';
  rsEnterExceedProbabilityValue =
    'Enter exceedance probability value F1(%)';
  rsEnterReturnPeriodMax = 'Enter return period (Max) in years';
  rsEnterReturnPeriodMin = 'Enter return period (Min) in years';
  rsEnterValueInterpolate = 'Enter a value to interpolate probability';
  rsAllData = 'All data';
  rsValueWord = 'Value';

function ChartSeries2XValues(AChartSeries: TChartSeries): TArrayOfReal;
var
  i: Integer;
begin
  for i := 0 to AChartSeries.Count-1 do
    Result[i] := AChartSeries.XValue[i];
end;

function ChartSeries2YValues(AChartSeries: TChartSeries): TArrayOfReal;
var
  i: Integer;
begin
  for i := 0 to AChartSeries.Count-1 do
    Result[i] := AChartSeries.YValue[i];
end;

resourcestring
  rsInvalidDistribution = 'Invalid';

procedure TFrmStatistics.mnuToAProbabilityClick(Sender: TObject);
var
  AInterpolationValue: Real;
  AStatisticalDistribution: TStatisticalDistribution;
  AStraight: Boolean;
  ADataList: TDataList;
  i: Integer;
  AString: string;
begin
  AStraight := True;
  {dummy initialization, to avoid compiler warnings}
  AInterpolationValue := 0.50;
  if Sender = mnuToAProbability then
  begin
    AString := '50';
    if not InputQuery(rsEnterProbabilityValue,
      rsEnterProbabilityValue, AString) then Exit;
    AInterpolationValue:= StrToFloat( AString );
    AInterpolationValue := 1 - AInterpolationValue / 100;
  end else if Sender = mnuToAnExceedanceProbability then begin
    AString := '50';
    if not InputQuery(rsEnterExceedProbabilityValue,
      rsEnterExceedProbabilityValue, AString) then Exit;
    AInterpolationValue:= StrToFloat( AString );
    AInterpolationValue := AInterpolationValue/100;
  end else if Sender = mnuToAReturnPeriodMax then begin
    AString := '100';
    if not InputQuery(rsEnterReturnPeriodMax,
      rsEnterReturnPeriodMax, AString) then Exit;
    AInterpolationValue:= StrToFloat( AString );
    if Abs(AInterpolationValue)>1e-9 then
      AInterpolationValue := 1/AInterpolationValue
    else
      AInterpolationValue := -1;
  end else if Sender = mnuToAReturnPeriodMin then begin
    AString := '100';
    if not InputQuery(rsEnterReturnPeriodMin,
      rsEnterReturnPeriodMin, AString) then Exit;
    AInterpolationValue:= StrToFloat( AString );
    if Abs(AInterpolationValue)>1e-9 then
      AInterpolationValue := 1 - 1/AInterpolationValue
    else
      AInterpolationValue := -1;
  end else if Sender = mnuToAValue then begin
    AString := '10';
    if not InputQuery(rsEnterValueInterpolate,
      rsEnterValueInterpolate, AString) then Exit;
    AInterpolationValue:= StrToFloat( AString );
    AStraight := False;
  end;
  if Sender <> mnuToAValue then
  begin
    if (AInterpolationValue <=0 ) or (AInterpolationValue >= 1) then
      Exit;
  end else begin
    if (AInterpolationValue <=0 ) then
      Exit;
  end;
  pgcPages.ActivePage := tbcParameters;
  if Sender <> mnuToAValue then
    sgrdData.ColCount := 2
  else
    sgrdData.ColCount := 5;
  sgrdData.RowCount := 28;
  sgrdData.DefaultColWidth := 60;
  sgrdData.ColWidths[0] := 200;
  if FMonthShowed>0 then
    sgrdData.Cells[0,0] := LongMonthNames[FMonthShowed]+' - '
  else
    sgrdData.Cells[0,0] := rsAllData+' - ';
  if Sender = mnuToAProbability then
  begin
    sgrdData.Cells[0,0] := sgrdData.Cells[0,0] +
      'F= '+FormatFloat('#.##',100-AInterpolationValue*100)+' %';
    sgrdData.Cells[1,0] := rsValueWord;
  end else if Sender = mnuToAnExceedanceProbability then begin
    sgrdData.Cells[0,0] := sgrdData.Cells[0,0] +
      'F1= '+FormatFloat('#.##',AInterpolationValue*100)+' %';
    sgrdData.Cells[1,0] := rsValueWord;
  end else if Sender = mnuToAReturnPeriodMax then begin
    sgrdData.Cells[0,0] := sgrdData.Cells[0,0] +
      'T(Max)= '+FormatFloat('#.##',1/AInterpolationValue)+' y';
    sgrdData.Cells[1,0] := rsValueWord;
  end else if Sender = mnuToAReturnPeriodMin then begin
    sgrdData.Cells[0,0] := sgrdData.Cells[0,0] +
      'T(Min)= '+FormatFloat('#.##',1/(1-AInterpolationValue))+' y';
    sgrdData.Cells[1,0] := rsValueWord;
  end else if Sender = mnuToAValue then begin
    sgrdData.Cells[0,0] := sgrdData.Cells[0,0] +
      rsValueWord+'= '+FormatFloat('#.##',AInterpolationValue);
    sgrdData.Cells[1,0] := 'F(%)';
    sgrdData.Cells[2,0] := 'F1(%)';
    sgrdData.Cells[3,0] := 'T(Max)(y)';
    sgrdData.Cells[4,0] := 'T(Min)(y)';
  end;
  for i := Low(pl_Distributions) to High(pl_Distributions) do
  begin
    if FMonthShowed = 0 then
    begin
      FFullDataList.Unbiased := FUnbiased;
      ADataList := TDataList (FFullDataList);
    end else begin
      FMonthlyDataList.Months[FMonthShowed].Unbiased := FUnbiased;
      ADataList := FMonthlyDataList.Months[FMonthShowed];
    end;
    AStatisticalDistribution := nil;
    try
      try
        AStatisticalDistribution :=
          TStatisticalDistribution.Create(pl_Distributions[i], ADataList,
            FGEVParameter);
      except
        on EMathError do
        begin
          sgrdData.Cells[0,i+1] :=  rsInvalidDistribution;
          sgrdData.Cells[1,i+1] := '';
          sgrdData.Cells[2,i+1] := '';
          sgrdData.Cells[3,i+1] := '';
          sgrdData.Cells[4,i+1] := '';
          Continue;
        end;
        else
          raise;
      end;
      sgrdData.Cells[0,i+1] := AStatisticalDistribution.Name;
      try
        if AStraight then
          sgrdData.Cells[1,i+1] := FormatFloat('#.##',
            AStatisticalDistribution.InvcdfValue(1-AInterpolationValue)) else
          sgrdData.Cells[1,i+1] := FormatFloat('#.####',
            AStatisticalDistribution.cdfValue(AInterpolationValue));
      except
        sgrdData.Cells[1,i+1] := '';
      end;
    finally
      AStatisticalDistribution.Free;
    end;
  end;
  if Sender = mnuToAValue then
  begin
    with sgrdData do
    begin
      for i := 1 to RowCount - 1 do
      begin
        try
          Cells[2,i] := FormatFloat('#.##',100-StrToFloat(Cells[1,i])*100);
          Cells[4,i] := FormatFloat('#.##',1/StrToFloat(Cells[1,i]));
          Cells[3,i] := FormatFloat('#.##',1/(1-StrToFloat(Cells[1,i])));
          Cells[1,i] := FormatFloat('#.##',StrToFloat(Cells[1,i])*100);
        except
          on EConvertError do
          begin
            Cells[1,i] := '';
            Cells[2,i] := '';
            Cells[3,i] := '';
            Cells[4,i] := '';
          end;
          on EMathError do
          begin
            Cells[1,i] := '';
            Cells[2,i] := '';
            Cells[3,i] := '';
            Cells[4,i] := '';
          end;
          else
            raise;
        end;
      end;
    end;
  end;
end;

procedure TFrmStatistics.mnuHideForecastsClick(Sender: TObject);
begin
  FillTheGrid;
end;

procedure TFrmStatistics.mnuMCSettingsClick(Sender: TObject);
begin
  with FrmConfIntSettings do
  begin
    rgrpMCCount.ItemIndex := SettingMCCount;
    rgrpMCPointsCount.ItemIndex := SettingMCPointsCount;
    rgrpMCConfidence.ItemIndex := SettingMCConfidence;
    if FrmConfIntSettings.ShowModal = mrOK then
    begin
      SettingMCCount := rgrpMCCount.ItemIndex ;
      SettingMCPointsCount := rgrpMCPointsCount.ItemIndex;
      SettingMCConfidence := rgrpMCConfidence.ItemIndex;
    end;
  end;
end;

{Basic points - lines checkboxes interaction}

procedure TFrmStatistics.btnWeibullPointsClick(Sender: TObject);
begin
  DrawGraph(True);
end;

procedure TFrmStatistics.btnNormalLineClick(Sender: TObject);
begin
  DrawGraph(True);
end;

procedure TFrmStatistics.btnResetClick(Sender: TObject);
begin
  ResetAllButtons;
end;

procedure TFrmStatistics.btnLogClick(Sender: TObject);
begin
  DrawGraph(False);
end;

procedure TFrmStatistics.tbcMonthsChange(Sender: TObject);
begin
  with tbcMonths do
  begin
    if (TabIndex >=0) and (TabIndex <(12-FHYearOrigin+1)) then
      FMonthShowed := TabIndex + FHYearOrigin
    else if (TabIndex >=(12-FHYearOrigin+1)) and (TabIndex <12) then
      FMonthShowed := TabIndex - {2}(12-FHYearOrigin)
    else
      FMonthShowed := 0;
  end;
  Refresh(True);
end;

{Misc. Methods}

resourcestring
  rsOperationPermitedOnMonthlyYearly = 'Operation permited only in '+
    'time series with monthly or yearly time step';

procedure TFrmStatistics.SetTS(Timeseries: TTimeseries);
  function MonthConvert(Value: Integer): Integer;
  begin
    if Value<=(12-FHYearOrigin+1) then
      Result := Value+(FHYearOrigin-1) else
      Result := Value-(12-FHYearOrigin+1);
  end;

var
  i: Integer;
begin
  FTimeStep := Timeseries.TimeStep;
  FFullDataList.SetTS(Timeseries, mnuTruncateToZero.Checked);
  if Timeseries.TimeStep < tstAnnual then
    FMonthlyDataList.FillData(FFullDataList, mnuTruncateToZero.Checked);
  mnuNormal.Checked := True;
  mnuUnbiased.Checked := True;
  btnLog.Checked := False;
  FPaperType := pdptNormal;
  FUnbiased := True;
{Zero SetPaperMaxX so it is calculated programatically}
  FSetPaperMaxX := 0;
  if FTimeStep>=tstAnnual then
  begin
    FMonthShowed := 0;
    tbcMonths.Tabs.Clear;
    tbcMonths.Tabs.Add(rsAllData);
  end else
  begin
    FMonthShowed := 10;
    tbcMonths.Tabs.Clear;
    for i := 1 to 12 do
      tbcMonths.Tabs.Add(LongMonthNames[MonthConvert(i)]);
    tbcMonths.Tabs.Add(rsAllData);
    tbcMonths.TabIndex := 0;
  end;
  Refresh(False);
end;

procedure TFrmStatistics.Refresh(RefreshStatus: Boolean);
begin
  if RefreshStatus = False then
    FillTheGrid;
  DrawGraph(False);
end;

procedure TFrmStatistics.FillTheGrid;
  function MonthConvert(Value: Integer): Integer;
  begin
    if Value<FHYearOrigin then
      Result := Value+(12-FHYearOrigin+1) else
      Result := Value-(FHYearOrigin-1);
    if Value=0 then
      Result := sgrdData.ColCount-1;
  end;

var
  i: Integer;
  MaxCount: Integer;
  ALine: Integer;
  RecordsCount: Integer;
  AMeanValue, ALogMeanValue: Real;
  AStandardDeviation, ALogStandardDeviation: Real;
  AThirdCentralMoment,ALogThirdCentralMoment: Real;
  AKurtosis: Real;
  LMoment1, LMoment2, LMoment3, LMoment4: Real;
  LSkewness, LKurtosis: Real;
  AParameter1, AParameter2, AParameter3: Real;
  LMomentExist: Boolean;
  AAsymetry, ALogAsymetry: Real;
  AString: string;
  AKappa, AAlpha, APsi: Real;
begin
  sgrdData.RowCount := 81;
  if FTimeStep < tstAnnual then
    sgrdData.ColCount := 14
  else
    sgrdData.ColCount := 2;
  sgrdData.DefaultColWidth := 60;
  sgrdData.ColWidths[0] := 155;
  if FTimeStep < tstAnnual then
    MaxCount := 12 else
    MaxCount := 0;
  for i := 0 to MaxCount do
  begin
    if i = 0 then
    begin
      FFullDataList.Unbiased := FUnbiased;
      RecordsCount := FFullDataList.Count;
      AMeanValue := FFullDataList.MeanValue;
      AStandardDeviation := FFullDataList.StandardDeviation;
      AThirdCentralMoment := FFullDataList.ThirdCentralMoment;
      AAsymetry := FFullDataList.Skewness;
      AKurtosis := FFullDataList.Kurtosis;
      ALogMeanValue := FFullDataList.LogMeanValue;
      ALogStandardDeviation := FFullDataList.LogStandardDeviation;
      ALogThirdCentralMoment := FFullDataList.LogThirdCentralMoment;
      ALogAsymetry := FFullDataList.LogSkewness;
      LMoment1 := FFullDataList.LMoment1;
      LMoment2 := FFullDataList.LMoment2;
      LMoment3 := FFullDataList.LMoment3;
      LMoment4 := FFullDataList.LMoment4;
      LSkewness := FFullDataList.LSkewness;
      LKurtosis := FFullDataList.LKurtosis;
      LMomentExist := FFullDataList.LMomentExist;
    end else begin
      FMonthlyDataList.Months[i].Unbiased := FUnbiased;
      RecordsCount := FMonthlyDataList.Months[i].Count;
      AMeanValue := FMonthlyDataList.Months[i].MeanValue;
      AStandardDeviation :=
        FMonthlyDataList.Months[i].StandardDeviation;
      AThirdCentralMoment :=
        FMonthlyDataList.Months[i].ThirdCentralMoment;
      AAsymetry := FMonthlyDataList.Months[i].Skewness;
      AKurtosis := FMonthlyDataList.Months[i].Kurtosis;
      ALogMeanValue := FMonthlyDataList.Months[i].LogMeanValue;
      ALogStandardDeviation :=
        FMonthlyDataList.Months[i].LogStandardDeviation;
      ALogThirdCentralMoment :=
        FMonthlyDataList.Months[i].LogThirdCentralMoment;
      ALogAsymetry := FMonthlyDataList.Months[i].LogSkewness;
      LMoment1 := FMonthlyDataList.Months[i].LMoment1;
      LMoment2 := FMonthlyDataList.Months[i].LMoment2;
      LMoment3 := FMonthlyDataList.Months[i].LMoment3;
      LMoment4 := FMonthlyDataList.Months[i].LMoment4;
      LSkewness := FMonthlyDataList.Months[i].LSkewness;
      LKurtosis := FMonthlyDataList.Months[i].LKurtosis;
      LMomentExist := FMonthlyDataList.Months[i].LMomentExist;
    end;
{Set column titles}
    if i>0 then
      sgrdData.Cells[MonthConvert(i),0] :=
        LongMonthNames[i]
    else
      sgrdData.Cells[MonthConvert(i),0] := rsAllData;
    sgrdData.Cells[0,0] := '';
    ALine := 1;
{Consider with grid lines}
    While ALine <= sgrdData.RowCount do
    begin
{Set row title only to the first iteration i=0}
      if i=0 then
        sgrdData.Cells[0,ALine] := Memo.Lines[ALine-1];
      AString :='';
      if ALine = 1 then AString := IntToStr(RecordsCount);
{Print the parameters}
      try
        if AStandardDeviation>0 then
        begin
          case ALine of
            2: AString := FormatFloat('#.##',AMeanValue);
            3: AString := FormatFloat('#.##',AStandardDeviation);
            4: AString := FormatFloat('#.##',AThirdCentralMoment);
            5: AString := FormatFloat('#.##',AAsymetry);
            6: AString := FormatFloat('#.##',AKurtosis);
            11..12:
            begin
              LogNormalParam(AMeanValue, AStandardDeviation,
                AParameter1, AParameter2);
              case ALine of
                11: AString := FormatFloat('#.##',AParameter1);
                12: AString := FormatFloat('#.##',AParameter2);
              end;
            end;
            13..15:
            begin
              GaltonParam(AMeanValue, AStandardDeviation, AAsymetry,
                AParameter1, AParameter2, AParameter3);
              case ALine of
                13: AString := FormatFloat('#.##', AParameter2);
                14: AString := FormatFloat('#.##', AParameter3);
                15: AString := FormatFloat('#.##', AParameter1);
              end;
            end;
            16..17:
            begin
              ExponentialParam(AMeanValue, AStandardDeviation,
                AParameter1, AParameter2);
              case ALine of
                16: AString := FormatFloat('#.##', AParameter1);
                17: AString := FormatFloat('#.##', AParameter2);
              end;
            end;
            18:
            AString := FormatFloat('#.##', GammaShape(AMeanValue,
              Sqr(AStandardDeviation)));
            19: AString := FormatFloat('#.##', GammaScale(AMeanValue,
              Sqr(AStandardDeviation)));
            20..22:
            begin
              PearsonIIIParam(AMeanValue, AStandardDeviation, AAsymetry,
                AParameter1, AParameter2, AParameter3);
              case ALine of
                20: AString := FormatFloat('#.##', AParameter1);
                21: AString := FormatFloat('#.##', AParameter2);
                22: AString := FormatFloat('#.##', AParameter3);
              end;
            end;
            26..27:
            begin
              EV1MaxParam(AMeanValue, AStandardDeviation,
                AParameter1, AParameter2);
              case ALine of
                26: AString := FormatFloat('#.##', AParameter1);
                27: AString := FormatFloat('#.##', AParameter2);
              end;
            end;
            28..29:
            begin
              EV2MaxParam(AmeanValue,Sqr(AStandardDeviation),AParameter1,
                AParameter2);
              case ALine of
                28: AString := FormatFloat('#.##', AParameter1);
                29: AString := FormatFloat('#.##', AParameter2);
              end;
            end;
            30..31:
            begin
              EV1MinParam(AMeanValue, AStandardDeviation,
                AParameter1, AParameter2);
              case ALine of
                30: AString := FormatFloat('#.##', AParameter1);
                31: AString := FormatFloat('#.##', AParameter2);
              end;
            end;
            32..33:
            begin
              WeibParam(AMeanValue, Sqr(AStandardDeviation), AKappa, AAlpha);
              case ALine of
                32: AString := FormatFloat('#.##',AKappa);
                33: AString := FormatFloat('#.##',AAlpha);
              end;
            end;
            34..36:
            begin
              GEVMaxParam(AMeanValue,AStandardDeviation,AAsymetry,
                AKappa, AAlpha,APsi);
              case ALine of
                34: AString := FormatFloat('#.##', AKappa);
                35: AString := FormatFloat('#.##', AAlpha);
                36: AString := FormatFloat('#.##', APsi);
              end;
            end;
            37..39:
            begin
              GEVMinParam(AMeanValue,AStandardDeviation,AAsymetry,
                AKappa, AAlpha,APsi);
              case ALine of
                37: AString := FormatFloat('#.##', AKappa);
                38: AString := FormatFloat('#.##', AAlpha);
                39: AString := FormatFloat('#.##', APsi);
              end;
            end;
            40..42:
            begin
              ParetoParam(AMeanValue,AStandardDeviation,AAsymetry,
                AKappa, AAlpha, APsi);
              case ALine of
                40: AString := FormatFloat('#.##', AKappa);
                41: AString := FormatFloat('#.##', AAlpha);
                42: AString := FormatFloat('#.##', APsi);
              end;
            end;
            70..72:
            begin
              GEVMaxParamKS(AMeanValue,AStandardDeviation,FGEVParameter,
                AAlpha,APsi);
              case ALine of
                70: AString := FormatFloat('#.##', FGEVParameter);
                71: AString := FormatFloat('#.##', AAlpha);
                72: AString := FormatFloat('#.##', APsi);
              end;
            end;
            73..75:
            begin
              GEVMinParamKS(AMeanValue,AStandardDeviation,FGEVParameter,
                AAlpha,APsi);
              case ALine of
                73: AString := FormatFloat('#.##', FGEVParameter);
                74: AString := FormatFloat('#.##', AAlpha);
                75: AString := FormatFloat('#.##', APsi);
              end;
            end;
          end;
        end;
        if ALogStandardDeviation>0 then
        begin
          case ALine of
            7: AString := FormatFloat('#.##',ALogMeanValue);
            8: AString := FormatFloat('#.##',ALogStandardDeviation);
            9: AString := FormatFloat('#.##',ALogThirdCentralMoment);
            10: AString := FormatFloat('#.##',ALogAsymetry);
            23..25:
            begin
              PearsonIIIParam(ALogMeanValue, ALogStandardDeviation, ALogAsymetry,
                AParameter1, AParameter2, AParameter3);
              case ALine of
                23: AString := FormatFloat('#.##', AParameter1);
                24: AString := FormatFloat('#.##', AParameter2);
                25: AString := FormatFloat('#.##', AParameter3);
              end;
            end;
          end;
        end;
        if LMomentExist then
        begin
          case ALine of
            43: AString := FormatFloat('#.##', LMoment1);
            44: AString := FormatFloat('#.##', LMoment2);
            45: AString := FormatFloat('#.##', LMoment3);
            46: AString := FormatFloat('#.##', LMoment4);
            47: AString := FormatFloat('#.##', LSkewness);
            48: AString := FormatFloat('#.##', LKurtosis);
            49..50:
            begin
              NormalLParam(LMoment1, LMoment2, LMoment3, AParameter1,
                AParameter2);
              case ALine of
                49: AString := FormatFloat('#.##', AParameter1);
                50: AString := FormatFloat('#.##', AParameter2);
              end;
            end;
            51..52:
            begin
              ExponentialLParam(LMoment1, LMoment2, LMoment3,
                AParameter1, AParameter2);
              case ALine of
                51: AString := FormatFloat('#.##', AParameter1);
                52: AString := FormatFloat('#.##', AParameter2);
              end;
            end;
            61..63:
            begin
              GEVMaxLParam(LMoment1,LMoment2,LMoment3,
                AKappa, AAlpha,APsi);
              case ALine of
                61: AString := FormatFloat('#.##', AKappa);
                62: AString := FormatFloat('#.##', AAlpha);
                63: AString := FormatFloat('#.##', APsi);
              end;
            end;
            64..66:
            begin
              GEVMinLParam(LMoment1,LMoment2,LMoment3,
                AKappa, AAlpha,APsi);
              case ALine of
                64: AString := FormatFloat('#.##', AKappa);
                65: AString := FormatFloat('#.##', AAlpha);
                66: AString := FormatFloat('#.##', APsi);
              end;
            end;

            76..78:
            begin
              GEVMaxLParamKS(LMoment1,LMoment2,LMoment3,
                FGEVParameter, AAlpha,APsi);
              case ALine of
                76: AString := FormatFloat('#.##', FGEVParameter);
                77: AString := FormatFloat('#.##', AAlpha);
                78: AString := FormatFloat('#.##', APsi);
              end;
            end;
            79..81:
            begin
              GEVMinLParamKS(LMoment1,LMoment2,LMoment3,
                FGEVParameter, AAlpha,APsi);
              case ALine of
                79: AString := FormatFloat('#.##', FGEVParameter);
                80: AString := FormatFloat('#.##', AAlpha);
                81: AString := FormatFloat('#.##', APsi);
              end;
            end;
            67..69:
            begin
              ParetoLParam(LMoment1,LMoment2,LMoment3,
                AKappa, AAlpha,APsi);
              case ALine of
                67: AString := FormatFloat('#.##', AKappa);
                68: AString := FormatFloat('#.##', AAlpha);
                69: AString := FormatFloat('#.##', APsi);
              end;
            end;
            53..54:
            begin
              EV1MaxLParam(LMoment1, LMoment2, LMoment3,
                AParameter1, AParameter2);
              case ALine of
                53: AString := FormatFloat('#.##', AParameter1);
                54: AString := FormatFloat('#.##', AParameter2);
              end;
            end;
            55..56:
            begin
              EV2MaxLParam(LMoment1, LMoment2, LMoment3, AKappa, AAlpha);
              case ALine of
                55: AString := FormatFloat('#.##', AKappa);
                56: AString := FormatFloat('#.##', AALpha);
              end;
            end;
            57..58:
            begin
              EV1MinLParam(LMoment1, LMoment2, LMoment3,
                AParameter1, AParameter2);
              case ALine of
                57: AString := FormatFloat('#.##', AParameter1);
                58: AString := FormatFloat('#.##', AParameter2);
              end;
            end;
            59..60:
            begin
              WeibLParam(LMoment1, LMoment2, LMoment3, AKappa, AAlpha);
              case ALine of
                59: AString := FormatFloat('#.##', AKappa);
                60: AString := FormatFloat('#.##', AALpha);
              end;
            end;
          end;
        end;
      except
        on EMathError do
          AString := '';
        else
          raise;
      end;
      sgrdData.Cells[MonthConvert(i),ALine] :=
        AString;
      Inc(ALine);
    end; {While ALine < 5 do }
  end; { for i := 0 to MaxCount do }

end;

procedure TFrmStatistics.FormShow(Sender: TObject);
begin
  sgrdData.DefaultRowHeight := sgrdData.DefaultRowHeight *
    Screen.PixelsPerInch div 96;
  tbcMonths.TabWidth := tbcMonths.TabWidth * Screen.PixelsPerInch div 96;
end;

procedure TFrmStatistics.DrawGraph(DrawStatus: Boolean);
var
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    if DrawStatus = False then
    begin
      PrepareGraphSeries;
      DrawGrid;
    end;
    ShowSeries;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmStatistics.PrepareGraphSeries;
var
  ALogAxis: Boolean;
begin
{GEV Limits, must be recalculated form different parameter values}
  if FPaperType = pdptGEVMax then
  begin
    try
      FPaperMinZ[pdptGEVMax] := ProbabilityPaper(0.9995,pdptGEVMax,FGEVParameter);
      FPaperMaxZ[pdptGEVMax] := ProbabilityPaper(0.0002,pdptGEVMax,FGEVParameter);
    except
      on EMathError do
      begin
        FPaperMinZ[pdptGEVMax] := -2.0;
        FPaperMaxZ[pdptGEVMax] := 8.0;
      end;
      else
        raise;
    end;
  end;
  if FPaperType = pdptGEVMin then
  begin
    try
      FPaperMinZ[pdptGEVMin] := ProbabilityPaper(0.9995,pdptGEVMin,FGEVParameter);
      FPaperMaxZ[pdptGEVMin] := ProbabilityPaper(0.0002,pdptGEVMin,FGEVParameter);
    except
      on EMathError do
      begin
        FPaperMinZ[pdptGEVMin] := -5.0;
        FPaperMaxZ[pdptGEVMin] := 2.0;
      end;
      else
        raise;
    end;
  end;
{Prepare Axes Limits}
  ALogAxis := btnLog.Checked;
  if ALogAxis then FAutoLeftAxisMin := False;
  mnuAutoLeftAxis.Checked := FAutoLeftAxisMin;
  if ALogAxis then
  begin
{If log axis, try to consider min positive value}
    if FMonthShowed = 0 then
      FPaperMinX := FFullDataList.FirstPositiveValue
    else
      FPaperMinX := FMonthlyDataList.Months[FMonthShowed].FirstPositiveValue;
    if FPaperMinX >0 then
      FPaperMinX := Power(10, Floor(Log10(FPaperMinX)))
    else
      FPaperMinX := 1;
    if FPaperMinX>1 then
      FPaperMinX := 1;
  end else {if ALog Axis, else... set MinX to 0}
    FPaperMinX := 0;
{Set MaxX respectively the timestep (monthly (=month) or yearly (=0)) }
  if FMonthShowed = 0 then
    if FFullDataList.Count >0 then
      FPaperMaxX := 1.33* FFullDataList.First.Value
    else
      FPaperMaxX := 100
  else
    if FMonthlyDataList.Months[FMonthShowed].Count > 0 then
      FPaperMaxX := 1.33*  FMonthlyDataList.Months[FMonthShowed].First.Value
    else
      FPaperMaxX := 100;
  if FSetPaperMaxX>0 then
    FPaperMaxX := FSetPaperMaxX;
{Zero SetPaperMaxX so it is calculated programatically next time}
  FSetPaperMaxX := 0;
{A dummy solution to min < max in every case}
  Chart.LeftAxis.Logarithmic := False;
  Chart.LeftAxis.AutomaticMinimum := FAutoLeftAxisMin;
  if not FAutoLeftAxisMin then
    Chart.LeftAxis.Minimum := -1000000000;
  Chart.LeftAxis.Maximum := 1000000000;
  Chart.BottomAxis.Minimum := -100000000;
  Chart.BottomAxis.Maximum := 1000000000;
  Chart.TopAxis.Minimum := -100000000;
  Chart.TopAxis.Maximum := 1000000000;
  if ALogAxis then
    FPaperMaxX := Power(10.000,(Floor(Log10(FPaperMaxX))+1));
  if not FAutoLeftAxisMin then
    Chart.LeftAxis.Minimum := FPaperMinX;
  Chart.LeftAxis.Maximum := FPaperMaxX;
  Chart.BottomAxis.Minimum := FPaperMinZ[FPaperType];
  Chart.BottomAxis.Maximum := FPaperMaxZ[FPaperType];
  Chart.TopAxis.Minimum := FPaperMinZ[FPaperType];
  Chart.TopAxis.Maximum := FPaperMaxZ[FPaperType];
  Chart.LeftAxis.Logarithmic := ALogAxis;
  if ALogAxis then
    Chart.LeftAxis.MinorTickCount := 9
  else
    Chart.LeftAxis.MinorTickCount := 1;
  chartPDF.BottomAxis.Logarithmic := ALogAxis;
  if ALogAxis then
  begin
    ChartPDF.BottomAxis.MinorGrid.Visible := True;
    ChartPDF.BottomAxis.MinorTickCount := 9
  end else begin
    ChartPDF.BottomAxis.MinorGrid.Visible := False;
    ChartPDF.BottomAxis.MinorTickCount := 4;
  end;
  PreparePointSeries;
  PrepareHistogram;
  PrepareLineSeries;
end;

resourcestring
  rsSampleConsistingOf = 'Sample consisting from values of the month: ';
  rsPDFTitle = 'Probability Density Functions (PDF) - Histogram';

procedure TFrmStatistics.PrepareLineSeries;
var
  i: Integer;
  j: Integer;
  ADataList: TDataList;
  AStandardDeviation, ALogStandardDeviation: Real;
  AXValue: Real;
  AZValue: Real;
  LMomentExist: Boolean;
  ADistribution: TStatisticalDistribution;
  ActualMinX: Real;
begin
  if not FAutoLeftAxisMin then
    ActualMinX := FPaperMinX else
    ActualMinX := Min(FPaperMinX, WeibullPoints.MinYValue);
{Get statistical values}
  if FMonthShowed = 0 then
  begin
    FFullDataList.Unbiased := FUnbiased;
    AStandardDeviation := FFullDataList.StandardDeviation;
    ALogStandardDeviation := FFullDataList.LogStandardDeviation;
    LMomentExist := FFullDataList.LMomentExist;
    ADataList := TDataList(FFullDataList);
    chartPDF.Title.Text.Text := rsPDFTitle;
  end else begin
    FMonthlyDataList.Months[FMonthShowed].Unbiased := FUnbiased;
    AStandardDeviation :=
      FMonthlyDataList.Months[FMonthShowed].StandardDeviation;
    ALogStandardDeviation :=
      FMonthlyDataList.Months[FMonthShowed].LogStandardDeviation;
    LMomentExist := FMonthlyDataList.Months[FMonthShowed].LMomentExist;
    ADataList := TDataList(FMonthlyDataList.Months[FMonthShowed]);
    chartPDF.Title.Text.Text := rsPDFTitle + ' - ' + rsSampleConsistingOf +
      LongMonthNames[FMonthShowed];
  end;
{Do not plot if StdDev <= 0, occured by exception}
  if (AStandardDeviation <= 0) or (ALogStandardDeviation <= 0) then
    Exit;
  for j := 0 to 26 do
  begin
    ADistribution := nil;
    Chart.SeriesList[j+5].Clear;
    ChartPDF.SeriesList[j+1].Clear;
    if ADataList.Count<3 then
      Continue;
    try
      try
        ADistribution := TStatisticalDistribution.Create(pl_Distributions[j],
          ADataList, FGEVParameter);
        ADistribution.SetGEVShape(FGEVParameter);
      except
        on EMathError do
          Continue;
        else
          raise;
      end;
      try
        for i := 0 to 120 do
        begin
          AXValue := (FPaperMaxX-ActualMinX)*i/120+ActualMinX;
{Scale X Values for logarithmic, make the suitable for log. paper}
          if btnLog.Checked then
            AXValue := (Power(10, (AXValue-ActualMinX)/(FPaperMaxX-ActualMinX) )-1)*
              (FPaperMaxX-ActualMinX)/(9)+ActualMinX;
          if (not FAutoLeftAxisMin) and (AXValue <0) then
            Continue;
          if ADistribution.IsLowerBounded then
            if AXValue <= ADistribution.MinX then
              Continue;
          if ADistribution.IsUpperBounded then
            if AXValue >= ADistribution.MaxX then
              Continue;
          if ADistribution.IsLMomentMethod then
            if not LMomentExist then
              Continue;
          AZValue := 1-ADistribution.cdfValue(AXValue);
{Do this, to avoid numerical errors for Extreme XValues}
          if (AZValue>=FMinProbability) and (AZValue<=FMaxProbability) then
          begin
{Linearize...}
            AZValue := ProbabilityPaper(AZValue, FPaperType, FGEVParameter);
            if (AZValue >= FPaperMinZ[FPaperType]) and
              (AZValue <= FPaperMaxZ[FPaperType]) then
            Chart.Series[j+5].AddXY(AZValue,AXValue,'',
              Chart.Series[j+5].SeriesColor);
          end;
        end; {i := 0..120}
      except
        on EMathError do
          Continue;
        else
          raise;
      end;
{Prepare Histogram}
      try
        for i := 0 to 119 do
        begin
          AXValue := (FPaperMaxX-ActualMinX)*(i+0.5)/120+ActualMinX;
{Scale X Values for logarithmic, make the suitable for log. paper}
          if btnLog.Checked then
            AXValue := (Power(10, (AXValue-ActualMinX)/(FPaperMaxX-ActualMinX) )-1)*
              (FPaperMaxX-ActualMinX)/(9)+ActualMinX;
          if ADistribution.IsLowerBounded then
            if AXValue <= ADistribution.MinX+0.05 then
              Continue;
          if ADistribution.IsUpperBounded then
            if AXValue >= ADistribution.MaxX-0.05 then
              Continue;
          if ADistribution.IsLMomentMethod then
            if not LMomentExist then
              Continue;
          AZValue := (ADistribution.cdfValue(AXValue+0.05)-
            ADistribution.cdfValue(AXValue-0.05))/0.10;
          ChartPDF.Series[j+1].AddXY(AXValue,AZValue,'',
            ChartPDF.Series[j+1].SeriesColor);
        end; {i := 0..119}
      except
        on EMathError do
          Continue;
        else
          raise;
      end;
    finally
      ADistribution.Free;
    end;
  end;
end;

resourcestring
  rsPointLabel =
    'Date: %s'+#13#10+'Value: %.3f'+#13#10+'Probability function: %.4f';

procedure TFrmStatistics.PreparePointSeries;

  procedure AddPoint(APointSeries: TPointSeries; AValue, AProbability: Real;
    ADateTime: TDateTime);
  var
    ADateStr: string;
  begin
    ADateStr := DateTimeToStr(ADateTime);
    APointSeries.AddXY(
      ProbabilityPaper(AProbability,FPaperType,FGEVParameter), AValue,
      Format(rsPointLabel,[ADateStr, AValue, 1-AProbability]),clDefault);
  end;

var
  i: Integer;
  AFloat: Real;
  ADateStr: string;
  RecordsCount: Integer;
  AProbabilityRecord: TProbabilityRecord;
begin
{Clear point series}
  WeibullPoints.Clear;
  BlomPoints.Clear;
  CunnanePoints.Clear;
  GringortenPoints.Clear;
{Get Points Count}
  if FMonthShowed = 0 then
  begin
    RecordsCount := FFullDataList.Count;
{Recalculate Weibull. Do this because the values of
fulldata list and monthly data lists are stored to
the same records}
    FFullDataList.CalcWeibull;
  end else begin
    RecordsCount := FMonthlyDataList.Months[FMonthShowed].Count;
    FMonthlyDataList.Months[FMonthShowed].CalcWeibull;
  end;
{Plot Count points}
  for i := 0 to RecordsCount-1 do
  begin
    if FMonthShowed = 0 then
      AProbabilityRecord := FFullDataList[i]
    else
      AProbabilityRecord := FMonthlyDataList.Months[FMonthShowed][i];
    AFloat := AProbabilityRecord.WeibullProbability;
    ADateStr := DateTimeToStr(AProbabilityRecord.Date);
    AddPoint(WeibullPoints, AProbabilityRecord.Value, AFloat,
      AProbabilityRecord.Date);
    AFloat := AProbabilityRecord.BlomProbability;
    AddPoint(BlomPoints, AProbabilityRecord.Value, AFloat,
      AProbabilityRecord.Date);
    AFloat := AProbabilityRecord.CunnaneProbability;
    AddPoint(CunnanePoints, AProbabilityRecord.Value, AFloat,
      AProbabilityRecord.Date);
    AFloat := AProbabilityRecord.GringortenProbability;
    AddPoint(GringortenPoints, AProbabilityRecord.Value, AFloat,
      AProbabilityRecord.Date);
  end;
end;

procedure TFrmStatistics.PrepareHistogram;
var
  AIndex, i, RecordsCount, HClassesCount: Integer;
  AValue, AWidth, XMax, XMin, AScale: Real;
  HClasses: array of Integer;
begin
  seriesHistogram.Clear;
  if FMonthShowed = 0 then
  begin
    RecordsCount := FFullDataList.Count;
    if RecordsCount<1 then Exit;
    XMax := FFullDataList[0].Value;
    XMin := FFullDataList[RecordsCount-1].Value;
  end else begin
    RecordsCount := FMonthlyDataList.Months[FMonthShowed].Count;
    if RecordsCount<1 then Exit;
    XMax := FMonthlyDataList.Months[FMonthShowed][0].Value;
    XMin := FMonthlyDataList.Months[FMonthShowed][RecordsCount-1].Value;
  end;
  if Abs(XMax-XMin)<1e-10 then Exit;
  if RecordsCount<2 then Exit;
  HClassesCount := Ceil(Sqrt(RecordsCount))+1;
  SetLength(HClasses, HClassesCount);
  AWidth := (XMax-XMin)/HClassesCount;
  for i := 0 to HClassesCount-1 do
    HClasses[i] := 0;
  for i := 0 to RecordsCount-1 do
  begin
    if FMonthShowed = 0 then
      AValue := FFullDataList[i].Value
    else
      AValue := FMonthlyDataList.Months[FMonthShowed][i].Value;
    AIndex := Floor((AValue - XMin)/AWidth);
    if AIndex = HClassesCount then Dec(AIndex);
    Inc(HClasses[AIndex]);
  end;
  AScale := RecordsCount*AWidth;
  for i := 0 to HClassesCount-1 do
    seriesHistogram.AddXY(XMin+AWidth*0.5+AWidth*i, HClasses[i]/AScale, '',
      clDefault);
end;

resourcestring
  rsExceedanceProbability = 'Exceedance probability (%)';
  rsProbabilityFunction = 'Probability function (%)';
  rsReturnPeriodMax = 'Return period (T) for Maximum values in years';
  rsReturnPeriodMin = 'Return period (T) for Minimum values in years';
  rsPaperLinear = ' - scale: Linear';
  rsPaperNormal = ' - scale: Normal distribution';
  rsPaperGumbelMax = ' - scale: Gumbel (Max) distribution';
  rsPaperGumbelMin = ' - scale: Gumbel (Min) distribution';
  rsPaperWeibull = ' - scale: Weibull distribution';
  rsPaperGEV = ' - scale: GEV (Max) distribution';
  rsPaperGEVMin = ' - scale: GEV (Min) distribution';

procedure TFrmStatistics.DrawGrid;
var
  i: Integer;
  ALabel: string;
begin
  Chart.LeftAxis.Title.Caption := FFullDataList.MUnit;
  if mnuExceedanceProbability.Checked then
    Chart.TopAxis.Title.Caption := rsExceedanceProbability;
  if mnuProbabilityFunction.Checked then
    Chart.TopAxis.Title.Caption := rsProbabilityFunction;
  if mnuReturnPeriodMax.Checked then
    Chart.TopAxis.Title.Caption := rsReturnPeriodMax;
  if mnuReturnPeriodMin.Checked then
    Chart.TopAxis.Title.Caption := rsReturnPeriodMin;
  case FPaperType of
    pdptLinear:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperLinear;
    pdptNormal:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperNormal;
    pdptGumbelMax:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperGumbelMax;
    pdptGumbelMin:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperGumbelMin;
    pdptWeibull:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperWeibull;
    pdptGEVMax:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperGEV;
    pdptGEVMin:
      Chart.TopAxis.Title.Caption := Chart.TopAxis.Title.Caption +
        rsPaperGEVMin;
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
{Point Marks}
        if mnuExceedanceProbability.Checked then
          ALabel := FormatFloat('#.##',100*FProbabilityList[i])+'%';
        if mnuProbabilityFunction.Checked then
          ALabel := FormatFloat('#.##',100*(1-FProbabilityList[i]))+'%';
        if mnuReturnPeriodMax.Checked then
          ALabel := FormatFloat('#.##',1/FProbabilityList[i]);
        if mnuReturnPeriodMin.Checked then
          ALabel := FormatFloat('#.##',1/(1-FProbabilityList[i]));
        GridPoint.AddXY(ProbabilityPaper(FProbabilityList[i],
          FPapertype,FGEVParameter), FPaperMaxX*0.5, ALabel, clNone);
      end;
    except
      on EMathError do
        Continue;
      else
        raise;
    end;
  end;
end;

procedure TFrmStatistics.Showhistogram1Click(Sender: TObject);
begin
  seriesHistogram.Visible := (Sender as TMenuItem).Checked;
end;

procedure TFrmStatistics.ShowSeries;
begin
{Sometimes exception occurs when drawing lines, so use a
 try..finally statements to show points in almost every case}
  try
    DrawLines;
  finally
    DrawPoints;
  end;
end;

procedure TFrmStatistics.DrawLines;
var
  i: Integer;
begin
  HighSampleLimitLine.Active := False;
  LowSampleLimitLine.Active := False;
  HighConfidenceLimitLine.Active := False;
  LowConfidenceLimitLine.Active := False;
  for i := 0 to lstDistributions.Count-1 do
    (lstDistributions.Items.Objects[i] as TLineSeries).Active :=
      lstDistributions.Selected[i];
  for i := 1 to ChartPDF.SeriesCount-1 do
    ChartPDF.Series[i].Active := Chart.Series[i+4].Active;
end;

procedure TFrmStatistics.DrawPoints;
begin
{Get check boxes status, then draw}
  WeibullPoints.Active := btnWeibullPoints.Checked;
  BlomPoints.Active := btnBlomPoints.Checked;
  CunnanePoints.Active := btnCunnanePoints.Checked;
  GringortenPoints.Active := btnGringortenPoints.Checked;
end;

{Reset switch [[set all checkboxes to false]]}

procedure TFrmStatistics.ResetAllButtons;
var
  i: Integer;
begin
  for i := 0 to lstDistributions.Count-1 do
    lstDistributions.Selected[i] := False;
  lstDistributions.Selected[0] := True;
  lstDistributions.Selected[4] := True;
  DrawGraph(True);
end;

{Confidence intervals - direct menu iteraction}

procedure TFrmStatistics.mnuAllowZoomAndPanClick(Sender: TObject);
begin
  mnuAllowZoomAndPan.Checked := not mnuAllowZoomAndPan.Checked;
  FAllowZoomAndPan := mnuAllowZoomAndPan.Checked;
  Chart.AllowZoom := FAllowZoomAndPan;
  if FAllowZoomAndPan then
    Chart.AllowPanning := pmBoth else
    Chart.AllowPanning := pmNone;
end;

procedure TFrmStatistics.mnuAutoLeftAxisClick(Sender: TObject);
var
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    mnuAutoLeftAxis.Checked := not mnuAutoLeftAxis.Checked;
    FAutoLeftAxisMin := mnuAutoLeftAxis.Checked;
    Refresh(True);
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmStatistics.mnuConfidenceClick(Sender: TObject);
var
  i: Integer;
  CDFCode: Integer;
  ADistributionType: TStatisticalDistributionType;
begin
{Read the distribution type from menu tag property}
  CDFCode := TMenuItem(Sender).Tag;
{Uncheck all distribution check buttons}
  ResetAllButtons;
{Activate the appropriate distribution line}
  for i := 0 to lstDistributions.Count-1 do
    if  i = (CDFCode-1) then
      lstDistributions.Selected[i] := True else
      lstDistributions.Selected[i] := False;
  DrawGraph(True);
  ADistributionType := sdtNormal;
  case CDFCode of
    1: ADistributionType := sdtNormal;
    2: ADistributionType := sdtLogNormal;
    3: ADistributionType := sdtGalton;
    4: ADistributionType := sdtExponential;
    5: ADistributionType := sdtGamma;
    6: ADistributionType := sdtPearsonIII;
    7: ADistributionType := sdtLogPearsonIII;
    8: ADistributionType := sdtEV1Max;
    9: ADistributionType := sdtEV2Max;
    10: ADistributionType := sdtEV1Min;
    11: ADistributionType := sdtEV3Min;
    12: ADistributionType := sdtGEVMax;
    13: ADistributionType := sdtGEVMin;
    14: ADistributionType := sdtPareto;
    15: ADistributionType := sdtLNormal;
    16: ADistributionType := sdtLExponential;
    17: ADistributionType :=  sdtLEV1Max;
    18: ADistributionType := sdtLEV2Max;
    19: ADistributionType := sdtLEV1Min;
    20: ADistributionType := sdtLEV3Min;
    21: ADistributionType := sdtLGEVMax;
    22: ADistributionType := sdtLGEVMin;
    23: ADistributionType := sdtLPareto;
    24: ADistributionType := sdtGEVMaxK;
    25: ADistributionType := sdtGEVMinK;
    26: ADistributionType := sdtLGEVMaxK;
    27: ADistributionType := sdtLGEVMinK;
  else
    Assert(False);
  end;
  DoMonteCarloSimul(ADistributionType);
end;

resourcestring
  rsConfidenceIntervalLimits = 'Confidence interval limits ';
  rsSampleLimits = 'Sample limits ';
  rsAtLeastFiveValues = 'A sample containing at least five values is required'+
    ' in order to calculate confindence limits';
  rsLMomentsUndefined = 'L moments not defined';

procedure TFrmStatistics.DoMonteCarloSimul(ADistributionType: TStatisticalDistributionType);
var
  i, j: Integer;
  AFValue, AZValue, APrevFValue: Real;
  Upper, Lower: Real;
  TrueUpper, TrueLower: Real;
  MCCount: Integer;
  MCPointsCount: Integer;
  MCConfidenceLevel: Real;
  MaxXValue, MinXValue: Real;
  ADistribution: TStatisticalDistribution;
  ADataList: TDataList;
begin
{Set the basic parameters from the settings form}
  MCCount := 60000;
  case SettingMCCount of
    0: MCCount := 30000;
    1: MCCount := 60000;
    2: MCCount := 120000;
    else Assert(False);
  end;
  MCPointsCount := 10;
  case SettingMCPointsCount of
    0: MCPointsCount := 5;
    1: MCPointsCount := 10;
    2: MCPointsCount := 20;
    else Assert(False);
  end;
  MCConfidenceLevel := 0.950;
  case SettingMCConfidence of
    0: MCConfidenceLevel := 0.900;
    1: MCConfidenceLevel := 0.950;
    2: MCConfidenceLevel := 0.990;
    3: MCConfidenceLevel := 0.999;
    else Assert(False);
  end;
  if FMonthShowed = 0 then
  begin
    FFullDataList.Unbiased := FUnbiased;
    ADataList := TDataList (FFullDataList);
  end else begin
    FMonthlyDataList.Months[FMonthShowed].Unbiased := FUnbiased;
    ADataList := FMonthlyDataList.Months[FMonthShowed];
  end;
  if ADataList.Count < 5 then
    raise EMathError.Create(rsAtLeastFiveValues);
{Dummy initialization}
  MinXValue := 0;
  MaxXValue := 1;
{Set the appropriate min and max values.
 Loop will never reach last series (confidence lines)
 and this is the wanted behaviour}
  for i := 5 to Chart.SeriesCount-1 do
  begin
    if Chart.Series[i].Active then
    begin
      MaxXValue := Chart.Series[i].MaxXValue;
      MinXValue := Chart.Series[i].MinXValue;
      Break;
    end;
  end;
{Initialize confidence lines, set the Legent titles}
  HighSampleLimitLine.Clear;
  LowSampleLimitLine.Clear;
  HighConfidenceLimitLine.Clear;
  LowConfidenceLimitLine.Clear;
  with chartPDF do
    for i := SeriesCount-1 downto SeriesCount-4 do
    begin
      Series[i].Clear;
      Series[i].Active := True;
    end;
  LowConfidenceLimitLine.Title := rsConfidenceIntervalLimits +
    FormatFloat('#.##',MCConfidenceLevel*100)+'%';
  LowSampleLimitLine.Title := rsSampleLimits +
    FormatFloat('#.##',MCConfidenceLevel*100)+'%';
  with chartPDF do Series[SeriesCount-4].Title := LowSampleLimitLine.Title;
  with chartPDF do Series[SeriesCount-1].Title := LowConfidenceLimitLine.Title;
  HighSampleLimitLine.Active := True;
  LowSampleLimitLine.Active := True;
  HighConfidenceLimitLine.Active := True;
  LowConfidenceLimitLine.Active := True;
  RandSeed := 0;
  ADistribution := nil;
  try try
    ADistribution := TStatisticalDistribution.Create(ADistributionType,
      ADataList, FGEVParameter);
    if ADistribution.IsLMomentMethod then
      if not ADataList.LMomentExist then
        raise EMathError.Create(rsLMomentsUndefined);
{ Deactivate related forms to avoid fatal exceptions when the
  user query close forms}
    Enabled := False;
{ Initialize progress indicator}
    FrmProcessingDialog.Initialize( MCPointsCount*
      (MCCount div 100)*(ADistribution.ParameterCount+1));
    FrmProcessingDialog.Show;
{ Loop for MCPointsCount points}
    for i := 0 to MCPointsCount-1 do
    begin
{ Get a graph value (linear)}
      AZValue := (i/(MCPointsCount-1))*(MaxXValue-MinXValue)+MinXValue;
{ Get the F value (Distribution function value)}
      AFValue := InvProbabilityPaper(AZValue, FPaperType,FGEVParameter);
      Application.ProcessMessages;
      MonteCarloLimits(AFValue, ADistribution,  MCCount,
        MCConfidenceLevel, Upper, Lower, TrueUpper, TrueLower,
        MCLimitsProgress);
      HighConfidenceLimitLine.AddXY(AZValue,TrueUpper,'',
        HighConfidenceLimitLine.SeriesColor);
      LowConfidenceLimitLine.AddXY(AZValue,TrueLower,'',
        LowConfidenceLimitLine.SeriesColor);
      HighSampleLimitLine.AddXY(AZValue,Upper,'',
        HighSampleLimitLine.SeriesColor);
      LowSampleLimitLine.AddXY(AZValue,Lower,'',LowSampleLimitLine.SeriesColor);
      if i <> 0 then
      begin
        for j := 1 to 4 do
          with chart do
            if Abs(Series[SeriesCount-j].YValue[i]-
                  Series[SeriesCount-j].YValue[i-1])>1e-6 then
              ChartPDF.Series[ChartPDF.SeriesCount-j].AddXY(
                0.5*(Series[SeriesCount-j].YValue[i]+
                  Series[SeriesCount-j].YValue[i-1]),
                (AFValue-APrevFValue)/
                  (Series[SeriesCount-j].YValue[i]-
                    Series[SeriesCount-j].YValue[i-1]),'',clDefault);
      end;
      APrevFValue := AFValue;
    end;
  except
    for i := 1 to 4 do
    begin
      with Chart do Series[SeriesCount-i].Clear;
      with Chart do Series[SeriesCount-i].Active := False;
      with ChartPDF do Series[SeriesCount-i].Clear;
      with ChartPDF do Series[SeriesCount-i].Active := False;
    end;
    raise;
  end;
  finally
    Enabled := True;
    FrmProcessingDialog.Hide;
    ADistribution.Free;
  end;
end;

procedure TFrmStatistics.MCLimitsProgress(var Stop: Boolean);
begin
  Stop := FrmProcessingDialog.ToStopPlease;
  FrmProcessingDialog.StepOne;
end;

resourcestring
  rsInOrderToGetForecastForAConfidence = 'In order to get forecast for a'+
    'confidence interval, you may first draw a set of Upper-Lower limit lines';
  rsInterpolatedValue = 'Interpolated';
  rsUpperWord = 'Upper ';
  rsLowerWord = 'Lower ';
  rsPointWord = 'Point ';

procedure TFrmStatistics.mnuConfidenceForecastsClick(Sender: TObject);
var
  i, AGridRow: Integer;
  ALineSeries: TLineSeries;
  AFValue: Real;
  AString: string;
  XValues, YValues: TArrayOfReal;
begin
  if not LowSampleLimitLine.Active then
    raise Exception.Create(rsInOrderToGetForecastForAConfidence);
  ALineSeries := nil;
  pgcPages.ActivePage := tbcParameters;
  for i := 5 to Chart.SeriesCount-1 do
  begin
    if Chart.Series[i].Active then
    begin
      ALineSeries := TLineSeries(Chart.Series[i]);
      Break;
    end;
  end;
  AGridRow := 1;
  sgrdData.RowCount := 2;
  sgrdData.ColCount := 8;
  sgrdData.DefaultColWidth := 50;
  sgrdData.ColWidths[0] := 120;
  sgrdData.Cells[0,0] := ALineSeries.Title;
  sgrdData.Cells[1,0] := 'F';
  sgrdData.Cells[2,0] := 'F1';
  sgrdData.Cells[3,0] := rsValueWord;
  sgrdData.ColWidths[4] := 120;
  sgrdData.Cells[4,0] := rsUpperWord+LowSampleLimitLine.Title;
  sgrdData.ColWidths[5] := 120;
  sgrdData.Cells[5,0] := rsLowerWord+LowSampleLimitLine.Title;
  sgrdData.ColWidths[6] := 180;
  sgrdData.Cells[6,0] := rsUpperWord+LowConfidenceLimitLine.Title;
  sgrdData.ColWidths[7] := 180;
  sgrdData.Cells[7,0] := rsLowerWord+LowConfidenceLimitLine.Title;
  try
    for i := 0 to LowSampleLimitLine.Count-1 do
    begin
      sgrdData.RowCount := AGridRow+1;
      AFValue := InvProbabilityPaper(LowSampleLimitLine.XValues[i],
        FPaperType,FGEVParameter);
      sgrdData.Cells[0, AGridRow] := rsPointWord+IntToStr(i+1);
      sgrdData.Cells[1, AGridRow] := FormatFloat('#.##',AFValue*100)+'%';
      sgrdData.Cells[2, AGridRow] := FormatFloat('#.##',100-AFValue*100)+'%';
      SetLength(XValues, ALineSeries.Count);
      SetLength(YValues, ALineSeries.Count);
      XValues := ChartSeries2XValues(ALineSeries);
      YValues := ChartSeries2YValues(ALineSeries);
      sgrdData.Cells[3, AGridRow] := FormatFloat('#.##',
        ProbabilityLinearInterpolate(XValues, YValues, 1-AFValue, FPaperType,
          FGEVParameter));
      sgrdData.Cells[4, AGridRow] := FormatFloat('#.##',
        HighSampleLimitLine.YValues[i]);
      sgrdData.Cells[5, AGridRow] := FormatFloat('#.##',
        LowSampleLimitLine.YValues[i]);
      sgrdData.Cells[6, AGridRow] := FormatFloat('#.##',
        HighConfidenceLimitLine.YValues[i]);
      sgrdData.Cells[7, AGridRow] := FormatFloat('#.##',
        LowConfidenceLimitLine.YValues[i]);
      Inc(AGridRow);
    end;
    AString := '50';
    if not InputQuery(rsEnterProbabilityValue,
      rsEnterProbabilityValue, AString) then Exit;
    AFValue:= StrToFloat( AString );
    AFValue := AFValue / 100;
    sgrdData.RowCount := AGridRow+1;
    sgrdData.Cells[0, AGridRow] := rsInterpolatedValue;
    sgrdData.Cells[1, AGridRow] := FormatFloat('#.##',AFValue*100)+'%';
    sgrdData.Cells[2, AGridRow] := FormatFloat('#.##',100-AFValue*100)+'%';
    SetLength(XValues, ALineSeries.Count);
    SetLength(YValues, ALineSeries.Count);
    XValues := ChartSeries2XValues(ALineSeries);
    YValues := ChartSeries2YValues(ALineSeries);
    sgrdData.Cells[3, AGridRow] := FormatFloat('#.##',
      ProbabilityLinearInterpolate(XValues, YValues, 1-AFValue,
        FPaperType, FGEVParameter));
    SetLength(XValues, HighSampleLimitLine.Count);
    SetLength(YValues, HighSampleLimitLine.Count);
    XValues := ChartSeries2XValues(HighSampleLimitLine);
    YValues := ChartSeries2YValues(HighSampleLimitLine);
    sgrdData.Cells[4, AGridRow] := FormatFloat('#.##',
      ProbabilityLinearInterpolate(XValues, YValues, 1-AFValue,
        FPaperType, FGEVParameter));
    SetLength(XValues, LowSampleLimitLine.Count);
    SetLength(YValues, LowSampleLimitLine.Count);
    XValues := ChartSeries2XValues(LowSampleLimitLine);
    YValues := ChartSeries2YValues(LowSampleLimitLine);
    sgrdData.Cells[5, AGridRow] := FormatFloat('#.##',
      ProbabilityLinearInterpolate(XValues, YValues, 1-AFValue,
        FPaperType, FGEVParameter));
    SetLength(XValues, HighConfidenceLimitLine.Count);
    SetLength(YValues, HighConfidenceLimitLine.Count);
    XValues := ChartSeries2XValues(HighConfidenceLimitLine);
    YValues := ChartSeries2YValues(HighConfidenceLimitLine);
    sgrdData.Cells[6, AGridRow] := FormatFloat('#.##',
      ProbabilityLinearInterpolate(XValues, YValues, 1-AFValue,
        FPaperType, FGEVParameter));
    SetLength(XValues, LowConfidenceLimitLine.Count);
    SetLength(YValues, LowConfidenceLimitLine.Count);
    XValues := ChartSeries2XValues(LowConfidenceLimitLine);
    YValues := ChartSeries2YValues(LowConfidenceLimitLine);
    sgrdData.Cells[7, AGridRow] := FormatFloat('#.##',
      ProbabilityLinearInterpolate(XValues, YValues, 1-AFValue,
        FPaperType, FGEVParameter));
  finally
    SetLength(XValues, 0);
    SetLength(YValues, 0);
  end;
end;

resourcestring
  rsPASS = 'ACCEPT';
  rsFAIL = 'REJECT';
  rsEnterNumberOfClasses =
    'Enter number of classes or keep the default value';
  rsClassNumberShouldBeWithin =
    'Number of classes shold be between 4 and n/5=';
  rsXSqTest = 'X-Square test for ';
  rsatofail = 'Attained a';
  rsPearsonParam = 'Pearson Param.';

procedure TFrmStatistics.mnuXSquareTestClick(Sender: TObject);
var
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    DoXSquareTest;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmStatistics.mnuKolmogorovSminovTestClick(Sender: TObject);
var
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    DoKolmogorovSmirnovTest;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmStatistics.DoXSquareTest;
var
  ANumberOfClasses, j: Integer;
  ADataList: TDataList;
  ALevelToFail, APearsonParam, AStandardDeviation, ALogStandardDeviation: Real;
  ADistribution: TStatisticalDistribution;
  s: string;
begin
{Get statistical values}
  if FMonthShowed = 0 then
  begin
    FFullDataList.Unbiased := FUnbiased;
    AStandardDeviation := FFullDataList.StandardDeviation;
    ALogStandardDeviation := FFullDataList.LogStandardDeviation;
    ADataList := TDataList(FFullDataList);
  end else begin
    FMonthlyDataList.Months[FMonthShowed].Unbiased := FUnbiased;
    AStandardDeviation :=
      FMonthlyDataList.Months[FMonthShowed].StandardDeviation;
    ALogStandardDeviation :=
      FMonthlyDataList.Months[FMonthShowed].LogStandardDeviation;
    ADataList := TDataList(FMonthlyDataList.Months[FMonthShowed]);
  end;
{Do not plot if StdDev <= 0, occured by exception}
  if (AStandardDeviation <= 0) or (ALogStandardDeviation <= 0) then
    Exit;
  ANumberOfClasses := Max(4, (ADataList.Count div 5+4) div 2);
  s := IntToStr(ANumberOfClasses);
  if not InputQuery(rsEnterNumberOfClasses, rsEnterNumberOfClasses, s) then
    Exit;
  ANumberOfClasses := StrToInt(s);
  if (ANumberOfClasses<4) or (ANumberOfClasses>
    Max(4, ADataList.Count div 5 +1) ) then
    raise Exception.Create(rsClassNumberShouldBeWithin+
      IntToStr(ADataList.Count div 5 +1));
  pgcPages.ActivePage := tbcParameters;
  sgrdData.ColCount := 6;
  sgrdData.RowCount := 28;
  if FMonthShowed =0 then
    sgrdData.Cells[0,0] := rsXSqTest+rsAllData
  else
    sgrdData.Cells[0,0] := rsXSqTest+LongMonthNames[FMonthShowed];
  sgrdData.ColWidths[0] := 190;
  sgrdData.ColWidths[1] := 70;
  sgrdData.ColWidths[2] := 70;
  sgrdData.ColWidths[3] := 70;
  sgrdData.ColWidths[4] := 70;
  sgrdData.ColWidths[5] := 100;
  sgrdData.Cells[1,0] := 'a=1%';
  sgrdData.Cells[2,0] := 'a=5%';
  sgrdData.Cells[3,0] := 'a=10%';
  sgrdData.Cells[4,0] := rsatofail;
  sgrdData.Cells[5,0] := rsPearsonParam;
  for j := 0 to 26 do
   begin
    ADistribution := nil;
    sgrdData.Rows[j+1].Clear;
    if ADataList.Count<3 then
      Continue;
    try
      try
        ADistribution := TStatisticalDistribution.Create(pl_Distributions[j],
          ADataList, FGEVParameter);
        ADistribution.SetGEVShape(FGEVParameter);
      except
        on EMathError do
          Continue;
        else
          raise;
      end;
      sgrdData.Cells[0,j+1] := ADistribution.Name;
      try
        if ADistribution.XSquareTest(0.01, ANumberOfClasses, ALevelToFail,
          APearsonParam) then
          sgrdData.Cells[1,j+1] := rsPASS else
          sgrdData.Cells[1,j+1] := rsFAIL;
        if ADistribution.XSquareTest(0.05, ANumberOfClasses, ALevelToFail,
          APearsonParam) then
          sgrdData.Cells[2,j+1] := rsPASS else
          sgrdData.Cells[2,j+1] := rsFAIL;
        if ADistribution.XSquareTest(0.10, ANumberOfClasses, ALevelToFail,
          APearsonParam) then
          sgrdData.Cells[3,j+1] := rsPASS else
          sgrdData.Cells[3,j+1] := rsFAIL;
        sgrdData.Cells[4,j+1] := FormatFloat('0.0',ALevelToFail*100)+'%';
        sgrdData.Cells[5,j+1] := FormatFloat('0.00', APearsonParam);
      except
        on EMathError do
          Continue;
        else
          raise;
      end;
    finally
      ADistribution.Free;
    end;
  end;
end;

resourcestring
  rsKolmogorovSmirnovTest = 'Kolmogorov-Smirnov test for:';
  rsDMax = 'DMax';

procedure TFrmStatistics.DoKolmogorovSmirnovTest;
var
  j: Integer;
  ADataList: TDataList;
  ALevelToFail, ADMax, AStandardDeviation, ALogStandardDeviation: Real;
  ADistribution: TStatisticalDistribution;
begin
{Get statistical values}
  if FMonthShowed = 0 then
  begin
    FFullDataList.Unbiased := FUnbiased;
    AStandardDeviation := FFullDataList.StandardDeviation;
    ALogStandardDeviation := FFullDataList.LogStandardDeviation;
    ADataList := TDataList(FFullDataList);
  end else begin
    FMonthlyDataList.Months[FMonthShowed].Unbiased := FUnbiased;
    AStandardDeviation :=
      FMonthlyDataList.Months[FMonthShowed].StandardDeviation;
    ALogStandardDeviation :=
      FMonthlyDataList.Months[FMonthShowed].LogStandardDeviation;
    ADataList := TDataList(FMonthlyDataList.Months[FMonthShowed]);
  end;
{Do not plot if StdDev <= 0, occured by exception}
  if (AStandardDeviation <= 0) or (ALogStandardDeviation <= 0) then
    Exit;
  ADataList.CalcWeibull;
  pgcPages.ActivePage := tbcParameters;
  sgrdData.ColCount := 6;
  sgrdData.RowCount := 28;
  if FMonthShowed =0 then
    sgrdData.Cells[0,0] := rsKolmogorovSmirnovTest+rsAllData
  else
    sgrdData.Cells[0,0] := rsKolmogorovSmirnovTest+LongMonthNames[FMonthShowed];
  sgrdData.ColWidths[0] := 190;
  sgrdData.ColWidths[1] := 70;
  sgrdData.ColWidths[2] := 70;
  sgrdData.ColWidths[3] := 70;
  sgrdData.ColWidths[4] := 70;
  sgrdData.ColWidths[5] := 100;
  sgrdData.Cells[1,0] := 'a=1%';
  sgrdData.Cells[2,0] := 'a=5%';
  sgrdData.Cells[3,0] := 'a=10%';
  sgrdData.Cells[4,0] := rsatofail;
  sgrdData.Cells[5,0] := rsDMax;
  for j := 0 to 26 do
   begin
    ADistribution := nil;
    sgrdData.Rows[j+1].Clear;
    if ADataList.Count<3 then
      Continue;
    try
      try
        ADistribution := TStatisticalDistribution.Create(pl_Distributions[j],
          ADataList, FGEVParameter);
        ADistribution.SetGEVShape(FGEVParameter);
      except
        on EMathError do
          Continue;
        else
          raise;
      end;
      sgrdData.Cells[0,j+1] := ADistribution.Name;
      try
        if ADistribution.KolmogorovSmirnovTest(0.01, ALevelToFail, ADMax) then
          sgrdData.Cells[1,j+1] := rsPASS else
          sgrdData.Cells[1,j+1] := rsFAIL;
        if ADistribution.KolmogorovSmirnovTest(0.05, ALevelToFail, ADMax) then
          sgrdData.Cells[2,j+1] := rsPASS else
          sgrdData.Cells[2,j+1] := rsFAIL;
        if ADistribution.KolmogorovSmirnovTest(0.10, ALevelToFail, ADMax) then
          sgrdData.Cells[3,j+1] := rsPASS else
          sgrdData.Cells[3,j+1] := rsFAIL;
        sgrdData.Cells[4,j+1] := FormatFloat('0.0', ALevelToFail*100)+'%';
        sgrdData.Cells[5,j+1] := FormatFloat('0.00', ADMax);
      except
        on EMathError do
          Continue;
        else
          raise;
      end;
    finally
      ADistribution.Free;
    end;
  end;
end;

procedure TFrmStatistics.WeibullPointsClick(Sender: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowMessage(Sender.Labels[ValueIndex]);
end;

end.
