{******************************************************************}
{                                                                  }
{  Louise library                                                  }
{                                                                  }
{  Copyright (c) 2000-12 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Plotting of Climacograms. }

unit frmclmgr;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, tsgrid, ts, ExtCtrls, VclTee.TeeProcs, VclTee.TeEngine, VclTee.Chart,
  VclTee.Series, StdCtrls,
  Menus, VclTee.TeeGDIPlus;

type
  TTimeScale = record
    mult: Real;
    desc: string;
  end;

const
   scalePoints:  array[0..12] of TTimeScale = ((mult:1/1440; desc:'minute'),
     (mult:1/288; desc:'five min.'), (mult:1/144; desc:'ten min.'),
     (mult:1/24; desc:'hour'), (mult:1; desc:'Day'),
     (mult:7; desc:'week'), (mult:30.436875; desc:'month'),
     (mult:182.62125; desc:'six months'), (mult:365.2425; desc:'year'),
     (mult:1826.2125; desc:'five years'), (mult:3652.425; desc:'ten years'),
     (mult:36524.25; desc:'century'), (mult:365242.5; desc:'millenium'));

type
  TFrmClimacogram = class(TForm)
    Chart: TChart;
    lstSeries: TListBox;
    MainMenu: TMainMenu;
    Methods1: TMenuItem;
    First1: TMenuItem;
    Second1: TMenuItem;
    Both1: TMenuItem;
    Span1: TMenuItem;
    mnuOneToTen: TMenuItem;
    mnuOneToFive: TMenuItem;
    mnuOneToTwo: TMenuItem;
    Label1: TLabel;
    File1: TMenuItem;
    Edit1: TMenuItem;
    mnuCopyChart: TMenuItem;
    mnuCopyTabularData: TMenuItem;
    mnuPrintChart: TMenuItem;
    PrintDialog: TPrintDialog;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstSeriesClick(Sender: TObject);
    procedure First1Click(Sender: TObject);
    procedure mnuOneToTenClick(Sender: TObject);
    procedure mnuPrintChartClick(Sender: TObject);
    procedure mnuCopyChartClick(Sender: TObject);
    procedure mnuCopyTabularDataClick(Sender: TObject);
  private
    FTimeseriesGrid: TTimeseriesGrid;
    FXMin, FXMax, FYMin, FYMax: Double;
    procedure SetTimeseriesGrid(AValue: TTimeseriesGrid);
    procedure PrepareLineSeries(Timeseries: TTimeseries; Series: TLineSeries;
                                all: Boolean);
    procedure AddLineSeries(Timeseries: TTimeseries; Chart: TChart;
                                all: Boolean; seriesNo: Integer);
    procedure AddPeriodMarkers;
    procedure AddDiagonalLine;
    procedure FillList;
    procedure DrawLines;
    procedure Refresh;
  public
    property TimeseriesGrid: TTimeseriesGrid read FTimeseriesGrid
                                             write SetTimeseriesGrid;
  end;

implementation

{$R *.dfm}

uses climacgr, math, Clipbrd;

procedure TFrmClimacogram.FormCreate(Sender: TObject);
begin
  FTimeseriesGrid := nil;
end;

procedure TFrmClimacogram.FormShow(Sender: TObject);
begin
  FillList;
  Refresh;
end;

procedure TFrmClimacogram.Refresh;
begin
  DrawLines;
end;

procedure TFrmClimacogram.First1Click(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  Refresh;
end;

procedure TFrmClimacogram.lstSeriesClick(Sender: TObject);
begin
  Refresh;
end;

procedure TFrmClimacogram.mnuCopyChartClick(Sender: TObject);
begin
  Chart.CopyToClipboardMetafile(True);
end;

procedure TFrmClimacogram.mnuCopyTabularDataClick(Sender: TObject);
var
  i, j, multiplier: Integer;
  s, aLine: string;
  Flag: Boolean;
  scale, stdev, baseScale: Real;
  Timeseries: TTimeseries;
begin
  s := '';
  aLine := '';
  for i := 0 to Chart.SeriesCount-1 do
    if Chart.Series[i].Tag>=0 then
      aLine := aLine+Chart.Series[i].Title+#9+#9+#9;
  s := s+TrimRight(aLine)+#10;
  aLine := '';
  for i := 0 to Chart.SeriesCount-1 do
    if Chart.Series[i].Tag>=0 then
      aLine := aLine+'scale(days)'+#9+'multiplier'+#9+'stdev'+#9;
  s := s+TrimRight(aLine)+#10;
  j := 0;
  while True do
  begin
    Flag := False;
    aLine := '';
    for i := 0 to Chart.SeriesCount-1 do
      with Chart.Series[i] do
      begin
        if Tag<0 then Continue;
        if j>=Count then
        begin
          aLine := aLine+#9+#9+#9;
          Continue;
        end;
        Flag := True;
        scale := XValue[j];
        stdev := YValue[j];
        baseScale := 1;
        Timeseries := TTimeseries(lstSeries.Items.Objects[Tag]);
        if Timeseries.TimeStep.LengthMinutes>0 then
          baseScale := Timeseries.TimeStep.LengthMinutes/1440
        else if Timeseries.TimeStep.LengthMonths>0 then
          baseScale := Timeseries.TimeStep.LengthMonths*30.436875
        else
          Assert(False);
        multiplier := Round(scale/baseScale);
        aLine := aLine+FloatToStr(scale)+#9+IntToStr(multiplier)+#9+
                       FloatToStr(stdev)+#9;
      end;
    s := s+TrimRight(aLine);
    if not Flag then Break;
    s := s+#10;
    Inc(j);
  end;
  Clipboard.AsText := TrimRight(s);
end;

procedure TFrmClimacogram.mnuOneToTenClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  Refresh;
end;

procedure TFrmClimacogram.mnuPrintChartClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    Chart.Print;
end;

procedure TFrmClimacogram.SetTimeseriesGrid(AValue: TTimeseriesGrid);
begin
  FTimeseriesGrid := AValue;
end;

procedure TFrmClimacogram.DrawLines;
var
  i: Integer;
  ACursor: TCursor;
begin
  FXMin := 1e37;
  FYMin := 1e37;
  FXMax := 0;
  FYMax := 0;
  ACursor := Screen.Cursor;
  Chart.RemoveAllSeries;
  try
    Screen.Cursor := crHourGlass;
    for i := 0 to lstSeries.Count - 1 do
      if lstSeries.Selected[i] and
                TTimeseries(lstSeries.Items.Objects[i]).TimeStepStrict and
                (TTimeseries(lstSeries.Items.Objects[i]).Count>50) then
      begin
        if (First1.Checked) or (Both1.Checked) then
          AddLineSeries(TTimeseries(lstSeries.Items.Objects[i]),
                        Chart, False, i);
        if (Second1.Checked) or (Both1.Checked) then
          AddLineSeries(TTimeseries(lstSeries.Items.Objects[i]),
                        Chart, True, i);
      end;
  finally
    Screen.Cursor := ACursor;
  end;
  if (FXMax<FXMin) or (FYMax<FYMin) then Exit;
  Chart.LeftAxis.SetMinMax(Power(10, Floor(Log10(FYMin))), FYMax);
  AddPeriodMarkers;
  AddDiagonalLine;
end;

procedure TFrmClimacogram.FillList;
var
  i: Integer;
begin
  for i := 0 to FTimeseriesGrid.Count-1 do
    lstSeries.AddItem(IntToStr(i+1)+': '+FTimeseriesGrid.Data[i].Title,
                            FTimeseriesGrid.Data[i]);
  lstSeries.Selected[FTimeseriesGrid.ActiveIndex] := True;
end;

type
  TScaleFunc = function(ATimeseries: TTimeseries; SScale: Integer): Real;

const
  scaleFunctions: array[False..True] of TScaleFunc = (AggrStDev, AggrStDevAll);

  factors: array[0..15] of Integer = (1, 2, 4, 6, 8, 10, 12, 16, 20, 24, 30,
                                      36, 48, 60, 80, 100);

procedure TFrmClimacogram.PrepareLineSeries(Timeseries: TTimeseries;
                                            Series: TLineSeries; all: Boolean);
var
  scaleFunction: TScaleFunc;
  maxScale: Integer;
  i: Integer;
  fac: Integer;
  x, y, baseScale: Real;
  MaxSpan: Integer;
begin
  MaxSpan := 10;
  if mnuOneToTen.Checked then
    MaxSpan := 10
  else if mnuOneToFive.Checked then
    MaxSpan := 5
  else if mnuOneToTwo.Checked then
    MaxSpan := 2
  else
    Assert(False);
  scaleFunction := scaleFunctions[all];
  maxScale := Timeseries.Count div MaxSpan;
  baseScale := 1;
  if Timeseries.TimeStep.LengthMinutes>0 then
    baseScale := Timeseries.TimeStep.LengthMinutes/1440
  else if Timeseries.TimeStep.LengthMonths>0 then
    baseScale := Timeseries.TimeStep.LengthMonths*30.436875
  else
    Assert(False);
  i := 0;
  fac := factors[0];
  while fac<=maxScale do
  begin
    x := baseScale * fac;
    y := scaleFunction(Timeseries, fac);
    if y>=0 then
    begin
      Series.AddXY(x, y);
      FXMin := Min(FXMin, x);
      FXMax := Max(FXMax, x);
      FYMin := Min(FYMin, y);
      FYMax := Max(FYMax, y);
    end;
    Inc(i);
    if i<16 then
      fac := factors[i]
    else
      fac := Trunc(IntPower(2, i-15))*100;
  end;
end;

procedure TFrmClimacogram.AddLineSeries(Timeseries: TTimeseries; Chart: TChart;
                                all: Boolean; seriesNo: Integer);
var
  ASeries: TLineSeries;
  s: string;
begin
  ASeries := nil;
  try
    ASeries := TLineSeries.Create(Chart);
    s := Timeseries.Title;
    if s='' then s := IntToStr(seriesNo+1)+':';
    if all then s := s+' (multi pass)';
    ASeries.Title := s;
    ASeries.Pen.Width := 2;
    ASeries.Tag := seriesNo;
    ASeries.Shadow.VertSize := 2;
    ASeries.Shadow.HorizSize := 1;
    ASeries.Shadow.Color := RGB(210,210,190);
    PrepareLineSeries(Timeseries, ASeries, all);
    Chart.AddSeries(ASeries);
    ASeries := nil;
  finally
    ASeries.Free;
  end;
end;

procedure TFrmClimacogram.AddPeriodMarkers;
var
  ASeries: TPointSeries;
  i: Integer;
  AMin, AMax: Double;
begin
  Chart.BottomAxis.CalcMinMax(AMin, AMax);
  if Abs(AMin-AMax)<0.01 then
    Exit;
  ASeries := nil;
  try
    ASeries := TPointSeries.Create(Chart);
    ASeries.Visible := True;
    ASeries.ShowInLegend := False;
    ASeries.Pointer.Visible := False;
    Chart.Axes.Top.Maximum := 1e37;
    Chart.Axes.Top.Minimum := 0.001;
    Chart.Axes.Top.Maximum := AMax;
    Chart.Axes.Top.Minimum := AMin;
    ASeries.HorizAxis := aTopAxis;
    ASeries.Tag := -1;
    for i := 0 to Length(scalePoints) - 1 do
      with scalePoints[i] do
        if (mult>=AMin) and (mult<=AMax) then
          ASeries.AddXY(mult, 0, desc);
    Chart.AddSeries(ASeries);
    ASeries := nil;
  finally
    ASeries.Free;
  end;
end;

procedure TFrmClimacogram.AddDiagonalLine;
var
  ASeries: TLineSeries;
  H: Double;
begin
  ASeries := nil;
  try
    ASeries := TLineSeries.Create(Chart);
    ASeries.Title := 'slope=-0.5';
    ASeries.Color := clLtGray;
    ASeries.Visible := True;
    ASeries.Tag := -1;
    H := Chart.LeftAxis.Minimum*sqrt(FXMax/FXMin);
    ASeries.AddXY(FXMin, Chart.LeftAxis.Minimum+H);
    ASeries.AddXY(FXMax, Chart.LeftAxis.Minimum);
    Chart.AddSeries(ASeries);
    ASeries := nil;
  finally
    ASeries.Free;
  end;
end;

end.
