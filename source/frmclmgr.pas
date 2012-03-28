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
  Dialogs, tsgrid, ts, ExtCtrls, TeeProcs, TeEngine, Chart, Series;

type
  TTimeScale = record
    mult: Real;
    desc: string;
  end;

const
   scalePoints:  array[0..10] of TTimeScale = ((mult:1/1440; desc:'minute'),
     (mult:1/288; desc:'five min.'), (mult:1/144; desc:'ten min.'),
     (mult:1/24; desc:'hour'), (mult:1; desc:'Day'),
     (mult:7; desc:'week'), (mult:30.44; desc:'month'),
     (mult:365.25; desc:'year'), (mult:3652.5; desc:'ten years'),
     (mult:36525; desc:'century'), (mult:3625250; desc:'millenium'));

type
  TFrmClimacogram = class(TForm)
    Chart: TChart;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FTimeseriesGrid: TTimeseriesGrid;
    procedure SetTimeseriesGrid(AValue: TTimeseriesGrid);
    procedure PrepareLineSeries(Timeseries: TTimeseries; Series: TLineSeries;
                                all: Boolean);
    procedure AddLineSeries(Timeseries: TTimeseries; Chart: TChart;
                                all: Boolean);
  public
    property TimeseriesGrid: TTimeseriesGrid read FTimeseriesGrid
                                             write SetTimeseriesGrid;
  end;

implementation

{$R *.dfm}

uses climacgr, math;

procedure TFrmClimacogram.FormCreate(Sender: TObject);
begin
  FTimeseriesGrid := nil;
end;

procedure TFrmClimacogram.FormShow(Sender: TObject);
begin
  AddLineSeries(TimeseriesGrid.Data[0], Chart, False);
  AddLineSeries(TimeseriesGrid.Data[0], Chart, True);
end;

procedure TFrmClimacogram.SetTimeseriesGrid(AValue: TTimeseriesGrid);
begin
  FTimeseriesGrid := AValue;
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
begin
  scaleFunction := scaleFunctions[all];
  maxScale := Timeseries.Count div 5;
  if Timeseries.TimeStep.LengthMinutes>0 then
    baseScale := Timeseries.TimeStep.LengthMinutes/1440
  else if Timeseries.TimeStep.LengthMonths>0 then
    baseScale := Timeseries.TimeStep.LengthMonths*30.44
  else
    Assert(False);
  i := 0;
  fac := factors[0];
  while fac<=maxScale do
  begin
    x := baseScale * fac;
    y := scaleFunction(Timeseries, fac);
    Series.AddXY(x, y);
    Inc(i);
    if i<16 then
      fac := factors[i]
    else
      fac := Trunc(IntPower(2, i-15))*100;
  end;
end;

procedure TFrmClimacogram.AddLineSeries(Timeseries: TTimeseries; Chart: TChart;
                                all: Boolean);
var
  ASeries: TLineSeries;
begin
  ASeries := nil;
  try
    ASeries := TLineSeries.Create(Chart);
    PrepareLineSeries(Timeseries, ASeries, all);
    Chart.AddSeries(ASeries);
    ASeries := nil;
  finally
    ASeries.Free;
  end;
end;

end.
