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
     (mult:36252.5; desc:'century'), (mult:362525; desc:'millenium'));

type
  TFrmClimacogram = class(TForm)
    Chart: TChart;
    procedure FormCreate(Sender: TObject);
  private
    FTimeseriesGrid: TTimeseriesGrid;
    procedure SetTimeseriesGrid(AValue: TTimeseriesGrid);
    procedure PrepareLineSeries(Timeseries: TTimeseries; Series: TLineSeries;
                                all: Boolean);
  public
    property TimeseriesGrid: TTimeseriesGrid read FTimeseriesGrid
                                             write SetTimeseriesGrid;
  end;

implementation

{$R *.dfm}

uses climacgr;

procedure TFrmClimacogram.FormCreate(Sender: TObject);
begin
  FTimeseriesGrid := nil;
end;

procedure TFrmClimacogram.SetTimeseriesGrid(AValue: TTimeseriesGrid);
begin
  FTimeseriesGrid := AValue;
end;

type
  TScaleFunc = function(ATimeseries: TTimeseries; SScale: Integer): Real;

const
  scaleFunctions: array[False..True] of TScaleFunc = (AggrStDev, AggrStDevAll);

procedure TFrmClimacogram.PrepareLineSeries(Timeseries: TTimeseries;
                                            Series: TLineSeries; all: Boolean);
var
  scaleFunction: TScaleFunc;
  maxScale: Integer;
  i: Integer;
  x, y, baseScale: Real;
begin
  scaleFunction := scaleFunctions[all];
  maxScale := Timeseries.Count div 10;
  i := 1;
  if Timeseries.TimeStep.LengthMinutes>0 then
    baseScale := Timeseries.TimeStep.LengthMinutes/1440
  else if Timeseries.TimeStep.LengthMonths>0 then
    baseScale := Timeseries.TimeStep.LengthMonths*30.44
  else
    Assert(False);
  while i<=maxScale do
  begin
    x := baseScale * i;
    y := scaleFunction(Timeseries, i);
    Series.AddXY(x, y);
    i := i*2;
  end;
end;

end.
