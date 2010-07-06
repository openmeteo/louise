{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit tsdialogsdsgn;

{$R tsdialogs.dcr}

interface

uses tsdialogs, Classes, icomponent;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LoUISE Timeseries', [TRegularizeStepDialog,
    TAggregationDialog,
    TRangeCheckDialog, TFlagsDialog, TRegressionDialog, TRegrResultsDialog,
    TPenmanDialog, TLinearCombinationDialog, TTimeSeriesGraphForm,
    TStatisticsForm, TDoubleMassDialog,
    TIDFEvaluationDialog, TSetIDFTSDialog, TSimpleHydroModel,
    TStageDischargeDialog, TTsprocessSelectionsDialog, TMultiTimeseriesDialog,
    TComplexCalculationsDialog, TDisaggregationDialog, THydrometryDialog,
    TTimeseriesIntegrationDialog, TTimeseriesImportDataDialog,
    TTimeseriesWizard, TAggregateSeriesDialog, TRoseDiagramDialog]);
  RegisterComponents('LoUISE Components', [TIMainMenu, TIShape]);
end;

end.
