{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2009 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit frmdevchart;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, tsprocess, ExtCtrls, TeeProcs, TeEngine, Chart, StdCtrls, Series,
  Menus;

type
  TDeviationChartCoefs = array of Real;

  TDeviationChartPoint = record
    x, y: Real;
    Date: TDateTime;
  end;

  TDeviationChartPoints = array of TDeviationChartPoint;

  TDeviationChartData = record
    RegressionResults: TRegressionResults;
    ChartPoints: TDeviationChartPoints;
  end;

type
  TFrmDeviationChart = class(TForm)
    Chart: TChart;
    rgrpMarks: TRadioGroup;
    chkShowMarks: TCheckBox;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuPrintChart: TMenuItem;
    mnuPrintSetup: TMenuItem;
    N1: TMenuItem;
    mnuExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuCopyChart: TMenuItem;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    seriesLine: TLineSeries;
    seriesPoints: TPointSeries;
    procedure FormShow(Sender: TObject);
    procedure lstVariablesClick(Sender: TObject);
    procedure mnuPrintChartClick(Sender: TObject);
    procedure mnuPrintSetupClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuCopyChartClick(Sender: TObject);
  private
    procedure DrawAll;
    procedure SetControlStatus;
  public
    Data: TDeviationChartData;
  end;

implementation

{$R *.dfm}

resourcestring
  rsIndependentVariable = 'Independent variable';
  rsMeanIndependentVariable = 'Mean value of independent variables';


procedure TFrmDeviationChart.DrawAll;

  procedure DrawPoints;
  var
    i: Integer;
  begin
    seriesPoints.Clear;
    for i := 0 to Length(Data.ChartPoints)-1 do
      with Data.ChartPoints[i] do
        with seriesPoints do
          AddXY(x, y, DateToStr(Date), clDefault);
  end;

  procedure DrawLines;
  var
    i: Integer;
    CoefSum, ConstTerm, XMin, XMax: Real;
  begin
    seriesLine.Clear;
    XMin := seriesPoints.MinXValue;
    XMax := seriesPoints.MaxXValue;
    CoefSum := 0;
    for i := 2 to Data.RegressionResults.Size do
      CoefSum := CoefSum + Data.RegressionResults[i];
    ConstTerm := Data.RegressionResults[1];
    seriesLine.AddXY(XMin, ConstTerm+CoefSum*XMin,  '', clDefault);
    seriesLine.AddXY(XMax, ConstTerm+CoefSum*XMax,  '', clDefault);
  end;

begin
  if Data.RegressionResults.Size<=2 then
    Chart.BottomAxis.Title.Caption := rsIndependentVariable else
    Chart.BottomAxis.Title.Caption := rsMeanIndependentVariable;
  DrawPoints;
  DrawLines;
end;

procedure TFrmDeviationChart.FormShow(Sender: TObject);
begin
  if Data.RegressionResults = nil then Exit;
  SetControlStatus;
end;

procedure TFrmDeviationChart.lstVariablesClick(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmDeviationChart.mnuCopyChartClick(Sender: TObject);
begin
  Chart.CopyToClipboardMetafile(True);
end;

procedure TFrmDeviationChart.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmDeviationChart.mnuPrintChartClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    Chart.Print;
end;

procedure TFrmDeviationChart.mnuPrintSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TFrmDeviationChart.SetControlStatus;
begin
  if Data.RegressionResults = nil then Exit;
  DrawAll;
  seriesPoints.Pointer.Style := TSeriesPointerStyle(rgrpMarks.ItemIndex);
  seriesPoints.Marks.Visible := chkShowMarks.Checked;
end;

end.
