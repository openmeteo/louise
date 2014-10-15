{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit tsgraphdlg;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,  StdCtrls, ExtCtrls, VclTee.TeEngine, VclTee.TeeProcs, VclTee.Chart,
  VclTee.Series, tsgrid, Ts, Menus, contnrs, ToolWin, ComCtrls, ImgList,
  VclTee.TeeGDIPlus;

type
  EIncompatible = class (Exception);
  TTimeseriesGraphType = (tsgtLine, tsgtStackedBar, tsgtSideBar);

type
  TGraphSeriesList = class(TObjectList);

  TGraphTimeSeriesList = class(TObjectList);

  TTimeseriesGraphSeries = class(TComponent)
  private
    FBarSeries: TBarSeries;
    FLineSeries: TLineSeries;
    FSeriesType: TTimeseriesGraphType;
    FParent: TChart;
    FSeriesColor: TColor;
    FLineWidth: Integer;
    FTitle: string;
    procedure SetSeriesType(AValue: TTimeseriesGraphType);
    procedure SetParent(AValue: TChart);
    procedure SetSeriesColor(AValue: TColor);
    procedure SetTitle(AValue: string);
    procedure SetLineWidth(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddXY(Const AXValue, AYValue: Double;
      Const AXLabel: String; AColor: TColor);
    property Parent: TChart read FParent write SetParent;
    property SeriesType: TTimeseriesGraphType read FSeriesType
      write SetSeriesType;
    property SeriesColor: TColor read FSeriesColor write SetSeriesColor;
    property Title: string read FTitle write SetTitle;
    property LineWidth: Integer read FLineWidth write SetLineWidth;
  end;

  TFrmTSeriesGraph = class(TForm)
    chartTSGraph: TChart;
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuPrint: TMenuItem;
    mnuEdit: TMenuItem;
    mnuCopyClipboard: TMenuItem;
    mnuClear: TMenuItem;
    mnuView: TMenuItem;
    mnuRestoreZoom: TMenuItem;
    mnuZoomIn: TMenuItem;
    mnuZoomOut: TMenuItem;
    mnuSeparator2: TMenuItem;
    mnuHelp: TMenuItem;
    mnuTimeSeriesGraphHelp: TMenuItem;
    mnuSaveAsBitmap: TMenuItem;
    mnuExit: TMenuItem;
    mnuSeparator3: TMenuItem;
    mnuArrowKeysToPan: TMenuItem;
    mnuRefresh: TMenuItem;
    mnuCycleColors: TMenuItem;
    mnuThickLines: TMenuItem;
    mnuThinLines: TMenuItem;
    mnuSeparator1: TMenuItem;
    PrintDialog: TPrintDialog;
    N1: TMenuItem;
    mnuPrintSetup: TMenuItem;
    PrinterSetupDialog: TPrinterSetupDialog;
    ToolBar: TToolBar;
    tbtnLineSeries: TToolButton;
    Images: TImageList;
    tbtnSideBars: TToolButton;
    tbtnStackedBars: TToolButton;
    ToolButton4: TToolButton;
    tbtnZoomAll: TToolButton;
    tbtnZoomIn: TToolButton;
    tbtnZoomOut: TToolButton;
    ToolButton8: TToolButton;
    tbtnThick: TToolButton;
    tbtnThin: TToolButton;
    tbtnCycleColors: TToolButton;
    ToolButton1: TToolButton;
    tbtnLegent: TToolButton;
    SaveDialog: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure mnuPrintClick(Sender: TObject);
    procedure mnuCopyClipboardClick(Sender: TObject);
    procedure mnuClearClick(Sender: TObject);
    procedure mnuRestoreZoomClick(Sender: TObject);
    procedure mnuZoomInClick(Sender: TObject);
    procedure mnuZoomOutClick(Sender: TObject);
    procedure mnuTimeSeriesGraphHelpClick(Sender: TObject);
    procedure mnuSaveAsBitmapClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure IFormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chartTSGraphUndoZoom(Sender: TObject);
    procedure IFormCreate(Sender: TObject);
    procedure mnuRefreshClick(Sender: TObject);
    procedure mnuCycleColorsClick(Sender: TObject);
    procedure mnuThickLinesClick(Sender: TObject);
    procedure mnuThinLinesClick(Sender: TObject);
    procedure mnuPrintSetupClick(Sender: TObject);
    procedure IFormDestroy(Sender: TObject);
    procedure tbtnLineSeriesClick(Sender: TObject);
    procedure tbtnLegentClick(Sender: TObject);
    procedure chartTSGraphZoom(Sender: TObject);
  private
    FColor: array [1..12] of TColor;
    FColorIndex: Integer;
    FLineWidth: Integer;
    FMUnit: string;
    FAllowDifferentUnits: Boolean;
    FTimeseriesGraphType: TTimeseriesGraphType;
    FSeriesList: TGraphSeriesList;
    FTimeseriesList: TGraphTimeSeriesList;
    FLegent: Boolean;
    procedure SetTimeseriesGraphType(AValue: TTimeseriesGraphType);
    procedure SetLegent(AValue: Boolean);
    procedure SetDateAxisFormat;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    TimeSeriesGrid: TTimeSeriesGrid;
    property AllowDifferentUnits: Boolean read FAllowDifferentUnits
      write FAllowDifferentUnits;
    procedure Refresh;
    procedure ClearArea;
    function Add(ATimeSeries: TTimeSeries): Boolean;
    function Remove(ATimeSeries: TTimeSeries): Boolean;
    property TimeseriesGraphType: TTimeseriesGraphType read FTimeseriesGraphType
      write SetTimeseriesGraphType;
    property LegentVisible: Boolean read FLegent write SetLegent;
  end;

implementation

uses iStrUtils, Dates, Math;
{$R *.DFM}

{ TTimeseriesGraphSeries }

constructor TTimeseriesGraphSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBarSeries := TBarSeries.Create(nil);
  FLineSeries := TLineSeries.Create(nil);
  FLineSeries.XValues.DateTime := True;
  FBarSeries.XValues.DateTime := True;
  FBarSeries.Marks.Visible := False;
  FBarSeries.CustomBarWidth := 5;
  FLineSeries.Active := True;
  FBarSeries.Active := False;
  FSeriesType := tsgtLine;
  FBarSeries.MultiBar := VclTee.Series.mbNone;
  FSeriesColor := clDefault;
  FTitle := '';
end;

destructor TTimeseriesGraphSeries.Destroy;
begin
  FBarSeries.Free;
  FLineSeries.Free;
  inherited Destroy;
end;

procedure TTimeseriesGraphSeries.Clear;
begin
  FLineSeries.Clear;
  FBarSeries.Clear;
end;

procedure TTimeseriesGraphSeries.SetSeriesType(AValue: TTimeseriesGraphType);
begin
  FSeriesType := AValue;
  FLineSeries.Active := (FSeriesType = tsgtLine);
  FBarSeries.Active := (FSeriesType = tsgtSideBar) or
    (FSeriesType = tsgtStackedBar);
  if FSeriesType = tsgtStackedBar then
    FBarSeries.MultiBar := VclTee.Series.mbNone;
  if FSeriesType = tsgtSideBar then
    FBarSeries.MultiBar := mbSide;
end;

procedure TTimeseriesGraphSeries.SetParent(AValue: TChart);
begin
  FParent := AValue;
  FLineSeries.ParentChart := FParent;
  FBarSeries.ParentChart := FParent;
end;

procedure TTimeseriesGraphSeries.SetSeriesColor(AValue: TColor);
begin
  FSeriesColor := AValue;
  FLineSeries.SeriesColor := FSeriesColor;
  FBarSeries.SeriesColor := FSeriesColor;
end;

procedure TTimeseriesGraphSeries.SetTitle(AValue: string);
begin
  FTitle := AValue;
  FLineSeries.Title := FTitle;
  FBarSeries.Title := FTitle;
end;

procedure TTimeseriesGraphSeries.SetLineWidth(AValue: Integer);
begin
  FLineWidth := AValue;
  FLineSeries.LinePen.Width := FLineWidth;
  FBarSeries.CustomBarWidth := AValue + 4;
end;

procedure TTimeseriesGraphSeries.AddXY(Const AXValue, AYValue: Double;
      Const AXLabel: String; AColor: TColor);
begin
  FLineSeries.AddXY(AXValue, AYValue, AXLabel, AColor);
  FBarSeries.AddXY(AXValue, AYValue, AXLabel, AColor);
end;

{ TFrmTSeriesGraph }

procedure TFrmTSeriesGraph.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := WS_EX_APPWINDOW;
end;

procedure TFrmTSeriesGraph.IFormCreate(Sender: TObject);
begin
  FSeriesList := TGraphSeriesList.Create(True);
  FTimeSeriesList := TGraphTimeSeriesList.Create(False);
  FTimeseriesGraphType := tsgtLine;
  tbtnLineSeries.Down := True;
  tbtnStackedBars.Down := False;
  tbtnSideBars.Down := False;
  FColorIndex := 0;
  FLineWidth := 1;
  FColor[1] := clBlack;
  FColor[2] := clGreen;
  FColor[3] := clBlue;
  FColor[4] := clRed;
  FColor[5] := clOlive;
  FColor[6] := clAqua;
  FColor[7] := clPurple;
  FColor[8] := clNavy;
  FColor[9] := clMaroon;
  FColor[10] := clTeal;
  FColor[11] := clLime;
  Fcolor[12] := clFuchsia;
  chartTSGraph.BottomAxis.ExactDateTime := True;
  chartTSGraph.BottomAxis.LabelsSeparation := 10;
end;


procedure TFrmTSeriesGraph.IFormDestroy(Sender: TObject);
begin
  FSeriesList.Free;
  FTimeSeriesList.Free;
end;


procedure TFrmTSeriesGraph.FormShow(Sender: TObject);
begin
  chartTSGraph.RemoveAllSeries;
  FSeriesList.Clear;
  FTimeSeriesList.Clear;
  FMUnit := '';
  chartTSGraph.Legend.Visible := True;
  tbtnLegent.Down := True;
end;


procedure TFrmTSeriesGraph.FormHide(Sender: TObject);
begin
  ClearArea;
end;

procedure TFrmTSeriesGraph.SetDateAxisFormat;
var
  ATimeStep: TTimeStep;
  ANominalOffset, AActualOffset: TDateOffset;
  i: Integer;
begin
  ATimeStep := tstFiveMinute;
  ANominalOffset := TDateOffset.Create(0, 0);
  for i := 0 to FTimeseriesList.Count -1 do
  begin
    with FTimeseriesList.Items[i] as TTimeseries do
      if TimeStep>ATimeStep then
      begin
        ATimeStep := TimeStep;
        if not Timestep.IsVariable then if TimeStepStrict then
          ANominalOffset := NominalOffset;
        AActualOffset := ActualOffset;
      end;
  end;
  if ATimestep<tstDaily then
    chartTSGraph.BottomAxis.DateTimeFormat := 'yyyy-mm-dd hh:nn'
  else if ATimeStep<tstMonthly then
  begin
    if AActualOffset.Minutes=1440 then
      chartTSGraph.BottomAxis.DateTimeFormat := 'yyyy-mm-dd'
    else
      chartTSGraph.BottomAxis.DateTimeFormat := 'yyyy-mm-dd hh:nn';
  end else if ATimeStep<tstAnnual then
    chartTSGraph.BottomAxis.DateTimeFormat := 'MMM yyyy'
  else
    chartTSGraph.BottomAxis.DateTimeFormat := 'yyyy';
  chartTSGraph.BottomAxis.Increment := 0;
  if ATimeStep<tstMonthly then
    chartTSGraph.BottomAxis.LabelsAngle := 0 else
    chartTSGraph.BottomAxis.LabelsAngle := 90;
end;

resourcestring
  rsIncompatibilityError ='Time series with incompatible units.';

function TFrmTSeriesGraph.Add(ATimeSeries: TTimeseries):Boolean;
var
  i: Integer;
  AGraphSeries: TTimeseriesGraphSeries;
  AMean: Double;
  AString: string;
begin
  Result := False;
  {Check if time series is already on graph}
  for i := 0 to FTimeSeriesList.Count-1 do
    if TTimeSeries(FTimeSeriesList.Items[i]) = ATimeSeries then
      Exit;
  if (ATimeSeries.Count<2) and
    (ATimeSeries.TimeStep = tstAnnual) then Exit; //Workarround to system crash!
  if FTimeSeriesList.Count < 1 then
  begin
    FMunit := ATimeSeries.MUnit;
    chartTSGraph.LeftAxis.Title.Caption := FMUnit;
  end;
  if (ATimeSeries.MUnit<>FMUnit) and
    not FAllowDifferentUnits then
      raise EIncompatible.Create(rsIncompatibilityError);
  FTimeSeriesList.Add(ATimeSeries);
  AGraphSeries := nil;
  try
    AGraphSeries := TTimeseriesGraphSeries.Create(Self);
    AGraphSeries.Parent := chartTSGraph;
    AGraphSeries.SeriesType := FTimeseriesGraphType;
    Inc(FColorIndex);
    if FColorIndex>12 then FColorIndex := 1;
    AGraphSeries.SeriesColor := FColor[FColorIndex];
    try
      if ATimeseries.Comment='' then
        raise EListError.Create('No such comments');
      AString := ATimeSeries.Title+' '+
        DelimitedStringItem(ATimeSeries.Comment,1,#13)+
        DelimitedStringItem(ATimeSeries.Comment,2,#13);
      AString := StringReplace(AString,'Variable:','',
        [rfReplaceAll, rfIgnoreCase]);
      AString := StringReplace(AString,#10,', ',
        [rfReplaceAll, rfIgnoreCase]);
    except
      on EListError do
        AString := ATimeSeries.Title;
      else
        raise;
    end;
    AGraphSeries.Title := AString;
    AGraphSeries.LineWidth := FLineWidth;
    if ATimeSeries.CountNotNull>0 then
      AMean := ATimeSeries.Mean
    else
      AMean := 0;
    with AGraphSeries do
    begin
      for i := 0 to ATimeSeries.Count-1 do
      begin
        if not ATimeSeries.Items[i].isNull then
        begin
          AddXY(ATimeSeries.Items[i].Date,ATimeSeries.Items[i].Asfloat,'',
            clTeeColor);
        end else begin
          AddXY(ATimeSeries.Items[i].Date,AMean,'',clNone);
        end;
      end;
    end;
    FSeriesList.Add(AGraphSeries);
    Result := True;
    AGraphSeries := nil;
    chartTSGraph.Refresh;
    SetDateAxisFormat;
  finally
    AGraphSeries.Free;
  end;
end;


function TFrmTSeriesGraph.Remove(ATimeSeries: TTimeseries):Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FTimeSeriesList.Count -1 do
  begin
    if TTimeSeries(FTimeSeriesList.Items[i]) = ATimeSeries then
    begin
      chartTSGraph.RemoveSeries(
        TTimeseriesGraphSeries(FSeriesList[i]).FLineSeries);
      chartTSGraph.RemoveSeries(
        TTimeseriesGraphSeries(FSeriesList[i]).FBarSeries);
      FSeriesList.Delete(i);
      FTimeSeriesList.Delete(i);
      Result := True;
      SetDateAxisFormat;
      Exit;
    end;
  end;

end;


procedure TFrmTSeriesGraph.Refresh;
var
  i,j: Integer;
  ATimeSeries: TTimeseries;
  AGraphSeries: TTimeseriesGraphSeries;
  AMean: Double;
begin
  for i := 0 to FTimeSeriesList.Count-1 do
  begin
    ATimeSeries := TTimeseries(FTimeSeriesList.Items[i]);
    AGraphSeries := TTimeseriesGraphSeries(FSeriesList.Items[i]);
    AGraphSeries.Clear;
    if ATimeSeries.CountNotNull>0 then
      AMean := ATimeSeries.Mean
    else
      AMean := 0;
    With AGraphSeries do
    begin
      for j := 0 to ATimeSeries.Count-1 do
      begin
        if not ATimeSeries.Items[j].isNull then
        begin
          AddXY(ATimeSeries.Items[j].Date,ATimeSeries.Items[j].Asfloat,'',
            clTeeColor);
        end else begin
          AddXY(ATimeSeries.Items[j].Date,AMean,'',clNone);
        end;
      end;
    end;
  end;
  SetDateAxisFormat;  
end;


procedure TFrmTSeriesGraph.ClearArea;
begin
  chartTSGraph.UndoZoom;
  chartTSGraph.RemoveAllSeries;
  FSeriesList.Clear;
  FTimeSeriesList.Clear;
  FColorIndex := 0;
end;


procedure TFrmTSeriesGraph.mnuPrintClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    chartTSGraph.Print;
end;


procedure TFrmTSeriesGraph.mnuPrintSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;


procedure TFrmTSeriesGraph.mnuCopyClipboardClick(Sender: TObject);
begin
  chartTSGraph.CopyToClipboardMetafile(False);
end;


procedure TFrmTSeriesGraph.mnuClearClick(Sender: TObject);
begin
  ClearArea;
end;


procedure TFrmTSeriesGraph.mnuRestoreZoomClick(Sender: TObject);
begin
  chartTSGraph.UndoZoom;
end;


procedure TFrmTSeriesGraph.mnuZoomInClick(Sender: TObject);
begin
  chartTSGraph.ZoomPercent(105);
end;


procedure TFrmTSeriesGraph.mnuZoomOutClick(Sender: TObject);
begin
  chartTSGraph.ZoomPercent(95);
end;


procedure TFrmTSeriesGraph.mnuTimeSeriesGraphHelpClick(Sender: TObject);
begin
  Application.HelpContext(1390);
end;

procedure TFrmTSeriesGraph.mnuSaveAsBitmapClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    chartTSGraph.SaveToBitmapFile(SaveDialog.FileName);
end;


procedure TFrmTSeriesGraph.mnuExitClick(Sender: TObject);
begin
  Hide;
end;


procedure TFrmTSeriesGraph.IFormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AMin, AMax, APan: Double;
begin
  case Key of
    VK_RIGHT:
    begin
      Amin := chartTSGraph.BottomAxis.Minimum;
      Amax := chartTSGraph.BottomAxis.Maximum;
      APan := (Amax-Amin)*0.0250;
      chartTSGraph.BottomAxis.SetMinMax(AMin+APan, AMax+APan);
    end;
    VK_LEFT:
    begin
      Amin := chartTSGraph.BottomAxis.Minimum;
      Amax := chartTSGraph.BottomAxis.Maximum;
      APan := (Amax-Amin)*0.0250;
      chartTSGraph.BottomAxis.SetMinMax(AMin-APan, AMax-APan);
    end;
    VK_UP:
    begin
      Amin := chartTSGraph.LeftAxis.Minimum;
      Amax := chartTSGraph.LeftAxis.Maximum;
      APan := (Amax-Amin)*0.0250;
      chartTSGraph.LeftAxis.SetMinMax(AMin+APan, AMax+APan);
    end;
    VK_DOWN:
    begin
      Amin := chartTSGraph.LeftAxis.Minimum;
      Amax := chartTSGraph.LeftAxis.Maximum;
      APan := (Amax-Amin)*0.0250;
      chartTSGraph.LeftAxis.SetMinMax(AMin-APan, AMax-APan);
    end;
  end;
end;


procedure TFrmTSeriesGraph.chartTSGraphUndoZoom(Sender: TObject);
begin
  chartTSGraph.BottomAxis.Automatic := True;
  chartTSGraph.LeftAxis.Automatic := True;
  chartTSGraph.Refresh;
  chartTSGraph.BottomAxis.Increment := 0;
end;


procedure TFrmTSeriesGraph.chartTSGraphZoom(Sender: TObject);
begin
  chartTSGraph.BottomAxis.Increment := 0;
end;

procedure TFrmTSeriesGraph.mnuRefreshClick(Sender: TObject);
begin
  Refresh;
end;


procedure TFrmTSeriesGraph.mnuCycleColorsClick(Sender: TObject);
var
  i,j: Integer;
begin
  for i :=0 to FSeriesList.Count-1 do
  begin
    for j := 1 to 12 do
    begin
      if TTimeseriesGraphSeries(FSeriesList.Items[i]).SeriesColor = FColor[j] then
      begin
        if j<12 then
          TTimeseriesGraphSeries(FSeriesList.Items[i]).SeriesColor := FColor[j+1] else
          TTimeseriesGraphSeries(FSeriesList.Items[i]).SeriesColor := FColor[1];
        Break;
      end;
    end;
  end;
  Inc(FColorIndex);
  if FColorIndex > 12 then
    FColorIndex := 0;
end;


procedure TFrmTSeriesGraph.mnuThickLinesClick(Sender: TObject);
var
  i: Integer;
begin
  if FLineWidth <=12 then
  begin
    Inc(FLineWidth);
    for i := 0 to FSeriesList.Count-1 do
    begin
      TTimeseriesGraphSeries(FSeriesList.Items[i]).LineWidth := FLineWidth;
    end;
  end;
end;

procedure TFrmTSeriesGraph.mnuThinLinesClick(Sender: TObject);
var
  i: Integer;
begin
  if FLineWidth >1 then
  begin
    Dec(FLineWidth);
    for i := 0 to FSeriesList.Count-1 do
    begin
      TTimeseriesGraphSeries(FSeriesList.Items[i]).LineWidth := FLineWidth;
    end;
  end;
end;

procedure TFrmTSeriesGraph.tbtnLineSeriesClick(Sender: TObject);
begin
  if Sender = tbtnLineSeries then
    TimeseriesGraphType := tsgtLine;
  if Sender = tbtnStackedBars then
    TimeseriesGraphType := tsgtStackedBar;
  if Sender = tbtnSideBars then
    TimeseriesGraphType := tsgtSideBar;
end;

procedure TFrmTSeriesGraph.SetTimeseriesGraphType(AValue: TTimeseriesGraphType);
var
  i: Integer;
begin
  FTimeseriesGraphType := AValue;
  for i := 0 to FSeriesList.Count-1 do
    with FSeriesList.Items[i] as TTimeseriesGraphSeries do
      SeriesType := FTimeseriesGraphType;
end;

procedure TFrmTSeriesGraph.SetLegent(AValue: Boolean);
begin
  FLegent := AValue;
  chartTSGraph.Legend.Visible := FLegent;
end;

procedure TFrmTSeriesGraph.tbtnLegentClick(Sender: TObject);
begin
  LegentVisible := tbtnLegent.Down;
end;

end.

