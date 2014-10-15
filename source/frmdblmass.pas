{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{Double mass curve GUI}
unit frmdblmass;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, tsprocess, VclTee.TeEngine, VclTee.Series, ExtCtrls,
  VclTee.TeeProcs, VclTee.Chart, ImgList, ComCtrls, ToolWin, Menus, Dates,
  contnrs, tsdblmass, VclTee.TeeGDIPlus;

type
  TDoubleMassStatus = (dbmsNormal, dbmsMove, dbmsRotate, dbmsSeekMove,
    dbmsSeekRotate);

type
  TArrayOfReal = array of Real;

type
  TFrmDoubleMass = class(TForm)
    BtnOK: TButton;
    ImageList: TImageList;
    mnuMainMenu: TMainMenu;
    mnuFile: TMenuItem;
    Chart: TChart;
    seriesPoints: TPointSeries;
    seriesMark: TPointSeries;
    btnCancel: TButton;
    lineFull: TLineSeries;
    lineRotateLeft: TLineSeries;
    lineRotateRight: TLineSeries;
    lineMoveLeft: TLineSeries;
    lineMoveRight: TLineSeries;
    mnuEdit: TMenuItem;
    mnuReverse: TMenuItem;
    mnuPrint: TMenuItem;
    mnuPrinterSetup: TMenuItem;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    seriesStartEnd: TPointSeries;
    ToolBar: TToolBar;
    btnUndo: TToolButton;
    btnRedo: TToolButton;
    ToolButton3: TToolButton;
    btnRotate: TToolButton;
    btnMagicRotate: TToolButton;
    ToolButton4: TToolButton;
    btnMove: TToolButton;
    btnMagicMove: TToolButton;
    ToolButton6: TToolButton;
    btnLeft: TToolButton;
    btnRight: TToolButton;
    ToolButton1: TToolButton;
    btnProceed: TToolButton;
    btnAbort: TToolButton;
    ToolButton5: TToolButton;
    btnReverse: TToolButton;
    N1: TMenuItem;
    mnuUndo: TMenuItem;
    mnuRedo: TMenuItem;
    N2: TMenuItem;
    Copycharttoclipboard1: TMenuItem;
    N3: TMenuItem;
    mnuProceed: TMenuItem;
    mnuAbort: TMenuItem;
    procedure IFormShow(Sender: TObject);
    procedure btnRotateClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure ChartClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure btnAbortClick(Sender: TObject);
    procedure btnLeftClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure btnMagicRotateClick(Sender: TObject);
    procedure btnMagicMoveClick(Sender: TObject);
    procedure btnProceedClick(Sender: TObject);
    procedure mnuReverseClick(Sender: TObject);
    procedure mnuPrintClick(Sender: TObject);
    procedure mnuPrinterSetupClick(Sender: TObject);
    procedure IFormCreate(Sender: TObject);
    procedure IFormDestroy(Sender: TObject);
    procedure IFormClose(Sender: TObject; var Action: TCloseAction);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuRedoClick(Sender: TObject);
    procedure Copycharttoclipboard1Click(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure IFormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FUndoPointer: Integer;
    FActionList: TObjectList;
    FSelectedPoint: Integer;
    InputStatus: TDoubleMassStatus;
    FFa, FFb, FFrxy: Real;
    FLa, FLb, FLrxy: Real;
    FRa, FRb, FRrxy: Real;
    procedure SetControlStatus;
    procedure DrawPoints;
    procedure CalcParams;
    procedure DrawLines;
    procedure UpdateMoveTitle;
  public
    DoubleMassCurve: TDoubleMassCurve;
  end;

implementation

{$R *.DFM}

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
  rsHomogeneity = 'homogeneity restoration';
  rsInconsistency = 'outlier removal';
  rsReverse = 'dates reverse';
  rsDeterminationFactorEq = ' Determination factor=';
  rsHomogeneousLineSlope = 'Homogeneous line, slope l=';
  rsLeftLineSlope = 'Left line slope l1=';
  rsRightLineSlope = ', right line slope l2=';
  rsReductionFactorEq = ', reduction factor m=l1/l2=';  

procedure TFrmDoubleMass.SetControlStatus;
  function GetActionName(Action: TDblMassAction): string;
  begin
    case Action.ActionType of
      dmatRotate: Result := rsHomogeneity;
      dmatMove: Result := rsInconsistency;
      dmatReverse: Result := rsReverse;
      else Assert(False);
    end;
  end;

begin
  case InputStatus of
    dbmsNormal:
      begin
        btnMove.Down := False;
        btnRotate.Down := False;
        btnProceed.Enabled := False;
        seriesPoints.Cursor := crDefault;
        lineFull.Active := True;
        lineRotateLeft.Active := False;
        lineMoveLeft.Active := False;
        lineFull.Title := rsHomogeneousLineSlope+
          FormatFloat('#.###',FFb)+rsDeterminationFactorEq+
          FormatFloat('#.###',FFrxy);
      end;
    dbmsMove:
      begin
        btnMove.Down := False;
        btnRotate.Down := False;
        btnProceed.Enabled := True;
        seriesPoints.Cursor := crDefault;
        lineFull.Active := False;
        lineRotateLeft.Active := False;
        lineMoveLeft.Active := True;

      end;
    dbmsRotate:
      begin
        btnMove.Down := False;
        btnRotate.Down := False;
        btnProceed.Enabled := True;
        seriesPoints.Cursor := crDefault;
        lineFull.Active := False;
        lineRotateLeft.Active := True;
        lineMoveLeft.Active := False;
        seriesMark.Title := rsLeftLineSlope+
          FormatFloat('#.###',FLb)+rsRightLineSlope+
          FormatFloat('#.###',FRb)+rsReductionFactorEq+
          FormatFloat('#.###',FLb/FRb);
      end;
    dbmsSeekMove:
      begin
        btnMove.Down := True;
        btnRotate.Down := False;
        btnProceed.Enabled := False;
        seriesPoints.Cursor := crCross;
        lineFull.Active := False;
        lineRotateLeft.Active := False;
        lineMoveLeft.Active := False;
      end;
    dbmsSeekRotate:
      begin
        btnMove.Down := False;
        btnRotate.Down := True;
        btnProceed.Enabled := False;
        seriesPoints.Cursor := crCross;
        lineFull.Active := False;
        lineRotateLeft.Active := False;
        lineMoveLeft.Active := False;
      end;
    else
      Assert(False);
  end;
  seriesMark.Active := btnProceed.Enabled;
  lineMoveRight.Active := lineMoveLeft.Active;
  lineRotateRight.Active := lineRotateLeft.Active;
  btnAbort.Enabled := btnProceed.Enabled;
  btnLeft.Enabled := btnProceed.Enabled;
  btnRight.Enabled := btnProceed.Enabled;
  mnuUndo.Enabled := (FUndoPointer>-1);
  mnuRedo.Enabled := (FUndoPointer < FActionList.Count-1);
  btnUndo.Enabled := mnuUndo.Enabled;
  btnRedo.Enabled := mnuRedo.Enabled;
  mnuUndo.Caption := 'Undo ';
  mnuRedo.Caption := 'Redo ';
  if mnuUndo.Enabled then
    mnuUndo.Caption := mnuUndo.Caption+
      GetActionName(TDblMassAction(FActionList.Items[FUndoPointer]));
  if mnuRedo.Enabled then
    mnuRedo.Caption := mnuRedo.Caption+
      GetActionName(TDblMassAction(FActionList.Items[FUndoPointer+1]));
end;

procedure TFrmDoubleMass.IFormShow(Sender: TObject);
begin
  FUndoPointer := -1;
  FActionList.Clear;
  FSelectedPoint := 1;
  InputStatus := dbmsNormal;
  DrawPoints;
  CalcParams;
  DrawLines;
  SetControlStatus;
end;

procedure TFrmDoubleMass.DrawPoints;
var
  i: Integer;
  ADateTimeList: TDateTimeList;
begin
  seriesPoints.Clear;
  seriesMark.Clear;
  seriesStartEnd.Clear;
  ADateTimeList := nil;
  try
    ADateTimeList := TDateTimeList.Create;
    for i := 0 to Length(DoubleMassCurve)-1 do
    begin
      if DoubleMassCurve[i].Active then
      begin
        ADateTimeList.Add(DoubleMassCurve[i].Date);
        seriesPoints.AddXY(DoubleMassCurve[i].Independent,
          DoubleMassCurve[i].Dependent, '', clBlack);
      end;
    end;
    seriesMark.AddXY(seriesPoints.XValues[FSelectedPoint],
      seriesPoints.YValues[FSelectedPoint],
      FormatDateTime('yyyy/mm',ADateTimeList.Items[FSelectedPoint]), clGreen);
    seriesStartEnd.AddXY(seriesPoints.XValues.First,
      seriesPoints.YValues.First,
      FormatDateTime('yyyy/mm',ADateTimeList.First), clGreen);
    seriesStartEnd.AddXY(seriesPoints.XValues.Last,
      seriesPoints.YValues.Last,
      FormatDateTime('yyyy/mm',ADateTimeList.Last),
      clGreen);
  finally
    ADateTimeList.Free;
  end;
end;

procedure TFrmDoubleMass.btnRotateClick(Sender: TObject);
begin
  if InputStatus = dbmsSeekRotate then
    InputStatus := dbmsNormal else
    InputStatus := dbmsSeekRotate;
  DrawPoints;
  CalcParams;
  DrawLines;
  SetControlStatus;
end;

procedure TFrmDoubleMass.btnMoveClick(Sender: TObject);
begin
  if InputStatus = dbmsSeekMove then
    InputStatus := dbmsNormal else
    InputStatus := dbmsSeekMove;
  SetControlStatus;
end;

procedure TFrmDoubleMass.ChartClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if InputStatus = dbmsSeekRotate then
    InputStatus := dbmsRotate
  else if InputStatus = dbmsSeekMove then
    InputStatus := dbmsMove;
  if seriesPoints.Count < 5 then
    InputStatus := dbmsNormal;
  FSelectedPoint := ValueIndex;
  if ValueIndex > seriesPoints.Count - 2 then
    FSelectedPoint := seriesPoints.Count - 2;
  if ValueIndex < 1 then
    FSelectedPoint := 1;
  DrawPoints;
  CalcParams;
  DrawLines;
  UpdateMoveTitle;  
  SetControlStatus;
end;

procedure TFrmDoubleMass.btnAbortClick(Sender: TObject);
begin
  InputStatus := dbmsNormal;
  DrawPoints;
  CalcParams;
  DrawLines;
  SetControlStatus
end;

procedure TFrmDoubleMass.btnLeftClick(Sender: TObject);
begin
  Dec(FSelectedPoint);
  if FSelectedPoint < 1 then FSelectedPoint := 1;
  DrawPoints;
  CalcParams;
  DrawLines;
  UpdateMoveTitle;
  SetControlStatus;
end;

procedure TFrmDoubleMass.btnRightClick(Sender: TObject);
begin
  Inc(FSelectedPoint);
  if FSelectedPoint > seriesPoints.Count - 2 then
    FSelectedPoint := seriesPoints.Count - 2;
  DrawPoints;
  CalcParams;
  DrawLines;
  UpdateMoveTitle;
  SetControlStatus;
end;

resourcestring
  rsValueOf = 'Value of: ';

procedure TFrmDoubleMass.UpdateMoveTitle;
var
  AValue: Real;
begin
    AValue := seriesPoints.YValue[0];
    if FSelectedPoint>0 then
      AValue := seriesPoints.YValue[FSelectedPoint]-
      seriesPoints.YValues[FSelectedPoint-1];
    seriesMark.Title := rsValueOf+FormatFloat('#.##',AValue);
end;

procedure TFrmDoubleMass.CalcParams;
var
  XValues, YValues: TArrayOfReal;
begin
  try
    SetLength(XValues, seriesPoints.Count);
    SetLength(YValues, seriesPoints.Count);
    XValues := ChartSeries2XValues(seriesPoints);
    YValues := ChartSeries2YValues(seriesPoints);
    case InputStatus of
      dbmsNormal:
        begin
          FitHomogeneousLineOnChartSeries(XValues, YValues, 0,
            seriesPoints.Count-1, FFa, FFb);
          FFrxy := BiLDeterminationFactor(XValues, YValues, 0,
            0, 0, FFa, FFb);
        end;
      dbmsRotate:
        begin
          FitHomogeneousLineOnChartSeries(XValues, YValues, 0,
            FSelectedPoint, FLa, FLb);
          FitLineOnChartSeriesWithConstraint(XValues, YValues, FSelectedPoint,
            seriesPoints.Count-1, FLb, FRa, FRb);
          FFrxy := BiLDeterminationFactor( XValues, YValues, FSelectedPoint,
            FLa, FLb, FRa, FRb);
        end;
      dbmsMove:
        begin
          FitLineOnChartSeries(XValues, YValues, 0, FSelectedPoint-1,
            FLa, FLb, FLrxy);
          FitLineOnChartSeries(XValues, YValues, FSelectedPoint,
            seriesPoints.Count-1, FRa, FRb, FRrxy);
          FFrxy := BiLDeterminationFactor(XValues, YValues, FSelectedPoint,
            FLa, FLb, FRa, FRb);
        end;
    end;
  finally
    SetLength(XValues, 0);
    SetLength(YValues, 0);
  end;
end;

procedure TFrmDoubleMass.DrawLines;
begin
  lineFull.Clear;
  lineRotateLeft.Clear;
  lineRotateRight.Clear;
  lineMoveLeft.Clear;
  lineMoveRight.Clear;
  lineFull.AddXY(seriesPoints.XValues.First,
    FFa+FFb*seriesPoints.XValues.First,'');
  lineFull.AddXY(seriesPoints.XValues.Last,
    FFa+FFb*seriesPoints.XValues.Last,'');
  lineRotateLeft.AddXY(seriesPoints.XValues.First,
    FLa+FLb*seriesPoints.XValues.First,'');
  lineRotateLeft.AddXY(seriesPoints.XValue[FSelectedPoint],
    FLa+FLb*seriesPoints.XValues[FSelectedPoint],'');
  lineRotateRight.AddXY(seriesPoints.XValue[FSelectedPoint],
    FRa+FRb*seriesPoints.XValue[FSelectedPoint],'');
  lineRotateRight.AddXY(seriesPoints.XValues.Last,
    FRa+FRb*seriesPoints.XValues.Last,'');
  lineMoveLeft.AddXY(seriesPoints.XValues.First,
    FLa+FLb*seriesPoints.XValues.First,'');
  if FSelectedPoint>0 then
    lineMoveLeft.AddXY(seriesPoints.XValue[FSelectedPoint-1],
      FLa+FLb*seriesPoints.XValues[FSelectedPoint-1],'');
  lineMoveRight.AddXY(seriesPoints.XValue[FSelectedPoint],
    FRa+FRb*seriesPoints.XValue[FSelectedPoint],'');
  lineMoveRight.AddXY(seriesPoints.XValues.Last,
  FRa+FRb*seriesPoints.XValues.Last,'');
end;

procedure TFrmDoubleMass.btnMagicRotateClick(Sender: TObject);
var
  i,j: Integer;
  AValue, ATestValue: Real;
  ACursor: TCursor;
begin
  if seriesPoints.Count < 5 then
  begin
    InputStatus := dbmsNormal;
    Exit;
  end;
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    i := 0;
    AValue := 0;
    InputStatus := dbmsRotate;
    for j := seriesPoints.Count - 3 downto 2 do
    begin
      FSelectedPoint := j;
      CalcParams;
      ATestValue := FFrxy;
      if ATestValue > AValue then
      begin
        AValue := ATestValue;
        i := j;
      end;
    end;
    FSelectedPoint := i;
    DrawPoints;
    CalcParams;
    DrawLines;
    SetControlStatus;
  finally
    Screen.Cursor := ACursor;
  end;
end;

resourcestring
  rsIsSuperiorOutlier = ' is superior outlier with confidence level of 95%, ';
  rsMoreOutliers = ' more outliers';
  
procedure TFrmDoubleMass.btnMagicMoveClick(Sender: TObject);
var
  j,n: Integer;
  AValue, Dsup: Real;
  ACursor: TCursor;
  XValues, YValues: TArrayOfReal;
begin
  ACursor := Screen.Cursor;
  try
    SetLength(XValues, seriesPoints.Count);
    SetLength(YValues, seriesPoints.Count);
    XValues := ChartSeries2XValues(seriesPoints);
    YValues := ChartSeries2YValues(seriesPoints);
    Screen.Cursor := crHourglass;
    FindSupOutlier(XValues, YValues, n, j, Dsup);
  finally
    Screen.Cursor := ACursor;
    SetLength(XValues, 0);
    SetLength(YValues, 0);
  end;
  if IsOutlier(n, Dsup) then
  begin
    AValue := seriesPoints.YValue[0];
    if j>0 then
      AValue := seriesPoints.YValue[j]-seriesPoints.YValues[j-1];
    seriesMark.Title := rsValueOf+FormatFloat('#.##',AValue)+
      rsIsSuperiorOutlier + IntToStr(seriesPoints.Count-1-n)+ rsMoreOutliers;
    FSelectedPoint := j;
    InputStatus := dbmsMove;
  end;
  DrawPoints;
  CalcParams;
  DrawLines;
  SetControlStatus;
end;

procedure TFrmDoubleMass.btnProceedClick(Sender: TObject);
var
  Action: TDblMassAction;
  i: Integer;
  AValue, ATestValue: Real;
begin
  Action := nil;
  try
    Action := TDblMassAction.Create;
    AValue := Abs(seriesPoints.MaxXValue - seriesPoints.MinXValue);
    for i := 0 to Length(DoubleMassCurve) -1 do
    begin
      if not DoubleMassCurve[i].Active then Continue;
      ATestValue := Abs(seriesPoints.XValue[FSelectedPoint] -
      DoubleMassCurve[i].Independent);
      if ATestValue < AValue then
      begin
        AValue := ATestValue;
        Action.Point := i;
      end;
    end;
    case InputStatus of
      dbmsRotate:
      begin
        Action.ActionType := dmatRotate;
        Action.Lambda := FLb / FRb;
      end;
      dbmsMove:
      begin
        Action.ActionType := dmatMove;
        Action.Lambda := 1;
      end;
      else
        Assert(False);
    end;
    ReduceDblMassCurve(DoubleMassCurve, Action);
    Inc(FUndoPointer);
    FActionList.Count := FUndoPointer;
    FActionList.Add(Action);
    Action := nil;
    FSelectedPoint := 1;
    InputStatus := dbmsNormal;
    DrawPoints;
    CalcParams;
    DrawLines;
    SetControlStatus;
  finally
    Action.Free;
  end;
end;

procedure TFrmDoubleMass.mnuReverseClick(Sender: TObject);
var
  Action: TDblMassAction;
begin
  Action := nil;
  try
    Action := TDblMassAction.Create;
    Action.ActionType := dmatReverse;
    ReduceDblMassCurve(DoubleMassCurve, Action );
    Inc(FUndoPointer);
    FActionList.Count := FUndoPointer;
    FActionList.Add(Action);
    Action := nil;
  finally
    Action.Free;
  end;
  InputStatus := dbmsNormal;
  DrawPoints;
  CalcParams;
  DrawLines;
  SetControlStatus;
end;

procedure TFrmDoubleMass.mnuPrintClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    Chart.Print;
end;

procedure TFrmDoubleMass.mnuPrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TFrmDoubleMass.IFormCreate(Sender: TObject);
begin
  FActionList := TObjectList.Create;
end;

procedure TFrmDoubleMass.IFormDestroy(Sender: TObject);
begin
  FActionList.Destroy;
end;

procedure TFrmDoubleMass.IFormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FActionList.Clear;
end;

procedure TFrmDoubleMass.mnuUndoClick(Sender: TObject);
begin
  if FUndoPointer<0 then
    Assert(False);
  UnreduceDblMassCurve(DoubleMassCurve,
    TDblMassAction(FActionList[FUndoPointer]) );
  Dec(FUndoPointer);
  InputStatus := dbmsNormal;
  DrawPoints;
  CalcParams;
  DrawLines;
  SetControlStatus;
end;

procedure TFrmDoubleMass.mnuRedoClick(Sender: TObject);
begin
  if FUndoPointer >FActionList.Count-1 then
    Assert(False);
  Inc(FUndoPointer);
  ReduceDblMassCurve(DoubleMassCurve,
    TDblMassAction(FActionList[FUndoPointer]) );
  InputStatus := dbmsNormal;
  DrawPoints;
  CalcParams;
  DrawLines;
  SetControlStatus;
end;

procedure TFrmDoubleMass.Copycharttoclipboard1Click(Sender: TObject);
begin
  chart.CopyToClipboardMetafile(True);
end;

resourcestring
  rsSomeChanges =
    'You have not proceed to the last requested action. Press OK, but '+
    'last action will be lost, or press Cancel to return';

procedure TFrmDoubleMass.BtnOKClick(Sender: TObject);
begin
  if InputStatus <> dbmsNormal then
  begin
    if MessageDlg(rsSomeChanges, mtConfirmation, mbOKCancel, 0) = mrOK then
    begin
      FUndoPointer := -1;
      ModalResult := mrOK;
    end;
  end else
  begin
    FUndoPointer := -1;
    ModalResult := mrOK;
  end;
end;

resourcestring
  rsAbortChanges =
    'Some changes have being done. Press OK to leave changes and abort, '+
    'or press Cancel to return';

procedure TFrmDoubleMass.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmDoubleMass.IFormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
  if FUndoPointer > -1 then
  begin
    if MessageDlg(rsAbortChanges, mtConfirmation, mbOKCancel, 0) = mrOK then
      CanClose := True;
  end else
    CanClose := True;
end;

end.
