{******************************************************************}
{                                                                  }
{  Itia library                                                    }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Open channel properties for stage-discharge construction GUI}
unit frmhydxs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ToolWin, ComCtrls, ExtCtrls, TeeProcs, TeEngine, Chart,
  interpol, Series, contnrs, ImgList, StdCtrls, Grids, GanttCh, tsgrid,
  tshydraulics, StrGrdOd;

type
  EMyError = class(Exception);
  TSingleAction = class(TXSection)
  public
    Caption: string;
    procedure Assign(Source: TPersistent); override;
  end;

  TCurvesActionList = class(TObjectList)
  end;

  TFrmHydrXSections = class(TForm)
    ToolBar: TToolBar;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuEdit: TMenuItem;
    mnuUndo: TMenuItem;
    mnuRedo: TMenuItem;
    tbtnUndo: TToolButton;
    tbtnRedo: TToolButton;
    ImageList: TImageList;
    mnuTools: TMenuItem;
    N1: TMenuItem;
    mnuCopyCurve: TMenuItem;
    mnuPasteCurve: TMenuItem;
    N2: TMenuItem;
    mnuCopyChart: TMenuItem;
    mnuLoad: TMenuItem;
    mnuSave: TMenuItem;
    N3: TMenuItem;
    mnuNew: TMenuItem;
    N4: TMenuItem;
    mnuPrintChart: TMenuItem;
    mnuPrinterSetup: TMenuItem;
    N5: TMenuItem;
    mnuExit: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    DisabledImageList: TImageList;
    chartMain: TChart;
    seriesSection: TLineSeries;
    btnCalculate: TButton;
    edtMinimumStage: TEdit;
    edtMaximumStage: TEdit;
    lblMaxStage: TLabel;
    lblMinStage: TLabel;
    edtRoughness: TEdit;
    edtSlope: TEdit;
    lblRoughness: TLabel;
    lblSlope: TLabel;
    spinSegments: TUpDown;
    edtSegments: TEdit;
    lblSegments: TLabel;
    rgrpCoefficients: TRadioGroup;
    edtComplexCoefficient: TEdit;
    lblComplexCoefficient: TLabel;
    ToolButton1: TToolButton;
    tbtnFit: TToolButton;
    lblPointsCount: TLabel;
    edtPointsCount: TEdit;
    spinPointsCount: TUpDown;
    sgrdData: TOdStringGrid;
    btnInsert: TButton;
    btnRemove: TButton;
    N6: TMenuItem;
    mnuInsertNode: TMenuItem;
    mnuRemoveNode: TMenuItem;
    N7: TMenuItem;
    mnuRefresh: TMenuItem;
    ToolButton2: TToolButton;
    tbtnRefresh: TToolButton;
    mnuAddCircular: TMenuItem;
    N8: TMenuItem;
    mnuMoveSection: TMenuItem;
    mnuScaleSection: TMenuItem;
    mnuAddRectangular: TMenuItem;
    mnuAddTrapezoid: TMenuItem;
    chkLogSegments: TCheckBox;
    mnuAddEgg: TMenuItem;
    mnuAddPetalSection: TMenuItem;
    rgrpModel: TRadioGroup;
    procedure LFormShow(Sender: TObject);
    procedure LFormCreate(Sender: TObject);
    procedure LFormDestroy(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuRedoClick(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuPrintChartClick(Sender: TObject);
    procedure mnuPrinterSetupClick(Sender: TObject);
    procedure mnuCopyChartClick(Sender: TObject);
    procedure mnuLoadClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure LFormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCalculateClick(Sender: TObject);
    procedure rgrpCoefficientsClick(Sender: TObject);
    procedure tbtnFitClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure sgrdDataSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure mnuAddCircularClick(Sender: TObject);
    procedure mnuMoveSectionClick(Sender: TObject);
    procedure mnuScaleSectionClick(Sender: TObject);
    procedure mnuCopyCurveClick(Sender: TObject);
    procedure mnuPasteCurveClick(Sender: TObject);
    procedure mnuAddRectangularClick(Sender: TObject);
    procedure mnuAddTrapezoidClick(Sender: TObject);
    procedure mnuAddEggClick(Sender: TObject);
    procedure mnuAddPetalSectionClick(Sender: TObject);
  private
    CurrentIndex: Integer;
    ActionList: TCurvesActionList;
    CurrentPointer: TSingleAction;
    function GetIsModified: Boolean;
    function GetNewAction(OldAction: TSingleAction;
      MessageStr: string): TSingleAction;
    procedure FinilizeNewAction(AAction: TSingleAction);
    procedure Undo;
    procedure Redo;
    procedure SetControlStatus; overload;
    procedure SetControlStatus(RefreshGrid: Boolean); overload;
    procedure RedrawChartMain;
    procedure SetAxisLimits;
    function CheckForChanges: Boolean;
    procedure DrawGrid;
    procedure DrawGridNumbering;
    procedure DrawGridTitles;
    procedure SetGrid(AAction: TSingleAction);
  public
    TransientCurveList: TTransientCurveList;
    CurveNo: Integer;
    procedure HideStageCurveControls;
    procedure ReadSection(ASection: TXSection);
    procedure WriteSection(ASection: TXSection);
    property Modified: Boolean read GetIsModified;
  end;

implementation

uses Math, Dates, uiUtils, Clipbrd;

{$R *.DFM}


procedure TSingleAction.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSingleAction then
    Caption := TSingleAction(Source).Caption;
end;

procedure TFrmHydrXSections.LFormCreate(Sender: TObject);
begin
  ActionList := nil;
  try
    ActionList := TCurvesActionList.Create(True);
    CurrentPointer := nil;
    try
      CurrentPointer := TSingleAction.Create;
      CurrentPointer.Caption := '';
    except
      CurrentPointer.Free;
      raise;
    end;
    ActionList.Add(CurrentPointer);
    CurrentIndex := 0;
  except
    ActionList.Free;
    raise;
  end;
  FixComponentDecSeparators(Self);
end;

procedure TFrmHydrXSections.LFormDestroy(Sender: TObject);
begin
  ActionList.Free;
end;

procedure TFrmHydrXSections.LFormShow(Sender: TObject);
begin
  sgrdData.DefaultRowHeight :=
    sgrdData.DefaultRowHeight * Screen.PixelsPerInch div 96;
  SetControlStatus;
end;

function TFrmHydrXSections.GetNewAction(OldAction: TSingleAction;
  MessageStr: string): TSingleAction;
begin
  Result := nil;
  try
    Result := TSingleAction.Create;
    Result.Assign(OldAction);
    Result.Caption := MessageStr;
  except
    Result.Free;
    raise;
  end;
end;

procedure TFrmHydrXSections.FinilizeNewAction(AAction: TSingleAction);
var
  i: Integer;
begin
  for i := ActionList.Count-1 downto CurrentIndex+1 do
    ActionList.Delete(i);
  Inc(CurrentIndex);
  ActionList.Add(AAction);
  CurrentPointer := AAction;
end;

procedure TFrmHydrXSections.Undo;
begin
  if CurrentIndex>0 then
    Dec(CurrentIndex);
  CurrentPointer := TSingleAction(ActionList.Items[CurrentIndex]);
end;

procedure TFrmHydrXSections.Redo;
begin
  if CurrentIndex<ActionList.Count-1 then
    Inc(CurrentIndex);
  CurrentPointer := TSingleAction(ActionList.Items[CurrentIndex]);    
end;

resourcestring
  rsUndo = 'Undo';
  rsRedo = 'Redo';
  rsManningRoughness = 'Manning roughness';
  rsChezyRoughness = 'Chezy roughness';

procedure TFrmHydrXSections.SetControlStatus(RefreshGrid: Boolean);
begin
{Undo - Redo}
  mnuUndo.Caption := rsUndo;
  if (CurrentIndex>0) and (CurrentIndex<ActionList.Count) then
    mnuUndo.Caption := mnuUndo.Caption+' '+
      TSingleAction(ActionList.Items[CurrentIndex]).Caption;
  mnuRedo.Caption := rsRedo;
  if (CurrentIndex>=0) and (CurrentIndex<ActionList.Count-1) then
    mnuRedo.Caption := mnuRedo.Caption+' '+
      TSingleAction(ActionList.Items[CurrentIndex+1]).Caption;
  mnuUndo.Enabled := (CurrentIndex>0);
  tbtnUndo.Enabled := mnuUndo.Enabled;
  mnuRedo.Enabled := (CurrentIndex<ActionList.Count-1);
  tbtnRedo.Enabled := mnuRedo.Enabled;
  tbtnUndo.Hint := mnuUndo.Caption;
  tbtnRedo.Hint := mnuRedo.Caption;
{Redraw Charts}
  SetAxisLimits;
  RedrawChartMain;
{Item index}
  case rgrpCoefficients.ItemIndex of
    0: edtRoughness.Enabled := True;
    1: edtRoughness.Enabled := False;
  else
    Assert(False);
  end;
  case rgrpModel.ItemIndex of
    0: lblRoughness.Caption := rsManningRoughness;
    1: lblRoughness.Caption := rsChezyRoughness;
  else
    Assert(False);
  end;
  lblRoughness.Enabled := edtRoughness.Enabled;
  edtSlope.Enabled := edtRoughness.Enabled;
  lblSlope.Enabled := edtSlope.Enabled;
  lblComplexCoefficient.Enabled := not edtRoughness.Enabled;
  edtComplexCoefficient.Enabled := not edtRoughness.Enabled;
{Set grid}
  if RefreshGrid then DrawGrid;
{Tools}
  mnuMoveSection.Enabled := CurrentPointer.Count>0;
  mnuScaleSection.Enabled := CurrentPointer.Count>0;
end;

procedure TFrmHydrXSections.SetControlStatus;
begin
  SetControlStatus(True);
end;

procedure TFrmHydrXSections.mnuUndoClick(Sender: TObject);
begin
  Undo;
  SetControlStatus;
end;

procedure TFrmHydrXSections.mnuRedoClick(Sender: TObject);
begin
  Redo;
  SetControlStatus;
end;

procedure TFrmHydrXSections.SetAxisLimits;
var
  ADimension: Real;
begin
  with CurrentPointer do
  begin
    if Count<1 then
      Exit;
    ADimension := Max(Abs(MaxX-MinX), Abs(MaxY-MinY));
    chartMain.BottomAxis.Maximum := 1e37;
    chartMain.BottomAxis.Minimum := 0.5*(MaxX+MinX)-0.55*ADimension;
    chartMain.BottomAxis.Maximum := 0.5*(MaxX+MinX)+0.55*ADimension;
    chartMain.LeftAxis.Maximum := 1e37;
    chartMain.LeftAxis.Minimum := 0.5*(MaxY+MinY)-0.55*ADimension;
    chartMain.LeftAxis.Maximum := 0.5*(MaxY+MinY)+0.55*ADimension;
  end;
end;

procedure TFrmHydrXSections.RedrawChartMain;
var
  i: Integer;
begin
  seriesSection.Clear;
  with CurrentPointer do
    for i := 0 to Count-1 do
      seriesSection.AddXY(Nodes[i].x, Nodes[i].y, '', clGreen);
end;

procedure TFrmHydrXSections.mnuEditClick(Sender: TObject);
begin
  mnuPasteCurve.Enabled := (CurrentPointer.Count>0) and
    (StrLen(PChar((Clipboard.AsText)))>0);
end;

resourcestring
  rsResetAll = 'reset all';

procedure TFrmHydrXSections.mnuNewClick(Sender: TObject);
var
  ANewAction: TSingleAction;
begin
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsResetAll);
    ANewAction.Clear;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

procedure TFrmHydrXSections.mnuPrintChartClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    chartMain.Print;
end;

procedure TFrmHydrXSections.mnuPrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TFrmHydrXSections.mnuCopyChartClick(Sender: TObject);
begin
  chartMain.CopyToClipboardMetafile(True);
end;

resourcestring
  rsLoadFromFile = 'load from file';

function TFrmHydrXSections.GetIsModified: Boolean;
begin
  Result := CurrentIndex>0;
end;

procedure TFrmHydrXSections.mnuLoadClick(Sender: TObject);
var
  ANewAction: TSingleAction;
begin
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsLoadFromFile);
    if OpenDialog.Execute then
      ANewAction.LoadFromFile(OpenDialog.FileName);
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

procedure TFrmHydrXSections.mnuSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    CurrentPointer.WriteToFile(SaveDialog.FileName);
end;

procedure TFrmHydrXSections.mnuExitClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFrmHydrXSections.LFormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;
end;

resourcestring
  rsMaxStageShouldBeGreater =
    'Maximum stage should be greater than minimum stage';
  rsMaxStageShouldBeLessThanMaxY =
    'Maxsimum stage should not be greater than the maximum y-coordinate of '+
      'the cross-section';
  rsMaxStageShouldBeGreaterThanMinY =
    'Minimum stage should not be smaller than the the minimum y-coordinate of '+
      'the cross-section';
  rsNotSectionDefined = 'No valid cross-section defined';

procedure TFrmHydrXSections.btnCalculateClick(Sender: TObject);
var
  MinStage, MaxStage: Real;
  Roughness, Slope: Real;
  AStage, ADischarge: Real;
  i: Integer;
  ATransientCurve: TTransientCurve;
begin
  if CurrentPointer.Count<1 then
    raise Exception.Create(rsNotSectionDefined);
  MinStage := StrToFloat(edtMinimumStage.Text);
  MaxStage := StrToFloat(edtMaximumStage.Text);
  Roughness := 1;
  Slope := 1;
  case rgrpCoefficients.ItemIndex of
    0:
    begin
      Roughness := StrToFloat(edtRoughness.Text);
      Slope := StrToFloat(edtSlope.Text);
    end;
    1:
    begin
      Roughness := 1;
      Slope := Sqr(StrToFloat(edtComplexCoefficient.Text));
    end;
  else
    Assert(False);
  end;
  if MaxStage <=MinStage then
    raise Exception.Create(rsMaxStageShouldBeGreater);
  if MaxStage >= CurrentPointer.MaxY then
    raise Exception.Create(rsMaxStageShouldBeLessThanMaxY);
  if MinStage <= CurrentPointer.MinY then
    raise Exception.Create(rsMaxStageShouldBeGreaterThanMinY);
  ATransientCurve := nil;
  try
    ATransientCurve := TTransientCurve.Create(True);
    ATransientCurve.Assign(TransientCurveList[CurveNo]);
    ATransientCurve.Clear;
    for i := 0 to spinSegments.Position do
    begin
      if not chkLogSegments.Checked then
        AStage := MinStage + (MaxStage-MinStage)*i/spinSegments.Position
      else
        AStage := MinStage + (Power(10, i/spinSegments.Position)-1)*
          (MaxStage-MinStage)/9;
      ADischarge := 0; {dummy...}
      case rgrpModel.ItemIndex of
        0: ADischarge := CurrentPointer.CalcWaterDischarge(AStage,
          Roughness, Slope, ocmManning);
        1: ADischarge := CurrentPointer.CalcWaterDischarge(AStage,
          Roughness, Slope, ocmChezy);
      else
        Assert(False);
      end;
      ATransientCurve.Add(AStage, ADischarge);
    end;
    TransientCurveList[CurveNo].Assign(ATransientCurve);
  finally
    ATransientCurve.Free;
  end;
  ModalResult := mrOk;
end;

procedure TFrmHydrXSections.rgrpCoefficientsClick(Sender: TObject);
begin
  SetControlStatus;
end;

resourcestring
  rsFitOnHydrometricPoints = 'fit on hydrometric points';
  rsDeterminationFactorIs = 'Determination factor is: ';

procedure TFrmHydrXSections.tbtnFitClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  Coefficient, Determination: Real;
begin
  if CurrentPointer.Count<1 then
    Exit;
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsFitOnHydrometricPoints);
    case rgrpModel.ItemIndex of
      0: ANewAction.FitOnHydrometricPoints(TransientCurveList, CurveNo,
           spinPointsCount.Position, Coefficient, Determination, ocmManning);
      1: ANewAction.FitOnHydrometricPoints(TransientCurveList, CurveNo,
           spinPointsCount.Position, Coefficient, Determination, ocmChezy);
    else
      Assert(False);
    end;
    ShowMessage(rsDeterminationFactorIs+FloatToStr(Determination));
    edtComplexCoefficient.Text := FormatFloat('0.000', Coefficient);
    rgrpCoefficients.ItemIndex := 1;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
end;

procedure TFrmHydrXSections.DrawGridNumbering;
var
  i: Integer;
begin
  for i := 1 to sgrdData.RowCount-1 do
    sgrdData.Cells[0,i] := IntToStr(i);
end;

resourcestring
  rsXAxis = 'x';
  rsYAxis = 'y';


procedure TFrmHydrXSections.DrawGridTitles;
begin
  if sgrdData.RowCount<2 then
    Exit;
  sgrdData.FixedRows := 1;
  sgrdData.Cells[1,0] := rsXAxis;
  sgrdData.Cells[2,0] := rsYAxis;
end;

procedure TFrmHydrXSections.DrawGrid;
var
  i: Integer;
begin
  sgrdData.RowCount := CurrentPointer.Count+1;
  DrawGridNumbering;
  DrawGridTitles;
  for i := 0 to CurrentPointer.Count-1 do
  begin
    sgrdData.Cells[1,i+1] := FormatFloat('0.000',CurrentPointer.Nodes[i].x);
    sgrdData.Cells[2,i+1] := FormatFloat('0.000',CurrentPointer.Nodes[i].y);
  end;
end;

function TFrmHydrXSections.CheckForChanges: Boolean;
var
  i: Integer;
  AAction: TSingleAction;
begin
  Result := False;
  AAction := nil;
  try
    AAction := TSingleAction.Create;
    SetGrid(AAction);
    if AAction.Count <> CurrentPointer.Count then
    begin
      Result := True;
      Exit;
    end;
    for i := 0 to AAction.Count-1 do
    begin
      if not XSectionNodesEqual(AAction.Nodes[i], CurrentPointer.Nodes[i]) then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    AAction.Free;
  end;
end;

procedure TFrmHydrXSections.SetGrid(AAction: TSingleAction);
var
  i: Integer;
  AXValue, AYValue: Real;
begin
  AAction.Clear;
  for i := 1 to sgrdData.RowCount-1 do
  begin
    try
      AXValue := StrToFloat(sgrdData.Cells[1,i]);
      AYValue := StrToFloat(sgrdData.Cells[2,i]);
      AAction.Add(AXValue, AYValue);
    except
      on EConvertError do
        Continue;
      else
        raise;
    end;
  end;
end;

resourcestring
  rsRemoveXSectionNode = 'remove x-section node';

procedure TFrmHydrXSections.btnRemoveClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  i: Integer;
begin
  if sgrdData.Row<1 then
    Exit;
  for i := sgrdData.Row to sgrdData.RowCount-2 do
  begin
    sgrdData.Cells[1,i] := sgrdData.Cells[1,i+1];
    sgrdData.Cells[2,i] := sgrdData.Cells[2,i+1];
  end;
  sgrdData.RowCount := sgrdData.RowCount - 1;
  DrawGridNumbering;
  DrawGridTitles;
  if CheckForChanges then
  begin
    ANewAction := nil;
    try
      ANewAction := GetNewAction(CurrentPointer, rsRemoveXSectionNode);
      SetGrid(ANewAction);
      FinilizeNewAction(ANewAction);
    except
      ANewAction.Free;
      raise;
    end;
    SetControlStatus(False);
  end;
end;

procedure TFrmHydrXSections.btnInsertClick(Sender: TObject);
var
  i: Integer;
begin
  Assert(sgrdData.Row>-1);
  sgrdData.RowCount := sgrdData.RowCount+1;
  DrawGridNumbering;
  DrawGridTitles;
  for i := sgrdData.RowCount-1 downto sgrdData.Row+2 do
  begin
    sgrdData.Cells[1, i] := sgrdData.Cells[1, i-1];
    sgrdData.Cells[2, i] := sgrdData.Cells[2, i-1];
  end;
  sgrdData.Cells[1,sgrdData.Row+1] := '';
  sgrdData.Cells[2,sgrdData.Row+1] := '';
end;

procedure TFrmHydrXSections.btnRefreshClick(Sender: TObject);
begin
  SetControlStatus;
end;

resourcestring
  rsEditCrossSectionNodes = 'edit x-section nodes';

procedure TFrmHydrXSections.sgrdDataSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  ANewAction: TSingleAction;
begin
  if sgrdData.EditorMode then
    Exit;
  if CheckForChanges then
  begin
    ANewAction := nil;
    try
      ANewAction := GetNewAction(CurrentPointer, rsEditCrossSectionNodes);
      SetGrid(ANewAction);
      FinilizeNewAction(ANewAction);
    except
      ANewAction.Free;
      raise;
    end;
    SetControlStatus(False);
  end;
end;

resourcestring
  rsEnterCircleRadius = 'Enter circle radius';
  rsEnterCentreX = 'Enter circle centre x';
  rsEnterCentreY = 'Enter circle centre y';
  rsEnterCircularSection = 'enter circular section';

procedure TFrmHydrXSections.mnuAddCircularClick(Sender: TObject);
var
  i: Integer;
  x0, y0, radius, AXValue, AYValue: Real;
  ANewAction: TSingleAction;
  s: string;
begin
  s := '5';
  if not InputQuery(rsEnterCircleRadius, rsEnterCircleRadius, s) then Exit;
  radius := StrToFloat(s);
  s := '0';
  if not InputQuery(rsEnterCentreX, rsEnterCentreX, s) then Exit;
  x0 := StrToFloat(s);
  s := '5';
  if not InputQuery(rsEnterCentreY, rsEnterCentreY, s) then Exit;
  y0 := StrToFloat(s);
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsEnterCircularSection);
    ANewAction.Clear;
    for i := 0 to 72 do
    begin
      AXValue := x0+radius*sin((-i/72)*Pi*2);
      AYValue := y0+radius*cos((-i/72)*Pi*2);
      ANewAction.Add(AXValue, AYValue);
    end;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

resourcestring
  rsMoveSection = 'move section';
  rsGiveXOffset = 'Give x-offset';
  rsGiveYOffset = 'Give y-offset';

procedure TFrmHydrXSections.mnuMoveSectionClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  AXOffset, AYOffset: Real;
  s: string;
  i: Integer;
begin
  s := '0';
  if not InputQuery(rsGiveXOffset, rsGiveXOffset, s) then Exit;
  AXOffset := StrToFloat(s);
  s := '0';
  if not InputQuery(rsGiveYOffset, rsGiveYOffset, s) then Exit;
  AYOffset := StrToFloat(s);
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsMoveSection);
    ANewAction.Clear;
    with CurrentPointer do
      for i := 0 to Count-1 do
        ANewAction.Add(Nodes[i].x+AXOffset, Nodes[i].y+AYOffset);
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

resourcestring
  rsScaleSection = 'scale section';
  rsGiveXScale = 'Give x-scale';
  rsGiveYScale = 'Give y-scale';

procedure TFrmHydrXSections.mnuScaleSectionClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  AXScale, AYScale: Real;
  s: string;
  i: Integer;
begin
  s := '1';
  if not InputQuery(rsGiveXScale, rsGiveXScale, s) then Exit;
  AXScale := StrToFloat(s);
  if not InputQuery(rsGiveYScale, rsGiveYScale, s) then Exit;
  AYScale := StrToFloat(s);
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsScaleSection);
    ANewAction.Clear;
    with CurrentPointer do
      for i := 0 to Count-1 do
        ANewAction.Add(Nodes[i].x*AXScale, Nodes[i].y*AYScale);
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

procedure TFrmHydrXSections.mnuCopyCurveClick(Sender: TObject);
begin
  if CurrentPointer.Count<1 then
    Exit;
  StringGridToClipboard(sgrdData, 1, 2, 1, sgrdData.RowCount-1);
end;

resourcestring
  rsPasteNodesFromClipboard = 'paste nodes from clipboard';
  rsTooManyColumns = 'Too many columns of pasted text';

procedure TFrmHydrXSections.mnuPasteCurveClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  Cliptext, s: string;
  p: PChar;
  j: Integer;
  ANode: TXSectionNode;
begin
  Cliptext := Clipboard.AsText;
  j := 0;
  p := PChar(Cliptext);
  s := '';
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsPasteNodesFromClipboard);
    ANewAction.Clear;
    while p^<>#0 do
    begin
      case p^ of
        #13:
          begin
            if j=0 then
              ANode.x := StrToFloat(s)
            else
              ANode.y := StrToFloat(s);
            ANewAction.Add(ANode);
            s := '';
            Inc(p); { Skip #10 as well }
            j := 0;
          end;
        #9:
          begin
            if j=0 then
              ANode.x := StrToFloat(s)
            else
              ANode.y := StrToFloat(s);
            s := '';
            Inc(j);
            if j>1 then
              raise Exception.Create(rsTooManyColumns);
          end;
      else
        s := s+p^;
      end;
      Inc(p);
    end;
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise;
  end;
  SetControlStatus;
end;

resourcestring
  rsAddRectangularSection = 'add rectangular section';
  rsGiveBaseLength = 'Give base length';
  rsGiveWallHeight = 'Give wall height';

procedure TFrmHydrXSections.mnuAddRectangularClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  s: string;
  ABaseLength, AWallHeight: Real;
begin
  s := '10';
  if not InputQuery(rsGiveBaseLength, rsGiveBaseLength, s) then Exit;
  ABaseLength := StrToFloat(s);
  s := '5';
  if not InputQuery(rsGiveWallHeight, rsGiveWallHeight, s) then Exit;
  AWallHeight := StrToFloat(s);
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsAddRectangularSection);
    ANewAction.Clear;
    ANewAction.Add(0,AWallHeight);
    ANewAction.Add(0,0);
    ANewAction.Add(ABaseLength,0);
    ANewAction.Add(ABaseLength, AWallHeight);
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise
  end;
  SetControlStatus;
end;

resourcestring
  rsAddTrapezoidSection = 'add trapezoid section';
  rsGiveSlopeHeight = 'Give slope height';
  rsGiveSlopeRatio =
    'Give slope ratio (tangent) z (z/1, z: vertical, 1: horizontal)';

procedure TFrmHydrXSections.mnuAddTrapezoidClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  s: string;
  ABaseLength, ASlopeHeight, ASlopeRatio: Real;
begin
  s := '10';
  if not InputQuery(rsGiveBaseLength, rsGiveBaseLength, s) then Exit;
  ABaseLength := StrToFloat(s);
  s := '5';
  if not InputQuery(rsGiveSlopeHeight, rsGiveSlopeHeight, s) then Exit;
  ASlopeHeight := StrToFloat(s);
  s := '1';
  if not InputQuery(rsGiveSlopeRatio, rsGiveSlopeRatio, s) then Exit;
  ASlopeRatio := StrToFloat(s);
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsAddTrapezoidSection);
    ANewAction.Clear;
    ANewAction.Add(-ASlopeHeight/ASlopeRatio,ASlopeHeight);
    ANewAction.Add(0,0);
    ANewAction.Add(ABaseLength,0);
    ANewAction.Add(ABaseLength+ASlopeHeight/ASlopeRatio, ASlopeHeight);
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise
  end;
  SetControlStatus;
end;

resourcestring
  rsAddEggShaped = 'add egg-shaped section';
  rsGiveSectionHeight = 'Give section height';

procedure TFrmHydrXSections.mnuAddEggClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  s: string;
  AHeight: Real;
  i: Integer;
begin
  s := '10';
  if not InputQuery(rsGiveSectionHeight, rsGiveSectionHeight, s) then Exit;
  AHeight := StrToFloat(s);
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsAddEggShaped);
    ANewAction.Clear;
    for i := 0 to 17 do
      ANewAction.Add((AHeight/3)*sin((-i/72)*Pi*2),(2*AHeight/3)+
        (AHeight/3)*cos((-i/72)*Pi*2));
    for i := 0 to 17 do
      ANewAction.Add((2*AHeight/3)-AHeight*cos((i/18)*arctan(0.75)),
        (2*AHeight/3)-AHeight*sin((i/18)*arctan(0.75)));
    for i := 0 to 6 do
      ANewAction.Add(-AHeight*cos(arctan(0.75)+(i/6)*arctan(4/3))/6,
        (AHeight/6)-AHeight*sin(arctan(0.75)+(i/6)*arctan(4/3))/6);
    for i := ANewAction.Count-2 downto 0 do
      ANewAction.Add(-ANewAction.Nodes[i].x, ANewAction.Nodes[i].y);
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise
  end;
  SetControlStatus;
end;

resourcestring
  rsAddPetalSection = 'add petal-shaped section';

procedure TFrmHydrXSections.mnuAddPetalSectionClick(Sender: TObject);
var
  ANewAction: TSingleAction;
  s: string;
  AHeight, AAngle: Real;
  i: Integer;
begin
  s := '10';
  if not InputQuery(rsGiveSectionHeight, rsGiveSectionHeight, s) then Exit;
  AHeight := StrToFloat(s);
  AAngle := Pi/4-arcsin(Sqrt(2)/3);
  ANewAction := nil;
  try
    ANewAction := GetNewAction(CurrentPointer, rsAddPetalSection);
    ANewAction.Clear;
    for i := 0 to 17 do
      ANewAction.Add((AHeight/2)*sin((-i/72)*Pi*2),(AHeight/2)+
        (AHeight/2)*cos((-i/72)*Pi*2));
    for i := 0 to 17 do
      ANewAction.Add(AHeight-1.5*AHeight*cos((i/18)*AAngle),
        (AHeight/2)-1.5*AHeight*sin((i/18)*AAngle));
    for i := 18 downto 0 do
      ANewAction.Add(-AHeight*1.5*sin((i/18)*AAngle),
        1.5*AHeight-1.5*AHeight*cos((i/18)*AAngle));
    for i := ANewAction.Count-2 downto 0 do
      ANewAction.Add(-ANewAction.Nodes[i].x, ANewAction.Nodes[i].y);
    FinilizeNewAction(ANewAction);
  except
    ANewAction.Free;
    raise
  end;
  SetControlStatus;
end;

procedure TFrmHydrXSections.HideStageCurveControls;
begin
  lblPointsCount.Visible := False;
  edtPointsCount.Visible := False;
  spinPointsCount.Visible := False;
  rgrpCoefficients.Visible := False;
  rgrpModel.Visible := False;
  tbtnFit.Visible := False;
  lblRoughness.Visible := False;
  edtRoughness.Visible := False;
  lblSlope.Visible := False;
  edtSlope.Visible := False;
  lblComplexCoefficient.Visible := False;
  edtComplexCoefficient.Visible := False;
  btnCalculate.Visible := False;
  lblMinStage.Visible := False;
  edtMinimumStage.Visible := False;
  lblMaxStage.Visible := False;
  edtMaximumStage.Visible := False;
  lblSegments.Visible := False;
  edtSegments.Visible := False;
  spinSegments.Visible := False;
  chkLogSegments.Visible := False;
  Height := Height - 32;
  btnInsert.Top := 425;
  btnRemove.Top := 425;
  sgrdData.Height := 377;
end;

procedure TFrmHydrXSections.ReadSection(ASection: TXSection);
var
  i: Integer;
begin
  with CurrentPointer do
  begin
    Clear;
    for i := 0 to ASection.Count-1 do
      Add(ASection.Nodes[i]);
  end;
  SetControlStatus;
end;

procedure TFrmHydrXSections.WriteSection(ASection: TXSection);
var
  i: Integer;
begin
  with ASection do
  begin
    Clear;
    for i := 0 to CurrentPointer.Count-1 do
      Add(CurrentPointer.Nodes[i]);
  end;
end;

end.
