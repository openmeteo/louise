{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit idfsetdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tsidf, ts, tsgrid, StdCtrls, ExtCtrls;

type
  TFrmIDFSet = class(TForm)
    lstTimeseries: TListBox;
    memoTSComments: TMemo;
    memoTSRecords: TMemo;
    rgrpTimeseriesValues: TRadioGroup;
    lstIDFTimeseries: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnLoad: TButton;
    btnSave: TButton;
    btnClose: TButton;
    btnAnalysis: TButton;
    rgrpAmount: TRadioGroup;
    grpControls: TGroupBox;
    lblDuration: TLabel;
    btnAdd: TButton;
    btnRemove: TButton;
    edtDuration: TEdit;
    Panel1: TPanel;
    rdbDurationMinutes: TRadioButton;
    rdbDurationHours: TRadioButton;
    chkTimeResolutionUnknown: TCheckBox;
    pnlTimeResolution: TPanel;
    lblTimeResolution: TLabel;
    Panel2: TPanel;
    rdbResolutionMinutes: TRadioButton;
    rdbResolutionHours: TRadioButton;
    edtTimeResolution: TEdit;
    chkEtaTheta: TCheckBox;
    chkDataList: TCheckBox;
    chkExplicitSetEtaTheta: TCheckBox;
    edtExplicitEta: TEdit;
    edtExplicitTheta: TEdit;
    lblExplicitEta: TLabel;
    lblExplicitTheta: TLabel;
    procedure IFormCreate(Sender: TObject);
    procedure IFormDestroy(Sender: TObject);
    procedure IFormShow(Sender: TObject);
    procedure lstTimeseriesClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure chkTimeResolutionUnknownClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lstIDFTimeseriesClick(Sender: TObject);
    procedure rgrpAmountClick(Sender: TObject);
    procedure btnAnalysisClick(Sender: TObject);
  private
    IDFTimeseriesCollection: TIDFTimeseriesCollection;
    FTimeseriesGrid: TTimeseriesGrid;
    function CheckConsistencyHandler(AMessage: string): Integer;
    procedure SetControlStatus;
    function IDFTimeseriesCaption(AIDFTimeseries: TIDFTimeseries): string;
  public
    procedure SetTimeseriesGrid(ATimeseriesGrid: TTimeseriesGrid);
  end;

implementation

uses
  idfcurvesdlg, uiutils;

{$R *.DFM}

procedure TFrmIDFSet.IFormCreate(Sender: TObject);
begin
  IDFTimeseriesCollection := TIDFTimeseriesCollection.Create(1/3);
  FixComponentDecSeparators(Self);
end;

procedure TFrmIDFSet.IFormDestroy(Sender: TObject);
begin
  IDFTimeseriesCollection.Free;
end;

procedure TFrmIDFSet.SetTimeseriesGrid(ATimeseriesGrid: TTimeseriesGrid);
begin
  FTimeseriesGrid := ATimeseriesGrid;
end;

resourcestring
  rsTimeseriesTitleUnspecified =
    'Time series title unspecified';

procedure TFrmIDFSet.IFormShow(Sender: TObject);
var
  i: Integer;
  s: string;
begin
  lstTimeseries.Clear;
  IDFTimeseriesCollection.Clear;
  for i := 0 to FTimeseriesGrid.Count-1 do
  begin
    s := FTimeseriesGrid.Data[i].Title;
    if s = '' then
      s := rsTimeseriesTitleUnspecified;
    lstTimeseries.Items.Add(s);
  end;
  SetControlStatus;
end;

procedure TFrmIDFSet.SetControlStatus;
begin
  pnlTimeResolution.Enabled := not chkTimeResolutionUnknown.Checked;
  rdbResolutionMinutes.Enabled := pnlTimeResolution.Enabled;
  rdbResolutionHours.Enabled := pnlTimeResolution.Enabled;
  edtTimeResolution.Enabled := pnlTimeResolution.Enabled;
  lblTimeResolution.Enabled := pnlTimeResolution.Enabled;
  case Round(IDFTimeseriesCollection.DesiredAmount*100) of
    10: rgrpAmount.ItemIndex := 0;
    20: rgrpAmount.ItemIndex := 1;
    33: rgrpAmount.ItemIndex := 2;
    50: rgrpAmount.ItemIndex := 3;
    67: rgrpAmount.ItemIndex := 4;
    100: rgrpAmount.ItemIndex := 5;
  else
    Assert(False);
  end;
  lblExplicitEta.Enabled := chkExplicitSetEtaTheta.Checked;
  lblExplicitTheta.Enabled := chkExplicitSetEtaTheta.Checked;
  edtExplicitEta.Enabled := chkExplicitSetEtaTheta.Checked;
  edtExplicitTheta.Enabled := chkExplicitSetEtaTheta.Checked;
end;

resourcestring
  rsTimeseriesCommentUnspecified =
    'Time series comment unspecified';

procedure TFrmIDFSet.lstTimeseriesClick(Sender: TObject);
var
  i: Integer;
  ATimeseries: TTimeseries;
  s: string;
begin
  ATimeseries := FTimeseriesGrid.Data[lstTimeseries.ItemIndex];
  memoTSComments.Lines.Clear;
  s := ATimeseries.Comment;
  if s = '' then
    s := rsTimeseriesCommentUnspecified;
  memoTSComments.Lines.Add(s);
  memoTSRecords.Lines.Clear;
  for i := 0 to ATimeseries.Count-1 do
    memoTSRecords.Lines.Add(ATimeseries[i].DateAsString+'  '+
      ATimeseries[i].AsString);
end;

resourcestring
  rsDuration = 'Intensity (mm/h), Duration';
  rsTimeResolution = 'Time Resolution';
  rsUnknown = 'Unknown';

function TFrmIDFSet.IDFTimeseriesCaption(AIDFTimeseries: TIDFTimeseries): string;
begin
  Result := rsDuration+': ';
  if AIDFTimeseries.Duration< 1 then
    Result := Result + FloatToStr(AIDFTimeseries.Duration *60)+' min. '
  else
    Result := Result + FloatToStr(AIDFTimeseries.Duration)+' hours ';
  Result := Result + rsTimeResolution+': ';
  if AIDFTimeseries.BaseTimeStep < 0.000005 then
    Result := Result + rsUnknown else
  begin
    if AIDFTimeseries.BaseTimeStep< 1 then
      Result := Result + FloatToStr(AIDFTimeseries.BaseTimeStep *60)+' min. '
    else
      Result := Result + FloatToStr(AIDFTimeseries.BaseTimeStep)+' hours ';
  end;
end;

resourcestring
  rsTimeseriesShouldBeAnnual =
    'Time series should be of annual time step';
  rsDuplicateNotPermited =
    'Duplicate duration not permited';

procedure TFrmIDFSet.btnAddClick(Sender: TObject);
var
  i, AIndex: Integer;
  ATimeseries: TTimeseries;
  ADuration, ABaseTimeStep: Real;
  AIDFTimeseriesType: TIDFTimeseriesType;
begin
  AIndex := lstTimeseries.ItemIndex;
  if AIndex <0 then
    Exit;
  ATimeseries := FTimeseriesGrid.Data[AIndex];
  if ATimeseries.TimeStep <> tstAnnual then
    raise Exception.Create(rsTimeseriesShouldBeAnnual);
  AIDFTimeseriesType := idftsHeight;
  case rgrpTimeseriesValues.ItemIndex of
    0: AIDFTimeseriesType := idftsHeight;
    1: AIDFTimeseriesType := idftsIntensity;
    else Assert(False);
  end;
  ADuration := StrToFloat(edtDuration.Text);
  if rdbDurationMinutes.Checked then ADuration := ADuration / 60;
  if chkTimeResolutionUnknown.Checked then
    ABaseTimeStep := 0.000001
  else begin
    ABaseTimeStep := StrToFloat(edtTimeResolution.Text);
    if rdbResolutionMinutes.Checked then ABaseTimeStep := ABaseTimeStep / 60;
  end;
  AIndex := -1;
  with IDFTimeseriesCollection do
    for i := 0 to Count-1 do
    begin
      if ADuration<Items[i].Duration then
      begin
        AIndex := i;
        Break;
      end;
      if ADuration=Items[i].Duration then
        raise Exception.Create(rsDuplicateNotPermited);
    end;
  if AIndex<0 then
  IDFTimeseriesCollection.Add(ATimeseries,AIDFTimeseriesType,ADuration,
    ABaseTimeStep) else
  IDFTimeseriesCollection.Insert(AIndex,ATimeseries,AIDFTimeseriesType,ADuration,
    ABaseTimeStep);
  IDFTimeseriesCollection.Last.IsActiveEtaTheta :=
    chkEtaTheta.Checked;
  IDFTimeseriesCollection.Last.IsActiveDataList :=
    chkDataList.Checked;
  if AIndex<0 then
  lstIDFTimeseries.Items.Add(IDFTimeseriesCaption(IDFTimeseriesCollection.Last))
    else
  lstIDFTimeseries.Items.Insert(AIndex,
    IDFTimeseriesCaption(IDFTimeseriesCollection.Items[AIndex]));
  SetControlStatus;
end;

procedure TFrmIDFSet.chkTimeResolutionUnknownClick(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmIDFSet.btnRemoveClick(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := lstIDFTimeseries.ItemIndex;
  if AIndex <0 then
    Exit;
  IDFTimeseriesCollection.Delete(AIndex);
  lstIDFTimeseries.Items.Delete(AIndex);
  SetControlStatus;
end;

procedure TFrmIDFSet.lstIDFTimeseriesClick(Sender: TObject);
var
  i: Integer;
  AIDFTimeseries: TIDFTimeseries;
  ATimeseries: TTimeseries;
begin
  if lstIDFTimeseries.ItemIndex<0 then Exit;
  AIDFTimeseries := IDFTimeseriesCollection.Items[lstIDFTimeseries.ItemIndex];
  ATimeseries := AIDFTimeseries.Timeseries;
  memoTSComments.Lines.Clear;
  memoTSComments.Lines.Add(IDFTimeseriesCaption(AIDFTimeseries));
  memoTSRecords.Lines.Clear;
  for i := 0 to ATimeseries.Count-1 do
    memoTSRecords.Lines.Add(ATimeseries[i].DateAsString+'  '+
      ATimeseries[i].AsString);
  chkEtaTheta.Checked := AIDFTimeseries.IsActiveEtaTheta;
  chkDataList.Checked := AIDFTimeseries.IsActiveDataList;
  if AIDFTimeseries.Duration <1 then
  begin
    rdbDurationMinutes.Checked := True;
    edtDuration.Text := FloatToStr(AIDFTimeseries.Duration*60);
  end else begin
    rdbDurationHours.Checked := True;
    edtDuration.Text := FloatToStr(AIDFTimeseries.Duration);
  end;
  chkTimeResolutionUnknown.Checked := (AIDFTimeseries.BaseTimeStep < 0.000005);
  if not chkTimeResolutionUnknown.Checked then
  begin
    if AIDFTimeseries.BaseTimeStep <1 then
    begin
      rdbResolutionMinutes.Checked := True;
      edtTimeResolution.Text := FloatToStr(AIDFTimeseries.BaseTimeStep*60);
    end else begin
      rdbResolutionHours.Checked := True;
      edtTimeResolution.Text := FloatToStr(AIDFTimeseries.BaseTimeStep);
    end;
  end else begin
    rdbResolutionMinutes.Checked := True;
    edtTimeResolution.Text := '';
  end;
end;

procedure TFrmIDFSet.rgrpAmountClick(Sender: TObject);
begin
  case rgrpAmount.ItemIndex of
    0: IDFTimeseriesCollection.DesiredAmount := 0.100;
    1: IDFTimeseriesCollection.DesiredAmount := 0.200;
    2: IDFTimeseriesCollection.DesiredAmount := 1/3;
    3: IDFTimeseriesCollection.DesiredAmount := 0.500;
    4: IDFTimeseriesCollection.DesiredAmount := 2/3;
    5: IDFTimeseriesCollection.DesiredAmount := 1.000;
  else
    Assert(False);
  end;
end;

resourcestring
  rsFixValues =
    '. Fixing inconsistency by setting the second value to the first? '+
    'Press Yes to fix, No to continue without fixing or Cancel to '+
    'halt calculations.';

function TFrmIDFSet.CheckConsistencyHandler(AMessage: string): Integer;
begin
  Result := 0;
  case MessageDlg(AMessage+ rsFixValues, mtWarning, mbYesNoCancel,
    0) of
    mrYes: Result := idfcFix;
    mrNo: Result := idfcContinue;
    mrCancel: Result := idfcAbort;
  else
    Assert(False);
  end;
end;

resourcestring
  rsTooFewTimeseries = 'Too few time series for IDF analysis '+
    '(should be at least ';

procedure TFrmIDFSet.btnAnalysisClick(Sender: TObject);
var
  IDFCurvesDialog: TFrmIDFCurves;
  AReqTSCount: Integer;
begin
  if chkExplicitSetEtaTheta.Checked then
    AReqTSCount := 1 else
    AReqTSCount := 2;
  if IDFTimeseriesCollection.Count <AReqTSCount then
    raise Exception.Create(rsTooFewTimeseries+IntToStr(AReqTSCount)+' )');
  IDFTimeseriesCollection.CheckConsistency(CheckConsistencyHandler);
  IDFCurvesDialog := nil;
  try
    IDFCurvesDialog := TFrmIDFCurves.Create(Self);
    IDFCurvesDialog.IDFTimeseriesCollection :=
      IDFTimeseriesCollection;
    if chkExplicitSetEtaTheta.Checked then
    begin
      IDFCurvesDialog.ExplicitEtaTheta := True;
      IDFCurvesDialog.ExplicitEta :=
        StrToFloat(edtExplicitEta.Text);
      IDFCurvesDialog.ExplicitTheta :=
        StrToFloat(edtExplicitTheta.Text);
    end;
    IDFCurvesDialog.ShowModal;
  finally
    IDFCurvesDialog.Free;
  end;
end;

end.
