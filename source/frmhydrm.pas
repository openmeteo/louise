{******************************************************************}
{                                                                  }
{  Itia library                                                    }
{                                                                  }
{  Copyright (c) 2000-09 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Hydrometry unit}
unit frmhydrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, ExtCtrls, Tabs, VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs,
    VclTee.Chart, frmhydxs, tshydraulics, StdCtrls, Math, Menus, contnrs, Dates,
    Mask, uiutils, ts, inifiles, istrutils, Grids, ComCtrls, VclTee.TeeGDIPlus;

const
  SectionsCount = 10;

type
  THydrometrySession = class
  public
    Slices: TSlicesMasterArray;
    Date: TDateTime;
    Stage: Real;
    SectionIndex, ID: Integer;
    IsMeasuredFromCut: Boolean;
  end;

type
  TFrmHydrometry = class(TForm)
    tbsSections: TTabSet;
    pnlSections: TPanel;
    chartSection: TChart;
    seriesGround: TLineSeries;
    btnEditSection: TButton;
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    grpSessions: TGroupBox;
    lstSessions: TListBox;
    btnAddSession: TButton;
    btnRemove: TButton;
    grpProperties: TGroupBox;
    lblStage: TLabel;
    edtStage: TEdit;
    edtDate: TMaskEdit;
    lblDate: TLabel;
    btnChangeSessionSection: TButton;
    btnSaveStageDateValidate: TButton;
    seriesCutSection: TLineSeries;
    grpSlices: TGroupBox;
    lstSlices: TListBox;
    btnAddSlice: TButton;
    btnRemoveSlice: TButton;
    btnChangeSlice: TButton;
    lblSlicePosition: TLabel;
    edtSlicePosition: TEdit;
    cmbSliceMode: TComboBox;
    btnValidateSlices: TButton;
    seriesMeasurePoints: TPointSeries;
    seriesSlices: TLineSeries;
    chkFromCut: TCheckBox;
    rgrpOutputTimeseries: TRadioGroup;
    btnCalculateTimeseries: TButton;
    mnuNew: TMenuItem;
    N1: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSaveAs: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    mnuSave: TMenuItem;
    mnuCalculate: TMenuItem;
    N3: TMenuItem;
    Exitwithoutcalculations1: TMenuItem;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    N4: TMenuItem;
    mnuPrintSection: TMenuItem;
    mnuPrinterSetup: TMenuItem;
    mnuEdit: TMenuItem;
    mnuCopyChart: TMenuItem;
    grpMeasurements: TGroupBox;
    grdMeasurements: TStringGrid;
    chartMeasurements: TChart;
    seriesVelocityCurve: TLineSeries;
    btnSaveMeasurements: TButton;
    seriesVelocitiesMeasurements: TPointSeries;
    grpMeasurementsNumber: TGroupBox;
    edtMeasurementsNum: TEdit;
    spnMeasurementsNum: TUpDown;
    chkActualDepth: TCheckBox;
    mnuPrintVelocityProfile: TMenuItem;
    mnuCopyvelocityprofile: TMenuItem;
    N5: TMenuItem;
    mnuCopymeasurementstable: TMenuItem;
    seriesSectionMeasurements: TPointSeries;
    mnuProcess: TMenuItem;
    mnuCalculateOnly: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure tbsSectionsClick(Sender: TObject);
    procedure btnEditSectionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddSessionClick(Sender: TObject);
    procedure lstSessionsClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnChangeSessionSectionClick(Sender: TObject);
    procedure btnSaveStageDateValidateClick(Sender: TObject);
    procedure btnAddSliceClick(Sender: TObject);
    procedure lstSlicesClick(Sender: TObject);
    procedure btnRemoveSliceClick(Sender: TObject);
    procedure btnChangeSliceClick(Sender: TObject);
    procedure btnValidateSlicesClick(Sender: TObject);
    procedure btnCalculateTimeseriesClick(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
    procedure rgrpOutputTimeseriesClick(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Exitwithoutcalculations1Click(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuPrintSectionClick(Sender: TObject);
    procedure mnuPrinterSetupClick(Sender: TObject);
    procedure mnuCopyChartClick(Sender: TObject);
    procedure chkActualDepthClick(Sender: TObject);
    procedure spnMeasurementsNumClick(Sender: TObject; Button: TUDBtnType);
    procedure btnSaveMeasurementsClick(Sender: TObject);
    procedure mnuPrintVelocityProfileClick(Sender: TObject);
    procedure mnuCopyvelocityprofileClick(Sender: TObject);
    procedure mnuCopymeasurementstableClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FModifiedFlag: Boolean;
    InitialCaption: string;
    Sections: array of TXSection;
    HydrometrySessionList: TObjectList;
    procedure SetControlStatus;
    procedure SetAxisLimits;
    procedure DrawSectionGraph;
    procedure DrawCutSection;
    procedure DrawSlices;
    procedure RefreshListboxes;
    procedure RefreshGrid;
    procedure OpenAll(AFileName: string);
    procedure SaveAll(AFileName: string);
    procedure SetModifiedFlag(AValue: Boolean);
    procedure ResetAll;
    function ModifiedSaveChangesConfirmation: Boolean;
  public
    StageTimeseries, DischargeTimeseries: TTimeseries;
    property ModifiedFlag: Boolean read FModifiedFlag write SetModifiedFlag;
  end;

implementation

{$R *.dfm}

procedure SwapNumbers(var a,b: Real);
var
  c: Real;
begin
  c := a;
  a := b;
  b := c;
end;

function HydrometrySessionCompare(Item1, Item2: Pointer): Integer;
var
  ADate1, ADate2: TDateTime;
begin
  ADate1 := THydrometrySession(Item1).Date;
  ADate2 := THydrometrySession(Item2).Date;
  Result := DiffInSecs(ADate1, ADate2);
end;

procedure TFrmHydrometry.SetModifiedFlag(AValue: Boolean);
begin
  FModifiedFlag := AValue;
  if FModifiedFlag then
    Caption := InitialCaption + '  - '+ExtractFileName(SaveDialog.FileName)+ ' (*)'
  else
    Caption := InitialCaption + '  - '+ ExtractFileName(SaveDialog.FileName);
end;

procedure TFrmHydrometry.spnMeasurementsNumClick(Sender: TObject;
  Button: TUDBtnType);
begin
  if lstSessions.ItemIndex<0 then Exit;
  if lstSlices.ItemIndex<0 then Exit;
  SetLength(THydrometrySession(
    HydrometrySessionList[lstSessions.ItemIndex]).Slices[
    lstSlices.ItemIndex].Properties.Measures, (Sender as TUpDown).Position);
  RefreshGrid;
end;

procedure TFrmHydrometry.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModifiedFlag then
    CanClose := ModifiedSaveChangesConfirmation;
end;

resourcestring
  rsSection = 'Section ';

procedure TFrmHydrometry.FormCreate(Sender: TObject);
var
  i: Integer;
  ASection: TXSection;
begin
  InitialCaption := Caption;
  ModifiedFlag := False;
  HydrometrySessionList := TObjectList.Create(True);
  SetLength(Sections, 10);
  for i := 0 to Length(Sections)-1 do
  begin
    ASection := nil;
    try
      ASection := TXSection.Create;
      Sections[i] := ASection;
      ASection := nil;
    except
      ASection.Free;
      raise;
    end;
  end;
  tbsSections.Tabs.Clear;
  for i := 0 to SectionsCount-1 do
    tbsSections.Tabs.Add(rsSection+IntToStr(i+1));
  tbsSections.TabIndex := 0;
  FixComponentDecSeparators(Self);
end;

procedure TFrmHydrometry.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Length(Sections)-1 do
    Sections[i].Free;
  HydrometrySessionList.Destroy;
end;

procedure TFrmHydrometry.FormShow(Sender: TObject);
begin
  grdMeasurements.DefaultRowHeight :=
    grdMeasurements.DefaultRowHeight * Screen.PixelsPerInch div 96;
end;

procedure TFrmHydrometry.lstSessionsClick(Sender: TObject);
var
  ASession: THydrometrySession;
  AIndex: Integer;
begin
  edtStage.Text := '';
  edtDate.Text := '';
  chkFromCut.Checked := True;
  AIndex := lstSessions.ItemIndex;
  if AIndex<0 then Exit;
  Assert(lstSessions.Count = HydrometrySessionList.Count);
  ASession := THydrometrySession(HydrometrySessionList[AIndex]);
  edtStage.Text := FloatToStr(ASession.Stage);
  chkFromCut.Checked := ASession.IsMeasuredFromCut;
  edtDate.Text := FormatDateTime('yyyy/mm/dd hh:nn', ASession.Date);
  tbsSections.TabIndex := ASession.SectionIndex;
  lstSessions.ItemIndex := AIndex;
  RefreshListboxes;
  DrawCutSection;
  if lstSlices.Count>0 then
    if lstSlices.ItemIndex<0 then
      lstSlices.ItemIndex := 0;
  lstSlicesClick(nil);
end;

procedure TFrmHydrometry.lstSlicesClick(Sender: TObject);
var
  AIndex: Integer;
begin
  edtSlicePosition.Text := '';
  cmbSliceMode.ItemIndex := 0;
  if lstSessions.ItemIndex<0 then Exit;
  AIndex := lstSlices.ItemIndex;
  if AIndex<0 then Exit;
  with THydrometrySession(HydrometrySessionList[lstSessions.ItemIndex]) do
  begin
    Assert(lstSlices.Count = Length(Slices));
    edtSlicePosition.Text := FloatToStr(Slices[AIndex].Properties.XPosition);
    cmbSliceMode.ItemIndex := Ord(Slices[AIndex].Properties.Mode);
    lstSlices.ItemIndex := AIndex;
  end;
  RefreshListboxes;
end;

procedure TFrmHydrometry.mnuCopyChartClick(Sender: TObject);
begin
  chartSection.CopyToClipboardMetafile(True);
end;

procedure TFrmHydrometry.mnuCopymeasurementstableClick(Sender: TObject);
begin
  with grdMeasurements do
    StringGridToClipboard(grdMeasurements, 0, ColCount-1, 0, RowCount-1);
end;

procedure TFrmHydrometry.mnuNewClick(Sender: TObject);
begin
  if ModifiedFlag then
    if not ModifiedSaveChangesConfirmation then Exit;
  SaveDialog.FileName := '';
  ResetAll;
  ModifiedFlag := False;
end;

procedure TFrmHydrometry.mnuOpenClick(Sender: TObject);
var
  ACursor: TCursor;
begin
  if ModifiedFlag then
    if not ModifiedSaveChangesConfirmation then Exit;
  ACursor := Screen.Cursor;
  try
    OpenDialog.FileName := '';
    if OpenDialog.Execute then
    begin
      Screen.Cursor := crHourGlass;
      OpenAll(OpenDialog.FileName);
      SaveDialog.FileName := OpenDialog.FileName;
      ModifiedFlag := False;
      Caption := InitialCaption + '  - '+ ExtractFileName(SaveDialog.FileName);
    end;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmHydrometry.mnuPrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TFrmHydrometry.mnuPrintSectionClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    chartSection.Print;
end;

resourcestring
  rsChangeHaveBeenMade =
    'Changes have been made. Save the changes before proceed? ';

function TFrmHydrometry.ModifiedSaveChangesConfirmation: Boolean;
begin
  Result := False;
  case MessageDlg(rsChangeHaveBeenMade, mtConfirmation, mbYesNoCancel, 0) of
    mrYes:
    begin
      mnuSaveClick(nil);
      if not ModifiedFlag then Result := True;
    end;
    mrNo: Result := True;
    mrCancel: Result := False;
    else
      Assert(False);
  end;
end;

procedure TFrmHydrometry.mnuSaveAsClick(Sender: TObject);
var
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  try
    if SaveDialog.Execute then
    begin
      Screen.Cursor := crHourGlass;
      SaveAll(SaveDialog.FileName);
      ModifiedFlag := False;
    end;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmHydrometry.mnuSaveClick(Sender: TObject);
var
  ACursor: TCursor;
begin
  if SaveDialog.FileName='' then mnuSaveAsClick(Sender) else
  begin
    ACursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      SaveAll(SaveDialog.FileName);
      ModifiedFlag := False;
    finally
      Screen.Cursor := ACursor;
    end;
  end;
end;

procedure TFrmHydrometry.SetControlStatus;
begin
  SetAxisLimits;
  DrawSectionGraph;
  RefreshListBoxes;
end;

resourcestring
  rsNr = 'Nr. ';
  rsat = ' at: ';

procedure TFrmHydrometry.RefreshListboxes;
var
  i, AIndex: Integer;
begin
  AIndex := lstSessions.ItemIndex;
  lstSessions.Clear;
  for i := 0 to HydrometrySessionList.Count-1 do
    lstSessions.Items.Add(FormatDateTime('yyyy/mm/dd hh:nn',
      THydrometrySession(HydrometrySessionList[i]).Date));
  lstSessions.ItemIndex := Min(AIndex, lstSessions.Count-1);
  AIndex := lstSlices.ItemIndex;
  lstSlices.Clear;
  if lstSessions.ItemIndex>-1 then
    with THydrometrySession(HydrometrySessionList[lstSessions.ItemIndex]) do
      for i := 0 to Length(Slices)
        -1 do
        lstSlices.Items.Add(rsNr+IntToStr(i+1)+rsat+
          FloatToStr(Slices[i].Properties.XPosition)+' m - '+
          cmbSliceMode.Items[Ord(Slices[i].Properties.Mode)]);
  lstSlices.ItemIndex := AIndex;
  RefreshGrid;
end;

resourcestring
  rsyHratio = 'y/H ratio';
  rsDepthy = 'Depth y (m)';

procedure TFrmHydrometry.RefreshGrid;
var
  i: Integer;
  ASlice: THydrometricSliceValues;
  ARatio: Real;
begin
  for i := 0 to grdMeasurements.RowCount-1 do
    grdMeasurements.Rows[i].Text := #13#10#13#10;
  grdMeasurements.RowCount := 1;
  seriesVelocitiesMeasurements.Clear;
  seriesVelocityCurve.Clear;
  if lstSlices.ItemIndex<0 then
    Exit;
  if lstSessions.ItemIndex<0 then
    Exit;
  ASlice := THydrometrySession(
    HydrometrySessionList[lstSessions.ItemIndex]).Slices[lstSlices.ItemIndex];
  with ASlice do
  begin
    with grdMeasurements do
    begin
      if Properties.Mode in [thymOnePointStandard, thymTwoPointsStandard,
        thymThreePointsStandard] then
      begin
        FixedCols := 1;
        RowCount := Length(Properties.Measures)+2;
        FixedRows := 1;
        Cells[0,0] := rsyHratio;
        Cells[1,0] := 'V (m/s)';
        Cells[0, RowCount-1] := 'Vm';
        Cells[1, RowCount-1] := FloatToStr(MeanVelocity);
        grpMeasurementsNumber.Enabled := False;
      end else
      begin
        FixedCols := 0;
        RowCount := Length(Properties.Measures)+5;
        FixedRows := 1;
        grpMeasurementsNumber.Enabled := True;
        if Properties.IsMeasuredActualDepth then
          Cells[0,0] := rsDepthy else
          Cells[0,0] := rsyHratio;
        Cells[1,0] := 'V (m/s)';
        Cells[0, RowCount-1] := 'R';
        Cells[0, RowCount-2] := 'c2';
        Cells[0, RowCount-3] := 'c1';
        Cells[0, RowCount-4] := 'Vm';
        Cells[1, RowCount-4] := FloatToStr(MeanVelocity);
        Cells[1, RowCount-3] := FloatToStr(coef1);
        Cells[1, RowCount-2] := FloatToStr(coef2);
        Cells[1, RowCount-1] := FloatToStr(detfac);
        spnMeasurementsNum.Position := Length(Properties.Measures);
        chkActualDepth.OnClick := nil;
        try
          chkActualDepth.Checked := Properties.IsMeasuredActualDepth;
        finally
          chkActualDepth.OnClick := chkActualDepthClick;
        end;
      end;
      for i := 0 to Length(Properties.Measures)-1 do
      begin
        Cells[1,i+1] := FloatToStr(Properties.Measures[i].Velocity);
        if (Properties.Mode in [thymRandomPointsLogarithmic,
          thymRandomPointsParabola]) and Properties.IsMeasuredActualDepth then
          Cells[0,i+1] := FloatToStr(Properties.Measures[i].Depth) else
          Cells[0,i+1] := FloatToStr(Properties.Measures[i].DepthRatio);
      end;
    end;
    for i  := 1 to 49 do
    begin
      ARatio := i/50;
      case Properties.Mode of
        thymOnePointStandard, thymTwoPointsStandard, thymThreePointsStandard:
          seriesVelocityCurve.AddXY((coef1 + coef2*Log10(ARatio))*MeanVelocity,
            1-ARatio, '', clDefault);
        thymRandomPointsLogarithmic:
          seriesVelocityCurve.AddXY((coef1 + coef2*Log10(ARatio)), 1-ARatio, '',
            clDefault);
        thymRandomPointsParabola:
          seriesVelocityCurve.AddXY(coef1*ARatio + coef2*Sqr(ARatio),
            1-ARatio, '', clDefault);
      else
        Assert(False);
      end;
    end;
    for i := 0 to Length(Properties.Measures)-1 do
      seriesVelocitiesMeasurements.AddXY(Properties.Measures[i].Velocity,
        Properties.Measures[i].DepthRatio, '', clDefault);
  end;
end;

procedure TFrmHydrometry.rgrpOutputTimeseriesClick(Sender: TObject);
begin
  ModifiedFlag := True;
end;

procedure TFrmHydrometry.tbsSectionsClick(Sender: TObject);
begin
  SetControlStatus;
end;

resourcestring
  rsRecordAlreadyExist = 'Record already exists';
  rsNewHydrometricsession = 'New hydrometric session';
  rsInputDateAndTimeFormatedAs = 'Input date and time formated as '+
    'yyyy/mm/dd hh:mm, or yyyy/mm/dd, etc.';


procedure TFrmHydrometry.btnAddSessionClick(Sender: TObject);
var
  s: string;
  ANewSession: THydrometrySession;
  ADate: TDateTime;
  i: Integer;
begin
  s := InputBox(rsNewHydrometricsession, rsInputDateAndTimeFormatedAs, '');
  if s='' then Exit;
  ANewSession := nil;
  try
    ADate := FormatStrToDateTime(GetDateFormat(s,[gdfRaiseOnFail]),
      s);
    for i := 0 to HydrometrySessionList.Count-1 do
      if Abs(DiffInSecs(ADate,
        THydrometrySession(HydrometrySessionList[i]).Date))<2 then
        raise Exception.Create(rsRecordAlreadyExist);
    ANewSession := THydrometrySession.Create;
    with ANewSession do
    begin
      Date := ADate;
      SectionIndex := tbsSections.TabIndex;
      Stage := Sections[SectionIndex].MinY;
      IsMeasuredFromCut := True;
    end;
    HydrometrySessionList.Add(ANewSession);
    i := HydrometrySessionList.IndexOf(ANewSession);
    ANewSession := nil;
    HydrometrySessionList.Sort(HydrometrySessionCompare);
    lstSessions.ItemIndex := -1;
    SetControlStatus;
    ModifiedFlag := True;
    Assert(lstSessions.Items.Count = HydrometrySessionList.Count);
    lstSessions.ItemIndex := i;
    lstSessions.OnClick(Sender);
  except
    ANewSession.Free;
    raise;
  end;
end;

resourcestring
  rsRemoveSessionConfirm = 'Are you sure to remove current session?';

procedure TFrmHydrometry.btnRemoveClick(Sender: TObject);
var
  i: Integer;
begin
  if lstSessions.ItemIndex<0 then Exit;
  Assert(lstSessions.Count = HydrometrySessionList.Count);
  i := lstSessions.ItemIndex;
  if MessageDlg(rsRemoveSessionConfirm, mtConfirmation, mbYesNo, 0) = mrNo then
    Exit;
  HydrometrySessionList.Delete(i);
  i := Min(i, HydrometrySessionList.Count-1);
  SetControlStatus;
  ModifiedFlag := True;
  lstSessions.ItemIndex := i;
  lstSessions.OnClick(Sender);
end;

resourcestring
  rsSureToChangeDate = 'Are you sure to change session date?';

procedure TFrmHydrometry.btnSaveMeasurementsClick(Sender: TObject);
var
  i: Integer;
  ASlice: THydrometricSliceValues;
begin
  if lstSessions.ItemIndex<0 then Exit;
  if lstSlices.ItemIndex<0 then Exit;
  ASlice := THydrometrySession(HydrometrySessionList[
    lstSessions.ItemIndex]).Slices[lstSlices.ItemIndex];
  with ASlice.Properties do
  begin
    for i := 0 to Length(Measures)-1 do
    begin
      Measures[i].Velocity := StrToFloat(grdMeasurements.Cells[1, i+1]);
      if (Mode in [thymRandomPointsLogarithmic, thymRandomPointsParabola]) and
        IsMeasuredActualDepth then
        Measures[i].Depth := StrToFloat(grdMeasurements.Cells[0, i+1]) else
        Measures[i].DepthRatio := StrToFloat(grdMeasurements.Cells[0, i+1]);
    end;
  end;
  RefreshGrid;
  ModifiedFlag := True;
end;

procedure TFrmHydrometry.btnSaveStageDateValidateClick(Sender: TObject);
var
  ADate: TDateTime;
  ASession: THydrometrySession;
  i: Integer;
  DateToChange, Changed: Boolean;
begin
  if lstSessions.ItemIndex<0 then Exit;
  Assert(lstSessions.Count = HydrometrySessionList.Count);
  ADate := FormatStrToDateTime(GetDateFormat(edtDate.Text,[gdfRaiseOnFail]),
      edtDate.Text);
  ASession := THydrometrySession(HydrometrySessionList[lstSessions.ItemIndex]);
  DateToChange := Abs(DiffInSecs(ADate, ASession.Date))>1;
  Changed := DateToChange;
  if DateToChange then
    if MessageDlg(rsSureToChangeDate, mtConfirmation, mbYesNo, 0)=mrNo then
      Exit;
  if DateToChange then
    for i := 0 to HydrometrySessionList.Count-1 do
      if Abs(DiffInSecs(ADate,
        THydrometrySession(HydrometrySessionList[i]).Date))<2 then
      begin
        edtDate.Text := FormatDateTime('yyyy/mm/dd hh:nn', ADate);
        raise Exception.Create(rsRecordAlreadyExist);
      end;
  if Abs(ASession.Stage - StrToFloat(edtStage.Text))>0.0002 then
  begin
    Changed := True;
    ASession.Stage := StrToFloat(edtStage.Text);
  end;
  if (ASession.IsMeasuredFromCut <> chkFromCut.Checked) then
  begin
    Changed := True;
    ASession.IsMeasuredFromCut := chkFromCut.Checked;
  end;
  tbsSections.TabIndex := ASession.SectionIndex;
  if DateToChange then
  begin
    ASession.Date := ADate;
    HydrometrySessionList.Sort(HydrometrySessionCompare);
    i := HydrometrySessionList.IndexOf(ASession);
    lstSessions.ItemIndex := -1;
    SetControlStatus;
    Assert(lstSessions.Items.Count = HydrometrySessionList.Count);
    lstSessions.ItemIndex := i;
    lstSessions.OnClick(Sender);
  end;
  ModifiedFlag := Changed;
  DrawCutSection;
end;

resourcestring
  rsMeanVelocity = 'Mean velocity = ';

procedure TFrmHydrometry.btnValidateSlicesClick(Sender: TObject);
var
  Velocity: Real;
begin
  if lstSessions.ItemIndex<0 then Exit;
  if lstSlices.ItemIndex<0 then Exit;
  with THydrometrySession(HydrometrySessionList[lstSessions.ItemIndex]) do
  begin
    Velocity := CalcHydrometricMeanVelocityFromSection(Stage,
      Sections[SectionIndex], IsMeasuredFromCut, Slices);
    DrawSlices;
    RefreshGrid;
    ShowMessage(rsMeanVelocity+FloatToStr(Velocity)+' m/s');
  end;
end;

procedure TFrmHydrometry.chkActualDepthClick(Sender: TObject);
begin
  if lstSessions.ItemIndex<0 then Exit;
  if lstSlices.ItemIndex<0 then Exit;
  THydrometrySession(
    HydrometrySessionList[lstSessions.ItemIndex]).Slices[
    lstSlices.ItemIndex].Properties.IsMeasuredActualDepth :=
      (Sender as TCheckBox).Checked;
  RefreshGrid;
end;

procedure TFrmHydrometry.mnuCopyvelocityprofileClick(Sender: TObject);
begin
  chartMeasurements.CopyToClipboardMetafile(True);
end;

procedure TFrmHydrometry.btnAddSliceClick(Sender: TObject);
var
  i,j: Integer;
  XPosition: Real;
begin
  if lstSessions.ItemIndex<0 then Exit;
  XPosition := StrToFloat(edtSlicePosition.Text);
  with THydrometrySession(HydrometrySessionList[lstSessions.ItemIndex]) do
  begin
    SetLength(Slices, Length(Slices)+1);
    j := Length(Slices)-1;
    Slices[j].Properties.XPosition := XPosition;
    with Slices[j] do
    begin
      MeanVelocity := 0;
      coef1 := 1.1321928;
      coef2 := 0.3321928;
      DetFac := 1.00;
    end;
    with Slices[j].Properties do
    begin
      Mode := THydrometryMode(cmbSliceMode.ItemIndex);
      case Slices[j].Properties.Mode of
        thymOnePointStandard: SetLength(Measures, 1);
        thymTwoPointsStandard: SetLength(Measures, 2);
        thymThreePointsStandard,
          thymRandomPointsLogarithmic, thymRandomPointsParabola:
          SetLength(Measures, 3);
      else
        Assert(False);
      end;
      if Length(Measures)=1 then
      begin
        Measures[0].DepthRatio := 0.6;
        Measures[0].Velocity := 0;
      end else if Length(Measures)=2 then
      begin
        Measures[0].DepthRatio := 0.2;
        Measures[0].Velocity := 0;
        Measures[1].DepthRatio := 0.8;
        Measures[1].Velocity := 0;
      end else if Length(Measures)=3 then
      begin
        Measures[0].DepthRatio := 0.2;
        Measures[0].Velocity := 0;
        Measures[1].DepthRatio := 0.6;
        Measures[1].Velocity := 0;
        Measures[2].DepthRatio := 0.8;
        Measures[2].Velocity := 0;
      end;
    end;
    for i := Length(Slices)-2 downto 0 do
      if Slices[i+1].Properties.XPosition<=Slices[i].Properties.XPosition then
      begin
        j := i;
        SwapNumbers(Slices[i+1].Properties.XPosition,
          Slices[i].Properties.XPosition);
      end;
  end;
  ModifiedFlag := True;
  SetControlStatus;
  lstSlices.ItemIndex := j;
  lstSlices.OnClick(Sender);
end;

resourcestring
  rsCalculatedStage = 'Calculated Stage H (m)';
  rsCalculatedDischarge = 'Calculated Discharge Q (m3/s)';
  rsCalculatedMeanVelocity = 'Calculated Mean Velocity Vm (m3/s)';
  rsErrorInHydrometrySession = 'Error in Hydrometry session Nr. ';
  rsWithDate = ' with date: ';
  rsAbortingCalculations = '". Aborting calculations';
  rsValidationCompletedWithoutErrors = 'Validation completed without errors';

procedure TFrmHydrometry.btnCalculateTimeseriesClick(Sender: TObject);
var
  i: Integer;
  Area, Perimeter, Velocity: Real;
begin
  if HydrometrySessionList.Count<1 then Exit;
  Assert((DischargeTimeseries<>nil) and (StageTimeseries<>nil));
  StageTimeseries.Title := rsCalculatedStage;
  StageTimeseries.Comment := rsCalculatedStage;
  if rgrpOutputTimeseries.ItemIndex=0 then
    DischargeTimeseries.Title := rsCalculatedDischarge else
    DischargeTimeseries.Title := rsCalculatedMeanVelocity;
  DischargeTimeseries.Comment := DischargeTimeseries.Title;
  StageTimeseries.Clear;
  DischargeTimeseries.Clear;
  for i := 0 to HydrometrySessionList.Count-1 do
    with THydrometrySession(HydrometrySessionList[i]) do
    begin
      try
        StageTimeseries.Add(Date, False, Stage, '', msNew);
        if Length(Slices)>0 then
        begin
          Velocity := CalcHydrometricMeanVelocityFromSection(Stage,
            Sections[SectionIndex], IsMeasuredFromCut, Slices);
          Sections[SectionIndex].CalcAreaAndPerimeter(Stage, Area, Perimeter);
          if rgrpOutputTimeseries.ItemIndex = 0 then
            Velocity := Velocity * Area;
          DischargeTimeseries.Add(Date, False, Velocity, '', msNew);
        end else
          DischargeTimeseries.Add(Date, True, 0, '', msNew);
      except
        on E: Exception do
          raise Exception.Create(rsErrorInHydrometrySession+
            IntToStr(i)+rsWithDate+FormatDateTime('yyyy/mm/dd hh:nn',Date)+
            ' : "'+E.Message+rsAbortingCalculations);
      end;
    end;
  if Sender <> mnuCalculateOnly then
    ModalResult := mrOk
  else
    ShowMessage(rsValidationCompletedWithoutErrors);
end;

resourcestring
  rsConfirmSectionChange = 'Change X-Section? All slice data of this '+
    'hydrometry session will be erased. Stage will be reseted';

procedure TFrmHydrometry.btnChangeSessionSectionClick(Sender: TObject);
var
  ASession: THydrometrySession;
begin
  if lstSessions.ItemIndex<0 then Exit;
  Assert(lstSessions.Count = HydrometrySessionList.Count);
  ASession := THydrometrySession(
    HydrometrySessionList[lstSessions.ItemIndex]);
  if MessageDlg(rsConfirmSectionChange,mtConfirmation,mbYesNo,0) = mrNo then
    Exit;
  ASession.SectionIndex := tbsSections.TabIndex;
  ASession.Stage := Sections[ASession.SectionIndex].MinY;
  SetLength(ASession.Slices,0);
  ModifiedFlag := True;
end;

resourcestring
  rsEditSection = 'Edit and draw section';
  rsWarningWillEraseSlices =
    'Warning, editing x-sections will erase all slice data to associated '+
      'hydrometry sessions. Stage also will be reseted. Continue Operation?';

procedure TFrmHydrometry.btnEditSectionClick(Sender: TObject);
var
  AFrmHydrXSections: TFrmHydrXSections;
  i: Integer;
  AFlag, AModified: Boolean;
begin
  AFrmHydrXSections := nil;
  try
    AFrmHydrXSections := TFrmHydrXSections.Create(Self);
    with AFrmHydrXSections do
    begin
      HideStageCurveControls;
      Caption := rsEditSection;
      ReadSection(Sections[tbsSections.TabIndex]);
      ShowModal;
      AModified := Modified;
      if Modified then
      begin
        AFlag := False;
        for i  := 0 to HydrometrySessionList.Count - 1 do
          AFlag := AFlag or
            (THydrometrySession(HydrometrySessionList[i]).SectionIndex =
            tbsSections.TabIndex);
        if AFlag then
        begin
          if MessageDlg(rsWarningWillEraseSlices, mtWarning, mbYesNo, 0,
            mbNo) = mrNo then Exit;
          for i  := 0 to HydrometrySessionList.Count - 1 do
            if THydrometrySession(HydrometrySessionList[i]).SectionIndex =
              tbsSections.TabIndex then
            begin
              SetLength(THydrometrySession(HydrometrySessionList[i]).Slices, 0);
              THydrometrySession(HydrometrySessionList[i]).Stage :=
                Sections[tbsSections.TabIndex].MinY;
            end;
        end;
      end;
      WriteSection(Sections[tbsSections.TabIndex]);
      ModifiedFlag := AModified;
      SetControlStatus;
    end;
  finally
    AFrmHydrXSections.Release;
  end;
end;

procedure TFrmHydrometry.SetAxisLimits;
var
  ADimension: Real;
begin
  with Sections[tbsSections.TabIndex] do
  begin
    if Count<1 then
      Exit;
    ADimension := Max(Abs(MaxX-MinX), Abs(MaxY-MinY));
    with chartSection do
    begin
      BottomAxis.Maximum := 1e37;
      BottomAxis.Minimum := 0.5*(MaxX+MinX)-0.55*ADimension;
      BottomAxis.Maximum := 0.5*(MaxX+MinX)+0.55*ADimension;
      LeftAxis.Maximum := 1e37;
      LeftAxis.Minimum := 0.5*(MaxY+MinY)-0.55*ADimension;
      LeftAxis.Maximum := 0.5*(MaxY+MinY)+0.55*ADimension;
    end;
  end;
end;

procedure TFrmHydrometry.DrawSectionGraph;
var
  i: Integer;
begin
  seriesGround.Clear;
  seriesCutSection.Clear;
  seriesMeasurePoints.Clear;
  seriesSlices.Clear;
  seriesSectionMeasurements.Clear;
  with Sections[tbsSections.TabIndex] do
    for i := 0 to Count-1 do
      seriesGround.AddXY(Nodes[i].x, Nodes[i].y, '', clDefault);
end;

procedure TFrmHydrometry.DrawCutSection;
var
  CutNodes: TArrayOfTXSectionNode;
  ASession: THydrometrySession;
  Dummy1, Dummy2: Real;
  i: Integer;
begin
  seriesCutSection.Clear;
  seriesMeasurePoints.Clear;
  seriesSlices.Clear;
  seriesSectionMeasurements.Clear;
  if lstSessions.ItemIndex<0 then Exit;
  Assert(lstSessions.Count = HydrometrySessionList.Count);
  ASession := THydrometrySession(HydrometrySessionList[lstSessions.ItemIndex]);
  CutNodes := Sections[ASession.SectionIndex].CalcAreaAndPerimeter(
    ASession.Stage, Dummy1, Dummy2);
  i := 0;
  while i< Length(CutNodes)-1 do
  begin
    seriesCutSection.AddXY(CutNodes[i].x, CutNodes[i].y, '', clDefault);
    seriesCutSection.AddXY(CutNodes[i+1].x, CutNodes[i+1].y, '', clDefault);
    seriesCutSection.AddXY(CutNodes[i+1].x, CutNodes[i+1].y, '', clNone);
    Inc(i, 2);
  end;
end;

procedure TFrmHydrometry.DrawSlices;
var
  i, j: Integer;
  CutNodes: TArrayOfTXSectionNode;
  AOffset, Dummy1, Dummy2: Real;
begin
  DrawCutSection;
  if lstSessions.ItemIndex<0 then Exit;
  with THydrometrySession(HydrometrySessionList[lstSessions.ItemIndex]) do
  begin
    CutNodes := Sections[SectionIndex].CalcAreaAndPerimeter(Stage, Dummy1,
      Dummy2);
    AOffset := 0;
    if IsMeasuredFromCut then AOffset := CutNodes[0].x;
    for i := 0 to Length(Slices)-1 do
    begin
      seriesMeasurePoints.AddXY(AOffset+Slices[i].Properties.XPosition,
        CutNodes[0].y, '', clDefault);
      with Slices[i] do
      begin
        for j := 0 to Length(Sketch)-1 do
          seriesSlices.AddXY(Sketch[j].x, Sketch[j].y, '', clDefault);
        if Length(Sketch)>0 then
          seriesSlices.AddXY(Sketch[Length(Sketch)-1].x,
            Sketch[Length(Sketch)-1].y, '', clNone);
      end;
      with Slices[i].Properties do
        for j := 0 to Length(Measures)-1 do
          seriesSectionMeasurements.AddXY(AOffset+Slices[i].Properties.XPosition,
            Stage-Measures[j].Depth, '', clDefault);
    end;
  end;
end;

procedure TFrmHydrometry.Exitwithoutcalculations1Click(Sender: TObject);
begin
  Close;
end;

resourcestring
  rsRemoveSlice = 'Remove slice?';

procedure TFrmHydrometry.btnRemoveSliceClick(Sender: TObject);
var
  i, j: Integer;
begin
  if lstSessions.ItemIndex<0 then Exit;
  if lstSlices.ItemIndex<0 then Exit;
  with THydrometrySession(HydrometrySessionList[lstSessions.ItemIndex]) do
  begin
    Assert(lstSlices.Count = Length(Slices));
    i := lstSlices.ItemIndex;
    if MessageDlg(rsRemoveSlice, mtConfirmation, mbYesNo, 0) = mrNo then
      Exit;
    for j := i to Length(Slices)-2 do
      Slices[j] := Slices[j+1];
    SetLength(Slices, Length(Slices)-1);
    i := Min(i, Length(Slices)-1);
    SetControlStatus;
    ModifiedFlag := True;
    lstSlices.ItemIndex := i;
    lstSlices.OnClick(Sender);
  end;
end;

procedure TFrmHydrometry.btnChangeSliceClick(Sender: TObject);
var
  AIndex, i, j: Integer;
  ANewPosition : Real;
  ANewMode: THydrometryMode;
begin
  ANewPosition := StrToFloat(edtSlicePosition.Text);
  if lstSessions.ItemIndex<0 then Exit;
  AIndex := lstSlices.ItemIndex;
  if AIndex<0 then Exit;
  j := AIndex;
  with THydrometrySession(HydrometrySessionList[lstSessions.ItemIndex]) do
  begin
    if Slices[AIndex].Properties.XPosition<>ANewPosition then
    begin
      Slices[AIndex].Properties.XPosition := ANewPosition;
      for i := AIndex-1 downto 0 do
        if Slices[i+1].Properties.XPosition<=Slices[i].Properties.XPosition then
        begin
          j := i;
          SwapNumbers(Slices[i+1].Properties.XPosition,
            Slices[i].Properties.XPosition);
        end;
      for i := j to Length(Slices)-2 do
        if Slices[i].Properties.XPosition>=Slices[i+1].Properties.XPosition then
        begin
          j := i+1;
          SwapNumbers(Slices[i+1].Properties.XPosition,
            Slices[i].Properties.XPosition);
        end;
    end;
    ANewMode := THydrometryMode(cmbSliceMode.ItemIndex);
    if ANewMode<> Slices[AIndex].Properties.Mode then
    begin
      Slices[AIndex].Properties.Mode := ANewMode;
      with Slices[AIndex] do
      begin
        MeanVelocity := 0;
        coef1 := 1.1321928;
        coef2 := 0.3321928;
        DetFac := 1.00;
      end;
      with Slices[AIndex].Properties do
      begin
        case ANewMode of
          thymOnePointStandard: SetLength(Measures, 1);
          thymTwoPointsStandard: SetLength(Measures, 2);
          thymThreePointsStandard,
            thymRandomPointsLogarithmic, thymRandomPointsParabola:
            SetLength(Measures, 3);
        else
          Assert(False);
        end;
        if Length(Measures)=1 then
        begin
          Measures[0].DepthRatio := 0.6;
          Measures[0].Velocity := 0;
        end else if Length(Measures)=2 then
        begin
          Measures[0].DepthRatio := 0.2;
          Measures[0].Velocity := 0;
          Measures[1].DepthRatio := 0.8;
          Measures[1].Velocity := 0;
        end else if Length(Measures)=3 then
        begin
          Measures[0].DepthRatio := 0.2;
          Measures[0].Velocity := 0;
          Measures[1].DepthRatio := 0.6;
          Measures[1].Velocity := 0;
          Measures[2].DepthRatio := 0.8;
          Measures[2].Velocity := 0;
        end;
      end;
    end;
  end;
  SetControlStatus;
  ModifiedFlag := True;
  lstSlices.ItemIndex := j;
  lstSlices.OnClick(Sender);
end;

procedure TFrmHydrometry.OpenAll(AFileName: string);
var
  AMemIniFile: TMemIniFile;
  ADecimalSeparator: Char;
  ADateFormat: string;
  TotalSections, TotalSessions, TotalSlices, TotalMeasurements: Integer;
  AValueList: TStringList;

  procedure ReadGeneralSection;
  begin
    with AMemIniFile do
    begin
      ADateFormat := ReadString('General', 'DateFormat', 'dd-mm-yyyy');
      TotalSections := ReadInteger('General', 'SectionsCount', 0);
      TotalSessions := ReadInteger('General', 'SessionsCount', 0);
      TotalSlices := ReadInteger('General', 'TotalSlices', 0);
      TotalMeasurements := ReadInteger('General', 'TotalMeasurements', 0);
      rgrpOutputTimeseries.ItemIndex :=
        ReadInteger('General', 'OutputTimeseries', 0);
    end;
  end;

  procedure ReadXSections;
  var
    i, j: Integer;
    x, y : Real;
    s: string;
  begin
    for i := 0 to TotalSections do
    begin
      AValueList.Clear;
      AMemIniFile.ReadSectionValues('XSection'+IntToStr(i), AValueList);
      for j := 0 to AValueList.Count-1 do
      begin
        s := AValueList.Values['Point'+IntToStr(j)];
        x := StrToFloat(DelimitedStringItem(s, 1, '$'));
        y := StrToFloat(DelimitedStringItem(s, 2, '$'));
        Sections[i].Add(x, y);
      end;
    end;
  end;

  procedure ReadSessions;
  var
    i: Integer;
    s: string;
    AHydrometrySession: THydrometrySession;
  begin
    AHydrometrySession := nil;
    with AMemIniFile do
      for i := 0 to TotalSessions-1 do
      begin
        try
          AHydrometrySession := THydrometrySession.Create;
          with AHydrometrySession do
          begin
            s := 'Session'+IntToStr(i);
            ID := ReadInteger(s, 'ID', 0);
            Date := FormatStrToDateTime(ADateFormat,
              ReadString(s, 'Date', ''));
            Stage := ReadFloat(s, 'Stage', 0);
            SectionIndex := ReadInteger(s, 'XSection', 0);
            IsMeasuredFromCut := ReadBool(s, 'IsMeasuredFromCut', True);
            SetLength(Slices, 0);
          end;
          HydrometrySessionList.Add(AHydrometrySession);
          AHydrometrySession := nil;
        finally
          AHydrometrySession.Free;
        end;
      end;
    HydrometrySessionList.Sort(HydrometrySessionCompare);
  end;

  procedure ReadSlices;
  var
    i, j, ASessionID, ASessionIndex: Integer;
  begin
    for i := 0 to TotalSlices-1 do
    begin
      AValueList.Clear;
      AMemIniFile.ReadSectionValues('Slice'+IntToStr(i), AValueList);
      ASessionID := StrToInt(AValueList.Values['SessionID']);
      ASessionIndex := -1;
      for j := 0 to HydrometrySessionList.Count-1 do
      begin
        ASessionIndex := j;
        if THydrometrySession(HydrometrySessionList[j]).ID = ASessionID then
          Break;
      end;
      with THydrometrySession(HydrometrySessionList[ASessionIndex]) do
      begin
        SetLength(Slices, Length(Slices)+1);
        with Slices[Length(Slices)-1] do
        begin
          ID := StrToInt(AValueList.Values['ID']);
          Properties.XPosition := StrToFloat(AValueList.Values['XPosition']);
          Properties.Mode :=
            THydrometryMode(StrToInt(AValueList.Values['Mode']));
          Properties.IsMeasuredActualDepth :=
            StrToBool(AValueList.Values['IsMeasuredActualDepth']);
          coef1 := StrToFloat(AValueList.Values['coef1']);
          coef2 := StrToFloat(AValueList.Values['coef2']);
          detfac := StrToFloat(AValueList.Values['detfac']);
          MeanVelocity := StrToFloat(AValueList.Values['MeanVelocity']);
          SetLength(Properties.Measures, 0);
        end;
      end;
    end;
  end;

  procedure ReadMeasurments;
  var
    i, j, k, ASessionIndex, ASliceID, ASliceIndex: Integer;
  label
    SkipLoop;
  begin
    for i := 0 to TotalMeasurements-1 do
    begin
      AValueList.Clear;
      AMemIniFile.ReadSectionValues('Measure'+IntToStr(i), AValueList);
      ASliceID := StrToInt(AValueList.Values['SliceID']);
      ASessionIndex := -1;
      ASliceIndex := -1;
      for j := 0 to HydrometrySessionList.Count-1 do
      begin
        ASessionIndex := j;
        with THydrometrySession(HydrometrySessionList[j]) do
        begin
          for k := 0 to Length(Slices)-1 do
          begin
            ASliceIndex := k;
            if Slices[k].ID = ASliceID then
            goto SkipLoop;
          end;
        end;
      end;
SkipLoop:
      with THydrometrySession(HydrometrySessionList[ASessionIndex]).Slices[
        ASliceIndex].Properties do
      begin
        SetLength(Measures, Length(Measures)+1);
        with Measures[Length(Measures)-1] do
        begin
          ID := StrToInt(AValueList.Values['ID']);
          Depth := StrToFloat(AValueList.Values['Depth']);
          DepthRatio := StrToFloat(AValueList.Values['DepthRatio']);
          Velocity := StrToFloat(AValueList.Values['Velocity']);
        end;
      end;
    end;
  end;

begin
  AMemIniFile := nil;
  AValueList := nil;
  ADecimalSeparator := SysUtils.FormatSettings.DecimalSeparator;
  try
    AValueList := TStringList.Create;
    ResetAll;
    SysUtils.FormatSettings.DecimalSeparator := '.';
    AMemIniFile := TMemIniFile.Create(AFileName, TEncoding.UTF8);
    ReadGeneralSection;
    ReadXSections;
    ReadSessions;
    ReadSlices;
    ReadMeasurments;
  finally
    SysUtils.FormatSettings.DecimalSeparator := ADecimalSeparator;
    AMemIniFile.Free;
    AValueList.Free;
  end;
  SetControlStatus;
  if lstSessions.Count>0 then
  begin
    lstSessions.ItemIndex := 0;
    lstSessions.OnClick(nil);
  end;
end;

procedure TFrmHydrometry.SaveAll(AFileName: string);
var
  AMemIniFile: TMemIniFile;
  ADecimalSeparator: Char;
  TotalSlices, TotalMeasurements: Integer;

  procedure WriteGeneralSection;
  var
    i, j, k: Integer;
  begin
    TotalSlices := 0;
    TotalMeasurements := 0;
    for i := 0 to HydrometrySessionList.Count-1 do
      with THydrometrySession(HydrometrySessionList[i]) do
      begin
        ID := i;
        for j := 0 to Length(Slices)-1 do
        begin
          Inc(TotalSlices);
          Slices[j].ID := TotalSlices-1;
          for k := 0 to Length(Slices[j].Properties.Measures)-1 do
          begin
            Inc(TotalMeasurements);
            Slices[j].Properties.Measures[k].ID := TotalMeasurements-1;
          end;
        end;
      end;
    with AMemIniFile do
    begin
      WriteString('General', 'DateFormat', 'yyyy-mm-dd hh:nn');
      WriteInteger('General', 'SectionsCount', Length(Sections));
      WriteInteger('General', 'SessionsCount', HydrometrySessionList.Count);
      WriteInteger('General', 'TotalSlices', TotalSlices);
      WriteInteger('General', 'TotalMeasurements', TotalMeasurements);
      WriteInteger('General', 'OutputTimeseries',
        rgrpOutputTimeseries.ItemIndex);
    end;
  end;

  procedure WriteXSections;
  var
    i, j: Integer;
  begin
    for i := 0 to Length(Sections)-1 do
      for j := 0 to Sections[i].Count-1 do
        with AMemIniFile do
          WriteString('XSection'+IntToStr(i), 'Point'+IntToStr(j),
            FloatToStr(Sections[i].Nodes[j].x)+'$'+
            FloatToStr(Sections[i].Nodes[j].y));
  end;

  procedure WriteSessions;
  var
    i, j, k: Integer;
    s: string;
  begin
    with AMemIniFile do
      for i := 0 to HydrometrySessionList.Count-1 do
        with THydrometrySession(HydrometrySessionList[i]) do
        begin
          s := 'Session'+IntToStr(i);
          WriteInteger(s, 'ID', ID);
          WriteString(s, 'Date', FormatDateTime('yyyy-mm-dd hh:nn', Date));
          WriteFloat(s, 'Stage', Stage);
          WriteInteger(s, 'XSection', SectionIndex);
          WriteBool(s, 'IsMeasuredFromCut', IsMeasuredFromCut);
          for j := 0 to Length(Slices)-1 do
            with Slices[j] do
            begin
              s := 'Slice'+IntToStr(Slices[j].ID);
              WriteInteger(s, 'ID', Slices[j].ID);
              WriteInteger(s, 'SessionID',
                THydrometrySession(HydrometrySessionList[i]).ID);
              WriteFloat(s, 'XPosition', Properties.XPosition);
              WriteFloat(s, 'coef1', coef1);
              WriteFloat(s, 'coef2', coef2);
              WriteFloat(s, 'detfac', detfac);
              WriteFloat(s, 'MeanVelocity', MeanVelocity);
              WriteInteger(s, 'Mode', Ord(Properties.Mode));
              WriteBool(s, 'IsMeasuredActualDepth',
                Properties.IsMeasuredActualDepth);
              for k := 0 to Length(Properties.Measures)-1 do
                with Properties.Measures[k] do
                begin
                  s := 'Measure'+IntToStr(Properties.Measures[k].ID);
                  WriteInteger(s, 'ID', Properties.Measures[k].ID);
                  WriteInteger(s, 'SliceID', Slices[j].ID);
                  WriteFloat(s, 'Depth', Depth);
                  WriteFloat(s, 'DepthRatio', DepthRatio);
                  WriteFloat(s, 'Velocity', Velocity);
                end;
            end;
        end;
  end;

begin
  AMemIniFile := nil;
  ADecimalSeparator := SysUtils.FormatSettings.DecimalSeparator;
  try
    SysUtils.FormatSettings.DecimalSeparator := '.';
    AMemIniFile := TMemIniFile.Create(AFileName, TEncoding.UTF8);
    WriteGeneralSection;
    WriteXSections;
    WriteSessions;
    AMemIniFile.UpdateFile;
  finally
    SysUtils.FormatSettings.DecimalSeparator := ADecimalSeparator;
    AMemIniFile.Free;
  end;
end;

procedure TFrmHydrometry.ResetAll;
var
  i: Integer;
begin
  HydrometrySessionList.Clear;
  for i := 0 to Length(Sections)-1 do
    Sections[i].Clear;
  SetControlStatus;
  rgrpOutputTimeseries.ItemIndex := 0;
  for i := 0 to ComponentCount-1 do
    if Components[i] is TEdit then
      with Components[i] as TEdit do
        Text := '';
  cmbSliceMode.ItemIndex := 0;
  spnMeasurementsNum.Position := 3;
  edtMeasurementsNum.Text := '3';
end;

procedure TFrmHydrometry.mnuPrintVelocityProfileClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    chartMeasurements.Print;
end;

end.
