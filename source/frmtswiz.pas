{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-10 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit frmtswiz;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, ts, ButtonGroup, CategoryButtons, Spin;

type
  TOffsetBounds = record
    MinutesMin, MinutesMax, MonthsMin, MonthsMax: Integer;
  end;

type
  TFrmTimeseriesWizard = class(TForm)
    PageControl: TPageControl;
    tabGeneral: TTabSheet;
    tabTimestep: TTabSheet;
    tabIntervalType: TTabSheet;
    lblTitle: TLabel;
    edtTitle: TEdit;
    lblComments: TLabel;
    memoComments: TMemo;
    lblUnit: TLabel;
    cmbUnits: TComboBox;
    cmbTimeZone: TComboBox;
    lblTimezone: TLabel;
    lblVariable: TLabel;
    cmbVariable: TComboBox;
    rbVariableTimeStep: TRadioButton;
    rbRegularTimeStep: TRadioButton;
    ImageList: TImageList;
    chkTimestepStrict: TCheckBox;
    btnPrevious: TButton;
    btnNext: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    rbInstantaneous: TRadioButton;
    rbInterval: TRadioButton;
    cmbIntervalType: TComboBox;
    chkIntervalAdvanced: TCheckBox;
    grpActualOffset: TGroupBox;
    lblIntervalType: TLabel;
    lblHydrologicalYearOrigin: TLabel;
    cmbHydrologicalYearOrigin: TComboBox;
    cmbOtherTimeStep: TComboBox;
    lblOtherTimeStep: TLabel;
    spinOtherTimestep: TSpinEdit;
    btnsTimeSteps: TCategoryButtons;
    lblPrecision: TLabel;
    spinPrecision: TSpinEdit;
    grpTemplates: TGroupBox;
    lstTemplates: TListBox;
    lblTemplates: TLabel;
    Label1: TLabel;
    memoTemplateDescription: TMemo;
    btnLoadTemplate: TButton;
    btnDeleteTemplate: TButton;
    Label2: TLabel;
    edtActualOffsetMinutes: TEdit;
    spinActualOffsetMinutesHours: TSpinButton;
    spinActualOffsetMinutesMins: TSpinButton;
    Label3: TLabel;
    spinActualOffsetMonths: TSpinEdit;
    procedure FormShow(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure chkIntervalAdvancedClick(Sender: TObject);
    procedure btnsTimeStepsSelectedCategoryChange(Sender: TObject;
      const Category: TButtonCategory);
    procedure edtTitleChange(Sender: TObject);
    procedure memoCommentsChange(Sender: TObject);
    procedure cmbVariableChange(Sender: TObject);
    procedure cmbTimeZoneChange(Sender: TObject);
    procedure cmbUnitsChange(Sender: TObject);
    procedure rbVariableTimeStepClick(Sender: TObject);
    procedure rbRegularTimeStepClick(Sender: TObject);
    procedure cmbHydrologicalYearOriginChange(Sender: TObject);
    procedure spinOtherTimestepChange(Sender: TObject);
    procedure chkTimestepStrictClick(Sender: TObject);
    procedure rbInstantaneousClick(Sender: TObject);
    procedure rbIntervalClick(Sender: TObject);
    procedure cmbIntervalTypeChange(Sender: TObject);
    procedure spinPrecisionChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDeleteTemplateClick(Sender: TObject);
    procedure btnLoadTemplateClick(Sender: TObject);
    procedure spinActualOffsetMonthsChange(Sender: TObject);
    procedure spinActualOffsetMinutesMinsUpClick(Sender: TObject);
    procedure spinActualOffsetMinutesMinsDownClick(Sender: TObject);
    procedure spinActualOffsetMinutesHoursDownClick(Sender: TObject);
    procedure spinActualOffsetMinutesHoursUpClick(Sender: TObject);
  private
    FNewTimeseriesMode: Boolean;
    FOnLoadingTemplate: Boolean;
    FMainTs, FTsCopy: TTimeseries;
    FHYearOrigin: Integer;
    FTemplatePath: string;
    FActualOffsetBounds: TOffsetBounds;
    procedure SetActualOffsetBounds;
    procedure IncDecActualOffsetMin(IncBy: Integer);
    procedure SetControlStatus;
    procedure SelectFromTimestep(ATimeseries: TTimeseries);
    procedure TsButonClick(Sender: TObject);
  public
    property NewTimeseriesMode: Boolean read FNewTimeseriesMode write
      FNewTimeseriesMode;
    property MainTs: TTimeseries read FMainTs write FMainTs;
    property TsCopy: TTimeseries read FTsCopy write FTsCopy;
    property HYearOrigin: Integer read FHYearOrigin write FHYearOrigin;
    property TemplatePath: string read FTemplatePath write FTemplatePath;
  end;

  type
    TTStepsCategory = record
      Name: string;
    end;

    TTStepItem = record
      Name: string;
      Timestep: TTimeStep;
      ImgIdx: Integer;
      Cat: Integer;
      Offset: Integer;
      Data: TButtonItem;
      constructor Create(AName: string; ATimestep: TTimestep;
        AImgIdx, ACat, AOffset: Integer);
    end;

resourcestring
  rsCommonsteps='Common time steps';
  rsSmalltimesteps='Precise time steps';
  rsHourlytimesteps='Hourly time steps';
  rsSeasonaltimesteps='Seasonal time steps';
  rsAnnualandoveryeartimesteps='Annual and over-year time steps';
  rsOthertimesteps='Other time steps';

  const
    ButtonsCategories: array[0..5] of TTStepsCategory =(
      (Name: rsCommonsteps),
      (Name: rsSmalltimesteps),
      (Name: rsHourlytimesteps),
      (Name: rsSeasonaltimesteps),
      (Name: rsAnnualandoveryeartimesteps),
      (Name: rsOthertimesteps)
    );

implementation

{$R *.dfm}

uses Math, itzones, ifile;

var
  ButtonsItems: array[0..25] of TTStepItem;

constructor TTStepItem.Create(AName: string; ATimestep: TTimestep;
  AImgIdx, ACat, AOffset: Integer);
begin
  Name := AName;
  Timestep := ATimestep;
  ImgIdx := AImgIdx;
  Cat := ACat;
  Offset := AOffset;
end;

function AddPlusSign(AValue: Real): string;
begin
  Result := Format('%.4d', [Round(AValue*100)]);
  if AValue>=0 then
    Result := '+'+Result;
end;

procedure TFrmTimeseriesWizard.SetActualOffsetBounds;
begin
  with FActualOffsetBounds do
  begin
    MonthsMin := 0;
    MonthsMax := FTsCopy.TimeStep.LengthMonths;
    if FTsCopy.TimeStep>=tstMonthly then
    begin
      MinutesMin := -1440;
      MinutesMax := 1440;
    end else begin
      MinutesMin := 0;
      MinutesMax := FTsCopy.TimeStep.LengthMinutes;
    end;
  end;
end;

procedure TFrmTimeseriesWizard.IncDecActualOffsetMin(IncBy: Integer);
var
  AMin, ASign: Integer;
begin
  AMin := FTsCopy.ActualOffset.Minutes+IncBy;
  if AMin<0 then ASign := -1 else ASign := 1;
  AMin := (Abs(AMin) div IncBy)*IncBy*ASign;
  AMin := Max(AMin, FActualOffsetBounds.MinutesMin);
  AMin := Min(AMin, FActualOffsetBounds.MinutesMax);
  FTsCopy.ActualOffset :=
    TDateOffset.Create(AMin, FTsCopy.ActualOffset.Months);
end;

procedure TFrmTimeseriesWizard.TsButonClick(Sender: TObject);
var
  i, j: Integer;
begin
  for i := 0 to High(ButtonsItems) do
  begin
    j := i;
    if ButtonsItems[i].Data = btnsTimeSteps.SelectedItem then
      Break;
  end;
  if j< 25 then FTsCopy.TimeStep := ButtonsItems[j].Timestep else
    FTsCopy.TimeStep := TTimeStep.Create(1,0,0);
  if FTsCopy.TimeStep = tstAnnual then
    FTsCopy.NominalOffset := TDateOffset.Create(0, ButtonsItems[j].Offset);
  if j = 21 then {Ugly!}
    FTsCopy.NominalOffset := TDateOffset.Create(0,
      cmbHydrologicalYearOrigin.ItemIndex);
  if (FNewTimeseriesMode) and (not FOnLoadingTemplate) then
  begin
    if FTsCopy.TimeStep<=tstDaily then
      FTsCopy.ActualOffset := TDateOffset.Create(0,0) else
      FTsCopy.ActualOffset := TDateOffset.Create(
        FTsCopy.TimeStep.LengthMinutes, FTsCopy.TimeStep.LengthMonths);
    SetActualOffsetBounds;
  end;
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.SelectFromTimestep(ATimeseries: TTimeseries);
var
  i, j: Integer;
  AFound: Boolean;
begin
  if ATimeseries.TimeStep.IsVariable then Exit;
  AFound := False;
  for i := 0 to High(ButtonsItems)-1 do
  begin
    j :=i ;
    with ButtonsItems[i] do
    begin
      if ATimeseries.TimeStep = Timestep then AFound := True;
      if (ATimeseries.TimeStep=tstAnnual) then
        AFound := AFound and (ATimeseries.NominalOffset.Months = Offset);
      if (ATimeseries.TimeStep=tstAnnual) and (i=21) then AFound := True;
      if AFound then Break;
    end;
  end;
  if AFound then btnsTimeSteps.SelectedItem := ButtonsItems[j].Data else
  btnsTimeSteps.SelectedItem := ButtonsItems[High(ButtonsItems)].Data;
  SetControlStatus;
end;

resourcestring
  rsConfirmDeleteTemplate =
    'Are you sure you want to delete template file?';

procedure TFrmTimeseriesWizard.btnDeleteTemplateClick(Sender: TObject);
begin
  if lstTemplates.Count<1 then Exit;
  if lstTemplates.ItemIndex<0 then Exit;
  if MessageDlg(rsConfirmDeleteTemplate, mtConfirmation, mbYesNo, 0,
    mbNo)=mrNo then Exit;
  with lstTemplates do
    DeleteFile(TTimeseries(Items.Objects[ItemIndex]).FileName);
  with lstTemplates do
    Items.Delete(ItemIndex);
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.btnLoadTemplateClick(Sender: TObject);
var
  i: Integer;
begin
  if lstTemplates.Count<1 then Exit;
  if lstTemplates.ItemIndex<0 then Exit;
  with lstTemplates do
    FTsCopy.AssignMeta(TTimeseries(Items.Objects[ItemIndex]));
  SetActualOffsetBounds;
  try
    FOnLoadingTemplate := True;
    rbVariableTimeStep.Checked := True;
    if not FTsCopy.TimeStep.IsVariable then
      rbRegularTimeStep.Checked := True;
    rbInstantaneous.Checked := True;
    if not (FTsCopy.VariableType = vtInstantaneous) then
      rbInterval.Checked := True;
    for i := 0 to High(ButtonsCategories) do
      btnsTimeSteps.Categories[i].Collapsed := True;
    if not FTsCopy.TimeStep.IsVariable then
    begin
      SelectFromTimestep(FTsCopy);
      TButtonItem(btnsTimeSteps.SelectedItem).Category.Collapsed := False;
    end;
  finally
    FOnLoadingTemplate := False;
  end;
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.btnNextClick(Sender: TObject);
begin
  PageControl.ActivePageIndex :=
    Min(2, PageControl.ActivePageIndex+1);
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.btnOKClick(Sender: TObject);
var
  s: string;
begin
  s := RemoveTZDescr(FTsCopy.TimeZone);
  if s<>'' then FTsCopy.TimeZone := s;
end;

procedure TFrmTimeseriesWizard.btnPreviousClick(Sender: TObject);
begin
  PageControl.ActivePageIndex :=
    Max(0, PageControl.ActivePageIndex-1);
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.btnsTimeStepsSelectedCategoryChange(
  Sender: TObject; const Category: TButtonCategory);
begin
  SelectFromTimestep(FTsCopy);
end;

procedure TFrmTimeseriesWizard.chkIntervalAdvancedClick(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.chkTimestepStrictClick(Sender: TObject);
var
  SavedCursor: TCursor;
  CanSetStrict: Boolean;
  s: string;
  ANominalOffset: TDateOffset;
begin
  if FNewTimeseriesMode then
  begin
    FTsCopy.TimeStepStrict := (Sender as TCheckBox).Checked;
    Exit;
  end;
  if FTsCopy.TimeStepStrict = chkTimestepStrict.Checked then Exit;
  if not chkTimestepStrict.Checked then FTsCopy.TimeStepStrict := False;
  if chkTimestepStrict.Checked then
  begin
    SavedCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      CanSetStrict := FMainTs.CheckTimeStepStrict(s, ANominalOffset);
      FTsCopy.TimeStepStrict := CanSetStrict;
      if CanSetStrict then FTsCopy.NominalOffset := ANominalOffset;
      if not CanSetStrict then
        MessageDlg(s, mtError, [mbOK], 0);
    finally
      Screen.Cursor := SavedCursor;
    end;
  end;
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.cmbHydrologicalYearOriginChange(Sender: TObject);
begin
  if FTsCopy.TimeStep<>tstAnnual then Exit;
  FTsCopy.NominalOffset :=
    TDateOffset.Create(0, cmbHydrologicalYearOrigin.ItemIndex);
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.cmbIntervalTypeChange(Sender: TObject);
begin
  if not cmbIntervalType.Enabled then Exit;
  case cmbIntervalType.ItemIndex of
    0: FTsCopy.VariableType := vtCumulative;
    1: FTsCopy.VariableType := vtAverage;
    2: FTsCopy.VariableType := vtMaximum;
    3: FTsCopy.VariableType := vtMinimum;
    4: FTsCopy.VariableType := vtVectorAverage;
  else
    FTsCopy.VariableType := vtInstantaneous;
  end;
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.cmbTimeZoneChange(Sender: TObject);
begin
  FTsCopy.TimeZone := (Sender as TComboBox).Text;
end;

procedure TFrmTimeseriesWizard.cmbUnitsChange(Sender: TObject);
begin
  FTsCopy.MUnit := (Sender as TComboBox).Text;
end;

procedure TFrmTimeseriesWizard.cmbVariableChange(Sender: TObject);
begin
  FTsCopy.VarName := (Sender as TComboBox).Text;
end;

procedure TFrmTimeseriesWizard.edtTitleChange(Sender: TObject);
begin
  FTsCopy.Title := (Sender as TEdit).Text;
end;

procedure TFrmTimeseriesWizard.FormCreate(Sender: TObject);
var
  i: Integer;
  AButtonItem: TButtonItem;
begin
  with FActualOffsetBounds do
  begin
    MinutesMin := 0;
    MinutesMax := 0;
    MonthsMin := 0;
    MonthsMax := 0;
  end;
  FHYearOrigin := 10;
  FTemplatePath := '';
  FOnLoadingTemplate := False;
  for i := 0 to Length(TimeZonesArray)-1 do
    with TimeZonesArray[i] do
      cmbTimeZone.Items.Add(Abbrev+' (UTC'+ AddPlusSign(Offset) +') '+ Descr);
  for i := 1 to 12 do
    cmbHydrologicalYearOrigin.Items.Add(SysUtils.LongMonthNames[i]);
  cmbHydrologicalYearOrigin.ItemIndex := 9;
  for i := 0 to High(ButtonsCategories) do
    with btnsTimeSteps.Categories do
    begin
      Add;
      Items[i].Caption := ButtonsCategories[i].Name;
  end;
  for i := 0 to High(ButtonsItems) do
    with ButtonsItems[i] do
    begin
      AButtonItem := btnsTimeSteps.Categories[Cat].Items.Add;
      AButtonItem.Caption := Name;
      AButtonItem.ImageIndex := ImgIdx;
      AButtonItem.OnClick := TsButonClick;
      Data := AButtonItem;
    end;
end;

procedure TFrmTimeseriesWizard.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lstTemplates.Count-1 do
    if lstTemplates.Items.Objects[i]<>nil then
      lstTemplates.Items.Objects[i].Free;
end;

procedure TFrmTimeseriesWizard.FormShow(Sender: TObject);

  procedure LoadTemplateFiles;
  var
    i, j: Integer;
    AStringList: TStringList;
    ATimeseries: TTimeseries;
  begin
    AStringList := nil;
    try
      AStringList := TStringList.Create;
      FindFiles(AStringList, FTemplatePath, '*.hts', False);
      ATimeseries := nil;
      j := 0;
      for i := 0 to AStringList.Count-1 do
      begin
        try
          ATimeseries := TTimeseries.Create;
          try
            ATimeseries.LoadMetaFromFile(AStringList[i]);
            Inc(j);
            with lstTemplates do
              AddItem(IntToStr(j)+': '+ ATimeseries.Title, ATimeseries);
            ATimeseries := nil;
          except
            {do not raise if cannot load, just do not add to list ...}
          end;
        finally
          ATimeseries.Free;
        end;
      end;
    finally
      AStringList.Free;
    end;
  end;

var
  i: Integer;
begin
  grpTemplates.Visible := FNewTimeseriesMode;
  if FNewTimeseriesMode then
    ButtonsItems[21].Offset := FHYearOrigin-1;
  cmbHydrologicalYearOrigin.ItemIndex := FHYearOrigin-1;
  if not FNewTimeseriesMode then
  begin
    PageControl.HotTrack := True;
    SetActualOffsetBounds;
  end;
  for i := 0 to High(ButtonsCategories) do
    btnsTimeSteps.Categories[i].Collapsed := True;
  if (not FNewTimeseriesMode) and (not FTsCopy.TimeStep.IsVariable) then
  begin
    SelectFromTimestep(FTsCopy);
    TButtonItem(btnsTimeSteps.SelectedItem).Category.Collapsed := False;
  end;
  if (FNewTimeseriesMode) and (FTemplatePath<>'') then  LoadTemplateFiles;
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.memoCommentsChange(Sender: TObject);
begin
  FTsCopy.Comment := (Sender as TMemo).Text;
end;

procedure TFrmTimeseriesWizard.PageControlChange(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange := not FNewTimeseriesMode;
end;

procedure EnableControlAndChilds(AControl: TWinControl; ToEnable: Boolean);
var
  i: Integer;
begin
  with AControl do
    for i := 0 to ControlCount-1 do
      if Controls[i] is TWinControl then
        EnableControlAndChilds(Controls[i] as TWinControl, ToEnable)
      else
        Controls[i].Enabled := ToEnable;
  AControl.Enabled := ToEnable;
end;

resourcestring
  rsBtnOK = 'OK';
  rsBtnFinish = 'Finish';

procedure TFrmTimeseriesWizard.SetControlStatus;

  procedure DisplayCustomTimeStep;
  var
    AValue: Integer;
  begin
    AValue := 1;
    if FTsCopy.TimeStep.LengthMonths>=1 then
    begin
      if FTsCopy.TimeStep.LengthMonths>=12 then
      begin
        if (FTsCopy.TimeStep.LengthMonths mod 12) = 0 then
          cmbOtherTimeStep.ItemIndex := 4;
      end else
        cmbOtherTimeStep.ItemIndex := 3;
    end;
    if FTsCopy.TimeStep.LengthMinutes>=1 then
    begin
      if FTsCopy.TimeStep.LengthMinutes>=1440 then
      begin
        if (FTsCopy.TimeStep.LengthMonths mod 1440) = 0 then
          cmbOtherTimeStep.ItemIndex := 2;
      end
      else if FTsCopy.TimeStep.LengthMinutes>=60 then begin
        if (FTsCopy.TimeStep.LengthMonths mod 60) = 0 then
          cmbOtherTimeStep.ItemIndex := 1;
      end else
        cmbOtherTimeStep.ItemIndex := 0;
    end;
    case cmbOtherTimeStep.ItemIndex of
      0: AValue := FTsCopy.TimeStep.LengthMinutes;
      1: AValue := FTsCopy.TimeStep.LengthMinutes div 60;
      2: AValue := FTsCopy.TimeStep.LengthMinutes div 1440;
      3: AValue := FTsCopy.TimeStep.LengthMonths;
      4: AValue := FTsCopy.TimeStep.LengthMonths div 12;
    end;
    spinOtherTimestep.Value := AValue;
  end;

  procedure SetTimeZoneCombo;
  var
    i,j: Integer;
  begin
    cmbTimeZone.ItemIndex := -1;
    j := -1;
    for i := 0 to cmbTimeZone.Items.Count-1 do
    begin
      if StrPos(PChar(cmbTimeZone.Items[i]), PChar(FTsCopy.TimeZone))<>nil then
      begin
        j := i;
        Break;
      end;
    end;
    if j>-1 then cmbTimeZone.ItemIndex := j else
      cmbTimeZone.Text := FTsCopy.TimeZone;
  end;

resourcestring
  rsFilename = 'File name: ';
  rsTitle = 'Timeseries title: ';
  rsStepMinutes = 'Step minutes: ';
  rsStepMonths = 'Step months: ';
  rsVariableStep = 'Variable time step';
  rsTimeStepIsStrict = 'Time step is strict';

  procedure UpdateTemplateMemo;
  begin
    with lstTemplates do
      with TTimeseries(Items.Objects[ItemIndex]) do
      begin
        memoTemplateDescription.Text :=
          rsFilename+ExtractFileName(FileName)+#13#10+
          rsTitle+Title;
        if TimeStep.IsVariable then
          memoTemplateDescription.Lines.Add(rsVariableStep)
        else
        begin
          if TimeStep.LengthMinutes>0 then
            memoTemplateDescription.Lines.Add(rsStepMinutes +
              IntToStr(TimeStep.LengthMinutes));
          if TimeStep.LengthMonths>0 then
            memoTemplateDescription.Lines.Add(rsStepMonths +
              IntToStr(TimeStep.LengthMonths));
          if TimeStepStrict then
            memoTemplateDescription.Lines.Add(rsTimeStepIsStrict);
        end;
      end;
  end;

var
  s: string;
begin
  if FNewTimeseriesMode then btnOK.Caption := rsBtnFinish else
    btnOK.Caption := rsBtnOK;
  btnPrevious.Visible := PageControl.ActivePageIndex>0;
  btnNext.Enabled := PageControl.ActivePageIndex<2;
  if FNewTimeseriesMode then
    btnOK.Enabled := PageControl.ActivePageIndex=2;
  with FTsCopy do
  begin
    edtTitle.Text := Title;
    memoComments.Text := Comment;
    cmbVariable.Text := VarName;
    cmbUnits.Text := MUnit;
    SetTimeZoneCombo;
    spinPrecision.Value := Precision;
    rbVariableTimeStep.Enabled := FNewTimeseriesMode;
    rbRegularTimeStep.Enabled := FNewTimeseriesMode;
    rbVariableTimeStep.Checked := TimeStep.IsVariable;
    rbRegularTimeStep.Checked := not TimeStep.IsVariable;
    btnsTimeSteps.Enabled := (FNewTimeseriesMode) and (not TimeStep.IsVariable);
    chkTimestepStrict.Enabled :=
      (not TimeStep.IsVariable) and (TimeStep<tstMonthly);
    chkTimestepStrict.Checked := (TimeStep>=tstMonthly) or (TimeStepStrict);
    lblHydrologicalYearOrigin.Visible :=
      (not TimeStep.IsVariable) and
        (ButtonsItems[21].Data = btnsTimeSteps.SelectedItem);
    lblHydrologicalYearOrigin.Enabled := FNewTimeseriesMode;
    cmbHydrologicalYearOrigin.Visible := lblHydrologicalYearOrigin.Visible;
    cmbHydrologicalYearOrigin.Enabled := lblHydrologicalYearOrigin.Enabled;
    if (TimeStep = tstAnnual) and (TimeStepStrict) then
      cmbHydrologicalYearOrigin.ItemIndex :=
        NominalOffset.Months;    //Ugly Workarround
    lblOtherTimeStep.Visible :=
      ButtonsItems[25].Data = btnsTimeSteps.SelectedItem;
    lblOtherTimeStep.Enabled := FNewTimeseriesMode;
    spinOtherTimestep.Visible := lblOtherTimeStep.Visible;
    spinOtherTimestep.Enabled := lblOtherTimeStep.Enabled;
    cmbOtherTimeStep.Visible := lblOtherTimeStep.Visible;
    cmbOtherTimeStep.Enabled := lblOtherTimeStep.Enabled;
    DisplayCustomTimeStep;
    rbInstantaneous.Enabled := not TimeStep.IsVariable;
    rbInterval.Enabled := rbInstantaneous.Enabled;
    rbInstantaneous.Checked := VariableType=vtInstantaneous;
    rbInterval.Checked := not rbInstantaneous.Checked;
    cmbIntervalType.Enabled := (rbInterval.Checked) and rbInterval.Enabled;
    lblIntervalType.Enabled := cmbIntervalType.Enabled;
    chkIntervalAdvanced.Enabled := rbInstantaneous.Enabled;
    case VariableType of
      vtCumulative, vtInstantaneous: cmbIntervalType.ItemIndex := 0;
      vtAverage: cmbIntervalType.ItemIndex := 1;
      vtMaximum: cmbIntervalType.ItemIndex := 2;
      vtMinimum: cmbIntervalType.ItemIndex := 3;
      vtVectorAverage: cmbIntervalType.ItemIndex := 4;
    else
      cmbIntervalType.ItemIndex := -1;
    end;
    EnableControlAndChilds(grpActualOffset, rbInterval.Enabled and
      chkIntervalAdvanced.Checked);
    s := '';
    if ActualOffset.Minutes<0 then s := '-';
    s := s+FormatFloat('00',Abs(ActualOffset.Minutes div 60))+':';
    s := s+FormatFloat('00',Abs(ActualOffset.Minutes) mod 60);
    edtActualOffsetMinutes.Text := s;
    spinActualOffsetMonths.Value := ActualOffset.Months;
  end;
// Templates
  btnLoadTemplate.Enabled := (lstTemplates.Count>0) and
    (lstTemplates.ItemIndex>-1);
  btnDeleteTemplate.Enabled := btnLoadTemplate.Enabled;
  memoTemplateDescription.Clear;
  if (lstTemplates.Count>0) and (lstTemplates.ItemIndex>-1) then
    UpdateTemplateMemo;
end;

procedure TFrmTimeseriesWizard.spinActualOffsetMinutesHoursDownClick(
  Sender: TObject);
begin
  if FOnLoadingTemplate then Exit;
  IncDecActualOffsetMin(-60);
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.spinActualOffsetMinutesHoursUpClick(
  Sender: TObject);
begin
  if FOnLoadingTemplate then Exit;
  IncDecActualOffsetMin(60);
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.spinActualOffsetMinutesMinsDownClick(
  Sender: TObject);
begin
  if FOnLoadingTemplate then Exit;
  IncDecActualOffsetMin(-1);
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.spinActualOffsetMinutesMinsUpClick(
  Sender: TObject);
begin
  if FOnLoadingTemplate then Exit;
  IncDecActualOffsetMin(1);
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.spinActualOffsetMonthsChange(Sender: TObject);
begin
  if FOnLoadingTemplate then Exit;
  if (FTsCopy.ActualOffset.Months = spinActualOffsetMonths.Value) then Exit;
  with Sender as TSpinEdit do
  begin
    Value := Min(FActualOffsetBounds.MonthsMax, Value);
    Value := Max(FActualOffsetBounds.MonthsMin, Value);
    FTsCopy.ActualOffset :=
      TDateOffset.Create(FTsCopy.ActualOffset.Minutes, Value);
  end;
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.spinOtherTimestepChange(Sender: TObject);
var
  AMultiplier: Integer;
  ATimestep: TTimestep;
begin
  if btnsTimeSteps.SelectedItem <> ButtonsItems[25].Data then Exit;
  AMultiplier := 1;
  case cmbOtherTimeStep.ItemIndex of
    0: AMultiplier := 1;
    1: AMultiplier := 60;
    2: AMultiplier := 1440;
    3: AMultiplier := 1;
    4: AMultiplier := 12;
  else
    Assert(False);
  end;
  if cmbOtherTimeStep.ItemIndex<3 then
    ATimestep := TTimestep.Create(AMultiplier*spinOtherTimestep.Value,
      0, 0) else
    ATimestep := TTimestep.Create(0, AMultiplier*spinOtherTimestep.Value, 0);
  FTsCopy.TimeStep := ATimestep;
  if ATimestep>=tstMonthly then
    FTsCopy.TimeStepStrict := True;
  if (FNewTimeseriesMode) and (not FOnLoadingTemplate) then
  begin
    if FTsCopy.TimeStep<=tstDaily then
      FTsCopy.ActualOffset := TDateOffset.Create(0,0) else
      FTsCopy.ActualOffset := TDateOffset.Create(
        FTsCopy.TimeStep.LengthMinutes, FTsCopy.TimeStep.LengthMonths);
      SetActualOffsetBounds;
  end;
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.spinPrecisionChange(Sender: TObject);
begin
  FTsCopy.Precision := spinPrecision.Value;
end;

procedure TFrmTimeseriesWizard.rbInstantaneousClick(Sender: TObject);
begin
  if FOnLoadingTemplate then Exit;
  FTsCopy.VariableType := vtInstantaneous;
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.rbIntervalClick(Sender: TObject);
begin
  if FOnLoadingTemplate then Exit;
  FTsCopy.VariableType := vtCumulative;
  SetControlStatus;
end;

procedure TFrmTimeseriesWizard.rbRegularTimeStepClick(Sender: TObject);
begin
  if not FNewTimeseriesMode then Exit;
  if FOnLoadingTemplate then Exit;
  FTsCopy.TimeStep := tstTenMinute;
  FTsCopy.VariableType := vtCumulative;
  FTsCopy.TimeStepStrict := True;
  btnsTimeSteps.Categories[0].Collapsed := False;
  SelectFromTimestep(FTsCopy);
  SetControlStatus;
  if btnsTimeSteps.CanFocus then
    btnsTimeSteps.SetFocus;
end;

procedure TFrmTimeseriesWizard.rbVariableTimeStepClick(Sender: TObject);
var
  i: Integer;
begin
  if not FNewTimeseriesMode then Exit;
  if FOnLoadingTemplate then Exit;
  FTsCopy.TimeStep := tstVariable;
  FTsCopy.VariableType := vtInstantaneous;
  FTsCopy.ActualOffset := TDateOffset.Create(0,0);
  SetActualOffsetBounds;
  for i := 0 to btnsTimeSteps.Categories.Count-1 do
    btnsTimeSteps.Categories[i].Collapsed := True;
  btnsTimeSteps.SelectedItem := nil;
  SetControlStatus;
end;

resourcestring
  rsFiveMinute = 'Five minute';
  rsTenMinute = 'Ten minute';
  rsFithteenMinute = 'Quarter (Fithteen minute)';
  rsTwentyMinute = 'Twenty minute';
  rsHalfHour = 'Half hour (Thirty minute)';
  rsHourly = 'Hourly';
  rsTwoHour = 'Two hour';
  rsThreeHour = 'Three hour';
  rsFourHour = 'Four hour';
  rsSixHour = 'Six hour';
  rsEightHour = 'Eight hour';
  rsTwelveHour = 'Half day (Twelve hour)';
  rsDaily = 'Daily';
  rsMonthly = 'Monthly';
  rsTwoMonth = 'Two month';
  rsThreeMonth = 'Trimester (Three month)';
  rsFourMonth = 'Term (Four month)';
  rsSixMonth = 'Semester (Six month)';
  rsAnnual = 'Annual';
  rsHydrologicalOctober = 'Hydrological year starting October';
  rsHydrologicalJully = 'Hydrological year starting Jully';
  rsHydrologicalOther = 'Hydrological year (other)';
  rsTwoYear = 'Two year';
  rsFiveYear = 'Five year';
  rsTenYear = 'Decade (ten year)';
  rsUnknown = 'Other';

initialization

  ButtonsItems[0] := TTStepItem.Create(rsFiveMinute, tstFiveMinute,0,1,0);
  ButtonsItems[1] := TTStepItem.Create(rsTenMinute, tstTenMinute,0,0,0);
  ButtonsItems[2] := TTStepItem.Create(rsFithteenMinute, tstFithteenMinute,
    0,1,0);
  ButtonsItems[3] := TTStepItem.Create(rsTwentyMinute, tstTwentyMinute,0,1,0);
  ButtonsItems[4] := TTStepItem.Create(rsHalfHour, tstHalfHour,0,1,0);
  ButtonsItems[5] := TTStepItem.Create(rsHourly, tstHourly, 1,0,0);
  ButtonsItems[6] := TTStepItem.Create(rsTwoHour, tstTwoHour, 1,2,0);
  ButtonsItems[7] := TTStepItem.Create(rsThreeHour, tstThreeHour, 1,2,0);
  ButtonsItems[8] := TTStepItem.Create(rsFourHour, tstFourHour, 1,2,0);
  ButtonsItems[9] := TTStepItem.Create(rsSixHour, tstSixHour, 1,2,0);
  ButtonsItems[10] := TTStepItem.Create(rsEightHour, tstEightHour, 1,2,0);
  ButtonsItems[11] := TTStepItem.Create(rsTwelveHour, tstTwelveHour, 1,2,0);
  ButtonsItems[12] := TTStepItem.Create(rsDaily, tstDaily, 2,0,0);
  ButtonsItems[13] := TTStepItem.Create(rsMonthly, tstMonthly, 3,0,0);
  ButtonsItems[14] := TTStepItem.Create(rsTwoMonth, tstTwoMonth, 4,3,0);
  ButtonsItems[15] := TTStepItem.Create(rsThreeMonth, tstThreeMonth, 4,3,0);
  ButtonsItems[16] := TTStepItem.Create(rsFourMonth, tstFourMonth, 4,3,0);
  ButtonsItems[17] := TTStepItem.Create(rsSixMonth, tstSixMonth, 4,3,0);
  ButtonsItems[18] := TTStepItem.Create(rsAnnual, tstAnnual, 5,0,0);
  ButtonsItems[19] := TTStepItem.Create(rsHydrologicalOctober,tstAnnual, 5,0,9);
  ButtonsItems[20] := TTStepItem.Create(rsHydrologicalJully, tstAnnual, 5,4,6);
  ButtonsItems[21] := TTStepItem.Create(rsHydrologicalOther, tstAnnual, 5,4,0);
  ButtonsItems[22] := TTStepItem.Create(rsTwoYear, tstTwoYear, 6,4,0);
  ButtonsItems[23] := TTStepItem.Create(rsFiveYear, tstFiveYear, 6,4,0);
  ButtonsItems[24] := TTStepItem.Create(rsTenYear, tstTenYear, 6,4,0);
  ButtonsItems[25] := TTStepItem.Create(rsUnknown, tstUnknown, 7,5,0);

end.
