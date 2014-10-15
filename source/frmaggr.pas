{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-09 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit frmaggr;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ts, ExtCtrls, ComCtrls, Spin, icomponent;

type
  TOffsetBounds = record
    MinutesMin, MinutesMax, MonthsMin, MonthsMax: Integer;
  end;

type
  TFrmAggregationDialog = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lblAggregationTimestep: TLabel;
    lstAggregationTimestep: TListBox;
    rgrpVariableType: TRadioGroup;
    grpActualOffset: TGroupBox;
    spinActualOffsetMinutesMins: TSpinButton;
    edtActualOffsetMinutes: TEdit;
    spinActualOffsetMinutesHours: TSpinButton;
    Label1: TLabel;
    spinActualOffsetMonths: TSpinEdit;
    Label2: TLabel;
    grpNominalOffset: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    spinNominalOffsetMinutesMins: TSpinButton;
    edtNominalOffsetMinutes: TEdit;
    spinNominallOffsetMinutesHours: TSpinButton;
    spinNominalOffsetMonths: TSpinEdit;
    Image: TImage;
    lblDestTimestamp1: TLabel;
    lblDestTimestamp2: TLabel;
    lblDestTimestamp3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    lblDestInterval1: TLabel;
    lblDestInterval2: TLabel;
    lblSourceTimestamp1: TLabel;
    lblSourceTimestamp4: TLabel;
    lblSourceInterval1: TLabel;
    lblSourceInterval2: TLabel;
    lblSourceInterval3: TLabel;
    lblSourceInterval4: TLabel;
    Line1: TIShape;
    Line2: TIShape;
    Line4: TIShape;
    Line3: TIShape;
    lblAggregationMode: TLabel;
    grpSettings: TGroupBox;
    chkOutputMissingCountSeries: TCheckBox;
    chkDeleteNullValueEnds: TCheckBox;
    lblAllowedMissingValues: TLabel;
    lblFlagSelection: TLabel;
    lblOtherTimestep: TLabel;
    spinOtherTimeStep: TSpinEdit;
    cmbFlagSelection: TComboBox;
    edtMissingValuesAllowed: TEdit;
    chkFineTunning: TCheckBox;
    chkSeasonalAggregation: TCheckBox;
    grpSeasonalAggregation: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    cmbFromDay: TComboBox;
    cmbToDay: TComboBox;
    cmbFromMonth: TComboBox;
    cmbToMonth: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstAggregationTimestepClick(Sender: TObject);
    procedure rgrpVariableTypeClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure spinActualOffsetMinutesMinsUpClick(Sender: TObject);
    procedure spinActualOffsetMinutesMinsDownClick(Sender: TObject);
    procedure spinActualOffsetMinutesHoursDownClick(Sender: TObject);
    procedure spinActualOffsetMinutesHoursUpClick(Sender: TObject);
    procedure spinActualOffsetMonthsChange(Sender: TObject);
    procedure spinNominalOffsetMonthsChange(Sender: TObject);
    procedure spinNominalOffsetMinutesMinsDownClick(Sender: TObject);
    procedure spinNominalOffsetMinutesMinsUpClick(Sender: TObject);
    procedure spinNominallOffsetMinutesHoursUpClick(Sender: TObject);
    procedure spinNominallOffsetMinutesHoursDownClick(Sender: TObject);
    procedure spinOtherTimeStepChange(Sender: TObject);
    procedure chkFineTunningClick(Sender: TObject);
    procedure chkSeasonalAggregationClick(Sender: TObject);
    procedure cmbFromDayChange(Sender: TObject);
  private
    FSourceSeries, FDestSeries: TTimeseries;
    FAggrOptions: TAggregationOptionsRec;
    FActualOffsetBounds: TOffsetBounds;
    FNominalOffsetBounds: TOffsetBounds;
    FHYearOrigin: Integer;
    procedure InitDestSeries;
    procedure SetControlStatus;
    procedure IncDecActualOffsetMin(IncBy: Integer);
    procedure IncDecNominalOffsetMin(IncBy: Integer);
    procedure DrawTimestamps;
  public
    property AggrOptions: TAggregationOptionsRec read FAggrOptions
      write FAggrOptions;
    property SourceSeries: TTimeseries read FSourceSeries write FSourceSeries;
    property DestSeries: TTimeseries read FDestSeries write FDestSeries;
    property HYearOrigin: Integer read FHYearOrigin write FHYearOrigin;
  end;

  TTStepItem = record
    Name: string;
    Timestep: TTimeStep;
    Offset: Integer;
    ItemIndex: Integer;
    constructor Create(AName: string; ATimestep: TTimestep;
      AItemIndex, AOffset: Integer);
  end;

implementation

{$R *.dfm}

uses
  Math, Dates, DateUtils;

var
  ButtonsItems: array[0..31] of TTStepItem;
  OtherStepMonths: Integer;


constructor TTStepItem.Create(AName: string; ATimestep: TTimestep;
      AItemIndex, AOffset: Integer);
begin
  Name := AName;
  Timestep := ATimestep;
  ItemIndex := AItemIndex;
  Offset := AOffset;
end;

procedure TFrmAggregationDialog.btnOKClick(Sender: TObject);
begin
  with FAggrOptions do
  begin
    CalcMissingSeries := chkOutputMissingCountSeries.Checked;
    DeleteNullEnds := chkDeleteNullValueEnds.Checked;
    MissingAllowed := StrToInt(edtMissingValuesAllowed.Text);
    MissingFlag := cmbFlagSelection.Text;
  end;
end;

procedure TFrmAggregationDialog.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FHYearOrigin := 9;
  FSourceSeries := nil;
  FDestSeries := nil;
  cmbFromMonth.Clear;
  cmbToMonth.Clear;
  for i := 1 to 12 do
  begin
    cmbFromMonth.Items.Add(FormatSettings.LongMonthNames[i]);
    cmbToMonth.Items.Add(FormatSettings.LongMonthNames[i]);
  end;
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

procedure TFrmAggregationDialog.FormShow(Sender: TObject);

  procedure FirstSelection;
  begin
    if SourceSeries.TimeStep<tstHourly then
      DestSeries.TimeStep := tstHourly
    else if SourceSeries.TimeStep<tstDaily then
      DestSeries.TimeStep := tstDaily
    else if SourceSeries.TimeStep<tstMonthly then
      DestSeries.TimeStep := tstMonthly
    else if SourceSeries.TimeStep<tstAnnual then
      DestSeries.TimeStep := tstAnnual
    else if SourceSeries.TimeStep<tstTenYear then
      DestSeries.TimeStep := tstTenYear
    else
      DestSeries.TimeStep := TTimeStep.Create(0, OtherStepMonths);
    DestSeries.NominalOffset := TDateOffset.Create(0, 0);
  end;

  procedure FindSelection;
  var
    i: Integer;
  begin
    for i := 0 to Length(ButtonsItems)-1 do
      if ButtonsItems[i].Timestep = DestSeries.TimeStep then
      begin
        lstAggregationTimestep.ItemIndex := ButtonsItems[i].ItemIndex;
        Break;
      end;
  end;

  procedure SetVariableTypeRadio;
  begin
    with rgrpVariableType do
      case DestSeries.VariableType of
        vtCumulative: ItemIndex := 0;
        vtAverage: ItemIndex := 1;
        vtMaximum: ItemIndex := 2;
        vtMinimum: ItemIndex := 3;
        vtVectorAverage: ItemIndex := 4;
        vtInstantaneous: ItemIndex := 5;
      else
        Assert(False);
      end;
  end;

var
  i: Integer;
begin
  OtherStepMonths := Max(OtherStepMonths,
    Max(SourceSeries.TimeStep.LengthMonths, 12));
  spinOtherTimeStep.Value := OtherStepMonths div 12;
  ButtonsItems[31].Timestep := TTimeStep.Create(0, OtherStepMonths);
  lstAggregationTimestep.Clear;
  cmbFlagSelection.Clear;
  cmbFlagSelection.Items.Text := SourceSeries.SelectionFlags;
  ButtonsItems[27].Offset := FHYearOrigin-1;
  with FAggrOptions do
  begin
    edtMissingValuesAllowed.Text := IntToStr(MissingAllowed);
    chkOutputMissingCountSeries.Checked := CalcMissingSeries;
    chkDeleteNullValueEnds.Checked := DeleteNullEnds;
    cmbFlagSelection.Text := MissingFlag;
  end;
  for i := 0 to Length(ButtonsItems)-1 do
    with ButtonsItems[i] do
      if Timestep>=SourceSeries.TimeStep then
        ItemIndex := lstAggregationTimestep.Items.Add(Name)
      else
        ItemIndex := -1;
  FirstSelection;
  FindSelection;
  InitDestSeries;
  SetVariableTypeRadio;
  if lstAggregationTimestep.ItemIndex=ButtonsItems[31].ItemIndex then
  begin
    spinOtherTimeStep.Visible := True;
    lblOtherTimestep.Visible := True;
  end;
  SetControlStatus;
  chkFineTunning.Checked := False;
  EnableControlAndChilds(grpActualOffset, False);
  EnableControlAndChilds(grpNominalOffset, False);
end;

procedure TFrmAggregationDialog.InitDestSeries;
var
  AActualMinutes, AActualMonths: Integer;
begin
  AActualMinutes := 0;
  with DestSeries do
  begin
    if VariableType = vtInstantaneous then
      AActualMonths := 0 else
      AActualMonths := TimeStep.LengthMonths;
    if (SourceSeries.TimeStep >= tstDaily) and
      (TimeStep>=tstMonthly) then
    begin
      AActualMinutes := SourceSeries.NominalOffset.Minutes+
        SourceSeries.ActualOffset.Minutes;
      while AActualMinutes>=1440 do
        AActualMinutes := AActualMinutes-1440;
    end;
    ActualOffset := TDateOffset.Create(AActualMinutes, AActualMonths);
    with FActualOffsetBounds do
    begin
      MonthsMin := 0;
      MonthsMax := TimeStep.LengthMonths;
      if TimeStep>=tstMonthly then
      begin
        MinutesMin := -1440;
        MinutesMax := 1440;
      end else begin
        MinutesMin := 0;
        MinutesMax := TimeStep.LengthMinutes;
      end;
    end;
    with FNominalOffsetBounds do
    begin
      MinutesMin := 0;
      MonthsMin := 0;
      MinutesMax := Max(0, TimeStep.LengthMinutes-1);
      MonthsMax := Max(0, TimeStep.LengthMonths-1);
    end;
  end;
end;

procedure TFrmAggregationDialog.lstAggregationTimestepClick(Sender: TObject);
var
  i: Integer;
begin
  lblOtherTimestep.Visible := False;
  spinOtherTimeStep.Visible := False;
  for i := 0 to Length(ButtonsItems)-1 do
    if ButtonsItems[i].ItemIndex = lstAggregationTimestep.ItemIndex then
    begin
      DestSeries.NominalOffset :=
        TDateOffset.Create(0, ButtonsItems[i].Offset);
      DestSeries.TimeStep := ButtonsItems[i].Timestep;
      if i = Length(ButtonsItems)-1 then
      begin
        lblOtherTimestep.Visible := True;
        spinOtherTimeStep.Visible := True;
      end;
      Break;
    end;
  InitDestSeries;
  chkSeasonalAggregation.Checked := False;
  SetControlStatus;
end;

procedure TFrmAggregationDialog.rgrpVariableTypeClick(Sender: TObject);
begin
  with DestSeries do
    case rgrpVariableType.ItemIndex of
      0: VariableType := vtCumulative;
      1: VariableType := vtAverage;
      2: VariableType := vtMaximum;
      3: VariableType := vtMinimum;
      4: VariableType := vtVectorAverage;
      5: VariableType := vtInstantaneous;
    else
      Assert(False);
    end;
  if rgrpVariableType.ItemIndex=5 then
    spinActualOffsetMonths.Value := 0
  else
    spinActualOffsetMonths.Value := DestSeries.TimeStep.LengthMonths;
end;

procedure TFrmAggregationDialog.IncDecActualOffsetMin(IncBy: Integer);
var
  AMin, ASign: Integer;
begin
  AMin := DestSeries.ActualOffset.Minutes+IncBy;
  if AMin<0 then ASign := -1 else ASign := 1;
  AMin := (Abs(AMin) div IncBy)*IncBy*ASign;
  AMin := Max(AMin, FActualOffsetBounds.MinutesMin);
  AMin := Min(AMin, FActualOffsetBounds.MinutesMax);
  DestSeries.ActualOffset :=
    TDateOffset.Create(AMin, DestSeries.ActualOffset.Months);
end;

procedure TFrmAggregationDialog.IncDecNominalOffsetMin(IncBy: Integer);
var
  AMin, ASign: Integer;
begin
  AMin := DestSeries.NominalOffset.Minutes+IncBy;
  if AMin<0 then ASign := -1 else ASign := 1;
  AMin := (Abs(AMin) div IncBy)*IncBy*ASign;
  AMin := Max(AMin, FNominalOffsetBounds.MinutesMin);
  AMin := Min(AMin, FNominalOffsetBounds.MinutesMax);
  DestSeries.NominalOffset :=
    TDateOffset.Create(AMin, DestSeries.NominalOffset.Months);
end;

procedure TFrmAggregationDialog.spinActualOffsetMinutesHoursDownClick(
  Sender: TObject);
begin
  IncDecActualOffsetMin(-60);
  SetControlStatus;
end;

procedure TFrmAggregationDialog.spinActualOffsetMinutesHoursUpClick(
  Sender: TObject);
begin
  IncDecActualOffsetMin(60);
  SetControlStatus;
end;

procedure TFrmAggregationDialog.spinActualOffsetMinutesMinsDownClick(Sender:
  TObject);
begin
  IncDecActualOffsetMin(-1);
  SetControlStatus;
end;

procedure TFrmAggregationDialog.spinActualOffsetMinutesMinsUpClick(Sender:
  TObject);
begin
  IncDecActualOffsetMin(1);
  SetControlStatus;
end;

procedure TFrmAggregationDialog.spinActualOffsetMonthsChange(Sender: TObject);
begin
  with Sender as TSpinEdit do
  begin
    Value := Min(FActualOffsetBounds.MonthsMax, Value);
    Value := Max(FActualOffsetBounds.MonthsMin, Value);
    DestSeries.ActualOffset :=
      TDateOffset.Create(DestSeries.ActualOffset.Minutes, Value);
  end;
  SetControlStatus;
end;

procedure TFrmAggregationDialog.spinNominallOffsetMinutesHoursDownClick(
  Sender: TObject);
begin
  IncDecNominalOffsetMin(-60);
  SetControlStatus;
end;

procedure TFrmAggregationDialog.spinNominallOffsetMinutesHoursUpClick(
  Sender: TObject);
begin
  IncDecNominalOffsetMin(60);
  SetControlStatus;
end;

procedure TFrmAggregationDialog.spinNominalOffsetMinutesMinsDownClick(
  Sender: TObject);
begin
  IncDecNominalOffsetMin(-1);
  SetControlStatus;
end;

procedure TFrmAggregationDialog.spinNominalOffsetMinutesMinsUpClick(
  Sender: TObject);
begin
  IncDecNominalOffsetMin(1);
  SetControlStatus;
end;

procedure TFrmAggregationDialog.spinNominalOffsetMonthsChange(Sender: TObject);
begin
  with Sender as TSpinEdit do
  begin
    Value := Min(FNominalOffsetBounds.MonthsMax, Value);
    Value := Max(FNominalOffsetBounds.MonthsMin, Value);
    DestSeries.NominalOffset :=
      TDateOffset.Create(DestSeries.NominalOffset.Minutes, Value);
  end;
  SetControlStatus;
end;

procedure TFrmAggregationDialog.spinOtherTimeStepChange(Sender: TObject);
begin
  with Sender as TSpinEdit do
  begin
    if not Visible then Exit;
    Value := Max(Value, Max(1, SourceSeries.TimeStep.LengthMonths div 12));
    ButtonsItems[31].Timestep := TTimeStep.Create(0, Value*12);
  end;
  DestSeries.TimeStep := ButtonsItems[31].Timestep;
  InitDestSeries;
  SetControlStatus;
end;

resourcestring
  rsAggregationByAddition = 'All aggregated intervals are intact, doing '+
    'simple aggregation';
  rsAggregationByInterpolation = 'Some aggregated intervals are cut and '+
    'some values will be interpolated';

procedure TFrmAggregationDialog.chkFineTunningClick(Sender: TObject);
begin
  with Sender as TCheckBox do
  begin
    EnableControlAndChilds(grpActualOffset, Checked);
    EnableControlAndChilds(grpNominalOffset, Checked);
  end;
end;

procedure TFrmAggregationDialog.chkSeasonalAggregationClick(Sender: TObject);
begin
  with Sender as TCheckBox do
  begin
    if not Enabled then Exit;
    FAggrOptions.SeasonalAggregation := Checked;
    lblAggregationTimestep.Visible := not Checked;
    lstAggregationTimestep.Visible := not Checked;
    grpSeasonalAggregation.Visible := Checked;
    with FAggrOptions do
    begin
      cmbFromMonth.ItemIndex := DestSeries.NominalOffset.Months;
      cmbToMonth.ItemIndex := cmbFromMonth.ItemIndex-1;
      if cmbToMonth.ItemIndex <0 then cmbToMonth.ItemIndex := 11;
      cmbFromDay.ItemIndex := 0;
      cmbToDay.ItemIndex := 32;
    end;
    spinActualOffsetMonths.Value := DestSeries.TimeStep.LengthMonths;
    if Checked then
      FActualOffsetBounds.MonthsMin := DestSeries.TimeStep.LengthMonths else
      FActualOffsetBounds.MonthsMin := 0;
  end;
end;

procedure TFrmAggregationDialog.cmbFromDayChange(Sender: TObject);
begin
  with FAggrOptions do
  begin
    FromMonth := cmbFromMonth.ItemIndex+1;
    ToMonth := cmbToMonth.ItemIndex+1;
    FromDay := cmbFromDay.ItemIndex;
    ToDay := cmbToDay.ItemIndex;
    if FromDay<>32 then FromDay := Min(FromDay, MonthDays[False][FromMonth]);
    if ToDay<>32 then ToDay := Min(ToDay, MonthDays[False][ToMonth]);
    cmbFromDay.ItemIndex := FromDay;
    cmbToDay.ItemIndex := ToDay;
  end;
  SetControlStatus;
end;

procedure TFrmAggregationDialog.DrawTimestamps;
var
  ADestTimestamp1,  ADestTimestamp2,  ADestTimestamp3: TDateTime;
  ADestInterval1, ADestInterval2: TDateTime;
  AStartNominal, AEndNominal: TDateTime;
  ASourceStartInterval1,ASourceStartInterval2, ASourceEndInterval1,
    ASourceEndInterval2: TDateTime;
  AUsedInterval1, AUnusedInterval1, AOut1 : TDateTime;
  AUsedInterval2, AUnusedInterval2, AOut2 : TDateTime;
  APctUsed1, APctUsed2: Real;
  DestTimestampsFormat,SourceTimestampsFormat, IntervalsFormat: string;

    procedure AssessDestInterval;
    var
      AYear: Integer;
      FromMonth2, ToMonth2, FromDay2, ToDay2: Integer;
    begin
      with FAggrOptions do
        if not SeasonalAggregation then
        begin
          ADestInterval1 := DestSeries.IntervalStartPoint(ADestTimestamp2);
          ADestInterval2 := DestSeries.IntervalEndPoint(ADestTimestamp2);
        end else
        begin
          FromMonth2 := FromMonth;
          ToMonth2 := ToMonth;
          FromDay2 := FromDay;
          ToDay2 := ToDay;
          if FromDay2=0 then FromDay2 := 1;
          if FromDay2=32 then begin Inc(FromMonth2); FromDay2 := 1; end;
          if ToDay2=0 then ToDay2 := 1;
          if ToDay2=32 then begin Inc(ToMonth2); ToDay2 := 1; end;
          if FromMonth2>12 then FromMonth2 := 1;
          if ToMonth2>12 then ToMonth2 := 1;
          AYear := YearOf(ADestTimestamp2);
          if FromMonth2<DestSeries.NominalOffset.Months+1 then
            Inc(AYear);
          ADestInterval1 := EncodeDate(AYear, FromMonth2, FromDay2);
          if ToMonth2<=FromMonth2 then
            Inc(AYear);
          ADestInterval2 := EncodeDate(AYear, ToMonth2, ToDay2);
          ADestInterval1 := AddDateTime(ADestInterval1,
            DestSeries.ActualOffset.Minutes/1440);
          ADestInterval2 := AddDateTime(ADestInterval2,
            DestSeries.ActualOffset.Minutes/1440);
        end;
    end;

begin
  ADestTimestamp1 := DestSeries.PreviousTimestamp(SourceSeries.First.Date);
  ADestTimestamp1 := DestSeries.TimeStep.IncStep(ADestTimestamp1);
  ADestTimestamp2 := DestSeries.TimeStep.IncStep(ADestTimestamp1);
  ADestTimestamp3 := DestSeries.TimeStep.IncStep(ADestTimestamp2);
  AssessDestInterval;
//  ADestInterval1 := DestSeries.IntervalStartPoint(ADestTimestamp2);
//  ADestInterval2 := DestSeries.IntervalEndPoint(ADestTimestamp2);
  AStartNominal := SourceSeries.ContainingInterval(ADestInterval1);
  AEndNominal := SourceSeries.ContainingInterval(ADestInterval2);
  ASourceStartInterval1 := SourceSeries.IntervalStartPoint(AStartNominal);
  ASourceEndInterval1 := SourceSeries.IntervalEndPoint(AStartNominal);
  ASourceStartInterval2 := SourceSeries.IntervalStartPoint(AEndNominal);
  ASourceEndInterval2 := SourceSeries.IntervalEndPoint(AEndNominal);
  AUsedInterval1 := SubtractDateTime(ASourceEndInterval1,
    ASourceStartInterval1);
  AUnusedInterval1 := 0;
  AUsedInterval2 := SubtractDateTime(ASourceEndInterval2,
    ASourceStartInterval2);
  AUnusedInterval2 := 0;
  if DiffInSecs(ASourceStartInterval1, ADestInterval1)<0 then
  begin
    AOut1 := SubtractDateTime(ADestInterval1, ASourceStartInterval1);
    AUnusedInterval1 := AddDateTime(AUnusedInterval1, AOut1);
    AUsedInterval1 := AddDateTime(AUsedInterval1, -AOut1);
  end;
  if DiffInSecs(ASourceEndInterval1, ADestInterval2)>0 then
  begin
    AOut1 := SubtractDateTime(ASourceEndInterval1, ADestInterval2);
    AUnusedInterval1 := AddDateTime(AUnusedInterval1, AOut1);
    AUsedInterval1 := AddDateTime(AUsedInterval1, -AOut1);
  end;
  APctUsed1 := AUsedInterval1/ AddDateTime(AUsedInterval1, AUnusedInterval1);
  if DiffInSecs(ASourceStartInterval2, ADestInterval1)<0 then
  begin
    AOut2 := SubtractDateTime(ADestInterval1, ASourceStartInterval2);
    AUnusedInterval2 := AddDateTime(AUnusedInterval2, AOut2);
    AUsedInterval2 := AddDateTime(AUsedInterval2, -AOut2);
  end;
  if DiffInSecs(ASourceEndInterval2, ADestInterval2)>0 then
  begin
    AOut2 := SubtractDateTime(ASourceEndInterval2, ADestInterval2);
    AUnusedInterval2 := AddDateTime(AUnusedInterval2, AOut2);
    AUsedInterval2 := AddDateTime(AUsedInterval2, -AOut2);
  end;
  APctUsed2 := AUsedInterval2/ AddDateTime(AUsedInterval2, AUnusedInterval2);
  IntervalsFormat := 'yyyy/mm/dd hh:nn';
  DestTimestampsFormat := IntervalsFormat;
  SourceTimestampsFormat := IntervalsFormat;
  if SourceSeries.TimeStep>=tstMonthly then SourceTimestampsFormat := 'yyyy/mm';
  if DestSeries.TimeStep>=tstMonthly then DestTimestampsFormat := 'yyyy/mm';
  lblDestTimestamp1.Caption :=
    FormatDateTime(DestTimestampsFormat, ADestTimestamp1);
  lblDestTimestamp2.Caption :=
    FormatDateTime(DestTimestampsFormat, ADestTimestamp2);
  lblDestTimestamp3.Caption :=
    FormatDateTime(DestTimestampsFormat, ADestTimestamp3);
  lblDestInterval1.Caption :=
    FormatDateTime(IntervalsFormat, ADestInterval1);
  lblDestInterval2.Caption :=
    FormatDateTime(IntervalsFormat, ADestInterval2);
  lblSourceTimestamp1.Caption :=
    FormatDateTime(SourceTimestampsFormat, AStartNominal);
  lblSourceTimestamp4.Caption :=
    FormatDateTime(SourceTimestampsFormat, AEndNominal);
  lblSourceInterval1.Caption :=
    FormatDateTime(IntervalsFormat, ASourceStartInterval1);
  lblSourceInterval2.Caption :=
    FormatDateTime(IntervalsFormat, ASourceEndInterval1);
  lblSourceInterval3.Caption :=
    FormatDateTime(IntervalsFormat, ASourceStartInterval2);
  lblSourceInterval4.Caption :=
    FormatDateTime(IntervalsFormat, ASourceEndInterval2);
  Line1.Top := Image.Top +
    ((117-Round(40*APctUsed1))*Image.Height) div 303;
  Line2.Top := Line1.Top;
  Line4.Top := Image.Top +
    ((231+Round(40*APctUsed2))*Image.Height) div 303;
  Line3.Top := Line4.Top - Line3.Height + 1;
  if (((Abs(APctUsed1)<1e-35) or (Abs(APctUsed1-1)<1e-35))) and
    (((Abs(APctUsed2)<1e-35) or (Abs(APctUsed2-1)<1e-35))) then
  begin
    lblAggregationMode.Font.Color := clBlue;
    lblAggregationMode.Caption := rsAggregationByAddition;
  end else
  begin
    lblAggregationMode.Font.Color := clPurple;
    lblAggregationMode.Caption := rsAggregationByInterpolation;
  end;
end;

procedure TFrmAggregationDialog.SetControlStatus;
var
  s: string;
begin
  s := '';
  if DestSeries.ActualOffset.Minutes<0 then s := '-';
  s := s+FormatFloat('00',Abs(DestSeries.ActualOffset.Minutes div 60))+':';
  s := s+FormatFloat('00',Abs(DestSeries.ActualOffset.Minutes) mod 60);
  edtActualOffsetMinutes.Text := s;
  spinActualOffsetMonths.Value := DestSeries.ActualOffset.Months;
  s := '';
  s := s+FormatFloat('00',DestSeries.NominalOffset.Minutes div 60)+':';
  s := s+FormatFloat('00',Abs(DestSeries.NominalOffset.Minutes) mod 60);
  edtNominalOffsetMinutes.Text := s;
  spinNominalOffsetMonths.Value := DestSeries.NominalOffset.Months;
  spinOtherTimeStep.Value := ButtonsItems[31].Timestep.LengthMonths div 12;
  DrawTimestamps;
  chkSeasonalAggregation.Enabled := DestSeries.TimeStep = tstAnnual;
end;

resourcestring
  rsOneMinute = 'One minute';
  rsTwoMinute = 'Two minute';
  rsThreeMinute = 'Three minute';
  rsFourMinute = 'Four minute';
  rsFiveMinute = 'Five minute';
  rsSixMinute = 'Six minute';
  rsTenMinute = 'Ten minute';
  rsTwelveMinute = 'Twelve minute';
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
  rsHydrologicalJuly = 'Hydrological year starting July';
  rsHydrologicalOther = 'Hydrological year (other)';
  rsTwoYear = 'Two year';
  rsFiveYear = 'Five year';
  rsTenYear = 'Decade (ten year)';
  rsUnknown = 'Other (years)';

initialization
  OtherStepMonths := 144;
  ButtonsItems[0] := TTStepItem.Create(rsOneMinute, TTimeStep.Create(1,0),-1,0);
  ButtonsItems[1] := TTStepItem.Create(rsTwoMinute, TTimeStep.Create(2,0),-1,0);
  ButtonsItems[2] := TTStepItem.Create(rsThreeMinute, TTimeStep.Create(3,0),-1,
    0);
  ButtonsItems[3] := TTStepItem.Create(rsFourMinute, TTimeStep.Create(4,0),-1,
    0);
  ButtonsItems[4] := TTStepItem.Create(rsFiveMinute, tstFiveMinute,-1,0);
  ButtonsItems[5] := TTStepItem.Create(rsSixMinute, TTimeStep.Create(6,0),-1,0);
  ButtonsItems[6] := TTStepItem.Create(rsTenMinute, tstTenMinute,-1,0);
  ButtonsItems[7] := TTStepItem.Create(rsTwelveMinute, TTimeStep.Create(12,0),
    -1,0);
  ButtonsItems[8] := TTStepItem.Create(rsFithteenMinute, tstFithteenMinute,
    -1,0);
  ButtonsItems[9] := TTStepItem.Create(rsTwentyMinute, tstTwentyMinute,-1,0);
  ButtonsItems[10] := TTStepItem.Create(rsHalfHour, tstHalfHour,-1,0);
  ButtonsItems[11] := TTStepItem.Create(rsHourly, tstHourly, -1,0);
  ButtonsItems[12] := TTStepItem.Create(rsTwoHour, tstTwoHour, -1,0);
  ButtonsItems[13] := TTStepItem.Create(rsThreeHour, tstThreeHour, -1,0);
  ButtonsItems[14] := TTStepItem.Create(rsFourHour, tstFourHour, -1,0);
  ButtonsItems[15] := TTStepItem.Create(rsSixHour, tstSixHour, -1,0);
  ButtonsItems[16] := TTStepItem.Create(rsEightHour, tstEightHour, -1,0);
  ButtonsItems[17] := TTStepItem.Create(rsTwelveHour, tstTwelveHour, -1,0);
  ButtonsItems[18] := TTStepItem.Create(rsDaily, tstDaily, -1,0);
  ButtonsItems[19] := TTStepItem.Create(rsMonthly, tstMonthly, -1,0);
  ButtonsItems[20] := TTStepItem.Create(rsTwoMonth, tstTwoMonth, -1,0);
  ButtonsItems[21] := TTStepItem.Create(rsThreeMonth, tstThreeMonth, -1,0);
  ButtonsItems[22] := TTStepItem.Create(rsFourMonth, tstFourMonth, -1,0);
  ButtonsItems[23] := TTStepItem.Create(rsSixMonth, tstSixMonth, -1,0);
  ButtonsItems[24] := TTStepItem.Create(rsAnnual, tstAnnual, -1,0);
  ButtonsItems[25] := TTStepItem.Create(rsHydrologicalOctober,tstAnnual, -1,9);
  ButtonsItems[26] := TTStepItem.Create(rsHydrologicalJuly, tstAnnual, -1,6);
  ButtonsItems[27] := TTStepItem.Create(rsHydrologicalOther, tstAnnual, -1,0);
  ButtonsItems[28] := TTStepItem.Create(rsTwoYear, tstTwoYear, -1,0);
  ButtonsItems[29] := TTStepItem.Create(rsFiveYear, tstFiveYear, -1,0);
  ButtonsItems[30] := TTStepItem.Create(rsTenYear, tstTenYear, -1,0);
  ButtonsItems[31] := TTStepItem.Create(rsUnknown,
    TTimeStep.Create(0,OtherStepMonths),-1,0);

end.
