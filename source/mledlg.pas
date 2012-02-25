{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit mledlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, statprocesses;

type
  TArrayOfReal2 = array[0..1] of Real;

type
  TFrmMLEDialog = class(TForm)
    Label1: TLabel;
    edtDistributionName: TEdit;
    GroupBox1: TGroupBox;
    lblLMomentsParam1: TLabel;
    edtMomentsParam1: TEdit;
    lblLMomentsParam2: TLabel;
    edtMomentsParam2: TEdit;
    lblLMomentsParam3: TLabel;
    edtMomentsParam3: TEdit;
    GroupBox2: TGroupBox;
    lblOptimParam1: TLabel;
    lblOptimParam2: TLabel;
    lblOptimParam3: TLabel;
    edtOptimizationParam1: TEdit;
    edtOptimizationParam2: TEdit;
    edtOptimizationParam3: TEdit;
    GroupBox3: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    edtSampleMin: TEdit;
    edtSampleMax: TEdit;
    GroupBox4: TGroupBox;
    lblLimitsParam1: TLabel;
    lblLimitsParam2: TLabel;
    lblLimitsParam3: TLabel;
    edtParam1Min: TEdit;
    edtParam2Min: TEdit;
    edtParam3Min: TEdit;
    edtParam1Max: TEdit;
    edtParam2Max: TEdit;
    edtParam3Max: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    btnCalculate: TButton;
    btnCancel: TButton;
    GroupBox5: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    edtOptMin: TEdit;
    edtOptMax: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edtParam1MinChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FParamNames: array[0..2] of string;
    FMomentValues: array[0..2] of Real;
    FParamCount: Integer;
    FSampleMin, FSampleMax: Real;
    FDistributionName: string;
    FDistributionType: TStatisticalDistributionType;
    FMomComponents: array[0..2] of TEdit;
    FMinComponents: array[0..2] of TEdit;
    FMaxComponents: array[0..2] of TEdit;
    FIniComponents: array[0..2] of TEdit;
    procedure SetParamNames(Index: Integer; Value: string);
    procedure SetParamCount(Value: Integer);
    procedure SetMomentValues(Index: Integer; Value: Real);
    procedure SetSampleMin(Value: Real);
    procedure SetSampleMax(Value: Real);
    procedure SetDistributionName(Value: string);
    function GetParamInivalues(Index: Integer): Real;
    function GetParamMinvalues(Index: Integer): Real;
    function GetParamMaxvalues(Index: Integer): Real;
    function GetOptimMin(p1, p2, p3: TArrayOfReal2): Real;
    function GetOptimMax(p1, p2, p3: TArrayOfReal2): Real;
    procedure CalcOptimLimits(var amin, amax: Real);
  public
    property DistributionType: TStatisticalDistributionType
      read FDistributionType write FDistributionType;
    property ParamCount: Integer read FParamCount write SetParamCount;
    property ParamNames[Index: Integer]: string write SetParamNames;
    property MomentValues[Index: Integer]: Real write SetMomentValues;
    property SampleMin: Real write SetSampleMin;
    property SampleMax: Real write SetSampleMax;
    property DistributionName: string write SetDistributionName;
    property ParamInivalues[Index: Integer]: Real read GetParamInivalues;
    property ParamMinvalues[Index: Integer]: Real read GetParamMinvalues;
    property ParamMaxvalues[Index: Integer]: Real read GetParamMaxvalues;
  end;

implementation

{$R *.dfm}

const
  inf=1e34;
  err=-1.23456e38;
  ff = '0.0000';

procedure TFrmMLEDialog.SetParamCount(Value: Integer);
var
  i: Integer;
begin
  Assert(Value in [2,3]);
  FParamCount := Value;
  if Value=2 then
  begin
    for i := 0 to ComponentCount - 1 do
      if (Components[i] is TLabel) or (Components[i] is TEdit) then
        if Components[i].Tag=3 then
          (Components[i] as TControl).Visible := False;
  end;
end;

procedure TFrmMLEDialog.SetParamNames(Index: Integer; Value: string);
var
  i: Integer;
begin
  Assert((Index>=0) and (Index<=2));
  FParamNames[Index] := Value;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TLabel then
      if Components[i].Tag = Index+1 then
        (Components[i] as TLabel).Caption := FParamNames[Index]
end;

procedure TFrmMLEDialog.edtParam1MinChange(Sender: TObject);
var
  AMin, AMax: Real;
  sMin, sMax: string;
begin
  CalcOptimLimits(AMin, AMax);
  if Abs(AMin-err)<0.00001e38 then
    sMin := 'Error'
  else if Abs(AMin+inf)<0.00001e34 then
    sMin := ''
  else
    sMin := FormatFloat(ff, AMin);
  if Abs(AMax-err)<0.00001e38 then
    sMax := 'Error'
  else if Abs(AMax-inf)<0.00001e34 then
    sMax := ''
  else
    sMax := FormatFloat(ff, AMax);
  edtOptMin.Text := sMin;
  edtOptMax.Text := sMax;
  if FSampleMin<=AMin then
    edtOptMin.Font.Color := clRed
  else
    edtOptMin.Font.Color := clBlack;
  if FSampleMax>=AMax then
    edtOptMax.Font.Color := clRed
  else
    edtOptMax.Font.Color := clBlack;
end;

resourcestring
  rsMinError = 'Minimum bound set incorectly to a value greater than the '+
               'Maximum bound; please correct.';
  rsInitParamNotInBounds = 'Initialization value not in parameter bounds; '+
                           'please enter a value between min and max';
  rsProblemOptim = 'Optimization limits specified have some problems:';
  rsMinLessThanMin = '- Sample Minimum is less or equal than expected minima '+
                     'according to the optimization limits of parameters.';
  rsMaxGreatThanMax = '- Sample Maximum is greater or equal than expected maxima '+
                     'according to the optimization limits of parameters.';
  rsExplanation = 'With these limits, some of the solutions for the parameters, '+
                  'as well as the calculated optimal solution, may discard some '+
                  'of the sample values. In this case the calculated parameters '+
                  'will not take account the discarded values and this is could be '+
                  'a serious error.'#13#10'Press Yes to continue in any way, No to ' +
                  'return and to correct the limits, or, Cancel to close the window. '+
                  #13#10#13#10'[Note than in some distributions an absolute limit for the '+
                  'random value exists whatever the parameters are, e.g. for the Gamma ' +
                  'CDF, random value should be always greater than zero (0).]';

procedure TFrmMLEDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: Integer;
  AMin, AMax: Real;
  s: string;
begin
  if Self.ModalResult=mrCancel then
    Exit;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TEdit then
      with (Components[i] as TEdit) do
        if not ReadOnly then
          try
            StrToFloat(Text);
          except
            SetFocus;
            raise;
          end;
  for i := 0 to 2 do
  begin
    if StrToFloat(FMinComponents[i].Text)>StrToFloat(FMaxComponents[i].Text) then
    begin
      FMinComponents[i].SetFocus;
      raise Exception.Create(rsMinError);
    end;
    if (StrToFloat(FMinComponents[i].Text)> StrToFloat(FIniComponents[i].Text)) or
      (StrToFloat(FMaxComponents[i].Text)< StrToFloat(FIniComponents[i].Text)) then
    begin
      FIniComponents[i].SetFocus;
      raise Exception.Create(rsInitParamNotInBounds);
    end;
  end;
  CalcOptimLimits(AMin, AMax);
  if (FSampleMin<=AMin) or (FSampleMax>=AMax) then
  begin
    s := rsProblemOptim;
    if FSampleMin<=AMin then
      s := s+#13#10+rsMinLessThanMin;
    if FSampleMax>=AMax then
      s := s+#13#10+rsMaxGreatThanMax;
    s := s+#13#10+#13#10+rsExplanation;
    case MessageDlg(s, mtWarning, mbYesNoCancel, 0, mbNo) of
      mrNo:
        CanClose := False;
      mrCancel:
        Self.ModalResult := mrCancel;
    end;
  end;
end;

procedure TFrmMLEDialog.FormCreate(Sender: TObject);
begin
  FMinComponents[0] := edtParam1Min;
  FMinComponents[1] := edtParam2Min;
  FMinComponents[2] := edtParam3Min;
  FMaxComponents[0] := edtParam1Max;
  FMaxComponents[1] := edtParam2Max;
  FMaxComponents[2] := edtParam3Max;
  FIniComponents[0] := edtOptimizationParam1;
  FIniComponents[1] := edtOptimizationParam2;
  FIniComponents[2] := edtOptimizationParam3;
  FMomComponents[0] := edtMomentsParam1;
  FMomComponents[1] := edtMomentsParam2;
  FMomComponents[2] := edtMomentsParam3;
end;

procedure TFrmMLEDialog.FormShow(Sender: TObject);
begin
  edtParam1MinChange(Sender);
end;

procedure TFrmMLEDialog.SetMomentValues(Index: Integer; Value: Real);
begin
  Assert((Index>=0) and (Index<=2));
  FMomentValues[Index] := Value;
  FMomComponents[Index].Text := FormatFloat(ff, Value);
  FIniComponents[Index].Text := FormatFloat(ff, Value);
  if Value>0 then
  begin
    FMinComponents[Index].Text := FormatFloat(ff, Value*0.5);
    FMaxComponents[Index].Text := FormatFloat(ff, Value*2);
  end else begin
    FMinComponents[Index].Text := FormatFloat(ff, Value*2);
    FMaxComponents[Index].Text := FormatFloat(ff, Value*0.5);
  end;
end;

procedure TFrmMLEDialog.SetSampleMin(Value: Real);
begin
  FSampleMin := Value;
  edtSampleMin.Text := FormatFloat(ff, Value);
end;

procedure TFrmMLEDialog.SetSampleMax(Value: Real);
begin
  FSampleMax := Value;
  edtSampleMax.Text := FormatFloat(ff, Value);
end;

procedure TFrmMLEDialog.SetDistributionName(Value: string);
begin
  FDistributionName := Value;
  edtDistributionName.Text := Value;
end;

function TFrmMLEDialog.GetParamInivalues(Index: Integer): Real;
begin
  Assert((Index>=0) and (Index<=2));
  Result := StrToFloat(FIniComponents[Index].Text);
end;

function TFrmMLEDialog.GetParamMinvalues(Index: Integer): Real;
begin
  Assert((Index>=0) and (Index<=2));
  Result := StrToFloat(FMinComponents[Index].Text);
end;

function TFrmMLEDialog.GetParamMaxvalues(Index: Integer): Real;
begin
  Assert((Index>=0) and (Index<=2));
  Result := StrToFloat(FMaxComponents[Index].Text);
end;

function TFrmMLEDialog.GetOptimMin(p1, p2, p3: TArrayOfReal2): Real;
var
  ADistribution: TStatisticalDistribution;
  i, j, k: Integer;
  AMin: Real;
begin
  ADistribution := TStatisticalDistribution.Create(FDistributionType, nil, 0);
  try
    Result := -inf;
    for i := 0 to 1 do
      for j := 0 to 1 do
        for k := 0 to 1 do
        begin
          AMin := ADistribution.GetMinXAtP(p1[i], p2[j], p3[k]);
          if AMin>Result then
            Result := AMin;
        end;
  finally
    ADistribution.Free;
  end;
end;

function TFrmMLEDialog.GetOptimMax(p1, p2, p3: TArrayOfReal2): Real;
var
  ADistribution: TStatisticalDistribution;
  i, j, k: Integer;
  AMax: Real;
begin
  ADistribution := TStatisticalDistribution.Create(FDistributionType, nil, 0);
  try
    Result := inf;
    for i := 0 to 1 do
      for j := 0 to 1 do
        for k := 0 to 1 do
        begin
          AMax := ADistribution.GetMaxXAtP(p1[i], p2[j], p3[k]);
          if AMax<Result then
            Result := AMax;
        end;
  finally
    ADistribution.Free;
  end;
end;

procedure TFrmMLEDialog.CalcOptimLimits(var amin, amax: Real);
var
  p1, p2, p3: TArrayOfReal2;
begin
  try
    p1[0] := StrToFloat(FMinComponents[0].Text);
    p2[0] := StrToFloat(FMinComponents[1].Text);
    p3[0] := StrToFloat(FMinComponents[2].Text);
    p1[1] := StrToFloat(FMaxComponents[0].Text);
    p2[1] := StrToFloat(FMaxComponents[1].Text);
    p3[1] := StrToFloat(FMaxComponents[2].Text);
  except
    on EConvertError do
    begin
      amin := err;
      amax := err;
      Exit;
    end;
    else
      raise;
  end;
  amin := GetOptimMin(p1, p2, p3);
  amax := GetOptimMax(p1, p2, p3);
end;

end.
