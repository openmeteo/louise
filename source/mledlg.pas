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
  Dialogs, StdCtrls;

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
    procedure FormCreate(Sender: TObject);
  private
    FParamNames: array[0..2] of string;
    FMomentValues: array[0..2] of Real;
    FParamCount: Integer;
    FSampleMin, FSampleMax: Real;
    FDistributionName: string;
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
  public
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
          (Components[i] as TControl).Enabled := False;
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
      if Components[i].Tag = i+1 then
        (Components[i] as TLabel).Caption := FParamNames[Index]
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

procedure TFrmMLEDialog.SetMomentValues(Index: Integer; Value: Real);
begin
  Assert((Index>=0) and (Index<=2));
  FMomentValues[Index] := Value;
  FMomComponents[Index].Text := FormatFloat('0.0000', Value);
  FIniComponents[Index].Text := FormatFloat('0.0000', Value);
  if Value>0 then
  begin
    FMinComponents[Index].Text := FormatFloat('0.000', Value*0.2);
    FMaxComponents[Index].Text := FormatFloat('0.000', Value*5);
  end else begin
    FMinComponents[Index].Text := FormatFloat('0.000', Value*5);
    FMaxComponents[Index].Text := FormatFloat('0.000', Value*0.2);
  end;
end;

procedure TFrmMLEDialog.SetSampleMin(Value: Real);
begin
  FSampleMin := Value;
  edtSampleMin.Text := FormatFloat('0.000', Value);
end;

procedure TFrmMLEDialog.SetSampleMax(Value: Real);
begin
  FSampleMax := Value;
  edtSampleMax.Text := FormatFloat('0.000', Value);
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

end.
