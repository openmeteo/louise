{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit tspropsdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ts;

type
  TFrmTimeseriesProperties = class(TForm)
    ChkHydrologicalYear: TCheckBox;
    CmbTimeStep: TComboBox;
    Label1: TLabel;
    ChkStrictTimeStep: TCheckBox;
    CmbVariableType: TComboBox;
    Label2: TLabel;
    EdtDateOffset: TEdit;
    LblDateOffset: TLabel;
    BtnOk: TButton;
    BtnCancel: TButton;
    MemComment: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    EdtTitle: TEdit;
    EdtMUnit: TEdit;
    Label5: TLabel;
    EdtPrecision: TEdit;
    Label6: TLabel;
    procedure SetControlStatus;
    procedure FormShow(Sender: TObject);
    procedure CmbTimeStepChange(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  TimeStepsSize = 10;
  VariableTypesSize = 7;

var
//  TimeSteps: array [0..TimeStepsSize-1] of Integer = (tstFiveMinute, tstTenMinute, tstHourly,
//    tstDaily, tstMonthly, tstAnnual, tstVariable, tstUnknown);
  VariableTypes: array[0..VariableTypesSize-1] of Integer =
    (vtUnknown, vtInstantaneous, vtCumulative, vtAverage, vtMaximum, vtMinimum,
      vtStdev);

  FrmTimeseriesProperties: TFrmTimeseriesProperties;

implementation

{$R *.DFM}

procedure TFrmTimeseriesProperties.SetControlStatus;
var
  TimeStep: TTimestep;
begin
  TimeStep := ts.AvailableTimesteps[CmbTimeStep.ItemIndex];
  ChkStrictTimeStep.Enabled := TimeStep.TimeStepIn([tstFiveMinute, tstTenMinute,
    tstHourly, tstDaily, tstTwoHour, tstTwoMonth]);
  ChkHydrologicalYear.Enabled := TimeStep = tstAnnual;
  EdtDateOffset.Enabled := TimeStep>=tstMonthly;
  LblDateOffset.Enabled := EdtDateOffset.Enabled;
end;

procedure TFrmTimeseriesProperties.FormShow(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmTimeseriesProperties.CmbTimeStepChange(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmTimeseriesProperties.BtnOkClick(Sender: TObject);
begin
  try
    StrToInt(Trim(EdtPrecision.Text));
    if Trim(EdtDateOffset.Text)<>'' then
      StrToInt(Trim(EdtDateOffset.Text));
  except
    ModalResult := mrNone;
    raise;
  end;
end;

end.
