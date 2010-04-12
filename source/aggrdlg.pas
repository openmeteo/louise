{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit AggrDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, iform;

type
  TFrmAggregation = class(TForm)
    RgrpMethod: TRadioGroup;
    EdtTimeOffset: TEdit;
    LblTimeOffset: TLabel;
    BtnOk: TButton;
    BtnCancel: TButton;
    ChkHydrologicalYear: TCheckBox;
    EdtMissingAllowed: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    CmbMissingFlag: TComboBox;
    chkSeasonal: TCheckBox;
    cmbFrom: TComboBox;
    cmbTo: TComboBox;
    lblFrom: TLabel;
    lblTo: TLabel;
    procedure ChkHydrologicalYearClick(Sender: TObject);
    procedure IFormShow(Sender: TObject);
  private
    procedure SetControlStatus;
  public

  end;

implementation

{$R *.DFM}

procedure TFrmAggregation.SetControlStatus;
var
  i: Integer;
begin
  cmbFrom.Enabled := chkSeasonal.Checked;
  lblFrom.Enabled := cmbFrom.Enabled;
  cmbTo.Enabled := chkSeasonal.Checked;
  lblTo.Enabled := cmbTo.Enabled;
  cmbFrom.Clear;
  cmbTo.Clear;
  if ChkHydrologicalYear.Checked then
  begin
    for i := 10 to 12 do
    begin
      cmbFrom.Items.Add(LongMonthNames[i]);
      cmbTo.Items.Add(LongMonthNames[i]);
    end;
    for i := 1 to 9 do
    begin
      cmbFrom.Items.Add(LongMonthNames[i]);
      cmbTo.Items.Add(LongMonthNames[i]);
    end;
  end else
  begin
    for i := 1 to 12 do
    begin
      cmbFrom.Items.Add(LongMonthNames[i]);
      cmbTo.Items.Add(LongMonthNames[i]);
    end;
  end;
  cmbFrom.ItemIndex := 0;
  cmbTo.ItemIndex := cmbTo.Items.Count-1;
end;

procedure TFrmAggregation.ChkHydrologicalYearClick(Sender: TObject);
begin
  SetControlStatus;
end;


procedure TFrmAggregation.IFormShow(Sender: TObject);
begin
  SetControlStatus;
end;

end.
