{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit RngChkDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFrmRangeCheck = class(TForm)
    lblLowLimit: TLabel;
    lblHighLimit: TLabel;
    EdtLowLimit: TEdit;
    EdtHighLimit: TEdit;
    BtnOk: TButton;
    BtnCancel: TButton;
    lblMarkOut: TLabel;
    CmbRangeFlag: TComboBox;
    chkAutoLow: TCheckBox;
    chkAutoHigh: TCheckBox;
    edtProbabilityLevel: TEdit;
    lblProbabilityLevel: TLabel;
    procedure chkAutoLowClick(Sender: TObject);
    procedure IFormShow(Sender: TObject);
  private
    procedure SetControlStatus;
  end;

implementation

{$R *.DFM}

procedure TFrmRangeCheck.SetControlStatus;
begin
  EdtLowLimit.Enabled := not chkAutoLow.Checked;
  EdtHighLimit.Enabled := not chkAutoHigh.Checked;
  lblProbabilityLevel.Enabled := chkAutoLow.Checked or chkAutoHigh.Checked;
  edtProbabilityLevel.Enabled := lblProbabilityLevel.Enabled;
end;

procedure TFrmRangeCheck.chkAutoLowClick(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmRangeCheck.IFormShow(Sender: TObject);
begin
  SetControlStatus;
end;

end.
