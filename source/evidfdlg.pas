{******************************************************************}
{                                                                  }
{  Itia library                                                    }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Form, settings for IDF curves construction }
unit evidfdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFrmIDFEval = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    cmbMarginalFlags: TComboBox;
    cmbMissingFlags: TComboBox;
    edtMultiplier: TEdit;
    chkHydrologicalYear: TCheckBox;
    rgrpAnalysisTimeStep: TRadioGroup;
    chkAllowMissingValues: TCheckBox;
    Label1: TLabel;
    rgrpIDFVariable: TRadioGroup;
    Label2: TLabel;
    Label3: TLabel;
    chkCalculatetMissing: TCheckBox;
    chkDayNumber: TCheckBox;
    procedure rgrpAnalysisTimeStepClick(Sender: TObject);
    procedure IFormShow(Sender: TObject);
  private
    procedure SetControlStatus;
  public

  end;

implementation

{$R *.DFM}

procedure TFrmIDFEval.SetControlStatus;
begin
  case rgrpAnalysisTimestep.ItemIndex of
    0: chkHydrologicalYear.Enabled := True;
    1: chkHydrologicalYear.Enabled := False;
  else Assert(False);
  end;
end;

procedure TFrmIDFEval.rgrpAnalysisTimeStepClick(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmIDFEval.IFormShow(Sender: TObject);
begin
  SetControlStatus;
end;

end.
