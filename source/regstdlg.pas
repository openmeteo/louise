{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit RegStDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin;

type
  TFrmRegularizeStep = class(TForm)
    Label2: TLabel;
    EdtTimeOffset: TEdit;
    BtnOk: TButton;
    BtnCancel: TButton;
    RgrpMethod: TRadioGroup;
    Label1: TLabel;
    CmbNewDateFlag: TComboBox;
    GroupBox1: TGroupBox;
    rbDefaultTimestep: TRadioButton;
    rbTimestepFromList: TRadioButton;
    rbOtherTimeStep: TRadioButton;
    cmbTimeSteps: TComboBox;
    spinCustomStep: TSpinEdit;
    lblDefaultStep: TLabel;
    procedure rbDefaultTimestepClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SetControlStatus;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TFrmRegularizeStep.SetControlStatus;
begin
  cmbTimeSteps.Enabled := rbTimestepFromList.Checked;
  spinCustomStep.Enabled := rbOtherTimeStep.Checked;
end;

procedure TFrmRegularizeStep.FormShow(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmRegularizeStep.rbDefaultTimestepClick(Sender: TObject);
begin
  SetControlStatus;
end;

end.
