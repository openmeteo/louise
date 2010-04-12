{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit regrdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFrmRegression = class(TForm)
    Label1: TLabel;
    EdtLag: TEdit;
    ChkCrossesZero: TCheckBox;
    ChkOrganic: TCheckBox;
    ChkSeasonal: TCheckBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    chkDoFilling: TCheckBox;
    chkRandomTerm: TCheckBox;
    chkTruncToZero: TCheckBox;
    chkRandomSeed: TCheckBox;
    chkDoExtendBefore: TCheckBox;
    chkDoExtendAfter: TCheckBox;
    chkDonotFillInnerValues: TCheckBox;
    chkMeanValue: TCheckBox;
    chkOptimize: TCheckBox;
    procedure IFormShow(Sender: TObject);
    procedure ChkOrganicClick(Sender: TObject);
  private
    procedure SetControlStatus;
  public
  end;

implementation

{$R *.DFM}

procedure TFrmRegression.SetControlStatus;
begin
  chkRandomTerm.Enabled := chkDoFilling.Checked;
  chkTruncToZero.Enabled := chkRandomTerm.Enabled;
  if ChkOrganic.Checked then EdtLag.Text := '0';
  EdtLag.Enabled := not ChkOrganic.Checked;
  Label1.Enabled := EdtLag.Enabled;
  ChkCrossesZero.Enabled := EdtLag.Enabled;
  chkRandomSeed.Enabled := chkRandomTerm.Enabled and chkRandomTerm.Checked;
  chkDoExtendBefore.Enabled := chkDoFilling.Checked;
  chkDoExtendAfter.Enabled := chkDoFilling.Checked;
  chkDonotFillInnerValues.Enabled :=
    chkDoExtendBefore.Enabled or chkDoExtendAfter.Enabled;
end;

procedure TFrmRegression.IFormShow(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmRegression.ChkOrganicClick(Sender: TObject);
begin
  SetControlStatus;
end;

end.
