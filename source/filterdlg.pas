{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-01 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{ A unit used internally by the TFilterDialog component }
unit FilterDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFrmFilterDialog = class(TForm)
    RbtnValue: TRadioButton;
    RbtnFlag: TRadioButton;
    EdtValue: TEdit;
    Label1: TLabel;
    CboValueCondition: TComboBox;
    CboFlag: TComboBox;
    CboFlagCondition: TComboBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    LblFilterPrompt: TLabel;
    procedure SetControlStatus(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    Value: Double;
  end;

implementation

{$R *.DFM}

procedure TFrmFilterDialog.SetControlStatus(Sender: TObject);
begin
  CboValueCondition.Enabled := RbtnValue.Checked;
  EdtValue.Enabled := RbtnValue.Checked and (CboValueCondition.ItemIndex<2);
  CboFlag.Enabled := RbtnFlag.Checked;
  CboFlagCondition.Enabled := RbtnFlag.Checked;
  BtnOK.Enabled := RbtnFlag.Checked or (RbtnValue.Checked and
    ((CboValueCondition.ItemIndex>=2) or (EdtValue.Text<>'')));
end;

procedure TFrmFilterDialog.FormShow(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFrmFilterDialog.BtnOKClick(Sender: TObject);
begin
  if RbtnValue.Checked and (CboValueCondition.ItemIndex<2) then
    Value := StrToFloat(EdtValue.Text);
end;

procedure TFrmFilterDialog.FormCreate(Sender: TObject);
begin
  CboValueCondition.ItemIndex := 0;
  CboFlag.ItemIndex := 0;
  CboFlagCondition.ItemIndex := 0;
end;

end.
