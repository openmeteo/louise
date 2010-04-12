unit frmdisaggr;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFrmDissagregate = class(TForm)
    rgrpVariableType: TRadioGroup;
    btnOK: TButton;
    btnCancel: TButton;
    rgrpRandomModel: TRadioGroup;
    procedure LFormShow(Sender: TObject);
  private
    procedure SetControlStatus;
  public

  end;

implementation

{$R *.dfm}

procedure TFrmDissagregate.SetControlStatus;
begin
  if rgrpVariableType.ItemIndex in [1,3] then
    rgrpRandomModel.Enabled := True else
    rgrpRandomModel.Enabled := False;
end;

procedure TFrmDissagregate.LFormShow(Sender: TObject);
begin
  SetControlStatus;
end;

end.
