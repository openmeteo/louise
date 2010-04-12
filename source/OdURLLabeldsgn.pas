unit OdURLLabeldsgn;

{$R OdURLLabel.DCR}

interface

uses Classes, OdURLLabel;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LoUISE Components', [TOdURLLabel]);
end;

end.
