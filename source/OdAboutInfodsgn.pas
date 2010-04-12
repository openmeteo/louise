unit OdAboutInfodsgn;

{$R OdAboutInfo.DCR}

interface

uses Classes;

procedure Register;

implementation

uses OdAboutInfo;

procedure Register;
begin
  RegisterComponents('LoUISE Dialogs', [TOdAboutInfo]);
end;

end.
