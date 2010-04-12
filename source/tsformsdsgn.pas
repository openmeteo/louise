{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit tsformsdsgn;

{$R tsforms.dcr}

interface

uses tsforms, Classes, flagcmpnt;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LoUISE Timeseries', [TDbTimeseriesForm]);
  RegisterComponents('LoUISE Dialogs', [TDbRemarksForm, TGentMultimediaForm,
    TDbEventsForm, TDBLookupsForm]);
end;

end.
