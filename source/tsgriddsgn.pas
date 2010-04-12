{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit tsgriddsgn;

{$R tsgrid.dcr}

interface

uses Classes, tsgrid;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LoUISE Timeseries', [TTimeseriesGrid, TFilterDialog]);
end;

end.
