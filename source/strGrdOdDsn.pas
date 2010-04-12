{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-05 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit strGrdOdDsn;

{$R strGrdOd.dcr}

interface

uses Classes, strGrdOd;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LoUISE Components', [TOdStringGrid, TOdDBGrid, TOdValueListEditor]);
end;

end.
