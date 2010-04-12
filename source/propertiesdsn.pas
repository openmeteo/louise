{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-05 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit propertiesdsn;

{$R properties.dcr}

interface

uses Classes;

procedure Register;

implementation

uses frmproperties;

procedure Register;
begin
  RegisterComponents('LoUISE Dialogs', [TPropertiesDialog]);
end;

end.
 