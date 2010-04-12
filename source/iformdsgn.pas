{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

{ Design time information for TIForm. }
unit iformdsgn;

interface

procedure Register;

implementation

uses Classes, iform, DesignIntf, DesignEditors;

procedure Register;
begin
  RegisterCustomModule(TIForm, TCustomModule);
  RegisterCustomModule(TLForm, TCustomModule);
end;

end.
