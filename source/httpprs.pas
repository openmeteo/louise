{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2006 National Technical University of Athens      }
{                                                                  }
{******************************************************************}

{** http requests parser library for Odysseus applications...
}
unit httpprs;

interface

uses Classes, SysUtils, genopts;

type
  TActionType = (atQuery_engine,
    atGet_selected_gisgentity,
    atGisgentity_exists,
    atAdd_new_gentity,
    atQuery_gisgentity,
    atQuery_allgisgentities,
    atQuery_selectedarea,
    atDisplay_gisgentityid,
    atDisplay_gisseveralgentititesid,
    atDisplay_gisgentityxy,
    atDisplay_gisseveralgentitiesxy,
    atDisplay_values);


  TActionSubType = (satTest1, satTest2);

type
  EBadRequest = class(Exception);
  ENotImplemented = class(Exception);

type
  TRequestRecord = record
{** See genopts.TOdModel
}
    ASource: TOdModel;
    AAction: TActionType;
    ASubAction: TActionSubType;
    ACardinal: Integer;
    AFloat: Real;
    AString: string;
    ABoolean: Boolean;
    SubActionExist, CardinalExist, FloatExist, StringExist, BooleanExist:
      Boolean;
  end;

type
  TActionProc = function(ARequestRecord: TRequestRecord; Params: TStringList;
    var AResponseText: string): Integer of object;

{** Use this class to parse the paramstr from an Http request.
    Paramstr should contain the "source" and "action" parameters or
    else 400 bad request is returned.
}
type
  TServerReqParser = class(TPersistent)
  private
    FActionProc: TActionProc;
    FParamStr: string;
    FResponseStr: string;
    FResponseNo: Integer;
  public
    constructor Create;
{** This is the procedure called within the Synchronize method of
    the thread object. Set the ActionProc and pass the parameter string to
    ParamStr before calling the ThreadProc.
}
    procedure ThreadProc;
{** Set the ActionProc to a function of type TActionProc. ActionProc is defined
    within your application and user interface.
    ActionProc is a function returning the Response number. If ActionProc
    result is not set to a proper value like 200, 400, 501, etc. then the
    default value of 200 is returned by Response No.
}
    property ActionProc: TActionProc read FActionProc write FActionProc;
{** This is the Parameter String (ParamStr) passed from the url of type:
    http://localhost/ParamStr
    ParamStr should be & delimited such as:
    ?param1=value1&param2=value2....
}
    property ParamStr: string read FParamStr write FParamStr;
{** This is the response string, CR/LF (cariage returns) delimited.
}
    property ResponseStr: string read FResponseStr write FResponseStr;
{** ResponseNo returns a valid response code, defined in:
    http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
    A default value of 200 is returned if ActionProc function does not
    return a proper value.
}
    property ResponseNo: Integer read FResponseNo write FResponseNo;
  end;

implementation

{TServerReqParser}

constructor TServerReqParser.Create;
begin
  inherited Create;
  FResponseNo := 200;
  FActionProc := nil;
end;

procedure TServerReqParser.ThreadProc;
var
  AStringList: TStringList;
  ARequestRecord: TRequestRecord;
  s: string;
  AIndex: Integer;

  procedure ParseSource;
  begin
    s := AStringList.Values['source'];
    AIndex := AStringList.IndexOfName('source');
    if AIndex<0 then raise EBadRequest.Create('400: Bad request');
    AStringList.Delete(AIndex);
    with ARequestRecord do
    begin
      if s='hydrognomon' then ASource := odHydrognomon
      else if s='hydronomeas' then ASource := odHydronomeas
      else if s='rypos' then ASource := odRypos
      else if s='dipsos' then ASource := odDipsos
      else raise ENotImplemented.Create('501: Not implemented');
    end;
  end;

  procedure ParseAction;
  begin
    s := AStringList.Values['action'];
    AIndex := AStringList.IndexOfName('action');
    if AIndex<0 then raise EBadRequest.Create('400: Bad request');
    AStringList.Delete(AIndex);
    with ARequestRecord do
    begin
      if s='query_engine' then AAction := atQuery_engine
      else if s='get_selected_gisgentity' then
        AAction := atGet_selected_gisgentity
      else if s='gisgentity_exists' then AAction := atGisgentity_exists
      else if s='add_new_gentity' then AAction := atAdd_new_gentity
      else if s='query_gisgentity' then AAction := atQuery_gisgentity
      else if s='query_allgisgentities' then AAction := atQuery_allgisgentities
      else if s='query_selectedarea' then AAction := atQuery_selectedarea
      else if s='display_gisgentityid' then AAction := atDisplay_gisgentityid
      else if s='display_gisseveralgentititesid' then
        AAction := atDisplay_gisseveralgentititesid
      else if s='display_gisgentityxy' then AAction := atDisplay_gisgentityxy
      else if s='display_gisseveralgentitiesxy' then
        AAction := atDisplay_gisseveralgentitiesxy
      else if s='display_values' then AAction := atDisplay_values
      else raise ENotImplemented.Create('501: Not implemented');
    end;
  end;

  procedure ParseSubAction;
  begin
    ARequestRecord.SubActionExist := False;
    s := AStringList.Values['subaction'];
    AIndex := AStringList.IndexOfName('subaction');
    {Subaction is optional, so if no subaction set then simply exit}
    if AIndex<0 then Exit;
    if AIndex >= 0 then AStringList.Delete(AIndex);
    with ARequestRecord do
    begin
      if s='test1' then ASubAction := satTest1
      else if s='test2' then ASubAction := satTest2
      else raise ENotImplemented.Create('501: Not implemented');
      SubActionExist := True;      
    end;
  end;

  procedure ParseOther;
  begin
    ARequestRecord.CardinalExist := False;
    s := AStringList.Values['cardinalvalue'];
    AIndex := AStringList.IndexOfName('cardinalvalue');
    if AIndex>=0 then
    begin
      try
        ARequestRecord.ACardinal := StrToInt(s);
        ARequestRecord.CardinalExist := True;
      except
        on EConvertError do raise EBadRequest.Create('400: Bad request');
        else raise;
      end;
      AStringList.Delete(AIndex);
    end;

    ARequestRecord.FloatExist := False;
    s := AStringList.Values['floatvalue'];
    AIndex := AStringList.IndexOfName('floatvalue');
    if AIndex>=0 then
    begin
      try
        ARequestRecord.AFloat := StrToFloat(s);
        ARequestRecord.FloatExist := True;
      except
        on EConvertError do raise EBadRequest.Create('400: Bad request');
        else raise;
      end;
      AStringList.Delete(AIndex);
    end;

    ARequestRecord.BooleanExist := False;
    s := AStringList.Values['boolvalue'];
    AIndex := AStringList.IndexOfName('boolvalue');
    if AIndex>=0 then
    begin
      try
        ARequestRecord.ABoolean := StrToBool(s);
        ARequestRecord.BooleanExist := True;        
      except
        on EConvertError do raise EBadRequest.Create('400: Bad request');
        else raise;
      end;
      AStringList.Delete(AIndex);
    end;

    ARequestRecord.StringExist := False;
    s := AStringList.Values['stringvalue'];
    AIndex := AStringList.IndexOfName('stringvalue');
    if AIndex>=0 then
    begin
      ARequestRecord.AString := s;
      ARequestRecord.StringExist := True;      
      AStringList.Delete(AIndex);
    end;
  end;

begin
  Assert(Assigned(FActionProc));
  AStringList := nil;
  try
    AStringList := TStringList.Create;
    AStringList.Delimiter := '&';
    AStringList.Text := FParamStr;
    try
      ParseSource;
      ParseAction;
      ParseSubAction;
      ParseOther;
      FResponseNo := FActionProc(ARequestRecord, AStringList, FResponseStr);
      if (FResponseNo<>200) and (FResponseNo<>400) and (FResponseNo<>404) and
        (FResponseNo<>501) then FResponseNo := 200;
    except
      on EBadRequest do ResponseNo := 400;
      on ENotImplemented do ResponseNo := 501
      else raise;
    end;
  finally
    AStringList.Free;
  end;
end;

end.
