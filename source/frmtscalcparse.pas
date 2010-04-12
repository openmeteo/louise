{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-07 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit frmtscalcparse;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Parser, Calculus, tsgrid, ts, StdCtrls, DateUtils;

type
  TFrmTimeseriesParser = class(TForm)
    btnCalculate: TButton;
    btnCancel: TButton;
    lblExpression: TLabel;
    memoExpression: TMemo;
    listVariables: TListBox;
    lblVariables: TLabel;
    Label1: TLabel;
    Memo1: TMemo;
    procedure LFormCreate(Sender: TObject);
    procedure LFormDestroy(Sender: TObject);
    procedure btnCalculateClick(Sender: TObject);
    procedure LFormShow(Sender: TObject);
  private
    FParser: TParser;
    FTimeseriesGrid: TTimeseriesGrid;
    FVarsArray: array of TVarDef;
    procedure CreateVars;
    procedure Calc(expr: string);
  public
    property TimeseriesGrid: TTimeseriesGrid read FTimeseriesGrid
      write FTimeseriesGrid;
  end;

implementation

{$R *.dfm}

uses
  tsprocess, contnrs, Dates;

procedure TFrmTimeseriesParser.LFormCreate(Sender: TObject);
begin
  FVarsArray := nil;
  FParser := TParser.Create;
end;

procedure TFrmTimeseriesParser.LFormDestroy(Sender: TObject);
begin
  FParser.Free;
  FVarsArray := nil;
end;

const
  ExtraVars: array[0..6] of string = ('index', 'year', 'month', 'day',
    'dayinyear', 'hour', 'minute');

procedure TFrmTimeseriesParser.CreateVars;
var
  i: Integer;
begin
  SetLength(FVarsArray, FTimeseriesGrid.Count+7);
  for i := 0 to FTimeseriesGrid.Count-1 do
  begin
    FVarsArray[i] := TVarDef.Create;
    FVarsArray[i].name := 'x'+IntToStr(i+1);
    FParser.addVar(FVarsArray[i]);
  end;
  for i := FTimeseriesGrid.Count to FTimeseriesGrid.Count+6 do
  begin
    FVarsArray[i] := TVarDef.Create;
    FVarsArray[i].name := ExtraVars[i-FTimeseriesGrid.Count];
    FParser.addVar(FVarsArray[i]);
  end;
  FParser.standardFunctions;
end;

resourcestring
  rsTimeseriesTitle = 'Calculated time series';
  rsTimeseriesComment = 'Created by time series calculations';
  rsErrorInExpression = 'Error in expression';
  rsTimeseriesShouldHaveSameTimestep =
    'Time series should be of the same time step';

procedure TFrmTimeseriesParser.Calc(expr: string);
var
  ATimeseries: TTimeseries;
  ATimeseriesList: TObjectList;
  ADateTimeList: TDateTimeList;
  AAllRecords: TDateTimeList;
  ACalculus: TCalculus;
  i, j, k: Integer;
  err: Byte;
  MathErrorFlag: Boolean;
  AResult: Real;

  procedure AssessCommonPeriod;
  var
    i: Integer;
  begin
    ATimeseriesList := nil;
    ATimeseriesList := TObjectList.Create(False);
    for i := 0 to FTimeseriesGrid.Count-1 do
      if Pos('x'+IntToStr(i+1),memoExpression.Lines.Text)>0 then
        ATimeseriesList.Add(FTimeseriesGrid.Data[i]);
    ADateTimeList := GetCommonPeriod(ATimeseriesList,0);
    AAllRecords := GetAllRecords(ATimeseriesList, 0);
{ Normally GetCommonPeriod or GetAllRecords raising exception if no time series
  specified. Make a double check with Assert to ensure this.}
    Assert(ATimeseriesList.Count>0);
  end;

begin
  if Length(FVarsArray)<1 then
    CreateVars;
  ADateTimeList := nil;
  AAllRecords := nil;
  ATimeseries := nil;
  try try
    AssessCommonPeriod;
    {AssesCommonPeriod raises if no time series specified}
    for i := 0 to ATimeseriesList.Count-2 do
      if TTimeseries(ATimeseriesList[i]).TimeStep <>
        TTimeseries(ATimeseriesList[i+1]).TimeStep then
          raise Exception.Create(rsTimeseriesShouldHaveSameTimestep);
    ATimeseries := TTimeseries.Create;
    ATimeseries.AssignMeta(TTimeseries(ATimeseriesList[0]));
    ATimeseries.Title :=   rsTimeseriesTitle;
    ATimeseries.Comment := rsTimeseriesComment;
    for k := 0 to AAllRecords.Count-1 do
    begin
      ATimeseries.Add(AAllRecords[k], True, 0, '', msNew);
      i := ADateTimeList.IndexOf(AAllRecords[k]);
      if i<0 then Continue;
      for j := 0 to FTimeseriesGrid.Count-1 do
        with FTimeseriesGrid.Data[j] do
          if (IndexOf(ADateTimeList[i])<0) or
            (Items[IndexOf(ADateTimeList[i])].IsNull) then
            FVarsArray[j].value := 0 else
            FVarsArray[j].value := Items[IndexOf(ADateTimeList[i])].AsFloat;
      j := FTimeseriesGrid.Count;
      FVarsArray[j].value := i; Inc(j);
      FVarsArray[j].value := YearOf(ADateTimeList[i]); Inc(j);
      FVarsArray[j].value := MonthOf(ADateTimeList[i]); Inc(j);
      FVarsArray[j].value := DayOf(ADateTimeList[i]); Inc(j);
      FVarsArray[j].value := DayOfTheYear(ADateTimeList[i]); Inc(j);
      FVarsArray[j].value := HourOfTheDay(ADateTimeList[i]); Inc(j);
      FVarsArray[j].value := MinuteOfTheHour(ADateTimeList[i]);
      ACalculus := nil;
      try
        MathErrorFlag := False;
        ACalculus := FParser.compile(expr, err);
        if ACalculus<>nil then
          AResult := ACalculus.eval;
      except
        on EMathError do
          MathErrorFlag := True;
        else
          raise;
      end;
      if ACalculus = nil then
        if err>0 then
          raise EMathError.Create(rsErrorInExpression);
      if MathErrorFlag then
        ATimeseries.Last.SetFlag('SUSPECT', True);
        ATimeseries.Last.AsFloat := AResult;
    end;
    FTimeseriesGrid.Add(ATimeseries);
    ATimeseries := nil;
  finally
    ATimeseriesList.Free;
    ATimeseries.Free;
    ADateTimeList.Free;
    AAllRecords.Free;
  end;
  except
    raise;
  end;
end;

procedure TFrmTimeseriesParser.btnCalculateClick(Sender: TObject);
var
  s: string;
  i: Integer;
  ACursor: TCursor;
begin
  s := '';
  ACursor := Screen.Cursor;
  for i := 0 to memoExpression.Lines.Count-1 do
    s := s + Trim(LowerCase(memoExpression.Lines[i]));
  try
    Screen.Cursor := crHourGlass;
    Calc(s);
  finally
    Screen.Cursor := ACursor;
  end;
  ModalResult := mrOk;
end;

procedure TFrmTimeseriesParser.LFormShow(Sender: TObject);
var
  i: Integer;
begin
  listVariables.Clear;
  with FTimeseriesGrid do
    for i := 0 to Count-1 do
      listVariables.Items.Add('x'+IntToStr(i+1)+': '+Data[i].Title+
        ', '+Data[i].Comment);
end;

end.
