unit testmsglist;

interface

uses
  TestFramework, Contnrs, msglist;

type

  TestTMessageList = class(TTestCase)
  strict private
    FMessageList: TMessageList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestErrorCount;
    procedure TestGetAllMessages;
    procedure TestShowOrRaiseAndFree;
    procedure TestAddMsgIf;
    procedure TestAddMsgIfNotReal;
    procedure TestAddMsgIfNegative;
    procedure TestAddMsgIfINegative;
    procedure TestAddMsgIfOutsideZeroOne;
  end;

implementation

uses SysUtils;

procedure TestTMessageList.SetUp;
begin
  FMessageList := TMessageList.Create;
end;

procedure TestTMessageList.TearDown;
begin
  FMessageList.Free;
  FMessageList := nil;
end;

procedure TestTMessageList.TestErrorCount;
begin
  CheckEquals(FMessageList.ErrorCount, 0);
  FMessageList.AddMsg('This is information', msgInfo);
  FMessageList.AddMsg('This is a warning', msgWarning);
  FMessageList.AddMsg('This is an error', msgError);
  CheckEquals(FMessageList.ErrorCount, 1);
  FMessageList.AddMsg('This is another error', msgError);
  FMessageList.AddMsg('This is yet another error', msgError);
  CheckEquals(FMessageList.ErrorCount, 3);
end;

procedure TestTMessageList.TestGetAllMessages;
var ExpectedResult, ActualResult: string;
begin
  FMessageList.AddMsg('This is information', msgInfo);
  FMessageList.AddMsg('This is a warning', msgWarning);
  FMessageList.AddMsg('This is an error', msgError);
  ActualResult := FMessageList.GetAllMessages;
  ExpectedResult :=
    '[Information] This is information' + #13#10 +
    '[Warning] This is a warning' + #13#10 +
    '[Error] This is an error' + #13#10;
  CheckEquals(ExpectedResult, ActualResult);
end;

procedure TestTMessageList.TestShowOrRaiseAndFree;
begin
  // There are no messages; should do nothing but free.
  FMessageList.ShowOrRaiseAndFree;
  FMessageList := nil;

  FMessageList := TMessageList.Create;

  // There is an error message; should raise exception.
  FMessageList.AddMsg('This is an error', msgError);
  try try
    FMessageList.ShowOrRaiseAndFree;
    Check(False, 'This should never execute');
  except
    on Exception do Check(True, 'ShowOrRaiseAndFree properly raised exception');
  end;
  finally
    FMessageList := nil;
  end;

  // We'd also like to test the case where there are messages but not errors,
  // but this would ShowMessage and would stop the test execution until OK
  // is pressed, and currently I don't have an idea of how to make it work.
end;

procedure TestTMessageList.TestAddMsgIf;
var s: string;
begin
  FMessageList.AddMsgIf(True, 'This message, 1234, should be in', msgWarning);
  FMessageList.AddMsgIf(False, 'This message, 5678, should be out', msgWarning);
  s := FMessageList.GetAllMessages;
  Check(s.Contains('1234'));
  CheckFalse(s.Contains('5678'))
end;

procedure TestTMessageList.TestAddMsgIfNotReal;
var s: string;
begin
  FMessageList.AddMsgIfNotReal(
    '', 'This message, 120, should be out', msgWarning);
  FMessageList.AddMsgIfNotReal(
    '', 'This message, 121, should be in', msgWarning, False);
  FMessageList.AddMsgIfNotReal(
    'notreal', 'This message, 34, should be in', msgWarning);
  FMessageList.AddMsgIfNotReal(
    '18', 'This message, 56, should be out', msgWarning);
  s := FMessageList.GetAllMessages;
  Check(s.Contains('34'));
  CheckFalse(s.Contains('120'));
  Check(s.Contains('121'));
  CheckFalse(s.Contains('56'));
end;

procedure TestTMessageList.TestAddMsgIfNegative;
var s: string;
begin
  FMessageList.AddMsgIfNegative(
    '', 'This message, 120, should be out', msgWarning);
  FMessageList.AddMsgIfNegative(
    '', 'This message, 121, should be in', msgWarning, False);
  FMessageList.AddMsgIfNegative(
    'notanumber', 'This message, 34, should be in', msgWarning);
  FMessageList.AddMsgIfNegative(
    '18.1', 'This message, 56, should be out', msgWarning);
  FMessageList.AddMsgIfNegative(
    '-18.2', 'This message, 78, should be in', msgWarning);
  s := FMessageList.GetAllMessages;
  Check(s.Contains('34'));
  Check(s.Contains('78'));
  CheckFalse(s.Contains('120'));
  Check(s.Contains('121'));
  CheckFalse(s.Contains('56'));
end;

procedure TestTMessageList.TestAddMsgIfINegative;
var s: string;
begin
  FMessageList.AddMsgIfINegative(
    '', 'This message, 120, should be out', msgWarning);
  FMessageList.AddMsgIfINegative(
    '', 'This message, 121, should be in', msgWarning, False);
  FMessageList.AddMsgIfINegative(
    'notanumber', 'This message, 34, should be in', msgWarning);
  FMessageList.AddMsgIfINegative(
    '18', 'This message, 56, should be out', msgWarning);
  FMessageList.AddMsgIfINegative(
    '-18', 'This message, 78, should be in', msgWarning);
  FMessageList.AddMsgIfINegative(
    '18.1', 'This message, 90, should be in', msgWarning);
  s := FMessageList.GetAllMessages;
  Check(s.Contains('34'));
  Check(s.Contains('78'));
  Check(s.Contains('90'));
  CheckFalse(s.Contains('120'));
  Check(s.Contains('121'));
  CheckFalse(s.Contains('56'));
end;

procedure TestTMessageList.TestAddMsgIfOutsideZeroOne;
var s: string;
begin
  FMessageList.AddMsgIfOutsideZeroOne(
    '', 'This message, 120, should be out', msgWarning);
  FMessageList.AddMsgIfOutsideZeroOne(
    '', 'This message, 121, should be in', msgWarning, False);
  FMessageList.AddMsgIfOutsideZeroOne(
    'notanumber', 'This message, 34, should be in', msgWarning);
  FMessageList.AddMsgIfOutsideZeroOne(
    '0.7', 'This message, 56, should be out', msgWarning);
  FMessageList.AddMsgIfOutsideZeroOne(
    '-0.1', 'This message, 78, should be in', msgWarning);
  FMessageList.AddMsgIfOutsideZeroOne(
    '1.1', 'This message, 90, should be in', msgWarning);
  s := FMessageList.GetAllMessages;
  Check(s.Contains('34'));
  Check(s.Contains('78'));
  Check(s.Contains('90'));
  CheckFalse(s.Contains('120'));
  Check(s.Contains('121'));
  CheckFalse(s.Contains('56'));
end;

initialization
  RegisterTest(TestTMessageList.Suite);

end.

