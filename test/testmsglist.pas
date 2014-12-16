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
    procedure TestGetAllMessages;
    procedure TestShowOrRaiseAndFree;
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

procedure TestTMessageList.TestGetAllMessages;
var ExpectedResult, ActualResult: string;
begin
  FMessageList.AddMessage('This is information', msgInfo);
  FMessageList.AddMessage('This is a warning', msgWarning);
  FMessageList.AddMessage('This is an error', msgError);
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
  FMessageList.AddMessage('This is an error', msgError);
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

initialization
  RegisterTest(TestTMessageList.Suite);

end.

