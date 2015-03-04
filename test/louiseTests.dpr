program louiseTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  testmsglist in 'testmsglist.pas',
  msglist in '..\source\msglist.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

