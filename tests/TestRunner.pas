program TestRunner;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, consoletestrunner, fpcunit, testregistry,
  TidyKit.Arrays.Tests;

type
  { TMyTestRunner }
  TMyTestRunner = class(TConsoleTestRunner)
  protected
    procedure WriteTestHeader(ATest: TTest); override;
  end;

{ TMyTestRunner }

procedure TMyTestRunner.WriteTestHeader(ATest: TTest);
begin
  WriteLn('===== Running Test Suite: ', ATest.TestName, ' =====');
end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
