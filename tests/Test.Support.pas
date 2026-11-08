unit Test.Support;

{$mode objfpc}{$H+}{$J-}

interface

function TestBaseURL: string;
function TestURL(const Path: string): string;
function TestFailureURL: string;
{ Host and port of the local test server; False when tests use another server. }
function LocalFixtureHostPort(out Host: string; out Port: Integer): Boolean;

implementation

uses
  SysUtils;

function TestBaseURL: string;
begin
  // Defaults to the local fixture started by run-tests.sh / run-tests.ps1.
  Result := GetEnvironmentVariable('REQUEST_FP_TEST_BASE_URL');
  if Result = '' then
    Result := 'http://127.0.0.1:18080';

  while (Length(Result) > 0) and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);
end;

function TestURL(const Path: string): string;
begin
  if (Path <> '') and (Path[1] = '/') then
    Result := TestBaseURL + Path
  else
    Result := TestBaseURL + '/' + Path;
end;

function TestFailureURL: string;
begin
  Result := GetEnvironmentVariable('REQUEST_FP_TEST_FAILURE_URL');
  if Result = '' then
    Result := 'http://127.0.0.1:1';
end;

function LocalFixtureHostPort(out Host: string; out Port: Integer): Boolean;
var
  Base: string;
  P: Integer;
begin
  Base := TestBaseURL;
  Host := '';
  Port := 0;
  Result := Pos('http://127.0.0.1:', Base) = 1;
  if not Result then
    Exit;
  Host := '127.0.0.1';
  P := Length('http://127.0.0.1:') + 1;
  Result := TryStrToInt(Copy(Base, P, MaxInt), Port);
end;

end.
