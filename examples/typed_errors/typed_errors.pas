// Tell timeouts, network failures, and HTTP errors apart
program typed_errors;

{$mode objfpc}{$H+}

uses
  SysUtils, Request;

procedure Fetch(const Title, URL: string);
var
  Response: TResponse;
begin
  Write(Title, ': ');
  try
    Response := Http.Get(URL);
    Response.RaiseForStatus; // opt in: non-2xx raises ERequestError
    WriteLn('OK, HTTP ', Response.StatusCode);
  except
    // Most specific first: ETimeoutError is an ENetworkError,
    // which is an ERequestError.
    on E: ETimeoutError do
      WriteLn('timed out - try again later');
    on E: ENetworkError do
      WriteLn('network problem - check the connection');
    on E: ERequestError do
      WriteLn('request failed - ', E.Message);
  end;
end;

var
  TryResult: TRequestResult;
begin
  Http.SetTimeout(2000);  // 2 s I/O timeout, to make the timeout case quick
  Http.SetRetry(0, 0);    // no automatic retries, so each case fails once

  Fetch('Success', 'https://httpbin.org/get');
  Fetch('HTTP error', 'https://httpbin.org/status/404');
  Fetch('Timeout', 'https://httpbin.org/delay/5');
  Fetch('Network error', 'http://no-such-host.invalid/');

  // The same without exceptions: Success is False only for transport errors.
  TryResult := Http.TryGet('https://httpbin.org/status/500');
  if TryResult.OK then
    WriteLn('TryGet: OK')
  else if TryResult.Success then
    WriteLn('TryGet: server answered HTTP ', TryResult.Response.StatusCode)
  else
    WriteLn('TryGet: failed - ', TryResult.Error);
end.
