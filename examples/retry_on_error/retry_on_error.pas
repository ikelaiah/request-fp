// Automatic retries, and a manual retry for POST
program retry_on_error;

{$mode objfpc}{$H+}

uses
  SysUtils, Request;

var
  Response: TResponse;
  Attempts: Integer;
begin
  try
    // GET, HEAD, PUT, and DELETE retry by themselves on network errors, 429,
    // and 503: 2 retries by default. Here: 3 retries, waiting 500, 1000,
    // then 2000 ms (or whatever Retry-After asks for).
    Http.SetRetry(3, 500);
    Response := Http.Get('https://httpbin.org/status/503'); // always 503
    WriteLn('GET after automatic retries: HTTP ', Response.StatusCode);

    // POST is never retried automatically, because repeating it could
    // create something twice. Retry it yourself only when that is safe.
    Attempts := 0;
    repeat
      Inc(Attempts);
      Response := Http.Post('https://httpbin.org/status/503', 'order=42');
      WriteLn('POST attempt ', Attempts, ': HTTP ', Response.StatusCode);
      if not Response.OK and (Attempts < 3) then
        Sleep(1000);
    until Response.OK or (Attempts >= 3);

    if not Response.OK then
      WriteLn('Giving up.');
  except
    on E: ERequestError do
      WriteLn('Request failed: ', E.Message);
  end;
end.
