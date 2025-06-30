// Niche Example: Custom error handling and retry logic
program retry_on_error;

uses Request, SysUtils;

var
  HttpReq:THttpRequest;
  Response: TResponse;
  Attempts: Integer;
  Success: Boolean;
begin
  Attempts := 0;
  Success := False;
  repeat
    Inc(Attempts);
    try
      Response := HttpReq
        .Get
        .URL('https://httpbin.org/status/503') // Always returns 503
        .WithTimeout(3000)
        .Send;
      if (Response.StatusCode >= 200) and (Response.StatusCode < 300) then
      begin
        WriteLn('Success on attempt ', Attempts, ': ', Response.Text);
        Success := True;
      end
      else
        WriteLn('Attempt ', Attempts, ' failed: HTTP ', Response.StatusCode);
    except
      on E: Exception do
        WriteLn('Attempt ', Attempts, ' error: ', E.Message);
    end;
    Sleep(1000);
  until Success or (Attempts >= 3);

  if not Success then
    WriteLn('All attempts failed.');
end.
