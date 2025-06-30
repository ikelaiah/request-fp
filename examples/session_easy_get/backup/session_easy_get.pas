// Example 1: Easy use - Simple GET with session
program session_easy_get;

uses Request.Session;

var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Response := Session.Get('https://httpbin.org/get');
  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end.
