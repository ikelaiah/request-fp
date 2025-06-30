program session_with_cookies_headers;

uses Request, Request.Session;

var
  Session: THttpSession;
  Response: TResponse;
begin
  Session.Init;
  Session.SetBaseURL('https://httpbin.org');
  Session.SetHeader('X-Test-Header', 'RequestFP');
  Session.SetCookie('mycookie', 'cookievalue');
  Response := Session.Get('/cookies');
  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end.
