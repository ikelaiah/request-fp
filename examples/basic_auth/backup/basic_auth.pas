program basic_auth;

uses Request;

var
  Request: THttpRequest;
  Response: TResponse;
begin
  Response := Request
    .Get
    .URL('https://httpbin.org/basic-auth/user/pass')
    .BasicAuth('user', 'pass')
    .Send;

  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end.
