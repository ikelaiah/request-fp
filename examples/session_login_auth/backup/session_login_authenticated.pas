program session_login_authenticated;

uses Request, Request.Session, fpjson;

var
  Session: THttpSession;
  Response: TResponse;
  LoginData: TJSONObject;
begin
  Session.Init;
  Session.SetBaseURL('https://httpbin.org');

  // Simulate login (httpbin.org/post just echoes data)
  LoginData := TJSONObject.Create;
  try
    LoginData.Add('username', 'user');
    LoginData.Add('password', 'pass');
    Response := Session.PostJSON('/post', LoginData.AsJSON);
    WriteLn('Login Status: ', Response.StatusCode);
    WriteLn('Login Response: ', Response.Text);
  finally
    LoginData.Free;
  end;

  // Authenticated request (simulate by sending a header)
  Session.SetHeader('Authorization', 'Bearer dummy_token');
  Response := Session.Get('/bearer');
  WriteLn('Auth Status: ', Response.StatusCode);
  WriteLn('Auth Response: ', Response.Text);
end.
