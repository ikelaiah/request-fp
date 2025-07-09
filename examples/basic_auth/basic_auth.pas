program basic_auth;

uses Request, Base64;

var
  Response: TResponse;
  AuthHeader: string;
begin
  // Create Basic Auth header: Base64 encode "username:password"
  AuthHeader := 'Basic ' + EncodeStringBase64('user:pass');
  Response := Http.Get('https://httpbin.org/basic-auth/user/pass',
                       [TKeyValue.Create('Authorization', AuthHeader)]);
  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end.
