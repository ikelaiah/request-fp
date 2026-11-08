// Log in through a redirect and keep the session cookie
program session_login_redirect;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson, Request, Request.Session;

var
  Session: THttpSession;
  Response: TResponse;
begin
  try
    Session.SetBaseURL('https://httpbin.org');

    // Many logins answer with Set-Cookie and a redirect. httpbin's
    // /cookies/set does the same: it sets the cookie, then redirects to
    // /cookies. The session stores cookies from the redirect response.
    Response := Session.Get('/cookies/set?session_id=abc123');
    WriteLn('After login redirect: HTTP ', Response.StatusCode);
    WriteLn('Stored cookie: ', Session.GetCookie('session_id'));

    // Later requests to the same site send the cookie automatically.
    Response := Session.Get('/cookies');
    WriteLn('Server sees: ', Response.JSON.FindPath('cookies').AsJSON);

    // Credentials for every request; dropped if a redirect leaves the host.
    Session.SetBearerToken('my-api-token');
    Response := Session.Get('/bearer');
    WriteLn('Bearer check: HTTP ', Response.StatusCode);
  except
    on E: ERequestError do
      WriteLn('Request failed: ', E.Message);
  end;
end.
