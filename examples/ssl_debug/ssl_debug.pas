program ssl_debug;

{$mode objfpc}{$H+}{$J-}
{$DEFINE DEBUG}  // Enable debug mode to see SSL DLL loading information

uses
  SysUtils, Request;

var
  Response: TResponse;
begin
  WriteLn('=== OpenSSL Debug Information ===');
  WriteLn;

  try
    // Make a simple HTTPS request - this will trigger SSL initialization
    // and show debug output about where DLLs are loaded from
    WriteLn('Making HTTPS request to verify OpenSSL is working...');
    Response := Http.Get('https://httpbin.org/get');

    WriteLn;
    WriteLn('=== SUCCESS ===');
    WriteLn('Status Code: ', Response.StatusCode);
    WriteLn('OpenSSL is working correctly!');
    WriteLn;
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('=== ERROR ===');
      WriteLn('Failed to make HTTPS request: ', E.Message);
      WriteLn;
      ExitCode := 1;
    end;
  end;
end.
