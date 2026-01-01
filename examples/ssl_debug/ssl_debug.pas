program ssl_debug;

{$mode objfpc}{$H+}{$J-}
// DEBUG mode is controlled by build configuration (Debug vs Default/Release)
// Use: lazbuild --build-mode=Debug ssl_debug.lpi

uses
  SysUtils, Request
  {$IFDEF WINDOWS}, Windows{$ENDIF};

{$IFDEF WINDOWS}
function SetDllDirectoryW(lpPathName: LPCWSTR): BOOL; stdcall; external 'kernel32.dll';
{$ENDIF}

var
  Response: TResponse;
begin
  {$IFDEF WINDOWS}
  // Force Windows to search the executable directory first for DLLs
  // This prevents System32 DLLs from taking priority over local DLLs
  SetDllDirectoryW(PWideChar(UnicodeString(ExtractFilePath(ParamStr(0)))));
  {$ENDIF}


  WriteLn('=== OpenSSL Debug Information ===');
  WriteLn;

  // Show executable architecture
  WriteLn('Executable architecture: ', {$IFDEF CPU64}'64-bit'{$ELSE}'32-bit'{$ENDIF});
  WriteLn('Required DLL names: ', {$IFDEF CPU64}'libssl-*-x64.dll and libcrypto-*-x64.dll'{$ELSE}'libssl-*.dll and libcrypto-*.dll (32-bit)'{$ENDIF});
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
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('=== ERROR ===');
      WriteLn('Failed to make HTTPS request: ', E.Message);
      WriteLn;
      WriteLn('Press Enter to exit...');
      ReadLn;
      ExitCode := 1;
    end;
  end;
end.
