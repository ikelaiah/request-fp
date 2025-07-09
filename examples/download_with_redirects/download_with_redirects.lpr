// Example: Downloading a file with automatic redirect handling (loads entire file in memory)
program download_with_redirects;

uses Request, SysUtils, Classes;

var
  Response: TResponse;
  FileStream: TFileStream;
  FileName: string;
 
begin
  FileName := GetTempDir + 'test_redirect_auto.txt';

  // Simple GET request - redirects are now handled automatically!
  Response := Http.Get('http://httpbin.org/redirect/3');

  WriteLn('Final status: ', Response.StatusCode);

  if (Response.StatusCode = 200) and (Length(Response.Text) > 0) then
  begin
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      FileStream.WriteBuffer(Pointer(Response.Text)^, Length(Response.Text));
      WriteLn('File downloaded to: ', FileName);
      WriteLn('Response size: ', Length(Response.Text), ' bytes');
    finally
      FileStream.Free;
    end;
  end
  else
    WriteLn('Download failed, status: ', Response.StatusCode);
end.

