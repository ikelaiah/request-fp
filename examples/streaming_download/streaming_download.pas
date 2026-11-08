// Stream a large download to disk with a progress bar
program streaming_download;

{$mode objfpc}{$H+}

uses
  SysUtils, Request;

const
  URL = 'https://httpbin.org/bytes/102400';
  FilePath = 'download.bin';
  BarWidth = 30;

procedure ShowProgress(Received, Total: Int64);
var
  Filled: Integer;
begin
  if Total > 0 then
  begin
    Filled := Received * BarWidth div Total;
    Write(#13'[', StringOfChar('#', Filled), StringOfChar('.', BarWidth - Filled),
      '] ', Received * 100 div Total, '%');
  end
  else
    Write(#13, Received, ' bytes'); // server sent no Content-Length
end;

var
  Response: TResponse;
begin
  try
    // The body goes straight to disk: download.bin.part, renamed when done.
    Response := Http.Download(URL, FilePath, @ShowProgress);
    WriteLn;
    if Response.OK then
      WriteLn('Saved ', FilePath, ' (', Response.HeaderValue('Content-Length'),
        ' bytes)')
    else
      // FilePath is left untouched; the error body is in Text.
      WriteLn('Download failed: HTTP ', Response.StatusCode, ' ', Response.Text);
  except
    on E: ERequestError do
      WriteLn('Download failed: ', E.Message);
  end;
end.
