program file_download;

uses Request, SysUtils, Classes;

var
  HttpReq: THttpRequest;
  Response: TResponse;
  FileStream: TFileStream;
  FileName: string;
begin
  FileName := GetTempDir + 'downloaded_image.png';
  Response := HttpReq
    .Get
    .URL('https://httpbin.org/image/png')
    .Send;

  if (Response.StatusCode = 200) and (Length(Response.Text) > 0) then
  begin
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      FileStream.WriteBuffer(Pointer(Response.Text)^, Length(Response.Text));
      WriteLn('File downloaded to: ', FileName);
    finally
      FileStream.Free;
    end;
  end
  else
    WriteLn('Download failed, status: ', Response.StatusCode);
end.
