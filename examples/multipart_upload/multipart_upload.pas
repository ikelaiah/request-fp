program multipart_upload;

uses Request, SysUtils;

var
  Response: TResponse;
  TempFile: string;
  F: TextFile;
begin
  // Create a temporary file to upload
  TempFile := GetTempDir + 'requestfp_example_upload.txt';
  AssignFile(F, TempFile);
  Rewrite(F);
  WriteLn(F, 'This is a test file for multipart upload.');
  CloseFile(F);

  Response := Http.PostMultipart('https://httpbin.org/post',
    [TKeyValue.Create('field1', 'value1')],
    [TKeyValue.Create('file1', TempFile)]);

  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);

  DeleteFile(TempFile);
end.
