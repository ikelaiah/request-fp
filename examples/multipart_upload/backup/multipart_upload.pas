// Example 3: More complex use - Multipart upload with file and fields
program multipart_upload;

uses Request, SysUtils;

var
  RequestObj: THttpRequest;
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

  RequestObj := RequestObj.Post
    .URL('https://httpbin.org/post')
    .AddFile('file1', TempFile)
    .AddFormField('field1', 'value1');
  Response := RequestObj.Send;

  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);

  DeleteFile(TempFile);
end.
