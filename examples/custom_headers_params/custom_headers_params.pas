program custom_headers_params;

uses Request;

var
  Response: TResponse;
begin
  Response := Http.Get('https://httpbin.org/get',
                       [TKeyValue.Create('X-Custom-Header', 'MyValue')],
                       [TKeyValue.Create('foo', 'bar'), TKeyValue.Create('baz', 'qux')]);
  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end.
