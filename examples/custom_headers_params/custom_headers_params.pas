program custom_headers_params;

uses Request;

var
  Response: TResponse;
begin
  Response := Http.Get('https://httpbin.org/get',
                       [KV('X-Custom-Header', 'MyValue')],
                       [KV('foo', 'bar'), KV('baz', 'qux')]);
  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end.
