program custom_headers_params;

uses Request;

var
  Response: TResponse;
begin
  Response := THttpRequest
    .Create
    .Get
    .URL('https://httpbin.org/get')
    .AddHeader('X-Custom-Header', 'RequestFP')
    .AddParam('foo', 'bar')
    .AddParam('baz', 'qux')
    .Send;

  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end.
