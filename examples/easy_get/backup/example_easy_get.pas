// Example 1: Easy use - Simple GET request
program example_easy_get;

uses Request;

var
  Response: TResponse;
begin
  Response := Http.Get('https://httpbin.org/get');
  WriteLn('Status: ', Response.StatusCode);
  WriteLn('Body: ', Response.Text);
end.
