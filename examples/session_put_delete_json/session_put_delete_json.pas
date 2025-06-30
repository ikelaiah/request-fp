// Example: Session PUT and DELETE with JSON
program session_put_delete_json;

uses Request, Request.Session, fpjson;

var
  Session: THttpSession;
  Response: TResponse;
  Data: TJSONObject;
begin
  Session.Init;
  Session.SetBaseURL('https://httpbin.org');

  // PUT example
  Data := TJSONObject.Create;
  try
    Data.Add('update', 'value');
    Response := Session.Put('/put', Data.AsJSON, 'application/json');
    WriteLn('PUT Status: ', Response.StatusCode);
    WriteLn('PUT Response: ', Response.Text);
  finally
    Data.Free;
  end;

  // DELETE example
  Response := Session.Delete('/delete');
  WriteLn('DELETE Status: ', Response.StatusCode);
  WriteLn('DELETE Response: ', Response.Text);
end.
