program post_json;

uses Request, fpjson;

var
  Response: TResponse;
  UserData, JsonResp: TJSONObject;
begin
  UserData := TJSONObject.Create;
  try
    UserData.Add('name', 'Alice');
    UserData.Add('email', 'alice@example.com');
    Response := Http.PostJSON('https://httpbin.org/post', UserData.AsJSON);
    WriteLn('Status: ', Response.StatusCode);
    JsonResp := TJSONObject(Response.JSON.FindPath('json'));
    if Assigned(JsonResp) then
      WriteLn('Echoed name: ', JsonResp.Get('name', ''));
  finally
    UserData.Free;
  end;
end.
