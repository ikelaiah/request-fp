unit Request.Session;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser,
  Request;

type


  { TSimpleMap }
  {
    @description Simple key-value map for string pairs, used for headers and cookies.

    @usage Use for storing and retrieving HTTP headers or cookies in a session.

    @example

      var 
        Map: TSimpleMap;
        
      Map.SetItem('Key', 'Value');
      if Map.ContainsKey('Key') then ...
      Map.Remove('Key');

    @note No manual memory management required; dynamic array is managed by Pascal.
  }
  TSimpleMap = record
  private
    FItems: array of TKeyValue;
    function FindIndex(const Key: string): Integer;
  public
    { Adds or updates a key-value pair (alias for SetItem) }
    procedure Add(const Key, Value: string);
    { Removes all items }
    procedure Clear;
    { Returns True if the key exists }
    function ContainsKey(const Key: string): Boolean;
    { Gets the value for a key, or returns Default if not found }
    function Get(const Key: string; const Default: string = ''): string;
    { Returns the number of items }
    function GetCount: Integer;
    { Gets the key at the given index }
    function GetKey(Index: Integer): string;
    { Gets the value at the given index }
    function GetValue(Index: Integer): string;
    { Returns the index of a key, or -1 if not found }
    function IndexOf(const Key: string): Integer;
    { Removes a key-value pair by key }
    procedure Remove(const Key: string);
    { Adds or updates a key-value pair }
    procedure SetItem(const Key, Value: string);
  end;

  { THttpSession }
  {
    @description HTTP session with connection pooling, header, and cookie management.

    @usage Use for making multiple HTTP requests with shared headers, cookies, and connection reuse.

    @example

      var 
        Session: THttpSession;

      Session.Init;
      Session.SetBaseURL('https://api.example.com');
      Session.SetHeader('Authorization', 'Bearer ...');
      Response := Session.Get('/endpoint');
      WriteLn('Status: ', Response.StatusCode);

    @note Advanced record: uses Initialize, Finalize, and Copy for safe resource management.
          No manual cleanup needed; memory is managed automatically.
  }
  THttpSession = record
  private
    FClient: TFPHTTPClient;
    FHeaders: TSimpleMap;
    FCookies: TSimpleMap;
    FBaseURL: string;
    FUserAgent: string;
    FTimeout: Integer;
    
    function GetClient: TFPHTTPClient;
    function GetFullURL(const URL: string): string;
    procedure SetupRequest(Client: TFPHTTPClient);
    procedure UpdateCookies(Headers: TStrings);
    
  public
    { Advanced record management: see Initialize, Finalize, and Copy operators below. }
    class operator Initialize(var Session: THttpSession);
    class operator Finalize(var Session: THttpSession);
    class operator Copy(constref Source: THttpSession; var Dest: THttpSession);
    
    { Explicit initialization method (call before use) }
    procedure Init;
    
    { Performs a HTTP GET request }
    function Get(const URL: string): TResponse;
    { Performs a HTTP POST request }
    function Post(const URL: string; const Body: string = ''; 
                 const ContentType: string = 'application/x-www-form-urlencoded'): TResponse;
    { Performs a HTTP PUT request }
    function Put(const URL: string; const Body: string = ''; 
                const ContentType: string = 'application/json'): TResponse;
    { Performs a HTTP DELETE request }
    function Delete(const URL: string): TResponse;
    
    { Sets a header for all requests }
    procedure SetHeader(const Name, Value: string);
    { Sets a cookie for all requests }
    procedure SetCookie(const Name, Value: string);
    { Sets the base URL for relative requests }
    procedure SetBaseURL(const URL: string);
    { Sets the User-Agent header }
    procedure SetUserAgent(const UserAgent: string);
    { Sets the timeout in milliseconds }
    procedure SetTimeout(Timeout: Integer);
    
    { Clears all cookies }
    procedure ClearCookies;
    { Gets a cookie value by name }
    function GetCookie(const Name: string): string;
    
    { Clears all headers (except Accept) }
    procedure ClearHeaders;
  end;

implementation

function ReadStreamAsUTF8String(AStream: TStream): string;
var
  Raw: RawByteString;
  Len: SizeInt;
begin
  Len := AStream.Size;
  SetLength(Raw, Len);
  if Len > 0 then
  begin
    AStream.Position := 0;
    AStream.ReadBuffer(Pointer(Raw)^, Len);
  end;
  SetCodePage(Raw, CP_UTF8, False);
  Result := string(Raw);
end;

{ TSimpleMap }

function TSimpleMap.FindIndex(const Key: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FItems) do
  begin
    if SameText(FItems[I].Key, Key) then // Case-insensitive comparison
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TSimpleMap.Add(const Key, Value: string);
begin
  SetItem(Key, Value);
end;

procedure TSimpleMap.Clear;
begin
  SetLength(FItems, 0);
end;

function TSimpleMap.ContainsKey(const Key: string): Boolean;
begin
  Result := FindIndex(Key) >= 0;
end;

function TSimpleMap.Get(const Key: string; const Default: string): string;
var
  Index: Integer;
begin
  Index := FindIndex(Key);
  if Index >= 0 then
    Result := FItems[Index].Value
  else
    Result := Default;
end;

function TSimpleMap.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TSimpleMap.GetKey(Index: Integer): string;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[Index].Key
  else
    Result := '';
end;

function TSimpleMap.GetValue(Index: Integer): string;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[Index].Value
  else
    Result := '';
end;

function TSimpleMap.IndexOf(const Key: string): Integer;
begin
  Result := FindIndex(Key);
end;

procedure TSimpleMap.Remove(const Key: string);
var
  Index, I: Integer;
begin
  Index := FindIndex(Key);
  if Index >= 0 then
  begin
    // Remove item by shifting remaining items left
    for I := Index to Length(FItems) - 2 do
      FItems[I] := FItems[I + 1];
    SetLength(FItems, Length(FItems) - 1);
  end;
end;

procedure TSimpleMap.SetItem(const Key, Value: string);
var
  Index: Integer;
begin
  Index := FindIndex(Key);
  if Index >= 0 then
  begin
    // Update existing item
    FItems[Index].Value := Value;
  end
  else
  begin
    // Add new item
    SetLength(FItems, Length(FItems) + 1);
    FItems[High(FItems)].Key := Key;
    FItems[High(FItems)].Value := Value;
  end;
end;

{ THttpSession }

class operator THttpSession.Initialize(var Session: THttpSession);
begin
  Session.FClient := nil;
  Session.FBaseURL := '';
  Session.FUserAgent := DEFAULT_USER_AGENT;
  Session.FTimeout := 30000; // 30 seconds default timeout

  // Set default headers - TSimpleMap doesn't need explicit initialization
  Session.FHeaders.SetItem('Accept', 'application/json');
  // Do NOT set Content-Type here; set per-request
end;

class operator THttpSession.Finalize(var Session: THttpSession);
begin
  if Assigned(Session.FClient) then
    FreeAndNil(Session.FClient);
  // TSimpleMap uses dynamic arrays and doesn't need manual cleanup
end;

class operator THttpSession.Copy(constref Source: THttpSession; var Dest: THttpSession);
var
  I: Integer;
begin
  // Skip if source and destination are the same
  if @Source = @Dest then
    Exit;
    
  // Copy simple fields
  Dest.FBaseURL := Source.FBaseURL;
  Dest.FUserAgent := Source.FUserAgent;
  Dest.FTimeout := Source.FTimeout;
  
  // Copy headers
  Dest.FHeaders.Clear;
  for I := 0 to Source.FHeaders.GetCount - 1 do
    Dest.FHeaders.SetItem(Source.FHeaders.GetKey(I), Source.FHeaders.GetValue(I));
    
  // Copy cookies
  Dest.FCookies.Clear;
  for I := 0 to Source.FCookies.GetCount - 1 do
    Dest.FCookies.SetItem(Source.FCookies.GetKey(I), Source.FCookies.GetValue(I));
  
  // Don't copy the client - it will be created on demand
  if Dest.FClient <> nil then
    FreeAndNil(Dest.FClient);
  Dest.FClient := nil;
end;

function THttpSession.GetClient: TFPHTTPClient;
begin
  // Defensive programming - initialize fields that might be accessed
  if Self.FTimeout <= 0 then
    Self.FTimeout := 30000;
    
  if Self.FUserAgent = '' then
    Self.FUserAgent := DEFAULT_USER_AGENT;
    
  // Create client if needed
  if Self.FClient = nil then
  begin
    Self.FClient := TFPHTTPClient.Create(nil);
    Self.FClient.AllowRedirect := True;
    Self.FClient.IOTimeout := Self.FTimeout;
  end;
  Result := Self.FClient;
end;

function THttpSession.GetFullURL(const URL: string): string;
begin
  if (Pos('http://', LowerCase(URL)) = 1) or (Pos('https://', LowerCase(URL)) = 1) then
    Result := URL
  else
    Result := FBaseURL + URL;
end;

procedure THttpSession.SetupRequest(Client: TFPHTTPClient);
var
  I: Integer;
  CookieStr: string;
begin
  // Clear any existing headers to prevent conflicts
  Client.RequestHeaders.Clear;
  
  // Set headers
  for I := 0 to FHeaders.GetCount - 1 do
    Client.AddHeader(FHeaders.GetKey(I), FHeaders.GetValue(I));

  // Handle cookies - AddHeader will override any existing header with the same name
  if FCookies.GetCount > 0 then
  begin
    CookieStr := '';
    for I := 0 to FCookies.GetCount - 1 do
    begin
      if I > 0 then
        CookieStr := CookieStr + '; ';
      CookieStr := CookieStr + FCookies.GetKey(I) + '=' + FCookies.GetValue(I);
    end;
    Client.AddHeader('Cookie', CookieStr);
  end;
  
  // Set timeout
  Client.IOTimeout := FTimeout;

  // Ensure User-Agent is present (session default unless overridden)
  if Client.RequestHeaders.IndexOfName('User-Agent') = -1 then
    Client.AddHeader('User-Agent', FUserAgent);
end;

procedure THttpSession.UpdateCookies(Headers: TStrings);
var
  I: Integer;
  CookieHeader: string;
  Key, Value: string;
  P: Integer;
begin
  for I := 0 to Headers.Count - 1 do
  begin
    if LowerCase(Headers.Names[I]) = 'set-cookie' then
    begin
      CookieHeader := Trim(Headers.ValueFromIndex[I]);
      P := Pos(';', CookieHeader);
      if P > 0 then
        CookieHeader := Copy(CookieHeader, 1, P - 1);
      CookieHeader := Trim(CookieHeader);
      P := Pos('=', CookieHeader);
      if P > 0 then
      begin
        Key := Trim(Copy(CookieHeader, 1, P - 1));
        Value := Trim(Copy(CookieHeader, P + 1, MaxInt));
        FCookies.SetItem(Key, Value);
      end;
    end;
  end;
end;

function THttpSession.Get(const URL: string): TResponse;
var
  Client: TFPHTTPClient;
  ResponseStream: TMemoryStream;
  Headers: TStringList;
begin
  Client := GetClient;
  SetupRequest(Client);
  
  ResponseStream := TMemoryStream.Create;
  Headers := TStringList.Create;
  try
    try
      // Make the request using our configured client
      Client.Get(GetFullURL(URL), ResponseStream);
      
      // Set the response status code
      Result.StatusCode := Client.ResponseStatusCode;
      // Always set content, regardless of Content-Type
      Result.SetContent(ReadStreamAsUTF8String(ResponseStream));
      // Update cookies from response
      UpdateCookies(Client.ResponseHeaders);
    except
      on E: Exception do
        raise ERequestError.Create('GET request failed: ' + E.Message);
    end;
  finally
    ResponseStream.Free;
    Headers.Free;
  end;
end;

function THttpSession.Post(const URL: string; const Body: string; const ContentType: string): TResponse;
var
  Client: TFPHTTPClient;
  RequestBody: TStringStream;
  ResponseStream: TMemoryStream;
begin
  Client := GetClient;
  SetupRequest(Client);

  // Set Content-Type header
  if ContentType <> '' then
    Client.AddHeader('Content-Type', ContentType);

  RequestBody := TStringStream.Create(Body);
  ResponseStream := TMemoryStream.Create;
  try
    try
      Client.RequestBody := RequestBody;
      Client.HTTPMethod('POST', GetFullURL(URL), ResponseStream, [200, 201, 204]);
      Result.StatusCode := Client.ResponseStatusCode;
      Result.SetContent(ReadStreamAsUTF8String(ResponseStream));
      UpdateCookies(Client.ResponseHeaders);
    finally
      Client.RequestBody := nil;
    end;
  finally
    RequestBody.Free;
    ResponseStream.Free;
  end;
end;

function THttpSession.Put(const URL: string; const Body: string; 
                        const ContentType: string): TResponse;
var
  Client: TFPHTTPClient;
  RequestBody: TStringStream;
  ResponseStream: TMemoryStream;
begin
  Client := GetClient;
  SetupRequest(Client);
  
  // Set content type
  if ContentType <> '' then
    Client.AddHeader('Content-Type', ContentType);
  
  RequestBody := TStringStream.Create(Body);
  ResponseStream := TMemoryStream.Create;
  try
    try
      // Make the request
      Client.RequestBody := RequestBody;
      try
        Client.HTTPMethod('PUT', GetFullURL(URL), ResponseStream, [200, 201, 204]);
      finally
        Client.RequestBody := nil;
      end;
      
      // Set the response status code
      Result.StatusCode := Client.ResponseStatusCode;
      // Always set content, regardless of Content-Type
      Result.SetContent(ReadStreamAsUTF8String(ResponseStream));
      // Update cookies from response
      UpdateCookies(Client.ResponseHeaders);
    except
      on E: Exception do
        raise ERequestError.Create('PUT request failed: ' + E.Message);
    end;
  finally
    RequestBody.Free;
    ResponseStream.Free;
  end;
end;

function THttpSession.Delete(const URL: string): TResponse;
var
  Client: TFPHTTPClient;
  ResponseStream: TMemoryStream;
begin
  Client := GetClient;
  SetupRequest(Client);
  
  ResponseStream := TMemoryStream.Create;
  try
    try
      // Make the request
      Client.HTTPMethod('DELETE', GetFullURL(URL), ResponseStream, [200, 204]);
      
      // Set the response status code
      Result.StatusCode := Client.ResponseStatusCode;
      // Always set content, regardless of Content-Type
      Result.SetContent(ReadStreamAsUTF8String(ResponseStream));
      // Update cookies from response
      UpdateCookies(Client.ResponseHeaders);
    except
      on E: Exception do
        raise ERequestError.Create('DELETE request failed: ' + E.Message);
    end;
  finally
    ResponseStream.Free;
  end;
end;

// Session Configuration

procedure THttpSession.SetHeader(const Name, Value: string);
begin
  FHeaders.SetItem(Name, Value);
end;

procedure THttpSession.SetCookie(const Name, Value: string);
begin
  FCookies.SetItem(Name, Value);
end;

procedure THttpSession.SetBaseURL(const URL: string);
begin
  FBaseURL := URL;
end;

procedure THttpSession.SetUserAgent(const UserAgent: string);
begin
  FUserAgent := UserAgent;
end;

procedure THttpSession.SetTimeout(Timeout: Integer);
begin
  FTimeout := Timeout;
  if FClient <> nil then
    FClient.IOTimeout := Timeout;
end;

// Cookie Management

procedure THttpSession.ClearCookies;
var
  idx: Integer;
begin
  FCookies.Clear;
  if Assigned(FClient) then
  begin
    if Assigned(FClient.RequestHeaders) then
    begin
      idx := FClient.RequestHeaders.IndexOfName('Cookie');
      if idx >= 0 then
      begin
        FClient.RequestHeaders.Delete(idx);
      end;
    end;
  end;
end;

function THttpSession.GetCookie(const Name: string): string;
begin
  Result := FCookies.Get(Name);
end;

// Header Management

procedure THttpSession.ClearHeaders;
begin
  FHeaders.Clear;
  FHeaders.SetItem('Accept', 'application/json');
  // Do NOT set Content-Type here; set per-request
end;

procedure THttpSession.Init;
begin
  // Explicitly initialize the session record
  Self.FClient := nil;
  Self.FBaseURL := '';
  Self.FUserAgent := 'Request-FP/1.0';
  Self.FTimeout := 30000; // 30 seconds default timeout

  // FHeaders and FCookies will self-initialize when their methods are called
  // since we added defensive nil checks to all TSimpleMap methods

  // Set default headers
  Self.FHeaders.SetItem('Accept', 'application/json');
end;

end.
