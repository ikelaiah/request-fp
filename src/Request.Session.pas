unit Request.Session;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser,
  Request;

type
  { TKeyValue - Simple key-value pair }
  TKeyValue = record
    Key: string;
    Value: string;
  end;

  { TSimpleMap - Simple string map using dynamic arrays }
  TSimpleMap = record
  private
    FItems: array of TKeyValue;
    function FindIndex(const Key: string): Integer;
  public
    procedure Add(const Key, Value: string);
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function Get(const Key: string; const Default: string = ''): string;
    function GetCount: Integer;
    function GetKey(Index: Integer): string;
    function GetValue(Index: Integer): string;
    function IndexOf(const Key: string): Integer;
    procedure Remove(const Key: string);
    procedure SetItem(const Key, Value: string);
  end;

  { THttpSession }
  // HTTP session with connection pooling and cookie handling
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
    class operator Initialize(var Session: THttpSession);
    class operator Finalize(var Session: THttpSession);
    class operator Copy(constref Source: THttpSession; var Dest: THttpSession);
    
    // New explicit initialization method that can be called from tests
    procedure Init;
    
    // HTTP Methods
    function Get(const URL: string): TResponse;
    function Post(const URL: string; const Body: string = ''; 
                 const ContentType: string = 'application/x-www-form-urlencoded'): TResponse;
    function Put(const URL: string; const Body: string = ''; 
                const ContentType: string = 'application/json'): TResponse;
    function Delete(const URL: string): TResponse;
    
    // Session Configuration
    procedure SetHeader(const Name, Value: string);
    procedure SetCookie(const Name, Value: string);
    procedure SetBaseURL(const URL: string);
    procedure SetUserAgent(const UserAgent: string);
    procedure SetTimeout(Timeout: Integer);
    
    // Cookie Management
    procedure ClearCookies;
    function GetCookie(const Name: string): string;
    
    // Header Management
    procedure ClearHeaders;
  end;

implementation

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
  Session.FUserAgent := 'Request-FP/1.0';
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
    Self.FUserAgent := 'Request-FP/1.0';
    
  // Create client if needed
  if Self.FClient = nil then
  begin
    Self.FClient := TFPHTTPClient.Create(nil);
    Self.FClient.AllowRedirect := True;
    Self.FClient.IOTimeout := Self.FTimeout;
    Self.FClient.AddHeader('User-Agent', Self.FUserAgent);
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

  // Preserve Content-Type if already set
  if Client.RequestHeaders.IndexOfName('Content-Type') = -1 then
    Client.RequestHeaders.Values['Content-Type'] := 'application/x-www-form-urlencoded';
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
  Response: TStringStream;
  Headers: TStringList;
begin
  Client := GetClient;
  SetupRequest(Client);
  
  Response := TStringStream.Create('');
  Headers := TStringList.Create;
  try
    try
      // Make the request using our configured client
      Client.Get(GetFullURL(URL), Response);
      
      // Set the response status code
      Result.StatusCode := Client.ResponseStatusCode;
      // Always set content, regardless of Content-Type
      Result.SetContent(Response.DataString);
      // Update cookies from response
      UpdateCookies(Client.ResponseHeaders);
    except
      on E: Exception do
        raise ERequestError.Create('GET request failed: ' + E.Message);
    end;
  finally
    Response.Free;
    Headers.Free;
  end;
end;

function THttpSession.Post(const URL: string; const Body: string; const ContentType: string): TResponse;
var
  Client: TFPHTTPClient;
  RequestBody: TStringStream;
  ResponseStream: TStringStream;
begin
  Client := GetClient;
  SetupRequest(Client);

  // Set Content-Type header
  if ContentType <> '' then
    Client.AddHeader('Content-Type', ContentType);

  RequestBody := TStringStream.Create(Body);
  ResponseStream := TStringStream.Create('');
  try
    try
      Client.RequestBody := RequestBody;
      Client.HTTPMethod('POST', GetFullURL(URL), ResponseStream, [200, 201, 204]);
      Result.StatusCode := Client.ResponseStatusCode;
      Result.SetContent(ResponseStream.DataString);
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
  Response: TStringStream;
begin
  Client := GetClient;
  SetupRequest(Client);
  
  // Set content type
  if ContentType <> '' then
    Client.AddHeader('Content-Type', ContentType);
  
  RequestBody := TStringStream.Create(Body);
  Response := TStringStream.Create('');
  try
    try
      // Make the request
      Client.RequestBody := RequestBody;
      try
        Client.HTTPMethod('PUT', GetFullURL(URL), Response, [200, 201, 204]);
      finally
        Client.RequestBody := nil;
      end;
      
      // Set the response status code
      Result.StatusCode := Client.ResponseStatusCode;
      // Always set content, regardless of Content-Type
      Result.SetContent(Response.DataString);
      // Update cookies from response
      UpdateCookies(Client.ResponseHeaders);
    except
      on E: Exception do
        raise ERequestError.Create('PUT request failed: ' + E.Message);
    end;
  finally
    RequestBody.Free;
    Response.Free;
  end;
end;

function THttpSession.Delete(const URL: string): TResponse;
var
  Client: TFPHTTPClient;
  Response: TStringStream;
begin
  Client := GetClient;
  SetupRequest(Client);
  
  Response := TStringStream.Create('');
  try
    try
      // Make the request
      Client.HTTPMethod('DELETE', GetFullURL(URL), Response, [200, 204]);
      
      // Set the response status code
      Result.StatusCode := Client.ResponseStatusCode;
      // Always set content, regardless of Content-Type
      Result.SetContent(Response.DataString);
      // Update cookies from response
      UpdateCookies(Client.ResponseHeaders);
    except
      on E: Exception do
        raise ERequestError.Create('DELETE request failed: ' + E.Message);
    end;
  finally
    Response.Free;
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
  // since we added defensive nil checks to all TStringMap methods

  // Set default headers
  Self.FHeaders.SetItem('Accept', 'application/json');
  Self.FHeaders.SetItem('Content-Type', 'application/json');
end;

end.
