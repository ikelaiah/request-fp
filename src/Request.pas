unit Request;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, openssl,
  HTTPDefs, sockets, fpjson, jsonparser, jsonscanner,
  httpprotocol
  {$IFDEF UNIX}, BaseUnix{$ENDIF}
  {$IFDEF WINDOWS}, Windows{$ENDIF};

const
  REQUEST_FP_VERSION = '1.4.0';
  DEFAULT_USER_AGENT = 'Request-FP/' + REQUEST_FP_VERSION;
  {$IFDEF DEBUG}
    DEBUG_MODE = True;
  {$ELSE}
    DEBUG_MODE = False;
  {$ENDIF}

type
  { Base class for every error raised by Request-FP. }
  ERequestError = class(Exception);
  { Connection, DNS, socket, or TLS transport failure. }
  ENetworkError = class(ERequestError);
  { Connect timeout, or a read/write failure after the I/O timeout elapsed. }
  ETimeoutError = class(ENetworkError);

  { Streaming progress: bytes written so far and the expected total, which is
    -1 when the server sends no Content-Length. Raise an exception to cancel. }
  TProgressProc = procedure(Received, Total: Int64);
  TProgressEvent = procedure(Received, Total: Int64) of object;

  { TKeyValue - Simple key-value pair }
  TKeyValue = record
    Key: string;
    Value: string;
    
    { Helper constructor for creating key-value pairs }
    class function Create(const AKey, AValue: string): TKeyValue; static;
  end;

  { Response record with automatic memory management }
  TResponse = record
  private
    FContent: string;
    FHeaders: string;
    FJSON: TJSONData;
    function GetOK: Boolean;
    
    {
      @description Returns the content body of the HTTP response as text
      
      @usage Use when you need to access the raw text content of the response
      
      @returns String containing the raw response body
    }
    function GetText: string;
    
    {
      @description Returns the content body of the HTTP response parsed as JSON
      
      @usage Use when you expect a JSON response and need to work with it as a JSON object
      
      @returns IJSONValue interface to the parsed JSON structure
      
      @warning Raises ERequestError if the content cannot be parsed as valid JSON
    }
    function GetJSON: TJSONData;
  public
    StatusCode: Integer;
    
    property Text: string read GetText;
    property JSON: TJSONData read GetJSON;
    { True when the response status is in the 200..299 range. }
    property OK: Boolean read GetOK;

    // Returns the value of a response header (case-insensitive), or empty string if not found
    function HeaderValue(const Name: string): string;
    // Sets raw header text captured from the HTTP client (for use by session API)
    procedure SetHeadersText(const AHeaders: string);
    // True if status is within 200..299
    function IsSuccessStatus: Boolean;
    // Raises ERequestError when status is outside 200..299
    procedure RaiseForStatus;
    // Saves the response body bytes to a file unchanged (safe for binary data)
    procedure SaveToFile(const FilePath: string);
    // Writes the response body bytes to Stream unchanged
    procedure SaveToStream(Stream: TStream);
    // Returns a copy of the response body as raw bytes
    function Bytes: TBytes;
    
    { Helper method to set the response content and JSON data }
    procedure SetContent(const AContent: string; AJSON: TJSONData = nil);
    
    { Management operators for automatic initialization/cleanup }
    
    {
      @description Initializes a TResponse record with default values
      
      @usage Called automatically when a TResponse is created:
             - When a variable of this type is declared
             - When memory for this type is allocated
             - When this type is used as a field in another record
      
      @warning No need to call this manually; Free Pascal handles this automatically
    }
    class operator Initialize(var Response: TResponse);
    
    {
      @description Finalizes a TResponse record, releasing resources
      
      @usage Called automatically when:
             - A variable goes out of scope
             - Memory for this type is freed
             - The containing record is finalized
      
      @warning No need to call this manually; Free Pascal handles this automatically
    }
    class operator Finalize(var Response: TResponse);
    
    {
      @description Copies a TResponse record, performing a deep copy of fields
      
      @usage Called automatically when:
             - A TResponse is assigned to another TResponse
             - A TResponse is passed by value to a function or method
      
      @warning No need to call this manually; Free Pascal handles this automatically
    }
    class operator Copy(constref Source: TResponse; var Dest: TResponse);
  end;
  
  {
    @description Structured result type that includes both success/error info and the response
    
    @usage Use for error handling when making HTTP requests where you want to handle
           potential failures gracefully without exceptions
  }
  TRequestResult = record
  private
    function GetOK: Boolean;
  public
    Success: Boolean;
    Response: TResponse;  // Will be automatically initialized and finalized
    Error: string;
    { True only when transport succeeded and the response status is 2xx. }
    property OK: Boolean read GetOK;
  end;

  { Writes a download to FilePath + '.part'. Commit replaces FilePath with it;
    freeing without Commit deletes the partial file, so FilePath is never left
    half-written. Used by Download; applications rarely need it directly. }
  TDownloadFile = class
  private
    FFilePath: string;
    FPartPath: string;
    FStream: TFileStream;
    FCommitted: Boolean;
  public
    { Raises ERequestError if the partial file cannot be created. }
    constructor Create(const AFilePath: string);
    destructor Destroy; override;
    { Replaces FilePath with the partial file. Raises ERequestError on failure. }
    procedure Commit;
    property Stream: TFileStream read FStream;
  end;

  { Global HTTP functions }
  THttp = record
    {
      @description Performs a simple HTTP GET request
      
      @usage Use for quick GET requests without needing to configure options
      
      @param URL The URL to send the GET request to
      
      @returns TResponse containing the result of the HTTP request
      
      @warning May raise ERequestError for network or protocol errors
      
      @example
        var
          Response: TResponse;
        begin
          Response := THttp.Get('https://api.example.com/users');
          // Or using the global constant:
          Response := Http.Get('https://api.example.com/users');
          
          WriteLn('Status: ', Response.StatusCode);
          WriteLn('Body: ', Response.Text);
        end;
    }
    class function Get(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse; static;
    
    {
      @description Performs a simple HTTP POST request
      
      @usage Use for quick POST requests without needing to configure many options
      
      @param URL The URL to send the POST request to
      @param Data Optional form data to include in the request body
      
      @returns TResponse containing the result of the HTTP request
      
      @warning May raise ERequestError for network or protocol errors.
               Sets Content-Type to application/x-www-form-urlencoded if Data is provided.
      
      @example
        var
          Response: TResponse;
        begin
          Response := THttp.Post('https://api.example.com/users', 'name=John&age=30');
          // Or using the global constant:
          Response := Http.Post('https://api.example.com/users', 'name=John&age=30');
          
          WriteLn('Status: ', Response.StatusCode);
        end;
    }
    class function Post(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse; static;
    
    {
      @description Performs a simple HTTP PUT request
      
      @usage Use for quick PUT requests without needing to configure many options
      
      @param URL The URL to send the PUT request to
      @param Data Optional form data to include in the request body
      
      @returns TResponse containing the result of the HTTP request
      
      @warning May raise ERequestError for network or protocol errors.
               Sets Content-Type to application/x-www-form-urlencoded if Data is provided.
      
      @example
        var
          Response: TResponse;
        begin
          Response := THttp.Put('https://api.example.com/users/1', 'name=John&age=30');
          // Or using the global constant:
          Response := Http.Put('https://api.example.com/users/1', 'name=John&age=30');
          
          WriteLn('Status: ', Response.StatusCode);
        end;
    }
    class function Put(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse; static;
    
    {
      @description Performs a simple HTTP DELETE request
      
      @usage Use for quick DELETE requests without needing to configure options
      
      @param URL The URL to send the DELETE request to
      
      @returns TResponse containing the result of the HTTP request
      
      @warning May raise ERequestError for network or protocol errors
      
      @example
        var
          Response: TResponse;
        begin
          Response := THttp.Delete('https://api.example.com/users/1');
          // Or using the global constant:
          Response := Http.Delete('https://api.example.com/users/1');
          
          WriteLn('Status: ', Response.StatusCode);
        end;
    }
    class function Delete(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse; static;
    
    {
      @description Performs a simple HTTP POST request with JSON data
      
      @usage Use for quick POST requests with JSON data without configuring many options
      
      @param URL The URL to send the POST request to
      @param JSON The JSON string to send in the request body
      
      @returns TResponse containing the result of the HTTP request
      
      @warning May raise ERequestError for network or protocol errors.
               Automatically sets Content-Type to application/json.
      
      @example
        var
          Response: TResponse;
          JSON: string;
        begin
          JSON := '{"name":"John","age":30}';
          Response := THttp.PostJSON('https://api.example.com/users', JSON);
          // Or using the global constant:
          Response := Http.PostJSON('https://api.example.com/users', JSON);
          
          WriteLn('Status: ', Response.StatusCode);
        end;
    }
    class function PostJSON(const URL: string; const JSON: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse; static;
    class function PostJSON(const URL: string; const JSON: TJSONData; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse; static; overload;

    { Encodes Fields as application/x-www-form-urlencoded and posts them. }
    class function PostForm(const URL: string; const Fields: array of TKeyValue; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse; static;
    
    {
      @description Performs a HTTP GET request with error handling
      
      @usage Use when you want to handle errors without exceptions
      
      @param URL The URL to send the GET request to
      
      @returns TRequestResult containing success flag, response, and error message
      
      @warning Never raises exceptions, even for network errors. Check the Success
               flag and Error string to handle failures.
      
      @example
        var
          Result: TRequestResult;
        begin
          Result := THttp.TryGet('https://api.example.com/users');
          // Or using the global constant:
          Result := Http.TryGet('https://api.example.com/users');
          
          if Result.Success then
            WriteLn('Success: ', Result.Response.Text)
          else
            WriteLn('Error: ', Result.Error);
        end;
    }
    class function TryGet(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult; static;
    
    {
      @description Performs a HTTP POST request with error handling
      
      @usage Use when you want to handle errors without exceptions
      
      @param URL The URL to send the POST request to
      @param Data Optional form data to include in the request body
      
      @returns TRequestResult containing success flag, response, and error message
      
      @warning Never raises exceptions, even for network errors. Check the Success
               flag and Error string to handle failures.
      
      @example
        var
          Result: TRequestResult;
        begin
          Result := THttp.TryPost('https://api.example.com/users', 'name=John&age=30');
          // Or using the global constant:
          Result := Http.TryPost('https://api.example.com/users', 'name=John&age=30');
          
          if Result.Success then
            WriteLn('Success: ', Result.Response.StatusCode)
          else
            WriteLn('Error: ', Result.Error);
        end;
    }
    class function TryPost(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult; static;
    class function TryPostForm(const URL: string; const Fields: array of TKeyValue; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult; static;
    class function TryPostJSON(const URL: string; const JSON: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult; static; overload;
    class function TryPostJSON(const URL: string; const JSON: TJSONData; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult; static; overload;
    {
      @description Performs a HTTP PUT request with error handling
    }
    class function TryPut(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult; static;
    {
      @description Performs a HTTP DELETE request with error handling
    }
    class function TryDelete(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult; static;
    
    {
      @description Performs a simple HTTP POST request with multipart/form-data
      
      @usage Use for quick POST requests with file uploads without configuring many options
      
      @param URL The URL to send the POST request to
      @param Fields Optional form fields to include in the request
      @param Files Optional files to upload
      
      @returns TResponse containing the result of the HTTP request
      
      @warning May raise ERequestError for network or protocol errors.
               Automatically sets Content-Type to multipart/form-data.
      
      @example
        var
          Response: TResponse;
        begin
          Response := THttp.PostMultipart('https://api.example.com/upload',
                                         ['field1=value1', 'field2=value2'],
                                         ['file1=/path/to/file1', 'file2=/path/to/file2']);
          // Or using the global constant:
          Response := Http.PostMultipart('https://api.example.com/upload',
                                         ['field1=value1', 'field2=value2'],
                                         ['file1=/path/to/file1', 'file2=/path/to/file2']);
          
          WriteLn('Status: ', Response.StatusCode);
        end;
    }
    class function PostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse; static;
    
    // Ergonomic overloads for procedural API
    class function Get(const URL: string): TResponse; static; overload;
    class function Get(const URL: string; const Headers: array of TKeyValue): TResponse; static; overload;
    class function GetWithParams(const URL: string; const Params: array of TKeyValue): TResponse; static;
    class function Post(const URL: string; const Data: string): TResponse; static; overload;
    class function Post(const URL: string; const Data: string; const Headers: array of TKeyValue): TResponse; static; overload;
    class function PostForm(const URL: string; const Fields: array of TKeyValue): TResponse; static; overload;
    class function PostForm(const URL: string; const Fields: array of TKeyValue; const Headers: array of TKeyValue): TResponse; static; overload;
    class function Put(const URL: string; const Data: string): TResponse; static; overload;
    class function Put(const URL: string; const Data: string; const Headers: array of TKeyValue): TResponse; static; overload;
    class function Delete(const URL: string): TResponse; static; overload;
    class function Delete(const URL: string; const Headers: array of TKeyValue): TResponse; static; overload;
    class function PostJSON(const URL: string; const JSON: string): TResponse; static; overload;
    class function PostJSON(const URL: string; const JSON: string; const Headers: array of TKeyValue): TResponse; static; overload;
    class function PostJSON(const URL: string; const JSON: TJSONData): TResponse; static; overload;
    class function PostJSON(const URL: string; const JSON: TJSONData; const Headers: array of TKeyValue): TResponse; static; overload;
    // Ergonomic overloads for TryGet and TryPost
    class function TryGet(const URL: string): TRequestResult; static; overload;
    class function TryGet(const URL: string; const Headers: array of TKeyValue): TRequestResult; static; overload;
    class function TryGetWithParams(const URL: string; const Params: array of TKeyValue): TRequestResult; static;
    class function TryPost(const URL: string; const Data: string): TRequestResult; static; overload;
    class function TryPost(const URL: string; const Data: string; const Headers: array of TKeyValue): TRequestResult; static; overload;
    class function TryPostForm(const URL: string; const Fields: array of TKeyValue): TRequestResult; static; overload;
    class function TryPostForm(const URL: string; const Fields: array of TKeyValue; const Headers: array of TKeyValue): TRequestResult; static; overload;
    class function TryPostJSON(const URL: string; const JSON: string): TRequestResult; static; overload;
    class function TryPostJSON(const URL: string; const JSON: string; const Headers: array of TKeyValue): TRequestResult; static; overload;
    class function TryPostJSON(const URL: string; const JSON: TJSONData): TRequestResult; static; overload;
    class function TryPostJSON(const URL: string; const JSON: TJSONData; const Headers: array of TKeyValue): TRequestResult; static; overload;
    class function TryPut(const URL: string; const Data: string): TRequestResult; static; overload;
    class function TryPut(const URL: string; const Data: string; const Headers: array of TKeyValue): TRequestResult; static; overload;
    class function TryDelete(const URL: string): TRequestResult; static; overload;
    class function TryDelete(const URL: string; const Headers: array of TKeyValue): TRequestResult; static; overload;
    // TryPostMultipart variants
    class function TryPostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult; static;
    // Ergonomic overloads for PostMultipart
    class function PostMultipart(const URL: string; const Fields, Files: array of TKeyValue): TResponse; static; overload;
    class function PostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue): TResponse; static; overload;
    // Ergonomic overloads for TryPostMultipart
    class function TryPostMultipart(const URL: string; const Fields, Files: array of TKeyValue): TRequestResult; static; overload;
    class function TryPostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue): TRequestResult; static; overload;

    { PATCH sends Data like Put. }
    class function Patch(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse; static; overload;
    class function Patch(const URL: string; const Data: string): TResponse; static; overload;
    class function Patch(const URL: string; const Data: string; const Headers: array of TKeyValue): TResponse; static; overload;
    class function TryPatch(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult; static; overload;
    class function TryPatch(const URL: string; const Data: string): TRequestResult; static; overload;
    class function TryPatch(const URL: string; const Data: string; const Headers: array of TKeyValue): TRequestResult; static; overload;

    { HEAD returns status and headers only; Text is empty. }
    class function Head(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse; static; overload;
    class function Head(const URL: string): TResponse; static; overload;
    class function Head(const URL: string; const Headers: array of TKeyValue): TResponse; static; overload;
    class function TryHead(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult; static; overload;
    class function TryHead(const URL: string): TRequestResult; static; overload;
    class function TryHead(const URL: string; const Headers: array of TKeyValue): TRequestResult; static; overload;

    { Streams a GET response to FilePath without holding it in memory.
      A 2xx body goes to FilePath + '.part', which replaces FilePath once
      complete; Response.Text is then empty. Any other status leaves FilePath
      untouched and puts the body in Response.Text. A failed or cancelled
      transfer deletes the partial file. }
    class function Download(const URL, FilePath: string): TResponse; static; overload;
    class function Download(const URL, FilePath: string; OnProgress: TProgressProc): TResponse; static; overload;
    class function Download(const URL, FilePath: string; OnProgress: TProgressEvent): TResponse; static; overload;
    class function Download(const URL, FilePath: string; const Headers, Params: array of TKeyValue): TResponse; static; overload;
    class function Download(const URL, FilePath: string; const Headers, Params: array of TKeyValue; OnProgress: TProgressProc): TResponse; static; overload;
    class function Download(const URL, FilePath: string; const Headers, Params: array of TKeyValue; OnProgress: TProgressEvent): TResponse; static; overload;
    class function TryDownload(const URL, FilePath: string): TRequestResult; static; overload;
    class function TryDownload(const URL, FilePath: string; OnProgress: TProgressProc): TRequestResult; static; overload;
    class function TryDownload(const URL, FilePath: string; OnProgress: TProgressEvent): TRequestResult; static; overload;
    class function TryDownload(const URL, FilePath: string; const Headers, Params: array of TKeyValue): TRequestResult; static; overload;
    class function TryDownload(const URL, FilePath: string; const Headers, Params: array of TKeyValue; OnProgress: TProgressProc): TRequestResult; static; overload;
    class function TryDownload(const URL, FilePath: string; const Headers, Params: array of TKeyValue; OnProgress: TProgressEvent): TRequestResult; static; overload;

    { Streams a GET response into Stream (written at its current position).
      Same rules as Download: only a 2xx body is written to Stream; any other
      body is returned in Response.Text. Bytes already written stay in Stream
      if the transfer fails. The caller owns Stream. }
    class function GetToStream(const URL: string; Stream: TStream): TResponse; static; overload;
    class function GetToStream(const URL: string; Stream: TStream; OnProgress: TProgressProc): TResponse; static; overload;
    class function GetToStream(const URL: string; Stream: TStream; OnProgress: TProgressEvent): TResponse; static; overload;
    class function GetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue): TResponse; static; overload;
    class function GetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue; OnProgress: TProgressProc): TResponse; static; overload;
    class function GetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue; OnProgress: TProgressEvent): TResponse; static; overload;
    class function TryGetToStream(const URL: string; Stream: TStream): TRequestResult; static; overload;
    class function TryGetToStream(const URL: string; Stream: TStream; OnProgress: TProgressProc): TRequestResult; static; overload;
    class function TryGetToStream(const URL: string; Stream: TStream; OnProgress: TProgressEvent): TRequestResult; static; overload;
    class function TryGetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue): TRequestResult; static; overload;
    class function TryGetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue; OnProgress: TProgressProc): TRequestResult; static; overload;
    class function TryGetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue; OnProgress: TProgressEvent): TRequestResult; static; overload;

    { I/O timeout in milliseconds for all Http requests (default 30000).
      0 waits forever. Sessions have their own SetTimeout. }
    class procedure SetTimeout(Milliseconds: Integer); static;
    class function GetTimeout: Integer; static;
  end;

{ Short key/value helper for headers, query parameters, forms, and multipart data. }
function KV(const Key, Value: string): TKeyValue;

{ Loads OpenSSL if it is not loaded yet. HTTPS requests call this
  automatically; call it at startup to fail fast on a missing OpenSSL.
  Raises ERequestError with setup hints on failure. }
procedure EnsureSSL;

{ Building blocks shared with Request.Session. Applications normally use
  Http or THttpSession instead. }

{ Runs one request on Client (any status is returned) and maps transport
  failures to ERequestError, ENetworkError, or ETimeoutError. }
function ExecuteRequest(Client: TFPHTTPClient; const Method, URL: string;
  Body: TStream): TResponse; overload;
{ Like ExecuteRequest, but writes a 2xx response body to Target and reports
  progress (either callback may be nil); Response.Text is then empty. Any
  other body stays in Response.Text. }
function ExecuteRequest(Client: TFPHTTPClient; const Method, URL: string;
  Body, Target: TStream; OnProgress: TProgressProc;
  OnProgressEvent: TProgressEvent): TResponse; overload;
{ Appends URL-encoded Params to URL. }
function AppendQueryParams(const URL: string;
  const Params: array of TKeyValue): string;
{ Encodes Fields as application/x-www-form-urlencoded. }
function EncodeFormFields(const Fields: array of TKeyValue): string;
{ Builds a multipart/form-data body. Files values are file paths. The caller
  frees the result. Raises ERequestError if a file cannot be read. }
function BuildMultipartBody(const Fields, Files: array of TKeyValue;
  out ContentType: string): TStream;



const
  Http: THttp = ();
  
implementation

uses
  ssockets;

const
  FORM_CONTENT_TYPE = 'application/x-www-form-urlencoded';
  TIMEOUT_TOLERANCE_MS = 50;

var
  SSLInitialized: Boolean = False;
  GDefaultTimeout: Integer = 30000;
 
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
  // Mark bytes as UTF-8 without conversion
  SetCodePage(Raw, CP_UTF8, False);
  Result := string(Raw);
end;
  
function EncodeURIComponent(const S: string): string;
var
  Bytes: RawByteString;
  I: SizeInt;
  B: Byte;
begin
  // Convert to UTF-8 bytes explicitly
  Bytes := UTF8Encode(S);
  Result := '';
  for I := 1 to Length(Bytes) do
  begin
    B := Byte(Bytes[I]);
    if Chr(B) in [
      'A'..'Z','a'..'z','0'..'9','-','_','.','~'
    ] then
      Result := Result + Chr(B)
    else if B = Ord(' ') then
      Result := Result + '%20'
    else
      Result := Result + '%' + IntToHex(B, 2);
  end;
end;

function EncodeFormFields(const Fields: array of TKeyValue): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Fields) do
  begin
    if Result <> '' then
      Result := Result + '&';
    Result := Result + EncodeURIComponent(Fields[I].Key) + '=' +
      EncodeURIComponent(Fields[I].Value);
  end;
end;

{$IFDEF WINDOWS}
function EnumProcessModules(hProcess: THandle; lphModule: LPDWORD; cb: DWORD; var lpcbNeeded: DWORD): BOOL; stdcall; external 'psapi.dll';

function FindSSLDLLPath(const SearchTerm: string): string;
var
  Modules: array[0..1023] of HMODULE;
  cbNeeded: DWORD;
  i, ModuleCount: Integer;
  ModulePath: array[0..MAX_PATH] of Char;
  PathStr, FileName: string;
begin
  Result := '';

  if EnumProcessModules(GetCurrentProcess, @Modules[0], SizeOf(Modules), cbNeeded) then
  begin
    ModuleCount := cbNeeded div SizeOf(HMODULE);
    for i := 0 to ModuleCount - 1 do
    begin
      if GetModuleFileName(Modules[i], ModulePath, MAX_PATH) > 0 then
      begin
        PathStr := string(ModulePath);
        FileName := ExtractFileName(LowerCase(PathStr));

        // Look for DLLs containing the search term (e.g., "libssl" or "libcrypto")
        if Pos(LowerCase(SearchTerm), FileName) > 0 then
        begin
          Result := PathStr;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure ConfigureOpenSSL3LibraryNames;
begin
  { FPC 3.2.2 supports the OpenSSL 3 API, but its Windows loader still
    searches for legacy and OpenSSL 1.1 DLL names. Use OpenSSL 3 as the
    primary candidate while leaving FPC's existing fallback names intact. }
  {$IFDEF CPU64}
  DLLSSLName := 'libssl-3-x64.dll';
  DLLUtilName := 'libcrypto-3-x64.dll';
  {$ELSE}
  DLLSSLName := 'libssl-3.dll';
  DLLUtilName := 'libcrypto-3.dll';
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF UNIX}
procedure ConfigureOpenSSL3Versions;
var
  I: Integer;
begin
  { FPC 3.2.2 tries the unversioned library (from the -dev package), then
    1.1 and older, but never OpenSSL 3 (".3"). Insert ".3" right after the
    unversioned name and drop the oldest 0.9.x entry. Newer FPC versions
    that already list ".3" are left alone. }
  for I := Low(DLLVersions) to High(DLLVersions) do
    if DLLVersions[I] = '.3' then
      Exit;
  for I := High(DLLVersions) downto Low(DLLVersions) + 2 do
    DLLVersions[I] := DLLVersions[I - 1];
  DLLVersions[Low(DLLVersions) + 1] := '.3';
end;
{$ENDIF}

{$IFDEF DARWIN}
procedure ConfigureDarwinOpenSSLPath;
const
  { Homebrew (Apple silicon, Intel) and MacPorts install locations. macOS
    does not search these by default, and its own libcrypto must not be
    loaded by applications. }
  Dirs: array[0..2] of string = (
    '/opt/homebrew/opt/openssl@3/lib/',
    '/usr/local/opt/openssl@3/lib/',
    '/opt/local/lib/');
var
  I: Integer;
begin
  for I := Low(Dirs) to High(Dirs) do
    if FileExists(Dirs[I] + 'libssl.3.dylib') and
       FileExists(Dirs[I] + 'libcrypto.3.dylib') then
    begin
      DLLSSLName := Dirs[I] + 'libssl';
      DLLUtilName := Dirs[I] + 'libcrypto';
      Exit;
    end;
end;
{$ENDIF}

function SSLSetupHint: string;
begin
  {$IFDEF WINDOWS}
  Result :=
    'Install OpenSSL and copy the required DLLs to your executable folder or add to PATH:' + LineEnding +
    {$IFDEF CPU64}
    '  This is a 64-bit executable. You need 64-bit DLLs:' + LineEnding +
    '  OpenSSL 1.1.x: libssl-1_1-x64.dll and libcrypto-1_1-x64.dll' + LineEnding +
    '  OpenSSL 3.x: libssl-3-x64.dll and libcrypto-3-x64.dll' + LineEnding +
    {$ELSE}
    '  This is a 32-bit executable. You need 32-bit DLLs:' + LineEnding +
    '  OpenSSL 1.1.x: libssl-1_1.dll and libcrypto-1_1.dll' + LineEnding +
    '  OpenSSL 3.x: libssl-3.dll and libcrypto-3.dll' + LineEnding +
    {$ENDIF}
    'Install via: choco install openssl OR scoop install openssl' + LineEnding +
    'Or download from: https://slproweb.com/products/Win32OpenSSL.html' + LineEnding +
    'IMPORTANT: Ensure DLL architecture (32-bit vs 64-bit) matches your executable!';
  {$ELSE}
  {$IFDEF DARWIN}
  Result :=
    'Install OpenSSL 3 with Homebrew: brew install openssl@3' + LineEnding +
    '(or MacPorts: sudo port install openssl3).';
  {$ELSE}
  Result :=
    'Install the OpenSSL 3 libraries:' + LineEnding +
    'On Ubuntu/Debian: sudo apt-get install libssl3' + LineEnding +
    'On Fedora/RHEL: sudo dnf install openssl-libs';
  {$ENDIF}
  {$ENDIF}
end;

procedure EnsureSSL;
var
  ErrorMsg: string;
  {$IFDEF WINDOWS}
  SSLPath, CryptoPath: string;
  {$ENDIF}
begin
  if SSLInitialized then
    Exit;

  try
    if DEBUG_MODE then
      WriteLn('[DEBUG] Initializing OpenSSL...');

    if not InitSSLInterface then
      raise ERequestError.Create('Could not load the OpenSSL libraries');
    SSLInitialized := True;

    if DEBUG_MODE then
    begin
      WriteLn('[DEBUG] OpenSSL initialized successfully');
      {$IFDEF WINDOWS}
      // Find the actual loaded SSL DLLs by searching all loaded modules
      SSLPath := FindSSLDLLPath('libssl');
      CryptoPath := FindSSLDLLPath('libcrypto');

      if SSLPath <> '' then
        WriteLn('[DEBUG] libssl loaded from: ', SSLPath)
      else
        WriteLn('[DEBUG] WARNING: Could not determine libssl DLL path');

      if CryptoPath <> '' then
        WriteLn('[DEBUG] libcrypto loaded from: ', CryptoPath)
      else
        WriteLn('[DEBUG] WARNING: Could not determine libcrypto DLL path');
      {$ENDIF}

      try
        WriteLn('[DEBUG] OpenSSL version: ', SSLeay_version(0));
      except
        on E: Exception do
          WriteLn('[DEBUG] ERROR getting OpenSSL version: ', E.Message);
      end;
    end;
  except
    on E: Exception do
    begin
      ErrorMsg := 'OpenSSL initialization failed: ' + E.Message + LineEnding +
                  SSLSetupHint;
      if DEBUG_MODE then
        WriteLn(ErrorMsg);
      raise ERequestError.Create(ErrorMsg);
    end;
  end;
end;

{ TKeyValue }

class function TKeyValue.Create(const AKey, AValue: string): TKeyValue;
begin
  Result.Key := AKey;
  Result.Value := AValue;
end;

function KV(const Key, Value: string): TKeyValue;
begin
  Result := TKeyValue.Create(Key, Value);
end;

{ TResponse }

// Advanced record management: TResponse uses Initialize, Finalize, and Copy operators
// to ensure automatic cleanup of owned TJSONData and safe assignment/copying.
class operator TResponse.Initialize(var Response: TResponse);
begin
  // Called automatically when a TResponse is created
  Response.FContent := '';
  Response.FHeaders := '';
  Response.FJSON := nil;  // Will be created on-demand in GetJSON
  Response.StatusCode := 0;
end;

class operator TResponse.Finalize(var Response: TResponse);
begin
  if DEBUG_MODE then WriteLn('[DEBUG] TResponse.Finalize called');

  // Called automatically when a TResponse goes out of scope or is freed
  if Assigned(Response.FJSON) then
    Response.FJSON.Free;
  Response.FJSON := nil;
end;

class operator TResponse.Copy(constref Source: TResponse; var Dest: TResponse);
begin
  if Assigned(Dest.FJSON) then
    Dest.FJSON.Free;
  Dest.FContent := Source.FContent;
  Dest.FHeaders := Source.FHeaders;
  Dest.StatusCode := Source.StatusCode;
  Dest.FJSON := nil; // Prevent double-free
end;

function TResponse.GetText: string;
begin
  Result := FContent;
end;

function TResponse.GetOK: Boolean;
begin
  Result := IsSuccessStatus;
end;

procedure TResponse.SetContent(const AContent: string; AJSON: TJSONData = nil);
begin
  if Assigned(FJSON) then
    FreeAndNil(FJSON);
  FContent := AContent;
  if Assigned(AJSON) then
    FJSON := AJSON
  else
    FJSON := nil;
end;

function TResponse.GetJSON: TJSONData;
var
  Parser: TJSONParser;
  Stream: TStringStream;
begin
  // Lazy initialization of JSON - only parse when needed
  if not Assigned(FJSON) and (FContent <> '') then
  begin
    try
      Stream := TStringStream.Create(FContent);
      try
        Parser := TJSONParser.Create(Stream, [joUTF8]);
        try
          FJSON := Parser.Parse;
        finally
          Parser.Free;
        end;
      finally
        Stream.Free;
      end;
    except
      on E: Exception do
        raise ERequestError.Create('JSON Parse Error: ' + E.Message);
    end;
  end;
  Result := FJSON;
end;

function TResponse.HeaderValue(const Name: string): string;
var
  Lines: TStringList;
  I, P: Integer;
  Key, Value: string;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    Lines.Text := FHeaders;
    for I := 0 to Lines.Count - 1 do
    begin
      P := Pos(':', Lines[I]);
      if P > 0 then
      begin
        Key := Trim(Copy(Lines[I], 1, P - 1));
        Value := Trim(Copy(Lines[I], P + 1, MaxInt));
        if SameText(Key, Name) then
        begin
          Result := Value;
          Exit;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TResponse.SetHeadersText(const AHeaders: string);
begin
  FHeaders := AHeaders;
end;

function TResponse.IsSuccessStatus: Boolean;
begin
  Result := (StatusCode >= 200) and (StatusCode <= 299);
end;

procedure TResponse.RaiseForStatus;
begin
  if not IsSuccessStatus then
    raise ERequestError.CreateFmt(
      'HTTP request returned non-success status %d', [StatusCode]);
end;

procedure TResponse.SaveToStream(Stream: TStream);
begin
  // FContent holds the body bytes exactly as received; write them unchanged
  // so binary downloads are not re-encoded.
  if Length(FContent) > 0 then
    Stream.WriteBuffer(Pointer(FContent)^, Length(FContent));
end;

function TResponse.Bytes: TBytes;
begin
  SetLength(Result, Length(FContent));
  if Length(FContent) > 0 then
    Move(Pointer(FContent)^, Result[0], Length(FContent));
end;

procedure TResponse.SaveToFile(const FilePath: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FilePath, fmCreate);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

function TRequestResult.GetOK: Boolean;
begin
  Result := Success and Response.IsSuccessStatus;
end;

{ Shared request pipeline }

function HasHeader(const Headers: array of TKeyValue; const Name: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Headers) do
    if SameText(Headers[I].Key, Name) then
      Exit(True);
  Result := False;
end;

function AppendQueryParams(const URL: string;
  const Params: array of TKeyValue): string;
var
  QueryStr: string;
begin
  Result := URL;
  QueryStr := EncodeFormFields(Params);
  if QueryStr = '' then
    Exit;
  if Pos('?', Result) > 0 then
    Result := Result + '&' + QueryStr
  else
    Result := Result + '?' + QueryStr;
end;

function LooksLikeNetworkError(const Msg: string): Boolean;
var
  L: string;
begin
  L := LowerCase(Msg);
  Result := (Pos('socket', L) > 0) or (Pos('connection', L) > 0) or
            (Pos('timeout', L) > 0) or (Pos('timed out', L) > 0);
end;

function CreateRequestError(E: Exception; ElapsedMS: QWord;
  TimeoutMS: Integer): ERequestError;
var
  Msg: string;
begin
  Msg := 'Network Error: ' + E.Message;
  if E is ESocketError then
  begin
    if ESocketError(E).Code = seConnectTimeOut then
      Exit(ETimeoutError.Create(Msg));
    Exit(ENetworkError.Create(Msg));
  end;
  if LooksLikeNetworkError(E.Message) then
  begin
    { FPC reports an I/O timeout as a generic socket read error, so treat a
      transport failure after the timeout has elapsed as a timeout. The
      tolerance covers the coarse tick (~16 ms) of GetTickCount64 on Windows. }
    if (TimeoutMS > 0) and
       (ElapsedMS + TIMEOUT_TOLERANCE_MS >= QWord(TimeoutMS)) then
      Exit(ETimeoutError.Create(Msg));
    Exit(ENetworkError.Create(Msg));
  end;
  Result := ERequestError.Create('HTTP Request Error: ' + E.Message);
end;

type
  { Receives the response body from TFPHTTPClient. A 2xx body goes to Target
    with progress reports; any other body is kept in ErrorBody. FPC does not
    write redirect bodies, so only the final response reaches the sink. }
  TResponseSink = class(TStream)
  private
    FClient: TFPHTTPClient;
    FTarget: TStream;
    FErrorBody: TMemoryStream;
    FWritten: Int64;
    FReceived: Int64;
    FTotal: Int64;
    FStarted: Boolean;
    FOnProgress: TProgressProc;
    FOnProgressEvent: TProgressEvent;
  public
    constructor Create(AClient: TFPHTTPClient; ATarget: TStream;
      AOnProgress: TProgressProc; AOnProgressEvent: TProgressEvent);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property ErrorBody: TMemoryStream read FErrorBody;
    property Received: Int64 read FReceived;
  end;

constructor TResponseSink.Create(AClient: TFPHTTPClient; ATarget: TStream;
  AOnProgress: TProgressProc; AOnProgressEvent: TProgressEvent);
begin
  inherited Create;
  FClient := AClient;
  FTarget := ATarget;
  FOnProgress := AOnProgress;
  FOnProgressEvent := AOnProgressEvent;
  FErrorBody := TMemoryStream.Create;
end;

destructor TResponseSink.Destroy;
begin
  FErrorBody.Free;
  inherited Destroy;
end;

function TResponseSink.Read(var Buffer; Count: Longint): Longint;
begin
  raise EStreamError.Create('TResponseSink is write-only');
end;

function TResponseSink.Write(const Buffer; Count: Longint): Longint;
var
  Status: Integer;
begin
  Result := Count;
  if Count <= 0 then
    Exit;
  Inc(FWritten, Count);
  Status := FClient.ResponseStatusCode;
  if (Status < 200) or (Status > 299) then
  begin
    FErrorBody.WriteBuffer(Buffer, Count);
    Exit;
  end;
  if not FStarted then
  begin
    FStarted := True;
    FTotal := StrToInt64Def(
      TFPHTTPClient.GetHeader(FClient.ResponseHeaders, 'Content-Length'), -1);
  end;
  FTarget.WriteBuffer(Buffer, Count);
  Inc(FReceived, Count);
  if Assigned(FOnProgress) then
    FOnProgress(FReceived, FTotal);
  if Assigned(FOnProgressEvent) then
    FOnProgressEvent(FReceived, FTotal);
end;

function TResponseSink.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  NewPos: Int64;
begin
  // Forward-only: allow position queries, refuse real moves.
  if Origin = soBeginning then
    NewPos := Offset
  else
    NewPos := FWritten + Offset;
  if NewPos <> FWritten then
    raise EStreamError.Create('TResponseSink cannot seek');
  Result := FWritten;
end;

{ TDownloadFile }

constructor TDownloadFile.Create(const AFilePath: string);
begin
  inherited Create;
  FFilePath := AFilePath;
  FPartPath := AFilePath + '.part';
  try
    FStream := TFileStream.Create(FPartPath, fmCreate);
  except
    on E: Exception do
      raise ERequestError.Create('Cannot write download file "' +
        FPartPath + '": ' + E.Message);
  end;
end;

destructor TDownloadFile.Destroy;
begin
  FreeAndNil(FStream);
  if not FCommitted and FileExists(FPartPath) then
    SysUtils.DeleteFile(FPartPath);
  inherited Destroy;
end;

procedure TDownloadFile.Commit;
begin
  FreeAndNil(FStream);
  if FileExists(FFilePath) and not SysUtils.DeleteFile(FFilePath) then
    raise ERequestError.Create('Cannot replace "' + FFilePath + '"');
  if not SysUtils.RenameFile(FPartPath, FFilePath) then
    raise ERequestError.Create('Cannot rename "' + FPartPath + '" to "' +
      FFilePath + '"');
  FCommitted := True;
end;

{ Runs the request, writing the body to Sink when given or to memory. }
function RunRequest(Client: TFPHTTPClient; const Method, URL: string;
  Body: TStream; Sink: TResponseSink): TResponse;
var
  ResponseStream: TMemoryStream;
  StartTick: QWord;
begin
  ResponseStream := nil;
  try
    StartTick := GetTickCount64;
    try
      if Pos('https://', LowerCase(URL)) = 1 then
        EnsureSSL;
      if Assigned(Body) then
        Body.Position := 0;
      Client.RequestBody := Body;
      try
        // Accept any status; callers inspect StatusCode, OK, or RaiseForStatus.
        if Assigned(Sink) then
          Client.HTTPMethod(Method, URL, Sink, [])
        else
        begin
          ResponseStream := TMemoryStream.Create;
          Client.HTTPMethod(Method, URL, ResponseStream, []);
        end;
      finally
        Client.RequestBody := nil;
      end;
      Result.StatusCode := Client.ResponseStatusCode;
      Result.FHeaders := Client.ResponseHeaders.Text;
      if Assigned(Sink) then
        Result.SetContent(ReadStreamAsUTF8String(Sink.ErrorBody))
      else
        Result.SetContent(ReadStreamAsUTF8String(ResponseStream));
    except
      on E: ERequestError do
        raise;
      on E: Exception do
        raise CreateRequestError(E, GetTickCount64 - StartTick,
          Client.IOTimeout);
    end;
  finally
    ResponseStream.Free;
  end;
end;

function ExecuteRequest(Client: TFPHTTPClient; const Method, URL: string;
  Body: TStream): TResponse;
begin
  Result := RunRequest(Client, Method, URL, Body, nil);
end;

function ExecuteRequest(Client: TFPHTTPClient; const Method, URL: string;
  Body, Target: TStream; OnProgress: TProgressProc;
  OnProgressEvent: TProgressEvent): TResponse;
var
  Sink: TResponseSink;
  Expected: Int64;
begin
  if Target = nil then
    raise ERequestError.Create('Target stream cannot be nil');
  Sink := TResponseSink.Create(Client, Target, OnProgress, OnProgressEvent);
  try
    Result := RunRequest(Client, Method, URL, Body, Sink);
    { FPC stops quietly when the server closes early, so check the length
      to avoid saving a truncated body as complete. }
    if Result.OK and not SameText(Method, 'HEAD') then
    begin
      Expected := StrToInt64Def(Result.HeaderValue('Content-Length'), -1);
      if (Expected >= 0) and (Sink.Received <> Expected) then
        raise ENetworkError.CreateFmt(
          'Network Error: connection closed after %d of %d bytes',
          [Sink.Received, Expected]);
    end;
  finally
    Sink.Free;
  end;
end;

function NewClient(const DefaultContentType: string;
  const Headers: array of TKeyValue): TFPHTTPClient;
var
  I: Integer;
begin
  Result := TFPHTTPClient.Create(nil);
  Result.AllowRedirect := True;
  Result.IOTimeout := GDefaultTimeout;
  for I := 0 to High(Headers) do
    Result.RequestHeaders.Add(Headers[I].Key + ': ' + Headers[I].Value);
  if (DefaultContentType <> '') and not HasHeader(Headers, 'Content-Type') then
    Result.RequestHeaders.Add('Content-Type: ' + DefaultContentType);
  if not HasHeader(Headers, 'User-Agent') then
    Result.RequestHeaders.Add('User-Agent: ' + DEFAULT_USER_AGENT);
end;

function SendStream(const Method, URL: string; Body: TStream;
  const DefaultContentType: string; const Headers: array of TKeyValue;
  const Params: array of TKeyValue): TResponse;
var
  Client: TFPHTTPClient;
begin
  Client := NewClient(DefaultContentType, Headers);
  try
    Result := ExecuteRequest(Client, Method, AppendQueryParams(URL, Params), Body);
  finally
    Client.Free;
  end;
end;

function StreamGet(const URL: string; Target: TStream;
  const Headers, Params: array of TKeyValue; OnProgress: TProgressProc;
  OnProgressEvent: TProgressEvent): TResponse;
var
  Client: TFPHTTPClient;
begin
  if Target = nil then
    raise ERequestError.Create('Stream cannot be nil');
  Client := NewClient('', Headers);
  try
    Result := ExecuteRequest(Client, 'GET', AppendQueryParams(URL, Params),
      nil, Target, OnProgress, OnProgressEvent);
  finally
    Client.Free;
  end;
end;

function DownloadFile(const URL, FilePath: string;
  const Headers, Params: array of TKeyValue; OnProgress: TProgressProc;
  OnProgressEvent: TProgressEvent): TResponse;
var
  Download: TDownloadFile;
begin
  Download := TDownloadFile.Create(FilePath);
  try
    Result := StreamGet(URL, Download.Stream, Headers, Params, OnProgress,
      OnProgressEvent);
    if Result.OK then
      Download.Commit;
  finally
    Download.Free;
  end;
end;

function SendText(const Method, URL, Data: string; HasBody: Boolean;
  const DefaultContentType: string; const Headers: array of TKeyValue;
  const Params: array of TKeyValue): TResponse;
var
  Body: TStringStream;
begin
  if HasBody then
    Body := TStringStream.Create(Data)
  else
    Body := nil;
  try
    Result := SendStream(Method, URL, Body, DefaultContentType, Headers, Params);
  finally
    Body.Free;
  end;
end;

{ Sends Data with a form content type only when there is a body, matching
  the original Post/Put behavior. }
function SendData(const Method, URL, Data: string;
  const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
begin
  if Data <> '' then
    Result := SendText(Method, URL, Data, True, FORM_CONTENT_TYPE, Headers, Params)
  else
    Result := SendText(Method, URL, '', False, '', Headers, Params);
end;

procedure SetTryFailure(var R: TRequestResult; E: Exception);
begin
  R.Success := False;
  R.Error := E.Message;
  R.Response.SetContent('');
  R.Response.FHeaders := '';
  R.Response.StatusCode := 0;
end;

{ Multipart }

const
  MimeTypes: array[0..15] of array[0..1] of string = (
    ('.txt', 'text/plain'), ('.html', 'text/html'), ('.htm', 'text/html'),
    ('.css', 'text/css'), ('.csv', 'text/csv'), ('.json', 'application/json'),
    ('.xml', 'application/xml'), ('.pdf', 'application/pdf'),
    ('.zip', 'application/zip'), ('.png', 'image/png'),
    ('.jpg', 'image/jpeg'), ('.jpeg', 'image/jpeg'), ('.gif', 'image/gif'),
    ('.svg', 'image/svg+xml'), ('.webp', 'image/webp'),
    ('.js', 'text/javascript'));

function GuessContentType(const FileName: string): string;
var
  Ext: string;
  I: Integer;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  for I := Low(MimeTypes) to High(MimeTypes) do
    if MimeTypes[I][0] = Ext then
      Exit(MimeTypes[I][1]);
  Result := 'application/octet-stream';
end;

{ Escapes a name or filename for a Content-Disposition quoted string, as
  browsers do. }
function EscapeDispositionValue(const S: string): string;
begin
  Result := StringReplace(S, '"', '%22', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '%0D', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '%0A', [rfReplaceAll]);
end;

function NewMultipartBoundary: string;
var
  G: TGUID;
begin
  CreateGUID(G);
  Result := '----RequestFP' +
    StringReplace(Copy(GUIDToString(G), 2, 36), '-', '', [rfReplaceAll]);
end;

procedure WriteRaw(Stream: TStream; const S: string);
begin
  if S <> '' then
    Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

function BuildMultipartBody(const Fields, Files: array of TKeyValue;
  out ContentType: string): TStream;
const
  CRLF = #13#10;
var
  Boundary, FileName: string;
  Body: TMemoryStream;
  FileStream: TFileStream;
  I: Integer;
begin
  Boundary := NewMultipartBoundary;
  ContentType := 'multipart/form-data; boundary=' + Boundary;
  Body := TMemoryStream.Create;
  try
    for I := 0 to High(Fields) do
      WriteRaw(Body, '--' + Boundary + CRLF +
        'Content-Disposition: form-data; name="' +
        EscapeDispositionValue(Fields[I].Key) + '"' + CRLF + CRLF +
        Fields[I].Value + CRLF);

    for I := 0 to High(Files) do
    begin
      FileName := ExtractFileName(Files[I].Value);
      try
        FileStream := TFileStream.Create(Files[I].Value,
          fmOpenRead or fmShareDenyWrite);
      except
        on E: Exception do
          raise ERequestError.Create('Cannot read upload file "' +
            Files[I].Value + '": ' + E.Message);
      end;
      try
        WriteRaw(Body, '--' + Boundary + CRLF +
          'Content-Disposition: form-data; name="' +
          EscapeDispositionValue(Files[I].Key) + '"; filename="' +
          EscapeDispositionValue(FileName) + '"' + CRLF +
          'Content-Type: ' + GuessContentType(FileName) + CRLF + CRLF);
        if FileStream.Size > 0 then
          Body.CopyFrom(FileStream, FileStream.Size);
        WriteRaw(Body, CRLF);
      finally
        FileStream.Free;
      end;
    end;

    WriteRaw(Body, '--' + Boundary + '--' + CRLF);
    Body.Position := 0;
    Result := Body;
  except
    Body.Free;
    raise;
  end;
end;

{ THttp }

class procedure THttp.SetTimeout(Milliseconds: Integer);
begin
  if Milliseconds < 0 then
    Milliseconds := 0;
  GDefaultTimeout := Milliseconds;
end;

class function THttp.GetTimeout: Integer;
begin
  Result := GDefaultTimeout;
end;

class function THttp.Get(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
begin
  Result := SendText('GET', URL, '', False, '', Headers, Params);
end;

class function THttp.Post(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
begin
  Result := SendData('POST', URL, Data, Headers, Params);
end;

class function THttp.Put(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
begin
  Result := SendData('PUT', URL, Data, Headers, Params);
end;

class function THttp.Patch(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
begin
  Result := SendData('PATCH', URL, Data, Headers, Params);
end;

class function THttp.Delete(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
begin
  Result := SendText('DELETE', URL, '', False, '', Headers, Params);
end;

class function THttp.Head(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
begin
  Result := SendText('HEAD', URL, '', False, '', Headers, Params);
end;

class function THttp.PostJSON(const URL: string; const JSON: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
begin
  Result := SendText('POST', URL, JSON, True, 'application/json', Headers, Params);
end;

class function THttp.PostJSON(const URL: string; const JSON: TJSONData;
  const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
begin
  if JSON = nil then
    raise ERequestError.Create('JSON data cannot be nil');
  Result := PostJSON(URL, JSON.AsJSON, Headers, Params);
end;

class function THttp.PostForm(const URL: string; const Fields: array of TKeyValue;
  const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
begin
  Result := SendText('POST', URL, EncodeFormFields(Fields), True,
    FORM_CONTENT_TYPE, Headers, Params);
end;

class function THttp.PostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue; const Params: array of TKeyValue): TResponse;
var
  Body: TStream;
  ContentType: string;
begin
  Body := BuildMultipartBody(Fields, Files, ContentType);
  try
    Result := SendStream('POST', URL, Body, ContentType, Headers, Params);
  finally
    Body.Free;
  end;
end;

{ Try* variants: never raise; failures are reported in the result. }

class function THttp.TryGet(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := Get(URL, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

class function THttp.TryPost(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := Post(URL, Data, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

class function THttp.TryPut(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := Put(URL, Data, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

class function THttp.TryPatch(const URL: string; const Data: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := Patch(URL, Data, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

class function THttp.TryDelete(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := Delete(URL, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

class function THttp.TryHead(const URL: string; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := Head(URL, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

class function THttp.TryPostForm(const URL: string;
  const Fields: array of TKeyValue; const Headers: array of TKeyValue;
  const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := PostForm(URL, Fields, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

class function THttp.TryPostJSON(const URL, JSON: string;
  const Headers: array of TKeyValue;
  const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := PostJSON(URL, JSON, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

class function THttp.TryPostJSON(const URL: string; const JSON: TJSONData;
  const Headers: array of TKeyValue;
  const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := PostJSON(URL, JSON, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

class function THttp.TryPostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue; const Params: array of TKeyValue): TRequestResult;
begin
  try
    Result.Response := PostMultipart(URL, Fields, Files, Headers, Params);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

{ Short overloads }

class function THttp.Get(const URL: string): TResponse;
begin
  Result := Get(URL, [], []);
end;

class function THttp.Get(const URL: string; const Headers: array of TKeyValue): TResponse;
begin
  Result := Get(URL, Headers, []);
end;

class function THttp.GetWithParams(const URL: string; const Params: array of TKeyValue): TResponse;
begin
  Result := Get(URL, [], Params);
end;

class function THttp.Post(const URL: string; const Data: string): TResponse;
begin
  Result := Post(URL, Data, [], []);
end;

class function THttp.Post(const URL: string; const Data: string; const Headers: array of TKeyValue): TResponse;
begin
  Result := Post(URL, Data, Headers, []);
end;

class function THttp.PostForm(const URL: string; const Fields: array of TKeyValue): TResponse;
begin
  Result := PostForm(URL, Fields, [], []);
end;

class function THttp.PostForm(const URL: string; const Fields: array of TKeyValue; const Headers: array of TKeyValue): TResponse;
begin
  Result := PostForm(URL, Fields, Headers, []);
end;

class function THttp.Put(const URL: string; const Data: string): TResponse;
begin
  Result := Put(URL, Data, [], []);
end;

class function THttp.Put(const URL: string; const Data: string; const Headers: array of TKeyValue): TResponse;
begin
  Result := Put(URL, Data, Headers, []);
end;

class function THttp.Patch(const URL: string; const Data: string): TResponse;
begin
  Result := Patch(URL, Data, [], []);
end;

class function THttp.Patch(const URL: string; const Data: string; const Headers: array of TKeyValue): TResponse;
begin
  Result := Patch(URL, Data, Headers, []);
end;

class function THttp.Delete(const URL: string): TResponse;
begin
  Result := Delete(URL, [], []);
end;

class function THttp.Delete(const URL: string; const Headers: array of TKeyValue): TResponse;
begin
  Result := Delete(URL, Headers, []);
end;

class function THttp.Head(const URL: string): TResponse;
begin
  Result := Head(URL, [], []);
end;

class function THttp.Head(const URL: string; const Headers: array of TKeyValue): TResponse;
begin
  Result := Head(URL, Headers, []);
end;

class function THttp.PostJSON(const URL: string; const JSON: string): TResponse;
begin
  Result := PostJSON(URL, JSON, [], []);
end;

class function THttp.PostJSON(const URL: string; const JSON: string; const Headers: array of TKeyValue): TResponse;
begin
  Result := PostJSON(URL, JSON, Headers, []);
end;

class function THttp.PostJSON(const URL: string; const JSON: TJSONData): TResponse;
begin
  Result := PostJSON(URL, JSON, [], []);
end;

class function THttp.PostJSON(const URL: string; const JSON: TJSONData; const Headers: array of TKeyValue): TResponse;
begin
  Result := PostJSON(URL, JSON, Headers, []);
end;

class function THttp.PostMultipart(const URL: string; const Fields, Files: array of TKeyValue): TResponse;
begin
  Result := PostMultipart(URL, Fields, Files, [], []);
end;

class function THttp.PostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue): TResponse;
begin
  Result := PostMultipart(URL, Fields, Files, Headers, []);
end;

class function THttp.TryGet(const URL: string): TRequestResult;
begin
  Result := TryGet(URL, [], []);
end;

class function THttp.TryGet(const URL: string; const Headers: array of TKeyValue): TRequestResult;
begin
  Result := TryGet(URL, Headers, []);
end;

class function THttp.TryGetWithParams(const URL: string; const Params: array of TKeyValue): TRequestResult;
begin
  Result := TryGet(URL, [], Params);
end;

class function THttp.TryPost(const URL: string; const Data: string): TRequestResult;
begin
  Result := TryPost(URL, Data, [], []);
end;

class function THttp.TryPost(const URL: string; const Data: string; const Headers: array of TKeyValue): TRequestResult;
begin
  Result := TryPost(URL, Data, Headers, []);
end;

class function THttp.TryPostForm(const URL: string; const Fields: array of TKeyValue): TRequestResult;
begin
  Result := TryPostForm(URL, Fields, [], []);
end;

class function THttp.TryPostForm(const URL: string; const Fields: array of TKeyValue; const Headers: array of TKeyValue): TRequestResult;
begin
  Result := TryPostForm(URL, Fields, Headers, []);
end;

class function THttp.TryPostJSON(const URL: string; const JSON: string): TRequestResult;
begin
  Result := TryPostJSON(URL, JSON, [], []);
end;

class function THttp.TryPostJSON(const URL: string; const JSON: string; const Headers: array of TKeyValue): TRequestResult;
begin
  Result := TryPostJSON(URL, JSON, Headers, []);
end;

class function THttp.TryPostJSON(const URL: string; const JSON: TJSONData): TRequestResult;
begin
  Result := TryPostJSON(URL, JSON, [], []);
end;

class function THttp.TryPostJSON(const URL: string; const JSON: TJSONData; const Headers: array of TKeyValue): TRequestResult;
begin
  Result := TryPostJSON(URL, JSON, Headers, []);
end;

class function THttp.TryPut(const URL: string; const Data: string): TRequestResult;
begin
  Result := TryPut(URL, Data, [], []);
end;

class function THttp.TryPut(const URL: string; const Data: string; const Headers: array of TKeyValue): TRequestResult;
begin
  Result := TryPut(URL, Data, Headers, []);
end;

class function THttp.TryPatch(const URL: string; const Data: string): TRequestResult;
begin
  Result := TryPatch(URL, Data, [], []);
end;

class function THttp.TryPatch(const URL: string; const Data: string; const Headers: array of TKeyValue): TRequestResult;
begin
  Result := TryPatch(URL, Data, Headers, []);
end;

class function THttp.TryDelete(const URL: string): TRequestResult;
begin
  Result := TryDelete(URL, [], []);
end;

class function THttp.TryDelete(const URL: string; const Headers: array of TKeyValue): TRequestResult;
begin
  Result := TryDelete(URL, Headers, []);
end;

class function THttp.TryHead(const URL: string): TRequestResult;
begin
  Result := TryHead(URL, [], []);
end;

class function THttp.TryHead(const URL: string; const Headers: array of TKeyValue): TRequestResult;
begin
  Result := TryHead(URL, Headers, []);
end;

class function THttp.TryPostMultipart(const URL: string; const Fields, Files: array of TKeyValue): TRequestResult;
begin
  Result := TryPostMultipart(URL, Fields, Files, [], []);
end;

class function THttp.TryPostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue): TRequestResult;
begin
  Result := TryPostMultipart(URL, Fields, Files, Headers, []);
end;

{ Streaming downloads }

function TryDownloadFile(const URL, FilePath: string;
  const Headers, Params: array of TKeyValue; OnProgress: TProgressProc;
  OnProgressEvent: TProgressEvent): TRequestResult;
begin
  try
    Result.Response := DownloadFile(URL, FilePath, Headers, Params,
      OnProgress, OnProgressEvent);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

function TryStreamGet(const URL: string; Target: TStream;
  const Headers, Params: array of TKeyValue; OnProgress: TProgressProc;
  OnProgressEvent: TProgressEvent): TRequestResult;
begin
  try
    Result.Response := StreamGet(URL, Target, Headers, Params, OnProgress,
      OnProgressEvent);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
      SetTryFailure(Result, E);
  end;
end;

class function THttp.Download(const URL, FilePath: string): TResponse;
begin
  Result := DownloadFile(URL, FilePath, [], [], nil, nil);
end;

class function THttp.Download(const URL, FilePath: string; OnProgress: TProgressProc): TResponse;
begin
  Result := DownloadFile(URL, FilePath, [], [], OnProgress, nil);
end;

class function THttp.Download(const URL, FilePath: string; OnProgress: TProgressEvent): TResponse;
begin
  Result := DownloadFile(URL, FilePath, [], [], nil, OnProgress);
end;

class function THttp.Download(const URL, FilePath: string; const Headers, Params: array of TKeyValue): TResponse;
begin
  Result := DownloadFile(URL, FilePath, Headers, Params, nil, nil);
end;

class function THttp.Download(const URL, FilePath: string; const Headers, Params: array of TKeyValue; OnProgress: TProgressProc): TResponse;
begin
  Result := DownloadFile(URL, FilePath, Headers, Params, OnProgress, nil);
end;

class function THttp.Download(const URL, FilePath: string; const Headers, Params: array of TKeyValue; OnProgress: TProgressEvent): TResponse;
begin
  Result := DownloadFile(URL, FilePath, Headers, Params, nil, OnProgress);
end;

class function THttp.TryDownload(const URL, FilePath: string): TRequestResult;
begin
  Result := TryDownloadFile(URL, FilePath, [], [], nil, nil);
end;

class function THttp.TryDownload(const URL, FilePath: string; OnProgress: TProgressProc): TRequestResult;
begin
  Result := TryDownloadFile(URL, FilePath, [], [], OnProgress, nil);
end;

class function THttp.TryDownload(const URL, FilePath: string; OnProgress: TProgressEvent): TRequestResult;
begin
  Result := TryDownloadFile(URL, FilePath, [], [], nil, OnProgress);
end;

class function THttp.TryDownload(const URL, FilePath: string; const Headers, Params: array of TKeyValue): TRequestResult;
begin
  Result := TryDownloadFile(URL, FilePath, Headers, Params, nil, nil);
end;

class function THttp.TryDownload(const URL, FilePath: string; const Headers, Params: array of TKeyValue; OnProgress: TProgressProc): TRequestResult;
begin
  Result := TryDownloadFile(URL, FilePath, Headers, Params, OnProgress, nil);
end;

class function THttp.TryDownload(const URL, FilePath: string; const Headers, Params: array of TKeyValue; OnProgress: TProgressEvent): TRequestResult;
begin
  Result := TryDownloadFile(URL, FilePath, Headers, Params, nil, OnProgress);
end;

class function THttp.GetToStream(const URL: string; Stream: TStream): TResponse;
begin
  Result := StreamGet(URL, Stream, [], [], nil, nil);
end;

class function THttp.GetToStream(const URL: string; Stream: TStream; OnProgress: TProgressProc): TResponse;
begin
  Result := StreamGet(URL, Stream, [], [], OnProgress, nil);
end;

class function THttp.GetToStream(const URL: string; Stream: TStream; OnProgress: TProgressEvent): TResponse;
begin
  Result := StreamGet(URL, Stream, [], [], nil, OnProgress);
end;

class function THttp.GetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue): TResponse;
begin
  Result := StreamGet(URL, Stream, Headers, Params, nil, nil);
end;

class function THttp.GetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue; OnProgress: TProgressProc): TResponse;
begin
  Result := StreamGet(URL, Stream, Headers, Params, OnProgress, nil);
end;

class function THttp.GetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue; OnProgress: TProgressEvent): TResponse;
begin
  Result := StreamGet(URL, Stream, Headers, Params, nil, OnProgress);
end;

class function THttp.TryGetToStream(const URL: string; Stream: TStream): TRequestResult;
begin
  Result := TryStreamGet(URL, Stream, [], [], nil, nil);
end;

class function THttp.TryGetToStream(const URL: string; Stream: TStream; OnProgress: TProgressProc): TRequestResult;
begin
  Result := TryStreamGet(URL, Stream, [], [], OnProgress, nil);
end;

class function THttp.TryGetToStream(const URL: string; Stream: TStream; OnProgress: TProgressEvent): TRequestResult;
begin
  Result := TryStreamGet(URL, Stream, [], [], nil, OnProgress);
end;

class function THttp.TryGetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue): TRequestResult;
begin
  Result := TryStreamGet(URL, Stream, Headers, Params, nil, nil);
end;

class function THttp.TryGetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue; OnProgress: TProgressProc): TRequestResult;
begin
  Result := TryStreamGet(URL, Stream, Headers, Params, OnProgress, nil);
end;

class function THttp.TryGetToStream(const URL: string; Stream: TStream; const Headers, Params: array of TKeyValue; OnProgress: TProgressEvent): TRequestResult;
begin
  Result := TryStreamGet(URL, Stream, Headers, Params, nil, OnProgress);
end;

initialization
  { OpenSSL loads lazily on the first HTTPS request. Only pick the
    library names here so FPC's own loader finds OpenSSL 3 as well. }
  {$IFDEF WINDOWS}
  ConfigureOpenSSL3LibraryNames;
  {$ENDIF}
  {$IFDEF UNIX}
  ConfigureOpenSSL3Versions;
  {$ENDIF}
  {$IFDEF DARWIN}
  ConfigureDarwinOpenSSLPath;
  {$ENDIF}

finalization
  // OpenSSL cleanup is handled by opensslsockets unit

end.
