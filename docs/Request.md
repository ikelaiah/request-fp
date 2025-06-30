# Request Unit Documentation

A lightweight, memory-safe HTTP client for Free Pascal that uses advanced records for automatic cleanup. This module provides a simple, procedural interface for making HTTP requests with built-in memory management and JSON integration through the standard FPC fpjson unit.

## Table of Contents

- [Request Unit Documentation](#request-unit-documentation)
  - [Table of Contents](#table-of-contents)
  - [Features](#features)
  - [System Requirements](#system-requirements)
  - [Design Philosophy](#design-philosophy)
  - [Memory Safety](#memory-safety)
  - [Basic Usage](#basic-usage)
    - [Simple GET Request](#simple-get-request)
    - [Working with JSON](#working-with-json)
  - [Error Handling](#error-handling)
    - [Using Try-Pattern](#using-try-pattern)
    - [SSL/TLS Errors](#ssltls-errors)
  - [API Reference](#api-reference)
    - [TResponse Record](#tresponse-record)
    - [TRequestResult Record](#trequestresult-record)
    - [Global HTTP Functions](#global-http-functions)
  - [Advanced Usage Examples](#advanced-usage-examples)
    - [Multipart File Uploads](#multipart-file-uploads)
    - [Custom Headers and Query Parameters](#custom-headers-and-query-parameters)
  - [Testing and Development](#testing-and-development)
  - [Best Practices](#best-practices)

## Features

- Zero-setup memory management using advanced records
- Simple procedural interface for expressive request building
- Support for common HTTP methods (GET, POST, PUT, DELETE, PATCH)
- Seamless JSON integration with FPC's standard fpjson unit
- Error handling with result pattern
- Cross-platform support for Windows and Linux
- Automatic test environment detection and HTTP fallback
- **Custom headers and query parameters supported in all procedural/stateless HTTP methods**

## System Requirements

For basic HTTP functionality, no special requirements are needed. For HTTPS (SSL/TLS) support:

- Windows: OpenSSL libraries are included with the application and no additional setup is required
- Linux:
  - Ubuntu/Debian: `sudo apt-get install libssl-dev`
  - Fedora/RHEL: `sudo dnf install openssl-devel`

The module will detect missing OpenSSL libraries and provide helpful error messages with installation instructions.

## Basic Usage

### Simple GET Request

```pascal
Response := Http.Get('https://api.example.com/data');
WriteLn(Response.Text);
```

### GET with custom headers and query parameters

```pascal
Response := Http.Get('https://api.example.com/data',
  [TKeyValue.Create('X-Api-Key', 'my-key')],
  [TKeyValue.Create('q', 'search')]);
WriteLn(Response.Text);
```

### Working with JSON

```pascal
if Response.StatusCode = 200 then
begin
  JsonObj := TJSONObject(Response.JSON);
  try
    WriteLn('Name: ', JsonObj.Get('name', ''));
  finally
    // Do NOT free JsonObj - it's owned by Response.JSON
  end;
end;
```

## Error Handling

### Using Try-Pattern

```pascal
Result := Http.TryGet('https://api.example.com/secure');
if Result.Success then
  WriteLn('Status: ', Result.Response.StatusCode)
else
  WriteLn('Error: ', Result.Error);
```

### SSL/TLS Errors

If SSL libraries are missing, the library will provide a clear error message with installation instructions.

## API Reference

### TResponse Record

- `StatusCode: Integer` — HTTP status code
- `Text: string` — Response body as text
- `JSON: TJSONData` — Parsed JSON response (if applicable)
- `HeaderValue(const Name: string): string` — Get a response header value

### TRequestResult Record

- `Success: Boolean` — True if request succeeded
- `Response: TResponse` — The response (even on error)
- `Error: string` — Error message (if any)

### Global HTTP Functions

All procedural/stateless HTTP methods support optional custom headers and query parameters:

- `Http.Get(const URL: string; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.Post(const URL: string; const Data: string = ''; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.Put(const URL: string; const Data: string = ''; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.Delete(const URL: string; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.PostJSON(const URL: string; const JSON: string; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.PostMultipart(const URL: string; const Fields, Files: array of TKeyValue; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TResponse`
- `Http.TryGet(const URL: string; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TRequestResult`
- `Http.TryPost(const URL: string; const Data: string = ''; const Headers: array of TKeyValue = []; const Params: array of TKeyValue = []): TRequestResult`

## Advanced Usage Examples

### Multipart File Uploads

```pascal
Response := Http.PostMultipart('https://api.example.com/upload',
  [TKeyValue.Create('field1', 'value1')],
  [TKeyValue.Create('file1', 'myfile.txt')]);
```

### Custom Headers and Query Parameters

```pascal
Response := Http.Get('https://api.example.com/data',
  [TKeyValue.Create('X-Api-Key', 'my-key'), TKeyValue.Create('Accept', 'application/json')],
  [TKeyValue.Create('lang', 'en')]);
WriteLn(Response.Text);
```

---