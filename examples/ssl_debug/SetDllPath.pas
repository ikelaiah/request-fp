unit SetDllPath;

{$mode objfpc}{$H+}

interface

implementation

uses SysUtils{$IFDEF WINDOWS}, Windows{$ENDIF};

{$IFDEF WINDOWS}
type
  DLL_DIRECTORY_COOKIE = Pointer;

function AddDllDirectory(NewDirectory: LPCWSTR): DLL_DIRECTORY_COOKIE; stdcall; external 'kernel32.dll';
function SetDefaultDllDirectories(DirectoryFlags: DWORD): BOOL; stdcall; external 'kernel32.dll';

const
  LOAD_LIBRARY_SEARCH_DEFAULT_DIRS = $00001000;
{$ENDIF}

initialization
  {$IFDEF WINDOWS}
  // Attempt to prioritize the executable directory for DLL loading
  // This MUST be in initialization section to run BEFORE uses clauses process
  //
  // SetDefaultDllDirectories restricts DLL search to safe directories only
  // AddDllDirectory adds the executable directory to the search path
  //
  // IMPORTANT: This helps in many cases, but Windows may still load from System32
  // if OpenSSL DLLs are installed there and registered in the Known DLLs registry.
  // To guarantee local DLL usage, remove System32 OpenSSL or use application manifests.
  SetDefaultDllDirectories(LOAD_LIBRARY_SEARCH_DEFAULT_DIRS);
  AddDllDirectory(PWideChar(UnicodeString(ExtractFilePath(ParamStr(0)))));
  {$ENDIF}

end.
