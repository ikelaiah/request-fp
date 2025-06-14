# 📋 Cheat Sheet

A comprehensive reference of TidyKit's features and usage examples.
 
## Table of Contents

- [📋 Cheat Sheet](#-cheat-sheet)
  - [Table of Contents](#table-of-contents)
  - [🔄 JSON Operations](#-json-operations)
    - [Creating Values](#creating-values)
    - [Objects](#objects)
    - [Arrays](#arrays)
    - [Type Safety](#type-safety)
    - [Parsing \& Formatting](#parsing--formatting)
    - [Error Handling](#error-handling)
  - [📁File System Operations](#file-system-operations)
  - [🌐 HTTP Request Operations](#-http-request-operations)
  - [🧵String operations](#string-operations)
  - [📚 Collection Operations](#-collection-operations)
    - [List Operations](#list-operations)
  - [🕙 DateTime Operations](#-datetime-operations)
    - [Basic Operations](#basic-operations)
    - [Component Access](#component-access)
    - [Component Modification](#component-modification)
    - [Date Arithmetic](#date-arithmetic)
    - [Period Operations](#period-operations)
    - [Interval Operations](#interval-operations)
    - [Date Comparison](#date-comparison)
    - [Period Boundaries](#period-boundaries)
    - [Date Rounding](#date-rounding)
    - [Timezone Operations](#timezone-operations)
  - [🔒 Cryptographic Operations](#-cryptographic-operations)
    - [Hash Functions](#hash-functions)
    - [Base64 Encoding/Decoding](#base64-encodingdecoding)
    - [XOR Encryption (Basic)](#xor-encryption-basic)
    - [Blowfish Encryption (Legacy Support)](#blowfish-encryption-legacy-support)
    - [AES-256 Encryption](#aes-256-encryption)
    - [Common Use Cases](#common-use-cases)
    - [Best Practices](#best-practices)
    - [Security Notes](#security-notes)
    - [Common Scenarios](#common-scenarios)
    - [Advanced Use](#advanced-use)
  - [📁 Archive Operations](#-archive-operations)
  - [🛠️ Error Handling](#️-error-handling)
  - [⚙️ Command-Line Argument Parsing](#️-command-line-argument-parsing)

## 🔄 JSON Operations

### Creating Values
```pascal
TJSON.Obj                    // Empty object: {}
TJSON.Arr                    // Empty array: []
TJSON.Str('text')           // String: "text"
TJSON.Int(123)              // Integer: 123
TJSON.Num(123.45)           // Number: 123.45
TJSON.Bool(True)            // Boolean: true
TJSON.Null                  // Null: null
```

### Objects
```pascal
// Add values
Obj.Add('str', 'value')     // Add string
Obj.Add('num', 123)         // Add integer
Obj.Add('dec', 123.45)      // Add decimal
Obj.Add('bool', True)       // Add boolean
Obj.Add('null', TJSON.Null) // Add null

// Access values
Value := Obj['key']         // Get value
Exists := Obj.Contains('key')
Obj.Remove('key')           // Remove key
Count := Obj.Count          // Number of items
Keys := Obj.GetOrderedKeys  // Get keys in order
```

### Arrays
```pascal
// Add values
Arr.Add('text')            // Add string
Arr.Add(123)               // Add integer
Arr.Add(123.45)            // Add decimal
Arr.Add(True)              // Add boolean

// Access values
Value := Arr[0]            // Get value
Arr.Delete(0)              // Delete item
Arr.Clear                  // Remove all
Count := Arr.Count         // Number of items
```

### Type Safety
```pascal
// Safe type checking
if Value.IsString then S := Value.AsString
if Value.IsNumber then
begin
  if Frac(Value.AsNumber) = 0 then
    I := Value.AsInteger   // Only for whole numbers
  else
    D := Value.AsNumber    // For any number
end
if Value.IsBoolean then B := Value.AsBoolean
if Value.IsObject then O := Value.AsObject
if Value.IsArray then A := Value.AsArray
if Value.IsNull then ...  // Handle null
```

### Parsing & Formatting
```pascal
// Parse JSON
Value := TJSON.Parse('{"key":"value"}')
Success := TJSON.TryParse(JSON, Value)

// Format JSON
Pretty := Value.ToString(True)   // With indentation
Compact := Value.ToString(False) // Without whitespace
```

### Error Handling
```pascal
try
  Value := TJSON.Parse(JSON);
except
  on E: EJSONException do
    // Handle JSON errors
end;
```

## 📁File System Operations

```pascal
// Basic file operations
Content := TFileKit.ReadTextFile('input.txt');             // Read entire file
TFileKit.WriteTextFile('output.txt', 'content');           // Write to file
TFileKit.AppendText('file.txt', 'new content');            // Append text to file
TFileKit.PrependText('file.txt', 'prefix text');           // Add text at start
TFileKit.ReplaceText('file.txt', 'old', 'new');            // Replace text in file
TFileKit.DeleteFile('temp.txt');                           // Delete file
TFileKit.CopyFile('source.txt', 'dest.txt');               // Copy file with attributes
TFileKit.MoveFile('old.txt', 'new.txt');                   // Move/rename file

// File content operations
LineCount := TFileKit.CountLines('file.txt');              // Count lines in file
FirstLine := TFileKit.GetFirstLine('file.txt');            // Get the first line
LastLine := TFileKit.GetLastLine('file.txt');              // Get the last line
if TFileKit.ContainsText('file.txt', 'search term') then ... // Check if file contains text
Chunk := TFileKit.GetChunk('file.txt', 1024, 2048);        // Read 2048 bytes starting at offset 1024

// Batch file operations
TFileKit.CopyFiles('source_dir', 'dest_dir', '*.txt');     // Copy all .txt files
TFileKit.MoveFiles('source_dir', 'dest_dir', '*.doc');     // Move all .doc files
TFileKit.DeleteFiles('temp_dir', '*.tmp');                 // Delete all .tmp files

// Directory operations
TFileKit.CreateDirectory('new_dir');                       // Create directory
TFileKit.DeleteDirectory('old_dir', True);                 // Delete directory (True = recursive)
TFileKit.EnsureDirectory('path/to/file.txt');              // Create all parent directories

// File listing with sorting options
Files := TFileKit.ListFiles('.', '*', False);              // List files in current dir
Files := TFileKit.ListFiles('.', '*', True);               // List files recursively
Files := TFileKit.ListFiles('.', '*.txt');                 // List only .txt files
Files := TFileKit.ListFiles('.', '*', False, fsName);      // Sort by name (ascending)
Files := TFileKit.ListFiles('.', '*', False, fsNameDesc);  // Sort by name (descending)
Files := TFileKit.ListFiles('.', '*', False, fsDate);      // Sort by date (ascending)
Files := TFileKit.ListFiles('.', '*', False, fsDateDesc);  // Sort by date (descending)
Files := TFileKit.ListFiles('.', '*', False, fsSize);      // Sort by size (ascending)
Files := TFileKit.ListFiles('.', '*', False, fsSizeDesc);  // Sort by size (descending)

// Directory listing with sorting options
Dirs := TFileKit.ListDirectories('.', '*', False);         // List directories in current dir
Dirs := TFileKit.ListDirectories('.', '*', True);          // List directories recursively
Dirs := TFileKit.ListDirectories('.', 'test_*');           // List dirs matching pattern
Dirs := TFileKit.ListDirectories('.', '*', False, fsName); // Sort by name (ascending)
Dirs := TFileKit.ListDirectories('.', '*', False, fsDate); // Sort by date (ascending)

// Path operations
Path := TFileKit.GetFileName('path/to/file.txt');         // Returns 'file.txt'
Path := TFileKit.GetFileNameWithoutExt('file.txt');       // Returns 'file'
Path := TFileKit.GetDirectory('path/to/file.txt');        // Returns 'path/to'
Path := TFileKit.GetExtension('file.txt');                // Returns '.txt'
Path := TFileKit.GetParentDir('path/to/file.txt');        // Returns 'path/to'
Path := TFileKit.CombinePaths('path', 'file.txt');        // Combine paths
Path := TFileKit.NormalizePath('path/./to/../file.txt');  // Normalize path
Path := TFileKit.ChangeExtension('file.txt', '.doc');     // Change file extension
if TFileKit.IsAbsolutePath('C:\file.txt') then ...        // Check if path is absolute

// Path analysis
CommonPath := TFileKit.GetCommonPath('/usr/local/bin', '/usr/local/lib');  // Returns '/usr/local'
RelPath := TFileKit.GetRelativePath('/usr/share', '/usr/local/bin');       // Returns '../local/bin'
if TFileKit.IsSubPath('/usr/local', '/usr/local/bin') then ...             // Check if path is subpath

// File information
if TFileKit.Exists('file.txt') then ...                 // Check file exists
if TFileKit.DirectoryExists('dir') then ...             // Check directory exists
Size := TFileKit.GetSize('file.txt');                   // Get file size
Time := TFileKit.GetCreationTime('file.txt');           // Get creation time
Time := TFileKit.GetLastAccessTime('file.txt');         // Get last access time
Time := TFileKit.GetLastWriteTime('file.txt');          // Get last write time
Attrs := TFileKit.GetAttributes('file.txt');            // Get file attributes
if TFileKit.IsTextFile('file.txt') then ...             // Check if text file
if TFileKit.IsBinaryFile('image.jpg') then ...          // Check if binary file
if TFileKit.IsFileEmpty('empty.txt') then ...           // Check if file is empty
Mime := TFileKit.GetMimeType('document.pdf');           // Get MIME type (e.g., 'application/pdf')
if TFileKit.IsExecutable('app.exe') then ...            // Check if file is executable (platform-specific)
if TFileKit.IsHidden('system.dat') then ...             // Check if file is hidden
Encoding := TFileKit.GetFileEncoding('file.txt');       // Get file encoding
if TFileKit.IsEmptyDirectory('dir') then ...            // Check if directory is empty

// File comparison
if TFileKit.AreFilesIdentical('file1.txt', 'file2.txt') then ... // Check if files are identical
Newer := TFileKit.GetNewerFile('file1.txt', 'file2.txt');       // Get the path of the newer file
Diffs := TFileKit.GetFileDifferences('file1.txt', 'file2.txt'); // Get differences between text files

// Search operations
Results := TFileKit.SearchFiles('.', '*.txt', True);    // Search files recursively
Results := TFileKit.SearchFilesIn('dir', '*.txt', True);// Search in specific dir
File := TFileKit.FindLastModifiedFile('.', '*.txt');    // Find newest file
File := TFileKit.FindFirstModifiedFile('.', '*.txt');   // Find oldest file
File := TFileKit.FindLargestFile('.', '*.txt');         // Find largest file
File := TFileKit.FindSmallestFile('.', '*.txt');        // Find smallest file

// System directories
Dir := TFileKit.GetUserDir;                             // Get user directory
Dir := TFileKit.GetCurrentDir;                          // Get current directory
Dir := TFileKit.GetTempDir;                             // Get temp directory

// Drive information
FreeSpace := TFileKit.GetDriveFreeSpace('C:\');         // Get free space on drive (bytes)
Capacity := TFileKit.GetDriveCapacity('C:\');           // Get total capacity of drive (bytes)
if TFileKit.HasEnoughSpace('C:\', 1024*1024) then ...   // Check if drive has at least 1MB free

// Temporary files
TempFile := TFileKit.CreateTempFile('prefix_');         // Create temp file
TempDir := TFileKit.CreateTempDirectory('prefix_');     // Create temp directory

// Symbolic link operations
TFileKit.CreateSymLink('target.txt', 'link.txt');       // Create file symlink
TFileKit.CreateSymLink('target_dir', 'link_dir', True); // Create directory symlink
TFileKit.DeleteSymLink('link.txt');                     // Delete symlink
Path := TFileKit.ResolveSymLink('link.txt');            // Get target path
if TFileKit.IsSymLink('link.txt') then ...              // Check if path is symlink

// File locking
if TFileKit.LockFile('file.txt') then                   // Lock file
try
  // Work with file
finally
  TFileKit.UnlockFile('file.txt');                      // Unlock file
end;
if TFileKit.IsFileLocked('file.txt') then ...           // Check if file is locked

// File validation and sanitization
if TFileKit.IsValidFileName('file.txt') then ...        // Check if filename is valid
Name := TFileKit.SanitizeFileName('file*.txt');         // Sanitize filename
Path := TFileKit.MakeValidPath('/path//to/./file');     // Make path valid
if TFileKit.IsPathTooLong('very/long/path') then ...    // Check if path is too long

// Directory information
Info := TFileKit.GetDirectoryInfo('dir');               // Get directory statistics
WriteLn('Files: ', Info.FileCount);                     // Number of files
WriteLn('Directories: ', Info.DirectoryCount);          // Number of subdirectories
WriteLn('Total size: ', Info.TotalSize);                // Total size in bytes
WriteLn('Oldest file: ', Info.OldestFile);              // Name of oldest file
WriteLn('Newest file: ', Info.NewestFile);              // Name of newest file
WriteLn('Largest file: ', Info.LargestFile);            // Name of largest file

// Pattern matching
if TFileKit.MatchesPattern('test.txt', '*.txt') then ... // Check if filename matches pattern
File := TFileKit.FindFirstMatch('dir', '*.txt');         // Find first matching file
Count := TFileKit.CountMatches('dir', '*.txt');          // Count matching files

// Note: On Windows, creating symlinks requires Administrator privileges or Developer Mode
// On Unix/Linux, regular users can create symlinks in their own directories
```

## 🌐 HTTP Request Operations

```pascal
// Simple requests
Response := Http.Get('https://api.example.com/data');         // GET request
Response := Http.Post('https://api.example.com/data', 'text'); // POST with data
Response := Http.Put('https://api.example.com/data', 'text');  // PUT with data
Response := Http.Delete('https://api.example.com/data');       // DELETE request

// Status code and response handling
if Response.StatusCode = 200 then                             // Check status code
  WriteLn(Response.Text);                                     // Get response text
if Response.StatusCode = 200 then                             // Check status code
  JsonObj := Response.JSON.AsObject;                          // Get JSON object

// Error handling with try-except
try
  Response := Http.Get('https://api.example.com/secure');     // Request might fail
except
  on E: ERequestError do 
    WriteLn('HTTP Error: ', E.Message);                       // Handle HTTP errors
end;

// Error handling with Try-pattern (recommended)
Result := Http.TryGet('https://api.example.com/secure');      // Won't throw exceptions
if Result.Success then
  WriteLn('Status: ', Result.Response.StatusCode)             // Access successful response
else
  WriteLn('Error: ', Result.Error);                           // Get error message

// Fluent interface for complex requests
Response := THttpRequest.Create
  .Get                                                        // HTTP Method
  .URL('https://api.example.com/v2/data')                     // URL
  .AddHeader('X-API-Key', 'abc123')                           // Add header
  .AddHeader('Accept', 'application/json')                    // Add another header
  .AddParam('page', '1')                                      // Add query parameter
  .AddParam('limit', '10')                                    // Add another parameter
  .BasicAuth('username', 'password')                          // Add authentication
  .WithTimeout(5000)                                          // Set timeout in ms
  .Send;                                                      // Execute request

// POST JSON data
JsonObj := TJSON.Obj;                                          // Create JSON
JsonObj.Add('name', 'John');                                   // Add properties
JsonObj.Add('age', 30);
Response := Http.PostJSON('https://api.example.com/users',     // POST with JSON
  JsonObj.ToString);

// POST form data
Response := THttpRequest.Create
  .Post                                                        // POST method
  .URL('https://api.example.com/form')                         // URL
  .WithData('name=John&age=30')                                // Form data
  .Send;                                                       // Execute

// SSL/TLS HTTPS errors
Result := Http.TryGet('https://secure.example.com');           // Try HTTPS request
if not Result.Success and
   (Pos('OpenSSL', Result.Error) > 0) then                     // Check for SSL error
begin
  // Handle SSL library missing
  WriteLn('Please install OpenSSL libraries for HTTPS support');
  // On Linux: sudo apt-get install libssl-dev
end;

// OpenSSL requirements
// Windows: Included with application
// Linux: 
//   - Ubuntu/Debian: sudo apt-get install libssl-dev
//   - Fedora/RHEL: sudo dnf install openssl-devel
```

## 🧵String operations

```pascal
// ---------- Basic String Manipulation ----------

// Trimming and Whitespace
Str := TStringKit.Trim(Text);                     // Trim whitespace from both ends
Str := TStringKit.TrimLeft(Text);                 // Trim left whitespace
Str := TStringKit.TrimRight(Text);                // Trim right whitespace
Str := TStringKit.CollapseWhitespace(Text);       // Collapse multiple spaces to one
Str := TStringKit.RemoveWhitespace(Text);         // Remove all whitespace

// Case Conversion
Str := TStringKit.ToLower(Text);                  // Convert to lowercase
Str := TStringKit.ToUpper(Text);                  // Convert to uppercase
Str := TStringKit.CapitalizeText(Text);           // Capitalize first letter of each word

// Advanced Case Conversion
Str := TStringKit.ToTitleCase(Text);              // Convert to Title Case
Str := TStringKit.ToCamelCase(Text);              // Convert to camelCase
Str := TStringKit.ToPascalCase(Text);             // Convert to PascalCase
Str := TStringKit.ToSnakeCase(Text);              // Convert to snake_case
Str := TStringKit.ToKebabCase(Text);              // Convert to kebab-case

// Padding and Alignment
Str := TStringKit.PadLeft(Text, Width, Char);     // Left pad with character
Str := TStringKit.PadRight(Text, Width, Char);    // Right pad with character
Str := TStringKit.PadCenter(Text, Width, Char);   // Center pad with character

// Text Transformation
Str := TStringKit.ReverseText(Text);              // Reverse text
Str := TStringKit.DuplicateText(Text, Count);     // Duplicate text
Str := TStringKit.Truncate(Text, MaxLen, '...');  // Truncate with ellipsis

// ---------- String Inspection ----------

// Basic Tests
if TStringKit.IsEmpty(Text) then                  // Check if empty
Length := TStringKit.GetLength(Text);             // Get string length
Count := TStringKit.CountWords(Text);             // Count words

// Content Inspection
if TStringKit.Contains(Text, SubStr) then         // Check if contains substring
if TStringKit.StartsWith(Text, Prefix) then       // Check if starts with prefix
if TStringKit.EndsWith(Text, Suffix) then         // Check if ends with suffix
Count := TStringKit.CountSubString(Text, SubStr); // Count occurrences of substring

// Validation
if TStringKit.IsValidEmail('user@example.com') then       // Validate email address
if TStringKit.IsValidURL('https://example.com') then      // Validate URL
if TStringKit.IsValidIP('192.168.1.1') then               // Validate IP address (v4 or v6)
if TStringKit.IsValidIPv4('192.168.1.1') then             // Validate IPv4 address
if TStringKit.IsValidIPv6('::1') then                     // Validate IPv6 address
if TStringKit.IsValidDate('2024-01-15', 'yyyy-mm-dd') then // Validate date format

// ---------- Substring Operations ----------

// Extraction
Str := TStringKit.SubString(Text, Start, Length); // Extract substring
Str := TStringKit.LeftStr(Text, Length);          // Get left part
Str := TStringKit.RightStr(Text, Length);         // Get right part

// Splitting and Joining
Words := TStringKit.GetWords(Text);               // Split into words
Strings := TStringKit.Split('a,b,c', ',');                // Split by delimiter
Strings := TStringKit.Split('a,b,,c', ',', 0, True);      // Split and remove empty entries
Str := TStringKit.Join(['one', 'two', 'three'], ', ');    // Join with delimiter

// ---------- Pattern Matching and Replacement ----------

// Regex Operations
Matches := TStringKit.ExtractMatches(Text, Pattern);      // Extract regex matches with positions
Words := TStringKit.ExtractAllMatches(Text, Pattern);     // Extract regex matches as strings
if TStringKit.MatchesPattern(Text, Pattern) then          // Check regex pattern
Str := TStringKit.ReplaceRegEx(Text, Pattern, Replace);   // Replace using regex

// Simple Replacement
Str := TStringKit.ReplaceText(Text, Old, New);            // Replace all occurrences

// ---------- Formatting and Conversion ----------

// Number Formatting
Str := TStringKit.FormatNumber(1234);                     // Format to "1,234"
Str := TStringKit.FormatFloat(1234.56, 2, '.', ',');      // Format to "1,234.56"
Str := TStringKit.FormatFileSize(1048576);                // Format to "1.00 MB"

// Number Conversions
Roman := TStringKit.ToRoman(1984);                        // Convert to Roman numerals (MCMLXXXIV)
Num := TStringKit.FromRoman('MMXXIV');                    // Convert from Roman numerals (2024)
Ordinal := TStringKit.ToOrdinal(21);                      // Convert to ordinal (21st)
Words := TStringKit.NumberToWords(42);                    // Convert to words (forty-two)

// ---------- String Similarity and Distance ----------

// Distance Metrics
Dist := TStringKit.LevenshteinDistance(S1, S2);        // Edit distance between strings
Dist := TStringKit.HammingDistance(S1, S2);            // Character differences at same positions

// Similarity Metrics (0-1 scale, higher is more similar)
Sim := TStringKit.LevenshteinSimilarity(S1, S2);       // Normalized similarity
Sim := TStringKit.JaroSimilarity(S1, S2);              // Jaro similarity for short strings
Sim := TStringKit.JaroWinklerSimilarity(S1, S2);       // Jaro-Winkler with prefix bonus
Sim := TStringKit.LCSSimilarity(S1, S2);               // LCS similarity ratio

// Common Subsequence
LCS := TStringKit.LongestCommonSubsequence(S1, S2);    // Longest common subsequence

// Fuzzy Matching
if TStringKit.IsFuzzyMatch(S1, S2) then               // Default: Levenshtein, threshold 0.7
if TStringKit.IsFuzzyMatch(S1, S2, 0.8) then          // Custom threshold
if TStringKit.IsFuzzyMatch(S1, S2, 0.7, 1) then       // Using Jaro-Winkler (method=1)
if TStringKit.IsFuzzyMatch(S1, S2, 0.7, 2) then       // Using LCS similarity (method=2)

// ---------- Phonetic Algorithms ----------

// Phonetic Encoding
Code := TStringKit.Soundex('Smith');                   // Get Soundex code (S530)
Code := TStringKit.Metaphone('Smith');                 // Get Metaphone code (SM0)

// ---------- Text Analysis ----------

// Readability and Analysis
Score := TStringKit.FleschKincaidReadability(Text);    // Calculate readability (0-100)
NGrams := TStringKit.GenerateNGrams(Text, 2);          // Generate bigrams

// ---------- Encoding/Decoding ----------

// HTML and URL Encoding
Encoded := TStringKit.HTMLEncode('<div>');             // HTML encoding
Decoded := TStringKit.HTMLDecode('&lt;div&gt;');       // HTML decoding
Encoded := TStringKit.URLEncode('a b');                // URL encoding (a+b)
Decoded := TStringKit.URLDecode('a+b');                // URL decoding

// Base64 and Hex Encoding
HexStr := TStringKit.HexEncode('abc');                   // Hex encoding (616263)
Original := TStringKit.HexDecode('616263');              // Hex decoding
```

## 📚 Collection Operations

### List Operations

```pascal
// TList<T> - Generic List Operations
List := TList<Integer>.Create;              // Create a new list
List := TList<Integer>.New;                 // Create list with interface (auto-cleanup)
IList := CreateList<Integer>;               // Alternative interface creation helper

// Basic Methods
List.Add(Item)                              // Add item to list, returns index
List.AddRange([Item1, Item2, Item3])        // Add multiple items
List.Insert(Index, Item)                    // Insert at specific position
List.Delete(Index)                          // Delete by index
List.Remove(Item, @EqualityFunc)            // Remove by value using equality function
List.Clear                                  // Remove all items

// Search & Access
Value := List[Index]                        // Get item by index
List[Index] := Value                        // Set item by index
Count := List.Count                         // Get number of items
List.Capacity := Value                      // Set/get allocated capacity
Index := List.IndexOf(Item, @EqualityFunc)  // Find item index
Found := List.Contains(Item, @EqualityFunc) // Check if item exists
Success := List.Find(@Predicate, FoundValue) // Find by predicate
Results := List.FindAll(@Predicate)         // Get all matching items

### HashSet<T> Operations
```pascal
// Creation
THashSet<T>.New(HashFunc: THashFunc<T>; EqualityFunc: TEqualityFunc<T>; 
  InitialCapacity: Integer = 16; LoadFactor: Single = 0.75): IHashSet<T>;

// Basic Operations
function Add(const Value: T): Boolean;
function Remove(const Value: T): Boolean;
function Contains(const Value: T): Boolean;
procedure Clear;
function ToArray: TArray<T>;
property Count: Integer read GetCount;

// Enumeration
for Item in HashSet do ...
```

### Deque<T> Operations
```pascal
// Creation
TDeque<T>.New: IDeque<T>;

// Basic Operations
procedure PushFront(const Value: T);
procedure PushBack(const Value: T);
function PopFront: T;
function PopBack: T;
function PeekFront: T;
function PeekBack: T;
procedure Clear;
function ToArray: TArray<T>;

// Search Operations
function Contains(const Value: T; EqualityFunc: TEqualityFunc<T>): Boolean;
function IndexOf(const Value: T; EqualityFunc: TEqualityFunc<T>): Integer;

// Transformation
procedure Reverse;

// Properties
property Count: Integer read GetCount;
property Capacity: Integer read GetCapacity write SetCapacity;
property Items[Index: Integer]: T read GetItem write SetItem; default;

// Enumeration
for Item in Deque do ...
```

### Dictionary<K, V> Operations
```pascal
// Creation
TDictionary<K, V>.New(KeyHashFunc: TKeyHashFunc<K>; KeyEqualityFunc: TKeyEqualityFunc<K>;
  InitialCapacity: Integer = 16; LoadFactor: Single = 0.75): IDictionary<K, V>;
IDictionary<K, V> := CreateDictionary<K, V>(@HashFunction, @EqualityFunction);

// Basic Operations
function Add(const Key: K; const Value: V): Boolean; // Returns true if added
function Remove(const Key: K): Boolean;              // Returns true if removed
function ContainsKey(const Key: K): Boolean;        // Check if key exists
function TryGetValue(const Key: K; out Value: V): Boolean; // Safe key lookup
procedure Clear;                                   // Remove all entries

// Access Operations
Value := Dict[Key];                               // Get value by key (raises exception if not found)
Dict[Key] := Value;                              // Set value by key (adds if key doesn't exist)
Count := Dict.Count;                             // Get number of entries

// Bulk Operations
Keys := Dict.GetKeys;                             // Get array of all keys
Values := Dict.GetValues;                         // Get array of all values
```

### Hash Functions (TidyKit.Collections.HashFunction)
```pascal
// String Hashing
function XXHash32(const Key: string): Integer;     // Best for strings > 64 bytes
function FNV1aHash(const Key: string): Integer;    // Good for short strings

// Numeric Hashing
function MultiplicativeHash(const Key: Integer): Integer;
function FloatHash(const Value: Extended): Integer;
function Int64Hash(const Value: Int64): Integer;

// Other Types
function BooleanHash(const Value: Boolean): Integer;
function DateTimeHash(const Value: TDateTime): Integer;
function CharHash(const Value: Char): Integer;

// Generic Fallback
function DefaultHash(const Key; Size: Integer): Integer;
```

### Equality Functions (TidyKit.Collections.EqualityFunction)
```pascal
// Basic Types
function TidyKitStringEquals(const A, B: string): Boolean;
function TidyKitIntegerEquals(const A, B: Integer): Boolean;
function TidyKitFloatEquals(const A, B: Extended): Boolean;
function TidyKitBooleanEquals(const A, B: Boolean): Boolean;
function TidyKitDateTimeEquals(const A, B: TDateTime): Boolean;
function TidyKitCharEquals(const A, B: Char): Boolean;
function TidyKitInt64Equals(const A, B: Int64): Boolean;

// Complex Types
function TidyKitPointEquals(const A, B: TPoint): Boolean;
function TidyKitRectEquals(const A, B: TRect): Boolean;
```

// Transformations
List.Sort(@CompareFunc)                     // Sort using comparison function
List.Reverse                                // Reverse list order
Slice := List.Slice(StartIdx, Count)        // Get subrange as array
Array := List.ToArray                       // Convert to array

// Function Types
// TEqualityFunc<T> = function(const A, B: T): Boolean
// TCompareFunc<T> = function(const A, B: T): Integer
// TPredicateFunc<T> = function(const Item: T): Boolean of object
```

## 🕙 DateTime Operations

### Basic Operations
```pascal
Now := TDateTimeKit.GetNow;                         // Current date and time
Today := TDateTimeKit.GetToday;                     // Current date (time = 00:00:00)
UnixTimestamp := TDateTimeKit.ToUnixTime(Now);      // Convert to Unix timestamp
DateTime := TDateTimeKit.FromUnixTime(1634567890);  // Convert from Unix timestamp
FormattedDate := TDateTimeKit.GetAsString(Now, 'yyyy-mm-dd hh:nn:ss');  // Format date
```

### Component Access
```pascal
Year := TDateTimeKit.GetYear(Now);                  // Extract year
Month := TDateTimeKit.GetMonth(Now);                // Extract month
Day := TDateTimeKit.GetDay(Now);                    // Extract day
DayOfWeek := TDateTimeKit.GetDayOfWeek(Now);        // Get day of week (1=Sunday)
DayOfYear := TDateTimeKit.GetDayOfYear(Now);        // Get day of year (1-366)
WeekOfYear := TDateTimeKit.GetWeekOfYear(Now);      // Get week number (1-53)
Hour := TDateTimeKit.GetHour(Now);                  // Extract hour
Minute := TDateTimeKit.GetMinute(Now);              // Extract minute
Second := TDateTimeKit.GetSecond(Now);              // Extract second
Millisecond := TDateTimeKit.GetMillisecond(Now);    // Extract millisecond
```

### Component Modification
```pascal
// Set individual components
NewDate := TDateTimeKit.SetYear(Now, 2024);
NewDate := TDateTimeKit.SetMonth(Now, 5);
NewDate := TDateTimeKit.SetDay(Now, 15);
NewDate := TDateTimeKit.SetHour(Now, 10);
NewDate := TDateTimeKit.SetMinute(Now, 30);
NewDate := TDateTimeKit.SetSecond(Now, 45);
NewDate := TDateTimeKit.SetMillisecond(Now, 500);

// Create date from components
Date := TDateTimeKit.CreateDate(2024, 5, 15);
Time := TDateTimeKit.CreateTime(10, 30, 45, 500);
DateTime := TDateTimeKit.CreateDateTime(2024, 5, 15, 10, 30, 45, 500);
```

### Date Arithmetic
```pascal
// Add or subtract time units
Tomorrow := TDateTimeKit.AddDays(Now, 1);
Yesterday := TDateTimeKit.AddDays(Now, -1);
NextWeek := TDateTimeKit.AddWeeks(Now, 1);
NextMonth := TDateTimeKit.AddMonths(Now, 1);
NextYear := TDateTimeKit.AddYears(Now, 1);
OneHourLater := TDateTimeKit.AddHours(Now, 1);
ThirtyMinutesAgo := TDateTimeKit.AddMinutes(Now, -30);

// Calculate differences
DaysDiff := TDateTimeKit.DaysBetween(Date1, Date2);
MonthsDiff := TDateTimeKit.MonthsBetween(Date1, Date2);
YearsDiff := TDateTimeKit.YearsBetween(Date1, Date2);
HoursDiff := TDateTimeKit.HoursBetween(Time1, Time2);
MinutesDiff := TDateTimeKit.MinutesBetween(Time1, Time2);
SecondsDiff := TDateTimeKit.SecondsBetween(Time1, Time2);
MillisecondsDiff := TDateTimeKit.MillisecondsBetween(Time1, Time2);
```

### Period Operations
```pascal
// Create periods
Period := TDateTimeKit.CreatePeriod(1, 2, 15);  // 1 year, 2 months, 15 days
Duration := TDateTimeKit.CreateDuration(0, 0, 0, 5, 30, 0, 0);  // 5h 30m

// Add periods to dates
NewDate := TDateTimeKit.AddSpan(Now, Period);

// Convert between periods and durations
Duration := TDateTimeKit.PeriodToDuration(Period, StartDate);
Period := TDateTimeKit.DurationToPeriod(Duration);
```

### Interval Operations
```pascal
// Create intervals
Interval := TDateTimeKit.CreateInterval(StartDate, EndDate);

// Check if date is within interval
if TDateTimeKit.DateInInterval(TestDate, Interval) then ...

// Get interval duration
Duration := TDateTimeKit.IntervalDuration(Interval);

// Interval operations
Union := TDateTimeKit.UnionIntervals(Interval1, Interval2);
Intersection := TDateTimeKit.IntersectIntervals(Interval1, Interval2);
```

### Date Comparison
```pascal
// Compare dates
if TDateTimeKit.SameDate(Date1, Date2) then ...
if TDateTimeKit.SameTime(Time1, Time2) then ...
if TDateTimeKit.SameDateTime(DateTime1, DateTime2) then ...

// Compare with tolerance
if TDateTimeKit.NearDateTime(DateTime1, DateTime2, 1/SecondsPerDay) then ...  // Within 1 second

// Comparison with components
if TDateTimeKit.SameMonth(Date1, Date2) then ...
if TDateTimeKit.SameYear(Date1, Date2) then ...
if TDateTimeKit.SameYearMonth(Date1, Date2) then ...

// Min/Max operations
EarlierDate := TDateTimeKit.MinDateTime(Date1, Date2);
LaterDate := TDateTimeKit.MaxDateTime(Date1, Date2);
```

### Period Boundaries
```pascal
// Start of time period
StartOfDay := TDateTimeKit.StartOfDay(Now);
StartOfWeek := TDateTimeKit.StartOfWeek(Now);
StartOfMonth := TDateTimeKit.StartOfMonth(Now);
StartOfQuarter := TDateTimeKit.StartOfQuarter(Now);
StartOfYear := TDateTimeKit.StartOfYear(Now);

// End of time period
EndOfDay := TDateTimeKit.EndOfDay(Now);
EndOfWeek := TDateTimeKit.EndOfWeek(Now);
EndOfMonth := TDateTimeKit.EndOfMonth(Now);
EndOfQuarter := TDateTimeKit.EndOfQuarter(Now);
EndOfYear := TDateTimeKit.EndOfYear(Now);
```

### Date Rounding
```pascal
// Round down (floor)
RoundToSecond := TDateTimeKit.FloorDate(Now, duSecond);  // 12:34:56.789 -> 12:34:56.000
RoundToMinute := TDateTimeKit.FloorDate(Now, duMinute);  // 12:34:56.789 -> 12:34:00.000
RoundToHour := TDateTimeKit.FloorDate(Now, duHour);      // 12:34:56.789 -> 12:00:00.000
RoundToDay := TDateTimeKit.FloorDate(Now, duDay);        // 2023-04-15 12:34 -> 2023-04-15 00:00
RoundToWeek := TDateTimeKit.FloorDate(Now, duWeek);      // 2023-04-15 -> start of week
RoundToMonth := TDateTimeKit.FloorDate(Now, duMonth);    // 2023-04-15 -> 2023-04-01
RoundToQuarter := TDateTimeKit.FloorDate(Now, duQuarter);// 2023-04-15 -> 2023-01-01
RoundToYear := TDateTimeKit.FloorDate(Now, duYear);      // 2023-04-15 -> 2023-01-01

// Round up (ceiling)
RoundToSecond := TDateTimeKit.CeilingDate(Now, duSecond); // 12:34:56.789 -> 12:34:57.000
RoundToMinute := TDateTimeKit.CeilingDate(Now, duMinute); // 12:34:56.789 -> 12:35:00.000
RoundToHour := TDateTimeKit.CeilingDate(Now, duHour);     // 12:34:56.789 -> 13:00:00.000
RoundToDay := TDateTimeKit.CeilingDate(Now, duDay);       // 2023-04-15 12:34 -> 2023-04-16 00:00
RoundToWeek := TDateTimeKit.CeilingDate(Now, duWeek);     // 2023-04-15 -> end of week + 1 second
RoundToMonth := TDateTimeKit.CeilingDate(Now, duMonth);   // 2023-04-15 -> 2023-05-01
RoundToQuarter := TDateTimeKit.CeilingDate(Now, duQuarter);// 2023-04-15 -> 2023-04-01
RoundToYear := TDateTimeKit.CeilingDate(Now, duYear);     // 2023-04-15 -> 2024-01-01

// Round to nearest
RoundToHour := TDateTimeKit.RoundDate(Now, duHour);      // Rounds to nearest hour
```

### Timezone Operations
```pascal
// Timezone information
TZInfo := TDateTimeKit.GetTimeZone(Now);
WriteLn('Timezone: ', TZInfo.Name);
WriteLn('Offset: ', TZInfo.Offset, ' minutes');
WriteLn('DST: ', BoolToStr(TZInfo.IsDST, True));

// Get system timezone
SystemTZ := TDateTimeKit.GetSystemTimeZone;

// List available timezones
TZNames := TDateTimeKit.GetTimeZoneNames;
for I := Low(TZNames) to High(TZNames) do
  WriteLn(TZNames[I]);

// Convert between timezones
UTCTime := TDateTimeKit.WithTimeZone(LocalTime, 'UTC');
LocalTime := TDateTimeKit.WithTimeZone(UTCTime, 'America/New_York');

// Force timezone interpretation
ForcedTime := TDateTimeKit.ForceTimeZone(AnyTime, 'Europe/London');

// Cross-platform environment variable handling
// Save original timezone
OriginalTZ := GetEnvVar('TZ');
try
  // Set timezone for testing
  SetEnvVar('TZ', 'America/New_York');
  // Now timezone operations will use this setting...
  
  // Special date checks
  DSTDate := EncodeDateTime(2024, 10, 6, 2, 0, 0, 0); // First Sunday in October
  TZInfo := TDateTimeKit.GetTimeZone(DSTDate);
  if TZInfo.IsDST then
    WriteLn('Australian DST is in effect');
finally
  // Restore original timezone
  SetEnvVar('TZ', OriginalTZ);
end;

// Handling timezone edge cases
// DST transitions for US, EU, Australia, and other regions are correctly handled
// Example: Check if specific date is in DST period
// First create date values near DST transition points
AustralianDate := EncodeDateTime(2024, 10, 6, 2, 0, 0, 0); // First Sunday in October
USDate := EncodeDateTime(2024, 3, 10, 2, 0, 0, 0);        // Second Sunday in March
EUDate := EncodeDateTime(2024, 3, 31, 1, 0, 0, 0);        // Last Sunday in March

// Then check if each date is in DST period using GetTimeZone
AUInfo := TDateTimeKit.GetTimeZone(AustralianDate);
WriteLn('Australia DST active: ', BoolToStr(AUInfo.IsDST, True));

USInfo := TDateTimeKit.GetTimeZone(USDate);
WriteLn('US DST active: ', BoolToStr(USInfo.IsDST, True));

EUInfo := TDateTimeKit.GetTimeZone(EUDate);
WriteLn('EU DST active: ', BoolToStr(EUInfo.IsDST, True));
```

## 🔒 Cryptographic Operations

### Hash Functions
```pascal
// MD5 (legacy support)
Hash := TCryptoKit.MD5Hash('text');                // Returns MD5 hash

// SHA1 (legacy support)
Hash := TCryptoKit.SHA1Hash('text');               // Returns SHA1 hash

// SHA2 Family
Hash := TCryptoKit.SHA256Hash('text');             // Returns SHA-256 hash
Hash := TCryptoKit.SHA512Hash('text');             // Returns SHA-512 hash
Hash := TCryptoKit.SHA512_256Hash('text');         // Returns SHA-512/256 hash

// SHA3 Family (Keccak)
Hash := TCryptoKit.SHA3_224Hash('text');           // Returns SHA3-224 hash
Hash := TCryptoKit.SHA3_256Hash('text');           // Returns SHA3-256 hash
Hash := TCryptoKit.SHA3_384Hash('text');           // Returns SHA3-384 hash
Hash := TCryptoKit.SHA3_512Hash('text');           // Returns SHA3-512 hash
```

### Base64 Encoding/Decoding
```pascal
// Base64 encoding/decoding
Encoded := TCryptoKit.Base64Encode('text');        // Encode to Base64
Decoded := TCryptoKit.Base64Decode(Encoded);       // Decode from Base64
```

### XOR Encryption (Basic)
```pascal
// Simple XOR encryption (not secure for sensitive data)
Encrypted := TCryptoKit.XORCrypt('text', 'key');   // Encrypt text
Decrypted := TCryptoKit.XORCrypt(Encrypted, 'key'); // Decrypt text (same operation)
```

### Blowfish Encryption (Legacy Support)
```pascal
// Blowfish encryption/decryption
Encrypted := TCryptoKit.BlowfishCrypt('text', 'key', bmEncrypt); // Encrypt
Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, 'key', bmDecrypt); // Decrypt
```

### AES-256 Encryption

```pascal
// High-level interface (with automatic Base64 encoding)
var
  Key: TAESKey;
  IV: TAESBlock;
  PlainText, CipherText: string;
begin
  // Generate secure key and IV
  Key := TCryptoKit.GenerateRandomKey;
  IV := TCryptoKit.GenerateIV;
  
  // CBC Mode (with PKCS7 padding)
  CipherText := TCryptoKit.AES256EncryptCBC('secret text', Key, IV);
  PlainText := TCryptoKit.AES256DecryptCBC(CipherText, Key, IV);
  
  CipherText := TCryptoKit.AES256EncryptCTR('secret text', Key, IV);
  PlainText := TCryptoKit.AES256DecryptCTR(CipherText, Key, IV);
end;

// Low-level interface (raw binary operations)
var
  Key: TAESKey;
  IV: TAESBlock;
  PlainBytes, CipherBytes: TBytes;
begin
  // Generate secure key and IV
  Key := TCryptoKit.GenerateRandomKey;
  IV := TCryptoKit.GenerateIV;
  
  // CBC Mode with configurable padding
  CipherBytes := TAES256.EncryptCBC(PlainBytes, Key, IV, apPKCS7); // With PKCS7 padding
  PlainBytes := TAES256.DecryptCBC(CipherBytes, Key, IV, apPKCS7);
  
  CipherBytes := TAES256.EncryptCBC(PlainBytes, Key, IV, apNone);  // No padding (for NIST vectors)
  PlainBytes := TAES256.DecryptCBC(CipherBytes, Key, IV, apNone);
  
  // CTR Mode (no padding needed)
  CipherBytes := TAES256.EncryptCTR(PlainBytes, Key, IV);
  PlainBytes := TAES256.DecryptCTR(CipherBytes, Key, IV);
end;

// Key derivation and generation
var
  Key: TAESKey;
  IV: TAESBlock;
begin
  // Generate random key and IV
  Key := TCryptoKit.GenerateRandomKey;
  IV := TCryptoKit.GenerateIV;
  
  // Derive key from password
  Key := TCryptoKit.DeriveKey('password', 'salt', 100000); // PBKDF2-SHA256
end;
```

### Common Use Cases

1. Password Hashing
```pascal
// Hash password before storage (use SHA-512 or SHA3-512)
HashedPassword := TCryptoKit.SHA512Hash(Password);
```

2. File Integrity Check
```pascal
// Calculate file hash
FileHash := TCryptoKit.SHA256Hash(TFileKit.ReadFile('file.txt'));
```

3. Secure Data Storage
```pascal
var
  Key: TAESKey;
  IV: TAESBlock;
  EncryptedData: string;
begin
  // Generate secure key and IV
  Key := TCryptoKit.GenerateRandomKey;
  IV := TCryptoKit.GenerateIV;
  
  // Encrypt and store (uses PKCS7 padding)
  EncryptedData := TCryptoKit.AES256EncryptCBC(SensitiveData, Key, IV);
  TFileKit.WriteFile('secure.dat', EncryptedData);
end;
```

### Best Practices

1. Key Management
   - Use `TCryptoKit.GenerateRandomKey` for secure key generation
   - Use `TCryptoKit.DeriveKey` for password-based keys
   - Never store encryption keys in source code
   - Rotate keys periodically
   - Securely erase keys from memory when done

2. IV (Initialization Vector) Handling
   - Use `TCryptoKit.GenerateIV` for secure IV generation
   - Use a unique IV for each encryption operation
   - Never reuse IVs with the same key
   - Store IV alongside encrypted data (it's not secret)

3. Mode Selection
   - Use CBC mode with PKCS7 padding for general encryption
   - Use CTR mode for streaming or random access
   - Use raw mode (apNone) only for NIST compliance testing

4. Hash Selection
   - Use SHA-256 or better for general hashing
   - Use SHA-512 for password hashing (with proper salting)
   - Avoid MD5 and SHA1 for security-critical operations

### Security Notes

1. Encryption Strength
   - AES-256 provides 256-bit security
   - Key size determines security level
   - Mode of operation affects security properties

2. Known Limitations
   - CBC mode requires padding (potential padding oracle attacks)
   - CTR mode requires unique counter values
   - XOR encryption is not cryptographically secure

3. Compliance
   - AES-256 implementation follows NIST standards
   - Supports FIPS-compliant modes of operation
   - Includes PKCS7 padding for CBC mode
   - Raw mode available for NIST test vectors
```

## 📝 Logging Operations

### Getting Started with TidyKit.Logger

```pascal
// STEP 1: Choose how to set up your logger

// Option A: Use the global logger singleton (recommended for most applications)
uses
  TidyKit.Logger;
  
// Set up once at application startup
TLogger.CreateConsoleAndFileLogger('application.log', llInfo);

// Then use anywhere in your code through the Logger function
Logger.Info('Application started');


// Option B: Create your own logger instance (for specialized components)
var
  ComponentLogger: TLogger;
begin
  ComponentLogger := TLogger.CreateFileLogger('component.log', llDebug);
  ComponentLogger.Debug('Component initialized');
end;


// STEP 2: Choose the appropriate log level based on the information
Logger.Debug('Detailed debug info - only for development');     // Use during development
Logger.Info('Normal operational messages');                     // General information
Logger.Warning('Something unusual happened');                   // Potential issues
Logger.Error('An operation failed');                            // Recoverable errors
Logger.Fatal('Critical error - application cannot continue');   // Critical failures


// STEP 3: Include variables in log messages (two equivalent styles)
Logger.InfoFmt('User %s logged in from %s', ['john', '192.168.1.10']);  // Format style
Logger.Info('User %s logged in from %s', ['john', '192.168.1.10']);     // Same result


// STEP 4: Organize logs by component/feature (optional)
var
  UILogger, DBLogger: ILogContext;
begin
  // Create category-based loggers for better organization
  UILogger := Logger.CreateContext('UI');
  DBLogger := Logger.CreateContext('DB');
  
  UILogger.Info('Window created');
  DBLogger.Warning('Slow query detected: %s', ['SELECT * FROM large_table']);
end;


// STEP 5: Measure operation performance (optional)
procedure ProcessData;
var
  Timer: ITimedOperation;
begin
  // Create a timer that logs when it goes out of scope
  Timer := Logger.TimedBlock('Data processing');
  
  // Do your work...
  // When this procedure ends, it logs something like:
  // "Data processing completed in 235ms"
end;


// STEP 6: Clean up when finished (important!)
procedure Shutdown;
begin
  Logger.Info('Application shutting down');
  Logger.CloseLogFiles;  // Closes files and flushes any pending messages
end;

// Program shutdown best practices:
// - CloseLogFiles is sufficient for normal program termination
// - ResetInstance is NOT required at program shutdown
// - The logger will automatically clean up when your application terminates
// - For guaranteed data integrity, explicitly call CloseLogFiles
try
  // Your application code
finally
  Logger.CloseLogFiles;  // This is sufficient for cleanup
end;
```

### Common Scenarios

```pascal
// FOR DEVELOPMENT: Include detailed debug information
TLogger.CreateConsoleLogger(llDebug);
Logger.Debug('Connection status: %d, buffer size: %d', [status, size]);

// FOR PRODUCTION: Filter out debug noise
TLogger.CreateConsoleAndFileLogger('app.log', llInfo);
Logger.SetMinLogLevel(llWarning);  // Only show warnings and above

// FOR TROUBLESHOOTING: Show file and line information
TLogger.CreateDebugLogger;  // Special logger that includes source file info
Logger.Debug('Variable value: %d', [value]);  // Shows file:line in output

// FOR SECURITY EVENTS: Track login attempts, permission changes, etc.
AuditLogger := TLogger.CreateAuditLogger('security.log');
AuditLogger.Warning('Failed login attempt for user %s from %s', [username, ip]);

// FOR HIGH-VOLUME LOGGING: Use batch mode for performance
Logger.BeginBatch;
try
  for i := 1 to 1000 do
    Logger.Info('Processing item ' + IntToStr(i));
finally
  Logger.EndBatch;  // Writes all messages at once for better performance
end;

// FOR TESTING: Capture logs in memory
var
  MemSink: TMemorySink;
begin
  MemSink := TMemorySink.Create(100);  // Keep last 100 messages
  Logger.AddSink(MemSink);
  
  // Later, retrieve messages for assertions
  Assert(MemSink.GetMessages.Count > 0);
end;
```

### Advanced Use
```pascal
// Configure with method chaining
Logger
  .SetLogDestinations([ldConsole, ldFile])
  .SetMinLogLevel(llInfo)
  .SetDateTimeFormat('yyyy-mm-dd hh:nn:ss')
  .SetFormat('[%time] [%level] %message');
  
// Custom message format patterns
Logger.SetFormat('[%time] [%level] [%category] %message');        // With category
Logger.SetFormat('[%time] [%level] [%file:%line] %message');      // With source location
Logger.SetFormat('[%time] [%level] [Thread %threadid] %message'); // With thread ID

// Add log files with size limits
LogIndex := Logger.AddLogFile('app.log', 25 * 1024 * 1024);      // 25MB limit
LogIndex := Logger.AddDefaultLogFile('system');                   // Creates logs/system.log
LogIndex := Logger.AddDefaultLogFile('errors', 5 * 1024 * 1024); // 5MB error log

// Structured logging with key-value pairs
Logger.LogStructured(llInfo, 'User login', [
  NameValuePair('username', 'john_doe'),
  NameValuePair('ip_address', '192.168.1.10'),
  NameValuePair('success', True),
  NameValuePair('attempt', 3)
]);

// Type-specific value logging
Logger.LogValue('count', 42, llInfo);            // Integer value
Logger.LogValue('temperature', 98.6, llInfo);    // Double value
Logger.LogValue('is_active', True, llInfo);      // Boolean value
Logger.LogValue('username', 'john_doe', llInfo); // String value

// Custom sink management
Logger.AddSink(TConsoleSink.Create);
Logger.AddSink(TFileSink.Create('app.log'));
Logger.AddSink(TRotatingFileSink.Create('app.log', 1024*1024, 5)); // 1MB, keep 5 files
Logger.AddSink(TDailyFileSink.Create('app.log'));                  // Rotates daily
Logger.AddSink(TMemorySink.Create(100));                           // Keep last 100 messages

// Configuration from environment or file
Logger.ConfigureFromEnvironment;  // LOGGER_LEVEL, LOGGER_DESTINATIONS, etc.
Logger.LoadConfiguration('logger.ini');

// Instance management
ID := Logger.GetInstanceID;  // Get unique ID of current logger instance
TLogger.ResetInstance;       // Destroy and recreate singleton instance

// When to use ResetInstance:
// 1. Complete reconfiguration - when you need totally different settings
// 2. Between test cases - ensures test isolation
// 3. Application phase changes - switching from init to runtime logging
// 4. Final cleanup - to release ALL logger resources

// Error recovery - logger attempts to handle errors gracefully
try
  Logger.AddLogFile('/invalid/path/file.log');  // Will handle error and continue
except
  on E: Exception do
    WriteLn('Error handled: ', E.Message);
end;
```

## 📁 Archive Operations

```pascal
// Archive operations
TArchiveKit.CompressToZip('source.txt', 'archive.zip');                // Compress single file
TArchiveKit.CompressToZip('sourcedir', 'archive.zip', True);          // Compress recursively
TArchiveKit.CompressToZip('sourcedir', 'archive.zip', True, '*.txt'); // Compress only .txt files
TArchiveKit.DecompressFromZip('archive.zip', 'destdir');              // Extract all files
TArchiveKit.DecompressFromZip('archive.zip', 'destdir', '*.txt');     // Extract only .txt files

TArchiveKit.CompressToTar('source.txt', 'archive.tar');               // Create TAR with single file
TArchiveKit.CompressToTar('sourcedir', 'archive.tar', True);          // Create TAR recursively
TArchiveKit.CompressToTar('sourcedir', 'archive.tar', True, '*.txt'); // Create TAR with only .txt files
TArchiveKit.DecompressFromTar('archive.tar', 'destdir');              // Extract all files
TArchiveKit.DecompressFromTar('archive.tar', 'destdir', '*.txt');     // Extract only .txt files
```

## 🛠️ Error Handling

TidyKit uses module-specific exception classes for better error granularity:

```pascal
// Module-specific exceptions
try
  // JSON operations
  JsonValue := TJSON.Parse(JsonStr);
except
  on E: EJSONException do
    // Handle JSON-specific errors
end;

try
  // File operations
  TFileKit.CopyFile(Source, Dest);
except
  on E: EFileSystemError do
    // Handle filesystem-specific errors
end;

try
  // HTTP operations
  Response := Http.Get(URL);
except
  on E: ERequestError do
    // Handle HTTP-specific errors
end;

try
  // Statistical operations
  Mean := TStatsKit.Mean(Data);
except
  on E: EStatsError do
    // Handle statistics-specific errors (e.g., empty dataset)
end;

try
  // Cryptographic operations
  Encrypted := TCryptoKit.AES256EncryptCBC(Data, Key, IV);
except
  on E: ECryptoError do
    // Handle general crypto errors
  on E: EAESError do
    // Handle AES-specific errors
end;

try
  // Archive operations
  TArchiveKit.CompressToZip(Path, ZipFile);
except
  on E: EArchiveError do
    // Handle archive-specific errors
end;

try
  // Logger operations
  Logger.Info('Message');
except
  on E: ELoggerException do
    // Handle logger-specific errors
end;
```

This modular approach makes error handling more precise and maintainable.

## ⚙️ Command-Line Argument Parsing

```pascal
var
  Parser: TArgParser;
begin
  Parser.Init;
  Parser.AddString('n', 'name', 'Your name', 'Guest');
  Parser.AddInteger('a', 'age', 'Your age', 30);
  Parser.Parse(ParamStrArray);
  if Parser.HasError then
    WriteLn('Error: ', Parser.Error)
  else
    WriteLn('Name: ', Parser.GetString('name'), ', Age: ', Parser.GetInteger('age'));
end;