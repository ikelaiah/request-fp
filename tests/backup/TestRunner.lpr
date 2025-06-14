program TestRunner;

{$mode objfpc}{$H+}{$J-}

uses
  Classes
  , consoletestrunner
  , TidyKit.DateTime.Test
  , TidyKit.FS.Test
  , TidyKit.Strings.Test
  , TidyKit.Crypto.Test
  , TidyKit.Request.Test
  , TidyKit.Archive.Test
  , TidyKit.Crypto.AES256.Test
  , TidyKit.JSON.Test
  , TidyKit.Logger.Test
  , TidyKit.ParseArgs.Test
  , TidyKit.Collections.List.Test
  , TidyKit.Collections.Deque.Test
  , TidyKit.Collections.HashSet.Test
  , TidyKit.Collections.Dictionary.Test;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
