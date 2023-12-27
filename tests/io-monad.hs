{-# LANGUAGE QuasiQuotes #-}
{-
 - Test the IO interface of hgdbmi.
 -
 - Requirements:
 -    - gdb (see config)
 -    - gcc (see setup)
 -
 - Steps:
 -    - create temporary directory
 -    - write test program (example)
 -    - compile test program with debugging
 -    - start debugger
 -    - set breakpoint
 -    - run debugger
 -    - wait for stop event, evaluate variable and continue (10 times)
 -    - quit
 -    - dump log file
 -
 - Output:
 -    - stream and notification events (excluding stop events)
 -    - log file with GDB/MI communication
-}
module Main (main) where

import Control.Concurrent     (newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Exception.Base (bracket)
import Control.Monad          (forM_, when)
import System.Directory       (removeDirectoryRecursive, getCurrentDirectory, setCurrentDirectory)
import System.Exit            (ExitCode(..))
import System.IO.Temp         (createTempDirectory)
import System.Process         (readProcessWithExitCode)
import Text.Printf            (printf)

import Paste (paste)

import qualified Data.Maybe
import Gdb

example :: String
example = [paste|
#include <stdio.h>

void print(int i) {
    printf("hello world %d\n", i);
}

int main() {
    int i;
    for (i=0; ; i++) {
        print(i);
    }
    return 0;
}
|]

buildExample :: IO ()
buildExample = do
  writeFile "example.c" example
  (ec, sout, serr) <- readProcessWithExitCode "gcc" (words "-o example -g example.c") ""
  case ec of
    ExitFailure ec' -> do
      putStrLn sout
      putStrLn serr
      error $ printf "failed to execute gcc: %s" (show ec')
    ExitSuccess -> pure ()

assert :: (Eq a, Show a, Monad m) => String -> a -> a -> m ()
assert what x y = if (x == y)
  then pure ()
  else error $ printf "assertion failed: %s: %s vs. %s" what (show x) (show y)

test :: IO (Either String ())
test = do
  buildExample
  runGdb $ do
    cli "tty /dev/null"
    file "example"
    let loc = file_function_location "example.c" "print"
    bp <- breakpoint loc
    run
    forM_ [(0::Int)..10] $ \counter -> do
        onBreak $ \stopped -> do
          assert
            "breakpoint number"
            ((bkptHitNumber . stoppedReason) stopped)
            (bkptNumber bp)

          value <- eval "i"
          assert
            "value of i"
            value
            (show counter)

withTemporaryDirectory :: IO a -> IO a
withTemporaryDirectory f = bracket acquire release inbetween
  where
    acquire = do
      tmpdir <- createTempDirectory "/tmp" "hgdbmi-test"
      curdir <- getCurrentDirectory
      setCurrentDirectory tmpdir
      pure (curdir, tmpdir)

    release (curdir, tmpdir) = do
      setCurrentDirectory curdir
      removeDirectoryRecursive tmpdir

    inbetween (_, _) = f

main :: IO ()
main =
  withTemporaryDirectory $ do
    test >>= either error pure
    readFile "gdb.log" >>= putStr
