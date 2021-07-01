{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}
-- useful considerations
-- IO is a new data type, it is a functor an applicatve and a monad
-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- (<*>) :: IO (a -> b) -> IO a -> IO b
-- (<$>) :: List (IO a) -> IO List a

-- getArgs :: IO (List Chars)
-- putStrLn :: Chars -> IO ()
-- readFile :: FilePath -> IO Chars
-- lines :: Chars -> List Chars
-- void :: IO a -> IO ()



-----the program
-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile :: FilePath -> Chars -> IO ()
-- printFile f c = do
--                   putStrLn ("======" ++ f);
--                   putStrLn(c);
printFile = \name contents -> putStrLn ("------" ++ name) *>
                              putStrLn (contents)


--  error "todo: Course.FileIO#printFile"

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles :: List (FilePath, Chars) -> IO ()
printFiles = \x -> void (sequence((\(n, c) -> printFile n c) <$> x))
-- you can uncurry printFile
--printFiles = \x -> void(sequence ((<$>)(uncurry printFile)x))
--printFiles = void.sequence . <$> . (uncurry printFile)
-- the above uncurrying needs work
--printFiles x = printFile <$> _
--  error "todo: Course.FileIO#printFiles"

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile :: FilePath -> IO (FilePath, Chars)
getFile = \name -> readFile name >>= \c ->  return (name, c)
--this can be re written with fmap
--getfile = \name -> (\c->( (,) name c)) <$> readFile name
-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles = \names -> sequence (getFile <$> names)
-- this uses sequence
-- sequence :: Applicative k => List (k a) -> k (List a)

--error "todo: Course.FileIO#getFiles"

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@, @lines@, and @printFiles@.
run :: FilePath -> IO ()
run filename = do
      content <- readFile filename
      output <- getFiles (lines content)
      printFiles output

--  error "todo: Course.FileIO#run"

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  do
    a <- getArgs
    case a of
      Nil -> putStrLn "wrong Arguments"
      h:._ -> run h
--this was with do notation

  -- getArgs >>= \args ->
  --   case args of
  --     filename :. Nil -> run filename
  --     _ -> putStrLn "usage: runhaskell io.hs filename"
----  error "todo: Course.FileIO#main"

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
