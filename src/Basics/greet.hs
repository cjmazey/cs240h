module Main where

import System.IO

greet :: Handle -> IO ()
greet h =
  do hPutStrLn h "What is your name?"
     name <- hGetLine h
     hPutStrLn h $ "Hi, " ++ name

withTty :: (Handle -> IO r) -> IO r
withTty = withFile "/dev/tty" ReadWriteMode

main :: IO ()
main = withTty greet
