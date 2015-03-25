module Basics.RockPaperScissors where

import Data.Char (isSpace)
import Network
import System.IO

data Move
  = Rock
  | Paper
  | Scissors
  deriving (Eq,Read,Show,Enum,Bounded)

data Outcome
  = Lose
  | Tie
  | Win
  deriving (Show,Eq,Ord)

outcome :: Move -> Move -> Outcome
outcome Rock Scissors = Win
outcome Paper Rock = Win
outcome Scissors Paper = Win
outcome us them
  | us == them = Tie
  | otherwise = Lose

parseMove :: String -> Maybe Move
parseMove s =
  case reads s of
    [(m,t)]
      | all isSpace t -> Just m
    _ -> Nothing

withTty :: (Handle -> IO r) -> IO r
withTty = withFile "/dev/tty" ReadWriteMode

withClient :: PortID -> (Handle -> IO a) -> IO a
withClient listenPort fn =
  do s <- listenOn listenPort
     (h,host,port) <- accept s
     putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
     sClose s -- Only accept one client
     a <- fn h
     hClose h
     return a

computerVsUser :: Move -> Handle -> IO ()
computerVsUser m h =
  do hPutStrLn h $
       "Please enter one of " ++
       show ([minBound ..] :: [Move])
     i <- hGetLine h
     case parseMove i of
       Nothing -> computerVsUser m h
       Just r ->
         hPutStrLn h $
         "You " ++
         show (outcome r m)
