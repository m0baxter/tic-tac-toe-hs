
module Main where

import Board
import Control.Exception
import System.IO


takeTurn :: Board -> Marker -> IO Board
takeTurn brd m = do
   putStr $ show m ++ "'s turn [0-8]: "
   sqr <- getLine
   en <- try (evaluate (read sqr :: Int) ) :: IO (Either SomeException Int)
   case en of
      Left _ -> do
         takeTurn brd m
      Right n -> if n >= 0 && n < 9 then
                    case mkMove brd m n of
                       Just brd' ->  return brd'
                       Nothing  -> takeTurn brd m
                 else takeTurn brd m


runGame :: Board -> IO ()
runGame brd = do
   --X turn:
   brd <- takeTurn brd X
   if gameWon brd then
      do
         putStrLn $ show brd
         putStrLn "X wins."
   else do --O takes turn:
      putStrLn $ show brd
      case noMoves brd of
         True  -> putStrLn "Mew."
         False -> do 
            brd <- takeTurn brd O
            if gameWon brd then
               do
                  putStrLn $ show brd
                  putStrLn "O wins."
            else do
               putStrLn $ show brd
               runGame brd


main :: IO ()
main = do
   hSetBuffering stdout NoBuffering
   let brd = newBoard
   putStrLn $ show brd
 
   --Run Game:
   runGame brd
 
   --Play again? 
   putStr "Play again? [y/n]: "
   answer <- getLine
   case answer of
      "n" -> return ()
      _   -> main
 



