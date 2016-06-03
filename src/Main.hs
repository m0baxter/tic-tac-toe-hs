
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
      Left _  -> takeTurn brd m
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
      do print brd
         putStrLn "X wins."
   else do --O takes turn:
      print brd
      case noMoves brd of
         True  -> putStrLn "Mew."
         False -> do 
            brd <- takeTurn brd O
            if gameWon brd then
               do print brd
                  putStrLn "O wins."
            else do print brd
                    runGame brd


playAgain :: IO ()
playAgain = do
   putStr "Play again? [y/n]: "
   answer <- getLine
   case answer of
      "n" -> return ()
      "y" -> main
      _   -> playAgain


main :: IO ()
main = do
   --Turn off buffering:
   hSetBuffering stdout NoBuffering

   --Run Game:
   let brd = newBoard
   print brd
   runGame brd
 
   --Play again? 
   playAgain

