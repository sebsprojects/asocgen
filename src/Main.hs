module Main where

import Data.List
import Data.Maybe
import Data.Time

import Control.Monad

import System.IO
import System.CPUTime
import System.Console.ANSI

import Param
import Gen


type StepResult = (Action, [MTab], Maybe MTab)

{-
 Main main main
-}
main :: IO ()
main = do
  time <- getCurrentTime
  let ftime = formatTime defaultTimeLocale "%F_%H-%M" time
  let filePath = "saves/groups_n_" ++ (show sizeM) ++ "_" ++ ftime ++ ".txt"

  file <- openFile filePath AppendMode
  let t0 = doStepIter Fill [genInitial]
  iterIO t0 0 0 0 file
  hClose file
  return ()

iterIO :: StepResult -> Int -> Int -> Integer -> Handle -> IO StepResult
iterIO (a, [], mres) _ _ _ _ = return (a, [], mres)
iterIO (a, ts, mres) cnt tot tm file= do
  let n = 1000
  (tot, ntm) <- if cnt == n
                then do
                  now <- getCPUTime
                  let diff = (fromIntegral (now - tm)) / (10^9) :: Double
                  clearScreen
                  printMTab (head ts)
                  putStrLn ("Time for " ++ (show n) ++ " steps: "
                            ++ (show diff) ++ "ms")
                  putStrLn ("Total steps        : " ++ (show tot) ++ " * "
                           ++ (show n))
                  return (tot + 1, now)
                else return (tot, tm)
  let nextStep = doStepIter a ts
  case nextStep of
    (_, _, Just r) -> hPutStr file ((showMTab r) ++ "\n")
    (_, _, _) -> return ()
  iterIO nextStep ((cnt + 1) `mod` (n + 1)) tot ntm file


{-
 Printing MTabs pretty please
-}
printMTab :: MTab -> IO ()
printMTab mtab = putStrLn $ showMTab mtab

showMTab mtab = concat (map toString (rows mtab))
rows mtab = map (\x -> selectRow x mtab) set
toString row = concat $ (map (\x -> buff $ show x) row) ++ ["\n"]
buff s | length s < 3 = buff (" " ++ s)
       | otherwise = s
