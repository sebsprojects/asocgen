module Main where

import Data.Time
import qualified Data.Vector.Unboxed as V

import System.IO
import System.CPUTime
import System.Console.ANSI

import Param
import Gen


type StepResult = (Action, [MTab], Maybe MTab)

-- PCount, Count, Number, Time
type Info = (Int, Int, Int, Integer)


main_ :: IO ()
main_ = do
  time <- getCurrentTime
  let ftime = formatTime defaultTimeLocale "%F_%H-%M" time
  let filePath = "saves/groups_n_" ++ (show sizeM) ++ "_" ++ ftime ++ ".txt"

  file <- openFile filePath AppendMode
  let t0 = doStepIter Fill [genInitial]
  _ <- iterIO t0 0 0 0 0 file
  hClose file
  return ()

main :: IO ()
main = do
  let t0 = genInitial
  --let z0 = (Fill, 0, pathLine $ maxEle)
  let z0 = (Fill, 0, pathQuad $ maxEle)
  let i0 = doStepIterZip [t0] z0
  now <- getCPUTime
  (_, _, _, (_, c, nu, _)) <- iterIOZip i0 (0, 0, 0, now) []
  putStrLn $ "Number of valid mtabs : " ++ (show nu)
  putStrLn $ "number of steps needed: " ++ (show c)
  --mapM_ printMTab r
  return ()


iterIOZip :: ([MTab], Zip) -> Info -> [MTab] -> IO ([MTab], Zip, [MTab], Info)
iterIOZip ([], z) inf res = return ([], z, res, inf)
iterIOZip (ts, z) (ic, c, nu, t) res = do
  let m = 1000
  (nc, nt) <- if ic == m
              then do
                now <- getCPUTime
                let diff = (fromIntegral (now - t)) / (10^9) :: Double
                clearScreen
                printMTab (head ts)
                putStrLn ("Time for " ++ (show m) ++ " steps: "
                          ++ (show diff) ++ "ms")
                putStrLn ("Total steps        : " ++ (show c) ++ " * "
                          ++ (show m))
                putStrLn ("Total groups       : " ++ (show nu))
                return (c + 1, now)
              else return (c, t)
  let nextRes = doStepIterZip ts z
  nnu <- case isCmpl (fst nextRes) of
    True -> return (nu + 1)
    False -> return nu
  iterIOZip nextRes ((ic + 1) `mod` (m + 1), nc, nnu, nt) res
    where isCmpl [] = False
          isCmpl (x : xs) = isComplete x

iterIO :: StepResult -> Int -> Int -> Int -> Integer -> Handle -> IO StepResult
iterIO (a, [], mres) _ _ _ _ _ = return (a, [], mres)
--iterIO (a, ts, mres) _ 500 _ _ = return (a, ts, mres)
iterIO (a, ts, _) cnt tot num tm file= do
  let n = 1000
  (tott, ntm) <- if cnt == n
                then do
                  now <- getCPUTime
                  let diff = (fromIntegral (now - tm)) / (10^9) :: Double
                  clearScreen
                  printMTab (head ts)
                  putStrLn ("Time for " ++ (show n) ++ " steps: "
                            ++ (show diff) ++ "ms")
                  putStrLn ("Total steps        : " ++ (show tot) ++ " * "
                           ++ (show n))
                  putStrLn ("Total groups       : " ++ (show num))
                  return (tot + 1, now)
                else return (tot, tm)
  let nextStep = doStepIter a ts
  nnum <- case nextStep of
    (_, _, Just r) -> do hPutStr file ((showMTab r) ++ "\n")
                         return (num + 1)
    (_, _, _) -> return num
  iterIO nextStep ((cnt + 1) `mod` (n + 1)) tott nnum ntm file


{-
 Printing MTabs pretty please

printMTab :: MTab -> IO ()
printMTab mtab = putStrLn $ showMTab mtab

showMTab :: MTab -> String
showMTab mtab = concat (map toString (rows mtab))

rows :: MTab -> [MTab]
rows mtab = map (\x -> selectRow x mtab) set

toString :: MTab -> String
toString row = concat $ (map (\x -> buff $ show x) (V.toList row)) ++ ["\n"]
  where buff s | length s < 3 = buff (" " ++ s)
               | otherwise = s
-}
