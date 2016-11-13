module Main where

import System.CPUTime
import System.Console.ANSI

import Param
import Gen


-- PCount, Count, Number, Time
type Info = (Int, Int, Int, Integer)

-- result list
type Info2 = ([MTab])
type StepFun = [MTab] -> Zip -> ([MTab], Zip)


main :: IO ()
main = mainN

mainN :: IO ()
mainN = do
  let t0 = genInitial
  --let z0 = (Fill, 0, pathLine $ maxEle)
  let z0 = (Fill, 0, pathQuad $ maxEle)
  let i0 = doStepA [t0] z0
  now <- getCPUTime
  (_, _, (_, c, nu, _)) <- iter i0 (0, 0, 0, now)
  putStrLn $ "Number of valid mtabs : " ++ (show nu)
  putStrLn $ "number of steps needed: " ++ (show c)
  return ()


mainT :: IO ()
mainT = do
  let t0 = genInitial
  let z0 = (Fill, 0, pathQuad $ maxEle)
  let (t1, z1) = doStepA [t0] z0
  (_, _, res) <- iterTest t1 z1 [] doStepA
  return ()

iter :: ([MTab], Zip) -> Info -> IO ([MTab], Zip, Info)
iter ([], z) inf = return ([], z, inf)
iter (ts, z) (ic, c, nu, t) = do
  let m = 10000
  (nc, nt) <- if ic == m
              then do
                now <- getCPUTime
                let diff = (fromIntegral (now - t)) / (10**9) :: Double
                clearScreen
                printMTab (head ts)
                putStrLn ("Time for " ++ (show m) ++ " steps: "
                          ++ (show diff) ++ "ms")
                putStrLn ("Total steps        : " ++ (show c) ++ " * "
                          ++ (show m))
                putStrLn ("Total groups       : " ++ (show nu))
                return (c + 1, now)
              else return (c, t)
  let nextRes = doStepA ts z
  nnu <- case isCmpl (fst nextRes) of
    True -> return (nu + 1)
    False -> return nu
  iter nextRes ((ic + 1) `mod` (m + 1), nc, nnu, nt)
    where isCmpl [] = False
          isCmpl (x : _) = isComplete x


iterTest :: [MTab] -> Zip -> Info2 -> StepFun -> IO ([MTab], Zip, Info2)
iterTest [] z i step = return ([], z, i)
iterTest ts z i step = do
  let (nts, nz) = step ts z
  ni <- case isComplete $ head ts of
    True -> return $ (head ts) : i
    _ -> return i
  iterTest nts nz ni step
