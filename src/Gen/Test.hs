module Gen.Test where

import System.CPUTime
import System.Console.ANSI

import Gen.Common
import Gen.GenSimple


test1 :: IO ()
test1 = do
  let param = paramN 9
  now <- getCPUTime
  let info = (0, 0, 0, now)
  (_, (isteps, steps, groups, _)) <- runGenIO param info printMeta
  putStrLn $ "\nFinal report: "
  putStrLn $ "Number of steps x 10^5   :   " ++ (show $ steps) ++ " ("
    ++ (show isteps) ++ ")"
  putStrLn $ "Number of groups         :   " ++ (show groups)
  return ()

-- (num internal, num steps, num groups, time per 10^5)
type Info = (Int, Int, Int, Integer)

printMeta :: SRes -> Info -> IO Info
printMeta ([], _) info = return info
printMeta (t : _, _) (isteps, steps, groups, lasttime) = do
  let ngroups = case isComplete t of
        True -> groups + 1
        False -> groups
  (nisteps, nsteps, ntime) <- case isteps of
    10000 -> do
      now <- getCPUTime
      let diff = (fromIntegral (now - lasttime)) / (10**9) :: Double
      clearScreen
      printMTab (t)
      putStrLn $ "Time for 10^5 steps      :    " ++ (show diff)
      putStrLn $ "Number of steps x 10^5   :    " ++ (show $ steps + 1)
      putStrLn $ "Number of groups         :    " ++ (show groups)
      return (0, steps + 1, now)
    _ -> return (isteps + 1, steps, lasttime)
  return (nisteps, nsteps, ngroups, ntime)
