module Gen where

import qualified Data.Vector.Unboxed as V
import Control.Monad

import Param


data Action = Fill | Up

-- Zipper: (Action to do, current position, iteration order)
type Zip = (Action, Int, V.Vector Int)
type MTab = V.Vector Int


evalMTab :: MTab -> Int -> Int -> Int
evalMTab mtab r c = mtab V.! (c + r * sizeM)

isAsoc :: MTab -> Bool
isAsoc mtab = null xs
  where xs = [a | a <- set, b <- set, c <- set, f (f a b) c /= f a (f b c)]
        f = evalMTab mtab

isAsocIncmpl :: MTab -> Bool
isAsocIncmpl mtab = null xs
  where xs = [a | a <- set, b <- set, c <- set, check a b c]
        check x y z = xy >= 0 && yz >= 0 &&
                      xy_z >= 0 && x_yz >= 0 &&
                      xy_z /= x_yz
          where xy = f x y
                yz = f y z
                xy_z = f xy z
                x_yz = f x yz
                f = evalMTab mtab

isComplete :: MTab -> Bool
isComplete mtab = not (V.elem (-1) mtab)

numEntries :: MTab -> Int
numEntries mtab = (V.length $ V.filter (>= 0) mtab) - maxEle * 2 + 1

pathLine :: Int -> V.Vector Int
pathLine n = V.fromList $ shiftByOne n [0..n*n-1]

pathQuad :: Int -> V.Vector Int
pathQuad n = V.fromList $ concat $ map (\m -> cycleN m n) [1..n]

cycleN :: Int -> Int -> [Int]
cycleN n d = reverse $ cycleN' [start] (cyc (n - 1))
  where start = (d + 1) * n + 1
        down = d + 1
        cycleN' xs [] = xs
        cycleN' [] _ = undefined --no compiler warning please
        cycleN' (x : xs) (m : mov) = cycleN' ((m x down) : x : xs) mov

cyc :: Int -> [Int -> Int -> Int]
cyc n = (replicate n goRight) ++ (replicate n goDown)
  where goRight i _ = i + 1
        goDown i d = i - d

shiftByOne :: Int -> [Int] -> [Int]
shiftByOne n ys = concat [map (+ i) zs | (i, zs) <- zip [n+2..2*n+1]
                                                    (chunks n ys [[]])]
  where chunks _ [] res = reverse (map reverse res)
        chunks m (x : xs) [[]] = chunks m xs [[x]]
        chunks m (x : xs) res
          | length (head res) == m = chunks m xs ([x] : res)
          | otherwise = chunks m xs ((x : (head res)) : (tail res))

{- |
  Inital of the form
     0  1  2  3 .
     1  0 -1 -1 .
     2 -1 -1 -1 .
     3 -1 -1 -1 .
     .  .  .  . .
  Note that the 0 at pos (1,1) is there to avoid index -1 issues
-}

genInitial :: MTab
genInitial = V.fromList $ concat $ set : (map row (tail set))
  where row 1 = 1 : 0 : (replicate (sizeM - 2) (-1))
        row m = m : (replicate (sizeM - 1) (-1))

procMTab :: Action -> MTab -> Maybe MTab
procMTab a mtab =
  join $ fmap (upEntry mtab) (toRC (fmap (+ (w a))
                                    (V.findIndex (== -1) mtab)))
  where w Fill = 0
        w Up = -1
        toRC i = fmap (\x -> (x `quot` length set, x `mod` length set)) i

selectRow :: Int -> MTab -> V.Vector Int
selectRow ri xs = V.drop (sizeM * ri) $ V.take (sizeM * (ri + 1)) xs

selectCol :: Int -> MTab -> V.Vector Int
selectCol ci mtab = V.map (mtab V.!) (V.fromList indexList)
  where indexList = map (\x -> x * sizeM + ci) set

replaceAtIndex :: Int -> Int -> MTab -> MTab
replaceAtIndex n item mtab = V.update mtab (V.singleton (n, item))

nextBiggest :: V.Vector Int -> Maybe Int
nextBiggest constr | null xs = Nothing
                   | otherwise = Just $ head xs
  where xs = [x | x <- set, not $ V.elem x constr]

upEntry :: MTab -> (Int, Int) -> Maybe MTab
upEntry mtab (r, c)
  | indEle == maxEle = Nothing
  | otherwise = fmap (\x -> replaceAtIndex ind x mtab) nextB
  where ind = r * sizeM + c
        indEle = mtab V.! ind
        constr = V.enumFromN 0 (indEle + 1) V.++
                 (selectRow r mtab) V.++ (selectCol c mtab)
        nextB = nextBiggest constr

-- Tries to increase the element at index i
upEntry_ :: MTab -> Int -> Maybe MTab
upEntry_ mtab i = upEntry mtab (i `quot` length set, i `mod` length set)


{- | doStep

  try to fill:
      fail: fill not possible, no index shift, try up
      s cm: up on current (surely fails, but to transmit result
                           try to up with p + 1, no outofbounds b/c up and
                           not fill)
      s in: fill was possible >> test asoc
          fail: up the newly filled entry: index shift + 1, r : t : ts
          succ: fill from the newly filled entry: index shift + 1, r : t : ts
  try to up:
      fail: up not possible, discard t, index shift -1
      succ: up was possible >> test asoc
          fail: try up again on result, no index shift
          succ: try fill on result, no index shift
-}
doStep :: [MTab] -> Zip -> ([MTab], Zip)
doStep [] zi = ([], zi)
doStep (t : ts) (Fill, p, iord) = case upEntry_ t (iord V.! (p + 1)) of
  Nothing -> (t : ts, (Up, p, iord))
  Just r -> case isComplete r of
    True -> (r : t : ts, (Up, p + 1, iord))
    False -> case isAsocIncmpl r of
      False -> (r : t : ts, (Up, p + 1, iord))
      True -> (r : t : ts, (Fill, p + 1, iord))
doStep (t : ts) (Up, p, iord) = case upEntry_ t (iord V.! p) of
  Nothing -> (ts, (Up, p - 1, iord))
  Just r -> case isAsocIncmpl r of
    False -> (r : ts, (Up, p, iord))
    True -> (r : ts, (Fill, p, iord))


doStepIO :: [MTab] -> Zip -> IO ([MTab], Zip)
doStepIO [] zi = return ([], zi)
doStepIO (t : ts) (Fill, p, iord) = case upEntry_ t (iord V.! (p + 1)) of
  Nothing -> do
    --printMTab t
    --putStrLn "Fill - Fail - *\n"
    return (t : ts, (Up, p, iord))
  Just r -> case isComplete r of
    True -> do
      --printMTab t
      --putStrLn "Fill - succ - COMPLETE\n"
      return (r : t : ts, (Up, p + 1, iord))
    False -> case isAsocIncmpl r of
      False -> do
        --printMTab t
        --putStrLn "Fill - Succ - a:False\n"
        --putStrLn "Checked Asoc"
        return (r : t : ts, (Up, p + 1, iord))
      True -> do
        --printMTab t
        --putStrLn "Fill - Succ - a:True\n"
        --putStrLn "Checked Asoc"
        return (r : t : ts, (Fill, p + 1, iord))
doStepIO (t : ts) (Up, p, iord) = case upEntry_ t (iord V.! p) of
  Nothing -> do
    --printMTab t
    --putStrLn "Up - Fail - *\n"
    return (ts, (Up, p - 1, iord))
  Just r -> case isAsocIncmpl r of
    False -> do
      --printMTab t
      --putStrLn "Up - Succ - a:False\n"
      --putStrLn "Checked Asoc"
      return (r : ts, (Up, p, iord))
    True -> do
      --printMTab t
      --putStrLn "Up - Succ - a:True\n"
      --putStrLn "Checked Asoc"
      return (r : ts, (Fill, p, iord))


printMTab :: MTab -> IO ()
printMTab mtab | V.length mtab == 0 = putStrLn "empty MTab"
               | otherwise = putStr $ showMTab mtab

showMTab :: MTab -> String
showMTab mtab = concat (map toString (rows mtab))

rows :: MTab -> [MTab]
rows mtab = map (\x -> selectRow x mtab) set

toString :: MTab -> String
toString row = concat $ (map (\x -> buff $ show x) (V.toList row)) ++ ["\n"]
  where buff s | length s < 3 = buff (" " ++ s)
               | otherwise = s
