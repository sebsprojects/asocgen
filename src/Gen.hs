module Gen where

import Data.Maybe
import qualified Data.Vector.Unboxed as V

import Control.Monad

import Param


{-
 Basics
-}
--type MTab = [Int]
type MTab = V.Vector Int

data Action = Fill | Up


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


{-
 Processing MTabs
-}
genInitial :: MTab
genInitial = V.fromList $ concat $ set : (map row (tail set))
  where row m = m : (replicate (sizeM - 1) (-1))

procMTab :: Action -> MTab -> Maybe MTab
procMTab a mtab =
  join $ fmap (tupleUp mtab) (toRC $ fmap (+ (w a))
                              (V.findIndex (== -1) mtab))
  where w Fill = 0
        w Up = -1
        tupleUp mtab = uncurry (upEntry mtab)
        toRC i = fmap (\x -> (x `quot` length set, x `mod` length set)) i

upEntry :: MTab -> Int -> Int -> Maybe MTab
upEntry mtab r c
  | evalMTab mtab r c == maxEle = Nothing
  | otherwise = fmap (\x -> replaceAtIndex (r * sizeM + c) x mtab) nextB
  where constr = V.enumFromN 0 ((evalMTab mtab r c) + 1) V.++
                 (selectRow r mtab) V.++ (selectCol c mtab)
        nextB = nextBiggest constr

selectRow :: Int -> MTab -> MTab
selectRow ri xs = V.drop (sizeM * ri) $ V.take (sizeM * (ri + 1)) xs

selectCol :: Int -> MTab -> MTab
selectCol ci mtab = V.map (mtab V.!) (V.fromList indexList)
  where indexList = map (\x -> x * sizeM + ci) set

replaceAtIndex :: Int -> Int -> MTab -> MTab
replaceAtIndex n item mtab = V.update mtab (V.singleton (n, item))

nextBiggest :: MTab -> Maybe Int
nextBiggest constr | null xs = Nothing
                   | otherwise = Just $ head xs
  where xs = [x | x <- set, not $ V.elem x constr]


-- | Pure, recursive
doStep :: Action -> ([MTab], [MTab]) -> ([MTab], [MTab])
doStep _ ([], res) = ([], res)
doStep Fill (t : ts, res) = case procMTab Fill t of
  Nothing -> doStep Up ((t : ts), res)
  Just r -> case isComplete r of
    True -> doStep Up (ts, r : res)
    False -> doStep Fill (r : t : ts, res)
doStep Up (t : ts, res) = case procMTab Up t of
  Nothing -> doStep Up (ts, res)
  Just r -> case isAsocIncmpl r of
    True -> doStep Fill (r : ts, res)
    False -> doStep Up (r : ts, res)

-- | Pure, for iteration
doStepIter :: Action -> [MTab] -> (Action, [MTab], Maybe MTab)
doStepIter _ [] = (Fill, [], Nothing)
doStepIter Fill (t : ts) = case procMTab Fill t of
  Nothing -> (Up, (t : ts), Nothing)
  Just r -> case isComplete r of
    True -> (Up, ts, Just r)
    False -> (Fill, r : t : ts, Nothing)
doStepIter Up (t : ts) = case procMTab Up t of
  Nothing -> (Up, ts, Nothing)
  Just r -> case isAsocIncmpl r of
    True -> (Fill, r : ts, Nothing)
    False -> (Up, r : ts, Nothing)
