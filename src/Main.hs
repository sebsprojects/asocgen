module Main where

import Data.List
import Data.Maybe
--import Data.Tree

import Control.Monad
--import Control.Monad.Loops


type BinOp = (Int -> Int -> Int)
type MTab = [Int]

data Action = Fill | Up

set :: [Int]
set = [0..3]

maxEle :: Int
maxEle = maximum set

sizeM :: Int
sizeM = length set


-- Main
main :: IO ()
main = do
  let t0 = [genInitial]
  let (m, res) = doStep Fill (t0, [])
  --mapM_ printMTab res
  putStrLn $ show (length res)

printMTab :: MTab -> IO ()
printMTab mtab = putStrLn $ showMTab mtab

showMTab mtab = (intercalate "\n" (rows mtab)) ++ "\n"
rows mtab = map (\x -> show $ selectRow x mtab) set

-- Basics
toBinOp :: MTab -> BinOp
toBinOp = evalMTab

toMTab :: BinOp -> MTab
toMTab = undefined

evalMTab :: MTab -> Int -> Int -> Int
evalMTab mtab r c = mtab !! (c + r * sizeM)
  --where s = floor (sqrt (fromIntegral (length mtab)) :: Double)

getEntry :: MTab -> Int -> Int -> Int
getEntry = evalMTab

isAsocOn :: BinOp -> Bool
isAsocOn f = null xs
  where xs = [a | a <- set, b <- set, c <- set, f (f a b) c /= f a (f b c)]


-- Generation
isComplete :: MTab -> Bool
isComplete mtab = not (-1 `elem` mtab)

genInitial :: MTab
genInitial = concat $ set : (map row (tail set))
  where row m = m : (replicate (sizeM - 1) (-1))

doStep :: Action -> ([MTab], [MTab]) -> ([MTab], [MTab])
doStep _ ([], res) = ([], res)
doStep Fill (t : ts, res) = case procMTab Fill t of
  Nothing -> doStep Up ((t : ts), res)
  Just r -> case isComplete r of
    True -> doStep Up (ts, r : res)
    False -> doStep Fill (r : t : ts, res)
doStep Up (t : ts, res) = case procMTab Up t of
  Nothing -> doStep Up (ts, res)
  Just r -> doStep Fill (r : ts, res)

procMTab :: Action -> MTab -> Maybe MTab
procMTab a mtab =
  join $ fmap (tupleUp mtab) (toRC $ fmap (+ (w a)) (findIndex (== -1) mtab))
  where w Fill = 0
        w Up = -1

tupleUp mtab = uncurry (upEntry mtab)
toRC i = fmap (\x -> (x `quot` length set, x `mod` length set)) i

upEntry :: MTab -> Int -> Int -> Maybe MTab
upEntry mtab r c
  | getEntry mtab r c == maxEle = Nothing
  | otherwise = fmap (\x -> replaceAtIndex (r * sizeM + c) x mtab) nextB
  where constr = [0..(getEntry mtab r c)] ++ (selectRow r mtab) ++
                 (selectCol c mtab)
        nextB = nextBiggest constr

selectRow ri xs = drop (sizeM * ri) $ take (sizeM * (ri + 1)) xs

selectCol ci xs = map (xs !!) indexList
  where indexList = map (\x -> x * sizeM + ci) set

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item : b)
  where (a, (_ : b)) = splitAt n ls

nextBiggest :: [Int] -> Maybe Int
nextBiggest constr | null xs = Nothing
                   | otherwise = Just $ head xs
  where xs = [x | x <- set, not $ x `elem` constr]
