module Main where

import Data.List
import Data.Maybe

type BinOp = (Int -> Int -> Int)
type MTab = [Int]

set :: [Int]
set = [0,1,2,3]

maxEle :: Int
maxEle = maximum set

sizeM :: Int
sizeM = length set


-- Main
main :: IO ()
main = do
  let t0 = genInitial
  let t1 = upEntry t0 1 1
  let t2 = upEntryMaybe t1 1 2
  let t3 = upEntryMaybe t2 1 3
  case t3 of
    Nothing -> putStrLn "Nothing"
    Just t -> printMTab t


printMTab :: MTab -> IO ()
printMTab mtab = putStrLn $ showMTab mtab

showMTab mtab = intercalate "\n" (rows mtab)
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


-- Gen
genInitial :: MTab
genInitial = concat $ set : (map row (tail set))
  where row m = m : (replicate (sizeM - 1) (-1))

upEntryMaybe :: Maybe MTab -> Int -> Int -> Maybe MTab
upEntryMaybe mtab r c | isNothing mtab = Nothing
                      | otherwise = upEntry (fromJust mtab) r c

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
