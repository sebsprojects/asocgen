module Gen.Common ( Param
                  , MTab
                  , SRes
                  , Action (Fill, Up)
                  , Zip
                  , evalMTab
                  , selectRow
                  , selectCol
                  , isComplete
                  , paramN
                  , printMTab
                  , showMTab) where

import qualified Data.Vector.Unboxed as V

-- | (Set size, set itself, col index lists)
type Param = (Int, V.Vector Int, [V.Vector Int])

-- | Row order multiplication table without labels
type MTab = V.Vector Int

-- | Result of one doStep
type SRes = ([MTab], Zip)

-- | Action for a step: Either fill a -1 entry or up a existing one
data Action = Fill | Up

-- | Zipper: (Action to do, current position, iteration order)
type Zip = (Action, Int, V.Vector Int)


evalMTab :: MTab -> Int -> Int -> Int -> Int
evalMTab mtab setsize r c = mtab V.! (c + r * setsize)
{-# INLINE evalMTab #-}

isComplete :: MTab -> Bool
isComplete mtab = V.last mtab /= (-1)

selectRow :: MTab -> Int -> Int -> V.Vector Int
selectRow mtab s ri = V.drop (s * ri) $ V.take (s * (ri + 1)) mtab

selectCol :: MTab -> Int -> [V.Vector Int] -> V.Vector Int
selectCol mtab ci colIndices = V.map (mtab V.!) (colIndices !! ci)

paramN :: Int -> Param
paramN n = (n, vset, colIndices)
  where vset = V.enumFromN 0 n
        colIndices = map (\ci -> (V.map (\x -> x * n + ci) vset)) [0..n-1]

printMTab :: MTab -> IO ()
printMTab mtab | V.length mtab == 0 = putStrLn "empty MTab"
               | otherwise = putStr $ showMTab mtab

showMTab :: MTab -> String
showMTab mtab = concat (map rowToString (rows mtab))

rows :: MTab -> [MTab]
rows mtab = map (\x -> selectRow mtab s x) [0..s-1]
  where s = floor $ sqrt (fromIntegral (V.length mtab) :: Double)

rowToString :: MTab -> String
rowToString row = concat $ (map (\x -> buff $ show x) (V.toList row)) ++ ["\n"]
  where buff s | length s < 3 = buff (" " ++ s)
               | otherwise = s
