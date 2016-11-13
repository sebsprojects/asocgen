module Param where

import qualified Data.Vector.Unboxed as V


set :: [Int]
set = [0..7]

vset :: V.Vector Int
vset = V.fromList set

maxEle :: Int
maxEle = maximum set

sizeM :: Int
sizeM = length set

colIndices :: [V.Vector Int]
colIndices = map (\ci -> (V.map (\x -> x * sizeM + ci) vset)) set
