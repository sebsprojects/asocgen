module Gen.AsocTest ( isAsocToList
                    , isAsocIncmplToList
                    , isAsocIncmplRec
                    , isAsocIncmplIO
                    , isAsocIncmplIncrm) where

import qualified Data.Vector.Unboxed as V
import Gen.Common

-- | Naive O(n^3) asoc check with bad implementation via V.toList
isAsocToList :: Param -> MTab -> Bool
isAsocToList (s, set, _) mtab = null xs
  where xs = [a | a <- ss, b <- ss, c <- ss,
              f (f a b) c /= f a (f b c)]
        f = evalMTab mtab s
        ss = V.toList $ V.tail set

-- | Naive O(n^3) asoc check for incomplete tables, bad via V.toList
isAsocIncmplToList :: Param -> MTab -> Bool
isAsocIncmplToList (s, vset, _) mtab = null xs
  where xs = [a | a <- set, b <- set, c <- set, check mtab s a b c]
        set = V.toList vset

-- | Naive O(n^3) asoc check from incomplete tables via recursion
isAsocIncmplRec :: Param -> MTab -> (Int, Int, Int) -> Bool
isAsocIncmplRec (s, vset, cols) mtab (a, b, c)
  | (a, b, c) == (maxE, maxE, maxE) = not $ check mtab s a b c
  | check mtab s a b c = False
  | otherwise = isAsocIncmplRec (s, vset, cols) mtab (nextTrip maxE a b c)
  where maxE = s - 1

-- | Utility wrapping into IO monad
isAsocIncmplIO :: Param -> MTab -> (Int, Int, Int) -> IO Bool
isAsocIncmplIO (s, vset, cols) mtab (a, b, c)
  | (a, b, c) == (maxE, maxE, maxE) = return (not $ check mtab s a b c)
  | check mtab s a b c = do
      putStrLn $ "not asoc: " ++ (show (a, b, c))
      return False
  | otherwise = isAsocIncmplIO (s, vset, cols) mtab (nextTrip maxE a b c)
  where maxE = s - 1

-- False if (x y) z = x (y z)
check :: MTab -> Int -> Int -> Int -> Int -> Bool
check mtab s x y z = xy >= 0 && yz >= 0 &&
                     xy_z >= 0 && x_yz >= 0 &&
                     xy_z /= x_yz
  where xy = evalMTab mtab s x y
        yz = evalMTab mtab s y z
        xy_z = evalMTab mtab s xy z
        x_yz = evalMTab mtab s x yz
{-# INLINE check #-}

nextTrip :: Int -> Int -> Int -> Int -> (Int, Int, Int)
nextTrip maxE x y z
  | y == maxE && z == maxE = (x + 1, 0, 0)
  | z == maxE = (x, y + 1, 0)
  | otherwise = (x, y, z + 1)


{- |
Incremental asoc check given that the table before entry (a b)
was made/modified was asoc already. These three cases have to be checked:

1. _ (a b) =          = (_ a) b      where _   = *
2. (a b) _ =     ab _ = a (b _)      where _   = *
3. (_ _) b =     ab   = _ (_ b)      where _ _ = a
4. a (_ _) =     ab   = (a _) _      where _ _ = b

Reduces complexety to O(n^2) (from O(n^3))
-}
isAsocIncmplIncrm :: Param -> MTab -> (Int, Int, Int) ->
                     [(Int, Int)] -> [(Int, Int)] -> Bool
isAsocIncmplIncrm (s, vs, _) mtab (a, b, ab) axy bxy =
  and [checkABC mtab vs s a b ab, checkCAB mtab vs s a b ab,
       checkA mtab s a ab bxy, checkB mtab s b ab axy]


-- 1. case
checkCAB :: MTab -> V.Vector Int -> Int -> Int -> Int -> Int -> Bool
checkCAB mtab set s a b ab = V.and $ V.map (\c -> chkCAB mtab s a b ab c) set

chkCAB :: MTab -> Int -> Int -> Int -> Int -> Int -> Bool
chkCAB mtab s a b ab c | any (< 0) [c_ab, ca, ca_b] = True
                       | otherwise = c_ab == ca_b
  where c_ab = evalMTab mtab s c ab
        ca = evalMTab mtab s c a
        ca_b = evalMTab mtab s ca b

-- 2. case
checkABC :: MTab -> V.Vector Int -> Int -> Int -> Int -> Int -> Bool
checkABC mtab set s a b ab = V.and $ V.map (\c -> chkABC mtab s a b ab c) set

chkABC :: MTab -> Int -> Int -> Int -> Int -> Int -> Bool
chkABC mtab s a b ab c | any (< 0) [ab_c, bc, a_bc] = True
                       | otherwise = ab_c == a_bc
  where ab_c = evalMTab mtab s ab c
        bc = evalMTab mtab s b c
        a_bc = evalMTab mtab s a bc

-- TODO: Change to Vector (Int, Int)
-- 3. case
checkA :: MTab -> Int -> Int -> Int -> [(Int, Int)] -> Bool
checkA mtab s a ab axy = and [chkA mtab s a ab xy | xy <- axy]

chkA :: MTab -> Int -> Int -> Int -> (Int, Int) -> Bool
chkA mtab s a ab (x, y) | any (< 0) [ax, ax_y] = True
                        | otherwise = ab == ax_y
  where ax = evalMTab mtab s a x
        ax_y = evalMTab mtab s ax y

-- TODO: Change to Vector (Int, Int)
-- 4. case
checkB :: MTab -> Int -> Int -> Int -> [(Int, Int)] -> Bool
checkB mtab s b ab bxy = and [chkB mtab s b ab xy | xy <- bxy]

chkB :: MTab -> Int -> Int -> Int -> (Int, Int) -> Bool
chkB mtab s b ab (x, y) | any (<0) [yb, x_yb] = True
                        | otherwise = ab == x_yb
  where yb = evalMTab mtab s y b
        x_yb = evalMTab mtab s x yb
