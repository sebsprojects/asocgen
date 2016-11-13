module Gen where

import qualified Data.Vector.Unboxed as V

import Param


data Action = Fill | Up

-- Param: (n, Set, Col Indices) Precomputed
type Param = (Int, V.Vector Int, [V.Vector Int])

-- Zipper: (Action to do, current position, iteration order)
type Zip = (Action, Int, V.Vector Int)
type MTab = V.Vector Int


evalMTab :: MTab -> Int -> Int -> Int
evalMTab mtab r c = mtab V.! (c + r * sizeM)
{-# INLINE evalMTab #-}

isComplete :: MTab -> Bool
isComplete mtab = not (V.elem (-1) mtab)


{-
 Asociativity
-}
isAsoc :: MTab -> Bool
isAsoc mtab = null xs
  where xs = [a | a <- ss, b <- ss, c <- ss,
              f (f a b) c /= f a (f b c)]
        f = evalMTab mtab
        ss = tail set

-- | Brute force it in O(n^3)
isAsocIncmpl :: MTab -> Bool
isAsocIncmpl mtab = null xs
  where xs = [a | a <- set, b <- set, c <- set, check mtab a b c]


-- | False if (x y) z = x (y z)
check :: MTab -> Int -> Int -> Int -> Bool
check mtab x y z = xy >= 0 && yz >= 0 &&
                   xy_z >= 0 && x_yz >= 0 &&
                   xy_z /= x_yz
  where xy = evalMTab mtab x y
        yz = evalMTab mtab y z
        xy_z = evalMTab mtab xy z
        x_yz = evalMTab mtab x yz
{-# INLINE check #-}

isAsocIncmplRec :: MTab -> (Int, Int, Int) -> Bool
isAsocIncmplRec mtab (a, b, c)
  | (a, b, c) == (maxEle, maxEle, maxEle) = not $ check mtab a b c
  | check mtab a b c = False
  | otherwise = isAsocIncmplRec mtab (nextTrip a b c)

nextTrip :: Int -> Int -> Int -> (Int, Int, Int)
nextTrip x y z
  | y == maxEle && z == maxEle = (x + 1, 0, 0)
  | z == maxEle = (x, y + 1, 0)
  | otherwise = (x, y, z + 1)


isAsocIncmplIO :: MTab -> (Int, Int, Int) -> IO Bool
isAsocIncmplIO mtab (a, b, c)
  | (a, b, c) == (maxEle, maxEle, maxEle) = return (not $ check mtab a b c)
  | check mtab a b c = do
      putStrLn $ "not asoc: " ++ (show (a, b, c))
      return False
  | otherwise = isAsocIncmplIO mtab (nextTrip a b c)


{- |
Asoc check given that the table before entry (a b) was made/modified
was asoc already. Then these three cases have to be checked:

1. _ (a b) =          = (_ a) b      where _   = *
2. (a b) _ =     ab _ = a (b _)      where _   = *
3. (_ _) b =     ab   = _ (_ b)      where _ _ = a
4. a (_ _) =     ab   = (a _) _      where _ _ = b

-}
isAsocIncmpl2 :: MTab -> (Int, Int, Int) -> [(Int, Int)] -> [(Int, Int)]
                 -> Bool
isAsocIncmpl2 mtab (a, b, ab) axy bxy = and [checkABC mtab a b ab
                                            , checkCAB mtab a b ab
                                            , checkA mtab a ab bxy
                                            , checkB mtab b ab axy
                                            ]

checkABC :: MTab -> Int -> Int -> Int -> Bool
checkABC mtab a b ab = and [checkABC_ mtab a b ab c | c <- set]

checkCAB :: MTab -> Int -> Int -> Int -> Bool
checkCAB mtab a b ab = and [checkCAB_ mtab a b ab c | c <- set]

checkABC_:: MTab -> Int -> Int -> Int -> Int -> Bool
checkABC_ mtab a b ab c | any (< 0) [ab_c, bc, a_bc] = True
                        | otherwise = ab_c == a_bc
  where ab_c = evalMTab mtab ab c
        bc = evalMTab mtab b c
        a_bc = evalMTab mtab a bc

checkCAB_:: MTab -> Int -> Int -> Int -> Int -> Bool
checkCAB_ mtab a b ab c | any (< 0) [c_ab, ca, ca_b] = True
                        | otherwise = c_ab == ca_b
  where c_ab = evalMTab mtab c ab
        ca = evalMTab mtab c a
        ca_b = evalMTab mtab ca b

checkA :: MTab -> Int -> Int -> [(Int, Int)] -> Bool
checkA mtab a ab axy = and [checkA_ mtab a ab xy | xy <- axy]

checkA_ :: MTab -> Int -> Int -> (Int, Int) -> Bool
checkA_ mtab a ab (x, y) | any (< 0) [ax, ax_y] = True
                         | otherwise = ab == ax_y
  where ax = evalMTab mtab a x
        ax_y = evalMTab mtab ax y

checkB :: MTab -> Int -> Int -> [(Int, Int)] -> Bool
checkB mtab b ab bxy = and [checkB_ mtab b ab xy | xy <- bxy]

checkB_ :: MTab -> Int -> Int -> (Int, Int) -> Bool
checkB_ mtab b ab (x, y) | any (<0) [yb, x_yb] = True
                         | otherwise = ab == x_yb
  where yb = evalMTab mtab y b
        x_yb = evalMTab mtab x yb

getPreimage :: MTab -> Int -> [(Int, Int)]
getPreimage mtab im = map toRC (V.toList $ V.findIndices (== im) mtab)



{-
 Gen Zipper iord
-}
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



{-
 Processing MTabs
-}
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

toRC :: Int -> (Int, Int)
toRC x = (x `quot` length set, x `mod` length set)

selectRow :: Int -> MTab -> V.Vector Int
selectRow ri xs = V.drop (sizeM * ri) $ V.take (sizeM * (ri + 1)) xs

selectCol :: Int -> MTab -> V.Vector Int
--selectCol ci mtab = V.map (mtab V.!) (V.fromList indexList)
--  where indexList = map (\x -> x * sizeM + ci) set
selectCol ci mtab = V.map (mtab V.!) (colIndices !! ci)


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
    --False -> case isAsocIncmplRec r (0, 0, 0) of
    False -> case isAsocIncmpl r of
      False -> (r : t : ts, (Up, p + 1, iord))
      True -> (r : t : ts, (Fill, p + 1, iord))
doStep (t : ts) (Up, p, iord) = case upEntry_ t (iord V.! p) of
  Nothing -> (ts, (Up, p - 1, iord))
  --Just r -> case isAsocIncmplRec r (0, 0, 0) of
  Just r -> case isAsocIncmpl r of
    False -> (r : ts, (Up, p, iord))
    True -> (r : ts, (Fill, p, iord))

doStepA :: [MTab] -> Zip -> ([MTab], Zip)
doStepA [] zi = ([], zi)
doStepA (t : ts) (Fill, p, iord) = case upEntry_ t (iord V.! (p + 1)) of
  Nothing -> (t : ts, (Up, p, iord))
  Just r -> case isComplete r of
    True -> (r : t : ts, (Up, p + 1, iord))
    False -> case isAsocIncmpl2_ r (Fill, (p + 1), iord) of
      False -> (r : t : ts, (Up, p + 1, iord))
      True -> (r : t : ts, (Fill, p + 1, iord))
doStepA (t : ts) (Up, p, iord) = case upEntry_ t (iord V.! p) of
  Nothing -> (ts, (Up, p - 1, iord))
  Just r -> case isAsocIncmpl2_ r (Up, p, iord) of
    False -> (r : ts, (Up, p, iord))
    True -> (r : ts, (Fill, p, iord))

isAsocIncmpl2_ :: MTab -> Zip -> Bool
isAsocIncmpl2_ mtab (_, p, iord) = isAsocIncmpl2 mtab (a, b, ab) axy bxy
  where ind = (iord V.! p)
        ab = mtab V.! ind
        (a, b) = toRC ind
        axy = getPreimage mtab a
        bxy = getPreimage mtab b

doStepIO :: [MTab] -> Zip -> IO ([MTab], Zip)
doStepIO [] zi = return ([], zi)
doStepIO (t : ts) (Fill, p, iord) = case upEntry_ t (iord V.! (p + 1)) of
  Nothing -> do
    return (t : ts, (Up, p, iord))
  Just r -> case isComplete r of
    True -> do
      return (r : t : ts, (Up, p + 1, iord))
    False -> do
      let a1 = isAsocIncmpl r
      let a2 = isAsocIncmpl2_ r (Up, p + 1, iord)
      if a1 /= a2
        then do putStrLn $ (show a1) ++ " vs " ++ (show a2)
                a3 <- isAsocIncmplIO r (0, 0, 0)
                printMTab r
                --putStrLn "\n"
        else return ()
      case a1 of
        False -> do
          return (r : t : ts, (Up, p + 1, iord))
        True -> do
          return (r : t : ts, (Fill, p + 1, iord))
doStepIO (t : ts) (Up, p, iord) = case upEntry_ t (iord V.! p) of
  Nothing -> do
    return (ts, (Up, p - 1, iord))
  Just r -> do
    let a1 = isAsocIncmpl r
    let a2 = isAsocIncmpl2_ r (Up, p, iord)
    if a1 /= a2
      then do putStrLn $ (show a1) ++ " vs " ++ (show a2)
              a3 <- isAsocIncmplIO r (0, 0, 0)
              printMTab r
              --putStrLn "\n"
      else return ()
    case a1 of
      False -> do
        return (r : ts, (Up, p, iord))
      True -> do
        return (r : ts, (Fill, p, iord))



{-
 Printing MTabs
-}
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
