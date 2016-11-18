module Gen.GenSimple ( runGenM
                     , runGenSilent ) where

import qualified Data.Vector.Unboxed as V

import Gen.AsocTest
import Gen.Common


{- |
  Inital of the form
     0  1  2  3 .
     1  0 -1 -1 .
     2 -1 -1 -1 .
     3 -1 -1 -1 .
     .  .  .  . .
  Note that the 0 at pos (1,1) is there to avoid index -1 issues
-}
-- TODO: Maybe just use vector not do the toList fromList thing
genInitial :: Param -> MTab
genInitial (s, vset, _) = V.fromList $ concat $ set : (map row (tail set))
  where row 1 = 1 : 0 : (replicate (s - 2) (-1))
        row m = m : (replicate (s - 1) (-1))
        set = V.toList vset

getPreimage :: MTab -> Int -> Int -> [(Int, Int)]
getPreimage mtab s im = map (toRC s) (V.toList $ V.findIndices (== im) mtab)

toRC :: Int -> Int -> (Int, Int)
toRC s x = (x `quot` s, x `mod` s)

replaceAtIndex :: MTab -> Int -> Int -> MTab
replaceAtIndex mtab ind item = V.update mtab (V.singleton (ind, item))

nextBiggest :: V.Vector Int -> V.Vector Int -> Maybe Int
nextBiggest set constr = V.find (\x -> not $ V.elem x constr) set

upEntry :: Param -> MTab -> Int -> Maybe MTab
upEntry (s, set, cols) mtab ind
  | x == maxE = Nothing
  | otherwise = fmap (\y -> replaceAtIndex mtab ind y) nextB
  where x = mtab V.! ind
        (r, c) = toRC s ind
        constr = V.enumFromN 0 (x + 1) -- [0, 1, 2] <> 0, 2+1=3
                 V.++ (selectRow mtab s r)
                 V.++ (selectCol mtab c cols)
        nextB = nextBiggest set constr
        maxE = s - 1

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

pathLine :: Int -> V.Vector Int
pathLine n = V.fromList $ shiftByOne n [0..n*n-1]

shiftByOne :: Int -> [Int] -> [Int]
shiftByOne n ys = concat [map (+ i) zs | (i, zs) <- zip [n+2..2*n+1]
                                                    (chunks n ys [[]])]
  where chunks _ [] res = reverse (map reverse res)
        chunks m (x : xs) [[]] = chunks m xs [[x]]
        chunks m (x : xs) res
          | length (head res) == m = chunks m xs ([x] : res)
          | otherwise = chunks m xs ((x : (head res)) : (tail res))



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
doStep :: Param -> [MTab] -> Zip -> ([MTab], Zip)
doStep _ [] zi = ([], zi)
doStep pr (t : ts) (Fill, p, iord) = case upEntry pr t (iord V.! (p + 1)) of
  Nothing -> (t : ts, (Up, p, iord))
  Just r -> case isComplete r of
    True -> (r : t : ts, (Up, p + 1, iord))
    False -> case testAsocIncrm pr r (Fill, (p + 1), iord) of
      False -> (r : t : ts, (Up, p + 1, iord))
      True -> (r : t : ts, (Fill, p + 1, iord))
doStep pr (t : ts) (Up, p, iord) = case upEntry pr t (iord V.! p) of
  Nothing -> (ts, (Up, p - 1, iord))
  Just r -> case testAsocIncrm pr r (Up, p, iord) of
    False -> (r : ts, (Up, p, iord))
    True -> (r : ts, (Fill, p, iord))

-- Helper for incremental asoc
testAsocIncrm :: Param -> MTab -> Zip -> Bool
testAsocIncrm pr mtab (_, p, iord) = isAsocIncmplIncrm pr mtab (a, b, ab) axy bxy
  where ind = (iord V.! p)
        ab = mtab V.! ind
        (a, b) = toRC s ind
        axy = getPreimage mtab s a
        bxy = getPreimage mtab s b
        s = (\(x, _, _) -> x) pr

-- | Version using bad asoc testing, for testing and validation
doStepB :: Param -> [MTab] -> Zip -> ([MTab], Zip)
doStepB _ [] zi = ([], zi)
doStepB pr (t : ts) (Fill, p, iord) = case upEntry pr t (iord V.! (p + 1)) of
  Nothing -> (t : ts, (Up, p, iord))
  Just r -> case isComplete r of
    True -> (r : t : ts, (Up, p + 1, iord))
    False -> case isAsocIncmplRec pr r (0, 0, 0) of
    --False -> case isAsocIncmplToList pr r of
      False -> (r : t : ts, (Up, p + 1, iord))
      True -> (r : t : ts, (Fill, p + 1, iord))
doStepB pr (t : ts) (Up, p, iord) = case upEntry pr t (iord V.! p) of
  Nothing -> (ts, (Up, p - 1, iord))
  Just r -> case isAsocIncmplRec pr r (0, 0, 0) of
  --Just r -> case isAsocIncmplToList pr r of
    False -> (r : ts, (Up, p, iord))
    True -> (r : ts, (Fill, p, iord))


-- | IO Variant using IO asoc testing
doStepIO :: Param -> [MTab] -> Zip -> IO ([MTab], Zip)
doStepIO _ [] zi = return ([], zi)
doStepIO pr (t : ts) (Fill, p, iord) = case upEntry pr t (iord V.! (p + 1)) of
  Nothing -> do
    return (t : ts, (Up, p, iord))
  Just r -> case isComplete r of
    True -> do
      return (r : t : ts, (Up, p + 1, iord))
    False -> do
      let a1 = isAsocIncmplToList pr r
      let a2 = testAsocIncrm pr r (Up, p + 1, iord)
      if a1 /= a2
        then do putStrLn $ (show a1) ++ " vs " ++ (show a2)
                _ <- isAsocIncmplIO pr r (0, 0, 0)
                return ()
                --printMTab r
                --putStrLn "\n"
        else return ()
      case a1 of
        False -> do
          return (r : t : ts, (Up, p + 1, iord))
        True -> do
          return (r : t : ts, (Fill, p + 1, iord))
doStepIO pr (t : ts) (Up, p, iord) = case upEntry pr t (iord V.! p) of
  Nothing -> do
    return (ts, (Up, p - 1, iord))
  Just r -> do
    let a1 = isAsocIncmplToList pr r
    let a2 = testAsocIncrm pr r (Up, p, iord)
    if a1 /= a2
      then do putStrLn $ (show a1) ++ " vs " ++ (show a2)
              _ <- isAsocIncmplIO pr r (0, 0, 0)
              return ()
              --printMTab r
              --putStrLn "\n"
      else return ()
    case a1 of
      False -> do
        return (r : ts, (Up, p, iord))
      True -> do
        return (r : ts, (Fill, p, iord))

-- | Run generation in a monadic setting
runGenM :: Monad m => Param -> info -> (SRes -> info -> m info)
          -> m (SRes, info)
runGenM param info f = do
  let t0 = genInitial param
  let z0 = (Fill, 0, pathQuad $ (\(s, _, _) -> s - 1) param)
  let s1 = doStep param [t0] z0
  infon <- f s1 info
  iterGen param s1 infon f

-- | Run generation without any output
runGenSilent :: Param -> IO ()
runGenSilent param = do
  _ <- runGenM param [] (\_ _ -> return [])
  return ()

-- | Iteration by recursion
iterGen :: Monad m => Param -> SRes -> info -> (SRes -> info -> m info)
           -> m (SRes, info)
iterGen _ ([], z) info _ = return (([], z), info)
iterGen param (ts, z) info f = do
  let sn = doStep param ts z
  infon <- f sn info
  iterGen param sn infon f
