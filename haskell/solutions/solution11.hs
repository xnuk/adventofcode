{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- {-# LANGUAGE QualifiedDo #-}

---------- WIP ----------

module Main where

import "containers" Data.IntMap.Lazy (IntMap, restrictKeys, withoutKeys)
import "containers" Data.IntMap.Lazy qualified as M
import "containers" Data.IntSet (IntSet)
import "containers" Data.IntSet qualified as S
import Data.List qualified as L
import Data.Monoid (First (..))
import "text" Data.Text qualified as T
import Debug.Trace

-- import Common.Chain qualified as Chain
-- import Common.Pot qualified as Pot

newtype Field = Field [[Word8]]

instance Show Field where
  show (Field a) = "Field " <> show a

newtype Size a = Size {unSize :: a} deriving (Show)

newtype Index a = Index a

data Indices
  = Indices
      (Size Int)
      [IntSet] -- 9, 8, 7, 6, ..., 1, 0
  deriving (Show)

data Case = Case (Index Int) Int Indices

newtype Pretty a = Pretty a

instance Show (Pretty Indices) where
  show (Pretty (Indices (Size size) datum)) =
    datum
      & zipWith zipper [pred size, pred (pred size) .. 0]
      & (map (getFirst . mconcat) . transpose)
      & chunksEach size
      & takeWhile (not . null . catMaybes)
      & map (concatMap (maybe " " show))
      & L.unlines
    where
      hole x i =
        replicate i (First Nothing)
          <> [First (Just x)]
          <> repeat (First Nothing)
      zipper :: Int -> IntSet -> [First Int]
      zipper x = toList &. map (hole x) &. transpose &. map mconcat

instance Show (Pretty Case) where
  show (Pretty (Case (Index i) fl indi)) =
    L.unlines
      [ "Step " <> show i <> ": (from " <> show fl <> " flashes)"
      , ""
      , show (Pretty indi)
      ]

chunksEach :: Int -> [a] -> [[a]]
chunksEach i xs = a : chunksEach i b
  where
    (a, b) = splitAt i xs

repl :: (Monoid m, Integral i) => i -> m -> [m]
repl i x = replicate (fromIntegral i) mempty <> [x] <> repeat mempty

fromField :: Field -> Indices
fromField (Field []) = Indices (Size 0) []
fromField (Field xs@(h : _)) =
  xs
    & concat
    & zip [0 ..]
    & sortOn snd
    & groupBy ((==) `on` snd)
    & map (unzip &. (fromList *** head) &. (\(x, i) -> repl i x))
    & transpose
    & take 10
    & map mconcat
    & reverse
    & Indices (Size $ length h)

-- (rows, cols)
decodeIndices :: (Integral i, Integral w) => Size i -> i -> (w, w)
decodeIndices (Size size) = (`divMod` size) &. (fromIntegral *** fromIntegral)

encodeIndices :: (Integral i, Integral w) => Size i -> (w, w) -> i
encodeIndices (Size size) (row, col) =
  fromIntegral row * size + fromIntegral col

-- assuming it's square
near :: Size Int -> Int -> [Int]
near = result
  where
    patch :: Size Int -> Int -> [Int]
    patch (Size size) =
      sequence [pred, id, succ] &. filter (\x -> x >= 0 && x < size)
    encode :: Size Int -> ([Int], [Int]) -> [Int]
    encode = encodeIndices &. curry &. zipWith &. uncurry
    result size = decodeIndices size &. (patch size *** patch size) &. encode size

addOne :: Num a => Int -> IntMap a -> IntMap a
addOne x = M.insertWith (+) x 1

nearby :: Integral a => Size Int -> IntSet -> IntMap a
nearby size set =
  toList set
    & foldr (near size &. filter (`S.notMember` set) &. foldr ((.) . addOne) id) mempty
    & (`withoutKeys` set)

lookMember :: Int -> IntSet -> Maybe (Int, IntSet)
lookMember v xs
  | S.member v xs = Just (v, S.delete v xs)
  | otherwise = Nothing

findAndAlter :: (a -> Maybe (b, a)) -> [a] -> (Maybe b, [a])
findAndAlter _ [] = (Nothing, [])
findAndAlter f (x : xs) =
  case f x of
    Just (res, n) -> (Just res, n : xs)
    Nothing ->
      -- this will overflow stack, but who cares \_()_/
      second (x :) $ findAndAlter f xs

popOut :: Int -> [IntSet] -> (Maybe (Index Int), [IntSet])
popOut v = zip (map Index [0 ..]) &. runner
  where
    runner :: [(Index a, IntSet)] -> (Maybe (Index a), [IntSet])
    runner = foldr go (Nothing, [])

    go (_, set) (result@(Just _), sets) = (result, set : sets)
    go (i, set) (Nothing, sets)
      | S.member v set = (Just i, S.delete v set : sets)
      | otherwise = (Nothing, set : sets)

separate :: IntMap a -> IntSet -> (IntMap a, IntMap a, IntSet)
separate intMap intSet = (diff, inter, remainder)
  where
    diff = withoutKeys intMap intSet
    inter = restrictKeys intMap intSet
    remainder = intSet `S.difference` M.keysSet intMap

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt _ _ [] = []
updateAt n f (x : xs)
  | n <= 0 = f x : xs
  | otherwise = x : updateAt (pred n) f xs

datumAt :: Int -> Indices -> IntSet
datumAt i (Indices _ datum) = datum !! i

updateDatum :: ([IntSet] -> [IntSet]) -> Indices -> Indices
updateDatum f (Indices size datum) = Indices size $ f datum

replaceAt :: Int -> (IntSet -> IntSet) -> Indices -> Indices
replaceAt i = updateDatum . updateAt i

getFlashes :: Indices -> Int
getFlashes (Indices _ []) = 0
getFlashes (Indices _ (nines : _)) = S.size nines

rotate :: Indices -> Indices
rotate (Indices size []) = Indices size []
rotate (Indices size (nines : others)) = Indices size (others <> [nines])

zarbIter :: (IntMap Int, Indices) -> (IntMap Int, Indices)
zarbIter = chub . step
  where
    perIndex :: IntMap Int -> Int -> Indices -> Indices
    perIndex iMap index indi = newIndi
      where
        subtractCount count
          | index <= count = 0
          | otherwise = index - count

        (newMap, inter, newSet) = separate iMap (datumAt index indi)

        updateToNewOne = replaceAt index (const newSet)

        foldup :: IntMap Int -> Indices -> Indices
        foldup = flip M.foldrWithKey id \key count ->
          (.) $
            replaceAt (subtractCount count) (S.insert key)

        -- findNines = M.filter $ subtractCount &. (== 0)

        newIndi = foldup inter . updateToNewOne $ indi
    -- nextMap =
    --   newMap <> nearby size (M.keysSet $ findNines inter)
    --     & traceShow (inter, index)

    step :: (IntMap Int, Indices) -> (IntMap Int, Indices)
    step (iMap, indi@(Indices size datum)) = (newMap, newIndi)
      where
        newIndi@(Indices _ (nines : _)) =
          foldr (perIndex iMap) indi [1 .. pred (length datum)]
        newMapKeys = nines `S.difference` M.keysSet iMap
        newMap :: IntMap Int
        newMap = nearby size newMapKeys

    chub :: (IntMap Int, Indices) -> (IntMap Int, Indices)
    chub a@(_, Indices _ []) = a
    chub (iMap, indi@(Indices _ (nines : _))) =
      (withoutKeys iMap nines, indi)
        & traceShow (Pretty indi)

zarbFirst :: Indices -> (IntMap Int, Indices)
zarbFirst indices@(Indices _ []) = (mempty, indices)
zarbFirst indices@(Indices size (nines : _)) = (nearby size nines, indices)

zarbFix :: (IntMap Int, Indices) -> Indices
zarbFix (iMap, indi)
  | M.null iMap = indi
  | otherwise = zarbFix . zarbIter $ (iMap, indi)

zarb :: Indices -> Indices
zarb = zarbFix . zarbFirst . trace "fuck"

runStep :: Indices -> (Int, Indices)
runStep = zarb &. (getFlashes &&& rotate)

runSteps :: Indices -> [Int]
runSteps = zarb &. ((:) <$> getFlashes <*> (runSteps . rotate))

runStepsDebug :: Indices -> [Case]
runStepsDebug = go (Index 0)
  where
    go i@(Index 0) = (:) <$> (Case i 0 <$> id) <*> go (Index 1)
    go i@(Index n) =
      (zarb &.) $
        (:)
          <$> (Case i <$> getFlashes <*> rotate)
          <*> (go (Index $ n + 1) . rotate)

withField :: ([[Word8]] -> [[Word8]]) -> Field -> Field
withField f (Field x) = Field (f x)

testInput :: [Text]
testInput =
  [ "5483143223"
  , "2745854711"
  , "5264556173"
  , "6141336146"
  , "6357385478"
  , "4167524645"
  , "2176841721"
  , "6882881134"
  , "4846848554"
  , "5283751526"
  ]

toDigit :: Char -> Maybe Int
toDigit c
  | zero <= o && o <= nine = Just $ o - zero
  | otherwise = Nothing
  where
    zero = ord '0'
    nine = ord '9'
    o = ord c

------ bringing functions from solution 09

wrap :: a -> [a] -> [a]
wrap a xs = a : xs <> [a]

unwrap :: [a] -> [a]
unwrap [] = []
unwrap [_] = []
unwrap (_ : xs) = init xs

wrapMatrix :: a -> [[a]] -> [[a]]
wrapMatrix a = wrap (repeat a) . map (wrap a)

unwrapMatrix :: [[a]] -> [[a]]
unwrapMatrix = map unwrap . unwrap

mapNear :: (a -> a -> a -> b) -> [a] -> [b]
mapNear f xs = zipWith3 f xs (drop 1 xs) (drop 2 xs)

zipNear :: [a] -> [(a, a, a)]
zipNear xs = zip3 xs (drop 1 xs) (drop 2 xs)

forNear :: [a] -> (a -> a -> a -> b) -> [b]
forNear = flip mapNear

------

-- 9 8 8 8 8 8 8 -> light all up on next step

zipMatrixNear :: [[a]] -> [[((a, a, a), (a, a, a), (a, a, a))]]
zipMatrixNear = mapNear zip3 . map zipNear

preLight :: Field -> Field
preLight = withField $ wrapMatrix 0

parseInput :: [Text] -> Field
parseInput = Field . map (mapMaybe (fmap fromIntegral . toDigit) . T.unpack)

firstHalf :: Field -> Int
firstHalf = fromField &. runSteps &. take 100 &. sum

main :: IO ()
main = do
  input <- parseInput <$> pure testInput
  let a = fromField input & runStepsDebug & map Pretty & take 101
  mapM_ print a
