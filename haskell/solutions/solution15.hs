module Main where

import "containers" Data.IntMap.Lazy (IntMap)
import "containers" Data.IntMap.Lazy qualified as I
import "psqueues" Data.IntPSQ (IntPSQ, minView)
import "psqueues" Data.IntPSQ qualified as Q
import "containers" Data.IntSet (IntSet)
import "containers" Data.IntSet qualified as S
import Data.List qualified as L

-- | (rows, cols)
newtype Size = Size (Int, Int) deriving (Show)

data Field a = Field Size (IntMap a) deriving (Show)

data Running s a = Complete a | Incomplete (Maybe a) s | Fail deriving (Show)

newtype Score = Score (Int, Int) deriving (Show, Eq, Ord)

newtype Risk = Risk Int deriving (Show, Eq, Ord)

type Viewer = IntPSQ Score (Risk, IntSet)

newtype Pretty a = Pretty a

instance Show (Pretty (Field Word8, IntSet)) where
  show (Pretty (Field (Size (rows, cols)) intmap, set)) =
    "\n" <> L.unlines result <> "\n"
    where
      maxPos = rows * cols - 1
      fieldMap :: IntMap Word8 -> IntMap Char
      fieldMap =
        I.mapKeys (maxPos -) . I.mapWithKey \i v ->
          if S.member i set
            then ['0' .. '9'] !! fromIntegral v
            else '.'

      flatten :: a -> [(Int, a)] -> [a]
      flatten x xs = concat $ zipWith f ((0, x) : xs) xs <> [repeat x]
        where
          f (i, a) (j, _)
            | i >= j = []
            | otherwise = a : replicate (j - i - 1) x

      result =
        fieldMap intmap
          & I.toAscList
          & flatten '.'
          & chunkBy cols &. take rows

chunkBy :: Int -> [a] -> [[a]]
chunkBy i = splitAt i &. \(a, b) -> a : chunkBy i b

score :: Size -> Int -> Risk -> Score
score (Size (_, col)) point (Risk risk) =
  Score (risk, pointDistance point)
  where
    pointDistance = uncurry (+) . (`divMod` col)

testInput :: [Text]
testInput =
  [ "1163751742"
  , "1381373672"
  , "2136511328"
  , "3694931569"
  , "7463417111"
  , "1319128137"
  , "1359912421"
  , "3125421639"
  , "1293138521"
  , "2311944581"
  ]

toDigit :: Integral a => Char -> Maybe a
toDigit c
  | inRange (0, 9) (x :: Int) = Just x
  | otherwise = Nothing
  where
    x :: Integral a => a
    x = ord c - ord '0'

parseInput :: [Text] -> Field Word8
parseInput =
  map (mapMaybe toDigit . unpackText) &. do
    (rows, cols) <- length . headDef [] &&& length
    concated <- concat

    let total = length concated
        intmap =
          -- reversed
          zipWith I.insert [total - 1, total - 2 .. 0] concated
            & foldr ($) mempty

    pure $ Field (Size (rows, cols)) intmap

nearbyPoints :: Size -> Int -> [Int]
nearbyPoints (Size (rows, cols)) point =
  [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
    & filter rangeCheck
    & map \(a, b) -> a * cols + b
  where
    (r, c) = point `divMod` cols
    belowNat :: Int -> Int -> Bool
    belowNat x a = 0 <= a && a < x
    rangeCheck = uncurry (&&) . (belowNat rows *** belowNat cols)

lookupField :: Int -> Field a -> Maybe a
lookupField k (Field _ intmap) = I.lookup k intmap

addRisk :: Int -> Risk -> Risk
addRisk i (Risk a) = Risk (a + i)

insertMin :: Ord p => Int -> p -> v -> IntPSQ p v -> IntPSQ p v
insertMin k p v = do
  f <-
    Q.lookup k &. \case
      Just (oldP, _)
        | p < oldP -> Q.delete k &. Q.insert k p v
        | otherwise -> id
      Nothing -> Q.insert k p v

  -- f . trace (show (k `divMod` 10, p, v))
  f

step :: Field Word8 -> Viewer -> Running Viewer (Risk, IntSet)
step field@(Field size _) m = case minView m of
  Nothing -> Fail
  Just (point, _, (risk, path), psq) ->
    if
        | point /= 0 -> Incomplete Nothing nextPSQ
        | not (Q.null m) -> Incomplete (Just (risk, path)) nextPSQ
        | otherwise -> Complete (risk, path)
    where
      nextPoints = nearbyPoints size point & filter (`S.notMember` path)
      fromPoint p =
        lookupField p field <&> fromIntegral
          &. (`addRisk` risk)
          &. \newRisk -> insertMin p (score size p newRisk) (newRisk, S.insert p path)
      nextPSQ =
        nextPoints
          & mapMaybe fromPoint
          & foldr ($) psq

startStep :: Field Word8 -> Running Viewer (Risk, IntSet)
startStep (Field size@(Size (rows, cols)) _) = Incomplete Nothing viewer
  where
    point = rows * cols - 1
    risk = Risk 0
    viewer = Q.singleton point (score size point risk) (risk, S.singleton point)

runner :: Field Word8 -> [(Risk, Pretty (Field Word8, IntSet))]
runner field =
  map (second $ Pretty . (field,)) . next $ startStep field
  where
    next (Complete risk) = [risk]
    next (Incomplete Nothing viewer) = next (step field viewer)
    next (Incomplete (Just risk) viewer) = risk : next (step field viewer)
    next Fail = []

bumpField :: Integral a => Field a -> Field a
bumpField (Field (Size (rows, cols)) intmap) = Field newSize newMap
  where
    newSize@(Size (newRows, newCols)) = Size (rows * 5, cols * 5)
    newMap = I.foldrWithKey spread mempty intmap
    spread :: Integral a => Int -> a -> IntMap a -> IntMap a
    spread k a = foldr (\(nk, nv) -> (.) (I.insert nk nv)) id newEntries
      where
        (r, c) = (rows * cols - 1 - k) `divMod` cols

        newEntries :: Integral a => [(Int, a)]
        newEntries = do
          ri <- [0 .. 4]
          ci <- [0 .. 4]

          let row = rows * ri + r
              col = cols * ci + c
              na = fromIntegral a + ri + ci
              chomped = ((na - 1) `mod` 9) + 1
              key = newRows * newCols - 1 - row * newCols - col
          pure (key, fromIntegral chomped)

main :: IO ()
main = do
  input <- getLines <&> parseInput
  let ~(Risk firstHalf, firstPretty) = head . runner $ input
      ~(Risk secondHalf, secondPretty) = head . runner $ bumpField input

  putStrLn $ "First Half: " <> showt firstHalf
  print firstPretty

  putStrLn $ "Second Half: " <> showt secondHalf
  print secondPretty

  pure ()
