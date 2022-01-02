module Main (main) where

import "unordered-containers" Data.HashSet (HashSet)
import "unordered-containers" Data.HashSet qualified as H
import "hashable" Data.Hashable (Hashable)
import Data.List qualified as L
import Data.Monoid (First (..))

newtype Pretty a = Pretty a

newtype Paper a = Paper (HashSet (a, a)) deriving (Show)

chunksAt :: Int -> [a] -> [[a]]
chunksAt i = splitAt i &. \(a, b) -> a : chunksAt i b

instance Show (Pretty (Paper Int)) where
  show (Pretty (Paper set)) = foldup list
    where
      list = toList set
      (maxX, maxY) = join (***) (succ . maximum) . unzip $ list
      putAt :: (Int, Int) -> [First Char]
      putAt (x, y) =
        replicate (y * maxX + x) (First Nothing)
          <> (First (Just '#') : repeat (First Nothing))
      foldup =
        map putAt
          &. transpose
          &. map (mconcat &. getFirst &. fromMaybe '.')
          &. chunks
          &. L.unlines
      chunks = chunksAt maxX &. take maxY

data FoldAlong a = FoldX a | FoldY a deriving (Eq, Show)

-- data Game a = Game (Paper a) [FoldAlong a]

type SaneInt a = (Integral a, Hashable a, Ix a, Show a)

mapPaper :: SaneInt a => ((a, a) -> (a, a)) -> Paper a -> Paper a
mapPaper f (Paper a) = Paper $ H.map f a

readTuple :: Integral a => Text -> Maybe (a, a)
readTuple =
  splitOn "," &. mapMaybe (decimal . strip) &. \case
    [a, b] -> Just (a, b)
    _ -> Nothing

altM :: (b -> Maybe a) -> (b -> Maybe a) -> b -> Maybe a
altM = liftM2 (<|>)

readFoldPrefix :: Integral a => (a -> b) -> Text -> Text -> Maybe b
readFoldPrefix f prefix = fmap f . (stripPrefix prefix >=> decimal)

readFoldAlong :: Integral a => Text -> Maybe (FoldAlong a)
readFoldAlong =
  (stripPrefix "fold along " . strip)
    >=> (readFoldPrefix FoldX "x=" `altM` readFoldPrefix FoldY "y=")

parseLine :: Integral a => Text -> Maybe (Either (a, a) (FoldAlong a))
parseLine = (fmap Right . readFoldAlong) `altM` (fmap Left . readTuple)

parseInput :: SaneInt a => [Text] -> (Paper a, [FoldAlong a])
parseInput =
  mapMaybe parseLine
    &. partitionEithers
    &. first (Paper . fromList)

origami :: SaneInt a => FoldAlong a -> Paper a -> Paper a
origami fl = mapPaper $ case fl of
  FoldX x -> first $ folding x
  FoldY y -> second $ folding y
  where
    folding line a
      | line < a = line * 2 - a
      | otherwise = a

firstHalf :: (Paper Int, [FoldAlong Int]) -> Int
firstHalf (Paper a, []) = length a
firstHalf (paper, cmd : _) = length a
  where
    (Paper a) = origami cmd paper

secondHalf :: (Paper Int, [FoldAlong Int]) -> Pretty (Paper Int)
secondHalf = uncurry (foldl (flip origami)) &. Pretty

main :: IO ()
main = do
  input <- getLines <&> parseInput
  putStrLn $ "First half is: " <> show (firstHalf input)
  putStrLn $ "Second half is: \n" <> show (secondHalf input)

  return ()
