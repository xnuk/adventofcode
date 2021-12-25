import "unordered-containers" Data.HashSet (HashSet)
import "unordered-containers" Data.HashSet qualified as S
import "hashable" Data.Hashable (Hashable (hashWithSalt))

newtype X a = X a deriving (Eq, Show, Ord)

newtype Y a = Y a deriving (Eq, Show, Ord)

instance Hashable a => Hashable (X a) where
  hashWithSalt salt (X a) = hashWithSalt salt a

instance Hashable a => Hashable (Y a) where
  hashWithSalt salt (Y a) = hashWithSalt salt a

newtype Intersect a = Intersect a

data Line a = (X a, Y a) :> (X a, Y a) deriving (Show)

data Occur = Once | Many deriving (Show, Enum, Eq)

parseLine :: Integral a => Text -> Maybe (Line a)
parseLine =
  splitOn "->"
    &. concatMap (splitOn "," . strip)
    &. mapMaybe decimal
    &. \case
      [x1, y1, x2, y2] -> Just $ (X x1, Y y1) :> (X x2, Y y2)
      _ -> Nothing

parseInput :: Integral a => Text -> [Line a]
parseInput = strip &. lines &. mapMaybe parseLine

filterRectLines :: Eq a => [Line a] -> [Line a]
filterRectLines = filter \((x1, y1) :> (x2, y2)) -> x1 == x2 || y1 == y2

enumOrd :: (Ord a, Enum a) => a -> a -> [a]
enumOrd from to
  | from == to = [from]
  | from > to = enumFromThenTo from (pred from) to
  | otherwise = enumFromTo from to

getRectPoints :: (Ord a, Enum a) => Line a -> [(X a, Y a)]
getRectPoints ((x1, y1) :> (x2, y2))
  | x1 == x2 = map (x1,) enumY
  | y1 == y2 = map (,y1) enumX
  | otherwise = zip enumX enumY -- diag
  where
    enumX = map X $ enumOrd unx1 unx2
      where
        (X unx1, X unx2) = (x1, x2)

    enumY = map Y $ enumOrd uny1 uny2
      where
        (Y uny1, Y uny2) = (y1, y2)

mergeSet ::
  (Eq a, Hashable a) =>
  HashSet a ->
  HashSet a ->
  (HashSet a, Intersect (HashSet a))
mergeSet a = (<>) a &&& (Intersect . S.intersection a)

-- naive implementation is: drawing all.
draw :: [Line Int] -> Int
draw = result
  where
    result =
      map (S.fromList . getRectPoints)
        &. foldr f (S.empty, S.empty)
        &. snd
        &. S.size
    f x (total, dups) = (nextTotal, dups <> nextDups)
      where
        (nextTotal, Intersect nextDups) = mergeSet total x

main :: IO ()
main = do
  a <- parseInput <$> getContents
  putStrLn $ "First half is: " <> showt (draw $ filterRectLines a)
  putStrLn $ "Second half is: " <> showt (draw a)
