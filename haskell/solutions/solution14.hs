module Main where

import "unordered-containers" Data.HashMap.Lazy (HashMap)
import "unordered-containers" Data.HashMap.Lazy qualified as H
import "hashable" Data.Hashable (Hashable)

type HashKey a = (Eq a, Hashable a)

type CountingUnpacked = HashMap (Char, Char) Int

newtype Counting = Counting CountingUnpacked deriving (Show)

data Insert a = Insert (a, a) a deriving (Show)

dummyChar :: Char
dummyChar = '\0'

padDummy :: [Char] -> [Char]
padDummy xs = dummyChar : xs <> [dummyChar]

resolveInsert :: Insert a -> ((a, a), (a, a))
resolveInsert (Insert (a, c) b) = ((a, b), (b, c))

convertInsert :: (a -> b) -> Insert a -> Insert b
convertInsert f (Insert (a, c) b) = Insert (f a, f c) (f b)

counting :: (HashKey k, Num v) => [k] -> HashMap k v
counting = flip foldr mempty \k -> H.insertWith (+) k 1

testInput :: [Text]
testInput =
  [ "NNCB"
  , ""
  , "CH -> B"
  , "HH -> N"
  , "CB -> H"
  , "NH -> C"
  , "HB -> C"
  , "HC -> B"
  , "HN -> C"
  , "NN -> C"
  , "BH -> H"
  , "NC -> B"
  , "NB -> B"
  , "BN -> B"
  , "BB -> N"
  , "BC -> B"
  , "CC -> N"
  , "CN -> C"
  ]

parseQuery :: Text -> Maybe Counting
parseQuery =
  unpackText &. padDummy &. \a -> case zip a (tailSafe a) of
    [] -> Nothing
    xs -> Just . Counting $ counting xs

parseArrow :: Text -> Maybe (Insert Char)
parseArrow =
  splitOn " -> " &. map (unpackText . strip) &. \case
    [[a, c], [b]] -> Just $ Insert (a, c) b
    _ -> Nothing

altM :: (Applicative f, Alternative g) => f (g a) -> f (g a) -> f (g a)
altM = liftA2 (<|>)

parseInput :: [Text] -> Maybe (Counting, [Insert Char])
parseInput =
  mapMaybe ((fmap Right . parseArrow) `altM` (fmap Left . parseQuery))
    &. partitionEithers
    &. \case
      ([], _) -> Nothing
      (_, []) -> Nothing
      (counts : _, commands) -> Just (counts, commands)

applyInsert ::
  Counting ->
  Insert Char ->
  Maybe ((Char, Char), CountingUnpacked -> CountingUnpacked)
applyInsert (Counting counts) insert@(Insert target _) = applied
  where
    (next1, next2) = resolveInsert insert
    applied =
      H.lookup target counts
        <&> \v ->
          (target, H.insertWith (+) next1 v &. H.insertWith (+) next2 v)

step :: Counting -> [Insert Char] -> Counting
step counts@(Counting uncounted) =
  mapMaybe (applyInsert counts)
    &. unzip
    &. f
    &. Counting
  where
    f ::
      ([(Char, Char)], [CountingUnpacked -> CountingUnpacked]) ->
      CountingUnpacked
    f (removingKeys, funcs) =
      uncounted
        & flip (foldr H.delete) removingKeys
        & flip (foldr ($)) funcs

charCounts :: Counting -> HashMap Char Int
charCounts (Counting counts) = result
  where
    folding (a, b) count
      | a == b = H.insertWith (+) a (count * 2)
      | otherwise = H.insertWith (+) a count . H.insertWith (+) b count
    result =
      H.foldrWithKey folding mempty counts
        & H.delete dummyChar
        & fmap (`div` 2)

getCount :: Int -> (Counting, [Insert Char]) -> Int
getCount index (counts, cmds) = maximum chars - minimum chars
  where
    res = iterate (`step` cmds) counts !! index
    chars = charCounts res

firstHalf, secondHalf :: (Counting, [Insert Char]) -> Int
firstHalf = getCount 10
secondHalf = getCount 40

main :: IO ()
main = do
  Just input <- getLines <&> parseInput
  putStrLn $ "First half is: " <> showt (firstHalf input)
  putStrLn $ "Second half is: " <> showt (secondHalf input)
  pure ()
