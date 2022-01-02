module Main (main, tests) where

import Control.Exception (AssertionFailed (..), throw)
import "bytestring" Data.ByteString.Short (ShortByteString)
import "bytestring" Data.ByteString.Short qualified as B
import "unordered-containers" Data.HashMap.Lazy (HashMap)
import "unordered-containers" Data.HashMap.Lazy qualified as M
import "unordered-containers" Data.HashSet (HashSet)
import "unordered-containers" Data.HashSet qualified as S
import "hashable" Data.Hashable (Hashable (..))
import "text" Data.Text qualified as T

-- data Line a = Line a a deriving (Show, Eq)

data Node
  = Start
  | End
  | Big !ShortByteString
  | Small !ShortByteString

data LineNode a
  = LineStartBig a
  | LineStartSmall a
  | LineEndBig a
  | LineEndSmall a
  | LineSmall a a
  | LineBigSmall a a

type Line = LineNode ShortByteString

fromNode :: Node -> ShortByteString
fromNode Start = "start"
fromNode End = "end"
fromNode (Big a) = a
fromNode (Small a) = a

instance Eq Node where
  Start == Start = True
  End == End = True
  Big a == Big b = a == b
  Small a == Small b = a == b
  _ == _ = False

instance Show Node where
  show = show . fromNode

instance Hashable Node where
  hashWithSalt salt = hashWithSalt salt . fromNode

assertEqual :: (Eq a, Show a) => Text -> a -> a -> [Text] -> [Text]
assertEqual message a b
  | a == b = id
  | otherwise =
    flip (<>) . (: []) . mconcat $
      ["Assertion ", message, ": ", showt a, " =/= ", showt b]

encodeAlpha :: ShortByteString -> Maybe Node
encodeAlpha xs
  | B.null xs = Nothing
  | xs == "start" = Just Start
  | xs == "end" = Just End
  | inRange (ord 'A', ord 'Z') h = Just $ Big xs
  | inRange (ord 'a', ord 'z') h = Just $ Small xs
  | otherwise = Nothing
  where
    h = B.index xs 0

fromText :: Text -> ShortByteString
fromText = strip &. T.unpack &. map ord &. B.pack

fromLines :: [Line] -> HashMap Node (HashSet Node)
fromLines = flip foldr mempty $ \case
  LineStartBig a -> insert Start (Big a)
  LineStartSmall a -> insert Start (Small a)
  LineEndBig a -> insert (Big a) End
  LineEndSmall a -> insert (Small a) End
  LineSmall a b -> insert (Small a) (Small b) . insert (Small b) (Small a)
  LineBigSmall a b -> insert (Big a) (Small b) . insert (Small b) (Big a)
  where
    insert k = M.insertWith (<>) k . S.singleton

isSmall :: Node -> Bool
isSmall (Small _) = True
isSmall _ = False

toLine :: Node -> Node -> Maybe Line
toLine Start Start = Nothing
toLine End End = Nothing
toLine (Big _) (Big _) = Nothing
toLine (Big a) (Small b) = Just $ LineBigSmall a b
toLine Start (Big x) = Just $ LineStartBig x
toLine Start (Small x) = Just $ LineStartSmall x
toLine End (Big x) = Just $ LineEndBig x
toLine End (Small x) = Just $ LineEndSmall x
toLine a Start = toLine Start a
toLine a End = toLine End a
toLine a@(Small _) b@(Big _) = toLine b a
toLine (Small a) (Small b)
  | a == b = Nothing
  | a < b = Just $ LineSmall a b
  | otherwise = Just $ LineSmall b a

parseLine :: Text -> Maybe Line
parseLine =
  strip &. splitOn "-" &. \case
    [a, b] -> do
      x <- encodeAlpha (fromText a)
      y <- encodeAlpha (fromText b)
      toLine x y
    _ -> Nothing

newtype IgnoreSmalls = IgnoreSmalls Bool

doing :: HashMap Node (HashSet Node) -> IgnoreSmalls -> [Node] -> HashSet [Node]
doing _ _ [] = mempty
doing field small@(IgnoreSmalls sm) path@(l : _) =
  case M.lookup l field of
    Just set
      | S.null set -> mempty
      | otherwise -> withoutDups set <> withDups set
    Nothing -> mempty
  where
    currentSmalls :: HashSet Node
    currentSmalls = fromList (filter isSmall path)
    noUsedSmalls = (`S.difference` currentSmalls)
    usedSmalls = (`S.intersection` currentSmalls)

    nextSet :: IgnoreSmalls -> HashSet Node -> HashSet [Node]
    nextSet nextSmall =
      foldr (\x -> (<> doing field nextSmall (x : path))) mempty

    step nextSmall set
      | S.null set = mempty
      | S.member End set =
        S.singleton (End : path) <> nextSet nextSmall (S.delete End set)
      | otherwise = nextSet nextSmall set

    withoutDups = step small . noUsedSmalls
    withDups
      | sm = const mempty
      | otherwise = step (IgnoreSmalls True) . usedSmalls

parseInput :: [Text] -> [Line]
parseInput = mapMaybe parseLine

miniInput, slightlyLargerInput, evenLargerInput :: [Text]
miniInput =
  [ "start-A"
  , "start-b"
  , "A-c"
  , "A-b"
  , "b-d"
  , "A-end"
  , "b-end"
  ]
slightlyLargerInput =
  [ "dc-end"
  , "HN-start"
  , "start-kj"
  , "dc-start"
  , "dc-HN"
  , "LN-dc"
  , "HN-end"
  , "kj-sa"
  , "kj-HN"
  , "kj-dc"
  ]
evenLargerInput =
  [ "fs-end"
  , "he-DX"
  , "fs-he"
  , "start-DX"
  , "pj-DX"
  , "end-zg"
  , "zg-sl"
  , "zg-pj"
  , "pj-he"
  , "RW-he"
  , "fs-DX"
  , "pj-RW"
  , "zg-RW"
  , "start-pj"
  , "he-WI"
  , "zg-he"
  , "pj-fs"
  , "start-RW"
  ]

firstHalf :: [Line] -> Int
firstHalf = fromLines &. (\x -> doing x (IgnoreSmalls True) [Start]) &. S.size

secondHalf :: [Line] -> Int
secondHalf = fromLines &. (\x -> doing x (IgnoreSmalls False) [Start]) &. S.size

tryTest :: ([Text] -> [Text]) -> a -> a
tryTest = f . ($ [])
  where
    f [] = id
    f xs = throw . AssertionFailed . T.unpack . unlines . ("" :) $ xs

tests :: a -> a
tests =
  tryTest $
    id
      &. assertEqual "firstHalf mini" (firstHalf mini) 10
      &. assertEqual "firstHalf slightlyLarger" (firstHalf slightlyLarger) 19
      &. assertEqual "firstHalf evenLarger" (firstHalf evenLarger) 226
      &. assertEqual "secondHalf mini" (secondHalf mini) 36
      &. assertEqual "secondHalf slightlyLarger" (secondHalf slightlyLarger) 103
      &. assertEqual "secondHalf evenLarger" (secondHalf evenLarger) 3509
  where
    mini = parseInput miniInput
    slightlyLarger = parseInput slightlyLargerInput
    evenLarger = parseInput evenLargerInput

main :: IO ()
main = do
  input <- getLines <&> parseInput
  putStrLn $ "First half is: " <> showt (firstHalf input)
  putStrLn $ "Second half is: " <> showt (secondHalf input)

  return ()
