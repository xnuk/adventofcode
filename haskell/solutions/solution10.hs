import Data.List (sort)
import "text" Data.Text qualified as T

data Shape = Round | Square | Curly | Angle deriving (Eq)

data Pair a = Open a | Close a deriving (Eq)

data Result = Incomplete [Shape] | IllegalClosing Shape | Success

newtype Stack a = Stack [a]

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x : xs)) = Just (x, Stack xs)

nullStack :: Stack a -> Bool
nullStack (Stack []) = True
nullStack _ = False

fromChar :: Char -> Maybe (Pair Shape)
fromChar = \case
  '(' -> Just $ Open Round
  ')' -> Just $ Close Round
  '{' -> Just $ Open Curly
  '}' -> Just $ Close Curly
  '[' -> Just $ Open Square
  ']' -> Just $ Close Square
  '<' -> Just $ Open Angle
  '>' -> Just $ Close Angle
  _ -> Nothing

toOpened, toClosed :: Pair Shape -> Pair Shape
toOpened = \case
  Close a -> Open a
  a -> a
toClosed = \case
  Open a -> Close a
  a -> a

isClosed :: Pair Shape -> Bool
isClosed = \case
  Close _ -> True
  _ -> False

parseLine :: Text -> [Pair Shape]
parseLine = T.unpack &. mapMaybe fromChar

matchPairs :: [Pair Shape] -> Result
matchPairs = go (Stack [])
  where
    go :: Stack Shape -> [Pair Shape] -> Result
    go (Stack st) []
      | null st = Success
      | otherwise = Incomplete $ st
    go (Stack []) ((Close a) : _) = IllegalClosing a
    go (Stack []) ((Open x) : xs) = go (Stack [x]) xs
    go (Stack (st : sts)) ((Close x) : xs)
      | st == x = go (Stack sts) xs
      | otherwise = IllegalClosing x
    go (Stack sts) ((Open x) : xs) = go (Stack $ x : sts) xs

scoreIllegal :: Shape -> Int
scoreIllegal = \case
  Round -> 3
  Square -> 57
  Curly -> 1197
  Angle -> 25137

scoreIncomplete :: [Shape] -> Int
scoreIncomplete = foldl' (\a b -> a * 5 + scoreItem b) 0
  where
    scoreItem = \case
      Round -> 1
      Square -> 2
      Curly -> 3
      Angle -> 4

firstHalf :: [Result] -> Int
firstHalf =
  sum . map \case
    IllegalClosing c -> scoreIllegal c
    _ -> 0

secondHalf :: [Result] -> Int
secondHalf =
  mid . sort . mapMaybe \case
    Incomplete c -> Just $ scoreIncomplete c
    _ -> Nothing
  where
    mid xs = xs !! (length xs `div` 2)

main :: IO ()
main = do
  matched <- getLines <&> map (matchPairs . parseLine)
  putStrLn $ "First half is: " <> showt (firstHalf matched)
  putStrLn $ "Second half is: " <> showt (secondHalf matched)
