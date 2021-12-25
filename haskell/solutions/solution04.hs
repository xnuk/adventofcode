import Data.List (nub, uncons)

newtype Line a = Line [a] deriving (Show)

newtype Bingo a = Bingo [Line a] deriving (Show)

newtype Rows a = Rows [Line a]

newtype Cols a = Cols [Line a]

tryMaybe :: (a -> Maybe a) -> a -> a
tryMaybe = (fromMaybe <*>)

withSplitAt :: (([a], [a]) -> ([c], [c])) -> Int -> [a] -> [c]
withSplitAt f i = splitAt i &. f &. uncurry (<>)

modifyAt :: (a -> a) -> Int -> [a] -> [a]
modifyAt = withSplitAt . second . tryMaybe . apply
  where
    apply g = uncons &. fmap (first g &. uncurry (:))

bingo :: Foldable f => f a -> Bingo a
bingo xs = Bingo result
  where
    size = floor @Double . sqrt . fromIntegral $ length xs
    initial = replicate size $ Line []

    step x (Rows rows, Cols cols, index) =
      (Rows $ addAt r rows, Cols $ addAt c cols, succ index)
      where
        (r, c) = index `divMod` size
        addAt = modifyAt (\(Line ls) -> Line (x : ls))

    result = row <> col
      where
        (Rows row, Cols col, _) =
          foldr step (Rows initial, Cols initial, 0) xs

reduceBingo :: Eq a => a -> Bingo a -> Either [a] (Bingo a)
reduceBingo x (Bingo xs) = result
  where
    remove (Line l)
      | null filtered = Nothing
      | otherwise = Just $ Line filtered
      where
        filtered = filter (/= x) l
    removed = map remove xs
    result = case sequence removed of
      Nothing -> catMaybes removed & concatMap (\(Line a) -> a) & nub & Left
      Just a -> Right (Bingo a)

firstHalf :: (Num a, Eq a) => [a] -> [Bingo a] -> a
firstHalf [] _ = undefined
firstHalf _ [] = undefined
firstHalf (input : inputs) bingos =
  case mapM (reduceBingo input) bingos of
    Right bs -> firstHalf inputs bs
    Left res -> input * sum res

secondHalf :: (Num a, Eq a) => [a] -> [Bingo a] -> a
secondHalf [] _ = undefined
secondHalf _ [] = undefined
secondHalf (input : inputs) bingos =
  case partitionEithers $ map (reduceBingo input) bingos of
    ([a], []) -> input * sum a
    (_, xs) -> secondHalf inputs xs

main :: IO ()
main = do
  inputs <- getLine <&> splitOn "," <&> mapMaybe decimal
  paragraphs <- getContents <&> (strip &. splitOn "\n\n")

  let bingos = paragraphs <&> strip <&> words <&> mapMaybe decimal <&> bingo

  putStrLn $ "First half is: " <> showt (firstHalf @Int inputs bingos)
  putStrLn $ "Second half is: " <> showt (secondHalf inputs bingos)
