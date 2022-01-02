import Data.Text qualified as T (unpack)

testInput :: [Text]
testInput =
  [ "2199943210"
  , "3987894921"
  , "9856789892"
  , "8767896789"
  , "9899965678"
  ]

toDigit :: Char -> Maybe Word8
toDigit c
  | 0 <= dec && dec <= 9 = Just dec
  | otherwise = Nothing
  where
    dec = ord c - ord @Word8 '0'

parseLines :: [Text] -> [[Word8]]
parseLines = map (mapMaybe toDigit . T.unpack)

parseWalls :: [[Word8]] -> [[Bool]]
parseWalls = map (map (== 9))

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

mayMatch :: a -> (a -> Bool) -> Maybe a
mayMatch x f
  | f x = Just x
  | otherwise = Nothing

lowPoints :: (Bounded a, Integral a) => [[a]] -> [[Maybe a]]
lowPoints = wrapMatrix maxBound &. mapNear zip3 &. map f
  where
    f = mapNear \(_, a, _) (b, x, c) (_, d, _) ->
      mayMatch x $ \z -> z == 0 || all (z <) [a, b, c, d]

fromMatrixMaybe :: [[Maybe a]] -> [a]
fromMatrixMaybe = catMaybes . concat

firstHalf :: Integral a => [[Word8]] -> a
firstHalf = lowPoints &. concat &. catMaybes &. map ((+ 1) . fromIntegral) &. sum

main :: IO ()
main = do
  inputs <- parseLines <$> pure testInput

  putStrLn $ "First half is: " <> showt @Int (firstHalf inputs)

-- putStrLn $ "Second half is: " <> showt (secondHalf answers)
