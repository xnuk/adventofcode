import "containers" Data.IntSet (IntSet)
import "text" Data.Text qualified as T

newtype Field = Field [[Word8]]

instance Show Field where
  show (Field a) = "Field " <> show a

data Sex a = Sex a

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

main :: IO ()
main = do
  input <- parseInput <$> pure testInput
  print input
