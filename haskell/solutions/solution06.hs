data Sexy a = Sexy [a]

sexy :: Num a => Sexy a -> Sexy a
sexy (Sexy []) = Sexy []
sexy (Sexy (z : zs)) = Sexy trimmed
  where
    newGen = [0, 0, 0, 0, 0, 0, z, 0, z] <> repeat 0
    paddedSexy = zs <> repeat 0
    result = zipWith (+) paddedSexy newGen
    trimmed = take 20 result

toSexy :: Num a => [Int] -> Sexy a
toSexy = Sexy . take 20 . foldr (zipWith (+) . toOccurence) (repeat 0)
  where
    toOccurence x = replicate x 0 <> [1] <> repeat 0

countSexy :: Num a => Sexy a -> a
countSexy (Sexy []) = 0
countSexy (Sexy xs) = sum xs

genCountsAt :: Num a => Int -> [a]
genCountsAt c =
  iterate sexy (toSexy [6])
    & drop c
    & take 7
    & map countSexy
    & reverse

firstHalf :: [Int] -> Int
firstHalf = sum . map (counts !!)
  where
    counts = genCountsAt 80

secondHalf :: [Int] -> Integer
secondHalf = sum . map (counts !!)
  where
    counts = genCountsAt 256

main :: IO ()
main = do
  inputs <- mapMaybe decimal . splitOn "," <$> getLine
  putStrLn $ "First half is: " <> showt (firstHalf inputs)
  putStrLn $ "Second half is: " <> showt (secondHalf inputs)
