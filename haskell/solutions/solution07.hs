import Data.List (sort)

duple :: a -> (a, a)
duple x = (x, x)

atMid :: [a] -> (a, a)
atMid xs = case length xs `divMod` 2 of
  (m, 1) -> duple $ xs !! m
  (0, _) -> duple $ head xs
  (m, _) -> (xs !! pred m, xs !! m)

atAvg :: Integral a => [a] -> (a, a)
atAvg xs = case sum xs `divMod` fromIntegral (length xs) of
  (m, 0) -> duple m
  (m, _) -> (m, succ m)

sumDistance, sumIncrementalDistance :: Integral a => a -> [a] -> a
sumDistance a = sum . map (abs . subtract a)
sumIncrementalDistance a = sum . map (total . abs . subtract a)
  where
    total x = x * (x + 1) `div` 2

halve :: Integral a => ([a] -> (a, a)) -> (a -> [a] -> a) -> [a] -> a
halve avg score xs
  | uncurry (==) mid = x
  | otherwise = min x y
  where
    sorted = sort xs
    mid = avg sorted
    x = score (fst mid) xs
    y = score (snd mid) xs

firstHalf, secondHalf :: Integral a => [a] -> a
firstHalf = halve atMid sumDistance
secondHalf = halve atAvg sumIncrementalDistance -- 왜인진 모르겠지만 암튼 맞음

main :: IO ()
main = do
  inputs <- getLine <&> (splitOn "," &. mapMaybe (decimal @Int))
  putStrLn $ "First half is: " <> showt (firstHalf inputs)
  putStrLn $ "Second half is: " <> showt (secondHalf inputs)
