{-# LANGUAGE
    PackageImports
  , LambdaCase
  , BlockArguments
  , NoImplicitPrelude
  , OverloadedStrings
  , NamedFieldPuns
  , TupleSections
#-}
{-# OPTIONS_GHC -Wall #-}

import Common
import qualified "text" Data.Text as T
import "base" Data.Bits

binary :: Bits a => [Bool] -> a
binary = reverse
    >>> zipWith lifter [0..]
    >>> catMaybes
    >>> foldr (flip setBit) zeroBits
    where
        lifter index flag
            | flag = Just index
            | otherwise = Nothing

firstHalf :: [[Bool]] -> Int
firstHalf = result . map popularBit . transpose where
    popularBit = fromMaybe True . popular
    result = do
        gamma <- binary
        epsilon <- binary . map not
        pure $ gamma * epsilon

popular :: [Bool] -> Maybe Bool
popular = partition id >>> (length *** length) >>> \case
    (truthy, falsy)
        | truthy > falsy -> Just True
        | falsy > truthy -> Just False
        | otherwise -> Nothing

oxygen, carbon :: [Bool] -> Bool
oxygen = fromMaybe True . popular
carbon = not . oxygen

at :: Int -> [a] -> Maybe a
at i xs
    | length xs > i && 0 <= i = Just $ xs !! i
    | otherwise = Nothing

criteria :: ([Bool] -> Bool) -> Int -> [[Bool]] -> [Bool]
criteria _ _ [] = []
criteria _ _ [xs] = xs
criteria rating index xs = criteria rating (succ index) select where
    poped = fromMaybe [] $ mapM (\x -> at index x <&> (, x)) xs
    rate = rating $ map fst poped
    select = map snd $ filter ((== rate) . fst) poped

secondHalf :: [[Bool]] -> Int
secondHalf xs = oxyRating * carbRating where
    most = oxygen $ map head xs
    (oxy, carb) = partition (\x -> head x == most) xs
    oxyRating = binary $ criteria oxygen 1 oxy
    carbRating = binary $ criteria carbon 1 carb

main :: IO ()
main = do
    inputs <- getLines <&> map (strip >>> T.unpack >>> map (== '1'))
    putStrLn $ "First half is: " <> showt (firstHalf inputs)
    putStrLn $ "Second half is: " <> showt (secondHalf inputs)
    pure ()
