{-# LANGUAGE
    PackageImports
  , LambdaCase
  , BlockArguments
  , NoImplicitPrelude
  , OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Common

firstHalf :: [Int] -> Int
firstHalf xs =
    zipWith (<) xs (drop 1 xs)
    & filter id
    & length

secondHalf :: [Int] -> Int
secondHalf xs =
    zipWith3 sum3 xs (drop 1 xs) (drop 2 xs)
    & firstHalf
    where sum3 a b c = a + b + c

main :: IO ()
main = do
    nums <- getLines <&> mapMaybe decimal
    putStrLn $ "First half is: " <> showt (firstHalf nums)
    putStrLn $ "Second half is: " <> showt (secondHalf nums)
    return ()
