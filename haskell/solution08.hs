{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

import Common
import "transformers" Control.Monad.Trans.Class (MonadTrans (lift))
import "transformers" Control.Monad.Trans.State.Lazy (StateT, evalStateT, state)
import qualified "base" Data.Bits as B (Bits (popCount, setBit, testBit, xor, (.&.), (.|.)))
import "base" Data.List (elemIndex)
import qualified "text" Data.Text as T
import "base" Data.Word (Word8)

{-

  0:6     1:2     2:5     3:5     4:4
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:5     6:6     7:3     8:7     9:6
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

-}

type BlockWord = Word8

newtype Block = Block {unBlock :: BlockWord} deriving (Eq)

instance Show Block where
  show (Block a) = map (bool '_' 'T' . B.testBit a) [0 .. 7]

popFilter :: Monad m => (a -> Bool) -> StateT [a] m [a]
popFilter = state . partition

countSegs :: Block -> Int
countSegs = B.popCount . unBlock

popByCount :: Monad m => Int -> StateT [Block] m [Block]
popByCount x = popFilter $ (== x) . countSegs

infixl 7 .&.

infixl 5 .|.

infixl 6 `xor`

(.&.), (.|.), xor :: Block -> Block -> Block
(Block a) .&. (Block b) = Block $ (B..&.) a b
(Block a) .|. (Block b) = Block $ (B..|.) a b
xor (Block a) (Block b) = Block $ B.xor a b

complement :: Block -> Block
complement (Block a) = Block $ B.xor a 0x7f

type State = StateT [Block] Maybe

popCDE :: State Block
popCDE = do
  sixNineZero <- popByCount 6
  let cde = map complement sixNineZero
  lift $ case cde of
    [x, y, z] -> Just $ x .|. y .|. z
    _ -> Nothing

solve :: State [Block]
solve = do
  [cf] <- popByCount 2
  [bcdf] <- popByCount 4
  [acf] <- popByCount 3
  [abcdefg] <- popByCount 7

  cde <- popCDE

  let bd = bcdf `xor` cf
      c = cde .&. cf
      de = c `xor` cde
      d = bd .&. de
      b = bd `xor` d
      e = de `xor` d
      f = c `xor` cf

  pure
    [ complement d
    , cf
    , complement (b .|. f)
    , complement (b .|. e)
    , bcdf
    , complement (c .|. e)
    , complement c
    , acf
    , abcdefg
    , complement e
    ]

encode :: [Char] -> Block
encode = map f &. foldr ($) 0 &. Block
  where
    f = (`elemIndex` "abcdefg") &. maybe id (flip B.setBit)

parseLine :: Text -> Maybe ([Block], [Block])
parseLine =
  splitOn "|"
    &. map (words &. map (encode . T.unpack))
    &. \case
      [a, b] -> Just (a, b)
      _ -> Nothing

solveLine :: ([Block], [Block]) -> Maybe [Int]
solveLine (st, query) = do
  solved <- evalStateT solve st
  mapM (`elemIndex` solved) query

firstHalf :: [[Int]] -> Int
firstHalf = concat &. filter (`elem` [1, 4, 7, 8]) &. length

toDecimal :: Integral a => [a] -> a
toDecimal = flip foldl' 0 \b a -> b * 10 + a

secondHalf :: [[Int]] -> Int
secondHalf = map toDecimal &. sum

main :: IO ()
main = do
  inputs <- getLines <&> mapMaybe parseLine
  answers <- case mapM solveLine inputs of
    Just a -> pure a
    Nothing -> fail "cannot find answers in some cases"

  putStrLn $ "First half is: " <> showt (firstHalf answers)
  putStrLn $ "Second half is: " <> showt (secondHalf answers)
