module Common
  ( module Data.Maybe,
    module Data.Either,
    module Data.Function,
    module Data.Functor,
    module Data.Char,
    module Data.Word,
    module Control.Arrow,
    module Control.Applicative,
    module Control.Monad,
    module Data.Text,
    module Data.Text.IO,
    module Data.Foldable,
    module Prelude,
    module Data.List,
    module Data.Bool,
    module GHC.Exts,
    module Safe,
    decimal,
    getLines,
    putStr,
    putStrLn,
    showt,
    (&.),
  )
where

import "base" Control.Applicative
import "base" Control.Arrow
import "base" Control.Monad
import "base" Data.Bool
import "base" Data.Char (chr, ord)
import "base" Data.Either
import "base" Data.Foldable hiding (toList)
import "base" Data.Function
import "base" Data.Functor
import "base" Data.List (partition, transpose, group, groupBy, sort, sortBy, sortOn)
import "base" Data.Maybe
import "text" Data.Text (Text, lines, split, splitOn, strip, stripPrefix, stripSuffix, unlines, unwords, words)
import "text" Data.Text qualified as T
import "text" Data.Text.IO hiding (putStr, putStrLn)
import "text" Data.Text.IO qualified as T
import "text" Data.Text.Read qualified as T
import "base" Data.Word
import "base" GHC.Exts (IsList (..))
import "safe" Safe
import Prelude hiding
  ( appendFile,
    getContents,
    getLine,
    interact,
    lines,
    putStr,
    putStrLn,
    readFile,
    unlines,
    unwords,
    words,
    writeFile,
  )
import Prelude qualified as P

decimal :: Integral a => Text -> Maybe a
decimal =
  T.decimal >>> \case
    Right (a, _) -> Just a
    Left _ -> Nothing

getLines :: IO [Text]
getLines = getContents <&> (lines . strip)

class PrintableStr a where
  putStr :: a -> IO ()
  putStrLn :: a -> IO ()

instance PrintableStr Text where
  putStr = T.putStr
  putStrLn = T.putStrLn

instance PrintableStr String where
  putStr = P.putStr
  putStrLn = P.putStrLn

showt :: Show a => a -> Text
showt = T.pack . show

infixl 9 &.

(&.) :: (a -> b) -> (b -> c) -> a -> c
(&.) = (>>>)
