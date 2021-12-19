{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Common
  ( module Data.Maybe,
    module Data.Either,
    module Data.Function,
    module Data.Functor,
    module Control.Arrow,
    module Control.Applicative,
    module Control.Monad,
    module Data.Text,
    module Data.Text.IO,
    module Data.Foldable,
    module Prelude,
    module Data.List,
    module Data.Bool,
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
import "base" Data.Either
import "base" Data.Foldable
import "base" Data.Function
import "base" Data.Functor
import "base" Data.List (partition, transpose)
import "base" Data.Maybe
import "text" Data.Text (Text, lines, split, splitOn, strip, stripPrefix, stripSuffix, unlines, unwords, words)
import qualified "text" Data.Text as T
import "text" Data.Text.IO hiding (putStr, putStrLn)
import qualified "text" Data.Text.IO as T
import qualified "text" Data.Text.Read as T
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
import qualified Prelude as P

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
