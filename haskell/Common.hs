{-# LANGUAGE
    PackageImports
  , LambdaCase
  , BlockArguments
  , NoImplicitPrelude
  , TypeSynonymInstances
  , FlexibleInstances
#-}
{-# OPTIONS_GHC -Wall #-}

module Common
( module Data.Maybe
, module Data.Either
, module Data.Function
, module Data.Functor
, module Control.Arrow
, module Control.Applicative
, module Control.Monad
, module Data.Text
, module Data.Text.IO
, module Data.Foldable
, module Prelude
, module Data.List
, module Data.Bool
, decimal
, getLines
, putStr
, putStrLn
, showt
) where

import Prelude hiding
    ( getContents
    , getLine
    , lines
    , readFile
    , writeFile
    , appendFile
    , interact
    , putStrLn
    , putStr
    , words
    , unlines
    , unwords
    )

import qualified Prelude as P

import "text" Data.Text (Text, strip, lines, words, unlines, unwords, split, stripPrefix, stripSuffix)
import "text" Data.Text.IO hiding (putStr, putStrLn)
import qualified "text" Data.Text as T
import qualified "text" Data.Text.Read as T
import qualified "text" Data.Text.IO as T

import "base" Control.Arrow
import "base" Control.Applicative
import "base" Control.Monad
import "base" Data.Functor
import "base" Data.Function
import "base" Data.Maybe
import "base" Data.Either
import "base" Data.Foldable
import "base" Data.List (transpose, partition)
import "base" Data.Bool

decimal :: Integral a => Text -> Maybe a
decimal = T.decimal >>> \case
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
