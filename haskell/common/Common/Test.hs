module Common.Test (Tester, assertEqual, assertTest, assertTestIO) where

import Common (IO, Show, showt)
import Control.Exception (AssertionFailed (..), throw, throwIO)
import Control.Monad (return)
import Data.Bool (otherwise)
import Data.Either
import Data.Eq (Eq ((==)))
import Data.Function (flip, id, ($), (.))
import Data.Monoid (mconcat, (<>))
import Data.Text (Text, unlines, unpack)

type Tester = [Text] -> [Text]

assertEqual :: (Eq a, Show a) => Text -> a -> a -> Tester
assertEqual message a b
  | a == b = id
  | otherwise =
    flip (<>) . (: []) . mconcat $
      ["Assertion ", message, ": ", showt a, " =/= ", showt b]

assertTestPre :: Tester -> Either AssertionFailed ()
assertTestPre = f . ($ [])
  where
    f [] = Right ()
    f xs = Left . AssertionFailed . unpack . unlines . ("" :) $ xs

assertTest :: Tester -> a -> a
assertTest t x = case assertTestPre t of
  Right () -> x
  Left e -> throw e

assertTestIO :: Tester -> IO ()
assertTestIO t = case assertTestPre t of
  Right () -> return ()
  Left e -> throwIO e
