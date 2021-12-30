module Common.Chain ((>>), (>>=), return, get) where

import Data.Function (const, flip, (.))

infixl 1 >>=, >>

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) = flip (.)

(>>=) :: (a -> (b, c)) -> (b -> c -> d) -> a -> d
query >>= f = \x -> let (z, a) = query x in f z a

return :: a -> b -> a
return = const

get :: (a -> b) -> a -> (b, a)
get f x = (f x, x)
