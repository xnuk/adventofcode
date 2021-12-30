module Common.Pot ((>>), (>>=), return, get) where

import Common.Chain qualified as C

infixl 1 >>=, >>

(>>) :: (a -> a) -> (a -> a) -> (a -> a)
(>>) = (C.>>)

(>>=) :: (a -> (b, a)) -> (b -> a -> a) -> a -> a
(>>=) = (C.>>=)

return :: a -> b -> a
return = C.return

get :: (a -> b) -> a -> (b, a)
get = C.get
