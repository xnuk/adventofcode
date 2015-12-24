import Control.Monad (liftM)
import Data.Maybe (isJust, fromJust)
import Data.List (nub, permutations)

data Happiness = Happiness { happyName :: String
                           , happyValue :: Integer
                           , _happyTarget :: String
                           }

main :: IO ()
main = do
    a <- liftM (fromJust . sequence . filter isJust . map (parser . words) . lines) getContents
    putStrLn $ "q1: " ++ show (maxHappy a)
    putStrLn $ "q2: " ++ show (maxHappy $ a ++ concatMap ((\x -> [Happiness "You" 0 x, Happiness x 0 "You"]) . happyName) a)

parser :: [String] -> Maybe Happiness
parser [name, "would", losegain, num, "happiness", "units", "by", "sitting", "next", "to", target] =
    Just $ Happiness name (if losegain=="gain" then read num else negate $ read num) (init target)
parser _ = Nothing

maxHappy :: [Happiness] -> Integer
maxHappy ds = maximum . map calc . permutations $ tail people
    where people = nub $ map happyName ds
          h = head people
          calc xs = sum . (\z -> zipWith z (h:xs) (xs ++ [h])) $ \a b -> let c = if a>b then (b,a) else (a,b)
                                                                         in sum . map happyValue $ filter (\(Happiness m _ n) -> (m,n)==c || (n,m)==c) ds
