import Control.Arrow (first, second)
import qualified Data.Set as Set

data Direction = U|R|D|L deriving (Show, Eq)

go :: Integral a => Direction -> (a, a) -> (a, a)
go U = second (subtract 1)
go R = first (+1)
go D = second (+1)
go L = first (subtract 1)

main :: IO ()
main = do
    a <- getContents
    let ds = parse a
    putStrLn . ("3-1: " ++) . show $ sexySanta ds
    putStrLn . ("3-2: " ++) . show $ roboSanta ds

parse :: String -> [Direction]
parse str =
    let Just a = sequence . filter (/= Nothing) . flip map str $
            (\v -> case v of
                    '^' -> Just U
                    '>' -> Just R
                    'v' -> Just D
                    '<' -> Just L
                    _   -> Nothing)
    in a

sexySanta :: [Direction] -> Int
sexySanta directions = loop directions (0,0) (Set.singleton (0,0))
    where loop [] _ set = Set.size set
          loop (d:ds) point set = loop ds p $ Set.insert p set
              where p = go d point :: (Integer, Integer)

roboSanta :: [Direction] -> Int
roboSanta directions = loop directions (0,0) (0,0) (Set.singleton (0,0))
    where loop []  _ _ set = Set.size set
          loop [_] _ _ set = Set.size set
          loop (dSanta:dRobo:ds) pSanta pRobo set = loop ds pSanta' pRobo' . Set.insert pRobo' . Set.insert pSanta' $ set
              where pSanta' = go dSanta pSanta :: (Integer, Integer)
                    pRobo'  = go dRobo pRobo :: (Integer, Integer)
