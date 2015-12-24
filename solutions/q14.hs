import Control.Monad (liftM)
import Data.List (transpose, maximumBy)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as Map (empty, insertWith, foldl)
import Data.Function (on)

data Performance = Performance String (Integer, Integer) Integer

main :: IO ()
main = do
    a <- liftM (fromJust . sequence . filter isJust . map (parser . words) . lines) getContents
    putStrLn $ "q1: " ++ show (maxDistance 2503 a)
    putStrLn $ "q2: " ++ show (fuckTheSanta 2503 a)

parser :: [String] -> Maybe Performance
parser [name, "can", "fly", velocity, "km/s", "for", runtime, "seconds,", "but", "then", "must", "rest", "for", rest, "seconds."] =
    Just $ Performance name (read velocity, read runtime) (read rest)
parser _ = Nothing

maxDistance :: Integer -> [Performance] -> Integer
maxDistance timeout ds = maximum . (`map` ds) $
    \(Performance _ (velocity, runtime) rest) -> let (d, m) = timeout `divMod` (runtime + rest)
                                                  in velocity*(runtime*d + if m>=runtime then runtime else m)

fuckTheSanta :: Integer -> [Performance] -> Integer
fuckTheSanta timeout ds = Map.foldl max 0 . foldl (\a b -> Map.insertWith (+) b 1 a) Map.empty . take (fromIntegral timeout) . map (fst . maximumBy (compare `on` snd)) . transpose . (`map` ds) $
    \(Performance name (velocity, runtime) rest) -> let a = [velocity, velocity*2 .. velocity*runtime] ++ replicate (fromIntegral rest) (velocity*runtime)
                                                    in map (\v -> (name, v)) . concat $ iterate ((`map` a) . (+) . last) a
