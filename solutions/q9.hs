import Data.List (nub, permutations)
import Control.Monad (zipWithM)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    cnt <- getContents
    let Just m = sequence . filter (/= Nothing) . map (\v -> case words v of
                    [a, "to", b, "=", c] -> Just (if a<b then (a, b) else (b, a), read c :: Integer)
                    _ -> Nothing) $ lines cnt
        mk = nub $ concatMap ((\a -> [fst a, snd a]) . fst) m
        dis xs = sum . fromJust . zipWithM (\a b -> if a<b then lookup (a,b) m else lookup (b,a) m) xs $ tail xs
        dists = map dis $ permutations mk
    putStrLn $ "q1: " ++ show (minimum dists)
    putStrLn $ "q2: " ++ show (maximum dists)
