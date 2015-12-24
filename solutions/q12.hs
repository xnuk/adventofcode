import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))
import Data.List (isInfixOf)
import Control.Monad (liftM2)
import Data.Maybe (isNothing)

main :: IO ()
main = do
    a <- getContents
    putStrLn $ "q1: " ++ show (intsum a)
    putStrLn $ "q2: " ++ show (let (Just b, _) = f a in b)

intsum :: String -> Integer
intsum = sum . map read . getAllTextMatches . (=~ "-?[0-9]+")

f :: String -> (Maybe Integer, String)
f ('[':strs) = let (a,b) = span (`notElem` "[{]") strs
               in case head b of
                               ']' -> (Just $ intsum a, tail b)
                               x|x`elem` "[{" ->
                                           let (p, g) = f b
                                               (c, d) = f $ '[':g
                                           in (foldl (liftM2 (+)) (return 0) [Just $ intsum a, if isNothing p then Just 0 else p, c], d)
                                | otherwise -> undefined

f ('{':strs) = let (a,b) = span (`notElem` "[{}") strs
               in case head b of
                               '}' -> (if ":\"red\"" `isInfixOf` a then Nothing else Just $ intsum a, tail b)
                               x|x `elem` "[{" ->
                                           let (p, g) = f b
                                               (c, d) = f $ '{':g
                                           in (if ":\"red\"" `isInfixOf` a
                                                then Nothing
                                                else foldl (liftM2 (+)) (return 0) [Just $ intsum a, if isNothing p then Just 0 else p, c], d)
                                | otherwise -> undefined

f _ = undefined
