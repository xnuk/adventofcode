import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack, append)

main :: IO ()
main = do
    a <- getLine
    let b = zeros 1 5 a
        c = zeros b 6 a
    putStrLn $ "4-1: " ++ show b
    putStrLn $ "4-2: " ++ show c

zeros :: Integer -> Int -> String -> Integer
zeros a n str = head $ filter ((== replicate n '0') . take n . show . md5 . (pack str `append`) . pack . show) [a..]
