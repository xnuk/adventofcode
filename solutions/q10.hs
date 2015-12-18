main :: IO ()
main = do
    a <- getLine
    let d = iterate las a
    putStrLn $ "q1: " ++ (show . length) (d !! 40)
    putStrLn $ "q2: " ++ (show . length) (d !! 50)

las :: String -> String
las "" = ""
las xs = let (a, b) = span (== head xs) xs
         in show (length a) ++ head xs:las b
