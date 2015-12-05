main :: IO ()
main = do
    a <- getContents
    let ls = lines a
    putStrLn . ("5-1: " ++) . show . length . filter id $ map isNice ls
    putStrLn . ("5-2: " ++) . show . length . filter id $ map newNice ls

isNice :: String -> Bool
isNice [] = False
isNice [_] = False
isNice [_,_] = False
isNice (prev:sstr) = niceFunc prev sstr (Just (if prev `elem` "aeiou" then 1 else 0) :: Maybe Int) False True
    where niceFunc _ [] Nothing True True = True
          niceFunc _ _ _ _ False = False
          niceFunc _ [] _ _ _ = False
          niceFunc p (c:str) vowel row cont =
            let a = case fmap (if c `elem` "aeiou" then (+1) else id) vowel of
                        Just 3 -> Nothing
                        x -> x
            in niceFunc c str a (row || p==c) (cont && ([p,c] `notElem` ["ab", "cd", "pq", "xy"]))

newNice :: String -> Bool
newNice str = newFunc False False arr
    where arr = let f "" _ = []
                    f [_] a = a
                    f (a:b:st) ys = f (b:st) ([a,b]:ys)
                in f str []
          newFunc True True _ = True
          newFunc _ _ [] = False
          newFunc _ _ [_] = False
          newFunc pair sandwich (a:b:st) = newFunc (pair || (a `elem` st)) (sandwich || (reverse a == b)) (b:st)
