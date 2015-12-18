import Numeric (readHex)
import Data.Char (chr)

main :: IO ()
main = do
    a <- getContents
    let ls = filter ('"' `elem`) $ lines a
        gs = map length ls
        ds = map (length . decode) ls
        es = map (length . encode) ls
    putStrLn . ("q1: " ++) . show . sum $ zipWith (-) gs ds
    putStrLn . ("q2: " ++) . show . sum $ zipWith (-) es gs

-- length "\x10ab" == 1, so we need this.
decode :: String -> String
decode xs = case dropWhile (/='"') xs of
              ('"':str) ->
                  let func ('\\':'x':x:y:zs) = case readHex (x:y:"") of
                                                (n, s):_ -> chr n:s ++ func zs
                                                []       -> func zs
                      func ('\\':x:zs) = x:func zs
                      func ('"':_) = ""
                      func (x:zs) = x:func zs
                      func "" = ""
                  in func str
              _ -> ""

-- print "\x10ab" => "\4267", so we need this.
encode :: String -> String
encode xs = ('"':) . (++"\"") $ xs >>=
    \x -> case x of
              '"'  -> "\\\""
              '\\' -> "\\\\"
              _    -> x:""

