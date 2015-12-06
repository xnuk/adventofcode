import Data.Array ((!), (//), array, accum, elems)

data Cmd a b = Toogle a b | On a b | Off a b deriving (Eq, Show)
type Point = (Int, Int)
type CmdPoint = Cmd Point Point

main :: IO ()
main = do
    a <- getContents
    let Just d = sequence . filter (/= Nothing) . map parse $ lines a
    putStrLn $ "4-1: " ++ show (litLights d)
    putStrLn $ "4-2: " ++ show (brightLights d)

parse :: String -> Maybe CmdPoint
parse str
    | length ws < 4 = Nothing
    | take 2 ws == ["turn", "on" ] = Just . uncurry On     . rd $ drop 2 ws
    | take 2 ws == ["turn", "off"] = Just . uncurry Off    . rd $ drop 2 ws
    | head ws   ==  "toggle"       = Just . uncurry Toogle . rd $ tail ws
    | otherwise = Nothing
    where ws = words str
          rd (a:_:b:_) = (read ('(':a++")"), read ('(':b++")"))
          rd _ = ((-1, -1), (-1, -1))


litLights :: [CmdPoint] -> Int
litLights sarr = length . filter id . elems $ func sarr initarr
    where initarr = array ((0,0), (999,999)) [((a,b), False) | a<-[0..999], b<-[0..999]]
          func [] arr = arr
          func (z:zs) arr = func zs . (arr //) $ case z of
                              On     (x1, y1) (x2, y2) -> [( (x,y), True )            | x<-[x1..x2], y<-[y1..y2]]
                              Off    (x1, y1) (x2, y2) -> [( (x,y), False)            | x<-[x1..x2], y<-[y1..y2]]
                              Toogle (x1, y1) (x2, y2) -> [( (x,y), not $ arr!(x,y) ) | x<-[x1..x2], y<-[y1..y2]]

brightLights :: [CmdPoint] -> Integer
brightLights sarr = sum  . elems $ func sarr initarr
    where initarr = array ((0,0), (999,999)) [((a,b), 0) | a<-[0..999], b<-[0..999]]
          func [] arr = arr
          func (z:zs) arr = func zs . accum (\a b -> b a) arr $ case z of
                              On     (x1, y1) (x2, y2) -> [( (x,y), (+1)) | x<-[x1..x2], y<-[y1..y2]]
                              Off    (x1, y1) (x2, y2) -> [( (x,y), \v -> if v==0 then 0 else v-1) | x<-[x1..x2], y<-[y1..y2]]
                              Toogle (x1, y1) (x2, y2) -> [( (x,y), (+2)) | x<-[x1..x2], y<-[y1..y2]]
