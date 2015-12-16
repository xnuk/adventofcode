import Prelude hiding (lookup)
import Control.Monad (liftM, liftM2, join)
import Data.Word (Word16)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Map (Map, fromList, lookup)
import Text.Read (readMaybe)

main :: IO ()
main = do
    a <- getContents
    let dat = filter ((/= []) . words) $ lines a
    case whatA dat of
      Just x  -> do{--}
          putStrLn $ "q1: " ++ show x
          case whatA . map (unwords . map (\v -> if v=="b" then show x else v)) . filter ((/= "b") . last) . map words $ dat of
            Just z  -> putStrLn $ "q2: " ++ show z
            Nothing -> putStrLn "q2: -"
      Nothing -> putStrLn "q1: -"

whatA :: [String] ->  Maybe Word16
whatA [] = Nothing
whatA xs = join $ lookup "a" mym
    where mym :: Map String (Maybe Word16)
          mym = fromList . (`map` xs) $
                        (\v -> case words v of
                                  [   "NOT",    a, "->", b] -> (b, liftM complement (lk a))
                                  [             a, "->", b] -> (b, lk a)
                                  [a, "OR",     b, "->", c] -> (c, liftM2 (.|.) (lk a) (lk b))
                                  [a, "AND",    b, "->", c] -> (c, liftM2 (.&.) (lk a) (lk b))
                                  [a, "LSHIFT", b, "->", c] -> (c, liftM2 shiftL (lk a) (readMaybe b :: Maybe Int))
                                  [a, "RSHIFT", b, "->", c] -> (c, liftM2 shiftR (lk a) (readMaybe b :: Maybe Int))
                                  _ -> ("", Nothing)
                                  )
          lk a = case (readMaybe a :: Maybe Word16) of
                     Just x -> Just x
                     Nothing -> join $ lookup a mym
