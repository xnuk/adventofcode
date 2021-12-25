data Cmd a = Forward a | Up a | Down a deriving (Show, Eq)

data Something a = Something {horizontal :: a, aim :: a, depth :: a}

initSomething :: Integral a => Something a
initSomething = Something{horizontal = 0, aim = 0, depth = 0}

parse :: Integral a => Text -> Maybe (Cmd a)
parse = do
  let trim :: Integral a => Text -> (a -> b) -> Text -> Maybe b
      trim prefix f = stripPrefix prefix >=> decimal >=> (pure . f)

  forward <- trim "forward " Forward
  up <- trim "up " Up
  down <- trim "down " Down

  pure $ forward <|> up <|> down

firstHalf :: [Cmd Int] -> Int
firstHalf =
  uncurry (*) . (sum *** sum) . partitionEithers . map \case
    Forward a -> Left a
    Up a -> Right $ negate a
    Down a -> Right a

secondHalf :: [Cmd Int] -> Int
secondHalf = unwrap . foldl' reducer initSomething
  where
    reducer state@Something{horizontal, aim, depth} = \case
      Forward a ->
        state{horizontal = horizontal + a, depth = depth + aim * a}
      Up a -> state{aim = aim - a}
      Down a -> state{aim = aim + a}
    unwrap Something{horizontal, depth} = horizontal * depth

main :: IO ()
main = do
  cmds <- getLines <&> mapMaybe (parse . strip)
  putStrLn $ "First half is: " <> showt (firstHalf cmds)
  putStrLn $ "Second half is: " <> showt (secondHalf cmds)
  pure ()
