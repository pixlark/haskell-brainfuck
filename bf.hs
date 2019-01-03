import Data.Maybe
import Data.Word

data Command = NoOperation
             | Increment
             | Decrement
             | MoveRight
             | MoveLeft
             | StartLoop
             | EndLoop
  deriving Show

assocCommand :: Char -> Maybe Command
assocCommand n = case n of
  '+' -> Just Increment
  '-' -> Just Decrement
  '>' -> Just MoveRight
  '<' -> Just MoveLeft
  '[' -> Just StartLoop
  ']' -> Just EndLoop
  _   -> Nothing

parseSource :: [Char] -> [Command]
parseSource source = [fromJust $ assocCommand x |
                      x <- source, isJust $ assocCommand x]

changeNth :: [a] -> a -> Integer -> [a]
changeNth list changeTo index = [if (fst x) == index
                                 then changeTo
                                 else (snd x) |
                                  x <- zip [0..(toInteger $ length list) - 1] list]

data BFState = BFState { cmds    :: [Command]
                       , cursor  :: Integer
                       , memory  :: [Word8]
                       , counter :: Integer
                       , loops   :: [Integer]
                       }
  deriving Show

firstEnd counter cmds = firstEndHelper counter cmds 0
  where firstEndHelper counter cmds depth = case (cmds !! (fromIntegral counter)) of
          StartLoop -> firstEndHelper (counter + 1) cmds (depth + 1)
          EndLoop   -> if depth == 0
                       then (counter + 1)
                       else firstEndHelper (counter + 1) cmds (depth - 1)
          _         -> firstEndHelper (counter + 1) cmds depth

interpretCommand :: Command -> BFState -> BFState
interpretCommand command (BFState cmds cursor memory counter loops) =
  case command of
    Increment -> BFState
                 cmds
                 cursor
                 (changeNth
                   memory
                   (memory !! fromIntegral cursor + 1)
                   cursor)
                 (counter + 1)
                 loops
    Decrement -> BFState
                 cmds
                 cursor
                 (changeNth
                   memory
                   (memory !! fromIntegral cursor - 1)
                   cursor)
                 (counter + 1)
                 loops
    MoveRight -> BFState
                 cmds
                 (cursor + 1)
                 memory
                 (counter + 1)
                 loops
    MoveLeft  -> BFState
                 cmds
                 (cursor - 1)
                 memory
                 (counter + 1)
                 loops
    StartLoop -> if (memory !! (fromIntegral cursor)) == 0
                 then BFState
                      cmds
                      cursor
                      memory
                      (firstEnd (counter + 1) cmds)
                      loops
                 else BFState
                      cmds
                      cursor
                      memory
                      (counter + 1)
                      (counter : loops)
    EndLoop   -> BFState
                 cmds
                 cursor
                 memory
                 (head loops)
                 (tail loops)

bfFold :: [Command] -> BFState
bfFold commands = bfLoop (BFState commands 0 (replicate 10 0) 0 [])
  where bfLoop state = if (counter state) >= (toInteger $ length $ cmds state)
                       then state
                       else bfLoop (interpretCommand ((cmds state) !! (fromIntegral $ counter state)) state)

main = do
  source <- getContents
  putStr $ show $ bfFold $ parseSource source
