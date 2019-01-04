import Data.Maybe
import Data.Word
import Data.Char
import System.IO
import System.Environment
import System.Exit

data Command = Increment
             | Decrement
             | MoveRight
             | MoveLeft
             | StartLoop
             | EndLoop
             | WriteOut
             | ReadIn
  deriving Show

assocCommand :: Char -> Maybe Command
assocCommand n = case n of
  '+' -> Just Increment
  '-' -> Just Decrement
  '>' -> Just MoveRight
  '<' -> Just MoveLeft
  '[' -> Just StartLoop
  ']' -> Just EndLoop
  '.' -> Just WriteOut
  ',' -> Just ReadIn
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

interpretCommand :: Command -> BFState -> IO BFState
interpretCommand command (BFState cmds cursor memory counter loops) =
  case command of
    Increment -> return $ BFState cmds cursor
                 (changeNth memory
                   (memory !! fromIntegral cursor + 1) cursor)
                 (counter + 1) loops
                 
    Decrement -> return $ BFState cmds cursor
                 (changeNth memory
                   (memory !! fromIntegral cursor - 1) cursor)
                 (counter + 1) loops
                 
    MoveRight -> return $ BFState cmds (cursor + 1) memory (counter + 1) loops
    
    MoveLeft  -> return $ BFState cmds (cursor - 1) memory (counter + 1) loops
    
    StartLoop -> return $
                 if (memory !! (fromIntegral cursor)) == 0
                 then BFState cmds cursor memory
                      (firstEnd (counter + 1) cmds) loops
                 else BFState cmds cursor memory (counter + 1) (counter : loops)
                 
    EndLoop   -> return $ BFState cmds cursor memory (head loops) (tail loops)
    
    WriteOut  -> (putStr $ [chr $ fromIntegral $ memory !! (fromIntegral cursor)]) >>
                 (return $ BFState cmds cursor memory (counter + 1) loops)

    ReadIn    -> do eof <- isEOF
                    if eof
                      then (return $ BFState cmds cursor memory (counter + 1) loops)
                      else (do c <- getChar
                               (return $ BFState cmds cursor
                                (changeNth memory (fromIntegral $ ord c) cursor) (counter + 1) loops))

bfFold :: [Command] -> IO BFState
bfFold commands = bfLoop (BFState commands 0 (replicate 30000 0) 0 [])
  where bfLoop state = if (counter state) >= (toInteger $ length $ cmds state)
                       then (return state)
                       else (interpretCommand
                             ((cmds state) !! (fromIntegral $ counter state))
                             state) >>= bfLoop

parseArgs :: [String] -> IO String
parseArgs [name] = return name
parseArgs _ = do
  putStrLn("Provide one brainfuck file to interpret")
  exitWith $ ExitFailure 1

main = do
  filename <- getArgs >>= parseArgs
  withFile filename ReadMode $ \handle -> do
    source <- hGetContents handle
    bfFold $ parseSource source
