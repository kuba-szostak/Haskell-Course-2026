module Solution where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)

-- 1. Stack machine

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving (Show, Eq)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n:)
execInstr POP = modify (\s -> case s of (_:xs) -> xs; [] -> [])
execInstr DUP = modify (\s -> case s of (x:xs) -> x:x:xs; [] -> [])
execInstr SWAP = modify (\s -> case s of (x:y:xs) -> y:x:xs; _ -> s)
execInstr ADD = modify (\s -> case s of (x:y:xs) -> (x+y):xs; _ -> s)
execInstr MUL = modify (\s -> case s of (x:y:xs) -> (x*y):xs; _ -> s)
execInstr NEG = modify (\s -> case s of (x:xs) -> (-x):xs; [] -> [])

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg instrs = execState (execProg instrs) []


-- 2. Expression evaluator with variable bindings

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr
  | Seq Expr Expr
  deriving (Show, Eq)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = return n
eval (Var v) = do
  env <- get
  case Map.lookup v env of
    Just val -> return val
    Nothing -> error $ "Unassigned variable: " ++ v
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)
eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 * v2)
eval (Neg e) = do
  v <- eval e
  return (-v)
eval (Assign name e) = do
  val <- eval e
  modify (Map.insert name val)
  return val
eval (Seq e1 e2) = do
  _ <- eval e1
  eval e2

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty


-- 3. Memoised edit (Levenshtein) distance

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get
  case Map.lookup (i, j) cache of
    Just d -> return d
    Nothing -> do
      d <- compute
      modify (Map.insert (i, j) d)
      return d
  where
    compute
      | i == 0 = return j
      | j == 0 = return i
      | otherwise = do
          let cX = xs !! (i - 1)
              cY = ys !! (j - 1)
          if cX == cY
            then editDistM xs ys (i - 1) (j - 1)
            else do
              del <- editDistM xs ys (i - 1) j
              ins <- editDistM xs ys i (j - 1)
              sub <- editDistM xs ys (i - 1) (j - 1)
              return $ 1 + minimum [del, ins, sub]

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty


-- 4, 5, 6. StateT and "Treasure Hunters" Game Simulation

data LocationType = Normal | Obstacle | Treasure | Trap | DecisionPoint [String] | Goal
  deriving (Show, Eq)

data GameState = GameState
  { position :: Int
  , energy :: Int
  , score :: Int
  , board :: Map Int LocationType
  } deriving (Show)

type AdventureGame a = StateT GameState IO a

movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
  st <- get
  let newPos = position st + roll
  put st { position = newPos, energy = energy st - 1 }
  return roll

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
  liftIO $ putStrLn "You have reached a decision point!"
  choice <- liftIO $ getPlayerChoice options
  return choice

handleLocation :: AdventureGame Bool
handleLocation = do
  st <- get
  let pos = position st
  let loc = Map.findWithDefault Normal pos (board st)
  case loc of
    Goal -> do
      liftIO $ putStrLn "Congratulations! You reached the treasure!"
      return True
    Obstacle -> do
      liftIO $ putStrLn "You hit an obstacle! You lose 1 energy."
      modify (\s -> s { energy = energy s - 1 })
      return False
    Treasure -> do
      liftIO $ putStrLn "You found an intermediate treasure! Gain 10 points."
      modify (\s -> s { score = score s + 10, board = Map.insert pos Normal (board s) })
      return False
    Trap -> do
      liftIO $ putStrLn "It's a trap! You lose 5 points."
      modify (\s -> s { score = max 0 (score s - 5), board = Map.insert pos Normal (board s) })
      return False
    DecisionPoint opts -> do
      _ <- makeDecision opts
      -- A real game might branch paths here, but we just continue
      return False
    Normal -> do
      liftIO $ putStrLn "The path is clear."
      return False

playTurn :: AdventureGame Bool
playTurn = do
  st <- get
  if energy st <= 0
    then do
      liftIO $ putStrLn "You ran out of energy! Game Over."
      return True
    else do
      liftIO $ displayGameState st
      roll <- liftIO getDiceRoll
      _ <- movePlayer roll
      handleLocation

playGame :: AdventureGame ()
playGame = do
  done <- playTurn
  if done
    then do
      st <- get
      liftIO $ putStrLn $ "Final Score: " ++ show (score st)
    else playGame

getDiceRoll :: IO Int
getDiceRoll = do
  putStrLn "Enter dice roll (1-6):"
  input <- getLine
  case reads input of
    [(n, "")] | n >= 1 && n <= 6 -> return n
    _ -> do
      putStrLn "Invalid input. Please enter a number between 1 and 6."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState st = do
  putStrLn "\n--- Current State ---"
  putStrLn $ "Position: " ++ show (position st)
  putStrLn $ "Energy:   " ++ show (energy st)
  putStrLn $ "Score:    " ++ show (score st)
  putStrLn "---------------------"

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
  putStrLn $ "Options: " ++ unwords options
  putStrLn "Enter your choice:"
  input <- getLine
  if input `elem` options
    then return input
    else do
      putStrLn "Invalid choice. Please try again."
      getPlayerChoice options