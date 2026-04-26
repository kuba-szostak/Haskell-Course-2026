module Solution where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM, guard)
import Data.List (permutations)
import Control.Monad.Writer

-- Task 1: Maze navigation

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

-- (a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    dirs <- Map.lookup pos maze
    Map.lookup dir dirs

-- (b)
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze = foldM (move maze)

-- (c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = Just [pos]
safePath maze pos (d:ds) = do
    nextPos <- move maze pos d
    rest <- safePath maze nextPos ds
    return (pos : rest)

-- Task 2: Decoding a message

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- Task 3: Seating arrangements

type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    perm <- permutations guests
    guard (isValid perm)
    return perm
  where
    isValid [] = True
    isValid p = all (not . isConflict) (pairs p)
    
    pairs p = zip p (tail p ++ [head p])
    
    isConflict (g1, g2) = (g1, g2) `elem` conflicts || (g2, g1) `elem` conflicts

-- Task 4: Result monad with warnings

data Result a = Failure String | Success a [String] deriving (Show, Eq)

-- (a)
instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success a warns) = Success (f a) warns

instance Applicative Result where
    pure a = Success a []
    Failure msg <*> _ = Failure msg
    Success f warns1 <*> r = case r of
        Failure msg -> Failure msg
        Success a warns2 -> Success (f a) (warns1 ++ warns2)

instance Monad Result where
    return = pure
    Failure msg >>= _ = Failure msg
    Success a warns1 >>= f = case f a of
        Failure msg -> Failure msg
        Success b warns2 -> Success b (warns1 ++ warns2)

-- (b)
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

-- (c)
validateAge :: Int -> Result Int
validateAge age
    | age < 0   = failure "Negative age"
    | age > 150 = do
        warn "Age above 150"
        return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- Task 5: Evaluator with simplification log

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show, Eq)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Neg e) = do
    e' <- simplify e
    case e' of
        Neg inner -> do
            tell ["Double negation: -(-e) -> e"]
            return inner
        _ -> return (Neg e')
simplify (Add e1 e2) = do
    e1' <- simplify e1
    e2' <- simplify e2
    case (e1', e2') of
        (Lit 0, right) -> do
            tell ["Add identity: 0 + e -> e"]
            return right
        (left, Lit 0) -> do
            tell ["Add identity: e + 0 -> e"]
            return left
        (Lit a, Lit b) -> do
            tell ["Constant folding: a + b -> c"]
            return (Lit (a + b))
        _ -> return (Add e1' e2')
simplify (Mul e1 e2) = do
    e1' <- simplify e1
    e2' <- simplify e2
    case (e1', e2') of
        (Lit 1, right) -> do
            tell ["Mul identity: 1 * e -> e"]
            return right
        (left, Lit 1) -> do
            tell ["Mul identity: e * 1 -> e"]
            return left
        (Lit 0, _) -> do
            tell ["Zero absorption: 0 * e -> 0"]
            return (Lit 0)
        (_, Lit 0) -> do
            tell ["Zero absorption: e * 0 -> 0"]
            return (Lit 0)
        (Lit a, Lit b) -> do
            tell ["Constant folding: a * b -> c"]
            return (Lit (a * b))
        _ -> return (Mul e1' e2')
