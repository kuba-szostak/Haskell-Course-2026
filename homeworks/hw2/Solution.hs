module Solution where

import Data.Foldable (toList)

-- Data definitions
data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
  deriving (Show, Eq)

-- 1. Functor for Sequence
instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- 2. Foldable for Sequence
instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

-- 3. Semigroup and Monoid for Sequence
instance Semigroup (Sequence a) where
    (<>) :: Sequence a -> Sequence a -> Sequence a
    Empty <> x = x
    x <> Empty = x
    l <> r = Append l r

instance Monoid (Sequence a) where
    mempty :: Sequence a
    mempty = Empty

-- 4. Tail Recursion and Sequence Search
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target seq = go [seq]
  where
    go [] = False
    go (Empty : rest) = go rest
    go (Single x : rest)
      | x == target = True
      | otherwise   = go rest
    go (Append l r : rest) = go (l : r : rest)

-- 5. Tail Recursion and Sequence Flatten
tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
  where
    go [] acc = acc
    go (Empty : rest) acc = go rest acc
    go (Single x : rest) acc = go rest (x : acc)
    go (Append l r : rest) acc = go (r : l : rest) acc

-- 6. Tail Recursion and Reverse Polish Notation
data Token = TNum Int | TAdd | TSub | TMul | TDiv
  deriving (Show, Eq)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [res] = Just res
    go [] _ = Nothing
    go (TNum n : ts) stack = go ts (n : stack)
    go (TAdd : ts) (y : x : stack) = go ts ((x + y) : stack)
    go (TSub : ts) (y : x : stack) = go ts ((x - y) : stack)
    go (TMul : ts) (y : x : stack) = go ts ((x * y) : stack)
    go (TDiv : ts) (y : x : stack)
      | y == 0    = Nothing
      | otherwise = go ts ((x `div` y) : stack)
    go _ _ = Nothing

-- 7. Expressing functions via foldr and foldl

-- (a) myReverse
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

{- Explanation for (a):
`foldl` is the natural choice because it processes the list from left to right,
accumulating elements by prepending them to the accumulator. This naturally
reverses the list in O(n) time. A naïve `foldr` version, such as
`foldr (\x acc -> acc ++ [x]) []`, takes O(n^2) time due to the repeated
use of the `++` operator which traverses the accumulated list each time.
-}

-- (b) myTakeWhile
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

{- Explanation for (b):
`foldr` can stop early thanks to Haskell's lazy evaluation. If the predicate
`p x` returns False, the function returns `[]` without evaluating `acc` (which
represents the rest of the fold). This means the remainder of the list is never
traversed, allowing it to work on infinite lists. `foldl`, however, always
traverses the entire list before reducing, so it would hang on an infinite list.
-}

-- (c) decimal
decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0

-- 8. Run-length encoding via folds

-- (a) encode
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x, 1)]
    step x ((y, n) : acc)
      | x == y    = (x, n + 1) : acc
      | otherwise = (x, 1) : (y, n) : acc

-- (b) decode
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []
