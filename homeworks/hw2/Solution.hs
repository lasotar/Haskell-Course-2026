module Solution where

import Data.Foldable (toList)
import Data.Monoid ((<>))

-- Part 1: Sequence data type
data Sequence a = Empty
                | Single a
                | Append (Sequence a) (Sequence a)
                deriving (Show, Eq)

-- Part 2: Functor instance
instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append left right) = Append (fmap f left) (fmap f right)

-- Part 3: Foldable instance
instance Foldable Sequence where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append left right) = foldMap f left <> foldMap f right

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

-- Part 4: Semigroup and Monoid instances
instance Semigroup (Sequence a) where
    (<>) = Append

instance Monoid (Sequence a) where
    mempty = Empty

-- Part 5: Tail-recursive search
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target seq = go seq []
  where
    go Empty stack = case stack of
        []          -> False
        (next:rest) -> go next rest
    
    go (Single x) stack
        | x == target = True
        | otherwise   = case stack of
            []          -> False
            (next:rest) -> go next rest
    
    go (Append left right) stack = go left (right : stack)

-- Part 6: Tail-recursive conversion to list
tailToList :: Sequence a -> [a]
tailToList seq = reverse (go seq [] [])
  where
    go Empty stack acc = case stack of
        []          -> acc
        (next:rest) -> go next rest acc
    
    go (Single x) stack acc = case stack of
        []          -> x : acc
        (next:rest) -> go next rest (x : acc)
    
    go (Append left right) stack acc = go left (right : stack) acc

-- Part 7: RPN Calculator
data Token = TNum Int | TAdd | TSub | TMul | TDiv
           deriving (Show, Eq)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _ = Nothing
    
    go (TNum n : rest) stack = go rest (n : stack)
    
    go (TAdd : rest) stack = case stack of
        (y : x : remaining) -> go rest ((x + y) : remaining)
        _ -> Nothing
    
    go (TSub : rest) stack = case stack of
        (y : x : remaining) -> go rest ((x - y) : remaining)
        _ -> Nothing
    
    go (TMul : rest) stack = case stack of
        (y : x : remaining) -> go rest ((x * y) : remaining)
        _ -> Nothing
    
    go (TDiv : rest) stack = case stack of
        (y : x : remaining) ->
            if y == 0
            then Nothing
            else go rest ((x `div` y) : remaining)
        _ -> Nothing

-- Part 8: Functions using folds
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred = foldr step []
  where
    step x acc
        | pred x    = x : acc
        | otherwise = []

decimal :: [Int] -> Int
decimal = foldl (\acc digit -> acc * 10 + digit) 0

-- Part 9: Run-length encoding
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x []  = [(x, 1)]
    step x ((y, count) : rest)
        | x == y    = (y, count + 1) : rest
        | otherwise = (x, 1) : (y, count) : rest

decode :: [(a, Int)] -> [a]
decode = foldr step []
  where
    step (x, count) acc = replicate count x ++ acc
