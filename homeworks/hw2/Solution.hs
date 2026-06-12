-- | Homework 2 solutions with very detailed explanations.
--   This file mirrors the style of the reference Solution.hs:
--   lots of visuals, lots of comments, and step-by-step intent.
module Solution where

-- ============================================================================
-- PART 0: SMALL HASKELL SYNTAX PRIMER
-- ============================================================================
--
-- 1) Type variables:
--    In "Sequence a", the "a" is a placeholder.
--    It means Sequence can store values of any type.
--
-- 2) Constructors:
--    data X = A | B Int
--    "A" and "B" are constructors used to build values of type X.
--
-- 3) Pattern matching:
--    f A = ...
--    f (B n) = ...
--    This means we handle each possible shape of the input.
--
-- 4) Type class instances:
--    instance Functor Sequence where ...
--    This says "Sequence behaves like a Functor, and here is how".

-- Import necessary modules for Foldable functionality
import Data.Foldable (toList)  -- Brings in the toList function
import Data.Monoid ((<>))       -- Brings in the <> operator for combining monoids

-- ============================================================================
-- PART 1: THE SEQUENCE DATA TYPE
-- ============================================================================

-- This defines a new data type called "Sequence".
-- A Sequence stores elements in a tree-like way:
--   - Empty: no elements
--   - Single: exactly one element
--   - Append: two sequences glued together
--
-- The "a" is a type variable.
-- It means the same structure can store Int, String, Bool, or any other type.
data Sequence a =
                  Empty                          -- An empty sequence (no elements)
                | Single a                       -- A sequence with one element
                | Append (Sequence a) (Sequence a) -- Two sequences joined together
                deriving (Show, Eq)              -- Auto-generate show and equality

-- Examples of sequences:
-- Empty                          represents: []
-- Single 5                       represents: [5]
-- Append (Single 1) (Single 2)   represents: [1, 2]
-- Append (Single 'a') (Append (Single 'b') (Single 'c'))  represents: ['a','b','c']

-- ============================================================================
-- PART 2: FUNCTOR INSTANCE
-- ============================================================================

-- WHAT IS A FUNCTOR?
-- A Functor is a type class for things you can "map over".
-- It provides the function:
--   fmap :: (a -> b) -> f a -> f b
--
-- Read that as:
--   If you can turn an 'a' into a 'b',
--   and you have a container of 'a's,
--   then you can get a container of 'b's.
--
-- The key idea: the container structure stays the same,
-- only the values inside change.

instance Functor Sequence where
    -- fmap applies a function to every element in the sequence.
    -- The tree structure (Empty, Single, Append) stays the same.

    -- Case 1: Empty sequence
    -- No elements to transform, so it stays Empty.
    fmap _ Empty = Empty

    -- Case 2: Single element
    -- Apply f to the single value and wrap it back in Single.
    fmap f (Single x) = Single (f x)

    -- Case 3: Append
    -- Recursively map over both sub-sequences, then re-append.
    fmap f (Append left right) = Append (fmap f left) (fmap f right)

-- EXAMPLES AND TESTS
-- Let seq1 = Append (Single 1) (Single 2)
-- fmap (+10) seq1  ==> Append (Single 11) (Single 12)
--
-- Let seq2 = Single "hi"
-- fmap length seq2 ==> Single 2

-- ============================================================================
-- PART 3: FOLDABLE INSTANCE
-- ============================================================================

-- WHAT IS A MONOID?
-- A Monoid is a type class for things that can be combined.
-- It provides:
--   mempty  :: m           -- identity element
--   (<>)    :: m -> m -> m -- combine operation
--
-- Examples:
--   Sum:    mempty = 0,   (<>) = (+)
--   Lists:  mempty = [],  (<>) = (++)
--   Strings: mempty = "", (<>) = (++)

-- WHAT IS FOLDABLE?
-- Foldable is a type class for data structures that can be "folded"
-- into a single value.
-- The key function is:
--   foldMap :: Monoid m => (a -> m) -> f a -> m
--
-- You turn each element into a monoid value,
-- and then combine them all using <>.

instance Foldable Sequence where
    -- Case 1: Empty sequence => identity element
    foldMap _ Empty = mempty

    -- Case 2: Single element => apply function to it
    foldMap f (Single x) = f x

    -- Case 3: Append => fold both sides and combine
    foldMap f (Append left right) = foldMap f left <> foldMap f right

-- ============================================================================
-- HOW foldMap WORKS: DETAILED TRACE
-- ============================================================================
--
-- Suppose we want to convert a Sequence to a list:
--
-- seqToList (Append (Single 1) (Append (Single 2) (Single 3)))
--
-- toList uses foldMap internally with f = (\x -> [x]).
-- That means:
--   foldMap f (Append (Single 1) (Append (Single 2) (Single 3)))
--
-- Step 1: split on Append:
--   foldMap f (Single 1) <> foldMap f (Append (Single 2) (Single 3))
--
-- Step 2: foldMap f (Single 1) = [1]
--
-- Step 3: foldMap f (Append (Single 2) (Single 3))
--   = foldMap f (Single 2) <> foldMap f (Single 3)
--   = [2] <> [3]
--   = [2,3]
--
-- Step 4: combine the two halves:
--   [1] <> [2,3] = [1,2,3]

-- ============================================================================
-- PART 4: USING FOLDABLE FUNCTIONS
-- ============================================================================

-- Convert a Sequence to a list using the Foldable instance.
-- toList is provided by Data.Foldable.
seqToList :: Sequence a -> [a]
seqToList = toList

-- seqLength gets the number of elements in a Sequence.
-- It works because seqToList turns it into a normal list.
seqLength :: Sequence a -> Int
seqLength = length

-- More Foldable examples you can now use:
-- sum     (Append (Single 1) (Single 2)) = 3
-- product (Append (Single 2) (Single 3)) = 6
-- null Empty = True
-- elem 2 (Append (Single 1) (Single 2)) = True

-- ============================================================================
-- PART 5: SEMIGROUP AND MONOID INSTANCES
-- ============================================================================

-- Semigroup defines how to combine two values of the same type.
-- For Sequence, combining means appending.
instance Semigroup (Sequence a) where
    (<>) = Append

-- Monoid adds an identity element to Semigroup.
-- For Sequence, the identity element is Empty.
instance Monoid (Sequence a) where
    mempty = Empty

-- ============================================================================
-- PART 6: TAIL-RECURSIVE SEARCH
-- ============================================================================

-- tailElem checks if a target exists in a Sequence.
-- It is tail-recursive using an explicit stack.
-- This avoids deep recursion on the right side of Append.
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target seq = go seq []
  where
    -- go processes the current sequence and a stack of remaining right branches.
    go Empty stack = case stack of
        -- If nothing is left, the target was not found.
        []          -> False
        -- Otherwise, continue with the next pending subtree.
        (next:rest) -> go next rest

    -- If we hit a Single value, compare it with the target.
    go (Single x) stack
        | x == target = True
        | otherwise   = case stack of
            []          -> False
            (next:rest) -> go next rest

    -- For Append, traverse left now, remember right for later.
    go (Append left right) stack = go left (right : stack)

-- ============================================================================
-- PART 7: TAIL-RECURSIVE CONVERSION TO LIST
-- ============================================================================

-- Convert a Sequence to a list in a tail-recursive way.
-- We build the list in reverse and then reverse it once at the end.
tailToList :: Sequence a -> [a]
tailToList seq = reverse (go seq [] [])
  where
    -- stack keeps pending right branches, acc is the reversed list.
    go Empty stack acc = case stack of
        []          -> acc
        (next:rest) -> go next rest acc

    -- For Single, add the element to the accumulator.
    go (Single x) stack acc = case stack of
        []          -> x : acc
        (next:rest) -> go next rest (x : acc)

    -- For Append, traverse left now and push right to the stack.
    go (Append left right) stack acc = go left (right : stack) acc

-- ============================================================================
-- PART 8: RPN (REVERSE POLISH NOTATION) CALCULATOR
-- ============================================================================

-- Tokens for a simple stack-based calculator.
data Token = TNum Int | TAdd | TSub | TMul | TDiv
           deriving (Show, Eq)

-- Evaluate an RPN expression.
-- Returns Nothing if the expression is invalid or divides by zero.
tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    -- If no tokens remain and the stack has exactly one result, success.
    go [] [result] = Just result
    -- Otherwise, if stack size is wrong, fail.
    go [] _ = Nothing

    -- If we see a number, push it onto the stack.
    go (TNum n : rest) stack = go rest (n : stack)

    -- For addition, pop two numbers, add them, push result.
    go (TAdd : rest) stack = case stack of
        (y : x : remaining) -> go rest ((x + y) : remaining)
        _ -> Nothing

    -- For subtraction, pop two numbers, subtract, push result.
    go (TSub : rest) stack = case stack of
        (y : x : remaining) -> go rest ((x - y) : remaining)
        _ -> Nothing

    -- For multiplication, pop two numbers, multiply, push result.
    go (TMul : rest) stack = case stack of
        (y : x : remaining) -> go rest ((x * y) : remaining)
        _ -> Nothing

    -- For division, pop two numbers and divide.
    -- If divisor is zero, fail.
    go (TDiv : rest) stack = case stack of
        (y : x : remaining) ->
            if y == 0
            then Nothing
            else go rest ((x `div` y) : remaining)
        _ -> Nothing

-- EXAMPLE:
-- [TNum 2, TNum 3, TAdd] represents "2 3 +"
-- tailRPN [TNum 2, TNum 3, TAdd] ==> Just 5

-- ============================================================================
-- PART 9: FUNCTIONS USING FOLDS
-- ============================================================================

-- Reverse a list using a left fold.
-- foldl builds the result from left to right.
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

-- Take elements while a predicate holds, using foldr.
-- foldr allows early termination by returning [].
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred = foldr step []
  where
    step x acc
        | pred x    = x : acc
        | otherwise = []

-- Convert a list of decimal digits into an integer.
-- Example: [1,2,3] -> 123
decimal :: [Int] -> Int
decimal = foldl (\acc digit -> acc * 10 + digit) 0

-- ============================================================================
-- PART 10: RUN-LENGTH ENCODING
-- ============================================================================

-- Encode consecutive duplicates as (value, count).
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x []  = [(x, 1)]
    step x ((y, count) : rest)
        | x == y    = (y, count + 1) : rest
        | otherwise = (x, 1) : (y, count) : rest

-- Decode a run-length encoded list back to its original form.
decode :: [(a, Int)] -> [a]
decode = foldr step []
  where
    step (x, count) acc = replicate count x ++ acc
