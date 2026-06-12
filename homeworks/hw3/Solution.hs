-- | Homework 3 solutions with very detailed explanations.
--   This file is intentionally verbose to help new Haskell learners.
module Solution where

-- ============================================================================
-- PART 0: SMALL HASKELL SYNTAX PRIMER
-- ============================================================================
--
-- 1) Function application:
--    f x y means "apply f to x, then apply the result to y".
--    There are no parentheses unless you add them yourself.
--
-- 2) do-notation:
--    do
--      x <- action
--      y <- action2
--      pure (x + y)
--    This is syntax sugar for (>>=) and (>>).
--    It sequences computations inside a Monad.
--
-- 3) Lists as a Monad:
--    In a list "do" block, each binding generates possibilities.
--    guard filters out invalid possibilities.
--
-- 4) Maybe:
--    Maybe is a Monad that represents failure (Nothing) or success (Just a).

-- ============================================================================
-- IMPORTS (WHAT THEY ARE AND WHY WE NEED THEM)
-- ============================================================================

-- We use Map from the containers library to represent dictionaries.
-- "Map k v" means: a collection mapping keys of type k to values of type v.
import Data.Map (Map)

-- We import the same module qualified so we can write M.lookup, M.insert, etc.
-- This avoids name clashes and makes it clear which functions come from Data.Map.
import qualified Data.Map as M

-- foldM lets us fold a list while staying inside a Monad (like Maybe).
-- guard is used in list comprehensions or list-monad code to filter results.
import Control.Monad (foldM, guard)

-- permutations generates all possible orderings of a list.
import Data.List (permutations)

-- Writer lets us build a result *and* keep a log at the same time.
-- We will use it to record simplification steps in EXERCISE 5.
import Control.Monad.Trans.Writer

-- ============================================================================
-- EXERCISE 1: MAZE NAVIGATION WITH Maybe
-- ============================================================================

-- WHAT IS Maybe?
-- Maybe is a type that represents "success or failure".
--   - Just value  means success and contains a value.
--   - Nothing     means failure.
--
-- It is useful for operations that can fail, like looking up a key in a map.

-- A position is a pair of integers (x coordinate, y coordinate).
-- Example: (0, 0) is the origin, (3, 2) is x=3, y=2.
type Pos = (Int, Int)

-- A direction can be North, South, East, or West.
-- deriving (Eq, Ord, Show) auto-generates:
--   Eq  -> we can compare directions
--   Ord -> we can order them (needed for Map keys)
--   Show -> we can print them
data Dir = N | S | E | W deriving (Eq, Ord, Show)

-- A maze maps each position to a map of outgoing directions.
-- So: Maze :: Map Pos (Map Dir Pos)
-- Meaning: at a position, you can look up a direction, and get the next position.
type Maze = Map Pos (Map Dir Pos)

-- (a)
-- move :: Maze -> Pos -> Dir -> Maybe Pos
-- Read as: given a maze, a current position, and a direction,
--          return Just nextPos if that move exists,
--          or Nothing if the move is impossible.
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  -- M.lookup checks a key in a Map.
  -- If it is missing, it returns Nothing.
  dirs <- M.lookup pos maze

  -- Now we have the inner Map from directions to positions.
  -- We try to look up the chosen direction.
  M.lookup dir dirs

-- (b)
-- followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
-- Read as: given a maze, a starting position, and a list of directions,
--          try to follow the whole path and return the final position.
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze start =
  -- foldM is like foldl, but it keeps the computation inside a Monad.
  -- Here the Monad is Maybe, so if any move fails, the whole result is Nothing.
  foldM (move maze) start

-- (c)
-- safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
-- Read as: like followPath, but return *all* visited positions, not just the end.
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start [] =
  -- Base case: no more directions.
  -- We return a list containing only the current position.
  Just [start]
safePath maze start (d:ds) = do
  -- Move one step in direction d.
  next <- move maze start d
  -- Recursively follow the rest of the directions.
  rest <- safePath maze next ds
  -- Prepend the current position to the path we got back.
  return (start : rest)

-- EXAMPLE:
-- If the maze allows N then E from (0,0):
-- safePath maze (0,0) [N,E] ==> Just [(0,0),(0,1),(1,1)]

-- ============================================================================
-- EXERCISE 2: DECRYPTING WITH A SUBSTITUTION KEY
-- ============================================================================

-- A key maps an encrypted character to a decrypted character.
-- For example: key could map 'x' -> 'a', 'y' -> 'b', etc.
type Key = Map Char Char

-- decrypt :: Key -> String -> Maybe String
-- Read as: given a key and an encrypted string,
--          return the decrypted string, or Nothing if any char is missing.
decrypt :: Key -> String -> Maybe String
decrypt key =
  -- traverse applies a function to every element and collects the results.
  -- With Maybe, if any lookup fails, the whole result is Nothing.
  traverse (`M.lookup` key)

-- decryptWords :: Key -> [String] -> Maybe [String]
-- Read as: decrypt a list of words using the same key.
decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key =
  -- traverse works for lists too:
  -- it applies decrypt to each word and stops on the first failure.
  traverse (decrypt key)

-- EXAMPLE:
-- If key maps 'a'->'x' and 'b'->'y',
-- decrypt key "ab" ==> Just "xy"

-- ============================================================================
-- EXERCISE 3: SEATING ARRANGEMENTS WITH CONFLICTS
-- ============================================================================

-- A guest is identified by a String (their name).
type Guest = String

-- A conflict means two guests cannot sit next to each other.
-- We store it as an unordered pair (a, b).
type Conflict = (Guest, Guest)

-- seatings :: [Guest] -> [Conflict] -> [[Guest]]
-- Read as: given a list of guests and a list of conflicts,
--          return all circular seatings that avoid conflicts.
seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  -- In the list monad, "do" means "generate possibilities".
  -- p will take each permutation of the guests.
  p <- permutations guests

  -- Create adjacent pairs, wrapping at the end (circular table).
  -- Example: [a,b,c] -> [(a,b),(b,c),(c,a)]
  let pairs = zip p (tail p ++ [head p])

  -- guard filters out invalid permutations.
  -- If the condition is False, this permutation disappears.
  guard $
    not $
      any
        (\(a, b) -> (a, b) `elem` conflicts || (b, a) `elem` conflicts)
        pairs

  -- If we reach this line, the seating is valid.
  return p

-- ============================================================================
-- EXERCISE 4: RESULT TYPE WITH WARNINGS
-- ============================================================================

-- Result is like Either, but with warnings on success.
-- Failure carries an error message.
-- Success carries a value *and* a list of warnings.
data Result a = Failure String | Success a [String] deriving (Show, Eq)

-- (a) Functor instance
-- Functor lets us apply a function to the success value.
instance Functor Result where
  -- If we already failed, we keep the same failure.
  fmap _ (Failure e) = Failure e
  -- If we succeeded, apply the function to the value, keep warnings.
  fmap f (Success a ws) = Success (f a) ws

-- Applicative instance
-- Applicative lets us combine independent Result computations.
instance Applicative Result where
  -- pure puts a value into Success with no warnings.
  pure a = Success a []
  -- If the function side failed, the whole application fails.
  Failure e <*> _ = Failure e
  -- If the argument side failed, the whole application fails.
  Success _ _ <*> Failure e = Failure e
  -- If both succeed, apply the function and concatenate warnings.
  Success f w1 <*> Success a w2 = Success (f a) (w1 ++ w2)

-- Monad instance
-- Monad lets us chain computations where later steps depend on earlier ones.
instance Monad Result where
  -- return is the same as pure.
  return = pure
  -- If we failed, we stay failed.
  Failure e >>= _ = Failure e
  -- If we succeeded, run the next step.
  -- If it fails, return the failure.
  -- If it succeeds, combine warning lists.
  Success a w1 >>= f = case f a of
    Failure e -> Failure e
    Success b w2 -> Success b (w1 ++ w2)

-- (b)
-- warn produces a success with a warning message.
warn :: String -> Result ()
warn msg = Success () [msg]

-- failure produces a failure with an error message.
failure :: String -> Result a
failure = Failure

-- (c)
-- validateAge checks a single age value.
-- Negative ages are errors.
-- Very large ages trigger a warning but still succeed.
validateAge :: Int -> Result Int
validateAge age
  | age < 0   = failure "Negative age"
  | age > 150 = warn "Age above 150" >> return age
  | otherwise = return age

-- validateAges checks a list of ages, collecting all warnings.
validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- ============================================================================
-- EXERCISE 5: EXPRESSION SIMPLIFICATION WITH Writer
-- ============================================================================

-- WHAT IS Writer?
-- Writer is a monad that pairs a value with a log.
-- It is useful when you want to compute a result AND keep a trace of steps.

-- Expr is a tiny arithmetic language:
-- Lit is a literal integer.
-- Add and Mul are binary operations.
-- Neg is unary negation.
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show, Eq)

-- simplify :: Expr -> Writer [String] Expr
-- Read as: simplify an expression, and log the rules we used.
simplify :: Expr -> Writer [String] Expr
simplify (Lit n) =
  -- Literals are already in simplest form.
  return (Lit n)
simplify (Add e1 e2) = do
  -- Simplify both sides first.
  s1 <- simplify e1
  s2 <- simplify e2
  -- Then apply algebraic rules.
  case (s1, s2) of
    (Lit 0, e) ->
      -- 0 + e = e
      tell ["Add identity: 0 + e -> e"] >> return e
    (e, Lit 0) ->
      -- e + 0 = e
      tell ["Add identity: e + 0 -> e"] >> return e
    (Lit a, Lit b) ->
      -- Constant folding: compute a + b at compile time.
      tell ["Constant folding: a + b -> a+b"] >> return (Lit (a + b))
    _ ->
      -- No rule applies; rebuild the Add node.
      return (Add s1 s2)
simplify (Mul e1 e2) = do
  -- Simplify both sides first.
  s1 <- simplify e1
  s2 <- simplify e2
  -- Then apply algebraic rules.
  case (s1, s2) of
    (Lit 1, e) ->
      -- 1 * e = e
      tell ["Mul identity: 1 * e -> e"] >> return e
    (e, Lit 1) ->
      -- e * 1 = e
      tell ["Mul identity: e * 1 -> e"] >> return e
    (Lit 0, _) ->
      -- 0 * e = 0
      tell ["Zero absorption: 0 * e -> 0"] >> return (Lit 0)
    (_, Lit 0) ->
      -- e * 0 = 0
      tell ["Zero absorption: e * 0 -> 0"] >> return (Lit 0)
    (Lit a, Lit b) ->
      -- Constant folding: compute a * b at compile time.
      tell ["Constant folding: a * b -> a*b"] >> return (Lit (a * b))
    _ ->
      -- No rule applies; rebuild the Mul node.
      return (Mul s1 s2)
simplify (Neg e) = do
  -- Simplify the inner expression first.
  s <- simplify e
  case s of
    Neg e' ->
      -- Double negation: -(-e) = e
      tell ["Double negation: -(-e) -> e"] >> return e'
    _ ->
      -- Otherwise keep the negation.
      return (Neg s)
