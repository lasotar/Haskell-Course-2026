module Solution where

-- Import necessary modules for Foldable functionality
import Data.Foldable (toList)  -- Brings in the toList function
import Data.Monoid ((<>))       -- Brings in the <> operator for combining monoids

-- ============================================================================
-- PART 1: The Sequence Data Type
-- ============================================================================

-- This defines a new data type called "Sequence" that holds a sequence of elements
-- The 'a' is a type variable - it means this works for any type (Int, String, etc.)
data Sequence a = Empty                          -- An empty sequence (no elements)
                | Single a                        -- A sequence with one element
                | Append (Sequence a) (Sequence a) -- Two sequences joined together
                deriving (Show, Eq)               -- Auto-generate show and equality functions

-- Examples of sequences:
-- Empty                          represents: []
-- Single 5                       represents: [5]
-- Append (Single 1) (Single 2)   represents: [1, 2]
-- Append (Single 'a') (Append (Single 'b') (Single 'c'))  represents: ['a', 'b', 'c']


-- ============================================================================
-- PART 2: Functor Instance
-- ============================================================================

-- WHAT IS A FUNCTOR?
-- A Functor is a type class (like an interface in other languages) that represents
-- things you can "map over" - applying a function to every element inside.
-- 
-- Think of it like this:
-- - You have a box containing values
-- - You want to apply a function to each value inside the box
-- - The box structure stays the same, only the values change
--
-- The main function in Functor is 'fmap':
-- fmap :: (a -> b) -> f a -> f b
-- Read as: "Given a function from 'a' to 'b', and a functor containing 'a's,
--          give me back a functor containing 'b's"

instance Functor Sequence where
    -- fmap applies a function to every element in the sequence
    -- The structure (Empty, Single, Append) stays the same!
    
    -- Case 1: Empty sequence
    -- If there are no elements, there's nothing to apply the function to
    -- So we just return Empty
    fmap _ Empty = Empty
    
    -- Case 2: Single element
    -- Apply the function 'f' to the single element 'x'
    -- f :: a -> b  (function from type 'a' to type 'b')
    -- x :: a       (value of type 'a')
    -- f x :: b     (result of applying f to x, type 'b')
    fmap f (Single x) = Single (f x)
    
    -- Case 3: Appended sequences
    -- We have two subsequences: left and right
    -- We need to apply the function to BOTH subsequences
    -- Then join them back together with Append
    fmap f (Append left right) = Append (fmap f left) (fmap f right)
    -- Here we recursively call fmap on both parts


-- ============================================================================
-- EXAMPLES AND TESTS
-- ============================================================================

-- Example 1: Adding 10 to each number
-- Let seq1 = Append (Single 1) (Single 2)  -- represents [1, 2]
-- fmap (+10) seq1 = Append (Single 11) (Single 12)  -- represents [11, 12]

-- Example 2: Converting numbers to strings
-- Let seq2 = Single 42
-- fmap show seq2 = Single "42"

-- Example 3: Working with nested structures
-- Let seq3 = Append (Single 1) (Append (Single 2) (Single 3))  -- represents [1, 2, 3]
-- fmap (*2) seq3 = Append (Single 2) (Append (Single 4) (Single 6))  -- represents [2, 4, 6]


-- ============================================================================
-- PART 3: Foldable Instance
-- ============================================================================

-- WHAT IS A MONOID?
-- Before we understand Foldable, we need to understand Monoid.
-- A Monoid is a type class for things that can be "combined" together.
-- 
-- A Monoid has two key properties:
-- 1. mempty  :: m           -- An "identity" or "neutral" element (like 0 for addition)
-- 2. mappend :: m -> m -> m -- A way to combine two values (like + for addition)
--    (can also write as <>)
--
-- Examples of Monoids:
-- - Numbers with addition: mempty = 0, mappend = (+)
-- - Lists: mempty = [], mappend = (++)
-- - Strings: mempty = "", mappend = (++)

-- WHAT IS FOLDABLE?
-- Foldable is a type class for structures that can be "folded" (collapsed/reduced)
-- into a single value by processing elements one by one.
--
-- Think of it like:
-- - You have a container with multiple elements
-- - You want to reduce/combine them all into one result
-- - Examples: sum a list, concatenate strings, find maximum, etc.
--
-- The key function is foldMap:
-- foldMap :: Monoid m => (a -> m) -> f a -> m
-- Read as: "Given a function that converts 'a' to a Monoid 'm',
--          and a structure containing 'a's,
--          combine all the results into one 'm'"

instance Foldable Sequence where
    -- foldMap processes elements LEFT-TO-RIGHT and combines results
    
    -- Case 1: Empty sequence
    -- No elements to process, so return the identity element
    -- mempty is the "neutral" value for the monoid (like 0 or empty list)
    foldMap _ Empty = mempty
    
    -- Case 2: Single element
    -- Apply the function 'f' to the element 'x'
    -- f :: a -> m  (converts element to monoid value)
    -- x :: a       (our element)
    -- f x :: m     (the monoid result)
    foldMap f (Single x) = f x
    
    -- Case 3: Appended sequences
    -- Process LEFT first, then RIGHT (left-to-right order!)
    -- Combine the results using <>  (which is the same as mappend)
    -- <> :: m -> m -> m   (combines two monoid values)
    foldMap f (Append left right) = foldMap f left <> foldMap f right
    --                               ^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^
    --                               Process left       Process right
    --                               (recursively)      (recursively)
    --                                      then combine with <>


-- ============================================================================
-- PART 4: Using Foldable Functions
-- ============================================================================

-- Now that we have a Foldable instance, we can use ALL the library functions
-- that work with Foldable! Here are two:

-- Convert sequence to list (left-to-right order)
-- The function 'toList' is provided by the Foldable type class
-- It uses our foldMap under the hood!
seqToList :: Sequence a -> [a]
seqToList = toList
-- That's it! toList is a library function that works for ANY Foldable
-- 
-- How it works internally:
-- toList uses foldMap with a function that wraps each element in a list: (: [])
-- Then combines them with (++) to build the final list
--
-- Example: seqToList (Append (Single 1) (Single 2)) = [1, 2]

-- Count the number of elements
-- The function 'length' is provided by the Foldable type class
seqLength :: Sequence a -> Int
seqLength = length
-- That's it! length is a library function that works for ANY Foldable
--
-- How it works internally:
-- length uses foldMap with a function that converts each element to 1 (using Sum monoid)
-- Then adds them all up
--
-- Example: seqLength (Append (Single 'a') (Single 'b')) = 2


-- ============================================================================
-- HOW FOLDMAP WORKS - DETAILED EXAMPLE
-- ============================================================================

-- Let's trace through an example step by step:
-- 
-- Consider: seqToList (Append (Single 1) (Append (Single 2) (Single 3)))
--
-- Step 1: toList calls foldMap with a function that converts each element to a list
--         foldMap (\x -> [x]) (Append (Single 1) (Append (Single 2) (Single 3)))
--
-- Step 2: Pattern match on Append, so we process left and right:
--         foldMap f (Single 1)  <>  foldMap f (Append (Single 2) (Single 3))
--
-- Step 3: Process left side (Single 1):
--         f 1  =  [1]
--
-- Step 4: Process right side (Append (Single 2) (Single 3)):
--         foldMap f (Single 2)  <>  foldMap f (Single 3)
--         =  f 2  <>  f 3
--         =  [2]  <>  [3]
--         =  [2, 3]
--
-- Step 5: Combine everything:
--         [1]  <>  [2, 3]
--         =  [1, 2, 3]
--
-- Final result: [1, 2, 3]


-- ============================================================================
-- MORE EXAMPLES OF FOLDABLE FUNCTIONS YOU CAN NOW USE
-- ============================================================================

-- Thanks to our Foldable instance, ALL these work automatically:
-- 
-- sum (Append (Single 1) (Single 2))          = 3
-- product (Append (Single 2) (Single 3))      = 6
-- maximum (Append (Single 1) (Single 5))      = 5
-- null Empty                                   = True
-- null (Single 1)                              = False
-- elem 2 (Append (Single 1) (Single 2))       = True
-- foldr (+) 0 (Append (Single 1) (Single 2))  = 3


-- ============================================================================
-- PART 5: Semigroup and Monoid for Sequence
-- ============================================================================

-- WHAT IS A SEMIGROUP?
-- A Semigroup is a type class for things that can be COMBINED together.
-- It has one main operation:
--   (<>) :: a -> a -> a
--   Read as: "Given two values of type 'a', combine them into one value of type 'a'"
--
-- The combination must be ASSOCIATIVE, meaning:
--   (x <> y) <> z  =  x <> (y <> z)
--   (The grouping doesn't matter, the result is the same)
--
-- Examples:
--   - Lists: [1,2] <> [3,4] = [1,2,3,4]
--   - Strings: "Hello" <> " " <> "World" = "Hello World"
--   - Numbers (with addition): 3 <> 5 = 8

instance Semigroup (Sequence a) where
    -- How do we combine two Sequences?
    -- Simple: use the Append constructor!
    -- 
    -- (<>) :: Sequence a -> Sequence a -> Sequence a
    -- x :: Sequence a  (first sequence)
    -- y :: Sequence a  (second sequence)
    -- Result: Append x y  (combines them)
    
    (<>) = Append
    -- This is a very short definition! 
    -- It says: "To combine two sequences, just Append them"
    --
    -- In longer form, this is equivalent to:
    -- x <> y = Append x y
    --
    -- Examples:
    -- Single 1 <> Single 2 = Append (Single 1) (Single 2)
    -- Empty <> Single 3 = Append Empty (Single 3)
    -- (Single 1 <> Single 2) <> Single 3 = Append (Append (Single 1) (Single 2)) (Single 3)


-- WHAT IS A MONOID?
-- A Monoid is a Semigroup WITH an identity element (called mempty).
-- The identity element is special: combining it with anything gives back that thing.
--
-- Monoid = Semigroup + mempty
--
-- The identity element must satisfy:
--   mempty <> x = x
--   x <> mempty = x
--
-- Examples:
--   - Lists: mempty = []         because  [] ++ xs = xs  and  xs ++ [] = xs
--   - Numbers (addition): mempty = 0  because  0 + x = x  and  x + 0 = x
--   - Numbers (multiplication): mempty = 1  because  1 * x = x  and  x * 1 = x

instance Monoid (Sequence a) where
    -- What's the identity element for Sequence?
    -- It should be a sequence that, when appended to anything, doesn't change it.
    -- 
    -- mempty :: Sequence a
    
    mempty = Empty
    -- Empty is perfect!
    -- Empty <> x = Append Empty x  (which represents the same sequence as x)
    -- x <> Empty = Append x Empty  (which represents the same sequence as x)
    --
    -- Examples:
    -- Empty <> Single 5 = Append Empty (Single 5)  -- represents [5]
    -- Single 5 <> Empty = Append (Single 5) Empty  -- represents [5]


-- ============================================================================
-- UNDERSTANDING THE RELATIONSHIP
-- ============================================================================

-- Now here's the beautiful thing:
-- We've now seen Monoid from BOTH sides!
--
-- PART 3 (Foldable): We USED monoids
--   - We had a Sequence and wanted to fold it into a monoid value
--   - We used foldMap to combine elements with <>
--   - Example: foldMap (\x -> [x]) seq  -- combines into a list
--
-- PART 5 (now): We MADE Sequence INTO a monoid
--   - Sequence itself can be combined with <>
--   - Sequence itself has an identity element (Empty)
--   - Now Sequence IS a monoid!


-- ============================================================================
-- EXAMPLES AND USAGE
-- ============================================================================

-- Now we can use <> and mempty directly on Sequences!

-- Example 1: Combining sequences
-- let s1 = Single 1
-- let s2 = Single 2
-- let s3 = Single 3
-- s1 <> s2         = Append (Single 1) (Single 2)
-- s1 <> s2 <> s3   = Append (Append (Single 1) (Single 2)) (Single 3)

-- Example 2: Using mempty (identity)
-- mempty <> Single 5  = Append Empty (Single 5)  -- still represents [5]
-- Empty <> Empty      = Append Empty Empty       -- still represents []

-- Example 3: Building sequences with <>
-- Single 'H' <> Single 'i'  = Append (Single 'H') (Single 'i')
-- This is now a sequence representing ['H', 'i']

-- Example 4: Combining the Functor, Foldable, and Monoid together!
-- let s = Single 1 <> Single 2 <> Single 3
-- fmap (*10) s        -- Apply function to each element
-- seqToList s         -- Convert to [1, 2, 3]
-- seqLength s         -- Get length: 3


-- ============================================================================
-- PART 6: Tail Recursion with Explicit Stack
-- ============================================================================

-- WHAT IS TAIL RECURSION?
-- 
-- First, let's understand NORMAL recursion vs TAIL recursion:
--
-- NORMAL RECURSION (NOT tail recursive):
--   factorial n = if n == 0 then 1 else n * factorial (n - 1)
--   
--   When computing factorial 3:
--   factorial 3
--   = 3 * factorial 2         -- Must remember to multiply by 3
--   = 3 * (2 * factorial 1)   -- Must remember to multiply by 3 and 2
--   = 3 * (2 * (1 * factorial 0))
--   = 3 * (2 * (1 * 1))
--   = 3 * (2 * 1)
--   = 3 * 2
--   = 6
--   
--   The call stack GROWS because we need to remember what to do after each call!
--
-- TAIL RECURSION:
--   factorial' n acc = if n == 0 then acc else factorial' (n - 1) (n * acc)
--   
--   When computing factorial' 3 1:
--   factorial' 3 1
--   = factorial' 2 3      -- Just jump to next call, nothing to remember
--   = factorial' 1 6      -- Just jump to next call, nothing to remember
--   = factorial' 0 6      -- Just jump to next call, nothing to remember
--   = 6                   -- Done!
--   
--   The call stack DOESN'T GROW because the recursive call is the LAST thing!
--   The compiler can optimize this into a loop!

-- WHY USE AN EXPLICIT STACK?
-- 
-- Our Sequence is a TREE structure:
--        Append
--       /      \
--   Single 1   Append
--             /      \
--         Single 2  Single 3
--
-- To search through it, we need to:
-- 1. Check the current node
-- 2. Remember to check the other branches later
-- 
-- Normally, we'd use recursion, and the CALL STACK remembers what to do.
-- With tail recursion, we can't use the call stack, so we maintain our OWN stack!


-- UNDERSTANDING THE ALGORITHM:
-- 
-- Think of it like exploring a maze with a notebook:
-- 1. You have a "current room" (current sequence)
-- 2. You have a "to-visit list" (the stack)
-- 3. When you find a fork, write down one path in your notebook and take the other
-- 4. When you hit a dead end, look at your notebook for the next path to explore


tailElem :: Eq a => a -> Sequence a -> Bool
-- Let's break down the type signature:
-- Eq a =>           Constraint: type 'a' must support equality (==)
-- a ->              Parameter 1: the element we're searching for
-- Sequence a ->     Parameter 2: the sequence to search in
-- Bool              Return: True if found, False if not found

tailElem target seq = go seq []
  -- We start by calling our helper function 'go' with:
  -- - The initial sequence to search
  -- - An empty stack []
  where
    -- The helper function 'go' does the actual work
    -- Type: Sequence a -> [Sequence a] -> Bool
    -- 
    -- Parameters:
    --   current :: Sequence a       The current sequence we're examining
    --   stack   :: [Sequence a]     Sequences we still need to check
    -- 
    -- Returns: Bool (True if element found, False otherwise)
    
    -- CASE 1: Current sequence is Empty
    -- Nothing here, so check the stack
    go Empty stack = case stack of
        []          -> False    -- Stack is empty, we've searched everything - NOT FOUND
        (next:rest) -> go next rest   -- Pop the next sequence from stack and continue
        -- 'next' is the top of the stack (next sequence to examine)
        -- 'rest' is the remaining stack
        -- This is TAIL RECURSIVE because 'go next rest' is the LAST thing we do!
    
    -- CASE 2: Current sequence is a Single element
    -- Check if this element matches what we're looking for
    go (Single x) stack
        | x == target = True    -- FOUND IT! Return True immediately
        | otherwise   = case stack of   -- Not a match, check the stack
            []          -> False         -- Nothing left to check - NOT FOUND
            (next:rest) -> go next rest  -- Pop next from stack and continue
    -- The vertical bar '|' means "guard" - it's like an if-condition
    -- 'x == target' is the condition to check
    
    -- CASE 3: Current sequence is an Append of two subsequences
    -- We need to search BOTH left and right, but we can only examine one at a time!
    -- Solution: Push one onto the stack, examine the other now
    go (Append left right) stack = go left (right : stack)
        -- We examine 'left' immediately
        -- We push 'right' onto the stack (using : which is "cons" - add to front of list)
        -- (right : stack) creates a new list with 'right' at the front
        --
        -- Why left first? Because we want LEFT-TO-RIGHT traversal!
        --
        -- This is TAIL RECURSIVE because 'go left (right : stack)' is the last thing!
    
    -- Let's trace through an example:
    -- tailElem 2 (Append (Single 1) (Append (Single 2) (Single 3)))
    --
    -- Initial call: go (Append (Single 1) (Append (Single 2) (Single 3))) []
    --
    -- Step 1: Match 'Append left right' where left = Single 1, right = Append (Single 2) (Single 3)
    --         go (Single 1) [Append (Single 2) (Single 3)]
    --         Stack: [Append (Single 2) (Single 3)]
    --
    -- Step 2: Match 'Single 1', check 1 == 2? No. Pop from stack.
    --         go (Append (Single 2) (Single 3)) []
    --         Stack: []
    --
    -- Step 3: Match 'Append left right' where left = Single 2, right = Single 3
    --         go (Single 2) [Single 3]
    --         Stack: [Single 3]
    --
    -- Step 4: Match 'Single 2', check 2 == 2? YES!
    --         Return True
    --
    -- FOUND IT!


-- ============================================================================
-- KEY HASKELL SYNTAX EXPLAINED
-- ============================================================================

-- 1. WHERE CLAUSE:
--    tailElem target seq = go seq []
--      where
--        go = ...
--    
--    'where' lets you define helper functions that are local to this function
--    'go' is only visible inside 'tailElem'

-- 2. GUARDS (|):
--    go (Single x) stack
--        | x == target = True
--        | otherwise   = ...
--    
--    Guards are like if-else chains
--    Each '|' is a condition to check
--    'otherwise' is like 'else' (it's always true)

-- 3. CASE EXPRESSION:
--    case stack of
--        []          -> False
--        (next:rest) -> go next rest
--    
--    Pattern matching inside an expression
--    [] matches empty list
--    (next:rest) matches non-empty list, splitting into head and tail

-- 4. LIST CONSTRUCTOR (:):
--    (right : stack)
--    
--    ':' is pronounced "cons"
--    It adds an element to the front of a list
--    1 : [2, 3] = [1, 2, 3]


-- ============================================================================
-- EXAMPLES AND TESTS
-- ============================================================================

-- Example 1: Element exists
-- let s = Single 1 <> Single 2 <> Single 3
-- tailElem 2 s = True

-- Example 2: Element doesn't exist
-- let s = Single 1 <> Single 2 <> Single 3
-- tailElem 5 s = False

-- Example 3: Empty sequence
-- tailElem 1 Empty = False

-- Example 4: Single element - found
-- tailElem 5 (Single 5) = True

-- Example 5: Single element - not found
-- tailElem 3 (Single 5) = False


-- ============================================================================
-- PART 7: Tail Recursive Sequence to List Conversion
-- ============================================================================

-- GOAL: Convert a Sequence to a list in left-to-right order
-- Challenge: Do it with tail recursion!

-- STRATEGY:
-- Similar to tailElem, we'll use:
-- 1. A helper function 'go'
-- 2. An explicit stack to track sequences we need to process
-- 3. An ACCUMULATOR to build the result list
--
-- The accumulator is NEW! We'll build the list as we traverse.

-- IMPORTANT TECHNIQUE: Build list in REVERSE, then reverse at the end
-- Why? Because adding to the FRONT of a list (with :) is O(1) - very fast!
-- Adding to the END of a list is O(n) - slow!
--
-- So we:
-- 1. Build the list backwards: [3, 2, 1]
-- 2. Reverse it at the end: [1, 2, 3]


tailToList :: Sequence a -> [a]
-- Type breakdown:
-- Sequence a  ->  Parameter: the sequence to convert
-- [a]             Return: a list of elements

tailToList seq = reverse (go seq [] [])
  -- Start with:
  -- - seq: the initial sequence
  -- - []: empty stack (no sequences waiting to be processed)
  -- - []: empty accumulator (no elements collected yet)
  --
  -- Then REVERSE the result because we build it backwards!
  where
    -- Helper function 'go'
    -- Type: Sequence a -> [Sequence a] -> [a] -> [a]
    -- Parameters:
    --   1. current :: Sequence a       - Current sequence being examined
    --   2. stack   :: [Sequence a]     - Sequences waiting to be processed
    --   3. acc     :: [a]              - Accumulated elements (in REVERSE order)
    -- Returns: [a] (the accumulated list, still in reverse)
    
    -- CASE 1: Empty sequence
    -- No elements here, check the stack
    go Empty stack acc = case stack of
        []          -> acc              -- Stack empty, we're done! Return accumulator
        (next:rest) -> go next rest acc -- Pop from stack, continue with same accumulator
        -- Notice: 'acc' is unchanged because Empty has no elements to add
    
    -- CASE 2: Single element
    -- Found an element! Add it to our accumulator
    go (Single x) stack acc = case stack of
        []          -> x : acc          -- Add x to accumulator, no more work
        (next:rest) -> go next rest (x : acc)  -- Add x to acc, continue with next from stack
        -- 'x : acc' adds x to the FRONT of the accumulator
        -- This is O(1) - very efficient!
        -- Example: 2 : [1] = [2, 1]
    
    -- CASE 3: Append of two subsequences
    -- We have two parts to process: left and right
    -- Process LEFT first (for left-to-right order), push RIGHT to stack
    go (Append left right) stack acc = go left (right : stack) acc
        -- Push 'right' onto stack
        -- Process 'left' immediately
        -- Pass accumulator unchanged (we'll add elements when we hit Singles)
        -- This is TAIL RECURSIVE - the recursive call is the last operation!


-- ============================================================================
-- DETAILED TRACE EXAMPLE
-- ============================================================================

-- Let's trace: tailToList (Append (Single 1) (Append (Single 2) (Single 3)))
--
-- Initial call: tailToList seq
--               = reverse (go seq [] [])
--
-- Step 1: go (Append (Single 1) (Append (Single 2) (Single 3))) [] []
--         Match: Append left right where left = Single 1, right = Append (Single 2) (Single 3)
--         Action: go (Single 1) [Append (Single 2) (Single 3)] []
--         State: current = Single 1
--                stack   = [Append (Single 2) (Single 3)]
--                acc     = []
--
-- Step 2: go (Single 1) [Append (Single 2) (Single 3)] []
--         Match: Single 1, stack is non-empty
--         Action: go (Append (Single 2) (Single 3)) [] (1 : [])
--         State: current = Append (Single 2) (Single 3)
--                stack   = []
--                acc     = [1]
--
-- Step 3: go (Append (Single 2) (Single 3)) [] [1]
--         Match: Append left right where left = Single 2, right = Single 3
--         Action: go (Single 2) [Single 3] [1]
--         State: current = Single 2
--                stack   = [Single 3]
--                acc     = [1]
--
-- Step 4: go (Single 2) [Single 3] [1]
--         Match: Single 2, stack is non-empty
--         Action: go (Single 3) [] (2 : [1])
--         State: current = Single 3
--                stack   = []
--                acc     = [2, 1]     ← Building in REVERSE!
--
-- Step 5: go (Single 3) [] [2, 1]
--         Match: Single 3, stack is empty
--         Action: return (3 : [2, 1])
--         Result: [3, 2, 1]           ← Still in REVERSE!
--
-- Step 6: reverse [3, 2, 1]
--         Result: [1, 2, 3]           ← Correct order! ✓


-- ============================================================================
-- WHY BUILD IN REVERSE?
-- ============================================================================

-- List operations in Haskell:
--
-- Adding to FRONT (cons with :):   O(1) - constant time, FAST! ✓
--   x : xs  =  just creates a new node pointing to existing list
--   Example: 1 : [2, 3] = [1, 2, 3]  (instant!)
--
-- Adding to END (append with ++):  O(n) - linear time, SLOW! ✗
--   xs ++ [x]  =  must traverse entire list xs, then add x
--   Example: [1, 2] ++ [3] = must walk past 1 and 2 (slow!)
--
-- Reversing a list:  O(n) - linear time
--   reverse [3, 2, 1] = [1, 2, 3]
--
-- Our approach:
--   Build with n cons operations: n × O(1) = O(n)
--   Reverse once: O(n)
--   Total: O(n) + O(n) = O(n) - LINEAR TIME! ✓
--
-- Alternative (slow approach):
--   Build with n append operations: n × O(n) = O(n²)
--   Total: O(n²) - QUADRATIC TIME! ✗ Very slow for large sequences!


-- ============================================================================
-- KEY DIFFERENCES FROM tailElem
-- ============================================================================

-- tailElem (from Part 6):
--   - Searches for ONE element
--   - Returns True/False
--   - Stops early when found
--   - No accumulator needed
--
-- tailToList (this part):
--   - Collects ALL elements
--   - Returns a list [a]
--   - Must traverse entire structure
--   - Uses accumulator to build result


-- ============================================================================
-- EXAMPLES
-- ============================================================================

-- Example 1: Empty sequence
-- tailToList Empty = []

-- Example 2: Single element
-- tailToList (Single 5) = [5]

-- Example 3: Multiple elements
-- tailToList (Single 1 <> Single 2 <> Single 3) = [1, 2, 3]

-- Example 4: Nested structure
-- let s = Append (Append (Single 1) (Single 2)) (Single 3)
-- tailToList s = [1, 2, 3]

-- Example 5: Should match seqToList from Part 4
-- For any sequence s:
-- tailToList s == seqToList s  (both give same result!)


-- ============================================================================
-- PART 8: Reverse Polish Notation (RPN) Calculator
-- ============================================================================

-- WHAT IS REVERSE POLISH NOTATION (RPN)?
-- 
-- RPN is a way to write mathematical expressions WITHOUT parentheses!
-- Instead of writing operators BETWEEN numbers (infix notation),
-- we write operators AFTER the numbers (postfix notation).
--
-- INFIX (normal):      (3 + 4) * 5
-- RPN (postfix):       3 4 + 5 *
--
-- How to read RPN:
-- - Numbers: push them onto a stack
-- - Operators: pop two numbers, apply operator, push result back
--
-- Example: 3 4 + 5 *
-- Step 1: Push 3           Stack: [3]
-- Step 2: Push 4           Stack: [4, 3]
-- Step 3: Apply +          Pop 4 and 3, compute 3+4=7, push 7   Stack: [7]
-- Step 4: Push 5           Stack: [5, 7]
-- Step 5: Apply *          Pop 5 and 7, compute 7*5=35, push 35 Stack: [35]
-- Result: 35

-- WHY USE RPN?
-- - No parentheses needed!
-- - Easy to evaluate with a stack
-- - Used in some calculators (HP calculators)
-- - Used in stack-based programming languages (Forth, PostScript)


-- ============================================================================
-- TOKEN DATA TYPE
-- ============================================================================

-- Define the tokens that can appear in an RPN expression
data Token = TNum Int  -- A number (e.g., 5, 42, -3)
           | TAdd      -- Addition operator (+)
           | TSub      -- Subtraction operator (-)
           | TMul      -- Multiplication operator (*)
           | TDiv      -- Division operator (/)
           deriving (Show, Eq)

-- Examples:
-- [TNum 3, TNum 4, TAdd]               represents: 3 4 +  (which is 3 + 4 = 7)
-- [TNum 10, TNum 5, TDiv]              represents: 10 5 / (which is 10 / 5 = 2)
-- [TNum 3, TNum 4, TAdd, TNum 5, TMul] represents: 3 4 + 5 * (which is (3+4)*5 = 35)


-- ============================================================================
-- THE MAYBE TYPE (Error Handling)
-- ============================================================================

-- WHAT IS MAYBE?
-- Maybe is Haskell's way to handle operations that might fail.
-- It's defined as:
--   data Maybe a = Nothing | Just a
--
-- - Just x   means "success, the result is x"
-- - Nothing  means "failure, something went wrong"
--
-- Examples:
-- Just 42    -- Success: the result is 42
-- Nothing    -- Failure: no result (error)
--
-- Why use Maybe instead of exceptions?
-- - Forces you to handle errors explicitly
-- - The type system ensures you check for errors
-- - More functional programming style

-- When do we return Nothing in our RPN evaluator?
-- 1. Too few operands for an operator (e.g., just "+" with no numbers)
-- 2. Division by zero (e.g., "5 0 /")
-- 3. Stack has more than one value at the end (e.g., "3 4" with no operator)


-- ============================================================================
-- RPN EVALUATOR IMPLEMENTATION
-- ============================================================================

tailRPN :: [Token] -> Maybe Int
-- Type breakdown:
-- [Token]     -> Parameter: list of tokens to evaluate
-- Maybe Int      Return: Just result on success, Nothing on error

tailRPN tokens = go tokens []
  -- Start with:
  -- - tokens: the full list of tokens to process
  -- - []: empty stack (no operands yet)
  where
    -- Helper function 'go'
    -- Type: [Token] -> [Int] -> Maybe Int
    -- Parameters:
    --   tokens :: [Token]  - Remaining tokens to process
    --   stack  :: [Int]    - Operand stack (accumulator)
    -- Returns: Maybe Int (Just result or Nothing on error)
    
    -- CASE 1: No more tokens, stack has exactly one value
    -- This is SUCCESS! The final value is our result.
    go [] [result] = Just result
    --  ^   ^         ^
    --  |   |         Return success with the result
    --  |   Stack has exactly one value
    --  No more tokens to process
    
    -- CASE 2: No more tokens, but stack is empty or has multiple values
    -- This is an ERROR - malformed expression!
    go [] _ = Nothing
    --  ^  ^    ^
    --  |  |    Return failure
    --  |  Any stack that isn't a single value (_, wildcard)
    --  No more tokens
    -- Examples of errors:
    -- - Stack empty: no result computed
    -- - Stack [3, 4]: two values left, missing an operator
    
    -- CASE 3: Process a number token
    -- Push the number onto the stack
    go (TNum n : rest) stack = go rest (n : stack)
    --  ^^^^^^^         ^        ^       ^^^^^^^
    --  |               |        |       Push n onto stack
    --  |               |        Continue with remaining tokens
    --  |               Current stack
    --  Token is a number n, rest are remaining tokens
    -- This is TAIL RECURSIVE - go is the last operation!
    
    -- CASE 4: Process an addition operator
    -- Pop two values, add them, push result
    go (TAdd : rest) stack = case stack of
        (y : x : remaining) -> go rest ((x + y) : remaining)
        --  ^   ^   ^            ^       ^^^^^^^    ^
        --  |   |   |            |       |          Rest of stack
        --  |   |   |            |       Push result (x + y)
        --  |   |   Rest of stack        Continue with remaining tokens
        --  |   First value (deeper in stack)
        --  Second value (top of stack)
        --
        -- IMPORTANT: Order matters!
        -- Stack [4, 3, ...] means 4 is on top
        -- We compute: 3 + 4 (not 4 + 3)
        -- For addition it doesn't matter, but it does for subtraction/division!
        
        _ -> Nothing  -- Not enough operands (error!)
        -- If stack has fewer than 2 values, we can't add
    
    -- CASE 5: Process a subtraction operator
    -- Pop two values, subtract them, push result
    go (TSub : rest) stack = case stack of
        (y : x : remaining) -> go rest ((x - y) : remaining)
        --                                ^   ^
        --                                |   Second operand (top of stack)
        --                                First operand (deeper in stack)
        -- Stack [4, 3, ...] computes 3 - 4 = -1
        _ -> Nothing  -- Not enough operands (error!)
    
    -- CASE 6: Process a multiplication operator
    -- Pop two values, multiply them, push result
    go (TMul : rest) stack = case stack of
        (y : x : remaining) -> go rest ((x * y) : remaining)
        _ -> Nothing  -- Not enough operands (error!)
    
    -- CASE 7: Process a division operator
    -- Pop two values, divide them, push result
    -- SPECIAL: Check for division by zero!
    go (TDiv : rest) stack = case stack of
        (y : x : remaining) ->
            if y == 0
            then Nothing  -- Division by zero error!
            else go rest ((x `div` y) : remaining)
            --              ^^^^^^^^^
            --              Integer division (rounds down)
            -- 'div' is pronounced "div" and used in backticks as infix operator
            -- x `div` y is the same as div x y
        _ -> Nothing  -- Not enough operands (error!)


-- ============================================================================
-- DETAILED TRACE EXAMPLE
-- ============================================================================

-- Let's trace: tailRPN [TNum 3, TNum 4, TAdd, TNum 5, TMul]
-- This represents: 3 4 + 5 * which is (3 + 4) * 5 = 35
--
-- Call: tailRPN [TNum 3, TNum 4, TAdd, TNum 5, TMul]
--     = go [TNum 3, TNum 4, TAdd, TNum 5, TMul] []
--
-- Step 1: go [TNum 3, TNum 4, TAdd, TNum 5, TMul] []
--         Match: TNum 3
--         Action: Push 3 onto stack
--         Next: go [TNum 4, TAdd, TNum 5, TMul] [3]
--         Stack: [3]
--
-- Step 2: go [TNum 4, TAdd, TNum 5, TMul] [3]
--         Match: TNum 4
--         Action: Push 4 onto stack
--         Next: go [TAdd, TNum 5, TMul] [4, 3]
--         Stack: [4, 3]  ← 4 is on top
--
-- Step 3: go [TAdd, TNum 5, TMul] [4, 3]
--         Match: TAdd, stack = [4, 3]
--         Action: Pop 4 and 3, compute 3 + 4 = 7, push 7
--         Next: go [TNum 5, TMul] [7]
--         Stack: [7]
--
-- Step 4: go [TNum 5, TMul] [7]
--         Match: TNum 5
--         Action: Push 5 onto stack
--         Next: go [TMul] [5, 7]
--         Stack: [5, 7]  ← 5 is on top
--
-- Step 5: go [TMul] [5, 7]
--         Match: TMul, stack = [5, 7]
--         Action: Pop 5 and 7, compute 7 * 5 = 35, push 35
--         Next: go [] [35]
--         Stack: [35]
--
-- Step 6: go [] [35]
--         Match: No tokens, stack has one value [35]
--         Result: Just 35 ✓


-- ============================================================================
-- ERROR CASES
-- ============================================================================

-- Error 1: Too few operands
-- tailRPN [TAdd]
-- go [TAdd] []
-- Stack is empty, can't pop two values
-- Result: Nothing

-- Error 2: Missing operator
-- tailRPN [TNum 3, TNum 4]
-- go [TNum 3, TNum 4] []
-- → go [TNum 4] [3]
-- → go [] [4, 3]
-- Stack has TWO values at the end (should have one)
-- Result: Nothing

-- Error 3: Division by zero
-- tailRPN [TNum 5, TNum 0, TDiv]
-- go [TNum 5, TNum 0, TDiv] []
-- → go [TNum 0, TDiv] [5]
-- → go [TDiv] [0, 5]
-- → Check: y == 0? Yes! Return Nothing
-- Result: Nothing


-- ============================================================================
-- NEW HASKELL SYNTAX
-- ============================================================================

-- 1. PATTERN MATCHING ON LISTS:
--    go (TNum n : rest) stack = ...
--    ^   ^^^^^^^   ^^^^
--    |   |         Rest of the list
--    |   First element (matches TNum constructor)
--    |
--    Pattern match: list must start with TNum n
--
--    The : operator splits a list into head and tail
--    [1, 2, 3] matches (1 : [2, 3])

-- 2. NESTED PATTERN MATCHING:
--    case stack of
--        (y : x : remaining) -> ...
--         ^   ^   ^
--         |   |   Rest of stack
--         |   Second element
--         First element (top)
--
--    This matches a stack with AT LEAST 2 elements

-- 3. GUARDS WITH IF-THEN-ELSE:
--    if y == 0
--    then Nothing
--    else go rest ...
--
--    Standard if-then-else expression
--    Both branches must return the same type

-- 4. BACKTICK INFIX OPERATORS:
--    x `div` y
--    Instead of: div x y
--    Backticks make a function infix (between arguments)

-- 5. WILDCARD PATTERN (_):
--    go [] _ = Nothing
--          ^
--          Matches anything, we don't care about the value
--    Used when we want to match but don't need the value


-- ============================================================================
-- EXAMPLES
-- ============================================================================

-- Example 1: Simple addition
-- tailRPN [TNum 3, TNum 4, TAdd] = Just 7

-- Example 2: Complex expression (3 + 4) * 5
-- tailRPN [TNum 3, TNum 4, TAdd, TNum 5, TMul] = Just 35

-- Example 3: Division
-- tailRPN [TNum 10, TNum 2, TDiv] = Just 5

-- Example 4: Multiple operations (15 - 5) / 2
-- tailRPN [TNum 15, TNum 5, TSub, TNum 2, TDiv] = Just 5

-- Example 5: Error - too few operands
-- tailRPN [TAdd] = Nothing

-- Example 6: Error - missing operator
-- tailRPN [TNum 3, TNum 4] = Nothing

-- Example 7: Error - division by zero
-- tailRPN [TNum 5, TNum 0, TDiv] = Nothing


-- ============================================================================
-- PART 9: Functions Using Folds (foldr and foldl)
-- ============================================================================

-- WHAT ARE FOLDS?
-- 
-- Folds are a way to "reduce" or "collapse" a list into a single value
-- by repeatedly applying a function. They're the ultimate list processors!
--
-- Think of it like this:
-- - You have a list: [1, 2, 3, 4]
-- - You want to combine all elements into one result
-- - You apply a function step by step to combine them
--
-- There are TWO main folds: foldr and foldl


-- ============================================================================
-- UNDERSTANDING FOLDR (Fold Right)
-- ============================================================================

-- foldr :: (a -> b -> b) -> b -> [a] -> b
--          ─────────────    ─    ───    ─
--               │           │     │     │
--               │           │     │     Result type
--               │           │     List to process
--               │           Initial value (starting point)
--               Combining function
--
-- foldr PROCESSES FROM RIGHT TO LEFT (like the name says: fold-right)
-- 
-- Example: foldr (+) 0 [1, 2, 3]
-- 
-- Visualization:
--     1 + (2 + (3 + 0))
--     │    │    │    │
--     │    │    │    Initial value
--     │    │    Start here! →
--     │    Then combine with 2
--     Finally combine with 1
--
-- Step by step:
-- foldr (+) 0 [1, 2, 3]
-- = 1 + foldr (+) 0 [2, 3]
-- = 1 + (2 + foldr (+) 0 [3])
-- = 1 + (2 + (3 + foldr (+) 0 []))
-- = 1 + (2 + (3 + 0))
-- = 1 + (2 + 3)
-- = 1 + 5
-- = 6


-- ============================================================================
-- UNDERSTANDING FOLDL (Fold Left)
-- ============================================================================

-- foldl :: (b -> a -> b) -> b -> [a] -> b
--          ─────────────    ─    ───    ─
--               │           │     │     │
--               │           │     │     Result type
--               │           │     List to process
--               │           Initial value (accumulator)
--               Combining function
--
-- foldl PROCESSES FROM LEFT TO RIGHT (like the name says: fold-left)
-- It builds up an accumulator as it goes
--
-- Example: foldl (+) 0 [1, 2, 3]
--
-- Visualization:
--     ((0 + 1) + 2) + 3
--       │    │    │    │
--       │    │    │    Last element
--       │    │    Second element
--       │    First element
--       Initial value (accumulator starts here)
--
-- Step by step:
-- foldl (+) 0 [1, 2, 3]
-- = foldl (+) (0 + 1) [2, 3]
-- = foldl (+) 1 [2, 3]
-- = foldl (+) (1 + 2) [3]
-- = foldl (+) 3 [3]
-- = foldl (+) (3 + 3) []
-- = foldl (+) 6 []
-- = 6


-- ============================================================================
-- KEY DIFFERENCE: foldr vs foldl
-- ============================================================================

-- ORDER OF OPERATIONS:
-- foldr: processes right-to-left, builds result from the inside out
-- foldl: processes left-to-right, builds result with accumulator
--
-- WHEN TO USE WHICH:
-- - Use foldr when:
--   * Building a new list (constructing with :)
--   * Need to potentially stop early (short-circuit)
--   * Working with infinite lists (foldr can be lazy!)
--
-- - Use foldl when:
--   * Building an accumulator (like a sum)
--   * Order matters and you want left-to-right
--   * Need strict evaluation (use foldl' for efficiency)
--
-- ASSOCIATION:
-- foldr f z [1,2,3] = 1 `f` (2 `f` (3 `f` z))  -- right-associative
-- foldl f z [1,2,3] = ((z `f` 1) `f` 2) `f` 3  -- left-associative


-- ============================================================================
-- PART 9a: myReverse using foldl
-- ============================================================================

myReverse :: [a] -> [a]
-- Reverses a list using foldl (NO explicit recursion!)
--
-- Strategy: Build the reversed list by repeatedly putting elements at the front
-- - Start with empty list []
-- - For each element x, put it at the FRONT of what we've built so far
-- - This naturally reverses the order!
--
-- Example: myReverse [1, 2, 3]
-- Step 1: acc = [],    x = 1,  result = 1 : [] = [1]
-- Step 2: acc = [1],   x = 2,  result = 2 : [1] = [2, 1]
-- Step 3: acc = [2,1], x = 3,  result = 3 : [2, 1] = [3, 2, 1]
-- Final: [3, 2, 1]

myReverse = foldl (\acc x -> x : acc) []
--          ^^^^^  ^^^^^^^^^^^^^^^^^^^^  ^^
--          |      |                     |
--          |      |                     Initial accumulator (empty list)
--          |      Combining function: put x at front of accumulator
--          Use foldl (left fold)
--
-- In more detail:
-- \acc x -> x : acc
--  ^   ^    ^^^^^^^
--  |   |    Put x in front of accumulator
--  |   Current element
--  Accumulator (result so far)
--
-- Why this works:
-- foldl processes LEFT to RIGHT
-- [1, 2, 3]
-- Start:  acc = []
-- See 1:  acc = 1 : [] = [1]
-- See 2:  acc = 2 : [1] = [2, 1]
-- See 3:  acc = 3 : [2, 1] = [3, 2, 1]
-- Result: [3, 2, 1] ✓


-- ============================================================================
-- PART 9b: myTakeWhile using foldr
-- ============================================================================

myTakeWhile :: (a -> Bool) -> [a] -> [a]
-- Returns the longest prefix of elements satisfying a predicate
-- Uses foldr (NO explicit recursion!)
--
-- Strategy: Build result from right to left
-- - If current element satisfies predicate AND rest is being kept, keep it
-- - Otherwise, stop (return empty list)
--
-- Example: myTakeWhile even [2, 4, 3, 6]
-- Should return [2, 4] (stops at 3, which is odd)

myTakeWhile pred = foldr step []
--              ^^^^      ^^^^  ^^
--              |         |     |
--              |         |     Initial value (empty list)
--              |         Combining function
--              Predicate function
  where
    -- step :: a -> [a] -> [a]
    -- Decide whether to include current element x
    step x acc
        | pred x    = x : acc  -- Predicate satisfied: include x in result
        | otherwise = []       -- Predicate failed: STOP, discard everything!
        --            ^^
        --            Return empty list (stop taking elements)
--
-- Why this works with foldr:
-- foldr processes RIGHT to LEFT
--
-- Example trace: myTakeWhile even [2, 4, 3, 6]
--
-- Reading right to left:
-- Step 1: x = 6, acc = [] (initial)
--         even 6? Yes → 6 : [] = [6]
--
-- Step 2: x = 3, acc = [6]
--         even 3? No → return []  ← STOP! Discard [6]
--
-- Step 3: x = 4, acc = []
--         even 4? Yes → 4 : [] = [4]
--
-- Step 4: x = 2, acc = [4]
--         even 2? Yes → 2 : [4] = [2, 4]
--
-- Final: [2, 4] ✓
--
-- Key insight: When we hit an element that fails the predicate,
-- we return [] which discards everything we built after it!


-- ============================================================================
-- PART 9c: decimal - Convert list of digits to number
-- ============================================================================

decimal :: [Int] -> Int
-- Interprets a list of digits as a decimal number
-- Example: decimal [1, 2, 3] = 123
--
-- Strategy: Build number from left to right
-- - Start with 0
-- - For each digit: multiply accumulator by 10, then add the digit
--
-- Example: decimal [1, 2, 3]
-- Step 1: acc = 0,  digit = 1,  result = 0 * 10 + 1 = 1
-- Step 2: acc = 1,  digit = 2,  result = 1 * 10 + 2 = 12
-- Step 3: acc = 12, digit = 3,  result = 12 * 10 + 3 = 123
-- Final: 123

decimal = foldl (\acc digit -> acc * 10 + digit) 0
--        ^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ^
--        |      |                                     |
--        |      |                                     Initial value (0)
--        |      Combining function
--        Use foldl (left fold, left to right)
--
-- Why this works:
-- [1, 2, 3]
-- Start:   acc = 0
-- See 1:   acc = 0 * 10 + 1 = 1
-- See 2:   acc = 1 * 10 + 2 = 12
-- See 3:   acc = 12 * 10 + 3 = 123
-- Result: 123 ✓
--
-- This is like reading a number left to right:
-- "1" → "12" → "123"


-- ============================================================================
-- PART 10: Run-Length Encoding with Folds
-- ============================================================================

-- WHAT IS RUN-LENGTH ENCODING?
--
-- Run-length encoding compresses data by replacing consecutive identical
-- elements with a pair: (element, count)
--
-- Example: "aaabccca" → [('a',3), ('b',1), ('c',3), ('a',1)]
--
-- Why useful?
-- - Compresses data with lots of repetition
-- - Used in image compression (BMP, TIFF)
-- - Used in data transmission


-- ============================================================================
-- PART 10a: encode - Run-length encoding using foldr
-- ============================================================================

encode :: Eq a => [a] -> [(a, Int)]
-- Compress a list using run-length encoding
-- Example: encode "aaabccca" = [('a',3),('b',1),('c',3),('a',1)]
--
-- Strategy with foldr (right to left):
-- - If list is empty, result is empty
-- - If current element equals the first element of the accumulated result,
--   increment its count
-- - Otherwise, start a new run

encode = foldr step []
  where
    -- step :: a -> [(a, Int)] -> [(a, Int)]
    step x []  = [(x, 1)]
    --   ^  ^^    ^^^^^^^^^
    --   |  |     Start a new run with count 1
    --   |  Accumulator is empty (first element from the right)
    --   Current element
    
    step x ((y, count) : rest)
    --   ^   ^   ^^^^^     ^^^^
    --   |   |   |         Rest of the encoded list
    --   |   |   Current count of y
    --   |   First element in accumulated result
    --   Current element we're processing
        | x == y    = (y, count + 1) : rest
        --            ^^^^^^^^^^^^^^^^
        --            Same element: increase count
        | otherwise = (x, 1) : (y, count) : rest
        --            ^^^^^^^^^^^^^^^^^^^^^
        --            Different element: start new run
--
-- Example trace: encode "aab"
-- Processing right to left: 'b', 'a', 'a'
--
-- Step 1: x = 'b', acc = []
--         Result: [('b', 1)]
--
-- Step 2: x = 'a', acc = [('b', 1)]
--         'a' == 'b'? No
--         Result: [('a', 1), ('b', 1)]
--
-- Step 3: x = 'a', acc = [('a', 1), ('b', 1)]
--         'a' == 'a'? Yes! Increment count
--         Result: [('a', 2), ('b', 1)]
--
-- Final: [('a', 2), ('b', 1)] ✓
--
-- More complex example: encode "aaabccca"
-- Right to left: 'a', 'c', 'c', 'c', 'b', 'a', 'a', 'a'
-- Step 1: 'a' → [('a',1)]
-- Step 2: 'c' → [('c',1), ('a',1)]
-- Step 3: 'c' → [('c',2), ('a',1)]
-- Step 4: 'c' → [('c',3), ('a',1)]
-- Step 5: 'b' → [('b',1), ('c',3), ('a',1)]
-- Step 6: 'a' → [('a',1), ('b',1), ('c',3), ('a',1)]
-- Step 7: 'a' → [('a',2), ('b',1), ('c',3), ('a',1)]
-- Step 8: 'a' → [('a',3), ('b',1), ('c',3), ('a',1)]
-- Final: [('a',3), ('b',1), ('c',3), ('a',1)] ✓


-- ============================================================================
-- PART 10b: decode - Reverse run-length encoding using foldr
-- ============================================================================

decode :: [(a, Int)] -> [a]
-- Decompress run-length encoded data
-- Example: decode [('a',3),('b',1),('c',3)] = "aaabccc"
--
-- Strategy with foldr:
-- - For each (element, count) pair
-- - Replicate the element 'count' times
-- - Append it to the accumulated result

decode = foldr step []
  where
    -- step :: (a, Int) -> [a] -> [a]
    step (x, count) acc = replicate count x ++ acc
    --    ^  ^^^^^   ^^^   ^^^^^^^^^^^^^^^^^^
    --    |  |       |     |
    --    |  |       |     Append to accumulated result
    --    |  |       Accumulated result (rest of decoded list)
    --    |  How many times to repeat
    --    Element to repeat
--
-- What is replicate?
-- replicate :: Int -> a -> [a]
-- replicate n x creates a list with n copies of x
-- Examples:
--   replicate 3 'a' = ['a', 'a', 'a'] = "aaa"
--   replicate 1 'b' = ['b'] = "b"
--   replicate 0 'x' = []
--
-- Example trace: decode [('a', 2), ('b', 1)]
-- Processing right to left: ('b',1), ('a',2)
--
-- Step 1: (x, count) = ('b', 1), acc = []
--         replicate 1 'b' = "b"
--         Result: "b" ++ [] = "b"
--
-- Step 2: (x, count) = ('a', 2), acc = "b"
--         replicate 2 'a' = "aa"
--         Result: "aa" ++ "b" = "aab"
--
-- Final: "aab" ✓
--
-- More complex: decode [('a',3), ('b',1), ('c',3), ('a',1)]
-- Right to left processing:
-- Step 1: ('a',1) → replicate 1 'a' ++ [] = "a"
-- Step 2: ('c',3) → replicate 3 'c' ++ "a" = "ccca"
-- Step 3: ('b',1) → replicate 1 'b' ++ "ccca" = "bccca"
-- Step 4: ('a',3) → replicate 3 'a' ++ "bccca" = "aaabccca"
-- Final: "aaabccca" ✓


-- ============================================================================
-- SUMMARY: Why Use Folds?
-- ============================================================================

-- Benefits of using folds over explicit recursion:
-- 1. More concise and declarative
-- 2. Less error-prone (no need to handle base cases manually)
-- 3. More composable (can be combined with other functions)
-- 4. Makes the pattern of computation explicit
-- 5. Often more efficient (especially foldl')
--
-- When you want to process a list:
-- - Ask yourself: "Am I combining elements into a result?"
-- - If yes, think about using a fold!
-- - Choose foldr for building lists, foldl for accumulators
