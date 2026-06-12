-- | Homework 4 solutions with very detailed explanations.
--   This file focuses on the Reader monad and a small banking example.
--   The comments explain both theory and syntax in small steps.
module Solution where

-- ============================================================================
-- PART 0: VERY SMALL HASKELL SYNTAX PRIMER
-- ============================================================================
--
-- 1) Function type syntax:
--    f :: A -> B -> C
--    This means: f takes an A, then a B, and returns a C.
--    Arrow types associate to the right: A -> B -> C = A -> (B -> C).
--
-- 2) Record syntax:
--    data X = X { field1 :: T1, field2 :: T2 }
--    This creates a type X and accessor functions field1, field2.
--
-- 3) do-notation:
--    do
--      x <- action
--      y <- anotherAction
--      pure (x + y)
--    It is syntax sugar for chaining monadic computations with (>>=).
--
-- 4) The "pure" function:
--    pure puts a normal value into a context (like Reader or Maybe).
--    In Reader, pure x means "ignore the environment and just give x".
--
-- 5) Lambdas:
--    \env -> f env
--    This is an anonymous function (like a short inline function).

-- ============================================================================
-- PART 1: READER DATA TYPE
-- ============================================================================

-- WHAT IS READER?
-- Reader is a type for computations that read from a shared environment.
-- Think of it as:
--   "a function that needs some config data to run".
--
-- The environment type is called 'r'.
-- The result type is called 'a'.
--
-- newtype is like data but with exactly one constructor and one field.
-- It has no runtime overhead compared to the wrapped type.
newtype Reader r a = Reader { runReader :: r -> a }

-- BREAKING DOWN THE LINE ABOVE:
-- - "Reader r a" is the type name (with type parameters r and a).
-- - "Reader" is the constructor.
-- - "{ runReader :: r -> a }" is record syntax:
--   it creates a field accessor function called runReader.
-- - runReader :: Reader r a -> (r -> a)
--   This means if you have a Reader, you can extract the function inside.

-- ============================================================================
-- PART 2: FUNCTOR INSTANCE
-- ============================================================================

-- WHAT IS A FUNCTOR?
-- A Functor is something you can "map over".
-- It provides fmap:
--   fmap :: (a -> b) -> f a -> f b
--
-- For Reader, fmap should change the result value,
-- while keeping the environment the same.
instance Functor (Reader r) where
  -- fmap takes a function f and a Reader ra.
  -- ra :: r -> a
  -- We return a new Reader that:
  --   1) takes an environment env
  --   2) computes ra env to get an a
  --   3) applies f to get a b
  fmap f (Reader ra) = Reader (\env -> f (ra env))

-- ============================================================================
-- PART 3: APPLICATIVE INSTANCE
-- ============================================================================

-- WHAT IS APPLICATIVE?
-- Applicative lets us combine independent computations.
-- It provides:
--   pure  :: a -> f a
--   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
instance Applicative (Reader r) where
  -- pure ignores the environment and always returns x.
  -- This means the Reader does not "look" at the environment at all.
  pure x = Reader (\_ -> x)

  -- liftA2 combines two Readers by feeding them the same environment.
  -- ra :: r -> a
  -- rb :: r -> b
  -- The new Reader computes both results from the same env,
  -- then combines them with f.
  liftA2 f (Reader ra) (Reader rb) = Reader (\env -> f (ra env) (rb env))

-- ============================================================================
-- PART 4: MONAD INSTANCE
-- ============================================================================

-- WHAT IS A MONAD?
-- A Monad lets later computations depend on earlier results.
-- It provides (>>=):
--   (>>=) :: m a -> (a -> m b) -> m b
instance Monad (Reader r) where
  -- (>>=) takes a Reader and a function that builds a new Reader.
  -- We run the first Reader to get a value,
  -- then pass that value into the function to get the next Reader.
  -- The environment is passed to BOTH computations.
  Reader ra >>= f =
    Reader (\env ->
      runReader (f (ra env)) env
    )

-- ============================================================================
-- PART 5: STANDARD READER HELPERS
-- ============================================================================

-- ask gives you the entire environment as the result.
-- It is like "read the config".
ask :: Reader r r
ask = Reader id

-- asks lets you extract one field from the environment.
-- It is like "read the config and project a value".
asks :: (r -> a) -> Reader r a
asks projection = Reader projection

-- local runs a computation under a modified environment.
-- It is like temporarily changing the config for one computation.
local :: (r -> r) -> Reader r a -> Reader r a
local modify (Reader ra) = Reader (\env -> ra (modify env))

-- ============================================================================
-- PART 6: DOMAIN TYPES
-- ============================================================================

-- BankConfig holds settings that are shared across computations.
-- This is the "environment" for Reader.
data BankConfig = BankConfig
  { interestRate :: Double   -- Annual interest rate (e.g., 0.05 means 5%).
  , transactionFee :: Int    -- Flat fee subtracted from each account.
  , minimumBalance :: Int    -- Minimum balance required.
  }
  deriving (Show)

-- Account represents a single bank account.
data Account = Account
  { accountId :: String  -- An identifier for the account.
  , balance :: Int       -- Current balance in the account.
  }
  deriving (Show)

-- ============================================================================
-- PART 7: READER-BASED BUSINESS LOGIC
-- ============================================================================

-- calculateInterest reads the interestRate from the environment
-- and calculates interest for the given account.
calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
  -- The <- syntax means "run the Reader and bind its result to rate".
  rate <- asks interestRate

  -- let introduces a local definition.
  -- fromIntegral converts Int to Double so we can multiply by rate.
  let interestAsDouble = fromIntegral (balance account) * rate

  -- round converts Double back to Int.
  -- pure wraps the Int into the Reader context.
  pure (round interestAsDouble)

-- applyTransactionFee subtracts the configured fee from the account.
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
  fee <- asks transactionFee
  -- Record update syntax:
  -- account {balance = ...} makes a copy with a new balance.
  pure account {balance = balance account - fee}

-- checkMinimumBalance verifies that the account meets the minimum.
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
  requiredMinimum <- asks minimumBalance
  pure (balance account >= requiredMinimum)

-- processAccount runs all checks and returns combined results.
processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
  -- Apply transaction fee first (produces a modified account).
  accountAfterFee <- applyTransactionFee account

  -- Calculate interest based on the original account.
  originalInterest <- calculateInterest account

  -- Check the minimum balance based on the original account.
  originalMeetsMinimum <- checkMinimumBalance account

  -- Return all three results as a tuple.
  pure (accountAfterFee, originalInterest, originalMeetsMinimum)

-- ============================================================================
-- PART 8: HOW TO RUN THESE COMPUTATIONS (EXAMPLE)
-- ============================================================================
--
-- Example (not executed here):
--
-- let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
-- let acc = Account { accountId = "A-1", balance = 120 }
--
-- runReader (calculateInterest acc) cfg
--   ==> 6
--
-- runReader (processAccount acc) cfg
--   ==> (Account {accountId = "A-1", balance = 118}, 6, True)
--
-- The Reader lets you separate "what to do" from "where the config comes from".
