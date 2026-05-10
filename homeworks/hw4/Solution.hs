module Solution where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (\env -> f (ra env))

instance Applicative (Reader r) where
  pure x = Reader (\_ -> x)
  liftA2 f (Reader ra) (Reader rb) = Reader (\env -> f (ra env) (rb env))

instance Monad (Reader r) where
  Reader ra >>= f = Reader (\env -> runReader (f (ra env)) env)

ask :: Reader r r
ask = Reader id
asks :: (r -> a) -> Reader r a
asks projection = Reader projection

local :: (r -> r) -> Reader r a -> Reader r a
local modify (Reader ra) = Reader (\env -> ra (modify env))

data BankConfig = BankConfig
  { interestRate :: Double
  , transactionFee :: Int
  , minimumBalance :: Int
  }
  deriving (Show)

data Account = Account
  { accountId :: String
  , balance :: Int
  }
  deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
  rate <- asks interestRate
  let interestAsDouble = fromIntegral (balance account) * rate
  pure (round interestAsDouble)

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
  fee <- asks transactionFee
  pure account {balance = balance account - fee}

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
  requiredMinimum <- asks minimumBalance
  pure (balance account >= requiredMinimum)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
  accountAfterFee <- applyTransactionFee account
  originalInterest <- calculateInterest account
  originalMeetsMinimum <- checkMinimumBalance account
  pure (accountAfterFee, originalInterest, originalMeetsMinimum)
