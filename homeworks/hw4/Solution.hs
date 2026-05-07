module Solution where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader (\r -> f (ra r))

instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  pure x = Reader (\_ -> x)
  
  -- liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
  liftA2 f (Reader ra) (Reader rb) = Reader (\r -> f (ra r) (rb r))

instance Monad (Reader r) where
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= f = Reader (\r -> runReader (f (ra r)) r)

-- Retrieves the entire environment.
ask :: Reader r r
ask = Reader (\r -> r)

-- Retrieves a value derived from the environment by applying a projection.
asks :: (r -> a) -> Reader r a
asks f = Reader f

-- Runs a subcomputation in a locally modified environment.
local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader (\r -> ra (f r))

-- Banking System Example

data BankConfig = BankConfig
  { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
  , transactionFee :: Int     -- flat fee charged per transaction
  , minimumBalance :: Int     -- minimum required balance on an account
  } deriving (Show)

data Account = Account
  { accountId :: String       -- account identifier
  , balance   :: Int          -- current balance
  } deriving (Show)

-- Computes the interest accrued on the account, based on the configured rate.
calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
  rate <- asks interestRate
  return $ round (fromIntegral (balance acc) * rate)

-- Deducts the transaction fee from the account and returns the updated account.
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  fee <- asks transactionFee
  return $ acc { balance = balance acc - fee }

-- Checks whether the account balance meets the configured minimum.
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
  minBalance <- asks minimumBalance
  return $ balance acc >= minBalance

-- Runs the three operations above on a single account and combines their results.
processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
  acc'     <- applyTransactionFee acc
  interest <- calculateInterest acc
  meetsMin <- checkMinimumBalance acc
  return (acc', interest, meetsMin)
