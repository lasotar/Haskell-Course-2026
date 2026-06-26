module Main where

import AST
import Parser (parseContract)
import Eval (State, TxCall(..), initState, executeTx)
import Ledger (Block(..), Chain(..), newChain, submitBlock, currentState)

import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure, exitSuccess)

type Test = (String, Bool)

runTests :: [Test] -> IO ()
runTests tests = do
  mapM_ printOne tests
  let failed = length (filter (not . snd) tests)
  putStrLn "---"
  putStrLn (show (length tests) ++ " tests, " ++ show failed ++ " failed")
  if failed > 0 then exitFailure else exitSuccess
  where
    printOne (name, True)  = putStrLn ("[PASS] " ++ name)
    printOne (name, False) = putStrLn ("[FAIL] " ++ name)

parsesOk :: String -> Bool
parsesOk src = case parseContract "<test>" src of
  Right _                                    -> True
  Left  (_ :: ParseErrorBundle String Void)  -> False

failsToParse :: String -> Bool
failsToParse = not . parsesOk

parsesTo :: String -> Contract -> Bool
parsesTo src expected = case parseContract "<test>" src of
  Right c -> c == expected
  Left  _ -> False

getBalance :: State -> String -> Int
getBalance state addr =
  case Map.lookup "balances" state of
    Just (VMap pairs) -> case lookup (VAddress addr) pairs of
      Just (VInt n) -> n
      _             -> 0
    _ -> 0

sumBalances :: State -> Int
sumBalances state =
  case Map.lookup "balances" state of
    Just (VMap pairs) -> sum [n | (_, VInt n) <- pairs]
    _ -> 0

unsafeParse :: String -> Contract
unsafeParse src = case parseContract "<test>" src of
  Right c -> c
  Left  e -> error (show e)

simpleCoinSrc :: String
simpleCoinSrc = unlines
  [ "contract SimpleCoin {"
  , "  state {"
  , "    balances: map<address, int> = empty;"
  , "    owner:    address           = sender;"
  , "  }"
  , "  transaction mint(to: address, amount: int) {"
  , "    require sender == owner;"
  , "    balances[to] := balances[to] + amount;"
  , "  }"
  , "  transaction transfer(to: address, amount: int) {"
  , "    require balances[sender] >= amount;"
  , "    balances[sender] := balances[sender] - amount;"
  , "    balances[to]     := balances[to] + amount;"
  , "  }"
  , "}"
  ]

coin :: Contract
coin = unsafeParse simpleCoinSrc

coinChain :: Chain
coinChain = newChain coin "alice"

mintTx :: String -> Int -> TxCall
mintTx to amount = TxCall "mint" "alice" [("to", VAddress to), ("amount", VInt amount)]

transferTx :: String -> String -> Int -> TxCall
transferTx from to amount = TxCall "transfer" from [("to", VAddress to), ("amount", VInt amount)]

vmContractSrc :: String
vmContractSrc = unlines
  [ "contract VM {"
  , "  state { x: int = 0; }"
  , "  transaction set(n: int) {"
  , "    require n >= 0;"
  , "    x := n;"
  , "  }"
  , "}"
  ]

vmContract :: Contract
vmContract = unsafeParse vmContractSrc

vmState :: State
vmState = initState vmContract "deployer"

parserTests :: [Test]
parserTests =
  [ ("empty contract parses",
      parsesTo "contract Empty {}" (Contract "Empty" [] []))
  , ("state var: int type",
      parsesOk "contract C { state { x: int = 0; } }")
  , ("state var: bool type",
      parsesOk "contract C { state { flag: bool = false; } }")
  , ("state var: address type",
      parsesOk "contract C { state { owner: address = sender; } }")
  , ("state var: map type",
      parsesOk "contract C { state { balances: map<address, int> = empty; } }")
  , ("init with integer literal",
      parsesOk "contract C { state { n: int = 42; } }")
  , ("init with sender",
      parsesOk "contract C { state { owner: address = sender; } }")
  , ("init with empty",
      parsesOk "contract C { state { m: map<address, int> = empty; } }")
  , ("init with true",
      parsesOk "contract C { state { b: bool = true; } }")
  , ("init with address literal",
      parsesOk "contract C { state { owner: address = \"alice\"; } }")
  , ("arithmetic expression",
      parsesOk "contract C { transaction t() { x := 1 + 2; } }")
  , ("comparison expression",
      parsesOk "contract C { transaction t() { require x >= 0; } }")
  , ("map index",
      parsesOk "contract C { transaction t() { require balances[sender] >= 0; } }")
  , ("nested map index",
      parsesOk "contract C { transaction t() { require a[b][c] >= 0; } }")
  , ("logical and",
      parsesOk "contract C { transaction t() { require x > 0 && y > 0; } }")
  , ("not expression",
      parsesOk "contract C { transaction t() { require not flag; } }")
  , ("parenthesised expression",
      parsesOk "contract C { transaction t() { require (x + y) > 0; } }")
  , ("require statement",
      parsesOk "contract C { transaction t() { require x > 0; } }")
  , ("assign to variable",
      parsesOk "contract C { transaction t() { x := 5; } }")
  , ("assign to map index",
      parsesOk "contract C { transaction t() { balances[sender] := 0; } }")
  , ("if without else",
      parsesOk "contract C { transaction t() { if x > 0 { y := 1; } } }")
  , ("if with else",
      parsesOk "contract C { transaction t() { if x > 0 { y := 1; } else { y := 0; } } }")
  , ("transaction with no parameters",
      parsesOk "contract C { transaction reset() { x := 0; } }")
  , ("transaction with one parameter",
      parsesOk "contract C { transaction set(n: int) { x := n; } }")
  , ("transaction with two parameters",
      parsesOk "contract C { transaction transfer(to: address, amount: int) { require amount > 0; } }")
  , ("line comment is ignored",
      parsesOk "-- comment\ncontract C {}")
  , ("inline comment is ignored",
      parsesOk "contract C { -- comment\n}")
  , ("keyword 'sender' rejected as identifier",
      failsToParse "contract C { state { sender: int = 0; } }")
  , ("keyword 'require' rejected as identifier",
      failsToParse "contract C { transaction require() {} }")
  , ("missing closing brace fails",
      failsToParse "contract C {")
  , ("missing semicolon in state fails",
      failsToParse "contract C { state { x: int = 0 } }")
  , ("SimpleCoin contract parses",
      parsesOk simpleCoinSrc)
  , ("empty contract AST is correct",
      parsesTo "contract Foo {}" (Contract "Foo" [] []))
  , ("state var name is captured correctly",
      case parseContract "<test>" "contract C { state { n: int = 0; } }" of
        Right c -> svName (head (contractState c)) == "n"
        Left  _ -> False)
  , ("transaction name is captured correctly",
      case parseContract "<test>" "contract C { transaction go() {} }" of
        Right c -> txName (head (contractTxs c)) == "go"
        Left  _ -> False)
  ]

vmTests :: [Test]
vmTests =
  [ ("require true: state is updated",
      let call = TxCall "set" "deployer" [("n", VInt 5)]
      in case executeTx vmContract vmState call of
           Right s -> Map.lookup "x" s == Just (VInt 5)
           Left  _ -> False)
  , ("require false: state is reverted",
      let call = TxCall "set" "deployer" [("n", VInt (-1))]
      in case executeTx vmContract vmState call of
           Right s -> Map.lookup "x" s == Just (VInt 0)
           Left  _ -> False)
  , ("unknown transaction returns Left",
      case executeTx vmContract vmState (TxCall "nonexistent" "deployer" []) of
        Left  _ -> True
        Right _ -> False)
  , ("wrong argument count returns Left",
      case executeTx vmContract vmState (TxCall "set" "deployer" []) of
        Left  _ -> True
        Right _ -> False)
  ]

ledgerTests :: [Test]
ledgerTests =
  [ ("genesis block is created",
      length (chainBlocks coinChain) == 1)
  , ("genesis block has no parent",
      blockParentId (head (chainBlocks coinChain)) == Nothing)
  , ("initial balances are zero",
      getBalance (currentState coinChain) "alice" == 0)
  , ("wrong parent block is rejected",
      case submitBlock coinChain 99 [] of
        Left  _ -> True
        Right _ -> False)
  , ("correct parent block is accepted",
      case submitBlock coinChain 0 [] of
        Right _ -> True
        Left  _ -> False)
  , ("mint adds balance",
      case submitBlock coinChain 0 [mintTx "alice" 100] of
        Right chain' -> getBalance (currentState chain') "alice" == 100
        Left  _      -> False)
  , ("non-owner mint is reverted",
      case submitBlock coinChain 0 [TxCall "mint" "bob" [("to", VAddress "bob"), ("amount", VInt 100)]] of
        Right chain' -> getBalance (currentState chain') "bob" == 0
        Left  _      -> False)
  , ("transfer moves balance",
      case do chain1 <- submitBlock coinChain 0 [mintTx "alice" 100]
              submitBlock chain1 1 [transferTx "alice" "bob" 30] of
        Right chain' ->
          getBalance (currentState chain') "alice" == 70 &&
          getBalance (currentState chain') "bob"   == 30
        Left _ -> False)
  , ("double-spend is reverted",
      case do chain1 <- submitBlock coinChain 0 [mintTx "alice" 100]
              submitBlock chain1 1 [transferTx "alice" "bob" 200] of
        Right chain' ->
          getBalance (currentState chain') "alice" == 100 &&
          getBalance (currentState chain') "bob"   == 0
        Left _ -> False)
  , ("chain grows by one block per submission",
      case submitBlock coinChain 0 [] of
        Right chain' -> length (chainBlocks chain') == 2
        Left  _      -> False)
  , ("total supply is preserved across transfers",
      case do chain1 <- submitBlock coinChain 0 [mintTx "alice" 100]
              chain2 <- submitBlock chain1 1 [transferTx "alice" "bob"     40]
              chain3 <- submitBlock chain2 2 [transferTx "bob"   "charlie" 15]
              submitBlock chain3 3          [transferTx "alice" "charlie"  10] of
        Right chain' -> sumBalances (currentState chain') == 100
        Left  _      -> False)
  ]

main :: IO ()
main = runTests (parserTests ++ vmTests ++ ledgerTests)
