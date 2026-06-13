module Main where

import AST
import Parser (parseContract)

import System.Exit (exitFailure, exitSuccess)

type Test = (String, Bool)

runTests :: [Test] -> IO ()
runTests tests = do
  mapM_ printOne tests
  let failed = length (filter (not . snd) tests)
  putStrLn "---"
  putStrLn (show (length tests) ++ " tests, " ++ show failed ++ " failed")
  if failed > 0
    then exitFailure
    else exitSuccess
  where
    printOne (name, True)  = putStrLn ("[PASS] " ++ name)
    printOne (name, False) = putStrLn ("[FAIL] " ++ name)

parsesOk :: String -> Bool
parsesOk src = case parseContract "<test>" src of
  Right _ -> True
  Left  _ -> False

failsToParse :: String -> Bool
failsToParse src = not (parsesOk src)

parsesTo :: String -> Contract -> Bool
parsesTo src expected = case parseContract "<test>" src of
  Right contract -> contract == expected
  Left  _        -> False


parserTests :: [Test]
parserTests =
  [
    ( "empty contract parses"
    , parsesTo
        "contract Empty {}"
        (Contract "Empty" [] [])
    )
  , ( "state var: int type"
    , parsesOk "contract C { state { x: int = 0; } }"
    )
  , ( "state var: bool type"
    , parsesOk "contract C { state { flag: bool = false; } }"
    )
  , ( "state var: address type"
    , parsesOk "contract C { state { owner: address = sender; } }"
    )
  , ( "state var: map type"
    , parsesOk "contract C { state { balances: map<address, int> = empty; } }"
    )
  , ( "init with integer literal"
    , parsesOk "contract C { state { n: int = 42; } }"
    )
  , ( "init with sender"
    , parsesOk "contract C { state { owner: address = sender; } }"
    )
  , ( "init with empty (empty map)"
    , parsesOk "contract C { state { m: map<address, int> = empty; } }"
    )
  , ( "init with true"
    , parsesOk "contract C { state { b: bool = true; } }"
    )
  , ( "init with address literal"
    , parsesOk "contract C { state { owner: address = \"alice\"; } }"
    )
  , ( "arithmetic expression"
    , parsesOk "contract C { transaction t() { x := 1 + 2; } }"
    )
  , ( "comparison expression"
    , parsesOk "contract C { transaction t() { require x >= 0; } }"
    )
  , ( "map index"
    , parsesOk "contract C { transaction t() { require balances[sender] >= 0; } }"
    )
  , ( "nested map index"
    , parsesOk "contract C { transaction t() { require a[b][c] >= 0; } }"
    )
  , ( "logical and"
    , parsesOk "contract C { transaction t() { require x > 0 && y > 0; } }"
    )
  , ( "not expression"
    , parsesOk "contract C { transaction t() { require not flag; } }"
    )
  , ( "parenthesised expression"
    , parsesOk "contract C { transaction t() { require (x + y) > 0; } }"
    )
  , ( "require statement"
    , parsesOk "contract C { transaction t() { require x > 0; } }"
    )
  , ( "assign to variable"
    , parsesOk "contract C { transaction t() { x := 5; } }"
    )
  , ( "assign to map index"
    , parsesOk "contract C { transaction t() { balances[sender] := 0; } }"
    )
  , ( "if without else"
    , parsesOk "contract C { transaction t() { if x > 0 { y := 1; } } }"
    )
  , ( "if with else"
    , parsesOk "contract C { transaction t() { if x > 0 { y := 1; } else { y := 0; } } }"
    )
  , ( "transaction with no parameters"
    , parsesOk "contract C { transaction reset() { x := 0; } }"
    )
  , ( "transaction with one parameter"
    , parsesOk "contract C { transaction set(n: int) { x := n; } }"
    )
  , ( "transaction with two parameters"
    , parsesOk "contract C { transaction transfer(to: address, amount: int) { require amount > 0; } }"
    )
  , ( "line comment is ignored"
    , parsesOk "-- this is a comment\ncontract C {}"
    )
  , ( "inline comment is ignored"
    , parsesOk "contract C { -- comment here\n}"
    )
  , ( "keyword 'sender' rejected as identifier"
    , failsToParse "contract C { state { sender: int = 0; } }"
    )
  , ( "keyword 'require' rejected as identifier"
    , failsToParse "contract C { transaction require() {} }"
    )
  , ( "missing closing brace fails"
    , failsToParse "contract C {"
    )
  , ( "missing semicolon in state fails"
    , failsToParse "contract C { state { x: int = 0 } }"
    )
  , ( "SimpleCoin contract parses"
    , parsesOk simpleCoinSrc
    )
  , ( "empty contract AST is correct"
    , parsesTo "contract Foo {}" (Contract "Foo" [] [])
    )
  , ( "state var name is captured correctly"
    , case parseContract "<test>" "contract C { state { n: int = 0; } }" of
        Right c -> svName (head (contractState c)) == "n"
        Left  _ -> False
    )
  , ( "transaction name is captured correctly"
    , case parseContract "<test>" "contract C { transaction go() {} }" of
        Right c -> txName (head (contractTxs c)) == "go"
        Left  _ -> False
    )
  ]

simpleCoinSrc :: String
simpleCoinSrc = unlines
  [ "contract SimpleCoin {"
  , "  state {"
  , "    balances: map<address, int> = empty;"
  , "    owner:    address           = sender;"
  , "  }"
  , "  transaction transfer(to: address, amount: int) {"
  , "    require balances[sender] >= amount;"
  , "    balances[sender] := balances[sender] - amount;"
  , "    balances[to]     := balances[to] + amount;"
  , "  }"
  , "}"
  ]

main :: IO ()
main = runTests parserTests
