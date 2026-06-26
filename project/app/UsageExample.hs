module Main where

import AST
import Parser   (parseContract)
import Eval     (State, TxCall(..), initState)
import Ledger   (Chain(..), newChain, submitBlock, currentState)

import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

defaultContractPath :: FilePath
defaultContractPath = "contracts/SimpleCoin.contract"

banner :: String -> IO ()
banner msg = do
  putStrLn ""
  putStrLn ("=== " ++ msg ++ " ===")

step :: Int -> String -> IO ()
step n msg = putStrLn ("\n[Step " ++ show n ++ "] " ++ msg)

ok :: String -> IO ()
ok msg = putStrLn ("    OK  " ++ msg)

info :: String -> IO ()
info msg = putStrLn ("        " ++ msg)

bad :: String -> IO ()
bad msg = putStrLn ("    !!  " ++ msg)

printBalances :: State -> IO ()
printBalances state =
  case Map.lookup "balances" state of
    Just (VMap pairs) ->
      mapM_ (\(VAddress addr, VInt n) ->
        info (addr ++ ": " ++ show n ++ " coins")) pairs
    _ -> info "(no balances)"

getBalance :: State -> String -> Int
getBalance state addr =
  case Map.lookup "balances" state of
    Just (VMap pairs) ->
      case lookup (VAddress addr) pairs of
        Just (VInt n) -> n
        _             -> 0
    _ -> 0

sumBalances :: State -> Int
sumBalances state =
  case Map.lookup "balances" state of
    Just (VMap pairs) -> sum [n | (_, VInt n) <- pairs]
    _                 -> 0


-- DEMO

main :: IO ()
main = do
  args <- getArgs
  let path = case args of
        (p : _) -> p
        []      -> defaultContractPath

  contractSource <- readFile path

  banner "BlockChainLang — Live Demo"

  step 1 ("Show the contract source (" ++ path ++ ")")

  putStrLn ""
  mapM_ (\line -> putStrLn ("    " ++ line)) (lines contractSource)

  step 2 "Parse the contract"

  let parseResult = parseContract path contractSource
  case parseResult of
    Left  err      -> do
      bad "Parse failed:"
      print err
      return ()
    Right contract -> do
      ok ("Parsed contract '" ++ contractName contract ++ "'")
      info (show (length (contractState contract)) ++ " state variables: "
            ++ unwords (map svName (contractState contract)))
      info (show (length (contractTxs contract)) ++ " transactions: "
            ++ unwords (map txName (contractTxs contract)))

      step 3 "Deploy (deployer = \"alice\")"

      let chain0 = newChain contract "alice"
      ok ("Genesis block created  (block id = 0)")
      info ("balances = {}  |  owner = alice")

      step 4 "Block 1 — alice mints 100 coins to herself"

      let mintCall = TxCall "mint" "alice"
                       [("to", VAddress "alice"), ("amount", VInt 100)]
      case submitBlock chain0 0 [mintCall] of
        Left  err    -> bad ("Rejected: " ++ err)
        Right chain1 -> do
          ok "Block 1 committed"
          printBalances (currentState chain1)

          step 5 "Block 2 — alice transfers 30 coins to bob"

          let xferCall = TxCall "transfer" "alice"
                           [("to", VAddress "bob"), ("amount", VInt 30)]
          case submitBlock chain1 1 [xferCall] of
            Left  err    -> bad ("Rejected: " ++ err)
            Right chain2 -> do
              ok "Block 2 committed"
              printBalances (currentState chain2)

              step 6 "Block 3 — alice tries to transfer 999 coins (more than she has)"

              let badXfer = TxCall "transfer" "alice"
                              [("to", VAddress "bob"), ("amount", VInt 999)]
              case submitBlock chain2 2 [badXfer] of
                Left  err    -> bad ("Block rejected: " ++ err)
                Right chain3 -> do
                  ok "Block 3 committed — but the transaction was silently reverted"
                  info "require (balances[sender] >= 999) failed — state rolled back"
                  printBalances (currentState chain3)

                  step 7 "Block 4 — bob tries to mint coins (he is not the owner)"

                  let bobMint = TxCall "mint" "bob"
                                  [("to", VAddress "bob"), ("amount", VInt 1000)]
                  case submitBlock chain3 3 [bobMint] of
                    Left  err    -> bad ("Block rejected: " ++ err)
                    Right chain4 -> do
                      ok "Block 4 committed — mint transaction silently reverted"
                      info "require (sender == owner) failed — bob is not alice"
                      printBalances (currentState chain4)

                      step 8 "Trying to submit a block with a wrong parent id"

                      case submitBlock chain4 0 [] of
                        Left  err -> bad ("Rejected: " ++ err)
                        Right _   -> bad "Should have been rejected!"

                      step 9 "Chain summary"

                      let final = currentState chain4
                      ok (show (length (chainBlocks chain4)) ++ " blocks in the chain")
                      ok ("Total coin supply: " ++ show (sumBalances final)
                          ++ " (minted 100, none created or destroyed by transfers)")

  banner "Demo complete"
