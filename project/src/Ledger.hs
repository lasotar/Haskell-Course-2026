module Ledger
  ( BlockId
  , Block(..)
  , Chain(..)
  , newChain
  , submitBlock
  , currentState
  ) where

import AST
import Eval (State, TxCall, initState, executeTx)

type BlockId = Int

data Block = Block
  { blockId       :: BlockId
  , blockParentId :: Maybe BlockId
  , blockTxCalls  :: [TxCall]
  } deriving (Show, Eq)

data Chain = Chain
  { chainContract :: Contract
  , chainBlocks   :: [Block]
  , chainState    :: State
  } deriving (Show)

newChain :: Contract -> String -> Chain
newChain contract deployer = Chain
  { chainContract = contract
  , chainBlocks   = [Block { blockId = 0, blockParentId = Nothing, blockTxCalls = [] }]
  , chainState    = initState contract deployer
  }

submitBlock :: Chain -> BlockId -> [TxCall] -> Either String Chain
submitBlock chain parentId txCalls =
  let latestId = blockId (last (chainBlocks chain))
  in if parentId /= latestId
       then Left ("invalid parent block: " ++ show parentId
               ++ ", expected " ++ show latestId)
       else
         let newId    = latestId + 1
             newBlock = Block
               { blockId       = newId
               , blockParentId = Just parentId
               , blockTxCalls  = txCalls
               }
             newState = foldl applyTx (chainState chain) txCalls
             newChain' = Chain
               { chainContract = chainContract chain
               , chainBlocks   = chainBlocks chain ++ [newBlock]
               , chainState    = newState
               }
         in Right newChain'
  where
    applyTx state tx =
      case executeTx (chainContract chain) state tx of
        Right s -> s
        Left  _ -> state

currentState :: Chain -> State
currentState = chainState
