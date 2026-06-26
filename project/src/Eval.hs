module Eval
  ( State
  , TxCall(..)
  , initState
  , executeTx
  ) where

import AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type State = Map String Value

data TxCall = TxCall
  { tcName   :: String
  , tcSender :: String
  , tcArgs   :: [(String, Value)]
  } deriving (Show, Eq)

initState :: Contract -> String -> State
initState contract deployer =
  Map.fromList
    [ (svName v, eval (svInit v))
    | v <- contractState contract
    ]
  where
    eval expr = case evalExpr Map.empty deployer [] expr of
      Right val -> val
      Left  _   -> VUnit

evalExpr :: State -> String -> [(String, Value)] -> Expr -> Either String Value
evalExpr _     sender _    Sender          = Right (VAddress sender)
evalExpr _     _      _    (Lit v)         = Right v
evalExpr state _      args (Var name)      =
  case lookup name args of
    Just v  -> Right v
    Nothing -> case Map.lookup name state of
      Just v  -> Right v
      Nothing -> Left ("undefined variable: " ++ name)
evalExpr state sender args (Not e)         = do
  v <- evalExpr state sender args e
  case v of
    VBool b -> Right (VBool (not b))
    _       -> Left "not: non-boolean operand"
evalExpr state sender args (BinOp op l r)  = do
  lv <- evalExpr state sender args l
  rv <- evalExpr state sender args r
  applyOp op lv rv
evalExpr state sender args (Index me ke)   = do
  mv <- evalExpr state sender args me
  kv <- evalExpr state sender args ke
  case mv of
    VMap pairs -> Right (maybe (VInt 0) id (lookup kv pairs))
    _          -> Left "index on non-map value"

applyOp :: Op -> Value -> Value -> Either String Value
applyOp Add (VInt a)  (VInt b)  = Right (VInt (a + b))
applyOp Sub (VInt a)  (VInt b)  = Right (VInt (a - b))
applyOp Mul (VInt a)  (VInt b)  = Right (VInt (a * b))
applyOp Div (VInt a)  (VInt b)
  | b == 0    = Left "division by zero"
  | otherwise = Right (VInt (a `div` b))
applyOp Eq  a         b         = Right (VBool (a == b))
applyOp Neq a         b         = Right (VBool (a /= b))
applyOp Lt  (VInt a)  (VInt b)  = Right (VBool (a <  b))
applyOp Lte (VInt a)  (VInt b)  = Right (VBool (a <= b))
applyOp Gt  (VInt a)  (VInt b)  = Right (VBool (a >  b))
applyOp Gte (VInt a)  (VInt b)  = Right (VBool (a >= b))
applyOp And (VBool a) (VBool b) = Right (VBool (a && b))
applyOp Or  (VBool a) (VBool b) = Right (VBool (a || b))
applyOp op  _         _         = Left ("type error in operator " ++ show op)

execStatements :: State -> String -> [(String, Value)] -> [Statement] -> Either String State
execStatements state _      _    []     = Right state
execStatements state sender args (s:ss) = do
  state' <- execStatement state sender args s
  execStatements state' sender args ss

execStatement :: State -> String -> [(String, Value)] -> Statement -> Either String State
execStatement state sender args (Require cond) = do
  v <- evalExpr state sender args cond
  case v of
    VBool True  -> Right state
    VBool False -> Left "require failed"
    _           -> Left "require: non-boolean condition"
execStatement state sender args (Assign lhs rhs) = do
  val <- evalExpr state sender args rhs
  setLhs state sender args lhs val
execStatement state sender args (If cond thenB elseB) = do
  v <- evalExpr state sender args cond
  case v of
    VBool True  -> execStatements state sender args thenB
    VBool False -> execStatements state sender args elseB
    _           -> Left "if: non-boolean condition"

setLhs :: State -> String -> [(String, Value)] -> Expr -> Value -> Either String State
setLhs state _ _ (Var name) val =
  Right (Map.insert name val state)
setLhs state sender args (Index (Var mapName) keyExpr) val = do
  kv <- evalExpr state sender args keyExpr
  let old = Map.findWithDefault (VMap []) mapName state
  case old of
    VMap pairs -> Right (Map.insert mapName (VMap (upsert kv val pairs)) state)
    _          -> Left ("not a map: " ++ mapName)
setLhs _ _ _ _ _ = Left "invalid assignment target"

upsert :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
upsert k v []               = [(k, v)]
upsert k v ((k0, v0) : rest)
  | k == k0   = (k, v) : rest
  | otherwise = (k0, v0) : upsert k v rest

executeTx :: Contract -> State -> TxCall -> Either String State
executeTx contract state call = do
  txDef <- lookupTx (tcName call) (contractTxs contract)
  let formals = map fst (txParams txDef)
  let actuals = map snd (tcArgs call)
  if length formals /= length actuals
    then Left ("wrong argument count for '" ++ tcName call ++ "'")
    else
      let args = zip formals actuals
      in Right (case execStatements state (tcSender call) args (txBody txDef) of
                  Right s -> s
                  Left  _ -> state)

lookupTx :: String -> [TransactionDef] -> Either String TransactionDef
lookupTx name []     = Left ("unknown transaction: '" ++ name ++ "'")
lookupTx name (t:ts)
  | txName t == name = Right t
  | otherwise        = lookupTx name ts
