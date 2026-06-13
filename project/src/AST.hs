module AST where

data Value
  = VInt     Int
  | VBool    Bool
  | VAddress String
  | VMap     [(Value, Value)]
  | VUnit
  deriving (Show, Eq)

data Type
  = TInt
  | TBool
  | TAddress
  | TMap Type Type
  deriving (Show, Eq)

data Op
  -- Arithmetic
  = Add
  | Sub
  | Mul
  | Div

  -- Comparison  
  | Eq    --  left == right
  | Neq   --  left != right
  | Lt    --  left <  right
  | Lte   --  left <= right
  | Gt    --  left >  right
  | Gte   --  left >= right

  -- Logical
  | And
  | Or
  deriving (Show, Eq)

data Expr
  = Var    String
  | Lit    Value
  | BinOp  Op Expr Expr
  | Index  Expr Expr
  | Sender
  | Not    Expr
  deriving (Show, Eq)

data Statement
  = Assign  Expr Expr
  | Require Expr
  | If Expr [Statement] [Statement]
  deriving (Show, Eq)

data TransactionDef = TransactionDef
  { txName    :: String
  , txParams  :: [(String, Type)]
  , txBody    :: [Statement]
  }
  deriving (Show, Eq)

data StateVar = StateVar
  { svName :: String
  , svType :: Type
  , svInit :: Expr
  }
  deriving (Show, Eq)

data Contract = Contract
  { contractName :: String
  , contractState :: [StateVar]
  , contractTxs   :: [TransactionDef]
  }
  deriving (Show, Eq)
