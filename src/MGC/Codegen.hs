{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MGC.Codegen where

  import Data.Word
  import Data.List
  import Data.Function
  import qualified Data.Map as Map

  import Control.Monad.State
  import Control.Applicative (Applicative)

  import LLVM.General.AST
  import LLVM.General.AST.Global
  import LLVM.General.AST.Operand

  import qualified LLVM.General.AST.CallingConvention as CC
  import qualified LLVM.General.AST.Constant as C
  import qualified LLVM.General.AST.Attribute as A
  import qualified LLVM.General.AST.FloatingPointPredicate as FP
  import qualified LLVM.General.AST.IntegerPredicate as IP
  import qualified LLVM.General.AST.Type as T

  import qualified MGC.Syntax as S

  data CodegenState
    = CodegenState {
      currentBlock :: Name                     -- Name of the active block to append to
    , blocks       :: Map.Map Name BlockState  -- Blocks for function
    , assigned     :: Map.Map Name Operand   -- Assigned var addrs                     
    , blockCount   :: Int                      -- Count of basic blocks
    , count        :: Word                     -- Count of unnamed instructions
    } deriving Show

  data BlockState
    = BlockState {
      idx   :: Int                            -- Block index
    , stack :: [Named Instruction]            -- Stack of instructions
    , term  :: Maybe (Named Terminator)       -- Block terminator
    } deriving Show

  newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState )

  newtype LLVM a = LLVM { unLLVM :: State Module a }
    deriving (Functor, Applicative, Monad, MonadState Module )

  runLLVM :: Module -> LLVM a -> Module
  runLLVM = flip (execState . unLLVM)

  emptyModule :: String -> Module
  emptyModule label = defaultModule { moduleName = label }

  addDefn :: Definition -> LLVM ()
  addDefn d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = defs ++ [d] }

  define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
  define retty label argtys body = addDefn $
    GlobalDefinition $ functionDefaults {
    name = Name label
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType = retty
    , basicBlocks = body
    }

  external :: Type -> String -> [(Type, Name)] -> LLVM ()
  external retty label argtys = addDefn $
    GlobalDefinition $ functionDefaults {
    name        = Name label
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = []
    }

  -- Codegen helpers

  fresh :: Codegen Word
  fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + i }
    return $ i + 1 

  uniqueName :: String -> Codegen String
  uniqueName nm = do -- fix entry block name
    idx <- fresh
    return $ nm ++ (show idx)

  assign :: Name -> Operand -> Codegen ()
  assign nm op = do
    lcls <- gets assigned
    modify $ \st -> st { assigned = Map.insert nm op lcls }

  getvar :: Name -> Codegen Operand
  getvar v = do
    lcls <- gets assigned
    case Map.lookup v lcls of
      Just x -> return x
      Nothing -> error $ "not possible" -- refactor

  entry :: Codegen Name
  entry = gets currentBlock

  emptyBlock :: Int -> BlockState
  emptyBlock i = BlockState i [] Nothing
  
  entryBlockName :: String
  entryBlockName = "entry"

  emptyCodegen :: CodegenState
  emptyCodegen = CodegenState (Name entryBlockName) Map.empty Map.empty 1 0

  createBlocks :: CodegenState -> [BasicBlock]
  createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

  sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
  sortBlocks = sortBy (compare `on` (idx . snd))

  makeBlock :: (Name, BlockState) -> BasicBlock
  makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
    where
      maketerm (Just x) = x
      maketerm Nothing = error $ "Block has no terminator: " ++ (show l) -- change!!!!!

  addBlock :: String -> Codegen Name
  addBlock bname = do
    bls <- gets blocks
    ix  <- gets blockCount
    qname <- uniqueName bname

    let new = emptyBlock ix

    modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                     , blockCount = ix + 1
                     }
    return (Name qname)

  setBlock :: Name -> Codegen Name
  setBlock bname = do
    modify $ \s -> s { currentBlock = bname }
    return bname

  getBlock :: Codegen Name
  getBlock = gets currentBlock

  modifyBlock :: BlockState -> Codegen ()
  modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

  current :: Codegen BlockState
  current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
      Just x -> return x
      Nothing -> error $ "No such block: " ++ show c  

  execCodegen :: Codegen a -> CodegenState
  execCodegen m = execState (runCodegen m) emptyCodegen

  local ::  Type -> Name -> Operand
  local = LocalReference

  externf :: Type -> Name -> Operand
  externf tp nm = ConstantOperand $ C.GlobalReference tp nm

  -- Helper methods for LLVM node types

  toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
  toArgs = map (\x -> (x, []))

  instr :: Type -> Instruction -> Codegen Operand
  instr tp ins = do
    n   <- fresh
    blk <- current
    let i = stack blk
    let ref = (UnName n)
    modifyBlock $ blk { stack = i ++ [ref := ins] }
    return $ local tp ref

  terminator :: Named Terminator -> Codegen (Named Terminator)
  terminator trm = do
    blk <- current
    case term blk of 
      Just _  -> return ()
      Nothing -> modifyBlock $ blk { term = Just trm }
    return trm

  -- Side Effects

  call :: Type -> Operand -> [Operand] -> Codegen Operand
  call tp fn args = instr tp $ Call False CC.C [] (Right fn) (toArgs args) [] []

  alloca :: Type -> Codegen Operand
  alloca ty = instr ty $ Alloca ty Nothing 0 []

  store :: Type -> Operand -> Operand -> Codegen Operand
  store tp ptr val = instr tp $ Store False ptr val Nothing 0 []

  load :: Type -> Operand -> Codegen Operand
  load tp ptr = instr tp $ Load False ptr Nothing 0 []

  -- Types

  double :: Type
  double = FloatingPointType 64 IEEE

  int :: Type
  int = T.i64

  bool :: Type
  bool = T.i1

  char :: Type
  char = T.i8  

  lltype :: S.Type -> Type
  lltype S.TInteger = int
  lltype S.TFloat = double
  lltype (S.Struct flds) = T.StructureType False (map (lltype . decType) flds)
  lltype (S.ReturnType tps) = case tps of
    [] -> T.void
    _  -> lltype (head tps)

  decType (S.AnonField    tp _) = tp
  decType (S.NamedField _ tp _) = tp

  -- Constants
  maxInt = cons $ C.Int 64 9223372036854775807

  one = cons  $ C.Int  64 1
  zero = cons $ C.Int 64 0

  false = cons $ C.Int 1 0
  true  = cons $ C.Int 1 1

  -- Instructions

  cons :: C.Constant -> Operand
  cons = ConstantOperand

  br :: Name -> Codegen (Named Terminator)
  br val = terminator $ Do $ Br val []

  cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
  cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

  ret :: Operand -> Codegen (Named Terminator)
  ret val = terminator $ Do $ Ret (Just val) []

  add :: S.Type -> Operand -> Operand -> Codegen Operand
  add tp a b = case tp of
    S.TInteger -> instr int $ Add False False a b []
    S.TFloat   -> instr double $ FAdd NoFastMathFlags a b []

  sub :: S.Type -> Operand -> Operand -> Codegen Operand
  sub tp a b = case tp of
    S.TInteger -> instr int $ Sub False False a b []
    S.TFloat   -> instr double $ FSub NoFastMathFlags a b []

  mul :: S.Type -> Operand -> Operand -> Codegen Operand
  mul tp a b = case tp of
    S.TInteger -> instr int $ Mul False False a b []
    S.TFloat   -> instr double $ FMul NoFastMathFlags a b []

  div :: S.Type -> Operand -> Operand -> Codegen Operand
  div tp a b = case tp of
    S.TInteger -> instr int $ Add False False a b []
    S.TFloat   -> instr double $ FDiv NoFastMathFlags a b []

  mod :: Operand -> Operand -> Codegen Operand
  mod a b = instr int $ SRem a b []

  shl :: Operand -> Operand -> Codegen Operand
  shl a b = instr int $ Shl False False a b []
  
  shr :: Operand -> Operand -> Codegen Operand
  shr a b = instr int $ LShr False a b []

  band :: Operand -> Operand -> Codegen Operand
  band a b = instr int $ And a b []
  
  bor  :: Operand -> Operand -> Codegen Operand
  bor a b = instr int $ Or a b  []

  xor  :: Operand -> Operand -> Codegen Operand
  xor a b = instr int $ Xor a b []

  not :: Operand -> Codegen Operand
  not a = instr bool $ Xor a true []

  bcomp :: Operand -> Codegen Operand
  bcomp a = xor a maxInt

  fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
  fcmp cond a b = instr bool $ FCmp cond a b []

  icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
  icmp cond a b = instr bool $ ICmp cond a b []

  eq :: S.Type -> Operand -> Operand -> Codegen Operand
  eq S.TInteger = icmp IP.EQ 
  eq S.TFloat   = fcmp FP.OEQ

  neq :: S.Type -> Operand -> Operand -> Codegen Operand
  neq S.TInteger = icmp IP.EQ
  neq S.TFloat   = fcmp FP.OEQ

  gt :: S.Type -> Operand -> Operand -> Codegen Operand
  gt S.TInteger = icmp IP.SGT
  gt S.TFloat   = fcmp FP.OGT

  lt :: S.Type -> Operand -> Operand -> Codegen Operand
  lt S.TInteger = icmp IP.SLT
  lt S.TFloat   = fcmp FP.OLT

  geq :: S.Type -> Operand -> Operand -> Codegen Operand
  geq S.TInteger = icmp IP.SGT
  geq S.TFloat   = fcmp FP.OGE

  leq :: S.Type -> Operand -> Operand -> Codegen Operand
  leq S.TInteger = icmp IP.SLE
  leq S.TFloat   = fcmp FP.OLE

  sitofp :: Operand -> Codegen Operand
  sitofp a = instr double $ SIToFP a int []

  fptosi :: Operand -> Codegen Operand
  fptosi a = instr int $ FPToSI a int []

  uitofp :: Operand -> Codegen Operand
  uitofp a = instr double $ UIToFP a double []

  fptoui :: Operand -> Codegen Operand
  fptoui a = instr double $ FPToUI a double []