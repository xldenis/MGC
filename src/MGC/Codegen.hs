{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MGC.Codegen where

  import Data.Word
  import Data.List
  import Data.Function
  import qualified Data.Map as Map

  import Prelude hiding (mod, div, not, and)

  import Control.Monad.State
  import Control.Applicative (Applicative)

  import LLVM.General.AST
  import LLVM.General.AST.Global as G

  import qualified LLVM.General.AST.AddrSpace as AS
  import qualified LLVM.General.AST.CallingConvention as CC
  import qualified LLVM.General.AST.Constant as C
  import qualified LLVM.General.AST.Attribute as A
  import qualified LLVM.General.AST.FloatingPointPredicate as FP
  import qualified LLVM.General.AST.IntegerPredicate as IP
  import qualified LLVM.General.AST.Type as T
  import qualified LLVM.General.AST.Linkage as L

  import qualified MGC.Syntax as S
  import qualified MGC.Check  as CK

  -- Codegen Data Types

  data CodegenState
    = CodegenState {
      currentBlock :: Name                     -- Name of the active block to append to
    , blocks       :: Map.Map Name BlockState  -- Blocks for function
    , assigned     :: Map.Map Name Operand     -- Assigned var addrs
    , blockCount   :: Int                      -- Count of basic blocks
    , count        :: Word                     -- Count of unnamed instructions
    , names        :: Map.Map String Int       -- Redeclarations of name
    , types        :: [S.TypeSpec S.Ann]       -- Type declarations to propagate upwards
    , constants    :: [(Name, Type, C.Constant)] -- Constants to propagate up
    , consOffset   :: Int                      -- Base constant number
    , nextBlock    :: Maybe Name               -- Block to Fallthrough to in switches
    , loopBlock    :: Maybe (Name, Name)       -- Blocks to jump to in loop
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

  define :: Type -> String -> [(Type, Name)] -> L.Linkage -> [BasicBlock] -> LLVM ()
  define retty label argtys linkage body = addDefn $
    GlobalDefinition $ functionDefaults {
    name = Name label
    , linkage    =  linkage
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

  constant :: Name -> Type -> C.Constant -> LLVM ()
  constant nm tp cons =  addDefn $
    GlobalDefinition $ globalVariableDefaults {
    name = nm
    , G.type' = tp
    , isConstant = True
    , initializer = Just cons

    }

  global :: Name -> Type -> C.Constant -> LLVM ()
  global nm tp cons =  addDefn $
    GlobalDefinition $ globalVariableDefaults {
    name = nm
    , G.type' = tp
    , isConstant = False
    , initializer = Just cons

    }
  typedef :: Name -> Type -> LLVM ()
  typedef nm tp = addDefn $ TypeDefinition nm (Just tp)

  -- Codegen helpers

  fresh :: Codegen Word
  fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + i }
    return $ i + 1

  uniqueName :: String -> Map.Map String Int -> (String, Map.Map String Int)
  uniqueName nm nms =
    case Map.lookup nm nms of
      Nothing -> (nm,  Map.insert nm 1 nms)
      Just ix -> (nm ++ show ix, Map.insert nm (ix+1) nms)

  assign :: Name -> Operand -> Codegen ()
  assign nm op = do
    lcls <- gets assigned
    modify $ \st -> st { assigned = Map.insert nm op lcls }

  getvar :: Name -> Codegen Operand
  getvar v = do
    lcls <- gets assigned
    case Map.lookup v lcls of
      Just x -> return x
      Nothing -> return $ externf T.void v -- refactor

  addConstant :: Type -> C.Constant -> Codegen Name
  addConstant tp c = do
    i <- gets consOffset
    modify $ \s -> s { consOffset = 1 + i, constants = (name i, tp, c) : constants s}
    return $ name i
    where name a =  Name $ "cons." ++ show a
  entry :: Codegen Name
  entry = gets currentBlock

  emptyBlock :: Int -> BlockState
  emptyBlock i = BlockState i [] Nothing

  entryBlockName :: String
  entryBlockName = "entry"

  emptyCodegen :: CodegenState
  emptyCodegen = CodegenState (Name entryBlockName) Map.empty Map.empty 1 0 Map.empty [] [] 0 Nothing Nothing

  createBlocks :: CodegenState -> [BasicBlock]
  createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

  sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
  sortBlocks = sortBy (compare `on` (idx . snd))

  makeBlock :: (Name, BlockState) -> BasicBlock
  makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
    where
      maketerm (Just x) = x
      maketerm Nothing = error $ "Block has no terminator: " ++ show l -- change!!!!!

  addBlock :: String -> Codegen Name
  addBlock bname = do
    bls <- gets blocks
    ix  <- gets blockCount
    nms <- gets names
    let new = emptyBlock ix
        (qname, supply) = uniqueName bname nms
    modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                     , blockCount = ix + 1
                     , names = supply
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

  fieldIdx :: S.Type -> String -> Maybe Int
  fieldIdx (S.Struct l) s = fieldIdx' l s 0
  fieldIdx _ _ = Nothing

  fieldIdx' (S.NamedField ids _ _:ls) s c = case findIndex ((== s)) ids of
    Just i  -> Just $ c + i
    Nothing -> fieldIdx' ls s (c + length ids)
  fieldIdx' (_:ls) s c = fieldIdx' ls s (c+1)
  fieldIdx' _ _ _ = Nothing -- not possible after typechecking

  toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
  toArgs = map (\x -> (x, []))

  instr :: Type -> Instruction -> Codegen Operand
  instr tp ins = do
    n   <- fresh
    blk <- current
    let i = stack blk
    let ref = UnName n
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
  call tp fn args = instr tp $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

  alloca :: Type -> Codegen Operand
  alloca ty = instr (ptr ty) $ Alloca ty Nothing 0 []

  store :: Type -> Operand -> Operand -> Codegen Operand
  store tp ptr val = instr tp $ Store False ptr val Nothing 0 []

  load :: Type -> Operand -> Codegen Operand
  load tp ptr = instr tp $ Load False ptr Nothing 0 []

  -- Types

  double :: Type
  double = FloatingPointType 64 IEEE

  int :: Type
  int = T.i64

  i32 :: Type
  i32 = T.i32

  bool :: Type
  bool = T.i1

  char :: Type
  char = T.i8

  sizeof :: Type -> Operand
  sizeof t = cons $ C.GetElementPtr False (C.Null . ptr $ t) [C.Int  64 1]

  -- llslice = T.StructureType False [T.i32, T.i32, T.i32, T.PointerType T.i8 (AS.AddrSpace 0)]
  llslice = T.NamedTypeReference $ Name "slice"
  llsliceptr = T.PointerType (lltype $ S.TypeName "slice") (AS.AddrSpace 0)
  llnewslice = T.FunctionType llsliceptr [T.i32, T.i32, T.i32] False

  lltype :: S.Type -> Type
  lltype S.TInteger = int
  lltype S.TFloat = double
  lltype (S.Struct flds) = T.StructureType False (map (lltype . decType) flds)
  lltype (S.ReturnType tps) = case tps of
    [] -> T.void
    _  -> lltype (head tps)
  lltype (S.Array l tp) = T.ArrayType (fromIntegral l) (lltype tp)
  lltype (S.Function s) = T.FunctionType (retty s) (argty s) False
    where retty (S.Signature _ ret) =  (\(S.Parameter _ t) -> lltype t) $ head ret
          argty (S.Signature arg _) = concatMap (\(S.Parameter ids t) -> map (\i -> lltype t) ids) arg
  lltype (S.TypeName n) = T.NamedTypeReference (Name n)
  lltype (S.Slice tp)   = T.NamedTypeReference (Name "slice")
  lltype S.TString   = T.NamedTypeReference (Name "slice")
  lltype S.TNil       = T.void
  lltype S.TBool      = T.i1
  ptr :: Type -> Type
  ptr t = T.PointerType t (AS.AddrSpace 0)

  decType :: S.FieldDecl -> S.Type
  decType (S.AnonField    tp _) = tp
  decType (S.NamedField _ tp _) = tp

  -- Constants
  maxInt = cons $ C.Int 64 9223372036854775807

  one  = cons $ C.Int  64 1
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

  retvoid :: Codegen (Named Terminator)
  retvoid = terminator $ Do $ Ret Nothing []

  add :: S.Type -> Operand -> Operand -> Codegen Operand
  add tp a b = case tp of
    S.TInteger -> instr int $ Add False False a b []
    S.TFloat   -> instr double $ FAdd NoFastMathFlags a b []
    S.TString  -> call llslice (externf stringadd (Name "add_string")) [a, b]
      where stringadd = T.FunctionType llslice [llslice, llslice] False
    _ -> error "Cannot generate code for +"

  sub :: S.Type -> Operand -> Operand -> Codegen Operand
  sub tp a b = case tp of
    S.TInteger -> instr int $ Sub False False a b []
    S.TFloat   -> instr double $ FSub NoFastMathFlags a b []
    _ -> error "Cannot generate code for -"

  mul :: S.Type -> Operand -> Operand -> Codegen Operand
  mul tp a b = case tp of
    S.TInteger -> imul int a b
    S.TFloat   -> instr double $ FMul NoFastMathFlags a b []
    _ -> error "Cannot generate code for *"

  imul :: Type -> Operand -> Operand -> Codegen Operand
  imul t a b = instr t $ Mul False False a b []

  div :: S.Type -> Operand -> Operand -> Codegen Operand
  div tp a b = case tp of
    S.TInteger -> instr int $ SDiv False a b []
    S.TFloat   -> instr double $ FDiv NoFastMathFlags a b []
    _ -> error "Cannot generate code for /"

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

  bclear :: Operand -> Operand -> Codegen Operand
  bclear a b = not b >>= band a

  bcomp :: Operand -> Codegen Operand
  bcomp a = xor a maxInt

  fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
  fcmp cond a b = instr bool $ FCmp cond a b []

  icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
  icmp cond a b = instr bool $ ICmp cond a b []

  eq :: S.Type -> Operand -> Operand -> Codegen Operand
  eq S.TInteger = icmp IP.EQ
  eq S.TFloat   = fcmp FP.OEQ
  eq _          = error "Cannot generate code for =="

  neq :: S.Type -> Operand -> Operand -> Codegen Operand
  neq S.TInteger = icmp IP.EQ
  neq S.TFloat   = fcmp FP.OEQ
  neq _          = error "Cannot generate code for !="

  gt :: S.Type -> Operand -> Operand -> Codegen Operand
  gt S.TInteger = icmp IP.SGT
  gt S.TFloat   = fcmp FP.OGT
  gt _          = error "Cannot generate code for >"

  lt :: S.Type -> Operand -> Operand -> Codegen Operand
  lt S.TInteger = icmp IP.SLT
  lt S.TFloat   = fcmp FP.OLT
  lt _          = error "Cannot generate code for <"

  geq :: S.Type -> Operand -> Operand -> Codegen Operand
  geq S.TInteger = icmp IP.SGT
  geq S.TFloat   = fcmp FP.OGE
  geq _          = error "Cannot generate code for >="

  leq :: S.Type -> Operand -> Operand -> Codegen Operand
  leq S.TInteger = icmp IP.SLE
  leq S.TFloat   = fcmp FP.OLE
  leq _          = error "Cannot generate code for <="

  sitofp :: Operand -> Codegen Operand
  sitofp a = instr double $ SIToFP a int []

  fptosi :: Operand -> Codegen Operand
  fptosi a = instr int $ FPToSI a int []

  uitofp :: Operand -> Codegen Operand
  uitofp a = instr double $ UIToFP a double []

  fptoui :: Operand -> Codegen Operand
  fptoui a = instr int $ FPToUI a double []

  bitcast :: Operand -> Type -> Codegen Operand
  bitcast o t = instr t $ BitCast o t []

  trunc :: Operand -> Type -> Codegen Operand
  trunc o t = instr t $ Trunc o t []

  sext :: Operand -> Type -> Codegen Operand
  sext o t = instr t $ SExt o t []

  gep :: Type -> Operand -> [Operand] -> Codegen Operand
  gep t a i = instr t $ GetElementPtr False a i []

  ckbnds :: S.Type -> Operand -> Operand -> Codegen ()
  ckbnds a s i  = do
    len <- case a of
      S.Slice _ -> gep T.i32 s [zero, zero]
      S.Array l _ -> return $ cons $ C.Int 32 (toInteger l)
    bt <- addBlock "check.true"
    bf <- addBlock "check.false"

    test <- lt S.TInteger i len
    cbr test bt bf
    setBlock bf
    call T.void (externf T.void $ Name "llvm.trap") []
    retvoid
    setBlock bt

    return ()
