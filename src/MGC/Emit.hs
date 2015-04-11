module MGC.Emit where

import Prelude hiding (mod, div, not)
import Control.Monad
import Control.Monad.State (modify, gets)
import Control.Monad.Except
import Data.List (findIndex, sortBy, intersperse)
import Data.Ord  (Ordering(..))
import Data.Either (rights)
import Data.Data (toConstr)
import Data.Char (toLower, ord)

import MGC.Syntax as S
import MGC.Check (typeOf, annOf, ttOf)
import MGC.Codegen

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.Target as TM
import qualified LLVM.General.AST.Linkage as L


import LLVM.General.PrettyPrint
import LLVM.General.PassManager
import LLVM.General.Context
import LLVM.General.Module

codegenPkg :: Package Ann -> LLVM ()
codegenPkg (Package nm tlds) = do
  modify (\s -> s {AST.moduleName = nm})
  typedef (AST.Name "slice") $ T.StructureType False [T.i32, T.i32, T.i32, ptr $ char]
  define (llslice) "append"    [(llslice, AST.UnName 0), (ptr $ char, AST.UnName 0)] L.External []
  define (llsliceptr) "new_slice" [(T.i32, AST.UnName 0), (T.i32, AST.UnName 0) , (T.i32, AST.UnName 0)] L.External []
  define (llsliceptr) "string_constant" [(ptr $ char, AST.UnName 0), (T.i32, AST.UnName 0)] L.External []
  define (llslice) "add_string" [(llslice, AST.UnName 0), (llslice, AST.UnName 0)] L.External []

  define (T.void) "print.tinteger" [(T.i64, AST.UnName 0)] L.External []
  define (T.void) "print.tstring" [(llslice, AST.UnName 0)] L.External []
  define (T.void) "print.trune" [(T.i8, AST.UnName 0)] L.External []
  mapM codegenTop tlds
  return ()

codegenTop :: TopLevelDeclaration Ann -> LLVM ()
codegenTop (FunctionDecl nm sig Empty) = define (lltype $ retty sig) nm (argty sig) L.External []
codegenTop (FunctionDecl nm sig body) = do
  codegenTop (Decl (TypeDecl $ types cg))
  mapM (\(nm, tp, cons) -> constant nm tp cons ) $ constants cg
  define (lltype $ retty  sig) nm largs linkage blks
  where
    linkage = if nm == "main" then L.External else L.Internal
    largs = argty sig
    blks  = createBlocks $ cg
    cg    = execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM largs $ \(atp, a) -> do
        var <- alloca atp
        store atp var (local atp a)
        assign a var
      codegenStmt body
      retvoid
codegenTop (Decl (TypeDecl ts)) = do
  mapM (\(TypeSpec a n t) -> case truety a of
    TypeName n -> case t of
      Struct _ -> typedef (AST.Name n) (lltype t)
      _ -> return ()) ts
  return ()
codegenTop (Decl (VarDecl s)) = do
  mapM codegenGbl s
  return ()

codegenGbl (VarSpec _ idens exps _) = do
  mapM (\(n,e) -> global (AST.Name n) (lltype $ ttOf e) (gVal e)) $ zip idens exps
-- codegenGbl (VarSpec a idens [] (Just tp)) = do
--   mapM (\n -> global $ (AST.Name n) (lltype $ truety a) (defVal $ truety a)) idens

gVal :: Expression Ann -> C.Constant
gVal (Integer i)   = C.Int 64 (toInteger i)
gVal (Float f)     = C.Float (F.Double f)
gVal (Bool b)      = C.Int 1 (toInteger $ fromEnum b)
gVal (IntString s) = C.Struct (Just $ AST.Name "slice") False [C.Int 32 0, C.Int 32 0, C.Int 32 0, C.Null (ptr $ char)]
gVal (RawString s) = C.Struct (Just $ AST.Name "slice") False [C.Int 32 0, C.Int 32 0, C.Int 32 0, C.Null (ptr $ char)]
gVal (Rune c)      = C.Int 8 (toInteger . ord $ head c)
gVal (Name tp n)   = C.GlobalReference (lltype $ truety tp) $ AST.Name n

defVal :: Type -> C.Constant
defVal (TInteger)  = C.Int 64 0
defVal (TFloat)    = C.Float (F.Double 0.0)
defVal (TString)   = C.Struct (Just $ AST.Name "slice") False [C.Int 32 0, C.Int 32 0, C.Int 32 0, C.Null (ptr $ char)]
defVal (TRune)     = C.Int 8 0
defVal (TBool)     = C.Int 1 0
defVal (Slice tp)  = C.Struct (Just $ AST.Name "slice") False [C.Int 32 0, C.Int 32 0, C.Int 32 0, C.Null (ptr $ char)]
-- defVal (Struct f)  = C.Struct 
defVal (Array l t) = C.Array (lltype t) $ replicate l (defVal t)

retty :: Signature -> Type
retty (Signature _ ret) = ReturnType $ map (\(Parameter _ t) -> t) ret

argty :: Signature -> [(AST.Type, AST.Name)]
argty (Signature arg _) = concatMap (\(Parameter ids t) -> map (\i -> (lltype t, AST.Name i)) ids) arg

codegenStmt :: Statement Ann -> Codegen ()
codegenStmt (Return exp) = (if length exp /= 0 then codegenExpr (head exp) >>= ret else retvoid) >> return ()
codegenStmt (ExpressionStmt e) = codegenExpr e >> return ()
codegenStmt (If s cnd l r) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  codegenStmt s
  cond <- codegenExpr cnd
  cbr cond ifthen ifelse
  setBlock ifthen
  codegenStmt l
  br ifexit
  ifthen <- getBlock
  setBlock ifelse
  codegenStmt r
  br ifexit
  ifelse <- getBlock
  setBlock ifexit
  return ()
codegenStmt (Empty) = return ()
codegenStmt (Inc e) = codegenStmt (Assignment Plus [e] [one])
  where one = if (truety $ annOf e) == TInteger then (Integer 1) else (Float 1.0)
codegenStmt (Dec e) = codegenStmt (Assignment Minus [e] [one])
  where one = if (truety $ annOf e) == TInteger then (Integer 1) else (Float 1.0)
codegenStmt (Block s) = do
  mapM codegenStmt s
  return ()
codegenStmt (For (Just (ForClause s e p)) body) = do
  loopstart <- addBlock "for.start"
  loopbody <- addBlock "for.body"
  loopend <- addBlock "for.end"

  codegenStmt s
  prev <- gets loopBlock
  modify (\s -> s {loopBlock = Just (loopstart, loopend)})
  br loopstart
  setBlock loopstart
  test <- case e of
    Just e -> codegenExpr e
    Nothing -> return true
  cbr  test loopbody loopend
  setBlock loopbody
  codegenStmt body
  codegenStmt p  
  br loopstart
  setBlock loopend
  modify (\s -> s {loopBlock = prev})

  return ()
codegenStmt (For cond body) = do
  loopstart <- addBlock "for.start"
  loopbody <- addBlock "for.body"
  loopend <- addBlock "for.end"
  prev <- gets loopBlock
  modify (\s -> s {loopBlock = Just (loopstart, loopend)})
  -- init vars
  br loopstart
  setBlock loopstart
  test <- case cond of
    Nothing -> return true
    Just (Condition e) -> codegenExpr e
  cbr test loopbody loopend

  setBlock loopbody
  codegenStmt body
  br loopstart

  setBlock loopend
  modify (\s -> s {loopBlock = prev})

  return ()
codegenStmt (VarDecl specs) = mapM codegenSpec specs >> return ()
codegenStmt (ShortDecl idens exps) = do
  mapM (\(n, e) -> do
    let tp = lltype $ ttOf e
    i <- alloca tp
    val <- codegenExpr e 
    store tp i val
    assign (AST.Name n) i) (zip idens exps) >> return ()
codegenStmt (Assignment Eq lh rh) = do
  lhs <- mapM getaddr lh
  rhs <- mapM  codegenExpr rh
  let lt = map ttOf lh
  mapM (\(t,l,r) -> store (lltype $ t) l r) $ zip3 lt lhs rhs
  return ()
codegenStmt (Assignment op lh rh) = do
  addr <- mapM getaddr lh
  updated <- mapM (\(a,b) -> codegenExpr $ (BinaryOp (annOf a) op a b)) (zip lh rh)
  mapM (\(t, a, v) -> store (lltype $ t) a v) $ zip3 (map ttOf lh) addr updated
  return ()
codegenStmt (TypeDecl ts) = do
  modify (\s -> s { types = (types s) ++ ts })
codegenStmt (Switch s exp clauses) = do
  codegenStmt s
  cases <- return $ sortBy (\a b -> case (a,b) of
    (Case _ _, Default _) -> LT
    (Default _, Case _ _) -> GT
    _ -> EQ) clauses
  blocks <- mapM (\c -> do
    let n = case c of
              (Case _ _) -> "clause"
              _ -> "default"
    head <- addBlock $ n ++ ".head"
    body <- addBlock $ n ++ ".body"
    return (head, body) 
    ) cases
  end <- addBlock "switch.end"
  br (fst . head $ blocks)
  test <- case exp of
    Just x -> do{cond <- codegenExpr x; return $ \n -> codegenExpr n >>= (eq (truety $ annOf x) cond)}
    Nothing -> return $ codegenExpr
  prevState <- gets nextBlock
  mapM (\(((h,b), c):((next,nextBody), _):[]) -> do
      setBlock h
      cond <- case c of
        (Case exps body) -> foldM (\p n -> do
          e <- test n
          bor e p) false exps
        (Default _ ) -> return true
      cbr cond b next
      setBlock b 
      modify (\s -> s{ nextBlock = Just nextBody})
      (\c -> case c of 
        (Case _ b) -> mapM codegenStmt b
        (Default b) -> mapM codegenStmt b)  c
      br end
    ) $ windowed $ (zip blocks cases)++[((end,end),(head cases))]
  modify (\s -> s {nextBlock = prevState})
  setBlock end
  return ()
codegenStmt Fallthrough = do
  blk <- gets nextBlock
  case blk of -- because of weeding this is the only possible case
    Just b -> br b
  return ()
codegenStmt Break = do
  loop <- gets loopBlock
  case loop of
    Just (start, end) -> br end
  return ()
codegenStmt Continue = do
  loop <- gets loopBlock
  case loop of
    Just (start, end) -> br start
  return ()

codegenSpec :: VarSpec Ann -> Codegen ()
codegenSpec (VarSpec a idens [] (Just tp)) = do
  mapM (\n -> do
    i <- case truety a of 
      Slice t -> call llsliceptr (externf llnewslice (AST.Name "new_slice")) [llint 0, llint 10, llint 1]
        where sltp = (lltype $ TypeName "slice")
      TString -> call llsliceptr (externf llnewslice (AST.Name "new_slice")) [llint 0, llint 10, llint 1]
        where sltp = (lltype $ TypeName "slice")
      _ -> alloca $ lltype $ truety a

    assign (AST.Name n) i) idens >> return ()
codegenSpec (VarSpec _ idens exps _) = do
  mapM (\(n, e) -> do
    let tp = (lltype $ ttOf e)
    i   <- alloca tp
    val <- codegenExpr e 
    store tp i val
    assign (AST.Name n) i) (zip idens exps) >> return ()

codegenExpr :: Expression Ann -> Codegen AST.Operand
codegenExpr (BinaryOp tp op a b) = do
  a' <- codegenExpr a
  b' <- codegenExpr b
  binFunc op (ttOf a) a' b'
codegenExpr (UnaryOp tp op a) = do
  a' <- codegenExpr a
  unOp op (ty tp) a'
codegenExpr (Arguments tp (Name fntp "append") args) = do
  slice <- codegenExpr (head args)
  addrs <- mapM (\a -> case a of 
    n@(Name _ _)-> getaddr n >>= \o -> bitcast o (ptr $ char)
    s@(Selector _ _ _)-> getaddr s >>= \o -> bitcast o (ptr $ char)
    i@(Index _ _ _) -> getaddr i >>= \o -> bitcast o (ptr $ char)
    a -> do
      val <- codegenExpr a
      mem <- alloca . lltype $ ttOf a
      store (lltype $ ttOf a) mem val
      bitcast mem (ptr $ char)) $ tail args
  call (llslice) (externf (llappend) (AST.Name "append")) (slice : addrs)
  where llappend = T.FunctionType (llsliceptr) [llsliceptr, ptr $ char] False
codegenExpr (Arguments tp (Name ann "println") args) = do
  mapM (\a -> codegenExpr (Arguments tp (Name ann $ func a) [a])) $ (intersperse (Rune " ") args) ++ [Rune "\n"]
  return one
  where func = ((++) "print.") . (map toLower) . show . toConstr . ttOf
codegenExpr (Arguments tp (Name ann "print") args) = do
  mapM (\a -> codegenExpr (Arguments tp (Name ann $ func a) [a])) (intersperse (Rune " ") args)
  return one
  where func = ((++) "print.") . (map toLower) . show . toConstr . ttOf
codegenExpr (Arguments tp fn args) = do
  largs <- mapM codegenExpr args
  let fnN = case fn of 
              Name _ n -> n
  call (lltype $ truety tp) (externf (lltype $ truety tp) (AST.Name fnN)) largs
codegenExpr (Name tp id) = do
  op <- getvar (AST.Name id)
  load (lltype $ truety tp) op
codegenExpr s@(Selector a _ _) = do
  ptr <- getaddr s
  load (lltype $ truety a) ptr
codegenExpr arr@(Index a _ _) = do
  ptr <- getaddr arr
  load (lltype $ truety a) ptr
codegenExpr (Conversion a t e) = do
  exp <- codegenExpr e
  let converter = case truety a of
                    TFloat   -> sitofp
                    TInteger -> fptosi
  if truety a == (truety $ annOf e)
  then return $ exp
  else converter exp
codegenExpr (Integer i) = return $ cons $ C.Int 64 (toInteger i)
codegenExpr (Float f) = return $ cons $ C.Float (F.Double f)
codegenExpr (Bool b) = return $ cons $ C.Int 1 (toInteger $ fromEnum b)
codegenExpr (IntString s) = do
  nm <- addConstant (arraytp s) $ C.Array char $ map (\c -> C.Int 8 $ toInteger $ ord c) s
  charptr <- gep (ptr $ char) (externf (arraytp s) nm) [zero, zero]
  ptr <- call llsliceptr (externf llstringcons (AST.Name "string_constant")) [charptr, cons $ C.Int 32 (toInteger $ length s)]
  load llslice ptr
  where llstringcons = T.FunctionType llsliceptr [ptr $ char, i32] False
        arraytp s = T.ArrayType (fromIntegral $ length s) T.i8 
codegenExpr (RawString s) = codegenExpr (IntString s)
codegenExpr (Rune c) = return $ cons $ C.Int 8 (toInteger . ord $ head c)

windowed ls = 
    (case ls of 
      [] -> []
      x:xs -> 
        if length ls >= 2 then 
          (take 2 ls) : windowed xs
        else windowed xs)

llint i = cons $ C.Int 32 (toInteger i)

getaddr :: Expression Ann -> Codegen AST.Operand
getaddr (Name _ x) = getvar (AST.Name x) 
getaddr (Selector a s nm) = do
  struct <- getaddr s
  idx <- case fieldIdx (typeOf s) nm of
    Just i -> return $ cons $ C.Int 64 (toInteger i)
    Nothing -> error $ "Invalid field access"
  gep (lltype $ truety a) struct [zero, idx]
getaddr (S.Index a s i) = do
  arr <- getaddr s
  longId <- codegenExpr i
  idx <- trunc longId i32
  elsize <- gep (ptr $ i32) arr [zero, llint 2] >>= load i32

  offset <- imul i32 idx elsize
  bufPtrPtr <- gep (ptr $ ptr $ char) arr [zero, llint 3]
  bufPtr    <- load (ptr $ ptr $ char) bufPtrPtr
  addr <- gep (ptr $ char) bufPtr [offset]
  bitcast addr (ptr $ lltype $ truety a)

binFunc :: BinOp -> Type -> AST.Operand -> AST.Operand -> Codegen AST.Operand
binFunc BitAnd _ = band
binFunc BitClear _ = bclear
binFunc BitOr  _ = bor
binFunc BitXor _ = xor
binFunc Div   t = div t
binFunc Mult  t = mul t
binFunc Plus  t = add t
binFunc Minus t = sub t
binFunc Mod   _ = mod
binFunc Eq    t = eq t
binFunc NEq   t = neq t
binFunc And   _ = band
binFunc Or    _ = bor
binFunc RShift _ = shl
binFunc LShift _ = shr
  
binFunc GreaterThan t   = gt t
binFunc GreaterThanEq t = geq t
binFunc LessThan t      = lt t
binFunc LessThanEq t    = leq t

unOp :: UOp -> S.Type -> AST.Operand -> Codegen AST.Operand
unOp Pos t o   = (codegenExpr (Integer  1)) >>= mul t o
unOp Neg t o   = (codegenExpr (Integer $ -1)) >>= mul t o
unOp Not _ o   = not o
unOp BComp _ o = bcomp o

unalias :: Ann -> Type
unalias a = case (ty a, truety a) of
  (TypeName n, Struct _) -> TypeName n
  (_, tt ) -> tt 

emit :: Package Ann -> String -> IO ()
emit pkg nm = do
  let mod = runLLVM (emptyModule "") (codegenPkg pkg)
  -- putStrLn $ showPretty  mod
  withContext $ \ctxt -> do
    err <- runExceptT $ withModuleFromLLVMAssembly ctxt (File "builtins/builtins.ll") $ \builtins -> do
      err <- liftM join $ runExceptT $ withModuleFromAST ctxt mod $ \m -> do
        withPassManager defaultCuratedPassSetSpec $ \pm -> do
          runExceptT $ linkModules False m builtins
          runPassManager pm m
          runExceptT $ writeLLVMAssemblyToFile (File $ nm ++ ".ll") m
          moduleLLVMAssembly m
          liftM join $ runExceptT $ TM.withDefaultTargetMachine $ \target -> do
            runExceptT $ writeTargetAssemblyToFile target (File $ nm ++ ".s") m
      case err of
        Left s  -> putStrLn s
        Right _ -> return ()
    case err of
      Left d  -> putStrLn $ show d
      Right _ -> return ()
  return ()
