module MGC.Emit where

import Prelude hiding (mod, div, not)

import MGC.Syntax as S
import MGC.Check (Ann(..), typeOf, annOf, ttOf)
import MGC.Codegen

import LLVM.General.Context
import LLVM.General.Module
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST as AST
import LLVM.General.PassManager

import Control.Monad
import Control.Monad.State (modify)
import Control.Monad.Except
import Data.List (findIndex)

codegenPkg :: Package Ann -> LLVM ()
codegenPkg (Package nm tlds) = do
  modify (\s -> s {AST.moduleName = nm})
  mapM codegenTop tlds
  return ()

codegenTop :: TopLevelDeclaration Ann -> LLVM ()
codegenTop (FunctionDecl nm sig body) = do
  codegenTop (Decl (TypeDecl $ types cg))
  define (lltype $ retty  sig) nm largs blks
  where
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
codegenTop (Decl (TypeDecl ts)) = do
  mapM (\(TypeSpec n t) -> typedef (AST.Name n) (lltype t)) ts
  return ()
--codegenTop (Decl VarDecl)
--codegenTop (Decl TypeDecl)
--codegenTop _

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
codegenStmt (Inc e) = codegenExpr (BinaryOp (annOf e) Plus  (Integer 1) e) >> return ()
codegenStmt (Dec e) = codegenExpr (BinaryOp (annOf e) Minus (Integer 1) e) >> return ()
codegenStmt (Block s) = do
  mapM codegenStmt s
  return ()
codegenStmt (For (Just (ForClause s e p)) body) = do
  loopstart <- addBlock "for.start"
  loopbody <- addBlock "for.body"
  loopend <- addBlock "for.end"

  codegenStmt s
  br loopstart
  test <- case e of
    Just e -> codegenExpr e
    Nothing -> return true
  cbr  test loopbody loopend
  setBlock loopbody
  codegenStmt body
  codegenStmt p  
  br loopstart
  setBlock loopend

  return ()
codegenStmt (For cond body) = do
  loopstart <- addBlock "for.start"
  loopbody <- addBlock "for.body"
  loopend <- addBlock "for.end"
  -- init vars
  br loopstart
  setBlock loopstart
  test <- codegenCond cond
  cbr test loopbody loopend

  setBlock loopbody
  codegenStmt body
  br loopstart

  setBlock loopend

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
  rhs <- mapM codegenExpr rh
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
-- Break
-- Continue -- need to keep track of loops 
-- Assignment BinOp [Expression a] [Expression a]
-- Fallthrough -- need to keep track of switches
-- For (Maybe (ForCond a)) (Statement a) -- need to implement clause looops
-- Switch (Statement a) (Maybe (Expression a)) [SwitchClause a]
-- TypeDecl [TypeSpec]

codegenCond :: Maybe (ForCond Ann) -> Codegen AST.Operand
codegenCond (Just (Condition e)) = codegenExpr e
codegenCond Nothing = return true

codegenSpec :: VarSpec Ann -> Codegen ()
codegenSpec (VarSpec idens [] (Just tp)) = do
  mapM (\n -> do
    i <- alloca $ lltype tp
    assign (AST.Name n) i) idens >> return ()
codegenSpec (VarSpec idens exps _) = do
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
codegenExpr (Integer i) = return $ cons $ C.Int 64 (toInteger i)
codegenExpr (Float f) = return $ cons $ C.Float (F.Single f)
codegenExpr (Bool b) = return $ cons $ C.Int 1 (toInteger $ fromEnum b)
codegenExpr (Arguments tp fn args) = do
  largs <- mapM codegenExpr args
  let fnN = case fn of 
              Name _ n -> n
  call (lltype $ truety tp) (externf (lltype $ truety tp) (AST.Name fnN)) largs
codegenExpr (Name tp id) = do
  op <- getvar (AST.Name id)
  load (lltype $ truety tp) op -- NEED TO IMPLEMENT NAME TABLE
codegenExpr s@(Selector a _ _) = do
  ptr <- getaddr s
  load (lltype $ truety a) ptr
codegenExpr arr@(Index a _ _) = do
  ptr <- getaddr arr
  load (lltype $ truety a) ptr
--Conversion a Type (Expression a)
--Rune String
--IntString String 
--RawString String

getaddr :: Expression Ann -> Codegen AST.Operand
getaddr (Name t x) = getvar (AST.Name x) 
getaddr (Selector a s nm) = do
  struct <- getaddr s
  idx <- case fieldIdx (typeOf s) nm of
    Just i -> return $ cons $ C.Int 64 (toInteger i)
    Nothing -> error $ "Invalid field access"
  gep (lltype $ truety a) struct idx
getaddr (S.Index a s i) = do
  arr <- getaddr s
  idx <- codegenExpr i
  gep (lltype $ truety a) arr idx

binFunc :: BinOp -> Type -> AST.Operand -> AST.Operand -> Codegen AST.Operand
binFunc BitAnd _ = band
--binFunc BitClear = bclear
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
testCode mod = do
  withContext $ \ctxt ->
    runExceptT $ withModuleFromAST ctxt mod $ \m -> do
      withPassManager defaultCuratedPassSetSpec {optLevel = Just 3} $ \pm -> do
        -- runPassManager pm m
        s <- moduleLLVMAssembly m
        putStrLn s
