module MGC.Emit where

import Prelude hiding (mod, div, not)


import MGC.Syntax as S
import MGC.Check (Ann, typeOf)
import MGC.Codegen

import LLVM.General.Context
import LLVM.General.Module
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.IntegerPredicate as IP


import Control.Applicative ((<$>), (<*>))

import Control.Monad
import Control.Monad.Except

codegenTop :: TopLevelDeclaration Ann -> LLVM ()
codegenTop (FunctionDecl nm sig body) = do
  define (lltype $ retty  sig) nm largs blks
  where
    largs = argty sig
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM largs $ \(atp, a) -> do
        var <- alloca atp
        store atp var (local atp a)
        assign a var
      codegenStmt body
--codegenTop (Decl VarDecl)
--codegenTop (Decl TypeDecl)
--codegenTop _


retty :: Signature -> Type
retty (Signature _ ret) = ReturnType $ map (\(Parameter _ t) -> t) ret

argty :: Signature -> [(AST.Type, AST.Name)]
argty (Signature arg _) = concatMap (\(Parameter ids t) -> map (\i -> (lltype t, AST.Name i)) ids) arg

codegenStmt :: Statement Ann -> Codegen ()
codegenStmt (Return exp) = codegenExpr (head exp) >>= ret >> return ()
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
codegenStmt (Inc e) = codegenExpr (BinaryOp (typeOf e) Plus  (Integer 1) e) >> return ()
codegenStmt (Dec e) = codegenExpr (BinaryOp (typeOf e) Minus (Integer 1) e) >> return ()
codegenStmt (Block s) = do
  mapM codegenStmt s
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

-- Break
-- Continue
-- Assignment BinOp [Expression a] [Expression a]
-- Fallthrough
-- For (Maybe (ForCond a)) (Statement a)
-- ShortDecl [Identifier] [Expression a]
-- Switch (Statement a) (Maybe (Expression a)) [SwitchClause a]
-- TypeDecl [TypeSpec]
-- VarDecl [VarSpec a] 

codegenCond :: Maybe (ForCond Ann) -> Codegen AST.Operand
codegenCond (Just (Condition exp)) = codegenExpr exp
codegenCond Nothing = return true
--codegenCond (ForClause stmt pre cnd post) = do -- deal w stmt (cant be rerun) same w pre
  

codegenExpr :: Expression Ann -> Codegen AST.Operand
codegenExpr (BinaryOp tp op a b) = do
  a' <- codegenExpr a
  b' <- codegenExpr b
  binFunc op (typeOf a) a' b'
codegenExpr (UnaryOp tp op a) = do
  a' <- codegenExpr a
  unOp op tp a'
codegenExpr (Integer i) = return $ cons $ C.Int 64 (toInteger i)
codegenExpr (Float f) = return $ cons $ C.Float (F.Single f)
--codegenExpr (Arguments _ fn args) = do
--  largs <- mapM codegenExpr args
--  call (externf (AST.Name fn) largs)
--codegenExpr ()
codegenExpr (Name tp id) = do
  op <- getvar (AST.Name id)
  load (lltype tp) op -- NEED TO IMPLEMENT NAME TABLE
--Conversion a Type (Expression a)
--Selector a (Expression a) Identifier
--Index a (Expression a) (Expression a)
--SimpleSlice a (Expression a) (Expression a) (Expression a)
--FullSlice a (Expression a) (Expression a) (Expression a) (Expression a)
--Arguments a (Expression a) [(Expression a)]
--Name a Identifier
--QualName Identifier Identifier
--Rune String
--IntString String 
--Bool Bool this is an int
--RawString String

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


testCode mod = do
  withContext $ \ctxt ->
    runExceptT $ withModuleFromAST ctxt mod $ \m -> do
      s <- moduleLLVMAssembly m
      putStrLn s