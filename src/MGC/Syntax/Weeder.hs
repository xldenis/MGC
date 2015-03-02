{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module MGC.Syntax.Weeder where
  import MGC.Syntax
  import Control.Applicative ((<$>), (<*>))
  import Control.Monad (liftM, liftM2, liftM3, liftM4, mapM)
  import Control.Monad.Except

  runWeeder :: Weedable a => a -> Either WeederError a
  runWeeder = weed State{lhs = False, func = False, funcReturn = False, loop = False}

  class Weedable a where
    weed :: WeederState -> a -> Either WeederError a
    weed _ = return
    weedList :: WeederState -> [a] -> Either WeederError [a]
    weedList st = mapM (weed st)

  data WeederError 
    = MultipleDefault
    | EmptyFuncBody
    | InvalidTopLevelDecl
    | InvalidPackageName
    | InvalidContinue
    | AssignSizeDifferent
    | BlankValue
    | InvalidBreak
    | InvalidArraySize
    | InvalidLValue
    | MultipleReturnValue deriving Show

  data WeederState = State{lhs :: Bool, func :: Bool, funcReturn :: Bool, loop :: Bool }

  pIdent s = case (s == "_") of
    True -> (throwError InvalidPackageName)
    _ -> return s

  instance Weedable Package where
    weed st (Package iden tlds) = liftM2 Package (pIdent iden) (weed st tlds)

  instance Weedable TopLevelDeclaration where
    weed _  (FunctionDecl _ _ Nothing) = throwError EmptyFuncBody
    weed st (FunctionDecl iden sig bdy) = liftM (FunctionDecl iden sig) (weed (st {func = True, funcReturn= ret sig}) bdy)
    weed st (Decl a@(TypeDecl _)) = Decl <$> (weed st a)
    weed st (Decl a@(VarDecl _ )) = Decl <$> (weed st a)
    weed _  (Decl _) = throwError InvalidTopLevelDecl

  ret (Signature _ retParams) = (length retParams) >= 1

  instance Weedable Statement where 
    weed st (Switch s e bdy) = liftM3 Switch (weed st s) (weed st e) (weed st bdy)
    weed st (For _ bdy) = weed (st {loop = True}) bdy
    weed st (Continue) = if (loop st)
      then return Continue
      else throwError InvalidContinue
    weed st (Break) = if (loop st)
      then return Break
      else throwError InvalidBreak
    weed st (If s e l r) = liftM4 If (weed st s) (weed st e) (weed st l) (weed st r)
    weed st (Block bdy) = Block <$> (weed st bdy) 
    weed st (Return e) = case (funcReturn st, length e) of
      (True, 1) ->  Return <$> (weed st e) 
      (False, 0) ->Return <$> (weed st e) 
      (_, _) -> throwError MultipleReturnValue
    weed st (Empty) = return Empty
    weed st (ExpressionStmt e) = ExpressionStmt <$> (weed st e)
    weed st (TypeDecl tps) = TypeDecl <$> mapM (weed st) tps
    weed st (VarDecl vars) = VarDecl  <$> mapM (weed st) vars
    weed st  (Inc e) = Inc <$> (weed (st {lhs = True}) e)
    weed st  (Dec e) = Dec <$> (weed (st {lhs = True}) e)
    weed _  (Fallthrough) = return Fallthrough
    weed st (ShortDecl lh rh) = if (length lh) == (length rh)
      then liftM (ShortDecl lh) (weed st rh)
      else throwError AssignSizeDifferent
    weed st (Assignment op lh rh) = if (length lh) /= (length rh)
      then throwError AssignSizeDifferent
      else if op == Eq
        then liftM2 (Assignment op) (weed (st {lhs = True}) lh) (weed st rh)
        else liftM2 (Assignment op) (weed st lh) (weed st rh)
  instance Weedable TypeSpec where
    weed st (TypeSpec iden tp) = TypeSpec iden <$> (weed st tp)

  instance Weedable VarSpec where
    weed st (VarSpec idens exps Nothing) = if (length idens) == (length exps)
      then liftM2 (VarSpec idens) (weed st exps) (return $ Nothing)
      else throwError AssignSizeDifferent
    weed st (VarSpec idens exps tp) = if (length exps == 0) || (length idens == length exps)
      then liftM2 (VarSpec idens) (weed st exps) (weed st tp)
      else throwError AssignSizeDifferent

  instance Weedable SwitchClause where
    weedList st cls = do
      if 1 < (length $ filter (((==) Nothing). fst) cls)
      then throwError MultipleDefault
      else mapM (weed st) cls
    weed st (e, s) = (,) <$> (weed st e) <*> (weed st s)
    --weed = throwError MultipleDefault 

  blankString n =  if n == "_"
    then throwError BlankValue
    else return n

  instance Weedable Expression where
    weed State{lhs = True} (Name n) = return (Name n)
    weed st (Selector e i)      = liftM2 Selector (weed (st {lhs = False}) e) (blankString i)
    weed st (Index l r)         = liftM2 Index (weed (st {lhs = False}) l) (weed (st {lhs = False}) r)
    weed State{lhs = True} _    = throwError InvalidLValue
    weed st (Name n)            = Name <$> blankString n
    weed st (BinaryOp o l r)    = liftM2 (BinaryOp o) (weed st l) (weed st r)
    weed st (UnaryOp o e)       = liftM  (UnaryOp o) (weed st e)
    weed st (Conversion tp e)   = liftM2 Conversion (weed st tp) (weed st e)
    weed st (SimpleSlice s l r) = liftM3 SimpleSlice (weed st s) (weed st l) (weed st r)
    weed st (FullSlice s l d r) = liftM4 FullSlice (weed st s) (weed st l) (weed st d) (weed st r)
    weed st (Arguments e a)     = liftM2 Arguments (weed st e) (weed st a)
    weed _  e = Right e

  instance Weedable Type where
    weed st (Array size tp) = Array <$> (case size of 
      Integer _ -> return size
      _ -> throwError InvalidArraySize
      ) <*> (weed st tp)
    weed _ a = return a

  instance Weedable a =>  Weedable [a] where weed st a = (weedList st a)

  instance Weedable a => Weedable (Maybe a) where
    weed st (Just b) = liftM Just (weed st b)
    weed _ Nothing  = return $ Nothing

