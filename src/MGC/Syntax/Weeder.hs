{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module MGC.Syntax.Weeder where
  import MGC.Syntax
  import Control.Applicative ((<$>))
  import Control.Monad (liftM, liftM2, liftM3, liftM4, mapM)
  import Control.Monad.Except

  class Weedable a where
    weed :: a -> Either WeederError a
    weed = return
    weedList :: [a] -> Either WeederError [a]
    weedList = mapM weed

  data WeederError 
    = MultipleDefault
    | EmptyFuncBody
    | InvalidTopLevelDecl
    | InvalidPackageName
    | InvalidContinue
    | AssignSizeDifferent
    | BlankValue
    | InvalidBreak 
    | MultipleReturnValue deriving Show

  pIdent s = case (s == "_") of
    True -> (throwError InvalidPackageName)
    _ -> return s

  instance Weedable Package where
    weed (Package iden tlds) = liftM2 Package (pIdent iden) (weed tlds)

  instance Weedable TopLevelDeclaration where
    weed (FunctionDecl _ _ Nothing) = throwError EmptyFuncBody
    weed (FunctionDecl iden sig bdy) = liftM (FunctionDecl iden sig) (weed bdy)
    weed (Decl a@(TypeDecl _)) = Decl <$> (weed a)
    weed (Decl a@(VarDecl _ )) = Decl <$> (weed a)
    weed (Decl _) = throwError InvalidTopLevelDecl

  instance Weedable Statement where 
    weed (Switch s e bdy) = liftM3 Switch (weed s) (weed e) (weed bdy)
    weed (For _ bdy) = weed' bdy
    weed (Continue) = throwError InvalidContinue
    weed (Break) = throwError InvalidBreak
    weed (If s e l r) = liftM4 If (weed s) (weed e) (weed l) (weed r)
    weed (Block bdy) = Block <$> (weed bdy) 
    weed (Return e) = if (length e) <= 1 
      then Return <$> (weed e) 
      else throwError MultipleReturnValue
    weed (Empty) = return Empty
    weed (ExpressionStmt e) = ExpressionStmt <$> (weed e)
    weed a@(TypeDecl _) = return a
    weed a@(VarDecl _)  = return a
    weed (Inc e) = Inc <$> (weed e)
    weed (Dec e) = Dec <$> (weed e)
    weed (Fallthrough) = return Fallthrough
    weed (ShortDecl lh rh) = if (length lh) == (length rh)
      then liftM (ShortDecl lh) (weed rh)
      else throwError AssignSizeDifferent
    weed (Assignment op lh rh) = if (length lh) /= (length rh)
      then throwError AssignSizeDifferent
      else liftM2 (Assignment op) (weed lh) (weed rh)
  
  weed' :: Statement -> Either WeederError Statement
  weed' (Continue) = return Continue
  weed' (Break) = return Break
  weed' (If s e l r) = liftM4 If (weed s) (weed e) (weed' l) (weed' r)
  weed' (Block bdy) = Block <$> (mapM weed' bdy) 
  weed' (For _ bdy) = weed' bdy
  weed' (Switch s e b) = liftM3 Switch (weed s) (weed e) (weedSC' b)
  weed' a = weed a

  weedSC' :: SwitchClause -> Either WeederError Statement
  weedSC' = weed
  instance Weedable SwitchClause where
    weedList cls = do
      if 1 < (length $ filter (((==) Nothing). fst) cls)
      then throwError MultipleDefault
      else mapM weed cls
    --weed = throwError MultipleDefault 

  blankString n =  if n == "_"
    then throwError BlankValue
    else return n

  instance Weedable Expression where
    weed (Name n) = Name <$> blankString n
    weed (BinaryOp o l r)    = liftM2 (BinaryOp o) (weed l) (weed r)
    weed (UnaryOp o e)       = liftM  (UnaryOp o) (weed e)
    weed (Conversion tp e)   = liftM  (Conversion tp) (weed e)
    weed (Selector e i)      = liftM2 Selector (weed e) (blankString i)
    weed (Index l r)         = liftM2 Index (weed l) (weed r)
    weed (SimpleSlice s l r) = liftM3 SimpleSlice (weed s) (weed l) (weed r)
    weed (FullSlice s l d r) = liftM4 FullSlice (weed s) (weed l) (weed d) (weed r)
    weed (Arguments e a)     = liftM2 Arguments (weed e) (weed a)
    weed e = Right e

  instance Weedable a =>  Weedable [a] where weed a = (weedList a)

  instance Weedable a => Weedable (Maybe a) where
    weed (Just b) = liftM Just (weed b)
    weed Nothing  = return $ Nothing
