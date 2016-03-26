{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module MGC.Syntax.Weeder where
  import MGC.Syntax
  import MGC.Error (WeederError(..), MGCError(..), transLeft)
  import Control.Applicative ((<$>), (<*>), (<*))
  import Control.Monad (when)
  import Control.Monad.Except
  import Control.Monad.State

  type Weed = ExceptT WeederError (State WeederState)

  runWeeder :: Weedable a => a -> Either MGCError a
  runWeeder = transLeft Weeder . fst . flip runState empty . runExceptT . weed

  runWeeder' :: Weedable a => a -> (Either WeederError a, WeederState)
  runWeeder' = flip runState empty . runExceptT . weed

  class Weedable a where
    weed ::  a -> Weed a
    weed = return
    weedList :: [a] -> Weed [a]
    weedList = mapM weed

  data WeederState = State{lhs :: Bool, func :: Bool, funcReturn :: Bool, loop :: Bool, branchRet :: Bool} deriving Show

  empty = State{lhs = False, func = False, funcReturn = False, loop = False, branchRet = False}

  pIdent s = if (s == "_") then (throwError InvalidPackageName) else return s

  instance Eq a => Weedable (Package a) where
    weed (Package iden tlds) = liftM2 Package (pIdent iden) (weed tlds)

  instance Eq a => Weedable (TopLevelDeclaration a) where
    weed  (FunctionDecl _ _ Empty) = throwError EmptyFuncBody
    weed (FunctionDecl iden sig bdy) = do
      modify (\st -> st {func = True, funcReturn= ret sig, branchRet = False})
      b <- weed  bdy
      s <- get
      when (ret sig && not (branchRet s)) $ throwError MissingReturn
      return $ FunctionDecl iden sig b
    weed (Decl a@(TypeDecl _)) = Decl <$> weed a
    weed (Decl a@(VarDecl _ )) = Decl <$> weed a
    weed  (Decl _) = throwError InvalidTopLevelDecl

  ret (Signature _ retParams) = not (null retParams)

  instance Eq a => Weedable (Statement a) where
    weed (Switch s e bdy) = liftM3 Switch (weed s) (weed e) (weed bdy)
    weed (For c bdy) = modify (\st -> st {loop = True}) >> liftM2 For (weed c) (weed bdy)
    weed Continue = getState loop>>= (\loop -> if loop
      then return Continue
      else throwError InvalidContinue)
    weed Break = getState loop >>= (\loop -> if loop
      then return Break
      else throwError InvalidBreak)
    weed (If s e l r) = do
      s <- weed s
      e <- weed e
      (l,r) <- orM (sBRet False) branchRet $ do{l <- weed l; r <- andM (sBRet False) branchRet (weed r); return (l,r)}
      return $ If s e l r
    weed (Block bdy) = Block <$> weed bdy
    weed (Return e) = getState funcReturn >>= (\funcRet -> case (funcRet, length e) of
      (True, 1) ->  Return <$> weed e <* modify (sBRet True)
      (False, 0) -> Return <$> weed e <* modify (sBRet True)
      (_, _) -> throwError MultipleReturnValue)
    weed Empty = return Empty
    weed (ExpressionStmt e) = ExpressionStmt <$> weed e
    weed (TypeDecl tps) = TypeDecl <$> mapM weed tps
    weed (VarDecl vars) = VarDecl  <$> mapM weed vars
    weed  (Inc e) = temp (sLhs True) $ Inc <$> weed e
    weed  (Dec e) = temp (sLhs True) $ Dec <$> weed e
    weed  Fallthrough = return Fallthrough
    weed (ShortDecl lh rh) = if length lh == length rh
      then liftM (ShortDecl lh) (weed rh)
      else throwError AssignSizeDifferent
    weed (Assignment op lh rh)
      | (length lh) /= (length rh) = throwError AssignSizeDifferent
      | op == Eq = liftM2 (Assignment op) ((temp (sLhs True)) $ weed lh) (weed rh)
      | otherwise = liftM2 (Assignment op) (weed lh) (weed rh)

  instance Eq a => Weedable (ForCond a) where
    weed (Condition e) = Condition <$> weed e
    weed (ForClause s e p) = liftM3 ForClause (weed s) (weed e) (weed p)

  instance Weedable (TypeSpec a) where
    weed (TypeSpec a iden tp) = TypeSpec a iden <$> weed tp

  instance Weedable (VarSpec a) where
    weed (VarSpec a idens exps Nothing) = if length idens == length exps
      then liftM2 (VarSpec a idens) (weed exps) (return Nothing)
      else throwError AssignSizeDifferent
    weed (VarSpec a idens exps tp) = if (null exps) || (length idens == length exps)
      then liftM2 (VarSpec a idens) (weed exps) (weed tp)
      else throwError AssignSizeDifferent

  instance Eq a => Weedable (SwitchClause a) where
    weedList cls =
      if (length $ filter (isDefault) cls) > 1
      then throwError MultipleDefault
      else if (fallthrough . stmts $ last cls)
        then throwError InvalidFallthrough
        else (orM (sBRet False) branchRet)  $ mapM ((andM (sBRet False) branchRet) . weed) cls
    weed (Case e s) = Case <$> weed e <*> weed s
    weed (Default s) = Default <$> weed s
    --weed = throwError MultipleDefault

  getState :: (WeederState -> a) -> Weed a
  getState f = get >>= (return . f)

  temp :: (WeederState -> WeederState) -> Weed a -> Weed a
  temp fs f = do
    s <- get
    put (fs s)
    ret <- f
    put s
    return ret

  orM :: (WeederState -> WeederState) -> (WeederState -> Bool) -> Weed a -> Weed a
  orM fs p f = do
    s <- get
    put (fs s)
    ret <- f
    Control.Monad.when (p s) $ put s
    return ret

  andM fs p f = do
    s <- get
    put (fs s)
    ret <- f
    s' <- get
    Control.Monad.when (p s && p s') $ put s
    return ret

  sLhs a x = x {lhs = a}
  sBRet a x = x {branchRet = a}
  isDefault (Default _ ) = True
  isDefault _ = False

  stmts (Default s) = s
  stmts (Case _ s)  = s

  blankString n =  if n == "_"
    then throwError BlankValue
    else return n

  fallthrough :: Eq a => [Statement a] -> Bool
  fallthrough = elem Fallthrough

  notlval :: Weed ()
  notlval = do{ s <- get; when (lhs s) $ throwError InvalidLValue}

  instance Weedable (Expression a) where
    weed (Name a n)           = do{s <- get; if lhs s == True then return (Name a n) else Name a <$> blankString n }
    weed (Selector a e i)      = liftM2 (Selector a) (temp (sLhs False) $ weed e) (blankString i)
    weed (Index a l r)         = liftM2 (Index a) (temp (sLhs False) $ weed l) (temp (sLhs False) $ weed r)
    weed (BinaryOp a o l r)    = notlval >> liftM2 (BinaryOp a o) (weed l) (weed r)
    weed (UnaryOp a o e)       = notlval >> liftM  (UnaryOp a o) (weed e)
    weed (Conversion a tp e)   = notlval >> liftM2 (Conversion a) (weed tp) (weed e)
    weed (SimpleSlice a s l r) = notlval >> liftM3 (SimpleSlice a) (weed s) (weed l) (weed r)
    weed (FullSlice a s l d r) = notlval >> liftM4 (FullSlice a) (weed s) (weed l) (weed d) (weed r)
    weed (Arguments a e arg)   = notlval >> liftM2 (Arguments a) (weed e) (weed arg)
    weed  e = notlval >> return  e

  instance Weedable Type where
    weed (Array size tp) = Array size <$> weed tp
    weed a = return a

  instance Weedable a =>  Weedable [a] where weed a = weedList a

  instance Weedable a => Weedable (Maybe a) where
    weed (Just b) = liftM Just (weed b)
    weed Nothing  = return Nothing

