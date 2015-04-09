{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}

module MGC.Check where
  import qualified Data.Map as M
  import Data.Map (Map)
  import MGC.Error (TypeError(..), MGCError(..), transLeft)
  import MGC.Syntax
  import Data.Maybe (listToMaybe, mapMaybe)
  import Control.Monad.State
  import Control.Monad.Except
  import Control.Applicative ((<$>), (<*>))

  ann t = Ann{ty = t, truety = t}

  type Env = [Map String Ann]
  data Counters= C{ret :: Type, typCnt :: Int}

  type Check = ExceptT TypeError (State (String, Counters, Env))

  class Checkable a where
    check :: (a ()) -> Check (a Ann)
    checkList :: [a ()] -> Check [a Ann]
    checkList = mapM (check)

  instance Checkable Package where
    check (Package name pkg) = do
      pushScope
      pkg' <- checkList pkg
      main <- decl "main"
      when (name == "main" && not main) $ throwError MissingMain
      popScope
      return $ Package name pkg'

  instance Checkable TopLevelDeclaration where
    check (Decl t@(TypeDecl _)) = Decl <$> (check t)
    check (Decl v@(VarDecl  _)) = Decl <$> (check v)
    check (FunctionDecl name sig body) = do
      addVar name $ Function sig
      pushScope
      mapM (\(Parameter idens tp) -> mapM (\nm -> addVar nm tp) idens) $ (\(Signature s _) -> s) sig
      ret <- mapM (\(Parameter _ tp) -> alias tp) $ (\(Signature _ s) -> s) sig
      let fRet = if length ret == 0 then TNil else ReturnType ret
      modify (\(l,st, e) -> (l, (st {ret= fRet}), e)) 
      b <- case body of
        Block s -> Block <$> (checkList s)
        Empty -> return Empty
        _ -> throwError $ ImpossibleError "Invalid function body"
      popScope
      modify (\(l,st, e) -> (l, (st {ret= TNil}), e)) 
      return $ FunctionDecl name sig b
    check (Decl _ ) = throwError $ ImpossibleError "Top Level Declaration invalid"

  instance Checkable Statement where
    check (TypeDecl tps) = do
      tps' <- mapM (\(TypeSpec _ n tp) -> do
        addType n tp
        uniqNm <- alias $ TypeName n
        return $ TypeSpec Ann{ty = (TypeName n), truety = uniqNm } n tp
        ) tps
      return $ TypeDecl tps'
    check (VarDecl vars) = do
      tps <- checkList vars
      return $ VarDecl tps
    check (Return exps) = do
      (_, tp, _) <- get
      e <- checkList exps
      tps <- mapM (\x -> case typeOf x of
        ReturnType t -> case length t of
          0 -> throwError InvalidReturn
          1 -> return t
          l -> if l == length e 
            then return t 
            else throwError $ InvalidReturn
        a -> return [a]) e
      case (ret tp) of
        ReturnType t -> when (t /= (concat tps)) $ throwError $ InvalidReturn
        TNil -> when (length e /= 0) $ throwError $ InvalidReturn
        _ -> throwError $ ImpossibleError "got non return type as function return"
      return $ Return e
    check (If stmt exp l r) = do -- check type
      pushScope
      s <- check stmt
      e <- check exp
      tp <- rawType $ typeOf e
      case tp of
        TBool -> return ()
        _ -> throwError $ InvalidCondition e
      lh <- check l
      rh <- check r
      popScope
      return $ If s e lh rh
    check (For (Just cond) body) = do -- check type
      pushScope
      c <- check cond
      b <- check body
      popScope
      return $ For (Just c) b
    check (For Nothing body) = For Nothing <$> (check body) -- check type
    check (Switch stmt exp cls) = do -- check type
      pushScope
      s <- check stmt
      (e, tp) <- case exp of
        Nothing -> return (Nothing, TBool)
        Just e -> (uncurry $ liftM2 (,)) (Just <$> (check e), typeOf <$> check e)
      cls' <- checkList cls
      mapM (\x -> case x of
        Default _ -> return ()
        Case e _ -> if (any ((tp /=) . typeOf ) e) 
          then throwError $ InvalidCase e tp
          else return ()) cls'
      popScope
      return $ Switch s e cls'
    check (Block stmts) = do
      pushScope
      s <- checkList stmts
      popScope
      return $ Block s
    check (ExpressionStmt e) = ExpressionStmt <$> (check e)
    check (Inc exp) = do
      e <- check exp
      case (truety $ annOf e) of
        TRune -> return $ Inc e
        TString -> return $ Inc e
        TFloat -> return $ Inc e
        TInteger -> return $ Inc e
        t -> throwError $ InvalidIncDec e t
    check (Dec exp) = do
      e <- check exp
      case (truety $ annOf e) of
        TRune -> return $ Dec e
        TString -> return $ Dec e
        TFloat -> return $ Dec e
        TInteger -> return $ Dec e
        t -> throwError $ InvalidIncDec e t
    check (Assignment Eq lh rh) = do
      lhs <- checkList lh
      rhs <- checkList rh
      mapM (\(l, r) -> when (l /= r) $ throwError (Mismatch l r)) $ zip (typeList lhs) (typeList rhs)
      return $ Assignment Eq  lhs rhs
    check (Assignment op lh rh) = do --accept += string
      lhs <- checkList lh
      rhs <- checkList rh
      mapM (\(l, r) -> when (l /= r) $ throwError (Mismatch l r)) $ zip (typeList lhs) (typeList rhs)
      alLhs <- alias $ typeOf $ head lhs
      if isNumOp op
      then when (not $ (TString == (truety $ annOf $ head rhs)) || (isNum $ alLhs)) $ throwError (InvalidOpType op (head lhs) (head rhs))
      else when (not $ isInt $ alLhs) $ throwError (InvalidOpType op (head lhs) (head rhs))
      return $ Assignment op lhs rhs
    check (ShortDecl idents exps) = do
      decls <- mapM decl idents
      when (all (==True) decls) (throwError NoNewVars)
      e <- checkList exps
      mapM (\(d, id, e) -> case d of 
        True -> do
          v <- checkVar id
          if (ty v) == (typeOf e)
          then return (typeOf e)
          else throwError NoNewVars
        False -> addVar id (typeOf e) >> return (typeOf e) ) $ zip3 decls idents e
      return $ ShortDecl idents e
    check (Empty) = return Empty
    check (Continue) = return Continue
    check (Break) = return Break
    check (Fallthrough) = return Fallthrough

  instance Checkable ForCond where
    check (Condition e)            = Condition <$> (check e >>= (hasType TBool (throwError InvalidFor)))
    check (ForClause s (Just a) p) = ForClause <$> (check s) <*> (Just <$> (check a >>= (hasType TBool (throwError InvalidFor)))) <*> (check p)
    check (ForClause s Nothing p)  = ForClause <$> (check s) <*> (return Nothing) <*> (check p)
  
  instance Checkable VarSpec where
    check (VarSpec _ idens exps Nothing) = do -- dont ignore tp check double declarations
      tps <- checkList exps
      mapM (\(i, t) -> addVar i (typeOf t) ) $ zip idens tps
      return $ VarSpec (ann TNil) idens tps Nothing
    check (VarSpec _ idens exps (Just t)) = do
      tps <- checkList exps
      trueT <- alias t
      when (any ((trueT /=).typeOf ) tps) $ throwError $ InvalidVarDec t tps
      mapM (\i -> addVar i trueT ) idens
      return $ VarSpec Ann{ty = t, truety = trueT} idens tps (Just t)

  instance Checkable (SwitchClause) where
    check (Default stmts) = inScope $ do{ Default <$> (checkList stmts) }
    check (Case es stmts) = Case <$> (checkList es) <*> (inScope $ checkList stmts)

  instance Checkable Expression where
    check (BinaryOp () op l r) = do -- check op type
      lh <- check l
      rh <- check r
      let lhtp = typeOf rh
          rhtp = typeOf lh
      lhraw <- rawType lhtp
      case op of
        _ | lhraw == TString -> when ((lhtp /= rhtp) && op == Plus)  $ throwError (InvalidOpType Plus lh rh)
        a | isCmpOp a -> when (lhtp /= rhtp) $ throwError (InvalidOpType a lh rh) -- check that non empty fields are eq, check assignabliity
        a | isOrdOp a -> when ((lhtp /= rhtp) || not (isOrd lhraw)) $ throwError (InvalidOpType a lh rh)
        a | isNumOp a -> when ((lhtp /= rhtp) || not (isNum lhraw)) $ throwError (InvalidOpType a lh rh)
        a | isIntOp a -> when ((lhtp /= rhtp) || not (isInt lhraw)) $ throwError (InvalidOpType a lh rh)
        a             -> when ((lhtp /= rhtp) || not (TBool==lhraw)) $ throwError (InvalidOpType a lh rh)
      return $ BinaryOp (binOpAnn op lhtp lhraw) op lh rh
    check (UnaryOp () op r) = do -- check op type
      rh <- check r
      case op of
        Neg   -> when (not$ isNum (truety $ annOf rh)) $ throwError (InvalidUOp op rh)
        Pos   -> when (not$ isNum (truety $ annOf rh)) $ throwError (InvalidUOp op rh)
        Not   -> when (not$ (==TBool) (truety $ annOf rh)) $ throwError (InvalidUOp op rh)
        BComp -> when (not$ isInt (truety $ annOf rh))     $ throwError (InvalidUOp op rh)
      return $ UnaryOp (ann $ typeOf rh) op rh
    check (Conversion () tp r) = do
      rh <- check r
      tTp <- rawType $ tp
      trh <- rawType $ typeOf rh
      alTp <- alias tp
      let a = Ann{ty = alTp, truety = tTp}
      let prims = [TInteger, TFloat, TBool, TRune]
      when (not (elem tTp prims || elem trh prims)) $ throwError $ InvalidCast tp (typeOf rh)
      return $ Conversion a tp rh
    check (Selector () l i) = do
      lh <- check l
      rawLh <- rawType $ typeOf lh
      case field rawLh i of
        Nothing -> throwError (MissingField (typeOf lh) i)
        Just tp -> do
          a <- alias tp
          t <- rawType tp
          return $ Selector Ann{ty = a, truety = t} lh i
    check (Index () l ind) = do
      lh  <- check l
      idx <- check ind
      tp <- case (truety $ annOf lh, typeOf idx) of
          (Slice   t, TInteger) -> return t
          (Array _ t, TInteger) -> return t
          _ -> throwError $ InvalidIndex (typeOf lh) (typeOf idx)
      a <- alias tp
      t <- rawType tp
      return $ Index (Ann{ ty = a, truety = t}) lh idx
    check (Arguments () (Name () "println") a) = do
      a' <- checkList a
      at <- mapM (rawType . typeOf) a'
      when (not $ all (isPrim) at) $ throwError InvalidPrint
      return $ Arguments (ann TNil) (Name (ann TNil) "println") a'
    check (Arguments () (Name () "print")  a) = do
      a' <- checkList a
      at <- mapM (rawType . typeOf) a'
      when (not $ all (isPrim) at) $ throwError InvalidPrint
      return $ Arguments (ann TNil) (Name (ann TNil) "println") a'
    check (Arguments () (Name () "append") a) = do
      a' <- checkList a
      if length a' /= 2 then (throwError InvalidAppend) else (return ())
      tp' <- rawType $ typeOf (head a')
      let altp = ann $ typeOf (head a')
      tp <- case tp' of
        Slice t -> alias t
        _ -> throwError InvalidAppend
      when (tp /= (typeOf $ last a')) $ throwError $ InvalidAppend
      return $ Arguments (altp) (Name (ann TNil) "append") a'
    check (Arguments () l args) = do
      lh <- check l
      case (typeOf lh , lh) of
        (Function (Signature farg tp),_) -> do
          rh <- checkList args
          if (map typeOf rh) /= (funcTypes farg)
          then throwError $ InvalidFuncCall
          else return $ Arguments (Ann{ty = head $ funcTypes tp, truety =head $ funcTypes tp}) lh rh
        (t, Name _ n) -> if not (isAlias t || isPrim t) 
          then  throwError $  InvalidFuncCall
          else if length args == 1
          then check (Conversion () (TypeName n) (head args))
          else throwError $ TypeError "Too many args for conversion"
        (_,_) -> throwError $ InvalidFuncCall
      
    check (Name () nm) = Name <$> (checkVar nm) <*> (return nm)
    check (Integer i) = return $ Integer i
    check (Float f)   = return $ Float f
    check (IntString s) = return $ IntString s
    check (RawString s) = return $ RawString s
    check (Rune r) = return $ Rune r
    check (Bool b) = return $ Bool b
    check (SimpleSlice _ _ _ _) = throwError $ ImpossibleError "Not Supported"
    check (FullSlice _ _ _ _ _) = throwError $ ImpossibleError "Not Supported"
    check (QualName _ _) = throwError $ ImpossibleError "Not Supported"

  funcTypes :: [Parameter] -> [Type]
  funcTypes = concat . map (\(Parameter idens typ) -> replicate (if (length idens) == 0 then 1 else (length idens)) typ)

  runCheck :: String -> Env -> Check a -> (Either MGCError a, (String, Counters, Env))
  runCheck s f =  flip runState (s, C{ret = TNil,typCnt = 0}, f) . liftM (transLeft Typechecker) . runExceptT

  typecheck :: Checkable a => (a ()) -> (Either MGCError (a Ann), (String, Counters, Env))
  typecheck = runCheck "" [] . check

  add f e n t = do
    exists <- decl n
    truety <- rawType t
    if exists
    then throwError $ e n
    else modify (f n Ann{ty = t, truety = truety})

  addType :: Identifier -> Type -> Check ()
  addType = add addType' RedeclaredType
  
  addVar :: Identifier -> Type -> Check ()
  addVar  = add addVar'  RedeclaredVar

  addType' :: Identifier -> Ann -> (String, Counters, Env) -> (String, Counters, Env)
  addType' i t (l, tp, x:xs) = (l,(tp { typCnt = (typCnt tp) + 1}), newX:xs)
    where incNm = i++"."++(show $ typCnt tp)
          incTp = TypeName incNm
          newX  = (M.insert i (Ann{ ty = incTp, truety = ty t}) (M.insert incNm t x))
  addType' _ _ (l, tp,   []) = (l, tp, [])

  addVar' :: Identifier -> Ann -> (String, Counters, Env) -> (String, Counters, Env)
  addVar' i t (l, tp, x:xs) = (l, tp, (M.insert i t x):xs)
  addVar' _ _ (l, tp, [])   = (l, tp, [])

  assign :: (Expression Ann) -> (Expression Ann) -> Check Bool
  assign a b = do
    ra <- rawType $ typeOf a
    rb <- rawType $ typeOf b
    return $ (typeOf a == typeOf b) || (ra == typeOf b) || (rb == typeOf a)

  isPrim :: Type -> Bool
  isPrim TInteger = True
  isPrim TFloat   = True 
  isPrim TString  = True
  isPrim TRune    = True
  isPrim TBool    = True
  isPrim _        = False

  isAlias :: Type -> Bool
  isAlias (TypeName _)  = True
  isAlias _ = False

  binOpAnn :: BinOp -> Type -> Type -> Ann
  binOpAnn Plus t TString     = Ann{ty = t, truety = TString}
  binOpAnn o t tt | isIntOp o = Ann{ty = if tt == TInteger then t else TInteger, truety = TInteger}
                  | isNumOp o = Ann{ty = t, truety = tt}
  binOpAnn _ t tt             = Ann{ty= if tt == TBool then t else TBool, truety = TBool}

  hasType t h exp = if (truety $ annOf exp) == t then return exp else h

  rawType :: Type -> Check Type
  rawType (TypeName t) = (checkVar t) >>= (\x -> rawType $ ty x)
  rawType a = return $ a

  alias :: Type -> Check Type
  alias (TypeName t) = ty <$> checkVar t
  alias a = return a

  typeList :: [Expression Ann] -> [Type]
  typeList (x:xs) = case typeOf x of 
    ReturnType ts -> ts ++ (typeList xs)
    t -> t:(typeList xs)
  typeList [] = []

  typeOf :: (Expression Ann) -> Type
  typeOf (Integer _)        = TInteger
  typeOf (Float _)          = TFloat 
  typeOf (IntString _)      = TString
  typeOf (RawString _)      = TString
  typeOf (Rune _)           = TRune
  typeOf (Bool _)           = TBool
  typeOf (Selector a _ _)   = ty a
  typeOf (Conversion a _ _) = ty a
  typeOf (BinaryOp a _ _ _) = ty a
  typeOf (UnaryOp a _ _)    = ty a
  typeOf (Index a _ _)      = ty a
  typeOf (Arguments a _ _)  = ty a
  typeOf (Name a _)         = ty a
  typeOf (SimpleSlice a _ _ _) = ty a
  typeOf (FullSlice a _ _ _ _) = ty a
  typeOf (QualName _ _) = TInteger
  
  ttOf :: (Expression Ann) -> Type
  ttOf = truety . annOf
  
  annOf :: (Expression Ann) -> Ann
  annOf (Integer _)        = ann TInteger
  annOf (Float _)          = ann TFloat 
  annOf (IntString _)      = ann TString
  annOf (RawString _)      = ann TString
  annOf (Rune _)           = ann TRune
  annOf (Bool _)           = ann TBool
  annOf (Selector a _ _)   = a
  annOf (Conversion a _ _) = a
  annOf (BinaryOp a _ _ _) = a
  annOf (UnaryOp a _ _)    = a
  annOf (Index a _ _)      = a
  annOf (Arguments a _ _)  = a
  annOf (Name a _)         = a
  annOf (SimpleSlice a _ _ _) = a
  annOf (FullSlice a _ _ _ _) = a
  annOf (QualName _ _) = ann TInteger

  field :: Type -> Identifier -> (Maybe Type)
  field (Struct decs) ident = listToMaybe $ mapMaybe (\x -> case x of 
    NamedField nms tp _ -> if any ((==) ident) nms
      then Just tp
      else Nothing
    _ -> Nothing) decs
  field _ _ = Nothing

  decl :: String -> Check Bool
  decl var = do
    (_, _, env) <- get
    case env of
      [] -> return False
      (x:_) -> return $ M.member var x

  checkVar :: String -> Check Ann
  checkVar var = do
    (log, ret, env) <- get
    case env of
      [] -> throwError $ NotInScope var 
      (x:xs) -> case M.lookup var x of
        Nothing -> do
          put (log, ret, xs)
          tp <- checkVar var
          put (log, ret, env)
          return tp
        (Just t) -> return t

  inScope :: Check a -> Check a
  inScope c = do{pushScope; r <- c; popScope; return r} 

  pushScope :: Check ()
  pushScope = do
    (log, tp, env) <- get
    put (log, tp, M.empty:env)

  showEnv :: (Map String Ann) -> String
  showEnv e = unlines $ map aux $ M.assocs e
    where aux (i,t) = i ++ ": " ++ (show $ ty t)

  popScope  :: Check ()
  popScope = do
    (log, tp, env) <- get
    let log' = case env of
               e:_ -> log ++ "---Frame---\n" ++ showEnv e
               [] -> log
    case env of
      _:xs -> put (log', tp, xs)
      [] -> return ()