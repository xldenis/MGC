{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MGC.Check where
  import qualified Data.Map as M
  import Data.Map (Map)
  import MGC.Syntax

  import Data.Maybe (listToMaybe, mapMaybe)
  import Control.Monad.State
  import Control.Monad.Except
  import Control.Applicative ((<$>), (<*>), (<*))

  type Env = [Map String Type]

  data TypeError 
    = TypeError String
    | ImpossibleError String
    | InvalidAppend
    | InvalidCase [Expression Type] Type
    | InvalidCast Type Type
    | InvalidCondition (Expression Type)
    | InvalidFuncCall
    | InvalidIncDec (Expression Type) Type
    | InvalidIndex Type Type
    | InvalidOpType BinOp (Expression Type) (Expression Type)
    | InvalidPrint
    | InvalidReturn
    | InvalidFor
    | InvalidUOp UOp (Expression Type)
    | InvalidVarDec Type [Expression Type] 
    | Mismatch Type Type 
    | MissingField Type Identifier
    | NoNewVars
    | NotInScope String
    | RedeclaredType String
    | RedeclaredVar String deriving Show

  type Check = ExceptT TypeError (State (String, Counters, Env))
  type Ann = Type

  data Counters= C{ret :: Type, typCnt :: Int}

  class Checkable a where
    check :: (a ()) -> Check (a Ann)
    checkList :: [a ()] -> Check [a Ann]
    checkList = mapM (check)

  instance Checkable Package where
    check (Package name pkg) = Package name <$> (pushScope >> (checkList pkg) <* popScope)

  instance Checkable TopLevelDeclaration where
    check (Decl t@(TypeDecl _)) = Decl <$> (check t)
    check (Decl v@(VarDecl  _)) = Decl <$> (check v)
    check (FunctionDecl name sig body) = do
      addVar name $ Function sig
      pushScope
      mapM (\(Parameter idens tp) -> mapM (\nm -> addVar nm tp) idens) $ (\(Signature s _) -> s) sig
      let ret = map (\(Parameter _ tp) -> tp) $ (\(Signature _ s) -> s) sig
      (a,tp,c) <- get
      put (a, (tp {ret = (ReturnType ret)}), c)
      b <- case body of
        Block s -> Block <$> (checkList s)
        Empty -> return Empty
        _ -> throwError $ ImpossibleError "Invalid function body"
      popScope
      put (a, tp, c)
      return $ FunctionDecl name sig b
    check (Decl _ ) = throwError $ ImpossibleError "Top Level Declaration invalid"

  instance Checkable Statement where
    check (TypeDecl tps) = do
      mapM (\(TypeSpec n tp) -> addType n tp) tps
      return $ TypeDecl tps
    check (VarDecl vars) = do
      tps <- checkList vars
      return $ VarDecl tps
    check (Return exps) = do
      (_, tp, _) <- get
      e <- checkList exps
      case (ret tp) of
        ReturnType t -> when (t /= (map typeOf e)) $ throwError $ InvalidReturn 
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
      case (typeOf e) of
        TRune -> return $ Inc e
        TString -> return $ Inc e
        TFloat -> return $ Inc e
        t -> throwError $ InvalidIncDec e t
    check (Dec exp) = do
      e <- check exp
      case (typeOf e) of
        TRune -> return $ Inc e
        TString -> return $ Inc e
        TFloat -> return $ Inc e
        t -> throwError $ InvalidIncDec e t
    check (Assignment Eq lh rh) = do
      lhs <- checkList lh
      rhs <- checkList rh
      mapM (\(l, r) -> when ((typeOf l) /= (typeOf r)) $ throwError (Mismatch (typeOf l) (typeOf r))) $ zip lhs rhs
      return $ Assignment Eq  lhs rhs
    check (Assignment op lh rh) = do --accept += string
      lhs <- checkList lh
      rhs <- checkList rh
      mapM (\(l, r) -> when ((typeOf l) /= (typeOf r)) $ throwError (Mismatch (typeOf l) (typeOf r))) $ zip lhs rhs
      alLhs <- alias $ typeOf $ head lhs
      if isNumOp op
      then when (not $ isNum $ alLhs) $ throwError (InvalidOpType op (head lhs) (head rhs))
      else when (not $ isInt $ alLhs) $ throwError (InvalidOpType op (head lhs) (head rhs))
      return $ Assignment op lhs rhs
    check (ShortDecl idents exps) = do
      decls <- mapM decl idents
      when (all (==True) decls) (throwError NoNewVars)
      e <- checkList exps
      mapM (\(d, id, e) -> case d of 
        True -> do
          v <- checkVar id
          if v == (typeOf e)
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
    check (VarSpec idens exps Nothing) = do -- dont ignore tp check double declarations
      tps <- checkList exps
      mapM (\(i, t) ->addVar i (typeOf t) ) $ zip idens tps
      return $ VarSpec idens tps Nothing
    check (VarSpec idens exps (Just t)) = do
      tps <- checkList exps
      trueT <- alias t
      when (any ((trueT /=).typeOf ) tps) $ throwError $ InvalidVarDec t tps
      mapM (\i -> addVar i trueT ) idens
      return $ VarSpec idens tps (Just t)


  instance Checkable (SwitchClause) where
    check (Default stmts) = Default <$> (checkList stmts)
    check (Case es stmts) = Case <$> (checkList es) <*> (checkList stmts)

  instance Checkable Expression where
    check (BinaryOp () op l r) = do -- check op type
      lh <- check l
      rh <- check r

      let lhtp = typeOf rh
          rhtp = typeOf lh
      lhraw <- rawType lhtp
      case op of
        a | isCmpOp a -> when ((lhtp /= rhtp)) $ throwError (InvalidOpType a lh rh) -- check that non empty fields are eq, check assignabliity
        a | isOrdOp a -> when ((lhtp /= rhtp) || not (isOrd lhraw)) $ throwError (InvalidOpType a lh rh)
        a | isNumOp a -> when ((lhtp /= rhtp) || not (isNum lhraw)) $ throwError (InvalidOpType a lh rh)
        a | isIntOp a -> when ((lhtp /= rhtp) || not (isInt lhraw)) $ throwError (InvalidOpType a lh rh)
        a             -> when ((lhtp /= rhtp) || not (TBool==lhraw)) $ throwError (InvalidOpType a lh rh)
      return $ BinaryOp (binOpTp op lhtp) op lh rh
    check (UnaryOp () op r) = do -- check op type
      rh <- check r
      case op of
        Neg   -> when (not$ isNum (typeOf rh)) $ throwError (InvalidUOp op rh)
        Pos   -> when (not$ isNum (typeOf rh)) $ throwError (InvalidUOp op rh)
        Not   -> when (not$ (==TBool) (typeOf rh)) $ throwError (InvalidUOp op rh)
        BComp -> when (not$ isInt (typeOf rh))     $ throwError (InvalidUOp op rh)
      return $ UnaryOp (typeOf rh) op rh
    check (Conversion () tp r) = do
      rh <- check r
      tTp <- rawType $ tp
      trh <- rawType $ typeOf rh
      alTp <- alias tp
      let prims = [TInteger, TFloat, TBool, TRune]
      when (not (elem tTp prims || elem trh prims)) $ throwError $ InvalidCast tp (typeOf rh)
      return $ Conversion alTp tp rh
    check (Selector () l i) = do
      lh <- check l
      rawLh <- rawType $ typeOf lh
      case field rawLh i of
        Nothing -> throwError (MissingField (typeOf lh) i)
        Just tp -> return $ Selector tp lh i
    check (Index () l ind) = do
      lh <- check l
      idx <- check ind
      tp <- case (typeOf lh, typeOf idx) of
          (Slice   t, TInteger) -> return t
          (Array _ t, TInteger) -> return t
          _ -> throwError $ InvalidIndex (typeOf lh) (typeOf idx)
      return $ Index tp lh idx
    --check (SimpleSlice () l b t)
    --check (FullSlice () l b d t)
    check (Arguments () (Name () "println") a) = do
      a' <- checkList a
      at <- mapM (rawType . typeOf) a'
      when (not $ all (isPrim) at) $ throwError InvalidPrint
      return $ Arguments TNil (Name TNil "println") a'
    check (Arguments () (Name () "print")  a) = do
      a' <- checkList a
      at <- mapM (rawType . typeOf) a'
      when (not $ all (isPrim) at) $ throwError InvalidPrint
      return $ Arguments TNil (Name TNil "println") a'
    check (Arguments () (Name () "append") a) = do
      a' <- checkList a
      if length a' /= 2 then (throwError InvalidAppend) else (return ())
      tp' <- rawType $ typeOf (head a')
      let altp=  typeOf (head a')
      tp <- case tp' of
        Slice t -> return t
        _ -> throwError InvalidAppend
      when (tp /= (typeOf $ last a')) $ throwError InvalidAppend
      return $ Arguments (altp) (Name TNil "append") a'
    check (Arguments () l args) = do
      lh <- check l
      case (typeOf lh , lh) of
        (Function (Signature farg tp),_) -> do
          rh <- checkList args
          if (map typeOf rh) /= (funcTypes farg)
          then throwError $ InvalidFuncCall
          else return $ Arguments (ReturnType $ funcTypes tp) lh rh
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

  funcTypes :: [Parameter] -> [Type]
  funcTypes = concat . map (\(Parameter idens typ) -> replicate (if (length idens) == 0 then 1 else (length idens)) typ)

  runCheck :: String -> Env -> Check a -> (Either TypeError a, (String, Counters, Env))
  runCheck s f = flip runState (s, C{ret = TNil,typCnt = 0}, f) . runExceptT

  typecheck :: Checkable a => (a ()) -> (Either TypeError (a Ann), (String, Counters, Env))
  typecheck = runCheck "" [] . check

  add f e n t = do
    exists <- decl n
    if exists
    then throwError $ e n
    else modify (f n t)

  addType :: Identifier -> Type -> Check ()
  addType = add addType' RedeclaredType
  addVar :: Identifier -> Type -> Check ()
  addVar  = add addVar'  RedeclaredVar

  addType' :: Identifier -> Type -> (String, Counters, Env) -> (String, Counters, Env)
  addType' i t (l, tp, x:xs) = (l,(tp { typCnt = (typCnt tp) + 1}), (M.insert i (TypeName incTp) (M.insert incTp t x)):xs)
    where incTp = i++"$"++(show $typCnt tp)
  addType' i t (l, tp,   []) = (l, tp, [])

  addVar' :: Identifier -> Type -> (String, Counters, Env) -> (String, Counters, Env)
  addVar' i t (l, tp, x:xs) = (l, tp, (M.insert i t x):xs)
  addVar' i t (l, tp, [])   = (l, tp, [])

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

  binOpTp :: BinOp -> Type -> Type
  binOpTp o t | isIntOp o = TInteger
              | isNumOp o = t
  binOpTp _ _             = TBool

  hasType t h exp = if typeOf exp == t then return exp else h

  rawType :: (Type) -> Check Type
  rawType (TypeName t) = (checkVar t) >>= (\x -> rawType x)
  rawType a = return $ a

  alias (TypeName t) = checkVar t
  alias a = return a

  typeOf :: (Expression Ann) -> Type
  typeOf (Integer _)        = TInteger
  typeOf (Float _)          = TFloat 
  typeOf (IntString _)      = TString
  typeOf (RawString _)      = TString
  typeOf (Rune _)           = TRune
  typeOf (Bool _)           = TBool
  typeOf (Selector t _ _)   = t
  typeOf (Conversion t _ _) = t
  typeOf (BinaryOp t _ _ _) = t
  typeOf (UnaryOp t _ _)    = t
  typeOf (Index t _ _)      = t
  typeOf (Arguments t _ _)  = t
  typeOf (Name t _)         = t

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

  checkVar :: String -> Check Type
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

  pushScope :: Check ()
  pushScope = do
    (log, tp, env) <- get
    put (log, tp, M.empty:env)

  showEnv :: (Map String Type) -> String
  showEnv e = unlines $ map aux $ M.assocs e
    where aux (i,t) = i ++ ": " ++ show t

  popScope  :: Check ()
  popScope = do
    (log, tp, env) <- get
    let log' = case env of
               e:_ -> log ++ "---Frame---\n" ++ showEnv e
               [] -> log
    case env of
      _:xs -> put (log', tp, xs)
      [] -> return ()