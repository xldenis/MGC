{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MGC.Type where
  import qualified Data.Map as M
  import Data.Map (Map)
  import MGC.Syntax

  import Data.Maybe (isNothing)
  import Control.Monad.State
  import Control.Monad.Except
  import Control.Applicative ((<$>), (<*>), (<*))

  type Env = [Map String Type]

  data TypeError 
    = TypeError
    | NoNewVars
    | NotInScope String
    | MissingField Type Identifier
    | InvalidIndex Type Type
    | InvalidFuncCall
    | InvalidIncDec (Expression Type) Type
    | ImpossibleError String
    | InvalidOpType BinOp (Expression Type) (Expression Type)
    | InvalidUOp UOp (Expression Type)
    | InvalidReturn
    | InvalidVarDec Type [Expression Type] 
    | RedeclaredVar
    | Mismatch Type Type deriving Show

  type Check = ExceptT TypeError (State (String, Type, Env))
  type Ann = Type

  class Checkable a where
    check :: (a ()) -> Check (a Ann)
    checkList :: [a ()] -> Check [a Ann]
    checkList = mapM (check)

  instance Checkable Package where
    check (Package name pkg) = Package name <$> (checkList pkg)

  instance Checkable TopLevelDeclaration where
    check (Decl t@(TypeDecl _)) = Decl <$> (check t)
    check (Decl v@(VarDecl  _)) = Decl <$> (check v)
    check (FunctionDecl name sig body) = do
      modify (addType name $ Function sig)
      pushScope
      mapM (\(Parameter idens tp) -> mapM (\nm -> modify (addType nm tp)) idens) $ (\(Signature s _) -> s) sig
      b <- case body of
        Block s -> Block <$> (mapM check s)
        Empty -> return Empty
        _ -> throwError $ ImpossibleError "Invalid function body"
      popScope
      return $ FunctionDecl name sig b
    check (Decl _ ) = throwError $ ImpossibleError "Top Level Declaration invalid"

  instance Checkable Statement where
    check (TypeDecl tps) = do
      mapM (\(TypeSpec n tp) -> modify $ addType n tp) tps
      return $ TypeDecl tps
    check (VarDecl vars) = do
      tps <- mapM check vars
      return $ VarDecl tps
    check (Return exps) = do
      (_, tp, _) <- get
      e <- checkList exps
      case tp of
        ReturnType t -> when (t /= (map typeOf e)) $ throwError $ InvalidReturn
        _ -> throwError $ ImpossibleError "got non return type as function return"
      return $ Return e
    check (If (Just stmt) exp l r) = do -- check type
      pushScope
      s <- check stmt
      e <- check exp
      lh <- check l
      rh <- check r
      popScope
      return $ If (Just s) e lh rh
    check (If Nothing exp l r) = If Nothing <$> (check exp) <*> (check l) <*> (check r) -- check exp type
    check (For (Just cond) body) = do -- check type
      pushScope
      c <- check cond
      b <- check body
      popScope
      return $ For (Just c) b
    check (For Nothing body) = For Nothing <$> (check body) -- check type
    check (Switch (Just stmt) (Just exp) cls) = do -- check type
      pushScope
      s <- check stmt
      e <- check exp
      cls' <- mapM check cls
      popScope
      return $ Switch (Just s) (Just e) cls'
    check (Block stmts) = do
      pushScope
      s <- mapM check stmts
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
    check (Assignment op lh rh) = do
      lhs <- mapM check lh
      rhs <- mapM check rh
      mapM (\(l, r) -> when ((typeOf l) /= (typeOf r)) $ throwError (Mismatch (typeOf l) (typeOf r))) $ zip lhs rhs
      if isNumOp op
      then when (not $ isNum $ typeOf (head lhs)) $ throwError (InvalidOpType op (head lhs) (head rhs))
      else when (not $ isInt $ (typeOf (head lhs))) $ throwError (InvalidOpType op (head lhs) (head rhs))
      return $ Assignment op lhs rhs
    check (ShortDecl idents exps) = do
      decls <- mapM decl idents
      when (all (==True) decls) (throwError NoNewVars)
      e <- mapM check exps
      mapM (\(d, id, e) -> case d of 
        True -> do
          v <- checkVar id
          if v == (typeOf e)
          then return (typeOf e)
          else throwError NoNewVars
        False -> modify (addType id (typeOf e)) >> return (typeOf e) ) $ zip3 decls idents e
      return $ ShortDecl idents e
    check (Empty) = return Empty
    check (Continue) = return Continue
    check (Break) = return Break
    check (Fallthrough) = return Fallthrough

  instance Checkable ForCond where
    check (Condition e)            = Condition <$> (check e)
    check (ForClause s (Just a) p) = ForClause <$> (check s) <*> (Just <$> (check a)) <*> (check p)
    check (ForClause s Nothing p)  = ForClause <$> (check s) <*> (return Nothing) <*> (check p)
  
  instance Checkable VarSpec where
    check (VarSpec idens exps Nothing) = do -- dont ignore tp check double declarations
      tps <- mapM check exps
      mapM (\(i, t) -> modify (addType i (typeOf t)) ) $ zip idens tps
      return $ VarSpec idens tps Nothing
    check (VarSpec idens exps (Just t)) = do
      tps <- mapM check exps
      when (any (/=t) (map typeOf tps)) $ throwError $ InvalidVarDec t tps
      mapM (\i -> modify (addType i t) ) idens
      return $ VarSpec idens tps (Just t)


  instance Checkable (SwitchClause) where
    check (Default stmts) = Default <$> (mapM check stmts)
    check (Case es stmts) = Case <$> (checkList es) <*> (checkList stmts)

  instance Checkable Expression where
    check (BinaryOp () op l r) = do -- check op type
      lh <- check l
      rh <- check r

      let lhtp = typeOf rh
          rhtp = typeOf lh
        in case op of
        a | isCmpOp a -> when ((lhtp /= rhtp)) $ throwError (InvalidOpType a lh rh) -- check that non empty fields are eq
        a | isOrdOp a -> when ((lhtp /= rhtp) || not (isOrd lhtp)) $ throwError (InvalidOpType a lh rh)
        a | isNumOp a -> when ((lhtp /= rhtp) || not (isNum lhtp)) $ throwError (InvalidOpType a lh rh)
        a | isIntOp a -> when ((lhtp /= rhtp) || not (isInt lhtp)) $ throwError (InvalidOpType a lh rh)
        a             -> when ((lhtp /= rhtp) || not (TBool==lhtp)) $ throwError (InvalidOpType a lh rh)
      return $ BinaryOp (typeOf rh) op lh rh

    check (UnaryOp () op r) = do -- check op type
      rh <- check r
      case op of
        Neg   -> when (not$ isNum (typeOf rh)) $ throwError (InvalidUOp op rh)
        Pos   -> when (not$ isNum (typeOf rh)) $ throwError (InvalidUOp op rh)
        Not   -> when (not$ (==TBool) (typeOf rh)) $ throwError (InvalidUOp op rh)
        BComp -> when (not$ isInt (typeOf rh))     $ throwError (InvalidUOp op rh)
      return $ UnaryOp (typeOf rh) op rh
    --check (Conversion tp r) = do
    --  rh <- check r
    --  return $ Conversion tp rh
    check (Selector () l i) = do
      lh <- check l
      case field (typeOf lh) i of
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
    check (Arguments () l args) = do
      lh <- check l
      (farg, tp) <- case (typeOf lh) of
        Function (Signature farg out) -> return (farg, out)
        _ -> throwError $ InvalidFuncCall
      rh <- mapM check args
      if (map typeOf rh) /= (funcTypes farg)
      then throwError $ InvalidFuncCall
      else return $ Arguments (ReturnType $ funcTypes tp) lh rh
    check (Name () nm) = Name <$> (checkVar nm) <*> (return nm)
    check (Integer i) = return $ Integer i
    check (Float f)   = return $ Float f
    check (IntString s) = return $ IntString s
    check (RawString s) = return $ RawString s
    check (Rune r) = return $ Rune r
    check (Bool b) = return $ Bool b

  funcTypes :: [Parameter] -> [Type]
  funcTypes = concat . map (\(Parameter idens typ) -> replicate (if (length idens) == 0 then 1 else (length idens)) typ)

  runCheck :: String -> Env -> Check a -> (Either TypeError a, (String, Type, Env))
  runCheck s f = flip runState (s, TNil, f) . runExceptT

  typecheck :: Checkable a => (a ()) -> (Either TypeError (a Ann), (String, Type, Env))
  typecheck = runCheck "" [M.empty] . check

  addType :: Identifier -> Type -> (String, Type, Env) -> (String, Type, Env)
  addType i t (l, tp, f) = case f of
    x:xs -> (l, tp, (M.insert i t x):xs)
    [] -> (l, tp, [])

  rawType :: (Expression Ann) -> Type
  rawType a = TInteger

  typeOf :: (Expression Ann) -> Type
  typeOf (Integer _)        = TInteger
  typeOf (Float _)          = TFloat 
  typeOf (IntString _)      = TString
  typeOf (RawString _)      = TString
  typeOf (Rune _)           = TRune
  typeOf (Bool _)           = TBool
  typeOf (BinaryOp t _ _ _) = t
  typeOf (UnaryOp t _ _)    = t
  typeOf (Index t _ _)      = t
  typeOf (Arguments t _ _)  = t
  typeOf (Name t _)         = t

  field :: Type -> Identifier -> (Maybe Type)
  field (Struct decs) ident = head . (filter isNothing) $ map (\x -> case x of 
    NamedField nms tp tag -> if any ((==) ident) nms
      then Just tp
      else Nothing
    _ -> Nothing) decs
  field _ _ = Nothing

  decl :: String -> Check Bool
  decl var = do
    (log, _, env) <- get
    case env of
      [] -> return False
      (x:xs) -> return $ M.member var x

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
               e:xs -> log ++ "\n" ++ showEnv e
               [] -> log

    case env of
      e:xs -> put (log', tp, xs)
      [] -> return ()