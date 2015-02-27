module MGC.Type where
  import Data.Map
  import MGC.Syntax

  import Control.Monad.Except

  type Env = [Map (String, Type)]

  data TypeError = TypeError

  type Check = ExceptT TypeError (State (String, Env))

  --checkTLD :: Env -> [TopLevelDeclaration] -> Check Type String TypeError
  --checkTLD (Decl (TypeDecl decls)) =
  --checkTLD (Decl (VarDecl  decls)) =  
  --checkTLD (FunctionDecl name sig body) =
  --checkTLD (Decl _) = -- fuck dat shit

  checkStmt :: Statement -> Check Type String TypeError

  checkExpr :: Expression () -> Check (Expression Type)


  lookup :: String -> Check Type
  lookup var = do
    (log, env) <- get
    case env of
      [] -> 
      case lookup x var of
        Nothing -> lookup xs var
        (Just t) -> t

  pushScope :: Env -> Env

  popScope  :: Env -> Env