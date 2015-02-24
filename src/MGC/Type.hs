module MGC.Type where
  import Data.Map
  import MGC.Syntax

  import Control.Monad.Except

  type Env = [Map (String, Type)]


  Check e w a = ExceptT e (Writer w) a

  typecheck :: [TopLevelDeclaration] -> Check Type String TypeError
  typecheck = return TInt