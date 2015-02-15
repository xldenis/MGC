import System.Environment
import MGC.Parser
import Text.Parsec

import Control.Applicative ((<$>))

main :: IO ()
main = do
  args <- getArgs
  ast  <- (parse topLevelDef "") <$> readFile (head args)
  print ast
