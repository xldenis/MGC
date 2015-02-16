import System.Environment
import MGC.Parser
import Text.Parsec

import Control.Applicative ((<$>), (<*))

main :: IO ()
main = do
  args <- getArgs
  ast  <- (parse (package <* eof) "") <$> readFile (head args)
  print ast
