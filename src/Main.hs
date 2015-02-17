import System.Environment
import MGC.Parser
import Text.Parsec

import MGC.Syntax.Pretty

import Control.Applicative ((<$>), (<*))

main :: IO ()
main = do
  args <- getArgs
  ast  <- (parse (package <* eof) "") <$> readFile (head args)
  case ast of 
    Left a -> putStrLn $ show a
    Right ast -> do 
      putStrLn $ prettyShow $ pPrint ast
