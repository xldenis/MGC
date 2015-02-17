import System.Environment
import MGC.Parser
import Text.Parsec
import System.FilePath.Posix

import Control.Monad
import MGC.Syntax.Pretty

import Control.Applicative ((<$>), (<*))

main :: IO ()
main = do
  args <- getArgs
  let fname = head args
  ast  <- (parse (package <* eof) "") <$> readFile (fname)
  case ast of 
    Left a -> putStrLn $ show a
    Right ast -> do 
      putStrLn $ prettyShow $ pPrint ast
      writeFile (replaceExtension (fname) "pretty.go") $ prettyShow $ pPrint ast
