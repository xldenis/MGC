{-# LANGUAGE DeriveDataTypeable #-}
import MGC.Parser
import Text.Parsec
import System.FilePath.Posix
import System.Console.CmdArgs

import MGC.Syntax.Pretty

import Control.Applicative ((<$>), (<*))

data Options = Options {astPrint :: Bool, files :: [String]} deriving (Show, Data, Typeable)


opts = Options{astPrint = False &= help "Print ast info", files = [] &= args }
main :: IO ()
main = do
  args <- cmdArgs opts
  let fname = head (files args)

  ast  <- (parse (package <* eof) "") <$> readFile (fname)
  case ast of 
    Left a -> putStrLn $ show a
    Right ast -> do 
      case (astPrint args) of
        True -> putStrLn $ show $ ast
        _ -> return ()
      putStrLn $ prettyShow $ pretty ast
      writeFile (replaceExtension (fname) "pretty.go") $ prettyShow $ pretty ast
