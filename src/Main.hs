{-# LANGUAGE DeriveDataTypeable #-}
import MGC.Parser
import Text.Parsec
import System.FilePath.Posix
import System.Console.CmdArgs
import System.Environment (withArgs, getArgs)

import MGC.Syntax.Pretty
import MGC.Check
import MGC.Syntax.Weeder (runWeeder)

import Control.Applicative ((<$>), (<*))
import Control.Monad (liftM)

data Options = Options {astPrint :: Bool, files :: [String], test :: Bool, dumpsymtab :: Bool, pptype :: Bool } deriving (Show, Data, Typeable)


opts = Options{
  astPrint = False &= help "Print ast info", 
  test = False &= help "Test directories", 
  dumpsymtab = False &= help "Print current scope on exit",
  pptype = False &= help "Print type information in output",
  files = [] &= args 
}

munge "-pptype"     = "--pptype"
munge "-dumpsymtab" = "--dumpsymtab"
munge a = a
main :: IO ()
main = do
  realArgs <- getArgs
  withArgs (map munge realArgs) realMain

realMain :: IO ()
realMain = do
  args <- cmdArgs opts
  case (test args) of
    False -> compile (head $ files args) args
    True  -> (mapM handleFile (files args)) >> (return ())

compile :: String -> Options -> IO ()
compile fname args = do
  ast <- (parse (package <* eof) "") <$> readFile (fname)
  case ast of 
    Left a -> putStrLn $ show a
    Right ast -> case runWeeder ast of 
      Left err -> putStrLn $ show err
      Right ast -> do
        case (typecheck ast) of
          (Left err, (l,_,_)) -> do
            putStrLn $ show err ++ "\n" ++  l
            if (dumpsymtab args) then saveFile fname "symtab" l else return ()
          (Right typedAst, (l,_,_)) -> do
            putStrLn l
            putStrLn $ prettyShow $ pretty typedAst
            if (dumpsymtab args) then saveFile fname "symtab" l else return ()
            if (pptype args) then saveFile fname "pptype.go" (prettyShow $ pretty typedAst) else return ()
            saveFile fname "pretty.go" $ prettyShow $ pretty ast

saveFile :: String -> String -> String -> IO ()
saveFile f e d = writeFile (replaceExtension f e) d

handleFile :: String -> IO ()
handleFile fname = do
  ast  <- (parse (package <* eof) "") <$> readFile (fname)
  case ast of 
    Left _ -> putStrLn $ fname ++ " got ParseError"
    Right ast -> case runWeeder ast of 
      Left err -> putStrLn $ fname ++ " got " ++ (show err)
      Right ast -> do
        putStrLn $ fname ++ " " ++ "parsed"
        case (typecheck ast) of
          (Right _,(l,_,_ ))-> putStrLn $ "Typechecked" ++ "\n"++ l
          (Left err, (l,_,_)) -> putStrLn $ show err ++ "\n" ++ l        
