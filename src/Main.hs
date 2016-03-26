{-# LANGUAGE DeriveDataTypeable #-}
import MGC.Parser
import Text.Parsec
import System.FilePath.Posix
import System.Console.CmdArgs
import System.Environment (withArgs, getArgs)

import MGC.Syntax.Pretty
import MGC.Check
import MGC.Syntax.Weeder (runWeeder)
import MGC.Emit (emit)

import Control.Applicative ((<$>), (<*))
import Control.Monad (liftM, when)

data Options = Options {astPrint :: Bool, files :: [String], test :: Bool, dumpsymtab :: Bool, pptype :: Bool, fmt :: Bool } deriving (Show, Data, Typeable)


opts = Options{
  astPrint = False &= help "Print ast info",
  test = False &= help "Test directories",
  dumpsymtab = False &= help "Print current scope on exit",
  pptype = False &= help "Print type information in output",
  fmt = False &= help "Format go code in .pretty.go",
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
  if (test args) then (mapM handleFile (files args)) >> (return ()) else compile (head $ files args) args

compile :: String -> Options -> IO ()
compile fname args = do
  ast <- liftM (parseProgram Control.Monad.>=> runWeeder) $ readFile fname
  case ast of
    Left err -> print err
    Right ast ->
      case (typecheck ast) of
        (Left err, (l,_,_)) -> do
          putStrLn $ show err ++ "\n" ++  l
          when (dumpsymtab args) $ saveFile fname "symtab" l
        (Right typedAst, (l,_,_)) -> do
          when (astPrint args) $ putStrLn $ show typedAst
          when (dumpsymtab args) $ saveFile fname "symtab" l
          when (pptype args) $ saveFile fname "pptype.go" (prettyShow $ pretty typedAst)
          when (fmt args) $ saveFile fname "pretty.go" $ prettyShow $ pretty ast
          emit typedAst (dropExtension fname)


saveFile :: String -> String -> String -> IO ()
saveFile f e d = writeFile (replaceExtension f e) d

handleFile :: String -> IO ()
handleFile fname = do
  ast  <- parse (package <* eof) "" <$> readFile fname
  case ast of
    Left _ -> putStrLn $ fname ++ " got ParseError"
    Right ast -> case runWeeder ast of
      Left err -> putStrLn $ fname ++ " got " ++ show err
      Right ast -> do
        putStr $ fname ++ " " ++ "parsed "
        case typecheck ast of
          (Right _,(l,_,_ ))-> putStrLn $ "typechecked" ++ "\n"
          (Left err, (l,_,_)) -> putStrLn $ show err ++ "\n"
