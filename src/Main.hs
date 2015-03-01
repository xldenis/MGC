{-# LANGUAGE DeriveDataTypeable #-}
import MGC.Parser
import Text.Parsec
import System.FilePath.Posix
import System.Console.CmdArgs

import MGC.Syntax.Pretty
import MGC.Syntax.Weeder (runWeeder)

import Control.Applicative ((<$>), (<*))

data Options = Options {astPrint :: Bool, files :: [String], test :: Bool } deriving (Show, Data, Typeable)


opts = Options{astPrint = False &= help "Print ast info", test = False &= help "Test directories", files = [] &= args }

main :: IO ()
main = do
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
        case (astPrint args) of
          True -> putStrLn $ show $ pretty ast
          _ -> return ()
        putStrLn $ prettyShow $ pretty ast
        writeFile (replaceExtension (fname) "pretty.go") $ prettyShow $ pretty ast

handleFile :: String -> IO ()
handleFile fname = do
  ast  <- (parse (package <* eof) "") <$> readFile (fname)
  case ast of 
    Left _ -> putStrLn $ fname ++ " got ParseError"
    Right ast -> case runWeeder ast of 
      Left err -> putStrLn $ fname ++ " got " ++ (show err)
      Right _ -> do
        putStrLn $ fname ++ " " ++ "parsed"
        --putStrLn $ prettyShow $ pretty ast
        --writeFile (replaceExtension (fname) "pretty.go") $ prettyShow $ pretty ast
