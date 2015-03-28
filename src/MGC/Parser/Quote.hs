module MGC.Parser.Quote where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import MGC.Parser
import MGC.Syntax.Weeder (runWeeder)
import MGC.Check (runCheck, check)

import Control.Applicative

import Text.Parsec
import Text.Parsec.Pos

import qualified Data.Map as M

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

tld :: QuasiQuoter
tld = QuasiQuoter {
    quoteExp =  \str -> do
        l <- location'
        c <- case parse (setPosition l *> spaces *> topLevelDef) "" str of
          Left e -> error $ show e
          Right a -> case runWeeder a of
            Left e -> error $ show e
            Right a -> case (runCheck "" [M.empty]) $ check a of
              (Left e, _) -> error $ show e 
              (Right a, _) -> return a

        dataToExpQ (const Nothing) c
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
}