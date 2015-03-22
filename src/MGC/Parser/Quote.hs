module MGC.Parser.Quote where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import MGC.Parser

import Control.Applicative

import Text.Parsec
import Text.Parsec.Pos

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

tld :: QuasiQuoter
tld = QuasiQuoter {
    quoteExp =  \str -> do
        l <- location'
        c <- runIO $ parseTest (setPosition l *> topLevelDef) str
        dataToExpQ (const Nothing) c
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
}