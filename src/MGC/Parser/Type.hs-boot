module MGC.Parser.Type where
  import Text.Parsec.String
  import MGC.Syntax (Type(..))
  typeParser :: Parser Type