module MGC.Expectation where
  import Test.Hspec.Expectations
  import Text.Parsec.String
  import Text.Parsec
  import Control.Monad (unless)

  parses :: Parser a -> String -> Either ParseError a
  parses par str = parse par "" str

  (~>) :: (Show a, Eq a, Show b) => Either b a -> a -> Expectation 
  res ~> want = case res of
      Right a -> unless (a == want) $ expectationFailure $ "got "++show a++" expected "++show want
      Left b -> expectationFailure $ show b

  wontParse :: Parser a -> String -> Expectation
  wontParse par str = case parse par "" str of
    Right _ -> expectationFailure $ "input `"++ str ++ "` shouldnt have parsed."
    _ -> return ()