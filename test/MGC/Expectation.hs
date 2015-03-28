module MGC.Expectation where
  import Test.Hspec.Expectations
  import Text.Parsec.String
  import Text.Parsec
  import MGC.Syntax (Type(..), Expression(..))
  import MGC.Check (typeOf, Ann)
  import Control.Monad (unless)

  parses :: Parser a -> String -> Either ParseError a
  parses par str = parse par "" str

  (~>) :: (Show a, Eq a, Show b) => Either b a -> a -> Expectation 
  res ~> want = case res of
      Right a -> unless (a == want) $ expectationFailure $ "got "++show a++" expected "++show want
      Left b -> expectationFailure $ show b

  success :: Either a b -> (b -> Expectation) -> Expectation
  success res f = case res of
      Right c -> f c
      Left _ -> expectationFailure $ "operation failed"

  hastype :: Type -> Expression Ann -> Expectation
  hastype t e = unless (typeOf e == t) $ expectationFailure  $ "got "++show (typeOf e)++" expected "++show t

  --(/~>) :: (Show b, Eq b, Show a) => Either b a -> b -> Expectation
  --res /~> want = case res of
  --  Left b -> unless (b == want) $ expectationFailure $ "got "++show b++" expected "++show want
  --  Right _ -> expectationFailure "operation succeeded"

  wontParse :: Parser a -> String -> Expectation
  wontParse par str = case parse par "" str of
    Right _ -> expectationFailure $ "input `"++ str ++ "` shouldnt have parsed."
    _ -> return ()