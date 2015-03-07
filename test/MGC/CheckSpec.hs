module MGC.CheckSpec (spec) where
  import Test.Hspec
  import MGC.Syntax
  import MGC.Expectation
  import MGC.Check

  spec :: Spec
  spec = do
    describe "expression" $ do
      it "checks literals" $ do
        (fst . typecheck) (Integer 0) `success` (hastype TInteger)