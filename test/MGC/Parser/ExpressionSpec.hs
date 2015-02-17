module MGC.Parser.ExpressionSpec (spec) where
  import Test.Hspec
  import MGC.Expectation
  import MGC.Parser.Expression 
  import Text.Parsec
  import MGC.Syntax
  spec :: Spec
  spec = do
    describe "expression" $ do
      it "parses arithmetic expression" $ do
        expression `parses` "0==0" ~> (BinaryOp Eq (Integer 0) (Integer 0))
        expression `parses` "0 == 0 " ~> (BinaryOp Eq (Integer 0) (Integer 0))
        expression `parses` "0&&0" ~> (BinaryOp And (Integer 0) (Integer 0))
      it "parses complex expressions" $ do
        expression `parses` "(0+1).count ^ 0x5" ~> (BinaryOp BitXor (Selector (BinaryOp Plus (Integer 0) $ Integer 1) "count") (Integer 5))
      it "parses different precedences" $ do 
        expression `parses` "0+1^ 2" ~> (BinaryOp BitXor (BinaryOp Plus (Integer 0) (Integer 1)) (Integer 2))
      it "handles parens" $ do
        expression `parses` "(0+1) + 1" ~> BinaryOp Plus (BinaryOp Plus (Integer 0) $ Integer 1) (Integer 1)
      it "parses array indices" $ do
        expression `parses` "0[0]" ~> (Index (Integer 0) (Integer 0))
    describe "expression Term" $ do
      it "parses literals" $ do
        parse primaryExpr "" "0x15" ~> (Integer 21)
    describe "operand" $ do
      it "parses literals" $ do
        operand `parses` "0X15" ~> (Integer 21)
        operand `parses` "\"asasdasdadsd\\n\"" ~> (IntString "asasdasdadsd\\n")
      it "allows parens" $ do
        operand `parses` "(0)" ~> Integer 0
    describe "slice" $ do
      it "parses constant indices" $ do
        pending
    describe "selector" $ do
      it "parses `(0).method`" $ do
        expression `parses` "(0).method " ~> Selector (Integer 0) "method"
      it "parses (0+1).method" $ do
        expression `parses` "(0+1).method " ~> Selector (BinaryOp Plus (Integer 0) (Integer 1)) "method"
