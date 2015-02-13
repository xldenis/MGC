module MGC.Parser.ExpressionSpec (spec) where
  import Test.Hspec
  import MGC.Expectation
  import MGC.Parser.Expression 
  import Text.Parsec
  import MGC.Syntax
  spec :: Spec
  spec = do
    describe "expression" $ do
      it "parses full expression" $ do
        expression `parses` "0==0" `to` (BinaryOp Eq (Integer 0) (Integer 0))
        expression `parses` "0 == 0 " `to` (BinaryOp Eq (Integer 0) (Integer 0))
        expression `parses` "0&&0" `to` (BinaryOp And (Integer 0) (Integer 0))
    describe "expression Term" $ do
      it "parses literals" $ do
        pending
        --parse primaryExpr "" "0x15" `to` (Integer 21)
    describe "operand" $ do
      it "parses literals" $ do
        operand `parses` "0X15" `to` (Integer 21)
      it "allows parens" $ do
        operand `parses` "(0)" `to` Integer 0
    describe "slice" $ do
      it "parses constant indices" $ do
        index `parses` "[0]" `to` Index (Integer 0)
    describe "selector" $ do
      it "parses `(0).method`" $ do
        selector `parses` "(0).method " `to` Selector (Integer 0) "method"