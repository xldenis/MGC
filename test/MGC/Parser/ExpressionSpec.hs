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
        expression `parses` "0==0" `to` (BinaryOp Eq (Integer 0) (Integer 0))
        expression `parses` "0 == 0 " `to` (BinaryOp Eq (Integer 0) (Integer 0))
        expression `parses` "0&&0" `to` (BinaryOp And (Integer 0) (Integer 0))
      it "parses complex expressions" $ do
        expression `parses` "(0+1).count ^ 0x5" `to` (BinaryOp BitXor (Selector (BinaryOp Plus (Integer 0) $ Integer 0) "count") (Integer 5))
      it "parses different precedences" $ do 
        expression `parses` "0+1^ 2" `to` (BinaryOp BitXor (BinaryOp Plus (Integer 0) (Integer 1)) (Integer 2))
      it "handles parens" $ do
        expression `parses` "(0+1) + 1" `to` BinaryOp Plus (BinaryOp Plus (Integer 0) $ Integer 1) (Integer 1)
    describe "expression Term" $ do
      it "parses literals" $ do
        parse primaryExpr "" "0x15" `to` (Integer 21)
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
        expression `parses` "(0).method " `to` Selector (Integer 0) "method"
      it "parses (0+1).method" $ do
        expression `parses` "(0+1).method " `to` Selector (BinaryOp Plus (Integer 0) (Integer 1)) "method"
