module MGC.Error where
  import MGC.Syntax
  import Text.Parsec.Error

  data MGCError
    = Parser ParseError
    | Typechecker TypeError
    | Weeder      WeederError deriving Show

  data WeederError 
    = MultipleDefault
    | EmptyFuncBody
    | InvalidTopLevelDecl
    | InvalidPackageName
    | InvalidContinue
    | AssignSizeDifferent
    | BlankValue
    | InvalidBreak
    | InvalidArraySize
    | InvalidLValue
    | InvalidFallthrough
    | MissingReturn
    | MultipleReturnValue deriving Show

  data TypeError
    = TypeError String
    | ImpossibleError String
    | InvalidAppend
    | InvalidCase [Expression Ann] Type
    | InvalidCast Type Type
    | InvalidCondition (Expression Ann)
    | InvalidFuncCall
    | InvalidIncDec (Expression Ann) Type
    | InvalidIndex Type Type
    | InvalidOpType BinOp (Expression Ann) (Expression Ann)
    | InvalidPrint
    | InvalidReturn
    | InvalidFor
    | InvalidUOp UOp (Expression Ann)
    | InvalidVarDec Type [Expression Ann] 
    | Mismatch Type Type 
    | MissingField Type Identifier
    | NoNewVars
    | NotInScope String
    | RedeclaredType String
    | MissingMain
    | RedeclaredVar String deriving Show

  transLeft :: (a -> b) -> Either a c -> Either b c
  transLeft f e = case e of
    Left a ->  Left  $ f a
    Right c -> Right $ c
