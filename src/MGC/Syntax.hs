module MGC.Syntax (
  Identifier(..), Signature(..), Package(..), TopLevelDeclaration(..), BinOp(..), UOp(..),
  Statement(..), SimpleStatement(..), ForCond(..), SwitchClause(..),
  Type(..), Expression(..), MethodSpec(..), VarSpec(..)) where

  type Identifier = String
  data QualIdent = QualIdent String String

  data Package = Package Identifier [TopLevelDeclaration]

  data TopLevelDeclaration = FunctionDecl Identifier Signature (Maybe Statement)
    | TypeDecl [(Identifier, Type)] 
    | VarDecl [VarSpec]

  data Signature = Signature Parameters (Maybe Parameters)

  type Parameters = [([Identifier], Type)]

  data VarSpec = VarSpec [Identifier] [Expression] Type

  data Expression = BinaryOp BinOp Expression Expression
   | UnaryOp UOp Expression
   | Conversion Type Expression
   | Selector Expression Identifier
   | Index Expression
   | SliceExpr Expression Expression
   | TypeAssertion Type
   | Arguments [Expression]
   | Operand

  data BinOp = Or | And 
    | Eq | NEq | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Plus | Minus | BinBitOr | BinBitXor
    | Mult | Div | Mod | LShift | RShift | BinBitAnd | BinBitClear
  data UOp = UnPlus | UnMinus | Not | BComp

  data Statement = Print
    | PrintLn
    | Return [Expression]
    | If (Maybe SimpleStatement) Expression Statement Statement
    | Switch (Maybe SimpleStatement) (Maybe Expression) [SwitchClause]
    | For ForCond Statement
    | Continue
    | Break
    | Block [Statement]

  data SimpleStatement = Empty
    | ExpressionStmt Expression
    | Inc Expression
    | Dec Expression
    | Assignment BinOp [Expression] [Expression]
    | ShortDecl [Identifier] [Expression]

  data ForCond = Condition Expression | ForClause SimpleStatement Expression SimpleStatement

  data SwitchClause = Case (Maybe [Expression]) [Statement]

  data Type = Name Identifier 
    | QualName Identifier
    | Array Expression Type 
    | Struct | Pointer Type 
    | Function Signature
    | Interface [MethodSpec] 
    | Slice Type 
    | Unit
  
  data MethodSpec = MethodSpec Identifier Signature | InterfaceName Identifier
