module MGC.Syntax (
  Identifier(..), Signature(..), Package(..), TopLevelDeclaration(..), 
  BinOp(..), UOp(..), Statement(..), ForCond(..), SwitchClause(..),
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
   | Integer Int
  data BinOp = Or | And 
    | Eq | NEq | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Plus | Minus | BitOr | BitXor
    | Mult | Div | Mod | LShift | RShift | BitAnd | BitClear
    
  data UOp = UnPlus | UnMinus | Not | BComp

  data Statement = Print
    | PrintLn
    | Return [Expression]
    | If (Maybe Statement) Expression Statement Statement
    | Switch (Maybe Statement) (Maybe Expression) [SwitchClause]
    | For ForCond Statement
    | Continue
    | Break
    | Block [Statement]
    | Empty
    | ExpressionStmt Expression
    | Inc Expression
    | Dec Expression
    | Assignment BinOp [Expression] [Expression]
    | ShortDecl [Identifier] [Expression]

  data ForCond = Condition Expression | ForClause Statement Expression Statement

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
