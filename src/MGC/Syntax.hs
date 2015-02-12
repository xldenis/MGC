module MGC.Syntax (
  Identifier(..), Signature(..), Package(..), TopLevelDeclaration(..), 
  Declaration(..), FuncDecl(..), Statement(..), SimpleStatement(..), ForCond(..), SwitchClause(..),
  Type(..), Expression(..), TypeName(..), TypeLit(..), MethodSpec(..), VarDecl(..), VarSpec(..)) where

  type Identifier = String
  data QualIdent = QualIdent String String

  data Package = Package Identifier [TopLevelDeclaration]

  data TopLevelDeclaration = Declaration | FuncDecl

  data Declaration = TpDecl | VarDecl

  data FuncDecl = FunctionDecl Identifier Signature Statement | FunctionSig Identifier Signature

  data Signature = Signature Parameters Parameters

  type Parameters = [(Identifier, Type)]

  data TpDecl = TypeDecl Parameters

  type VarDecl = [VarSpec]
  data VarSpec = VarSpec [Identifier] [Expression] Type

  data Expression = BinaryOp BinOp Expression Expression | UnaryExpr
  data UnaryExpr = PrimaryExpr | UnaryOp UOp UnaryExpr

  data BinOp = Or | And | RelOp | AddOp | MulOp
  data RelOp = Eq | NEq | LessThan | LessThanEq | GreaterThan | GreaterThanEq
  data AddOp =  Plus | Minus | BinBitOr | BinBitXor
  data MulOp = Mult | Div | Mod | LShift | RShift | BOr | BAndXor
  data UOp = UnPlus | UnMinus | Not | BComp


  data PrimaryExpr = Operand
    | Conversion Type Expression
    | Selector PrimaryExpr Identifier
    | Index Expression
    | SliceExpr Expression Expression
    | TypeAssertion Type
    | Arguments [Expression]

  data Statement = Print
    | PrintLn
    | Return [Expression]
    | If (Maybe SimpleStatement) Expression Statement Statement
    | Switch SimpleStatement Expression [SwitchClause]
    | For ForCond Statement
    | Continue
    | Break
    | Block [Statement]

  data ForCond = Condition Expression | ForClause SimpleStatement Expression SimpleStatement

  data SimpleStatement = Empty
    | ExpressionStmt Expression
    | Inc Expression
    | Dec Expression
    | Assignment [Expression] [Expression]
    | ShortDecl [Identifier] [Expression]

  data SwitchClause = Case (Maybe [Expression]) [Statement]

  data Type = TypeName | TypeLit
  data TypeName = Name Identifier | QualName Identifier
  data TypeLit = Array Expression Type | Struct | Pointer Type | Function Signature| Interface [MethodSpec] | Slice Type | Unit
  data MethodSpec = MethodSpec Identifier Signature | InterfaceName Identifier
