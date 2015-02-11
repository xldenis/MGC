module MGC.Syntax (Identifier, Package, TopLevelDeclaration(..), Statement(..), Type(..), Expression(..)) where

  type Identifier = String
  data QualIdent = QualIdent String String

  data Package = Package Identifier [TopLevelDeclaration]

  data TopLevelDeclaration = Declaration | FuncDecl

  data Declaration = TpDecl | VarDecl

  data FuncDecl = FunctionDecl Identifier Signature Statement

  data Signature = String

  data TpDecl = TypeDecl [(Identifier, Type)]

  data Expression = BinaryOp BinOp Expression Expression | UnaryExpr
  data UnaryExpr = PrimaryExpr | UnaryOp UOp UnaryExpr

  data BinOp = Or | And | RelOp | AddOp | MulOp
  data RelOp = Eq | NEq | LessThan | LessThanEq | GreaterThan | GreaterThanEq
  data AddOp =  Plus | Minus | BinBitOr | BinBitXor
  data MulOp = Mult | Div | Mod | LShift | RShift | BOr | BAndXor
  data UOp = UnPlus | UnMinus | Not | BComp

  data IncOp = Inc | Dec

  data PrimaryExpr = Operand
    | Conversion Type Expression
    | Selector PrimaryExpr Identifier
    | Index Expression
    | Slice Expression Expression
    | TypeAssertion Type
    | Arguments [Expression]

  data Statement = Print
    | PrintLn
    | Return [Expression]
    | If SimpleStatement Expression Statement Statement
    | Switch SimpleStatement Expression [SwitchClause]
    | For ForCond Statement
    | Continue
    | Break
    | Block [Statement]

  data ForCond = Condition Expression | ForClause SimpleStatement Expression SimpleStatement

  data SimpleStatement = Empty
    | ExpressionStmt Expression
    | IncDec Expression IncOp
    | Assignment [Expression] [Expression]
    | ShortDecl [Identifier] [Expression]

  data SwitchClause = Case [Expression] [Statement] | Default [Statement]

  data Type = TypeName | TypeLit
  data TypeName = Name Identifier | QualName Identifier
  data TypeLit = Array | Struct | Pointer | Function | Interface | SliceTp | Map | Channel

