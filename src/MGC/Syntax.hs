module MGC.Syntax where

  type Identifier = String
  data QualIdent = QualIdent String String

  data Package = Package Identifier [TopLevelDeclaration] deriving (Show, Eq)

  data TopLevelDeclaration = FunctionDecl Identifier Signature (Maybe Statement)
    | TypeDecl [TypeSpec] 
    | VarDecl [VarSpec] deriving (Show, Eq)

  data Signature = Signature [Parameter]  [Parameter] deriving (Show, Eq)

  data Parameter = Parameter [Identifier] Type deriving (Show, Eq)

  data TypeSpec = TypeSpec Identifier Type deriving (Show, Eq)

  data VarSpec = VarSpec [Identifier] [Expression] Type  deriving (Show, Eq)

  data Expression = BinaryOp BinOp Expression Expression
   | UnaryOp UOp Expression
   | Conversion Type Expression
   | Selector Expression Identifier
   | Index Expression Expression
   | SimpleSlice Expression Expression
   | FullSlice Expression Expression Expression
   | TypeAssertion Type
   | Arguments [Expression]
   | Name Identifier
   | QualName Identifier Identifier
   | Integer Int  
   | Rune Char
   | Float Float
   | String String deriving (Show, Eq)

  data BinOp = Or | And 
    | Eq | NEq | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Plus | Minus | BitOr | BitXor
    | Mult | Div | Mod | LShift | RShift | BitAnd | BitClear  deriving (Show, Eq)
    
  data UOp = Pos | Neg | Not | BComp  deriving (Show, Eq)

  data Statement = Print
    | PrintLn
    | Return [Expression]
    | If (Maybe Statement) Expression Statement Statement
    | Switch (Maybe Statement) (Maybe Expression) [SwitchClause]
    | For (Maybe ForCond) Statement
    | Continue
    | Break
    | Block [Statement]
    | Empty
    | ExpressionStmt Expression
    | Inc Expression
    | Dec Expression
    | Assignment BinOp [Expression] [Expression]
    | ShortDecl [Identifier] [Expression]  deriving (Show, Eq)

  data ForCond = Condition Expression | ForClause Statement Expression Statement  deriving (Show, Eq)

  type SwitchClause = (Maybe [Expression], [Statement])

  data Type = TypeName Identifier 
    | QualTypeName Identifier Identifier
    | Array Expression Type 
    | Struct [FieldDecl]
    | Pointer Type 
    | Function Signature
    | Interface [MethodSpec] 
    | Slice Type 
    | TInteger
    | TFloat
    | TString
    | TRune
    | TBool
    | Unit  deriving (Show, Eq)


  data FieldDecl  = NamedField [Identifier] Type (Maybe  Expression) | AnonField Type (Maybe Expression) deriving (Show, Eq)
  data MethodSpec = MethodSpec Identifier Signature | InterfaceName Identifier  deriving (Show, Eq)
