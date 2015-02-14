module MGC.Syntax (
  Identifier(..), Signature(..), Package(..), TopLevelDeclaration(..), 
  BinOp(..), UOp(..), Statement(..), ForCond(..), SwitchClause(..),
  Type(..), Expression(..), MethodSpec(..), VarSpec(..)) where

  type Identifier = String
  data QualIdent = QualIdent String String

  data Package = Package Identifier [TopLevelDeclaration]

  data TopLevelDeclaration = FunctionDecl Identifier Signature (Maybe Statement)
    | TypeDecl [(Identifier, Type)] 
    | VarDecl [VarSpec] deriving (Show, Eq)

  data Signature = Signature Parameters (Maybe Parameters) deriving (Show, Eq)

  type Parameters = [([Identifier], Type)]

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
    | Struct 
    | Pointer Type 
    | Function Signature
    | Interface [MethodSpec] 
    | Slice Type 
    | TInteger
    | TFloat
    | TString
    | TRune
    | Unit  deriving (Show, Eq)

  data MethodSpec = MethodSpec Identifier Signature | InterfaceName Identifier  deriving (Show, Eq)
