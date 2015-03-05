module MGC.Syntax where

  type Identifier = String
  data QualIdent = QualIdent String String

  data Package a = Package Identifier [TopLevelDeclaration a] deriving (Show, Eq)

  data TopLevelDeclaration a
    = FunctionDecl Identifier Signature (Maybe (Statement a))
    | Decl (Statement a) deriving (Show, Eq)

  data Signature = Signature [Parameter]  [Parameter] deriving (Show, Eq)

  data Parameter = Parameter [Identifier] Type deriving (Show, Eq)

  data TypeSpec  = TypeSpec Identifier Type deriving (Show, Eq)

  data VarSpec a = VarSpec [Identifier] [Expression a] (Maybe Type)  deriving (Show, Eq)

  data Expression a
   = BinaryOp a BinOp (Expression a) (Expression a)
   | UnaryOp a UOp (Expression a)
   | Conversion Type (Expression a)
   | Selector a (Expression a) Identifier
   | Index a (Expression a) (Expression a)
   | SimpleSlice a (Expression a) (Expression a) (Expression a)
   | FullSlice a (Expression a) (Expression a) (Expression a) (Expression a)
   | Arguments a (Expression a) [(Expression a)]
   | Name a Identifier
   | QualName Identifier Identifier
   | Integer Int  
   | Rune String
   | Float Float
   | IntString String 
   | Bool Bool
   | RawString String deriving (Show, Eq)

  data BinOp 
    = Or | And 
    | Eq | NEq | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Plus | Minus | BitOr | BitXor
    | Mult | Div | Mod | LShift | RShift | BitAnd | BitClear  deriving (Show, Eq)
    
  data UOp = Pos | Neg | Not | BComp  deriving (Show, Eq)

  data Statement a
    = Return [Expression a]
    | If (Maybe (Statement a)) (Expression a) (Statement a) (Statement a)
    | Switch (Maybe (Statement a)) (Maybe (Expression a)) [SwitchClause a]
    | For (Maybe (ForCond a)) (Statement a)
    | Continue
    | Break
    | Fallthrough
    | Block [Statement a]
    | Empty
    | ExpressionStmt (Expression a)
    | Inc (Expression a)
    | Dec (Expression a)
    | TypeDecl [TypeSpec]
    | VarDecl [VarSpec a] 
    | Assignment BinOp [Expression a] [Expression a]
    | ShortDecl [Identifier] [Expression a]  deriving (Show, Eq)

  data ForCond a = Condition (Expression a) | ForClause (Statement a) (Maybe (Expression a)) (Statement a)  deriving (Show, Eq)

  type SwitchClause a = (Maybe [Expression a], [Statement a])

  data Type
    = TypeName Identifier 
    | Array Int Type 
    | Struct [FieldDecl]
    | Function Signature
    | Interface [MethodSpec] 
    | ReturnType [Type]
    | Slice Type 
    | TInteger
    | TFloat
    | TString
    | TRune
    | TBool deriving (Show, Eq)


  data FieldDecl  
    = NamedField [Identifier] Type (Maybe String) 
    | AnonField Type (Maybe String) deriving (Show, Eq)
  data MethodSpec 
    = MethodSpec Identifier Signature 
    | InterfaceName Identifier  deriving (Show, Eq)
