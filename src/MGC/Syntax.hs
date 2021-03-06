{-# LANGUAGE DeriveDataTypeable #-}
module MGC.Syntax where
  import Data.Data

  type Identifier = String
  data QualIdent = QualIdent String String

  data Package a = Package Identifier [TopLevelDeclaration a] deriving (Show, Eq, Typeable, Data)

  data TopLevelDeclaration a
    = FunctionDecl Identifier Signature (Statement a)
    | Decl (Statement a) deriving (Show, Eq, Typeable, Data)

  data Signature = Signature [Parameter]  [Parameter] deriving (Show, Eq, Typeable, Data)

  data Parameter = Parameter [Identifier] Type deriving (Show, Eq, Typeable, Data)

  data Expression a
   = BinaryOp a BinOp (Expression a) (Expression a)
   | UnaryOp a UOp (Expression a)
   | Conversion a Type (Expression a)
   | Selector a (Expression a) Identifier
   | Index a (Expression a) (Expression a)
   | SimpleSlice a (Expression a) (Expression a) (Expression a)
   | FullSlice a (Expression a) (Expression a) (Expression a) (Expression a)
   | Arguments a (Expression a) [Expression a]
   | Name a Identifier
   | QualName Identifier Identifier
   | Integer Int
   | Rune String
   | Float Double
   | IntString String
   | Bool Bool
   | RawString String deriving (Show, Eq, Typeable, Data)

  data BinOp
    = Or | And
    | Eq | NEq | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Plus | Minus | BitOr | BitXor
    | Mult | Div | Mod | LShift | RShift | BitAnd | BitClear  deriving (Show, Eq, Typeable, Data)

  data UOp = Pos | Neg | Not | BComp  deriving (Show, Eq, Typeable, Data)

  data Statement a
    = Return [Expression a]
    | Assignment BinOp [Expression a] [Expression a]
    | Block [Statement a]
    | Break
    | Continue
    | Dec (Expression a)
    | Empty
    | ExpressionStmt (Expression a)
    | Fallthrough
    | For (Maybe (ForCond a)) (Statement a)
    | If (Statement a) (Expression a) (Statement a) (Statement a)
    | Inc (Expression a)
    | ShortDecl [Identifier] [Expression a]
    | Switch (Statement a) (Maybe (Expression a)) [SwitchClause a]
    | TypeDecl [TypeSpec a]
    | VarDecl [VarSpec a] deriving (Show, Eq, Typeable, Data)

  data ForCond a = Condition (Expression a) | ForClause (Statement a) (Maybe (Expression a)) (Statement a)  deriving (Show, Eq, Typeable, Data)

  data TypeSpec a = TypeSpec a Identifier Type deriving (Show, Eq, Typeable, Data)

  data VarSpec a = VarSpec a [Identifier] [Expression a] (Maybe Type) deriving (Show, Eq, Typeable, Data)

  data SwitchClause a
    = Default [Statement a]
    | Case [Expression a] [Statement a] deriving (Show, Eq, Typeable, Data)

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
    | TNil
    | TBool deriving (Show, Eq, Typeable, Data)

  data Ann = Ann{ ty :: Type, truety :: Type} deriving (Show, Typeable, Data)

  data FieldDecl
    = NamedField [Identifier] Type (Maybe String)
    | AnonField Type (Maybe String) deriving (Show, Eq, Typeable, Data)
  data MethodSpec
    = MethodSpec Identifier Signature
    | InterfaceName Identifier  deriving (Show, Eq, Typeable, Data)

  isMulOp :: BinOp -> Bool
  isMulOp Mult      = True
  isMulOp Div       = True
  isMulOp Mod       = True
  isMulOp LShift    = True
  isMulOp RShift    = True
  isMulOp BitAnd    = True
  isMulOp BitClear  = True
  isMulOp _         = False

  isAddOp :: BinOp -> Bool
  isAddOp Plus    = True
  isAddOp Minus   = True
  isAddOp BitOr   = True
  isAddOp BitXor  = True
  isAddOp _       = False

  isNum TInteger = True
  isNum TFloat   = True
  isNum TRune    = True
  isNum _        = False

  isInt TInteger = True
  isInt TRune    = True
  isInt _        = False

  isOrd TInteger = True
  isOrd TRune    = True
  isOrd TString  = True
  isOrd TFloat   = True
  isOrd _        = False

  isNumOp Plus  = True
  isNumOp Minus = True
  isNumOp Mult  = True
  isNumOp Div   = True
  isNumOp _     = False

  isOrdOp LessThan      = True
  isOrdOp LessThanEq    = True
  isOrdOp GreaterThan   = True
  isOrdOp GreaterThanEq = True
  isOrdOp _             = False

  isIntOp BitOr     = True
  isIntOp BitAnd    = True
  isIntOp LShift    = True
  isIntOp RShift    = True
  isIntOp BitClear  = True
  isIntOp BitXor    = True
  isIntOp Mod       = True
  isIntOp _         = False

  isCmpOp Eq        = True
  isCmpOp NEq       = True
  isCmpOp _         = False
