module MGC.Syntax.Pretty where
  --import Text.PrettyPrint.HughesPJClass
  import MGC.Syntax

  import Data.List (intercalate, intersperse)

  import MGC.Parser.Expression (binaryOps, unaryOps)

  ($-$) :: Doc -> Doc -> Doc
  a $-$ b = a <> (char '\n') <+> b

  data Doc = Doc Int String
    | Union Int [Doc]
    | Beside Doc Doc 
    | EmptyDoc deriving Show

  class Pretty a where
    pPrint :: a -> Doc
    pPrintList :: [a] -> Doc
    pPrintList lst = Union 0 (map pPrint lst)
 

  instance Pretty a => Pretty [a] where 
    pPrint x = pPrintList x


  instance Pretty a => Pretty (Maybe a) where
    pPrint (Just a) = pPrint a
    pPrint Nothing = empty

  text :: String -> Doc
  text = Doc 0 

  rawString :: String -> Doc
  rawString s = Doc 0 $ "`"++s++"`"

  intString :: String -> Doc
  intString s = Doc 0 $ "\""++s++ "\""

  char :: Char -> Doc
  char c = Doc 0 [c]

  flatten :: Doc -> Doc
  flatten (Union ind c) = case (foldl (\p n -> p <+> n ) empty c) of
    Doc i str -> Doc ind str
  flatten a = a

  (<>) :: Doc -> Doc -> Doc
  a <> EmptyDoc = a
  EmptyDoc <> a = a
  (Doc int s) <> (Doc int2 t) = Doc int (s++t)
  a <> b = Beside a b


  (<+>) :: Doc -> Doc -> Doc
  (Doc in1 s) <+> (Doc _ t) = Doc in1 (s++" "++t)
  (Doc in1 s) <+> a = Beside (Doc in1 $ s++" ") a
  (Union in1 s) <+> a = Beside (Union in1 $ (init s)++[(last s) <> (char ' ')]) a
  (Beside a b) <+> c = Beside a (b <+> c)
  a <+> EmptyDoc = a
  EmptyDoc <+> a = a

  ($$) :: Doc -> Doc -> Doc
  (Doc in1 s) $$ a = Union in1 ((Doc 0 s):[a])
  (Union in1 s) $$ a  = Union in1 (s++[a])

  empty :: Doc
  empty = EmptyDoc

  nest n (Doc ind txt) = Doc (ind+n) txt
  nest n (Union ind lst) = Union (ind+n) lst

  braces :: Doc -> Doc
  braces d = (char '{') $$ d $$ (char '}')

  brackets :: Doc -> Doc
  brackets d = (char '[') <> d <> (char ']')

  parens :: Doc -> Doc
  parens d = (char '(') <> d <> (char ')')

  quotes :: Doc -> Doc
  quotes d = (char '"') <> d <> (char '"')

  float :: Float -> Doc
  float f = Doc 0 (show f)

  int :: Int -> Doc
  int i = Doc 0 (show i)

  prettyShow :: Doc -> String
  prettyShow = prettyShow' 0

  prettyShow' n (Union ind stmts) = intercalate "\n" $ map (prettyShow' $ n+ind) stmts
  prettyShow' n (Doc ind stmt) = (replicate (n + ind) ' ')++ stmt
  prettyShow' n (Beside a b) = (prettyShow' n a) ++ (prettyShow' 0 b)
  prettyShow' n EmptyDoc = ""

  instance Pretty Package where
    pPrint (Package name content) = text "package" <+> text name <+> text "\n\n" <> (pPrint content)

  instance Pretty TopLevelDeclaration where
    pPrintList decs = foldl (<>) empty $ intersperse (text "\n\n") $ map pPrint decs

    pPrint (FunctionDecl ident sig body) = text "func" <+> 
      text ident <> 
      (pPrint sig) <+> 
      case body of 
        Nothing -> empty
        Just s -> (pPrint s)
    pPrint (Decl s) = pPrint s

  instance Pretty TypeSpec where
    pPrint (TypeSpec ident tp) = (text ident) <+> (pPrint tp)

  instance Pretty VarSpec where
    pPrint (VarSpec idents vals tp) = (text $ intercalate ", " idents) <+> (pPrint tp) <+> (
      if (length vals) > 0
      then (text "=") <+> (pPrint vals)
      else empty)
  instance Pretty Signature where
    pPrint (Signature params ret) = (parens $ pPrint params) <> (pPrint ret)

  instance Pretty Parameter where
    pPrintList params = foldl (<>) empty $ intersperse (text ", ") $ map pPrint params
    pPrint (Parameter idents tp) = text (intercalate ", " idents) <+> (pPrint tp)
  
  instance Pretty Statement where
    pPrint (Return exps) = text "return" -- not done
    pPrint (If stmt exp left right) = text "if" <+> 
      (case stmt of
        Nothing -> empty
        Just a  -> (pPrint a <+> text ";")) <+>
      (pPrint exp) <+> 
      (braces (pPrint left))
    pPrint (For cond body) = text "for" <+> (pPrint cond) <+> (pPrint body)
    pPrint Continue = text "continue"
    pPrint Break = text "break"
    pPrint (Block stmt) = braces $ (nest 2 (pPrint stmt))
    pPrint (ExpressionStmt exp) = pPrint exp
    pPrint (Inc exp) = (pPrint exp)<> text "++"
    pPrint (Dec exp) = (pPrint exp)<> text "--"
    pPrint (Assignment op lhs rhs) = (pPrint lhs) <+>(pPrint op) <+>(pPrint rhs)
    pPrint (ShortDecl lhs rhs) = (text $ intercalate ", " lhs) <+> (text ":=") <+> (pPrint rhs)
    pPrint Empty = empty

    pPrint (TypeDecl fields) = text "type" <+>
      (pPrint fields)

    pPrint (VarDecl vars) = text "var" <+>
      (pPrint vars)

  instance Pretty ForCond where
    pPrint (Condition exp) = pPrint exp
    pPrint (ForClause low cond nxt) = (pPrint low) <> (char ';') <+> (pPrint cond) <> (char ';') <+> (pPrint nxt)

  instance Pretty Type where
    pPrint (TypeName ident) = text ident
    pPrint (Array exp tp) = (brackets $ pPrint exp) <> (pPrint tp)
    pPrint (Struct decls) = text "struct" <+> (braces $ nest 2 $ pPrint decls)
    --pPrint (Pointer tp) = text "*" <> (pPrint tp)
    pPrint (Function sig) = text "func" <> (pPrint sig)
    pPrint (Interface fields) = text "interfaces" <+> (braces $ pPrint fields)
    pPrint (Slice tp) = text "[]" <> (pPrint tp)
    pPrint TInteger = text "int"
    pPrint TFloat = text "float64"
    pPrint TString = text "string"
    pPrint TRune = text "rune"
    pPrint TBool = text "bool"

  instance Pretty FieldDecl where
    pPrint (NamedField nm tp t) = (foldl (<>) empty $ intersperse (text ", ") $ map text nm) <+> (pPrint tp) <+> (
      case t of 
        Nothing -> empty
        Just a -> pPrint a)
    pPrint (AnonField tp t)  = (pPrint tp) <+> (pPrint t)

  instance Pretty MethodSpec where
    pPrint a = empty

  instance Pretty Expression where
    pPrint (BinaryOp op lhs rhs) = (pPrint lhs) <+> (pPrint op) <+> (pPrint rhs)
    pPrint (UnaryOp op exp) = (pPrint op) <> (pPrint exp)
    pPrint (Conversion tp exp) = (pPrint tp) <> (parens $ pPrint exp)
    pPrint (Selector exp ident) = (pPrint exp) <> (char '.') <> (text ident)
    pPrint (Index exp ind)  = (pPrint exp) <> (brackets $ pPrint ind)
    pPrint (SimpleSlice sliced lower upper) = (pPrint sliced) <> (brackets $ (pPrint lower) <> (char ':') <> (pPrint upper))
    pPrint (FullSlice sliced lower upper dir) = (pPrint sliced) <> (brackets $ (pPrint lower) <> (char ':') <> (pPrint upper) <> (char ':') <> (pPrint dir))
    --pPrint TypeAssertion tp -- not done
    pPrint (Name ident) = text ident
    pPrint (QualName pkg ident) = (text pkg) <> (char '.') <> (text ident)
    pPrint (Integer val) = int val
    pPrint (Rune ch) = char ch
    pPrint (Float val) = float val
    pPrint (RawString str) = rawString ( str)
    pPrint (IntString str) = intString ( str)
    pPrint (Arguments expr args) = (pPrint expr) <> (parens $ pPrint args)

  instance Pretty BinOp where
    pPrint op = text (fst.head $ filter (\(el) -> (snd el) == op) binaryOps)
  instance Pretty UOp where
    pPrint op = text (fst.head $ filter (\el -> (snd el) == op) unaryOps)