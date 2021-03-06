{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MGC.Syntax.Pretty where
  --import Text.PrettyPrint.HughesPJClass
  import MGC.Syntax
  import Data.List (intercalate, intersperse)

  import MGC.Parser.Expression (binaryOps, unaryOps)

  ($-$) :: Doc -> Doc -> Doc
  a $-$ b = a <> char '\n' <+> b

  data Doc = Doc Int String
    | VUnion Int [Doc]
    | HUnion Int [Doc]
    | EmptyDoc deriving Show

  class Pretty a where
    pretty :: a -> Doc
    prettyList :: [a] -> Doc
    prettyList lst = VUnion 0 (map pretty lst)

  instance Pretty a => Pretty [a] where
    pretty x = prettyList x

  instance Pretty a => Pretty (Maybe a) where
    pretty (Just a) = pretty a
    pretty Nothing = empty

  text :: String -> Doc
  text = Doc 0

  rawString :: String -> Doc
  rawString s = Doc 0 $ "`"++s++"`"

  intString :: String -> Doc
  intString s = Doc 0 $ "\""++s++ "\""

  char :: Char -> Doc
  char c = Doc 0 [c]

  flatten :: Doc -> Doc
  flatten (VUnion ind c) = case foldl (\p n -> p <+> n ) empty c of
    Doc _ str -> Doc ind str
    a -> a
  flatten a = a

  (<>) :: Doc -> Doc -> Doc
  a <> EmptyDoc = a
  EmptyDoc <> a = a
  (Doc int s) <> (Doc _ t) = Doc int (s++t)
  (HUnion in1 s) <> (VUnion in2 t) = VUnion in1 $ HUnion 0 (s ++ [head t]) : map (nest (in2 - in1)) (tail t)
  (HUnion in1 s) <> b@(Doc _ _) = HUnion in1 (s++[b])
  (HUnion in1 s) <> (HUnion _ t) = HUnion in1 (s++t)
  a <> b = HUnion 0 [a,b]

  (<+>) :: Doc -> Doc -> Doc
  a <+> EmptyDoc = a
  EmptyDoc <+> a = a
  (Doc in1 s) <+> (Doc _ t) = Doc in1 (s++" "++t)
  (Doc in1 s) <+> a = HUnion in1 [Doc 0 $ s++" "] <> a
  (VUnion in1 s) <+> (VUnion in2 t) = VUnion in1 (init s ++ [last s <+> head t, VUnion (in2 - in1) $ tail s])
  (VUnion in1 s) <+> b = HUnion in1 $ VUnion 0 s : [char ' ', b]
  (HUnion in1 s) <+> (VUnion in2 t) = HUnion in1 $ s ++ [head t, VUnion in2 (tail t)]
  (HUnion in1 s) <+> c = HUnion in1 $ s ++ [char ' ', c]

  (<?>) :: Doc -> Doc -> Doc
  _ <?> EmptyDoc = empty
  EmptyDoc <?> _ = empty
  a <?> b = a <+> b

  ($$) :: Doc -> Doc -> Doc
  a $$ (VUnion _ []) = a
  (Doc in1 s) $$ a = VUnion in1 (Doc 0 s:[a])
  (VUnion in1 s) $$ a  = VUnion in1 (s++[a])
  a $$ b = VUnion 0 [a,b]
  empty :: Doc
  empty = EmptyDoc

  nest n (Doc ind txt) = Doc (ind+n) txt
  nest n (VUnion ind lst) = VUnion (ind+n) lst
  nest n (HUnion ind lst) = HUnion (ind+n) lst
  nest _ EmptyDoc = EmptyDoc

  braces :: Doc -> Doc
  braces d = char '{' $$ d $$ char '}'

  braces' :: Doc -> Doc
  braces' d = char '{' <+> d <+> char '}'

  brackets :: Doc -> Doc
  brackets d = char '[' <> d <> char ']'

  parens :: Doc -> Doc
  parens d = char '(' <> d <> char ')'

  parens' :: Doc -> Doc
  parens' d = char '(' $$ d $$ char ')'

  quotes :: Doc -> Doc
  quotes d = char '"' <> d <> char '"'

  comment :: Doc -> Doc
  comment c = text "/*" <?> c <?> text "*/"

  annotate :: Pretty a => a -> Doc
  annotate = comment . pretty

  float :: Double -> Doc
  float f = Doc 0 (show f)

  int :: Int -> Doc
  int i = Doc 0 (show i)

  bool :: Bool -> Doc
  bool True  = Doc 0 "true"
  bool False = Doc 0 "false"

  prettyShow :: Doc -> String
  prettyShow = prettyShow' 0

  prettyShow' _ (VUnion _ []) = ""
  prettyShow' n (VUnion ind stmts) =  intercalate "\n" $ map (prettyShow' $ n+ind) stmts
  prettyShow' n (Doc ind stmt) = replicate (n + ind) ' '++ stmt
  prettyShow' n (HUnion ind s) = prettyShow' (ind+n) (head s) ++ concatMap (prettyShow' 0) (tail s)
  prettyShow' _ EmptyDoc = ""

  instance Pretty Ann where pretty a = pretty (ty a) <+> parens (pretty (truety a))

  instance Pretty a => Pretty (Package a) where
    pretty (Package name content) = text "package" <+> text name <+> text "\n\n" <> pretty content

  instance Pretty a => Pretty (TopLevelDeclaration a) where
    prettyList decs = foldl (<>) empty $ intersperse (text "\n\n") $ map pretty decs

    pretty (FunctionDecl ident sig body) = text "func" <+>
      text ident <> pretty sig <+> pretty body
    pretty (Decl s) = pretty s

  instance Pretty (TypeSpec a) where
    pretty (TypeSpec _ ident tp) = text ident <+> pretty tp

  instance Pretty a => Pretty (VarSpec a) where
    pretty (VarSpec _ idents vals tp) = text (intercalate ", " idents) <+> pretty tp <+> (
      if not (null vals)
      then text "=" <+> pretty vals
      else empty)

  instance Pretty Signature where
    pretty (Signature params ret) = parens (pretty params) <> pretty ret

  instance Pretty Parameter where
    prettyList params = foldl (<>) empty $ intersperse (text ", ") $ map pretty params
    pretty (Parameter idents tp) = text (intercalate ", " idents) <+> pretty tp

  instance Pretty a => Pretty (Statement a) where
    pretty (Return exps) = text "return" <+> pretty exps-- not done
    pretty (If stmt exp left right) = text "if" <+>(case stmt of
        Empty -> empty
        s -> pretty s <> char ';'
      )<+>
      pretty exp <+>
      pretty left <+>
      (case right of
        Empty -> empty
        _ -> text "else" <+> pretty right)

    pretty (For cond body) = text "for" <+> pretty cond <+> pretty body
    pretty (Switch Empty e body) = text "switch" <+> pretty e <+> braces (nest 2 $ pretty body)
    pretty (Switch s e body) = text "switch" <+> pretty s <> char ';' <+> pretty e <+> braces (nest 2 $ pretty body)
    pretty Continue = text "continue"
    pretty Break = text "break"
    pretty (Block stmt) = braces (nest 2 (pretty stmt))
    pretty (ExpressionStmt exp) = pretty exp
    pretty (Inc exp) = pretty exp<> text "++"
    pretty (Dec exp) = pretty exp<> text "--"
    pretty (Assignment Eq lhs rhs) = pretty lhs <+> char '=' <+> pretty rhs
    pretty (Assignment op lhs rhs) = pretty lhs <+> pretty op <> char '=' <+> pretty rhs
    pretty (ShortDecl lhs rhs) = text (intercalate ", " lhs) <+> text ":=" <+> pretty rhs
    pretty Fallthrough = text "fallthrough"
    pretty Empty = empty
    pretty (TypeDecl fields) = text "type" <+>
      pretty fields
    pretty (VarDecl vars) = text "var" <+> (if length vars == 1
      then pretty (head vars)
      else parens' $ nest 2 $ pretty vars)

  instance Pretty a => Pretty (SwitchClause a) where
    pretty (Default body) = text "default:" $$ nest 2 (pretty body)
    pretty (Case e  body) = text "case" <+> pretty e <> char ':' $$ nest 2 (pretty body)

  instance Pretty a => Pretty (ForCond a) where
    pretty (Condition exp) = pretty exp
    pretty (ForClause low cond nxt) = pretty low <> char ';' <+> pretty cond <> char ';' <+> pretty nxt

  instance Pretty Type where
    pretty (TypeName ident) = text ident
    pretty (Array len tp) = brackets (int len) <> pretty tp
    pretty (Struct []) = text "struct{}"
    pretty (Struct decls) = text "struct" <> (if length decls == 1
      then braces' $ pretty (head decls)
      else braces $ nest 2 $ pretty decls)
    pretty (Function sig) = text "func" <> pretty sig
    pretty (Interface fields) = text "interfaces" <+> braces (pretty fields)
    pretty (Slice tp) = text "[]" <> pretty tp
    pretty TInteger = text "int"
    pretty TFloat = text "float64"
    pretty TString = text "string"
    pretty TRune = text "rune"
    pretty TBool = text "bool"
    pretty TNil = empty
    pretty (ReturnType _) = empty -- not a user facing type

  instance Pretty FieldDecl where
    pretty (NamedField nm tp t) = foldl (<>) empty (intersperse (text ", ") $ map text nm) <+> pretty tp <+> (
      case t of
        Nothing -> empty
        Just a -> text a)
    pretty (AnonField tp t)  = pretty tp <+> (case t of
      Nothing -> empty
      Just a -> text a)

  instance Pretty MethodSpec where pretty _ = empty

  instance Pretty a => Pretty (Expression a) where
    prettyList exps = foldl (<>) empty $ intersperse (text ", ") $ map pretty exps

    pretty (BinaryOp t op lhs rhs) = pretty lhs <+> pretty op <+> pretty rhs <+> annotate t
    pretty (UnaryOp t op exp) = pretty op <> pretty exp  <+> annotate t
    pretty (Conversion t tp exp) = pretty tp <> parens (pretty exp) <+> annotate t
    pretty (Selector t exp ident) = pretty exp <> char '.' <> text ident <+> annotate t
    pretty (Index t exp ind)  = pretty exp <> brackets (pretty ind) <+> annotate t
    pretty (SimpleSlice _ sliced lower upper) = pretty sliced <> brackets ((pretty lower) <> (char ':') <> (pretty upper))
    pretty (FullSlice _ sliced lower upper dir) = pretty sliced <> brackets ((pretty lower) <> (char ':') <> (pretty upper) <> (char ':') <> (pretty dir))
    pretty (Name t ident) = text ident <+> annotate t
    pretty (QualName pkg ident) = text pkg <> char '.' <> text ident
    pretty (Integer val) = int val
    pretty (Rune ch) = text ch
    pretty (Float val) = float val
    pretty (Bool val) = bool val
    pretty (RawString str) = rawString str
    pretty (IntString str) = intString str
    pretty (Arguments _ expr args) = pretty expr <> parens (pretty args)

  instance Pretty () where pretty () = empty
  instance Pretty BinOp where pretty op = text (fst.head $ filter (\el -> snd el == op) binaryOps)
  instance Pretty UOp where pretty op = text (fst.head $ filter (\el -> snd el == op) unaryOps)
