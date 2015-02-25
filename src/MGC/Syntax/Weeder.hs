{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module MGC.Syntax.Weeder where
  import MGC.Syntax
  import Control.Applicative ((<$>))
  import Control.Monad (liftM, liftM2, liftM3, mapM)
  import Control.Monad.Except

  class Weedable a where
    weed :: a -> Either WeederError a
    weed = return
    weedList :: [a] -> Either WeederError [a]
    weedList = mapM weed

  data WeederError 
    = MultipleDefault
    | EmptyFuncBody
    | InvalidTopLevelDecl

  --weedTLD :: TopLevelDeclaration -> Except TopLevelDeclaration
  --weedTLD (FunctionDecl iden sig (Just stmt)) = 
  --weedTLD (FunctionDecl iden sig (Nothing)) =
  --weedTLD (Decl ())
  --weedTLD (Decl ())
  --weedTLD (Decl _ )

  pIdent = return

  instance Weedable Package where
    weed (Package iden tlds) = liftM2 Package (pIdent iden) (weed tlds)

  instance Weedable TopLevelDeclaration where
    weed (FunctionDecl _ _ Nothing) = throwError EmptyFuncBody
    weed (FunctionDecl iden sig bdy) = liftM (FunctionDecl iden sig) (weed bdy)
    weed (Decl a@(TypeDecl _)) = Decl <$> (weed a)
    weed (Decl a@(VarDecl _ )) = Decl <$> (weed a)
    weed (Decl _) = throwError InvalidTopLevelDecl

  instance Weedable Statement where 
    weed (Switch s e bdy) = liftM3 Switch (weed s) (weed e) (weed bdy)
    weed a = Right a

  instance Weedable SwitchClause where
    weedList cls = do
      if 1 == (length $ filter (((==) Nothing). fst) cls)
      then throwError MultipleDefault
      else
        mapM weed cls

  instance Weedable Expression where
    weed e = Right e

  instance Weedable a =>  Weedable [a] where weed a = (weedList a)

  instance Weedable a => Weedable (Maybe a) where
    weed (Just b) = liftM Just (weed b)
    weed Nothing  = return $ Nothing
