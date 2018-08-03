{-# LANGUAGE LambdaCase
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
 #-}
module Language.Alonzo.Transform.Eval where


import Control.Monad.Reader
import Data.Text (Text, pack)
import Language.Alonzo.Closure (Closure)
import Language.Alonzo.Syntax.Bound
import Language.Alonzo.Syntax.Prim
import Language.Alonzo.Value
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import qualified Language.Alonzo.Closure as CL


eval :: Closure -> Term -> Term
eval clos t = runEval clos (evalTerm t)

newtype Eval a = Eval { unEval :: ReaderT Closure FreshM a } 
  deriving (Functor, Applicative, Monad, MonadReader Closure, Fresh)


runEval :: Closure -> Eval a -> a
runEval clos (Eval m) = runFreshM $ runReaderT m clos

evalTerm :: Term -> Eval Term
evalTerm = \case
  TVar v -> return $ TVar v -- substitute in from the closure?

  TVal v -> return $ TVal v
  
  TPrim i t1 t2 -> do
    t1' <- evalTerm t1
    t2' <- evalTerm t2
    case (t1', t2') of
      (TVal v1, TVal v2) ->
           return . TVal $ evalInstr (i, v1, v2)
      _ -> return $ TPrim i t1' t2'

  TApp f as -> do
    f' <- evalTerm f
    case f' of
      TLam bnd -> do
        (xs, body)   <- unbind bnd
        (body', xs') <- apply body xs as 
        case xs' of
          []  -> return $ body'
          _   -> return $ TLam (bind xs' body') 
     
      TApp g bs -> evalTerm $ TApp g (bs ++ as)
      _         -> TApp f' <$> mapM evalTerm as


  TLam bnd -> do
    (tele, body) <- unbind bnd
    body' <- evalTerm body
    return $ TLam (bind tele body')

  TLet bnd -> do
    (r, body) <- unbind bnd
    let vars = unrec r
        -- Substitute all the terms into the body
        body' = foldr (\(v, Embed rhs) body -> subst v rhs body) body vars
        fvs = toListOf fv body'
    if any (\(v,_) -> v `elem` fvs) vars
      then evalTerm . TLet $ bind (rec vars) body'
      else evalTerm body'


  TLoc l t   -> evalTerm t


apply :: Term -> [Var] -> [Term] -> Eval (Term, [Var])
apply t [] (_:_) =  error "Too many arguments"

apply t (v:vs) (a:as) = do
  a' <- evalTerm a
  apply (subst v a' t) vs as

apply t vs _ = do
  t' <- evalTerm t
  return (t', vs)