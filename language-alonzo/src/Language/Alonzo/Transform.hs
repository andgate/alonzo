module Language.Alonzo.Transform where

import Language.Alonzo.Transform.CConv (cconv)
import Language.Alonzo.Transform.Lift  (lift)

import qualified Language.Alonzo.Transform.ANorm    as A
import qualified Language.Alonzo.Transform.NameBind as B
import qualified Language.Alonzo.Syntax.Source      as S



transform :: S.Term -> A.Term
transform = cconv . A.normalize . B.namebind