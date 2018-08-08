module Language.Alonzo.Transform where

import qualified Language.Alonzo.Transform.ANorm    as A
import qualified Language.Alonzo.Transform.NameBind as B
import qualified Language.Alonzo.Syntax.Source      as S



transform :: S.Term -> A.Exp
transform = A.normalize . B.namebind