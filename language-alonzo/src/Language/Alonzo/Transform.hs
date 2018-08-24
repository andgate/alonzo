module Language.Alonzo.Transform where

import Language.Alonzo.Transform.CConv (cconv)
import Language.Alonzo.Transform.Lift  (lift)

import qualified Language.Alonzo.Transform.ANorm    as A
import qualified Language.Alonzo.Transform.NameBind as B
import qualified Language.Alonzo.Syntax.Source      as S
import qualified Language.Alonzo.Transform.Lift     as Lifted


-- Syntax transformation goes Source -> NameBind -> ANorm -> Lifted

-- Basic tranc prepares a closure for reduction
transformANorm :: S.Closure -> A.Closure
transformANorm = A.normalize . B.namebind

-- Transform prepares a closure for compilation to llvm
-- DISABLED while i work on transformANorm
--transformLifted :: S.Closure -> Lifted.Closure
--transformLifted = lift . cconv . A.normalize . B.namebind

-- side note: 
--     I'd love to have the evaluator run compiled
--   llvm btyecode instead of reducing a-normal form,
--   but then we'd lose the ability to print programs
--   as evaluated terms.
--
--     Ultimately, compiled code will theoretically reduce
--   all given expressions, but it can only communicate with
--   the rest of the computer via external libraries. So side-effects
--   are generated during reduction. Alonzo has strict evaluation,
--   so the side effects can be predicted.