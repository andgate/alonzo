module Language.Alonzo.Closure where

import Data.Text       (Text)
import Data.Map (Map)
import Language.Alonzo.Syntax.Bound

import qualified Data.Map as M


data Closure = Closure (Map Text Term)

-- Put some helpers here
empty :: Closure
empty = Closure $ M.empty

lookup :: Text -> Closure -> Maybe Term
lookup v (Closure m) = M.lookup v m