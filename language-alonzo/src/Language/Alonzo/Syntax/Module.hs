{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
  #-}
module Language.Alonzo.Syntax.Module where


import Control.Lens
import Data.Map.Strict (Map)
import Data.Text (Text)

import qualified Data.Map.Strict     as Map

import Language.Alonzo.Syntax.Bound

-- -----------------------------------------------------------------------------
-- | Module

data Project = Project
  { _prjName    :: Text
  , _prjMods    :: Map Text Module
  , _prjClasses :: Map Text Class
  }

data Module = Module
  { _mdName    :: Text
  , _mdImports :: [Text]
  , _mdFns     :: Map Text Term
  }

data Class = Class
  { _clName :: Text
  , _clDict :: [Text]
  }


makeLenses ''Project
makeLenses ''Module
makeLenses ''Class


newProject :: Text -> Project
newProject n = Project n Map.empty Map.empty

loadProject :: Text -> [Text] -> Project 
loadProject n srcs = undefined

loadModule :: Text -> Module
loadModule src = undefined

loadClass :: Text -> Class
loadClass src = undefined