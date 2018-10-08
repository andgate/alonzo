module Language.Alonzo.Build.Types where

import Data.Default.Class

data Package contents =
  Package { pkgName :: String
          , pkgRoot :: FilePath
          , pkgMods :: [Module contents]
          }


data Module contents =
  Module { modName :: String
         , modPath :: FilePath
         , modHash :: Int
         , modContents :: contents
         }

instance Default contents => Default (Module contents) where
  def = Module { modName = ""
               , modPath = ""
               , modHash = 0
               , modContents  = def
               }
