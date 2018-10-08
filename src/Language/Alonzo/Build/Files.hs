{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Alonzo.Build.Files where

import Control.Monad.Chronicle
import Control.Monad.Except
import Control.Exception
import Data.Bifunctor
import Data.Char (isLower, isUpper, isAlphaNum)
import Data.Hashable
import Data.List
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust)
import Data.These
import Data.Text (Text)
import Language.Alonzo.Build.Types
import System.Directory
import System.Directory.Tree
import System.FilePath

import qualified Data.Text.IO as T


-------------------------------------------------------------------------------------
-- | Types

data PkgReadError
  = InvalidPackageName FilePath String
  | InvalidModuleName  FilePath String
  | InvalidExtension   FilePath


-------------------------------------------------------------------------------------
-- | Package Validation

validatePackage :: (Package Text) -> [PkgReadError]
validatePackage Package{..} =
  let err1 = if isPackageNameValid pkgName
                then []
                else [InvalidPackageName pkgRoot pkgName]
      err2 = mconcat $ map validateModule pkgMods
  in err1 ++ err2


validateModule :: (Module Text) -> [PkgReadError]
validateModule Module{..} =
  let err1 = if isModuleQNameValid modName
               then []
               else [InvalidModuleName modPath modName]
      err2 = if ".al" `isExtensionOf` modPath
               then []
               else [InvalidExtension modPath]
  in err1 ++ err2


-- Package names must start with a lowercase letter
-- Package names can contain letters, digits, underscores, dashes
isPackageNameValid :: String -> Bool
isPackageNameValid []     = False
isPackageNameValid (c:cs) = isLower c && validBody
  where validBody = and $ map (\c' -> isAlphaNum c' || c' == '_' || c' == '-') cs


-- Module names must start with an uppercase letter.
-- Module names can contain letters or digits.
-- For qualified module names, modules are seperated by periods.
isModuleQNameValid :: String -> Bool
isModuleQNameValid [] = False
isModuleQNameValid cs = and . map isModuleNameValid $ splitWhen (== '.') cs


-- Module names must start with an uppercase letter.
-- Module names can contain letters or digits.
isModuleNameValid :: String -> Bool
isModuleName []          = False
isModuleNameValid (c:cs) = isUpper c && validBody
  where validBody = and $ map (\c' -> isAlphaNum c') cs



-------------------------------------------------------------------------------------
-- | Package Loading

-- Read a package from a filepath.
-- This will either result in a list of IOErrors
-- or an unvalidated package.
-- Packages are checked for naming errors seperately. 
readPackage :: FilePath -> IO (Either [IOError] (Package Text))
readPackage fp = do
  dir <- readDirectoryWith (loadModuleFile fp) fp
  case processAnchoredDirTree dir of
    This errs    -> return $ Left errs
    That pkg     -> return $ Right pkg
    These errs _ -> return $ Left errs


-- Should take a path to the module and the root
-- folder of the package, and produce a module name.
pathToQName :: FilePath -> FilePath -> String
pathToQName root fp = intercalate "." mnames
  where
    rootLength = length $ splitDirectories root
    mnames = drop rootLength . splitDirectories . dropExtensions $ fp


processAnchoredDirTree :: AnchoredDirTree (Module Text) -> These [IOException] (Package Text)
processAnchoredDirTree (root :/ dir) = do
  ms <- processDirTree dir
  return Package { pkgName = takePathEnd root, pkgRoot = root, pkgMods = ms }

processDirTree :: DirTree (Module Text) -> These [IOException] [Module Text]
processDirTree = \case
  Failed _ err  -> disclose [err]
  Dir    _ dirs -> second mconcat $ traverse processDirTree dirs
  File   _ mod  -> return [mod]


loadModuleFile :: FilePath -> FilePath -> IO (Module Text)
loadModuleFile root fp = do
  contents <- T.readFile fp
  return Module { modName = pathToQName root fp
                , modPath = fp
                , modHash = hash contents
                , modContents  = contents
                }

--------------------------------------------------------------------------------------
-- | Helpers

-- Take the end of a path
takePathEnd :: FilePath -> String
takePathEnd = last . splitDirectories
