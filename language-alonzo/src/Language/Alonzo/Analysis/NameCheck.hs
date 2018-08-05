{-# LANGUAGE LambdaCase #-}
module Language.Alonzo.Analysis.NameCheck where

import Data.Foldable
import Data.List
import Data.Text (Text)
import Data.Set (Set)
import Language.Alonzo.Syntax.Location
import Language.Alonzo.Syntax.Source

import qualified Data.Set as Set
import qualified Data.List.NonEmpty  as NE


data NameError
  = NameNotFound Loc Text
  | NameConflict Loc Text

namecheck :: Set Text -> Loc -> Term -> [NameError]
namecheck vs l = \case
  TVar v -> 
    if v `Set.member` vs
      then []
      else [NameNotFound l v]

  TVal _ -> []
  
  TApp f xs -> 
    let nc = namecheck vs l
        f' = nc f
        xs' = nc <$> NE.toList xs
    in f' ++ fold xs'

  TPrim _ x y ->
    let x' = namecheck vs l x
        y' = namecheck vs l y
    in x' ++ y'
  
  TLam us body ->
    let us' = NE.toList us
        vs' = foldr (\v vs -> Set.insert v vs) vs us'
        freq s = map (\x -> (head x, length x)) . group . sort $ s
        conflicts = foldr (\(v, n) acc -> if n == 1 then acc else (NameConflict l v):acc) [] (freq us')
        body' = namecheck vs' l body
    in body' ++ (reverse conflicts)

  TLet bs body ->
    let bs' = NE.toList bs
        us = fst <$> bs'
        xs = snd <$> bs'
        vs' = foldr (\v vs -> Set.insert v vs) vs us
        xs' = namecheck vs' l <$> xs
        body' = namecheck vs' l body
    in body' ++ fold xs'
  
  TLoc    l' t -> namecheck vs l' t
  TParens t    -> namecheck vs l t
  TWild        -> []
