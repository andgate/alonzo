{-# LANGUAGE RecursiveDo
           , RankNTypes
           , OverloadedStrings
           , FlexibleContexts
           , TupleSections
  #-}
module Language.Alonzo.Parse.Decl where

import Control.Applicative hiding (optional)
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Text (Text, pack)

import Language.Alonzo.Parse.Helpers
import Language.Alonzo.Lex.Token (Token)
import Language.Alonzo.Syntax.Source
import Language.Alonzo.Syntax.Source.Helpers

import Text.Earley
import Text.Earley.Mixfix

import qualified Data.List.NonEmpty as NE


-- -----------------------------------------------------------------------------
-- Grammar for Alonzo
declGrammar :: Grammar r (Prod r String Token Decl)
declGrammar = mdo

-- -----------------------------------------------------------------------------
-- Declaration Rules

    decl <- rule $ (linefold decl') <|> (decl' <* eof)

    decl' <- rule $
        ( importDecl
      <|> termDecl
      <|> funDecl)
      <?> "Declaration"

    importDecl <- rule $
      let ex (n, _) = ImportDecl n
      in ex <$> (rsvp "import" *> conId)

    termDecl <- rule $
      (TermDecl <$> term)

    funDecl <- rule $
      (FunDecl <$> fun) <?> "Function"


-- -----------------------------------------------------------------------------
-- Declaration Rules


    fun <- rule $
      let ex (n, _) body = Fun n body
      in ex <$> varId <*> (rsvp "=" *> term)


-- -----------------------------------------------------------------------------
-- Value Rules

    val <- rule $
            ( first VInt   <$> intLit )
        <|> ( first VFloat <$> floatLit )
        <|> ( first VChar  <$> charLit )
        <|> ( first VBool  <$> boolLit )


-- -----------------------------------------------------------------------------
-- Term Rules

    -- The expression precedence chain, starting at aexp as the base with the highest precedence.
    term <- rule $
      cterm

    term0 <- rule $ optional term

    cterm <- rule $
          termLam
      <|> bterm

    cterm0 <- rule $ optional cterm

    bterm <- rule $
      termApp <|> aterm

    aterm <- rule $
          (termVar <?> "variable")
      <|> (termVal <?> "value")
      <|> (termPrim)
      <|> (termLet <?> "let ... in ...")
      <|> (termWild <?> "_")
      <|> termParens


    -- Terms

    termVar <- rule $
      let ex (v, l) = TLoc l $ TVar v
      in ex <$> varId

    termVal <- rule $
      let ex (v, l) = TLoc l $ TVal v
      in ex <$> val

    termPrim <- rule $
      let ex (i, l) t1 t2 = TLoc l $ TPrim (readPrimInstr i) t1 t2
      in ex <$> primId <*> aterm <*> aterm

    -- Evaluation
    termApp <- rule $
      let ex f@(TLoc l1 _) xs
            = TLoc (mconcat $ l1:(locOf <$> xs)) $ TApp f (NE.fromList xs)
      in ex <$> aterm <*> some aterm

    termLam <- rule $
      let ex (_,l1) vs body@(TLoc l2 _) =
            TLoc (l1<>l2) $ TLam (NE.fromList vs) body
      in ex <$> rsvp "\\" <*> some varId' <*> (rsvp "." *> term)

    termLet <- rule $
      let ex (_, l1) bs body@(TLoc l2 _) = TLoc (l1<>l2) $ TLet (NE.fromList bs) body
      in ex <$> rsvp "let" <*> block fun <*> (rsvp "in" *> term)

    termParens <- rule $
      let ex (t, l) = TLoc l $ TParens t
      in ex <$> parensLoc term

    termWild <- rule $
      let ex (_, l) = TLoc l $ TWild
      in ex <$> rsvp "_"

{-
-- -----------------------------------------------------------------------------
-- Branch Rules

    branches <- rule $ block branch

    branch <- rule $
      let ex p@(PLoc l1 _) e@(ELoc l2 _) = (l1<>l2, p, e)
      in ex <$> (pat <* rsvp "->") <*> bexp


-- -----------------------------------------------------------------------------
-- Type Alias Rules

    aliasDef <- rule $
      AliasDef <$> (fst <$> conId)
               <*> many (fst <$> varId)
               <*> (rsvp "=" *> typ)

-- -----------------------------------------------------------------------------
-- Data Type Rules

    dataDef <- rule $
      DataDef <$> (fst <$> conId)
              <*> many (fst <$> varId)
              <*> (rsvp ":=" *> constrs)

    constrs <- rule $
      sep (rsvp "|") (constr <|> recConstr)

    constr <- rule $
      ConstrDef <$> (fst <$> conId)
                <*> many atyp

    recConstr <- rule $
      RecordDef <$> (fst <$> conId) <*> recFields

    recFields <- rule $ curlys $ commaSep $
      (,) <$> (fst <$> varId) <*> (rsvp ":" *> typ)

-- -----------------------------------------------------------------------------
-- Type Class Rules

    classDef <- rule $
      ClassDef <$> (rsvp "class" *> tyContext)
               <*> (fst <$> conId)
               <*> many atyp
               <*> (rsvp "has" *> block0 sig)


-- -----------------------------------------------------------------------------
-- Type Class Instance Rules

    instDef <- rule $
      InstDef
        <$> tyContext
        <*> (fst <$> conId)
        <*> many atyp
        <*> (rsvp "has" *> block0 def)


-- -----------------------------------------------------------------------------
-- Foreign Rules

    forgnDef <- rule $
      rsvp "foreign" *> (forgnImport <|> forgnExport)

    forgnImport <- rule $
      let ex ft (srcN, l1) (hkN, l2) ty
            = ForeignImport ft (L l1 $ pack srcN) (L l2 hkN) ty
      in ex <$> (rsvp "import" *> forgnType)
            <*> strLit
            <*> (varId <|> conId <|> opId)
            <*> (rsvp ":" *> typ)

    forgnExport <- rule $
      let ex ft (n, l)
            = ForeignExport ft (L l n)
      in ex <$> (rsvp "export" *> forgnType)
         <*> (varId <|> conId <|> opId)

    forgnType <- rule $
      ForeignC <$ rsvp "ccall"


-- -----------------------------------------------------------------------------
-- Fixity Rules

    fixityDecl <- rule $
      let ex fx (p, l) ops =
            Fixity fx (L l $ fromIntegral p) (wrapL <$> ops)
      in ex <$> fixityKind <*> intLit <*> some opId

    fixityKind <- rule $
      infixL <|> infixR <|> infixN <|> prefix <|> postfix

    infixL <- rule $
      InfixL <$ rsvp "infixl"

    infixR <- rule $
      InfixR <$ rsvp "infixr" 

    infixN <- rule $
      InfixN <$ rsvp "infix" 

    prefix <- rule $
      Prefix <$ rsvp "prefix" 

    postfix <- rule $
      Postfix <$ rsvp "postfix" 
-}

    return decl