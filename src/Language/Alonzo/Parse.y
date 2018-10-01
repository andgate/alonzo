{

{-# LANGUAGE OverloadedStrings #-}
module Language.Alonzo.Parse where

import Language.Alonzo.Lex
import Language.Alonzo.Lex.Token
import Language.Alonzo.Syntax.Source
import Language.Alonzo.Syntax.Builtin
import Language.Alonzo.Syntax.Location

}

%name alonzoFile
%tokentype { Token }
%error { parseError }


%token
  backslash          { Token (TokenRsvp "\\") _ $$ }
  colon              { Token (TokenRsvp ":") _ $$ }
  comma              { Token (TokenRsvp ",") _ $$ }
  equals             { Token (TokenRsvp "=") _ $$ }
  lambda             { Token (TokenRsvp "λ") _ $$ }
  period             { Token (TokenRsvp ".") _ $$ }
  underscore         { Token (TokenRsvp "_") _ $$ }

  openParen          { Token (TokenRsvp "(") _ $$ }
  closeParen         { Token (TokenRsvp ")") _ $$ }
  openBracket        { Token (TokenRsvp "[") _ $$ }
  closeBracket       { Token (TokenRsvp "]") _ $$ }
  openCurly          { Token (TokenRsvp "{") _ $$ }
  closeCurly         { Token (TokenRsvp "}") _ $$ }

  sub                { Token (TokenRsvp ":>") _ $$ }
  super              { Token (TokenRsvp "<:") _ $$ }

  module             { Token (TokenRsvp "module") _ $$ }
  import             { Token (TokenRsvp "import") _ $$ }
  data               { Token (TokenRsvp "data"  ) _ $$ }

  let                { Token (TokenRsvp "let")  _ $$ }
  in                 { Token (TokenRsvp "in")   _ $$ }
  where              { Token (TokenRsvp "where") _ $$ }

  varId              { Token (TokenVarId  _) _ _ }
  conId              { Token (TokenConId  _) _ _ }
  qvarId             { Token (TokenQVarId _) _ _ }
  qconId             { Token (TokenQConId _) _ _ }

  primAdd            { Token (TokenPrimId "#add") _ $$ }
  primSub            { Token (TokenPrimId "#sub") _ $$ }

  integer            { Token (TokenInteger _) _ _ }
  double             { Token (TokenDouble  _) _ _ }
  char               { Token (TokenChar    _) _ _ }
  string             { Token (TokenString  _) _ _ }
  boolean            { Token (TokenBool    _) _ _ }

  blockOpen          { Token (TokenBlockOpen ) _ $$ }
  blockClose         { Token (TokenBlockClose) _ $$ }

  lineOpen           { Token (TokenLineOpen ) _ $$ }
  lineClose          { Token (TokenLineClose) _ $$ }

%%

Module : blockOpen ModDecl Stmts0 blockClose { mkModule $2 $3 }

ModDecl : lineOpen module ModName lineClose { $3 }

ModName : conId        { mkQName (extractId $1) }
        | qconId       { mkQName (extractId $1) }

Value : integer  { fmap VInt    (extractInteger $1) }
      | double   { fmap VFloat  (extractDouble  $1) }
      | char     { fmap VChar   (extractChar    $1) }
      | string   { fmap VString (extractString  $1) }
      | boolean  { fmap VBool   (extractBool    $1) }



StmtBlock : blockOpen Stmts blockClose { $2 }

Stmts : StmtLn       { [$1] }
      | Stmts StmtLn { $2 : $1 }

Stmts0 : {- empty -} { [] }
       | Stmts       { $1 }

StmtLn : lineOpen Stmt lineClose { $2 }

Stmt : Import { SImport $1 }

Import : import ModName { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
