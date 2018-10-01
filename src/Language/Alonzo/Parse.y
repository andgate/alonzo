{

{-# LANGUAGE OverloadedString #-}
module Language.Alonzo.Parser where

import Language.Alonzo.Lexer
import Language.Alonzo.Lex.Token
import Language.Alonzo.Syntax.Source
import Language.Alonzo.Syntax.Builtin
import Language.Alonzo.Syntax.Loc

}

%name alonzoExp
%tokentype { Token }
%error { parseError }


%token
  backslash          { Token (TokenRsvp "\\") _ $$ }
  lambda             { Token (TokenRsvp "λ") _ $$ }
  period             { Token (TokenRsvp ".") _ $$ }
  comma              { Token (TokenRsvp ",") _ $$ }
  openParen          { Token (TokenRsvp "(") _ $$ }
  closeParen         { Token (TokenRsvp ")") _ $$ }
  openBracket        { Token (TokenRsvp "[") _ $$ }
  closeBracket       { Token (TokenRsvp "]") _ $$ }
  openCurly          { Token (TokenRsvp "{") _ $$ }
  closeCurly         { Token (TokenRsvp "}") _ $$ }
  equals             { Token (TokenRsvp "=") _ $$ }
  underscore         { Token (TokenRsvp "_") _ $$ }
  colon              { Token (TokenRsvp ":") _ $$ }

  sub                { Token (TokenRsvp ":>") _ $$ }
  super              { Token (TokenRsvp "<:") _ $$ }

  module             { Token (TokenRsvp "module") _ $$ }
  import             { Token (TokenRsvp "import") _ $$ }
  data               { Token (TokenRsvp "data"  ) _ $$ }

  let                { Token (TokenRsvp "let")  _ $$ }
  in                 { Token (TokenRsvp "in")   _ $$ }

  varId              { Token (TokenVarId  _) _ _ }
  conId              { Token (TokenConId  _) _ _ }
  qvarId             { Token (TokenQVarId _) _ _ }
  qvarId             { Token (TokenQConId _) _ _ }


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

Module : blockOpen ModuleDecl Statements blockClose { mkModule $2 $3 }

ModuleDecl : lineOpen module ModuleName lineClose { $3 }

ModuleName : conId        { mkQname (extractId $1) }
           | qconId       { mkQName (extractId $1) }

Value : integer  { VInt    <$> (extractInteger $1) }
      | double   { VFloat  <$> (extractDouble  $1) }
      | char     { VChar   <$> (extractChar    $1) }
      | string   { VString <$> (extractString  $1) }
      | boolean  { VBool   <$> (extractBool    $1) }


Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
    | Exp '+' Exp            { Plus $1 $3 }
    | Exp '-' Exp            { Minus $1 $3 }
    | Exp '*' Exp            { Times $1 $3 }
    | Exp '/' Exp            { Div $1 $3 }
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Negate $2 }
    | int                    { Int $1 }
    | var { Var $1 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
