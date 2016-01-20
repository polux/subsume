-- Copyright 2015 Google Inc. All Rights Reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License")--
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


module Parser (parseModule) where

import Datatypes
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

language = javaStyle
  { P.reservedNames = ["CONSTRUCTORS", "FUNCTIONS", "RULES"]
  , P.reservedOpNames = ["=", "->", ":", "*"]
  }

lexer = P.makeTokenParser javaStyle

parens = P.parens lexer
identifier = P.identifier lexer
colon = P.colon lexer
comma = P.comma lexer
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer

equals = P.reservedOp lexer "="
arrow = P.reservedOp lexer "->"
pipe = P.reservedOp lexer "|"
star = P.reservedOp lexer "*"

constructorsKw = P.reserved lexer "CONSTRUCTORS"
functionsKw = P.reserved lexer "FUNCTIONS"
rulesKw = P.reserved lexer "RULES"

funName = FunName <$> identifier
varName = VarName <$> identifier
typeName = TypeName <$> identifier

term :: Parser Term
term = try (Appl <$> funName <*> parens (term `sepBy` comma))
       <|> Var <$> varName

rule :: Parser Rule
rule = mkRule <$> term <*> arrow <*> term
  where mkRule lhs _ rhs = Rule lhs rhs

rules :: Parser [Rule]
rules = many rule

funType :: Parser ([TypeName], TypeName)
funType = try (mkType <$> (typeName `sepBy` star) <*> arrow <*> typeName)
          <|> mkEmptyType <$> typeName
  where
    mkType domain _ range = (domain, range)
    mkEmptyType range = ([], range)

decl :: Parser Decl
decl = mkDecl <$> funName <*> colon <*> funType
  where mkDecl f _ (domain, range) = Decl f domain range

decls :: Parser [Decl]
decls = many (try decl)

modul :: Parser Module
modul = mkModule <$> constructorsKw <*> decls <*> functionsKw <*> decls <*> rulesKw <*> rules
  where mkModule _ ctors _ funs _ rules = Module (Signature ctors funs) rules

parseModule :: String -> String -> Either ParseError Module
parseModule sourceName input = parse (whiteSpace *> modul <* eof) sourceName input
