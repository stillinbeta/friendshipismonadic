module Fim.Parser (fimClass) where

import qualified Fim.Types as Types

import Control.Applicative (many)
import Control.Monad (void)
import Data.Maybe (isJust, catMaybes)
import Text.Parsec (try, (<|>), (<?>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, space, string, newline, noneOf, oneOf, char)
import Text.Parsec.Combinator (choice, manyTill, many1, optionMaybe)

fimClass :: Parser Types.Class
fimClass = do
  superclass <- string "Dear " >> fimSuperClass
  name <- string ": " >> identifier
  void newline
  funcs <-  classBody
  --fimClassSignoff
  return Types.Class { Types.className = name
                     , Types.classSuper = superclass
                     , Types.classBody = funcs
                     }

fimSuperClass :: Parser Types.Class
fimSuperClass = choice [fimClassCelestia, fimClassByName]

fimClassCelestia :: Parser Types.Class
fimClassCelestia = string "Princess Celestia" >> return Types.Celestia

fimClassByName :: Parser Types.Class
fimClassByName = do
  name <- many $ noneOf ":"
  return $ Types.ClassByName name

fimClassSignoff :: Parser ()
fimClassSignoff = do
  void $ string "Your faithful student, "
  -- TODO: record name?
  _ <- identifier
  void newline

identifier :: Parser String
identifier = manyTill anyChar punctuation

classBody :: Parser [Types.Function]
classBody = do
  funcs <- manyTill (method <|> emptyLine) (try fimClassSignoff)
  return $ catMaybes funcs

emptyLine :: Parser (Maybe Types.Function)
emptyLine = many1 space >> return Nothing


method :: Parser (Maybe Types.Function)
method = do
  isMain <- isJust <$> optionMaybe (string "Today ")
  void $ string "I learned "
  name <- identifier
  void $ newline
  stmts <- manyTill statements (try $ methodEnd name)
  return $ Just (Types.Function name isMain stmts)

methodEnd :: String -> Parser ()
methodEnd name = (string "That's all about " >> string name >> punctuation) <?> "method end"

punctuation :: Parser ()
punctuation = void $ oneOf ",.!?"

statements :: Parser Types.Statement
statements = choice [iSaid] <?> "statement"

iSaid :: Parser Types.Statement
iSaid = do
  void $ string "I said" >> space
  val <- value
  punctuation
  void $ newline
  return $ Types.SISaid val

value :: Parser Types.Value
value = Types.VLiteral <$> literal

literal :: Parser Types.Literal
literal = do
  void $ oneOf "\"“"
  str <- many $ noneOf "\"”"
  void $ oneOf "\"”"
  return $ Types.StringLiteral str
