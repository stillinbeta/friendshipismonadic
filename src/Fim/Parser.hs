module Fim.Parser (fimClass) where

import qualified Fim.Types as Types

import Control.Applicative (many)
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Text.Parsec (try, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, space, string, newline, noneOf, oneOf)
import Text.Parsec.Combinator (choice, manyTill, many1)

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
  void $ string "Today I learned "
  name <- identifier
  void newline
  void $ string "That's all about " >> string name >> punctuation
  return $ Just (Types.Function name)

punctuation :: Parser ()
punctuation = void $ oneOf ",.!?"
