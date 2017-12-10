module Language.Fim.Parser.Class (fimClass) where

import qualified Language.Fim.Types as Types

import Language.Fim.Parser.Tokens (identifier)
import qualified Language.Fim.Parser.Methods as Methods

import Control.Applicative (many)
import Control.Monad (void)
import qualified Data.Text as T
import Data.Maybe (isJust, catMaybes)
import Text.Parsec (try, (<|>), (<?>))
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (anyChar, space, string, newline, noneOf, oneOf, char)
import Text.Parsec.Combinator (choice, manyTill, many1, optionMaybe)

fimClass :: Parser Types.Class
fimClass = do
  string "Dear " <?> "salutation"
  superclass <- fimSuperClass
  name <- string ": " >> identifier
  void newline
  (funcs, student) <- classBody
  --fimClassSignoff
  return Types.Class { Types.className = name
                     , Types.classSuper = superclass
                     , Types.classBody = funcs
                     , Types.classStudent = student
                     }

fimSuperClass :: Parser Types.Class
fimSuperClass = choice [fimClassCelestia, fimClassByName]

fimClassCelestia :: Parser Types.Class
fimClassCelestia = string "Princess Celestia" >> return Types.Celestia

fimClassByName :: Parser Types.Class
fimClassByName = do
  name <- T.pack <$> many (noneOf ":")
  return $ Types.ClassByName name

fimClassSignoff :: Parser Types.Identifier
fimClassSignoff = do
  string "Your faithful student, " <?> "signoff (Your faithful student)"
  student <- identifier
  void newline
  return student

classBody :: Parser ([Types.Function], Types.Identifier)
classBody = do
  maybeStudent <- optionMaybe (try fimClassSignoff)
  case maybeStudent of
    Just student -> return ([], student)
    Nothing -> do
      maybeF <- (Methods.emptyLine <|> Methods.method) <?> "expected empty line or new method"
      (fs, student) <- classBody
      return $ case maybeF of
                 Nothing -> (fs, student)
                 Just f -> (f:fs, student)
