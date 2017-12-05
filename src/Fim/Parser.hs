module Fim.Parser (fimClass) where

import qualified Fim.Types as Types

import Fim.Parser.Tokens (identifier)
import qualified Fim.Parser.Methods as Methods

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

classBody :: Parser [Types.Function]
classBody = do
  funcs <- manyTill (Methods.emptyLine <|> Methods.method) (try fimClassSignoff)
  return $ catMaybes funcs
