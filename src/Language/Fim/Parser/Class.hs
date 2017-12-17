module Language.Fim.Parser.Class (fimClass) where

import qualified Language.Fim.Types as Types

import Language.Fim.Parser.Tokens (identifier)
import qualified Language.Fim.Parser.Methods as Methods

import Control.Applicative (many)
import Control.Monad (void)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.Parsec ((<|>), (<?>))
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (string, newline, noneOf)
import Text.Parsec.Combinator (choice, manyTill)

fimClass :: Parser Types.Class
fimClass = do
  string "Dear " <?> "salutation"
  superclass <- fimSuperClass
  name <- string ": " >> identifier
  void newline
  funcs <- classBody
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
  name <- T.pack <$> many (noneOf ":")
  return $ Types.ClassByName name

fimClassSignoff :: Parser ()
fimClassSignoff = do
  string "Your faithful student, " <?> "signoff (Your faithful student)"
  identifier
  void newline

classBody :: Parser [Types.Function]
classBody = do
  body <- manyTill ((Methods.emptyLine <?> "empty line")
                     <|> (Methods.method <?> "method"))
          fimClassSignoff
  return $ catMaybes body
