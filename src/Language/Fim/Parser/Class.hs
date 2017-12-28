module Language.Fim.Parser.Class (fimClass) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Tokens (identifier, terminator)
import Language.Fim.Parser.Util (Parser, token, token_)
import qualified Language.Fim.Lexer.Token as Token
-- import qualified Language.Fim.Parser.Methods as Methods

import Control.Applicative (many)
import Control.Monad (void)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.Parsec ((<|>), (<?>))
import Text.Parsec.Combinator (choice, manyTill)

fimClass :: Parser Types.Class
fimClass = do
  token_ Token.ClassStart
  superclass <- fimSuperClass <$> identifier
  token_ Token.Colon
  name <- Types.Identifier <$> identifier
  terminator
  token_ Token.Newline
  -- funcs <- classBody
  fimClassSignoff
  return Types.Class { Types.className = name
                     , Types.classSuper = superclass
                     , Types.classBody = []
                     }

fimSuperClass :: T.Text -> Types.Class
fimSuperClass id =
  if id == T.pack "Princess Celestia"
  then Types.Celestia
  else Types.ClassByName id

fimClassSignoff :: Parser ()
fimClassSignoff = do
  token_ Token.ClassEnd
  token_ Token.tIdentifier
  terminator
  token_ Token.Newline

-- classBody :: Parser [Types.Function]
-- classBody = do
--   body <- manyTill ((Methods.emptyLine <?> "empty line")
--                      <|> (Methods.method <?> "method"))
--           fimClassSignoff
--   return $ catMaybes body
