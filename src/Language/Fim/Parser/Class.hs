module Language.Fim.Parser.Class (fimClass) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Tokens (identifier, terminator)
import Language.Fim.Parser.Util (Parser, token_)
import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Parser.Methods (methods)

import qualified Data.Text as T

fimClass :: Parser Types.Class
fimClass = do
  token_ Token.ClassStart
  superclass <- fimSuperClass <$> identifier
  token_ Token.Colon
  name <- identifier
  terminator
  funcs <- methods
  fimClassSignoff
  return Types.Class { Types.className = name
                     , Types.classSuper = superclass
                     , Types.classBody = funcs
                     }

fimSuperClass :: T.Text -> Types.Class
fimSuperClass ident =
  if ident == T.pack "Princess Celestia"
  then Types.Celestia
  else Types.ClassByName ident

fimClassSignoff :: Parser ()
fimClassSignoff = do
  token_ Token.ClassEnd
  token_ Token.tIdentifier
  terminator
