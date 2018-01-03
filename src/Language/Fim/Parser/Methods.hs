module Language.Fim.Parser.Methods ( method
                                   , methods
                                   ) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Tokens (terminator, identifier)
import Language.Fim.Parser.Util (Parser, token, token_)
import Language.Fim.Parser.Statement (statements)
import qualified Language.Fim.Lexer.Token as Token

import Control.Applicative (many)
import Control.Monad (unless)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Text.Parsec.Combinator (optionMaybe)

methods :: Parser [Types.Function]
methods = many method

method :: Parser Types.Function
method = do
  isMain <- isJust <$> optionMaybe (token Token.MainMethod)
  token_ Token.MethodDec
  name <- identifier
  terminator
  stmts <- statements
  methodEnd name
  return (Types.Function name isMain stmts)

methodEnd :: T.Text -> Parser ()
methodEnd idt = do
  token_ Token.MethodDecEnd
  end <- identifier
  unless (idt == end) $ fail "expected method end"
  terminator
