module Language.Fim.Parser.Methods ( method
                                   , methods
                                   ) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Tokens (terminator, identifier)
import Language.Fim.Parser.Util (Parser, token, token_, manyWithNewlines)
import Language.Fim.Parser.Statement (statements)
import qualified Language.Fim.Lexer.Token as Token

import Control.Monad (unless)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Text.Parsec.Combinator (optionMaybe)

methods :: Parser [Types.Function]
methods = manyWithNewlines method

method :: Parser Types.Function
method = do
  isMain <- isJust <$> optionMaybe (token Token.MainMethod)
  methodDec
  name <- identifier
  terminator
  token_ Token.Newline
  stmts <- statements
  methodEnd name
  token_ Token.Newline
  return (Types.Function (Types.Identifier name) isMain stmts)

methodEnd :: T.Text -> Parser ()
methodEnd idt = do
  token_ Token.MethodDecEnd
  end <- identifier
  unless (idt == end) $ fail "expected method end"
  terminator

methodDec :: Parser ()
methodDec = token_ Token.I >> token_ Token.MethodDec
