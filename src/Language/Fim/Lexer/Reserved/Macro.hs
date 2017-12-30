module Language.Fim.Lexer.Reserved.Macro ( reservedWords
                                         ) where

import Language.Haskell.TH
import Data.List (intercalate)

reservedWords :: [String] -> Q [Dec]
reservedWords str = do
  dReservedWords <- genReservedWords str
  fToString <- genToString str
  return [dReservedWords, fToString]

genReservedWords :: [String] -> Q Dec
genReservedWords strs =
  return $ DataD [] (mkName "ReservedWords") [] Nothing (map makeCon strs) []

genToString :: [String] -> Q Dec
genToString = return . FunD toString . map makeToString

makeToString :: String -> Clause
makeToString str = let name = makeName str
                       lit = LitE $ StringL str in
                       Clause [ConP name []] (NormalB lit ) []

underscore :: String -> String
underscore = intercalate "_" . words

toString :: Name
toString = mkName "toString"

makeCon :: String -> Con
makeCon = flip NormalC [] . makeName

makeName :: String -> Name
makeName = mkName . underscore . ("R_"++)
