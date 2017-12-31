{-# LANGUAGE OverloadedStrings #-}

module Language.Fim.Eval.Errors ( noMainMethod
                                , compareNull
                                , redeclaredVariable
                                , undefinedVariable
                                , assignToConstant
                                , cantDeduceAnd
                                , unexpectedType
                                , variableTypeMismatch
                                ) where

import qualified Language.Fim.Types as Types
import qualified Language.Fim.Eval.Types as ETypes

import Data.Text as T

noMainMethod :: T.Text
noMainMethod = "No main method"

compareNull :: T.Text
compareNull = "Can't compare null"

redeclaredVariable :: Types.Variable -> T.Text
redeclaredVariable = T.append "Can't redeclare variable" . showVariable

undefinedVariable :: Types.Variable -> T.Text
undefinedVariable = T.append "Undefined variable " . showVariable

assignToConstant :: Types.Variable -> T.Text
assignToConstant = T.append "Can't assign to constant " . showVariable

cantDeduceAnd :: ETypes.ValueBox -> ETypes.ValueBox -> T.Text
cantDeduceAnd v1 v2 =
  T.concat ["Expected operands to be booleans or numberboxes,"
           , " instead got "
           , boxTypeName v1
           , " and "
           , boxTypeName v2
           ]

unexpectedType :: ETypes.ValueBox -> Types.Type -> T.Text
unexpectedType vbox typ =
  T.intercalate " " [ "expected"
                    , boxTypeName vbox
                    , "to be a"
                    , typeName typ
                    ]

variableTypeMismatch :: ETypes.ValueBox -> Types.Variable -> Types.Type -> T.Text
variableTypeMismatch box var typ =
  T.intercalate " " ["Can't assign"
                    , boxTypeName box
                    , "to variable"
                    , showVariable var
                    , "of type"
                    , typeName typ
                    ]
-- utilities

showVariable :: Types.Variable -> T.Text
showVariable Types.Variable {Types.vName = v} = T.concat ["<", v, ">"]

boxTypeName :: ETypes.ValueBox -> T.Text
boxTypeName = maybe "nothing" typeName . ETypes.typeForBox

typeName :: Types.Type -> T.Text
typeName typ = case typ of
  Types.TNumber    -> "number"
  Types.TString    -> "string"
  Types.TCharacter -> "character"
  Types.TBoolean   -> "argument"
