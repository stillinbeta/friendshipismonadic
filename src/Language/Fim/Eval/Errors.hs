{-# LANGUAGE OverloadedStrings #-}

module Language.Fim.Eval.Errors ( noMainMethod
                                , compareNull
                                , redeclaredVariable
                                , undefinedVariable
                                , assignToConstant
                                , cantDeduceAnd
                                , unexpectedType
                                , variableTypeMismatch
                                , iterateWrongType
                                , methodIncorrectArgCount
                                , methodIncorrectArgType
                                , noSuchMethod
                                , methodIncorrectReturn
                                , arrayTypeError
                                , cantIndex
                                , invalidIndex
                                ) where

import qualified Language.Fim.Types as Types
import qualified Language.Fim.Eval.Types as ETypes

import qualified Data.Text as T

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
  T.concat ["Can only iterate over numbers and characters,"
           , " instead got "
           , boxTypeName v1
           , " and "
           , boxTypeName v2
           ]

unexpectedType :: ETypes.ValueBox -> Types.Type -> T.Text
unexpectedType vbox typ =
  T.intercalate " " [ "Expected"
                    , boxTypeName vbox
                    , "to be of type"
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

iterateWrongType :: ETypes.ValueBox -> ETypes.ValueBox -> T.Text
iterateWrongType box1 box2 =
  T.intercalate " " [ "Can only iterate over numbers and letters, but got"
                    , boxTypeName box1
                    , "and"
                    , boxTypeName box2
                    ]

methodIncorrectArgCount :: Types.Function -> [ETypes.ValueBox] -> T.Text
methodIncorrectArgCount f vboxes =
  T.intercalate " " [ "Expected"
                    , Types.functionName f
                    , "to be called with"
                    ,  showLength $ Types.functionArgs f
                    , "arguments, but called with"
                    , showLength vboxes
                    , "arguments"
                    ]
  where showLength = T.pack . show . length

methodIncorrectArgType :: Types.Function -> Types.Argument -> ETypes.ValueBox -> T.Text
methodIncorrectArgType f arg vbox =
  T.intercalate " " [ "Method"
                    , Types.functionName f
                    , "argument"
                    , Types.argName arg
                    , "has type"
                    , typeName $ Types.argType arg
                    , "but got type"
                    , boxTypeName vbox
                    ]

noSuchMethod :: Types.Identifier -> T.Text
noSuchMethod = ("No such method" `T.append`)

methodIncorrectReturn :: Types.Function -> ETypes.ValueBox -> T.Text
methodIncorrectReturn f vbox =
  let returnType = case Types.functionReturnType f of
        Just typ -> "has return type " `T.append` typeName typ
        Nothing -> "does not return a value" in
      T.intercalate " " [ "Method"
                        , Types.functionName f
                        , returnType
                        , "but tried to return"
                        , boxTypeName vbox
                        ]

arrayTypeError :: Types.Variable -> Types.Type -> Int -> ETypes.ValueBox -> T.Text
arrayTypeError var typ i box =
  T.concat [ showVariable var
           , " is an array of "
           , typeName typ
           , "s, but element "
           , T.pack . show $ i
           , " is a "
           , boxTypeName box
           ]

invalidIndex :: Types.Variable -> Int -> T.Text
invalidIndex var i =
  T.intercalate " " [ "There is no element"
                    , T.pack . show $ i
                    , "of array"
                    , showVariable var
                    ]

cantIndex :: Types.Variable -> ETypes.ValueBox -> T.Text
cantIndex var box =
  T.intercalate " " ["Cannot index"
                    , showVariable var
                    , "of type"
                    , boxTypeName box
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
  Types.TArray typ' -> T.concat ["array of type ", typeName typ', "s"]
