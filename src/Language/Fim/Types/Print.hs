{-# LANGUAGE OverloadedStrings #-}

module Language.Fim.Types.Print (PrettyPrintable(..)) where

import Language.Fim.Types

import qualified Data.Text as T

class PrettyPrintable p where
  prettyPrint :: p -> T.Text


instance PrettyPrintable p => PrettyPrintable [p] where
  prettyPrint = T.concat . map prettyPrint

instance PrettyPrintable Class where
  prettyPrint cl =
    T.concat ["Dear ", ppClassName $ classSuper cl, ": ", prettyPrint $ className cl, "\n"
             , prettyPrint $ classBody cl
             , "Your faithful student, ", prettyPrint $ classStudent cl, "\n"]

ppClassName :: Class -> T.Text
ppClassName Celestia = "Princess Celestia"
ppClassName (ClassByName str) = str
ppClassName c = idName $ className c

instance PrettyPrintable Identifier where
  prettyPrint idt = T.concat [ idName idt, prettyPrint $ idTerminator idt]

instance PrettyPrintable Terminator where
  prettyPrint FullStop     = "."
  prettyPrint Comma        = ","
  prettyPrint Exclamation  = "!"
  prettyPrint QuestionMark = "?"

instance PrettyPrintable Function where
  prettyPrint f =
    let today = if isMain f then "Today " else "" in
      T.concat[ today, "I learned " , prettyPrint $ functionName f, "\n"
              -- body
              , "That's all about ", idName $ functionName f, "!\n"
              ]
