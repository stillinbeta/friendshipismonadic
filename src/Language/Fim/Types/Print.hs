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
              , T.concat $ map prettyPrint (functionBody f)
              , "That's all about ", idName $ functionName f, "!\n"
              ]

instance PrettyPrintable Statement where
  prettyPrint o@Output{} =
    let verb = case outputVerb o of
          Said -> "said"
          Sang -> "sang"
          Thought -> "thought"
          Wrote -> "wrote" in
      T.concat ["I ", verb, " ", prettyPrint $ outputValue o
               , prettyPrint $ outputTerminator o, "\n"]

instance PrettyPrintable Value where
  prettyPrint (VLiteral l) = prettyPrint l

instance PrettyPrintable Literal where
  prettyPrint s@StringLiteral{} =
    let (open, close) = case slWrap s of
          FancyQuote -> ("“", "”")
          SimpleQuote -> ("\"", "\"") in
      T.concat [open, slValue s, close]
  prettyPrint n@NumberLiteral{} = T.pack . show . nlValue $ n
  prettyPrint c@CharacterLiteral{} =
    let (open, close) = case clWrap c of
          FancyQuote -> ('‘', '’' )
          SimpleQuote -> ('\'', '\'') in
      T.pack [open, clValue c, close]
  prettyPrint Null = "null"
