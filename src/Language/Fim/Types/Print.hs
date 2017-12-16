{-# LANGUAGE OverloadedStrings #-}

module Language.Fim.Types.Print (PrettyPrintable(..)) where


import Language.Fim.Types

import Data.Maybe (isJust)
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
  prettyPrint d@Declaration{} =
      -- is this cheating?
      let verb = T.toLower . T.pack . show $ declareVerb d
          always = if declareIsConsnant d then " always" else "" in
        T.concat ["Did you know that ", prettyPrint $ declareName d
                 , " ", verb, always, " "
                 , printType (declareTypeArticle d) (declareTypeNoun d), " "
                 , prettyPrint $ declareValue d, "?\n"]
printType :: Maybe Article -> TypeNoun -> T.Text
printType maybeArticle noun =
  let pluralised = case noun of
                     Logic -> "logic"
                     _ -> T.concat [case noun of
                                      Letter -> "letter"
                                      Character -> "character"
                                      Word -> "word"
                                      Phrase -> "phrase"
                                      Sentence -> "sentence"
                                      Quote -> "quote"
                                      Name -> "name"
                                      Argument -> "argument"
                                      Number -> "number"
                                   , if isJust maybeArticle
                                     then "" else "s"
                                   ]
      article = maybe "" ((`T.snoc` ' ') . prettyPrint) maybeArticle in
    T.concat [article, pluralised]

instance PrettyPrintable Article where
  prettyPrint The = "the"
  prettyPrint A   = "a"
  prettyPrint An  = "an"







instance PrettyPrintable Value where
  prettyPrint v@VLiteral{} = prettyPrint $ vLiteral v
  prettyPrint v@VVariable{} = prettyPrint $ vVariable v

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

instance PrettyPrintable Variable where
  prettyPrint = vName
