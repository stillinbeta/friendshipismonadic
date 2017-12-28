module Language.Fim.Types ( Class(..)
                          , Function(..)
                          , Statement(..)
                          , Expression(..)
                          , BinaryOperator(..)
                          , Literal(..)
                          , StringQuote(..)
                          , Identifier(..)
                          , Variable(..)
                          , Type(..)
                          ) where

import Data.Text as T

data Class = Class { className :: Identifier
                   , classSuper :: Class
                   , classBody :: [Function]
                   }
           | Celestia
           | ClassByName T.Text
           deriving (Eq, Show)


data Function = Function { functionName :: Identifier
                         , isMain :: Bool
                         , functionBody :: [Statement]
                         } deriving (Eq, Show)


data Type = TNumber | TString | TCharacter deriving (Eq, Show)

data Statement = Output { outputExpr :: Expression
                        }
               | Declaration { declareName :: Variable
                             , declareExpr :: Maybe Expression
                             , declareIsConsnant :: Bool
                             , declareType :: Maybe Type
                             }
               | Assignment { assignmentName :: Variable
                            , assignmentExpr :: Expression
                            }
               deriving (Eq, Show)

data Expression = ELiteral { eLiteral :: Literal}
                | EVariable { eVariable :: Variable}
                | EBinaryOperator { eBinArg1 :: Expression
                                  , eBinArg2 :: Expression
                                  , eBinOp :: BinaryOperator
                                  }
  deriving (Eq, Show)

data BinaryOperator = Add deriving (Eq, Show)


newtype Variable = Variable { vName :: T.Text
                            } deriving (Eq, Show)


data Literal = StringLiteral { slValue :: T.Text}
             | NumberLiteral { nlValue :: Double} -- TODO: Supposed to be Float64
             | CharacterLiteral { clValue :: Char
                                }
             deriving Show

-- Manually define Eq so our tests don't barf on the doubles
instance Eq Literal where
  (StringLiteral val1) == (StringLiteral val2) = val1 == val2
  (CharacterLiteral val1) == (CharacterLiteral val2) = val1 == val2
  (NumberLiteral n1) == (NumberLiteral n2) = abs (n1 - n2) < 0.00000001
  _ == _ = False


data StringQuote = SimpleQuote | FancyQuote deriving (Eq, Show)

data Identifier = Identifier { idName :: T.Text
                             } deriving (Eq, Show)
