module Language.Fim.Types ( Class(..)
                          , Function(..)
                          , Statement(..)
                          , Value(..)
                          , BinaryOperator(..)
                          , UnaryOperator(..)
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

data Type = TNumber | TString | TCharacter | TBoolean deriving (Eq, Show)

data Statement = Output { outputValue :: Value
                        }
               | Input { inputName :: Variable
                       , inputType :: Maybe Type
                       }
               | Prompt { promptName :: Variable
                        , promptVal  :: Value
                        }
               | Declaration { declareName :: Variable
                             , declareVal :: Maybe Value
                             , declareIsConstant :: Bool
                             , declareType :: Maybe Type
                             }
               | Assignment { assignmentName :: Variable
                            , assignmentExpr :: Value
                            }
               | IfThenElse { ifOnVal :: Value
                            , ifThen :: [Statement]
                            , ifElse :: [Statement]
                            }
               | While { whileVal :: Value
                       , whileBody :: [Statement]
                       }
               | DoWhile { doWhileBody :: [Statement]
                         , doWhileVal :: Value
                         }
               | For { forVar  :: Variable
                     , forType :: Type
                     , forFrom :: Value
                     , forTo   :: Value
                     , forBody :: [Statement]
                     }
               deriving (Eq, Show)

data Value = VLiteral { vLiteral :: Literal}
           | VVariable { vVariable :: Variable}
           | VBinaryOperation { vBinArg1 :: Value
                              , vBinOpr  :: BinaryOperator
                              , vBinArg2 :: Value
                              }
           | VUnaryOperation { vUnArg :: Value
                            , vUnOpr :: UnaryOperator
                            }
           deriving (Eq, Show)

data BinaryOperator = And -- And can be "Add" or "&&"
                    | Add | Subtract | Multiply | Divide
                    | EqualTo | NotEqualTo
                    | LessThan    | LessThanOrEqual
                    | GreaterThan | GreaterThanOrEqual
                    -- Add doubles as boolean and
                    | Or | Xor
                    deriving (Eq, Show)

data UnaryOperator = Not
                   deriving (Eq, Show)

newtype Variable = Variable { vName :: T.Text
                            } deriving (Eq, Show)

data Literal = StringLiteral    { slValue :: T.Text }
             | NumberLiteral    { nlValue :: Double } -- TODO: Supposed to be Float64
             | CharacterLiteral { clValue :: Char   }
             | BooleanLiteral   { blValue :: Bool   }
             | NullLiteral

             deriving Show

-- Manually define Eq so our tests don't barf on the doubles
instance Eq Literal where
  (StringLiteral val1)    == (StringLiteral val2)    = val1 == val2
  (CharacterLiteral val1) == (CharacterLiteral val2) = val1 == val2
  (NumberLiteral n1)      == (NumberLiteral n2)      = abs (n1 - n2) < 0.00000001
  (BooleanLiteral b1)     == (BooleanLiteral b2)     = b1 == b2
  NullLiteral             == NullLiteral             = True
  _ == _ = False


data StringQuote = SimpleQuote | FancyQuote deriving (Eq, Show)

data Identifier = Identifier { idName :: T.Text
                             } deriving (Eq, Show)
