module Fim.Types ( Class(..)
                 , Function(..)
                 , Statement(..)
                 , Value(..)
                 , Literal(..)
                 ) where

data Class = Class { className :: String
                   , classSuper :: Class
                   , classBody :: [Function]
                   }
           | Celestia
           | ClassByName String
           deriving (Eq, Show)

data Function = Function { functionName :: String
                         , isMain :: Bool
                         , functionBody :: [Statement]
                         } deriving (Eq, Show)

data Statement = SISaid Value deriving (Eq, Show)

data Value = VLiteral Literal deriving (Eq, Show)

data Literal = StringLiteral String | Null deriving (Eq, Show)
