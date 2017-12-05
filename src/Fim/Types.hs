module Fim.Types (Class (..), Function (..)) where

data Class = Class { className :: String
                   , classSuper :: Class
                   , classBody :: [Function]
                   }
           | Celestia
           | ClassByName String
           deriving (Eq, Show)

data Function = Function { functionName :: String} deriving (Eq, Show)
