module Fim (run) where

import Control.Monad.Trans.Writer.Lazy

run :: String -> Writer [String] ()
run _ = return ()
