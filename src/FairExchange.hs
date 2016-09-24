{-# LANGUAGE OverloadedStrings #-}
module FairExchange
    ( fairExchange
    ) where

import Control.Concurrent.Chan (Chan)
import Discover (Location)
import PeerToPeer (Msg)

fairExchange :: Chan Msg -> Location -> Location -> IO ()
fairExchange _ _ _ = putStrLn "fairExchange called"
