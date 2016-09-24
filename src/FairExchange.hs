{-# LANGUAGE OverloadedStrings #-}
module FairExchange
    ( fairExchange
    ) where

import Control.Concurrent.Chan (Chan)
import Discover (Location)
import PeerToPeer (Msg)

fairExchange :: Chan Msg -> Location -> IO ()
fairExchange _ _ = putStrLn "fairExchange called"
