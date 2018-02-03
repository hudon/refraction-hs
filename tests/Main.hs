module Main where

import Network.Haskoin.Constants
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec

main :: IO ()
main = do
  setProdnet
  hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
