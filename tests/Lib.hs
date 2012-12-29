module Lib where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, Test)
import Test.HUnit (Assertion)
import Text.Printf (printf)

paste :: QuasiQuoter
paste = QuasiQuoter {
  quoteExp = stringE,
  quotePat = undefined,
  quoteType = undefined,
  quoteDec = undefined
  }

enumTestGroup :: String -> [Assertion] -> Test
enumTestGroup name =
  testGroup name . zipWith (testCase . printf "%.2d") [(1 :: Int)..]
