----------------------------------------------------------------------------------------
-- IMPORTS
----------------------------------------------------------------------------------------

module Main
  ( main
  ) where

import qualified Spec.Model
import qualified Spec.ModelWithClose
import qualified Spec.Trace
import qualified Spec.TraceWithClose
import           Test.Tasty

----------------------------------------------------------------------------------------
-- MAIN
----------------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

----------------------------------------------------------------------------------------
-- TESTS
----------------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup
  "token sale"
  [ Spec.Trace.tests
  , Spec.TraceWithClose.tests
  , Spec.Model.tests
  , Spec.ModelWithClose.tests
  ]
