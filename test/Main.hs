module Main where

import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Debug.Trace

import Data.String
import System.Path

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Round-trip relative path" $
      fromString (renderPath relativeDir) == relativeDir @?= True
  , testCase "Round-trip absolute path" $
      fromString (renderPath absoluteDir) == absoluteDir @?= True

  , testCase "Consequent or final slashes don't matter" $
      fromString "//aa/bb////cc//" == DA ["aa", "bb", "cc"] @?= True

  -- fix: How to test exceptions?
  -- , testCase "Getting relative path by fromString on slash starting path will explode" $
  --     fromString "/aa" == DR ["aa"] @?= True

  ]
  where
    relativeDir = DR ["a", "b", "c"] :: Dir Relative
    absoluteDir = DA ["a", "b", "c"] :: Dir Absolute
