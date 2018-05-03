module BitsSpec
    (
        tests
    ) where

import Test.HUnit

test1 = TestCase (assertEqual "basic test," True False)

tests = TestList [TestLabel "test one" test1]