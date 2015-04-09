{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.ByteString.Parse as BP
import Test.Tasty
import Test.Tasty.HUnit

assertEq :: (Eq a, Show a) => Result a -> Result a -> Assertion
assertEq x y =
    case (x, y) of
        (ParseFail xErr, ParseFail yErr) | xErr /= yErr -> assertFailure $ "ParseFail but different errors: " ++ xErr ++ " /= " ++ yErr
                                         | otherwise    -> return ()
        (ParseMore _   , ParseMore _ )                  -> return () -- Can't give a better test for now
        (ParseOK xbs xa, ParseOK ybs ya) | xbs /= ybs   -> assertFailure $ "ParseOK but different remains: " ++ (show xbs) ++ " /= " ++ (show ybs)
                                         | xa /= ya     -> assertFailure $ "ParseOK but different value: " ++ (show xa) ++ " /= " ++ (show ya)
                                         | otherwise    -> return ()
        _ -> assertFailure "Result type different"

testParserOk :: (Show a, Eq a)
             => String
             -> (Parser a, ByteString, Result a)
             -> TestTree
testParserOk msg (parser, bs, expected) =
    testCase msg (assertEq (parse parser bs) expected)

refTestsOk = testGroup "tests that must always pass"
    [ testParserOk "anybyte" $ (anyByte, "B", ParseOK "" 0x42)
    , testParserOk "byte (wrong)" $ (byte 0x41, "BA", ParseFail "byte 65 : failed")
    , testParserOk "byte + remain" $ (byte 0x42, "BA", ParseOK "A" ())
    , testParserOk "bytes" $ (bytes "haskell", "haskell rocks", ParseOK " rocks" ())
    , testParserOk "anyByte >> bytes" $ (anyByte *> bytes "askell", "haskell rocks", ParseOK " rocks" ())
    , testParserOk "take" $ (BP.take 7, "haskell rocks", ParseOK " rocks" "haskell")
    , testParserOk "EOB"  $ (isEndOfBuffer >>= \b -> if b then return 0x42 else anyByte, "A", ParseOK "" 0x41)
    , testParserOk "EOB"  $ (isEndOfBuffer >>= return . show, "", ParseOK "" "True")
    ]

tests = testGroup "bsparse: Unit test"
    [ refTestsOk
    ]

main = defaultMain tests
