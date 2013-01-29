{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Neautrino.HspecHelper (
  shouldContain    
, shouldEitherT
, shouldBeT
, shouldReturnT
, shouldErrorT
, shouldErrorReturnT
, assertNoErrorT
, assertNoIOErrorT
) where

import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Data.List (isInfixOf)

import Neautrino.Type
import Neautrino.Error (LispError(..), ErrorM)
import Control.Monad (liftM, liftM2)
import Data.Array (Array, listArray)


-- Arbitrary Class for LispVal

instance Arbitrary (Array Int LispVal) where
  arbitrary = do
    xs <- arbitrary :: Gen [LispVal]
    return $ listArray (0, length xs - 1) xs

instance Arbitrary LispVal where
  arbitrary = oneof [
      liftM Bool arbitrary
    , liftM Character arbitrary
    , liftM String arbitrary
    , liftM Integer arbitrary
    , liftM Float arbitrary
    , liftM Ratio arbitrary
    , liftM Complex arbitrary
    , liftM List arbitrary
    , liftM2 Pair arbitrary arbitrary
    , liftM Vector arbitrary
    , return Undefined
    ]
    

-- Newtype for LispError to be comparable each otehr

newtype ErrorType = ErrorType { getErrorHolder ::  LispError }

instance Eq ErrorType where
  (ErrorType (NumArgsError x _))        == (ErrorType (NumArgsError y _))        = x == y
  (ErrorType (TypeMismatchError x _))   == (ErrorType (TypeMismatchError y _))   = x == y
  (ErrorType (ParserError _))           == (ErrorType (ParserError _))           = True
  (ErrorType (SyntaxError x _))         == (ErrorType (SyntaxError y _))         = x == y
  (ErrorType (BadSpecialFormError x _)) == (ErrorType (BadSpecialFormError y _)) = x == y
  (ErrorType (NotFunctionError x _))    == (ErrorType (NotFunctionError y _))    = x == y
  (ErrorType (UnboundVarError x _))     == (ErrorType (UnboundVarError y _))     = x == y
  (ErrorType (DefaultError _))          == (ErrorType (DefaultError _))          = True
  _                                     == _                                     = False

instance Show ErrorType where
  show (ErrorType (NumArgsError x _))        = "NumArgsError " ++ show x
  show (ErrorType (TypeMismatchError x _))   = "TypeMismatchError " ++ x
  show (ErrorType (ParserError _))           = "ParseError"
  show (ErrorType (SyntaxError x _))         = "SyntaxError " ++ x
  show (ErrorType (BadSpecialFormError x _)) = "BadSpecialFormError " ++ x
  show (ErrorType (NotFunctionError x _))    = "NotFunctionError " ++ x
  show (ErrorType (UnboundVarError x _))     = "UnboundVarError " ++ x
  show (ErrorType (DefaultError _))          = "DefaultError"


message :: String -> String -> String
message expected got = "expected: " ++ expected ++ "\n but got: " ++ got

shouldContain :: String -> String -> Expectation
shouldContain got expected = assertBool (message (show expected) (show got))
                                        (expected `isInfixOf` got)

shouldEitherT :: (Eq a, Show a) => ErrorM a -> ErrorM a -> Expectation
shouldEitherT (Right x)  (Right expected) = x `shouldBe` expected
shouldEitherT (Left err) (Left expected)  = ErrorType err `shouldBe` ErrorType expected
shouldEitherT err        expected         = assertFailure $ message (show expected) (show err)

shouldBeT :: (Eq a, Show a) => ErrorM a -> a -> Expectation
shouldBeT got@(Right _) expected = got `shouldEitherT` Right expected
shouldBeT err           expected = assertFailure $ message (show expected) (show err)

shouldReturnT :: (Eq a, Show a) => IO (ErrorM a) -> a -> Expectation
shouldReturnT action expected = action >>= (`shouldBeT` expected)
  
shouldErrorT :: (Eq a, Show a) => ErrorM a -> LispError -> Expectation
shouldErrorT got@(Left _) expected = got `shouldEitherT` Left expected
shouldErrorT x            expected = assertFailure $ message (show $ ErrorType expected) (show x)

shouldErrorReturnT :: (Eq a, Show a) => IO (ErrorM a) -> LispError -> Expectation
shouldErrorReturnT action expected = action >>= (`shouldErrorT` expected)

assertNoErrorT :: (Eq a, Show a) => ErrorM a -> Expectation
assertNoErrorT (Right _) = return ()
assertNoErrorT x         = assertFailure $ "raise error: " ++ show x

assertNoIOErrorT :: (Eq a, Show a) => IO (ErrorM a) -> Expectation
assertNoIOErrorT action = action >>= assertNoErrorT
