module Neautrino.TestUtil (
  evalAST
, shouldContain    
, shouldEitherT
, shouldBeT
, shouldReturnT
, shouldErrorT
, shouldErrorReturnT
) where

import Test.Hspec
import Test.HUnit
import Data.List (isInfixOf)

import Neautrino.Error (LispError(..), ErrorM)
import Neautrino.Env (Env)
import Neautrino.Type (LispVal, runEvalExprMonad)
import Neautrino.Eval (eval)


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


evalAST :: Env -> LispVal -> IO (ErrorM LispVal)
evalAST env = runEvalExprMonad env . eval


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
