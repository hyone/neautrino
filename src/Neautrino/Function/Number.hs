{-# LANGUAGE RankNTypes #-}
module Neautrino.Function.Number
  ( isComplex
  , isReal
  , isRational
  , isInteger
  , exactToInexact
  , inexactToExact
  , numberToString
  , numberAdd
  , numberSub
  , numberMul
  , numberDiv
  ) where

import Neautrino.Type
import Neautrino.Error

import Control.Monad (foldM)
import Data.Complex (imagPart, realPart)
import Data.Ratio ((%), denominator, numerator)


-- Type Check ------------------------------------------------------------

isComplex :: LispVal -> Bool
isComplex = isNumber

isReal :: LispVal -> Bool
isReal (Integer _) = True
isReal (Float _)   = True
isReal (Ratio _)   = True
isReal (Complex n) = imagPart n == 0
isReal _           = False
                     
isRational :: LispVal -> Bool
isRational = isReal

isIntOfDouble :: Double -> Bool
isIntOfDouble d = realToFrac (round d :: Integer) == d

isInteger :: LispVal -> Bool
isInteger (Integer _) = True
isInteger (Float n)   = isIntOfDouble n
isInteger (Ratio n)   = numerator n `mod` denominator n == 0
isInteger (Complex n) = let r = realPart n in
                        imagPart n == 0 && isIntOfDouble r
isInteger _           = False


-- Type Conversion -------------------------------------------------------

exactToInexact :: Procedure
exactToInexact [Integer n]     = return $ Float (fromInteger n)
exactToInexact [Ratio n]       = return $ Float (fromRational n)
exactToInexact [n@(Float _)]   = return n
exactToInexact [n@(Complex _)] = return n
exactToInexact [arg]           = throwError $ TypeMismatchError "number" arg
exactToInexact badArgList      = throwError $ NumArgsError 1 badArgList


inexactToExact :: Procedure
inexactToExact [n@(Integer _)] = return n
inexactToExact [n@(Ratio _)]   = return n
inexactToExact [Float i]       = return . Integer $ round i
inexactToExact [n@(Complex _)] = throwError . DefaultError $ "exact complex is not supported: " ++ show n
inexactToExact [arg]           = throwError $ TypeMismatchError "number" arg
inexactToExact badArgList      = throwError $ NumArgsError 1 badArgList

numberToString :: Procedure
numberToString [x] | isNumber x = return $ String (show x)
                   | otherwise  = throwError $ TypeMismatchError "number" x
numberToString badArgList = throwError $ NumArgsError 1 badArgList


-- Arithmetic ------------------------------------------------------------

numberOp :: (forall a. Num a => (a -> a -> a)) -> ErrorM LispVal -> Procedure
numberOp _  ident []         = ident
numberOp op _     (arg:args) = foldM numberOp' arg args
  where
    numberOp' (Integer x) (Integer y) = return $ Integer (x `op` y)
    numberOp' (Float x)   (Float y)   = return $ Float   (x `op` y)
    numberOp' (Ratio x)   (Ratio y)   = return $ Ratio   (x `op` y)
    numberOp' (Complex x) (Complex y) = return $ Complex (x `op` y)
    numberOp' x y = throwError . DefaultError $
                       "operation is not defined between " ++ show x ++ " and " ++ show y

numberAdd :: Procedure
numberAdd = numberOp (+) (return $ Integer 0)

numberSub :: Procedure
numberSub = numberOp (-) (throwError $ NumArgsError 1 [])

numberMul :: Procedure
numberMul = numberOp (*) (return $ Integer 1)

numberDiv :: Procedure
numberDiv []         = throwError $ NumArgsError 1 []
numberDiv (arg:args) = foldM numberDiv' arg args
  where
    divisionByZeroError :: ErrorM a
    divisionByZeroError = throwError $ DefaultError "division by zero"
    numberDiv' :: LispVal -> LispVal -> ErrorM LispVal
    numberDiv' (Integer x) (Integer y) = if y /= 0   then return (Ratio (x % y))   else divisionByZeroError
    numberDiv' (Float x)   (Float y)   = if y /= 0.0 then return (Float (x / y))   else divisionByZeroError
    numberDiv' (Ratio x)   (Ratio y)   = if y /= 0   then return (Ratio (x / y))   else divisionByZeroError
    numberDiv' (Complex x) (Complex y) = if y /= 0   then return (Complex (x / y)) else divisionByZeroError
    numberDiv' x y = throwError . DefaultError $
                       "operation is not defined between " ++ show x ++ " and " ++ show y
