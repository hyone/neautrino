{-# LANGUAGE ExistentialQuantification #-}
module Scheme.Function.Equal (
  eqvP
, equalP
) where

import Scheme.Function.Helper
import Scheme.Type (LispVal(..), PrimitiveFunc)
import Scheme.Error (LispError(..), ThrowsError, catchError, throwError)

import Control.Monad (liftM)
import Data.Array (elems)


equalSeq :: LispVal -> LispVal -> PrimitiveFunc -> ThrowsError LispVal
equalSeq (Pair xs x) (Pair ys y) eq = eq [List $ xs ++ [x], List $ ys ++ [y]]
equalSeq (Vector xs) (Vector ys) eq = eq [List (elems xs), List (elems ys)]
equalSeq (List xs)   (List ys)   eq = return $ Bool $
    (length xs == length ys) && all eqTuple (zip xs ys)
  where
    eqTuple :: (LispVal, LispVal) -> Bool
    eqTuple (x, y) = case eq [x, y] of
      Right (Bool val) -> val
      _                -> False
equalSeq _            _          _  = return (Bool False)

-- |
-- equality of same type
-- 
-- >>> eqvP [Integer 22, Integer 22]
-- Right #t
eqvP :: PrimitiveFunc
eqvP [Bool      x, Bool      y] = return $ Bool (x == y)
eqvP [Integer   x, Integer   y] = return $ Bool (x == y)
eqvP [Float     x, Float     y] = return $ Bool (x == y)
eqvP [Ratio     x, Ratio     y] = return $ Bool (x == y)
eqvP [Complex   x, Complex   y] = return $ Bool (x == y)
eqvP [Character x, Character y] = return $ Bool (x == y)
eqvP [Atom      x, Atom      y] = return $ Bool (x == y)
eqvP [_, _]                     = return (Bool False)
eqvP badArgList = throwError $ NumArgsError 2 badArgList


data AnyUnpacker = forall a. Eq a => AnyUnpacker (Unpacker a)

unpackEquals :: LispVal -> LispVal -> AnyUnpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
   `catchError` const (return False)
  
-- |
-- equality between both same and diffent types
-- 
-- >>> equalP [Integer 22, Integer 22]
-- Right #t
equalP :: PrimitiveFunc
equalP [xs@(Pair _ _), ys@(Pair _ _)] = equalSeq xs ys equalP
equalP [xs@(Vector _), ys@(Vector _)] = equalSeq xs ys equalP
equalP [xs@(List _),   ys@(List _)]   = equalSeq xs ys equalP
equalP [x, y] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals x y)
                     [ AnyUnpacker unpackNumber
                     , AnyUnpacker unpackString
                     , AnyUnpacker unpackBool ]
  Bool eqvEqual   <- eqvP [x, y]
  return $ Bool (primitiveEquals || eqvEqual)
equalP badArgList = throwError $ NumArgsError 2 badArgList
