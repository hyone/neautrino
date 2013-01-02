module Scheme.Function.Helper where

import Scheme.Type
import Scheme.Error


type Unpacker a = LispVal -> ThrowsError a
type Packer   a = a -> ThrowsError LispVal


-- ----------------------------------------------------------------------------------------
-- Lisp Function Builder

function1 :: Unpacker a -> Packer b -> (a -> b) -> PrimitiveFunc
function1 unpacker packer f args = if length args /= 1
                                   then throwError $ NumArgsError 1 args
                                   else do val <- unpacker (head args)
                                           packer $ f val
                                      
function2 :: Unpacker a -> Packer b -> (a -> a -> b) -> PrimitiveFunc
function2 unpacker packer f args = if length args /= 2
                                   then throwError $ NumArgsError 2 args
                                   else do vals <- mapM unpacker args
                                           packer $ f (head vals) (vals !! 1)

function3 :: Unpacker a -> Packer b -> (a -> a -> a -> b) -> PrimitiveFunc
function3 unpacker packer f args = if length args /= 3
                                   then throwError $ NumArgsError 3 args
                                   else do vals <- mapM unpacker args
                                           packer $ f (head vals) (vals !! 1) (vals !! 2)

functionFold :: Unpacker a -> Packer a -> (a -> a -> a) -> PrimitiveFunc
functionFold _        _      _  param@[_] = throwError $ NumArgsError 2 param
functionFold unpacker packer op params = do
  vals <- mapM unpacker params
  packer $ foldl1 op vals


numberBinFunc :: (Integer -> Integer -> Integer) -> PrimitiveFunc
numberBinFunc = functionFold unpackNumber (return . Number)

boolBinFunc :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> PrimitiveFunc
boolBinFunc = function2 `flip` (return . Bool)

numberBoolFunc :: (Integer -> Integer -> Bool) -> PrimitiveFunc
numberBoolFunc  = boolBinFunc unpackNumber

stringBoolFunc :: (String -> String -> Bool) -> PrimitiveFunc
stringBoolFunc  = boolBinFunc unpackString

boolBoolFunc :: (Bool -> Bool -> Bool) -> PrimitiveFunc
boolBoolFunc    = boolBinFunc unpackBool


-- ----------------------------------------------------------------------------------------
-- Unpacker

unpackAny :: LispVal -> ThrowsError LispVal
unpackAny = return

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber notNum = throwError $ TypeMismatchError "number" notNum

unpackString :: LispVal -> ThrowsError String
unpackString (String a) = return a
unpackString notString = throwError $ TypeMismatchError "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatchError "boolean" notBool
