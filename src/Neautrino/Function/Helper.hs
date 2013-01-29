module Neautrino.Function.Helper where

import Neautrino.Type
import Neautrino.Error


type Unpacker a = LispVal -> ErrorM a
type Packer   a = a -> ErrorM LispVal


-- ----------------------------------------------------------------------------------------
-- Lisp Function Builder

function1 :: Unpacker a -> Packer b -> (a -> b) -> Procedure
function1 unpacker packer f args = if length args /= 1
                                   then throwError $ NumArgsError 1 args
                                   else do val <- unpacker (head args)
                                           packer $ f val
                                      
function2 :: Unpacker a -> Packer b -> (a -> a -> b) -> Procedure
function2 unpacker packer f args = if length args /= 2
                                   then throwError $ NumArgsError 2 args
                                   else do vals <- mapM unpacker args
                                           packer $ f (head vals) (vals !! 1)

function3 :: Unpacker a -> Packer b -> (a -> a -> a -> b) -> Procedure
function3 unpacker packer f args = if length args /= 3
                                   then throwError $ NumArgsError 3 args
                                   else do vals <- mapM unpacker args
                                           packer $ f (head vals) (vals !! 1) (vals !! 2)

functionFold :: Unpacker a -> Packer a -> (a -> a -> a) -> Procedure
functionFold _        _      _  param@[_] = throwError $ NumArgsError 2 param
functionFold unpacker packer op params = do
  vals <- mapM unpacker params
  packer $ foldl1 op vals


numberBinFunc :: (Integer -> Integer -> Integer) -> Procedure
numberBinFunc = functionFold unpackNumber (return . Integer)

boolBinFunc :: (LispVal -> ErrorM a) -> (a -> a -> Bool) -> Procedure
boolBinFunc = function2 `flip` (return . Bool)

numberBoolFunc :: (Integer -> Integer -> Bool) -> Procedure
numberBoolFunc  = boolBinFunc unpackNumber

stringBoolFunc :: (String -> String -> Bool) -> Procedure
stringBoolFunc  = boolBinFunc unpackString

boolBoolFunc :: (Bool -> Bool -> Bool) -> Procedure
boolBoolFunc    = boolBinFunc unpackBool


-- ----------------------------------------------------------------------------------------
-- Unpacker

unpackAny :: LispVal -> ErrorM LispVal
unpackAny = return

unpackNumber :: LispVal -> ErrorM Integer
unpackNumber (Integer n) = return n
unpackNumber notNum = throwError $ TypeMismatchError "number" notNum

unpackString :: LispVal -> ErrorM String
unpackString (String a) = return a
unpackString notString = throwError $ TypeMismatchError "string" notString

unpackBool :: LispVal -> ErrorM Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatchError "boolean" notBool
