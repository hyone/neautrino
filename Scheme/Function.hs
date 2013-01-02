{-# LANGUAGE ExistentialQuantification #-}

module Scheme.Function
  ( primitives
  , ioPrimitives
  , apply
  , eqv
  , car
  , cdr
  , cons
  , load
  ) where

import Scheme.Error
import {-# SOURCE #-} Scheme.Eval (apply)
import Scheme.Function.Helper
import Scheme.Parser (readExpr, readExprList)
import Scheme.Type

import Control.Monad
import Control.Monad.Error (catchError)
import Control.Monad.IO.Class (liftIO)
import System.IO
import qualified Data.List as List


primitives :: [(String, PrimitiveFunc)]
primitives = [("+", numberBinFunc (+)),
              ("-", numberBinFunc (-)),
              ("*", numberBinFunc (*)),
              ("/", numberBinFunc div),
              ("mod", numberBinFunc mod),
              ("remainder", numberBinFunc rem),
              ("=", numberBoolFunc (==)),
              ("<", numberBoolFunc (<)),
              (">", numberBoolFunc (>)),
              ("/=", numberBoolFunc (/=)),
              (">=", numberBoolFunc (>=)),
              ("<=", numberBoolFunc (<=)),
              ("&&", boolBoolFunc (&&)),
              ("||", boolBoolFunc (||)),
              ("string=?", stringBoolFunc (==)),
              ("string<?", stringBoolFunc (<)),
              ("string>?", stringBoolFunc (>)),
              ("string<=?", stringBoolFunc (<=)),
              ("string>=?", stringBoolFunc (>=)),
              ("symbol?",  function1 unpackAny (return . Bool) isSymbol),
              ("boolean?", function1 unpackAny (return . Bool) isBoolean),
              ("string?",  function1 unpackAny (return . Bool) isString),
              ("number?",  function1 unpackAny (return . Bool) isNumber),
              ("list?",    function1 unpackAny (return . Bool) isList),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons)
             ]

ioPrimitives :: [(String, IOFunc)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)
               ]


-- Type Check ------------------------------------------------------------

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _        = False

isBoolean :: LispVal -> Bool
isBoolean (Bool _) = True
isBoolean _        = False

isNumber :: LispVal -> Bool
isNumber (Number _)  = True
isNumber (Float _)   = True
isNumber (Ratio _)   = True
isNumber (Complex _) = True
isNumber _           = False

isString :: LispVal -> Bool
isString (String _)  = True
isString _           = False

isList :: LispVal -> Bool
isList (List _)  = True
isList _         = False


-- Equality --------------------------------------------------------------

equalSeq :: LispVal -> LispVal -> PrimitiveFunc -> ThrowsError LispVal
equalSeq (DottedList xs x) (DottedList ys y) eq = eq [List $ xs ++ [x], List $ ys ++ [y]]
equalSeq (List xs)         (List ys)         eq =
    return $ Bool $ (length xs == length ys) && all eqvPair (zip xs ys)
  where
    eqvPair (x, y) = case eq [x, y] of
      Left _           -> False
      Right (Bool val) -> val
      _                -> False
equalSeq _ _ _ = return (Bool False)

eqv :: PrimitiveFunc
eqv [Bool   arg1, Bool   arg2]  = return $ Bool (arg1 == arg2)
eqv [Number arg1, Number arg2]  = return $ Bool (arg1 == arg2)
eqv [String arg1, String arg2]  = return $ Bool (arg1 == arg2)
eqv [Atom   arg1, Atom   arg2]  = return $ Bool (arg1 == arg2)
eqv [xs@(DottedList {}), ys@(DottedList {})] = equalSeq xs ys eqv
eqv [xs@(List _), ys@(List _)] = equalSeq xs ys eqv
eqv [_, _] = return (Bool False)
eqv badArgList = throwError $ NumArgsError 2 badArgList


data AnyUnpacker = forall a. Eq a => AnyUnpacker (Unpacker a)

unpackEquals :: LispVal -> LispVal -> AnyUnpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
   `catchError` const (return False)
  
equal :: PrimitiveFunc
equal [x@(DottedList {}), y@(DottedList {})] = equalSeq x y equal
equal [x@(List _), y@(List _)]               = equalSeq x y equal
equal [arg1, arg2]                           = do
  primitiveEquals <- liftM List.or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNumber, AnyUnpacker unpackString, AnyUnpacker unpackBool]
  Bool eqvEqual <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || eqvEqual)
equal badArgList = throwError $ NumArgsError 2 badArgList


-- List ------------------------------------------------------------------

-- | car a list
car :: PrimitiveFunc
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [badArg]             = throwError $ TypeMismatchError "pair" badArg
car badArgList           = throwError $ NumArgsError 1 badArgList

-- | cdr a list
cdr :: PrimitiveFunc
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatchError "pair" badArg
cdr badArgList            = throwError $ NumArgsError 1 badArgList

-- | cons a list
cons :: PrimitiveFunc
cons [x, List xs]             = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x, y]                   = return $ DottedList [x] y
cons badArgList               = throwError $ NumArgsError 2 badArgList


-- IO Primitives ---------------------------------------------------------

applyProc :: IOFunc
applyProc [func, List args] = apply func args
applyProc (func:args)       = apply func args
applyProc _                 = throwError $ DefaultError "Apply Error"

makePort :: IOMode -> IOFunc
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _    badArgList        = throwError $ NumArgsError 1 badArgList

closePort :: IOFunc
closePort [Port handle] = liftIO (hClose handle) >> return (Bool True)
closePort _             = return (Bool False)

readProc :: IOFunc
readProc []            = readProc [Port stdin]
readProc [Port handle] = liftIO (hGetLine handle)
                         >>= liftThrowsError . readExpr
readProc badArgList    = throwError $ NumArgsError 1 badArgList

writeProc :: IOFunc
writeProc [obj]              = writeProc [obj, Port stdout]
writeProc [obj, Port handle] = liftIO (hPrint handle obj) >> return (Bool True)
writeProc badArgList         = throwError $ NumArgsError 2 badArgList

readContents :: IOFunc
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents badArgList        = throwError $ NumArgsError 1 badArgList

-- | evaluate expressions from a file
load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename)
                >>= liftThrowsError . readExprList

readAll :: IOFunc
readAll [String filename] = liftM List $ load filename
readAll badArgList        = throwError $ NumArgsError 1 badArgList
