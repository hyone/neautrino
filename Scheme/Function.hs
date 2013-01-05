module Scheme.Function where

import Scheme.Error
import {-# SOURCE #-} Scheme.Eval (apply)
import Scheme.Function.Helper
import Scheme.Function.Equal (eqvP, equalP)
import Scheme.Parser (readExpr, readExprList)
import Scheme.Type

import Control.Monad
import Control.Monad.Error (catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Complex (imagPart, realPart)
import Data.Ratio (denominator, numerator)
import System.IO
import qualified Data.List as List


primitives :: [(String, PrimitiveFunc)]
primitives = [("+",   numberBinFunc (+)),
              ("-",   numberBinFunc (-)),
              ("*",   numberBinFunc (*)),
              ("/",   numberBinFunc div),
              ("mod", numberBinFunc mod),
              ("remainder", numberBinFunc rem),
              ("=",   numberBoolFunc (==)),
              ("<",   numberBoolFunc (<)),
              (">",   numberBoolFunc (>)),
              ("/=",  numberBoolFunc (/=)),
              (">=",  numberBoolFunc (>=)),
              ("<=",  numberBoolFunc (<=)),
              ("&&", boolBoolFunc (&&)),
              ("||", boolBoolFunc (||)),
              ("string=?",  stringBoolFunc (==)),
              ("string<?",  stringBoolFunc (<)),
              ("string>?",  stringBoolFunc (>)),
              ("string<=?", stringBoolFunc (<=)),
              ("string>=?", stringBoolFunc (>=)),
              ("symbol?",   function1 unpackAny (return . Bool) isSymbol),
              ("boolean?",  function1 unpackAny (return . Bool) isBoolean),
              ("string?",   function1 unpackAny (return . Bool) isString),
              ("list?",     function1 unpackAny (return . Bool) isList),
              ("number?",   function1 unpackAny (return . Bool) isNumber),
              ("complex?",  function1 unpackAny (return . Bool) isComplex),
              ("real?",     function1 unpackAny (return . Bool) isReal),
              ("rational?", function1 unpackAny (return . Bool) isRational),
              ("integer?",  function1 unpackAny (return . Bool) isInteger),
              ("eq?",    eqvP),
              ("eqv?",   eqvP),
              ("equal?", equalP),
              ("car",  car),
              ("cdr",  cdr),
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
isNumber (Integer _) = True
isNumber (Float _)   = True
isNumber (Ratio _)   = True
isNumber (Complex _) = True
isNumber _           = False

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

isString :: LispVal -> Bool
isString (String _)  = True
isString _           = False

isList :: LispVal -> Bool
isList (List _)  = True
isList _         = False


-- List ------------------------------------------------------------------

-- | car a list
car :: PrimitiveFunc
car [List (x:_)]   = return x
car [Pair (x:_) _] = return x
car [badArg]       = throwError $ TypeMismatchError "pair" badArg
car badArgList     = throwError $ NumArgsError 1 badArgList

-- | cdr a list
cdr :: PrimitiveFunc
cdr [List (_:xs)]   = return $ List xs
cdr [Pair [_] x]    = return x
cdr [Pair (_:xs) x] = return $ Pair xs x
cdr [badArg]        = throwError $ TypeMismatchError "pair" badArg
cdr badArgList      = throwError $ NumArgsError 1 badArgList

-- | cons a list
cons :: PrimitiveFunc
cons [x, List xs]       = return $ List (x:xs)
cons [x, Pair xs xlast] = return $ Pair (x:xs) xlast
cons [x, y]             = return $ Pair [x] y
cons badArgList         = throwError $ NumArgsError 2 badArgList


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


readParse :: String -> IOThrowsError [LispVal]
readParse path = liftIO (readFile path)
                    >>= liftThrowsError . readExprList

readAll :: IOFunc
readAll [String filename] = liftM List $ readParse filename
readAll badArgList        = throwError $ NumArgsError 1 badArgList
