module Neautrino.Function where

import Neautrino.Env (equalVar)
import Neautrino.Error
import Neautrino.Eval (apply)
import Neautrino.Function.Helper
import Neautrino.Function.Equal (eqvP, equalP)
import Neautrino.Parser (readExpr, readExprList)
import Neautrino.Type

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Complex (imagPart, realPart)
import Data.Ratio (denominator, numerator)
import System.IO (IOMode(..), stdin, stdout, hPutStr, openFile, hClose, hGetLine)


primitiveFuncs :: [(String, PrimitiveFunc)]
primitiveFuncs =
  [ ("+",   numberBinFunc (+))
  , ("-",   numberBinFunc (-))
  , ("*",   numberBinFunc (*))
  , ("/",   numberBinFunc div)
  , ("mod", numberBinFunc mod)
  , ("remainder", numberBinFunc rem)
  , ("=",   numberBoolFunc (==))
  , ("<",   numberBoolFunc (<))
  , (">",   numberBoolFunc (>))
  , ("/=",  numberBoolFunc (/=))
  , (">=",  numberBoolFunc (>=))
  , ("<=",  numberBoolFunc (<=))
  , ("&&", boolBoolFunc (&&))
  , ("||", boolBoolFunc (||))
  , ("string=?",  stringBoolFunc (==))
  , ("string<?",  stringBoolFunc (<))
  , ("string>?",  stringBoolFunc (>))
  , ("string<=?", stringBoolFunc (<=))
  , ("string>=?", stringBoolFunc (>=))
  , ("symbol?",   function1 unpackAny (return . Bool) isSymbol)
  , ("boolean?",  function1 unpackAny (return . Bool) isBool)
  , ("string?",   function1 unpackAny (return . Bool) isString)
  , ("list?",     function1 unpackAny (return . Bool) isList)
  , ("pair?",     function1 unpackAny (return . Bool) isPair)
  , ("vector?",   function1 unpackAny (return . Bool) isVector)
  , ("number?",   function1 unpackAny (return . Bool) isNumber)
  , ("complex?",  function1 unpackAny (return . Bool) isComplex)
  , ("real?",     function1 unpackAny (return . Bool) isReal)
  , ("rational?", function1 unpackAny (return . Bool) isRational)
  , ("integer?",  function1 unpackAny (return . Bool) isInteger)
  , ("environment?", function1 unpackAny (return . Bool) isEnv)
  , ("identifier?", function1 unpackAny (return . Bool) isIdentifier)
  , ("eq?",    eqvP)
  , ("eqv?",   eqvP)
  , ("equal?", equalP)
  , ("car",  car)
  , ("cdr",  cdr)
  , ("cons", cons)
  , ("undefined", makeUndefined)
  , ("error", raiseException)
  , ("make-syntactic-closure", makeSyntacticClosure)
  , ("strip-syntactic-closures", stripSyntacticClosures)
  ]

ioPrimitiveFuncs :: [(String, IOPrimitiveFunc)]
ioPrimitiveFuncs =
  [ ("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-file", closePort)
  , ("close-output-file", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("display", displayProc)
  , ("newline", newlineProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  , ("identifier=?", identifierEqualP)
  ]


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


-- Other -----------------------------------------------------------------

makeUndefined :: PrimitiveFunc
makeUndefined []         = return Undefined
makeUndefined badArgList = throwError $ NumArgsError 0 badArgList

raiseException :: PrimitiveFunc
raiseException []            = throwError $ DefaultError ""
raiseException (reason:args) = throwError . DefaultError $
  show reason ++ " " ++ unwords (map show args)

makeSyntacticClosure :: PrimitiveFunc
makeSyntacticClosure [SyntacticEnv env, List ns, expr] = do
  freenames <- mapM (\atom -> catchError (return $ atomName atom)
                             (const . throwError $ TypeMismatchError "symbol" atom)) ns
  return $ SyntacticClosure env freenames expr
makeSyntacticClosure [_, _, _]  = throwError $ DefaultError "make-syntactic-closure"
makeSyntacticClosure badArgList = throwError $ NumArgsError 3 badArgList

stripSyntacticClosures :: PrimitiveFunc
stripSyntacticClosures [arg] = return $ stripSyntacticClosures' arg
  where
    stripSyntacticClosures' :: LispVal -> LispVal
    stripSyntacticClosures' (SyntacticClosure _ _ expr) = stripSyntacticClosures' expr
    stripSyntacticClosures' (List args)  = List (map stripSyntacticClosures' args)
    stripSyntacticClosures' (Pair h t)   = Pair (map stripSyntacticClosures' h) (stripSyntacticClosures' t)
    stripSyntacticClosures' (Vector arr) = Vector $ fmap stripSyntacticClosures' arr
    stripSyntacticClosures' expr = expr
stripSyntacticClosures badArgList = throwError $ NumArgsError 1 badArgList


-- IO Primitives ---------------------------------------------------------

applyProc :: IOPrimitiveFunc
applyProc [func, List args] = apply func args
applyProc (func:args)       = apply func args
applyProc _                 = throwError $ DefaultError "Apply Error"

makePort :: IOMode -> IOPrimitiveFunc
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _    badArgList        = throwError $ NumArgsError 1 badArgList

closePort :: IOPrimitiveFunc
closePort [Port handle] = liftIO (hClose handle) >> return (Bool True)
closePort _             = return (Bool False)

readProc :: IOPrimitiveFunc
readProc []            = readProc [Port stdin]
readProc [Port handle] = liftIO (hGetLine handle)
                         >>= liftErrorM . readExpr
readProc badArgList    = throwError $ NumArgsError 1 badArgList

writeProc :: IOPrimitiveFunc
writeProc [obj]              = writeProc [obj, Port stdout]
writeProc [obj, Port handle] = liftIO $ hPutStr handle (show obj) >> return Undefined
writeProc badArgList         = throwError $ NumArgsError 2 badArgList

displayProc :: IOPrimitiveFunc
displayProc [obj]                   = displayProc [obj, Port stdout]
displayProc [String s, Port handle] = liftIO $ hPutStr handle s  >> return Undefined
displayProc [obj,      Port handle] = liftIO $ hPutStr handle (show obj) >> return Undefined
displayProc badArgList              = throwError $ NumArgsError 2 badArgList

newlineProc :: IOPrimitiveFunc
newlineProc []            = newlineProc [Port stdout]
newlineProc [Port handle] = liftIO $ hPutStr handle "\n" >> return Undefined
newlineProc badArgList    = throwError $ NumArgsError 1 badArgList


readContents :: IOPrimitiveFunc
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents badArgList        = throwError $ NumArgsError 1 badArgList


readParse :: String -> IOErrorM [LispVal]
readParse path = liftIO (readFile path)
                   >>= liftErrorM . readExprList

readAll :: IOPrimitiveFunc
readAll [String filename] = liftM List $ readParse filename
readAll badArgList        = throwError $ NumArgsError 1 badArgList


identifierEqualP :: IOPrimitiveFunc
identifierEqualP [SyntacticEnv env1, id1, SyntacticEnv env2, id2]
  | isIdentifier id1 && isIdentifier id2 = do
    let (env1', var1) = case id1 of
                          SyntacticClosure synEnv _ (Atom var) -> (synEnv, var)
                          Atom var                             -> (env1, var)
                          _                                    -> (env1, "")
    let (env2', var2) = case id2 of
                          SyntacticClosure synEnv _ (Atom var) -> (synEnv, var)
                          Atom var                             -> (env2, var)
                          _                                    -> (env2, "")
    liftM Bool (liftIO $ equalVar env1' var1 env2' var2)
  | otherwise = return (Bool False)
identifierEqualP badArgList   = throwError $ NumArgsError 4 badArgList
