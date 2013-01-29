module Neautrino.Function where

import Neautrino.Env (equalVar, isBound)
import Neautrino.Error
import Neautrino.Eval (apply)
import Neautrino.Function.Helper
import Neautrino.Function.Equal
import Neautrino.Function.IO
import Neautrino.Function.List
import Neautrino.Function.Number
import Neautrino.Function.String
import Neautrino.Type

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import System.IO (IOMode(..))


procedures :: [(String, Procedure)]
procedures =
  [ ("+",               numberAdd)
  , ("-",               numberSub)
  , ("*",               numberMul)
  , ("/",               numberDiv)
  , ("mod",             numberBinFunc mod)
  , ("remainder",       numberBinFunc rem)
  , ("=",               numberBoolFunc (==))
  , ("<",               numberBoolFunc (<))
  , (">",               numberBoolFunc (>))
  , ("/=",              numberBoolFunc (/=))
  , (">=",              numberBoolFunc (>=))
  , ("<=",              numberBoolFunc (<=))
  , ("&&",              boolBoolFunc (&&))
  , ("||",              boolBoolFunc (||))
  , ("string=?",        stringBoolFunc (==))
  , ("string<?",        stringBoolFunc (<))
  , ("string>?",        stringBoolFunc (>))
  , ("string<=?",       stringBoolFunc (<=))
  , ("string>=?",       stringBoolFunc (>=))
  , ("symbol?",         function1 unpackAny (return . Bool) isSymbol)
  , ("boolean?",        function1 unpackAny (return . Bool) isBool)
  , ("string?",         function1 unpackAny (return . Bool) isString)
  , ("list?",           function1 unpackAny (return . Bool) isList)
  , ("pair?",           function1 unpackAny (return . Bool) isPair)
  , ("vector?",         function1 unpackAny (return . Bool) isVector)
  , ("number?",         function1 unpackAny (return . Bool) isNumber)
  , ("complex?",        function1 unpackAny (return . Bool) isComplex)
  , ("real?",           function1 unpackAny (return . Bool) isReal)
  , ("rational?",       function1 unpackAny (return . Bool) isRational)
  , ("integer?",        function1 unpackAny (return . Bool) isInteger)
  , ("environment?",    function1 unpackAny (return . Bool) isEnv)
  , ("identifier?",     function1 unpackAny (return . Bool) isIdentifier)
  , ("eq?",             eqvP)
  , ("eqv?",            eqvP)
  , ("equal?",          equalP)
  , ("car",             car)
  , ("cdr",             cdr)
  , ("cons",            cons)
  , ("vector",          return . vector)
  , ("undefined",       makeUndefined)
  , ("error",           raiseException)
  , ("exact->inexact",  exactToInexact)
  , ("inexact->exact",  inexactToExact)
  , ("string->symbol",  stringToSymbol)
  , ("symbol->string",  symbolToString)
  , ("->string",        toString)
  , ("make-string",     makeString)
  , ("string-append",   stringAppend)
  , ("string-ref",      stringRef)
  , ("string-length",   stringLength)
  , ("substring",       subString)
  , ("list->vector",    listToVector)
  , ("vector->list",    vectorToList)
  , ("number->string",  numberToString)
  , ("make-syntactic-closure", makeSyntacticClosure)
  , ("strip-syntactic-closures", stripSyntacticClosures)
  ]

ioProcedures :: [(String, IOProcedure)]
ioProcedures =
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
  , ("current-cputime", currentCPUTime)
  ]


-- Basic Operation ---------------------------------------------------------------

applyProc :: IOProcedure
applyProc [func, List args] = apply func args
applyProc (func:args)       = apply func args
applyProc _                 = throwError $ DefaultError "Apply Error"


makeUndefined :: Procedure
makeUndefined []         = return Undefined
makeUndefined badArgList = throwError $ NumArgsError 0 badArgList


raiseException :: Procedure
raiseException []            = throwError $ DefaultError ""
raiseException (reason:args) = throwError . DefaultError $
  show reason ++ " " ++ unwords (map show args)


makeSyntacticClosure :: Procedure
makeSyntacticClosure [SyntacticEnv env, List ns, expr] = do
  freenames <- mapM (\atom -> catchError (return $ atomName atom)
                             (const . throwError $ TypeMismatchError "symbol" atom)) ns
  return $ SyntacticClosure env freenames expr
makeSyntacticClosure [_, _, _]  = throwError $ DefaultError "make-syntactic-closure"
makeSyntacticClosure badArgList = throwError $ NumArgsError 3 badArgList


stripSyntacticClosures :: Procedure
stripSyntacticClosures [arg] = return $ stripSyntacticClosures' arg
  where
    stripSyntacticClosures' :: LispVal -> LispVal
    stripSyntacticClosures' (SyntacticClosure _ _ expr) = stripSyntacticClosures' expr
    stripSyntacticClosures' (List args)  = List (map stripSyntacticClosures' args)
    stripSyntacticClosures' (Pair h t)   = Pair (map stripSyntacticClosures' h) (stripSyntacticClosures' t)
    stripSyntacticClosures' (Vector arr) = Vector $ fmap stripSyntacticClosures' arr
    stripSyntacticClosures' expr = expr
stripSyntacticClosures badArgList = throwError $ NumArgsError 1 badArgList


identifierEqualP :: IOProcedure
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
    -- if both var1 and var2 are not bounded, compare by var name
    b1 <- liftIO $ isBound env1' var1
    b2 <- liftIO $ isBound env2' var2
    liftM Bool $ if not b1 && not b2 then
      return $ var1 == var2
    else
      liftIO $ equalVar env1' var1 env2' var2
  | otherwise = return (Bool False)
identifierEqualP badArgList   = throwError $ NumArgsError 4 badArgList
