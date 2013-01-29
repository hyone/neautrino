module Neautrino.Function where

import Neautrino.Env (equalVar, isBound)
import Neautrino.Error
import Neautrino.Eval (apply)
import Neautrino.Function.Helper
import Neautrino.Function.Equal (eqvP, equalP)
import Neautrino.Function.Number
import Neautrino.Parser (readExpr, readExprList)
import Neautrino.Type

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import System.IO (IOMode(..), stdin, stdout, hPutStr, openFile, hClose, hGetLine)


primitiveFuncs :: [(String, PrimitiveFunc)]
primitiveFuncs =
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
  , ("make-string",     makeString)
  , ("list->vector",    listToVector)
  , ("vector->list",    vectorToList)
  , ("number->string",  numberToString)
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


stringToSymbol :: PrimitiveFunc
stringToSymbol [String s] = return (Atom s)
stringToSymbol [arg]      = throwError $ TypeMismatchError "string" arg
stringToSymbol badArgList = throwError $ NumArgsError 1 badArgList


symbolToString :: PrimitiveFunc
symbolToString [Atom var]                        = return (String var)
symbolToString [SyntacticClosure _ _ (Atom var)] = return (String var)
symbolToString [x]                               = throwError $ TypeMismatchError "identifier" x
symbolToString badArgList                        = throwError $ NumArgsError 1 badArgList


stringAppend :: PrimitiveFunc
stringAppend = stringAppendReverse "" . reverse
  where
    stringAppendReverse :: String -> PrimitiveFunc
    stringAppendReverse acc []              = return $ String acc
    stringAppendReverse acc (String s : xs) = stringAppendReverse (s ++ acc) xs
    stringAppendReverse _   (x : _)         = throwError $ TypeMismatchError "string" x

makeString :: PrimitiveFunc
makeString [Integer n, Character c] = return $ String (replicate (fromInteger n) c)
makeString [x        , Character _] = throwError $ TypeMismatchError "integer" x
makeString [_        , y          ] = throwError $ TypeMismatchError "character" y
makeString badArgList               = throwError $ NumArgsError 1 badArgList


listToVector :: PrimitiveFunc
listToVector = undefined

vectorToList :: PrimitiveFunc
vectorToList = undefined


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
    -- if both var1 and var2 are not bounded, compare by var name
    b1 <- liftIO $ isBound env1' var1
    b2 <- liftIO $ isBound env2' var2
    liftM Bool $ if not b1 && not b2 then
      return $ var1 == var2
    else
      liftIO $ equalVar env1' var1 env2' var2
  | otherwise = return (Bool False)
identifierEqualP badArgList   = throwError $ NumArgsError 4 badArgList
