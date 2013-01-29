module Neautrino.Function.String
  ( toString
  , symbolToString
  , stringToSymbol
  , stringAppend
  , makeString
  , stringLength
  , stringRef
  , subString
  ) where

import Neautrino.Error
import Neautrino.Type


toString :: PrimitiveFunc
toString [x]        = return $ String (show x)
toString badArgList = throwError $ NumArgsError 1 badArgList


symbolToString :: PrimitiveFunc
symbolToString [Atom var]                        = return (String var)
symbolToString [SyntacticClosure _ _ (Atom var)] = return (String var)
symbolToString [x]                               = throwError $ TypeMismatchError "identifier" x
symbolToString badArgList                        = throwError $ NumArgsError 1 badArgList


stringToSymbol :: PrimitiveFunc
stringToSymbol [String s] = return (Atom s)
stringToSymbol [arg]      = throwError $ TypeMismatchError "string" arg
stringToSymbol badArgList = throwError $ NumArgsError 1 badArgList


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


stringLength :: PrimitiveFunc
stringLength [String s] = return $ Integer (toInteger (length s))
stringLength [arg]      = throwError $ TypeMismatchError "string" arg
stringLength badArgList = throwError $ NumArgsError 1 badArgList


stringRef :: PrimitiveFunc
stringRef [String s, Integer i] = let n = fromInteger i :: Int in
                                  if n < length s then
                                    return $ Character (s !! n)
                                  else
                                    throwError $ DefaultError ("out of range: " ++ show s ++ ", " ++ show i)
stringRef [String _, y]         = throwError $ TypeMismatchError "integer" y
stringRef [x       , _]         = throwError $ TypeMismatchError "string" x
stringRef badArgList            = throwError $ NumArgsError 2 badArgList


subString :: PrimitiveFunc
subString [String str, Integer s, Integer e] = let start = fromInteger s
                                                   end   = fromInteger e in
                                               return . String $ drop start . take end $ str
subString [String _, Integer _, z] = throwError $ TypeMismatchError "integer" z
subString [String _, y        , _] = throwError $ TypeMismatchError "integer" y
subString [x       , _        , _] = throwError $ TypeMismatchError "string" x
subString badArgList               = throwError $ NumArgsError 3 badArgList
