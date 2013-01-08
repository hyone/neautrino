{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal DataType module.
module Neautrino.Internal.Type
  ( SyntaxHandler
  , PrimitiveFunc
  , IOPrimitiveFunc
  , LispVal(..)
  , LispError(..)
  , ThrowsError
  , IOThrowsError
  , extractValue
  , liftThrowsError
  , Env
  , EvalExprMonad
  , liftEvalExprM
  , runEvalExprMonad
  ) where


import Control.Monad.Error (Error(..), ErrorT, MonadError, throwError, runErrorT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (Array, elems)
import Data.Complex (Complex)
import Data.Generics ( Constr, Data(..), DataType
                     , Fixity(Prefix), Typeable(..), TyCon
                     , mkConstr, mkDataType, mkTyCon3, mkTyConApp)
import Data.IORef (IORef)
import Text.Parsec (ParseError)
import System.IO (Handle)


-- Primitive Types -------------------------------------------------------

type SyntaxHandler = [LispVal] -> EvalExprMonad LispVal

type PrimitiveFunc   = [LispVal] -> ThrowsError LispVal
type IOPrimitiveFunc = [LispVal] -> IOThrowsError LispVal

data LispVal = Atom String
             | List [LispVal]
             | Pair [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Integer Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Character Char
             | String String
             | Bool Bool
             | Port Handle
             | Undefined
             | PrimitiveFunc PrimitiveFunc
             | IOPrimitiveFunc IOPrimitiveFunc
             | Syntax { syntaxName    :: String
                      , syntaxHandler :: SyntaxHandler }
             | Func { funcParams  :: [String]
                    , funcVararg  :: Maybe String
                    , funcBody    :: [LispVal]
                    , funcClosure :: Env }
  deriving (Typeable)

-- Eq class instance

instance Eq LispVal where
  (Atom x)      == (Atom y)      = x == y 
  (Integer x)   == (Integer y)   = x == y 
  (Float x)     == (Float y)     = x == y 
  (Ratio x)     == (Ratio y)     = x == y 
  (Complex x)   == (Complex y)   = x == y 
  (Character x) == (Character y) = x == y 
  (String x)    == (String y)    = x == y 
  (Bool x)      == (Bool y)      = x == y 
  Undefined     == Undefined     = True
  List xs       == List ys       = xs == ys
  Pair xs x     == Pair ys y     = xs == ys && x == y
  Vector xs     == Vector ys     = xs == ys
  _             == _             = False

-- Show class instance

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (Character c)      = "#\\" ++ [c]
showVal (String s)         = "\"" ++ s ++ "\""
showVal (Atom name)        = name
showVal (Integer i)        = show i
showVal (Float n)          = show n
showVal (Ratio n)          = show n
showVal (Complex n)        = show n
showVal (Bool True)        = "#t"
showVal (Bool False)       = "#f"
showVal (Port _)           = "#<io port>"
showVal Undefined          = "#<undef>"
showVal (Syntax name _)    = "#<syntax " ++ name ++ ">"
showVal PrimitiveFunc {}   = "#<primitive>"
showVal IOPrimitiveFunc {} = "#<io primitive>"
showVal Func {}            = "#<closure>"
showVal (List contents)    = "("  ++ unwordsList contents ++ ")"
showVal (Vector arr)       = "#(" ++ unwordsList (elems arr) ++ ")"
showVal (Pair h t)         = "("  ++ unwordsList h ++ " . " ++ showVal t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList =  unwords . map showVal

-- Data class instance

atomConstr, listConstr, pairConstr, vectorConstr :: Constr
characterConstr, stringConstr, integerConstr, floatConstr :: Constr
ratioConstr,complexConstr, boolConstr, undefinedConstr :: Constr
atomConstr      = mkConstr lispValDataType "Atom"      [] Prefix
listConstr      = mkConstr lispValDataType "List"      [] Prefix
pairConstr      = mkConstr lispValDataType "Pair"      [] Prefix
vectorConstr    = mkConstr lispValDataType "Vector"    [] Prefix
characterConstr = mkConstr lispValDataType "Character" [] Prefix
stringConstr    = mkConstr lispValDataType "String"    [] Prefix
integerConstr   = mkConstr lispValDataType "Integer"   [] Prefix
floatConstr     = mkConstr lispValDataType "Float"     [] Prefix
ratioConstr     = mkConstr lispValDataType "Ratio"     [] Prefix
complexConstr   = mkConstr lispValDataType "Complex"   [] Prefix
boolConstr      = mkConstr lispValDataType "Bool"      [] Prefix
undefinedConstr = mkConstr lispValDataType "Undefined" [] Prefix

lispValDataType :: DataType
lispValDataType = mkDataType "Neautrino.Internal.Type.LispVal"
                             [ atomConstr
                             , listConstr
                             , pairConstr
                             , vectorConstr
                             , characterConstr
                             , stringConstr
                             , integerConstr
                             , floatConstr
                             , ratioConstr
                             , complexConstr
                             , boolConstr
                             , undefinedConstr
                             ]
instance Data LispVal where
  gfoldl k z (Atom x)      = z Atom `k` x
  gfoldl k z (Integer x)   = z Integer `k` x
  gfoldl k z (Float x)     = z Float `k` x
  gfoldl k z (Ratio x)     = z Ratio `k` x
  gfoldl k z (Complex x)   = z Complex `k` x
  gfoldl k z (Character x) = z Character `k` x
  gfoldl k z (String x)    = z String `k` x
  gfoldl k z (Bool x)      = z Bool `k` x
  gfoldl k z (List xs)     = z List `k` xs
  gfoldl k z (Pair x xs)   = z Pair `k` x `k` xs
  gfoldl _ z Undefined     = z Undefined
  gfoldl _ _ x             = error $ "gfoldl: not implemented for " ++ show x

  gunfold _ _ _ = undefined

  toConstr (Atom _)      = atomConstr
  toConstr (Integer _)   = integerConstr
  toConstr (Float _)     = floatConstr
  toConstr (Ratio _)     = ratioConstr
  toConstr (Complex _)   = complexConstr
  toConstr (Character _) = characterConstr
  toConstr (String _)    = stringConstr
  toConstr (Bool _)      = boolConstr
  toConstr Undefined     = undefinedConstr
  toConstr (List _)      = listConstr
  toConstr (Pair _ _)    = pairConstr
  toConstr (Vector _)    = vectorConstr
  toConstr x             = error $ "toConstr: not implemented: " ++ show x

  dataTypeOf _ = lispValDataType


-- Error Types -------------------------------------------------------

data LispError = NumArgsError Int [LispVal]
               | TypeMismatchError String LispVal
               | ParserError ParseError
               | SyntaxError String LispVal
               | BadSpecialFormError String LispVal
               | NotFunctionError String String
               | UnboundVarError String String
               | DefaultError String
  deriving (Typeable)

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO 


-- Error class instance

instance Error LispError where
  noMsg  = DefaultError "An error has occurred"
  strMsg = DefaultError

-- Data class instance

parseErrorConstr :: Constr
parseErrorConstr = mkConstr parseErrorDataType "ParseError" [] Prefix
parseErrorDataType :: DataType
parseErrorDataType = mkDataType "Text.Parsec.Error.ParseError" [parseErrorConstr]
instance Data ParseError where
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = parseErrorDataType
  toConstr   _ = parseErrorConstr

instance Data a => Data (IOThrowsError a) where
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = error "dataTypeOf IOThrowsError"
  toConstr   _ = error "toConstr IOThrowsError"

-- Typeable class instance

parseErrorTc :: TyCon
parseErrorTc = mkTyCon3 "Text.Parsec" "Error" "ParseError"
instance Typeable ParseError where
  typeOf _ = mkTyConApp parseErrorTc []

ioThrowsErrorTc :: TyCon
ioThrowsErrorTc = mkTyCon3 "Neautrino" "Error" "IOThrowsError"
instance Typeable a => Typeable (IOThrowsError a) where
  typeOf _ = mkTyConApp ioThrowsErrorTc [typeOf (undefined :: a)]
  

-- Show class instance
 
instance Show LispError where
  show = showError

showError :: LispError -> String
showError (UnboundVarError message varname) = message ++ ": " ++ varname
showError (BadSpecialFormError message form) = message ++ ": " ++ show form
showError (NotFunctionError message func) = message ++ ": " ++ show func
showError (SyntaxError message exps) = "Syntax error at " ++ message ++ ": " ++ show exps
showError (NumArgsError expected found) = "Expected " ++ show expected
                                       ++ " args; found values " ++ (unwords . map show) found
showError (TypeMismatchError expected found) = "Invalid type: expected " ++ show expected
                                            ++ ", found " ++ show found
showError (ParserError parseError) = "Parse error at " ++ show parseError
showError (DefaultError message) = "Error: " ++ message

-- Functions

-- | extractValue from ThrowsError to String
extractValue :: (Show a) => ThrowsError a -> String
extractValue (Right val) = show val
extractValue (Left err)  = show err

-- Env Types -------------------------------------------------------

type Env = IORef [(String, IORef LispVal)]


-- EvalExprMonad ---------------------------------------------------

type EvalExprMonad a = ReaderT Env (ErrorT LispError IO) a

runEvalExprMonad :: Env -> EvalExprMonad a -> IO (ThrowsError a)
runEvalExprMonad env expr = runErrorT (runReaderT expr env)

liftEvalExprM :: ThrowsError a -> EvalExprMonad a
liftEvalExprM = lift . liftThrowsError

-- | lift ThrowsError to IOThrowsError.
liftThrowsError :: (MonadError e m) => Either e a -> m a
liftThrowsError (Left err)  = throwError err
liftThrowsError (Right val) = return val