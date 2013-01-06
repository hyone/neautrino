module Scheme.Syntax
  ( primitiveSyntax
  , defineForm
  , lambdaForm
  , quoteForm
  , quasiquoteForm
  , letForm
  , ifForm
  , condForm
  , caseForm
  ) where

import Scheme.Type (LispVal(..), PrimitiveFunc, SyntaxHandler)
import Scheme.Env (Env, Var, bindVars, defineVar, setVar)
import Scheme.Error
import {-# SOURCE #-} Scheme.Eval (eval, evalBody)
import Scheme.Function.Equal (eqvP)
import Scheme.Load (load)

import Control.Monad (liftM, liftM2)
import Control.Monad.IO.Class (liftIO)
import Data.Array (bounds, elems, listArray)


type UnarySyntaxHandler = Env -> LispVal -> IOThrowsError LispVal


syntaxError :: String -> [LispVal] -> IOThrowsError a
syntaxError name args = throwError $ SyntaxError name (List (Atom name : args))


fromUnarySyntaxHandler :: String -> UnarySyntaxHandler -> SyntaxHandler
fromUnarySyntaxHandler _    handler env [x]     = handler env x
fromUnarySyntaxHandler name _       _   badArgs = syntaxError name badArgs


-- helper to build function 
makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body =
  return $ Func (map show params) varargs body env

makeNormalFunc :: Monad m => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalFunc = makeFunc Nothing

makeVarargsFunc :: Monad m => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarargsFunc = makeFunc . Just . show


-- Primitive Syntax

primitiveSyntax :: [(String, SyntaxHandler)]
primitiveSyntax =
  [ ("define", defineForm)
  , ("lambda", lambdaForm)
  , ("quote", quoteForm)
  , ("quasiquote", quasiquoteForm)
  , ("set!", setForm)
  , ("load", loadForm)
  , ("if", ifForm)
  , ("let", letForm)
  , ("begin", beginForm)
  , ("cond", condForm)
  , ("case", caseForm)
  , ("unquote", unquoteForm)
  ]


defineForm :: SyntaxHandler
-- variable
defineForm env [Atom var, form] = eval env form >>= defineVar env var
-- normal function: (define (hoge a b) ...)
defineForm env (List (Atom var : params) : body) =
  makeNormalFunc env params body >>= defineVar env var
-- varargs function: (define (hoge a . b) ...)
defineForm env (Pair (Atom var : params) varargs : body) =
  makeVarargsFunc varargs env params body >>= defineVar env var
defineForm _   badArgs = throwError $ SyntaxError "define" (List (Atom "define" : badArgs))


lambdaForm :: SyntaxHandler
-- normal lambda expression: (lambda (a b) ...)
lambdaForm env (List params : body) = makeNormalFunc env params body
-- varargs lambda expression: (lambda (a . b) ...)
lambdaForm env (Pair params varargs : body) = makeVarargsFunc varargs env params body
-- only varargs lambda expression: (lambda a ...)
lambdaForm env (varargs@(Atom _) : body) = makeVarargsFunc varargs env [] body
lambdaForm _    badArgs = throwError $ SyntaxError "lambda" (List (Atom "lambda" : badArgs))


quoteForm :: SyntaxHandler
quoteForm = fromUnarySyntaxHandler "quote" quoteForm'
  where
    quoteForm' :: UnarySyntaxHandler
    quoteForm' _ = return


-- quasiquote and unquote
quasiquoteForm :: SyntaxHandler
quasiquoteForm = fromUnarySyntaxHandler "quasiquote" quasiquoteForm'

quasiquoteForm' :: UnarySyntaxHandler
quasiquoteForm' env (List [Atom "unquote", val]) = eval env val
quasiquoteForm' env (List xs) = liftM List $ mapM (quasiquoteForm' env) xs
quasiquoteForm' env (Pair xs x) =
  liftM2 Pair (mapM (quasiquoteForm' env) xs) (quasiquoteForm' env x)
quasiquoteForm' env (Vector as) =
  fmap (Vector . listArray (bounds as)) $ mapM (quasiquoteForm' env) (elems as)
quasiquoteForm' _   e = return e    -- quote


unquoteForm :: SyntaxHandler
unquoteForm _ args =
  throwError $ DefaultError $
    "unquote appeared outside quasiquote: " ++ show (List (Atom "unquote" : args))


setForm :: SyntaxHandler
setForm env [Atom var, form] = eval env form >>= setVar env var
setForm _   args             = syntaxError "set!" args


loadForm :: SyntaxHandler
loadForm env [String filename] = load env filename
loadForm _   args              = syntaxError "load" args


beginForm :: SyntaxHandler
beginForm = evalBody 


-- let bindings:
-- (let ((a 3) (b 5))
--   (print a) (print b) (* a b))
letForm :: SyntaxHandler
letForm env exps = case exps of
    []                   -> letError
    (_ : [])             -> return Undefined
    (List params : body) -> do
      params' <- mapM extractVarName params
      liftIO (bindVars env params')
             >>= (evalBody `flip` body)
    _ -> letError
  where
    extractVarName :: LispVal -> IOThrowsError (Var, LispVal)
    extractVarName (List (Atom x : y : _)) = return (x, y)
    extractVarName _ = letError
    letError :: IOThrowsError a
    letError = syntaxError "let" exps

ifForm :: SyntaxHandler
ifForm env [p, thenExp, elseExp] = do
  result <- eval env p
  case result of
    Bool True  -> eval env thenExp
    Bool False -> eval env elseExp
    val        -> throwError $ TypeMismatchError "bool" val
ifForm _   badArgs = syntaxError "if" badArgs


condForm :: SyntaxHandler
condForm _   [] = return Undefined
condForm env (List (p : body) : xs) = do
  result <- eval env p
  case result of
    Bool False -> condForm env xs
    _          -> evalBody env body
condForm _   exps = syntaxError "cond" exps


or' :: PrimitiveFunc
or' []                = return (Bool False)
or' (Bool False : xs) = or' xs
or' (x : _)           = return x

caseForm :: SyntaxHandler
caseForm _   [_] = return Undefined
caseForm env (p : List (List vs : body) : rest) =
  do base         <- eval env p
     results      <- liftThrowsError $ mapM (\v -> eqvP [base, v]) vs
     Bool matched <- liftThrowsError $ or' results
     if matched then
       evalBody env body
     else
       caseForm env (base:rest)
caseForm _    exps = syntaxError "case" exps
