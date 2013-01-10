{-# LANGUAGE FlexibleContexts #-}
module Neautrino.Syntax
  ( primitiveSyntaxes ) where

import Neautrino.Type (LispVal(..), EvalExprMonad, PrimitiveFunc, SyntaxHandler)
import Neautrino.Env (Env, Var, bindVars, defineVar, setVar)
import Neautrino.Error
import Neautrino.Eval (eval, evalBody)
import Neautrino.Function.Equal (eqvP)
import Neautrino.Load (load)

import Control.Monad (liftM, liftM2)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ask, local)
import Data.Array (bounds, elems, listArray)


type UnarySyntaxHandler = LispVal -> EvalExprMonad LispVal


syntaxError :: String -> [LispVal] -> EvalExprMonad a
syntaxError name args = throwError $ SyntaxError name (List (Atom name : args))


fromUnarySyntaxHandler :: String -> UnarySyntaxHandler -> SyntaxHandler
fromUnarySyntaxHandler _    handler [x]     = handler x
fromUnarySyntaxHandler name _       badArgs = syntaxError name badArgs


-- helper to build function 
makeClojure :: (Monad m, MonadReader Env m)
         => ([String] -> Maybe String -> [LispVal] -> Env -> LispVal)
         -> Maybe String -> [LispVal] -> [LispVal]
         -> m LispVal
makeClojure constructor varargs params body  = do
  env <- ask
  return $ constructor (map show params) varargs body env

makeNormalClojure :: (Monad m, MonadReader Env m)
                  => ([String] -> Maybe String -> [LispVal] -> Env -> LispVal)
                  -> [LispVal] -> [LispVal] -> m LispVal
makeNormalClojure constructor = makeClojure constructor Nothing

makeVarargsClojure :: (Monad m, MonadReader Env m)
                   => ([String] -> Maybe String -> [LispVal] -> Env -> LispVal)
                   -> LispVal -> [LispVal] -> [LispVal] -> m LispVal
makeVarargsClojure constructor = makeClojure constructor . Just . show


-- Primitive Syntax

primitiveSyntaxes :: [(String, SyntaxHandler)]
primitiveSyntaxes =
  [ ("define", defineForm)
  , ("lambda", lambdaForm)
  , ("define-macro", defineMacroForm)
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
defineForm [Atom var, form] = eval form >>= defineVar var
-- normal function: (define (hoge a b) ...)
defineForm (List (Atom var : params) : body) =
  makeNormalClojure Func params body >>= defineVar var
-- varargs function: (define (hoge a . b) ...)
defineForm (Pair (Atom var : params) varargs : body) =
  makeVarargsClojure Func varargs params body >>= defineVar var
defineForm badArgs = throwError $ SyntaxError "define" (List (Atom "define" : badArgs))


lambdaForm :: SyntaxHandler
-- normal lambda expression: (lambda (a b) ...)
lambdaForm (List params : body) = makeNormalClojure Func params body
-- varargs lambda expression: (lambda (a . b) ...)
lambdaForm (Pair params varargs : body) = makeVarargsClojure Func varargs params body
-- only varargs lambda expression: (lambda a ...)
lambdaForm (varargs@(Atom _) : body) = makeVarargsClojure Func varargs [] body
lambdaForm badArgs = throwError $ SyntaxError "lambda" (List (Atom "lambda" : badArgs))


defineMacroForm :: SyntaxHandler
-- define-macro with normal lambda expression: (define-macro hoge (lambda (a b) ...)
defineMacroForm [Atom name, List (Atom "lambda" : List params : body)] =
  makeNormalClojure (Macro name) params body >>= defineVar name
-- varargs lambda expression: (define-macro hoge (lambda (a . b) ...)
defineMacroForm [Atom name, List (Atom "lambda" : Pair params varargs : body)] =
  makeVarargsClojure (Macro name) varargs params body >>= defineVar name
-- normal macro: (define-macro (hoge a b) ...)
defineMacroForm (List (Atom name : params) : body) =
  makeNormalClojure (Macro name) params body >>= defineVar name
-- varargs macro: (define-macro (hoge a . b) ...)
defineMacroForm (Pair (Atom name : params) varargs : body) =
  makeVarargsClojure (Macro name) varargs params body >>= defineVar name
-- otherwise, error
defineMacroForm badArgs
  = throwError $ SyntaxError "define-macro" (List (Atom "define-macro" : badArgs))


quoteForm :: SyntaxHandler
quoteForm = fromUnarySyntaxHandler "quote" quoteForm'
  where
    quoteForm' :: UnarySyntaxHandler
    quoteForm' = return


-- quasiquote and unquote
quasiquoteForm :: SyntaxHandler
quasiquoteForm = fromUnarySyntaxHandler "quasiquote" quasiquoteForm'

quasiquoteForm' :: UnarySyntaxHandler
quasiquoteForm' (List [Atom "unquote", val]) = eval val
quasiquoteForm' (List xs) = liftM List $ mapM quasiquoteForm' xs
quasiquoteForm' (Pair xs x) =
  liftM2 Pair (mapM quasiquoteForm' xs) (quasiquoteForm' x)
quasiquoteForm' (Vector as) =
  fmap (Vector . listArray (bounds as)) $ mapM quasiquoteForm' (elems as)
quasiquoteForm' e = return e    -- quote


unquoteForm :: SyntaxHandler
unquoteForm args =
  throwError $ DefaultError $
    "unquote appeared outside quasiquote: " ++ show (List (Atom "unquote" : args))


setForm :: SyntaxHandler
setForm [Atom var, form] = eval form >>= setVar var
setForm args             = syntaxError "set!" args


loadForm :: SyntaxHandler
loadForm [String filename] = load filename
loadForm args              = syntaxError "load" args


beginForm :: SyntaxHandler
beginForm = evalBody 


-- let bindings:
-- (let ((a 3) (b 5))
--   (print a) (print b) (* a b))
letForm :: SyntaxHandler
letForm exps = case exps of
    []                   -> letError
    (_ : [])             -> return Undefined
    (List params : body) -> do
      env     <- ask
      params' <- mapM extractVarTuple params
      env'    <- liftIO (bindVars env params')
      local (const env') $ evalBody body
    _ -> letError
  where
    extractVarTuple :: LispVal -> EvalExprMonad (Var, LispVal)
    extractVarTuple (List [Atom x, y]) = do { y' <- eval y; return (x, y') }
    extractVarTuple _                  = letError
    letError :: EvalExprMonad a
    letError = syntaxError "let" exps


ifForm :: SyntaxHandler
ifForm  [p, thenExp] = ifForm [p, thenExp, Undefined]
ifForm  [p, thenExp, elseExp] = do
  result <- eval p
  case result of
    Bool False -> eval elseExp
    _          -> eval thenExp
ifForm badArgs = syntaxError "if" badArgs


condForm :: SyntaxHandler
condForm [] = return Undefined
condForm (List (p : body) : xs) = do
  result <- eval p
  case result of
    Bool False -> condForm xs
    _          -> evalBody body
condForm exps = syntaxError "cond" exps


or' :: PrimitiveFunc
or' []                = return (Bool False)
or' (Bool False : xs) = or' xs
or' (x : _)           = return x

caseForm :: SyntaxHandler
caseForm [_] = return Undefined
caseForm (p : List (List vs : body) : rest) =
  do base         <- eval p
     results      <- liftErrorM $ mapM (\v -> eqvP [base, v]) vs
     Bool matched <- liftErrorM $ or' results
     if matched then
       evalBody body
     else
       caseForm (base:rest)
caseForm exps = syntaxError "case" exps
