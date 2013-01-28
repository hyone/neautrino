{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}
module Neautrino.Syntax
  ( primitiveSyntaxes ) where

import Neautrino.Type
import Neautrino.Env (Env, defineVar, setVar, unsetVar)
import Neautrino.Error
import Neautrino.Eval (eval, evalBody, applyMacroTransformer)
import Neautrino.Function (stripSyntacticClosures)
import Neautrino.Load (load)

import Control.Monad (liftM, liftM2, (>=>))
import Control.Monad.Error (MonadError)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.Trans.Class (lift)
import Data.Array (bounds, elems, listArray)


type UnarySyntaxHandler = LispVal -> EvalExprMonad LispVal


syntaxError :: String -> [LispVal] -> EvalExprMonad a
syntaxError name args = throwError $ SyntaxError name (List (Atom name : args))


fromUnarySyntaxHandler :: String -> UnarySyntaxHandler -> SyntaxHandler
fromUnarySyntaxHandler _    handler [x]     = handler x
fromUnarySyntaxHandler name _       badArgs = syntaxError name badArgs


-- helper to build function 

makeClojure :: (Monad m, MonadReader Env m, MonadError LispError m)
            => Maybe String -> [LispVal] -> [LispVal]
            -> m LispVal
makeClojure varargs params body = do
  env     <- ask
  params' <- mapM (\param -> if (not . isIdentifier) param
                               then throwError $ DefaultError $ "non-symbol parameter: " ++ show param
                               else return param) params
  return $ Closure (Closure' params' varargs body env)

makeNormalClojure :: (Monad m, MonadReader Env m, MonadError LispError m)
                  => [LispVal] -> [LispVal]
                  -> m LispVal
makeNormalClojure = makeClojure Nothing

makeVarargsClojure :: (Monad m, MonadReader Env m, MonadError LispError m)
                   => LispVal -> [LispVal] -> [LispVal]
                   -> m LispVal
makeVarargsClojure = makeClojure . Just . show


-- Primitive Syntax

primitiveSyntaxes :: [(String, SyntaxHandler)]
primitiveSyntaxes =
  [ ("define", defineForm)
  , ("lambda", lambdaForm)
  , ("define-syntax", defineSyntaxForm)
  , ("quote", quoteForm)
  , ("syntax-quote", syntaxQuoteForm)
  , ("quasiquote", quasiquoteForm)
  , ("unquote", unquoteForm)
  , ("unquote-splicing", unquoteSplicingForm)
  , ("set!", setForm)
  , ("load", loadForm)
  , ("if", ifForm)
  , ("begin", beginForm)
  , ("macroexpand", macroExpand)
  ]


defineForm :: SyntaxHandler
-- variable
defineForm [Atom var, form] = eval form >>= defineVar var
-- normal function: (define (hoge a b) ...)
defineForm (List (Atom var : params) : body) =
  makeNormalClojure params body >>= defineVar var
-- varargs function: (define (hoge a . b) ...)
defineForm (Pair (Atom var : params) varargs : body) =
  makeVarargsClojure varargs params body >>= defineVar var
defineForm badArgs = throwError $ SyntaxError "define" (List (Atom "define" : badArgs))


lambdaForm :: SyntaxHandler
-- normal lambda expression: (lambda (a b) ...)
lambdaForm (List params : body) = makeNormalClojure params body
-- varargs lambda expression: (lambda (a . b) ...)
lambdaForm (Pair params varargs : body) = makeVarargsClojure varargs params body
-- only varargs lambda expression: (lambda a ...)
lambdaForm (varargs@(Atom _) : body) = makeVarargsClojure varargs [] body
lambdaForm badArgs = throwError $ SyntaxError "lambda" (List (Atom "lambda" : badArgs))


quoteForm :: SyntaxHandler
quoteForm = fromUnarySyntaxHandler "quote" quoteForm'
  where
    quoteForm' :: UnarySyntaxHandler
    quoteForm' (SyntacticClosure _ _ expr) = liftErrorM $ stripSyntacticClosures [expr]
    quoteForm' expr                        = return expr


syntaxQuoteForm :: SyntaxHandler
syntaxQuoteForm = fromUnarySyntaxHandler "syntax-quote" syntaxQuoteForm'
  where
    syntaxQuoteForm' :: UnarySyntaxHandler
    syntaxQuoteForm' = return


-- quasiquote and unquote
quasiquoteForm :: SyntaxHandler
quasiquoteForm = fromUnarySyntaxHandler "quasiquote" $
  unquote >=> unquoteSplicing

unquote :: UnarySyntaxHandler
unquote (List [Atom "unquote", val]) = eval val
unquote (List xs)   = liftM  List (mapM unquote xs)
unquote (Pair xs x) = liftM2 Pair (mapM unquote xs) (unquote x)
unquote (Vector as) = liftM  (Vector . listArray (bounds as)) (mapM unquote (elems as))
unquote e           = return e    -- quote

unquoteSplicing :: UnarySyntaxHandler
unquoteSplicing e = liftM head (unquoteSplicingList [e])
  where
    unquoteSplicingList :: [LispVal] -> EvalExprMonad [LispVal]
    unquoteSplicingList (List [Atom "unquote-splicing", val]:ys) = do
      x <- eval val
      case x of
        List xs -> liftM (xs ++) $ unquoteSplicingList ys
        _       -> liftM (x :)   $ unquoteSplicingList ys
    unquoteSplicingList (x:xs) = do
      x' <- case x of
        List ys   -> liftM  List (unquoteSplicingList ys)
        Pair ys y -> liftM2 Pair (unquoteSplicingList ys) (return y)
        Vector as -> do ys' <- unquoteSplicingList (elems as)
                        return $ Vector (listArray (0, length ys' - 1) ys')
        _         -> return x
      xs' <- unquoteSplicingList xs
      return (x':xs')
    unquoteSplicingList [] = return []


unquoteForm :: SyntaxHandler
unquoteForm args =
  throwError $ DefaultError $
    "unquote appeared outside quasiquote: " ++ show (List (Atom "unquote" : args))


unquoteSplicingForm :: SyntaxHandler
unquoteSplicingForm args =
  throwError $ DefaultError $
    "unquote-splicing appeared outside quasiquote: " ++ show (List (Atom "unquote-splicing" : args))


setForm :: SyntaxHandler
setForm [ Atom var, form ] = eval form >>= setVar var
setForm [ SyntacticClosure env _ (Atom var), form ] = do
  expr <- eval form
  local (const env) $ setVar var expr
setForm args = syntaxError "set!" args


loadForm :: SyntaxHandler
loadForm [String filename] = load filename
loadForm args              = syntaxError "load" args


beginForm :: SyntaxHandler
beginForm = evalBody 


ifForm :: SyntaxHandler
ifForm  [p, thenExp] = ifForm [p, thenExp, Undefined]
ifForm  [p, thenExp, elseExp] = do
  result <- eval p
  case result of
    Bool False -> eval elseExp
    _          -> eval thenExp
ifForm badArgs = syntaxError "if" badArgs


defineSyntaxForm :: SyntaxHandler
defineSyntaxForm exprs@[ Atom name, expr ] = do
   -- First of all, define name to enable to refer self name in expr
   defineVar name Undefined
   result <- eval expr
   case result of
     Closure closure -> do
       let macro = MacroTransformer name closure
       setVar name macro
     _               -> do
       unsetVar name   -- Remove self name when syntax error
       syntaxError "define-syntax" exprs
defineSyntaxForm exprs = syntaxError "define-syntax" exprs


macroExpand :: SyntaxHandler
macroExpand [form] = do
    expr <- eval form
    case expr of
      List (app:args) -> do
        f <- eval app
        case f of
          MacroTransformer _ mproc -> do
            useEnv <- ask
            lift $ applyMacroTransformer mproc (List (app:args)) useEnv
          _ -> return expr
      _ -> return expr
macroExpand exprs = syntaxError "macroexpand" exprs
