module Scheme.TH
  ( scheme
  , parseScheme
  ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Scheme.Eval (evalString, initEnv)
import Scheme.Parser (readExpr)
import Scheme.Type (LispVal(..))


quoteParseLispValExp :: String -> TH.ExpQ
quoteParseLispValExp s =
  dataToExpQ (const Nothing) $ 
    case readExpr s of
      Right ast -> ast
      Left _    -> Undefined

quoteEvalLispValExp :: String -> TH.ExpQ
quoteEvalLispValExp s = 
  dataToExpQ (const Nothing) $ do
    env <- initEnv
    evalString env s


parseScheme :: QuasiQuoter
parseScheme = QuasiQuoter {
  quoteExp  = quoteParseLispValExp
, quotePat  = undefined
, quoteType = undefined
, quoteDec  = undefined
}

scheme :: QuasiQuoter
scheme = QuasiQuoter {
  quoteExp  = quoteEvalLispValExp
, quotePat  = undefined
, quoteType = undefined
, quoteDec  = undefined
}
