module Neautrino.TH
  ( scheme ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Neautrino.Parser (readExpr)
import Neautrino.Type (LispVal(..))


quoteParseLispValExp :: String -> TH.ExpQ
quoteParseLispValExp s =
  dataToExpQ (const Nothing) $ 
    case readExpr s of
      Right ast -> ast
      Left  err -> String (show err)

scheme :: QuasiQuoter
scheme = QuasiQuoter {
  quoteExp  = quoteParseLispValExp
, quotePat  = undefined
, quoteType = undefined
, quoteDec  = undefined
}
