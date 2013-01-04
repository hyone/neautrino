module Scheme.ParserSpec (spec) where

import Test.Hspec
import Scheme.TestUtil (shouldBeT, shouldErrorT)
import Data.Array (listArray)

import Scheme.Parser
import Scheme.Error (LispError(..))
import Scheme.Type (LispVal(..))
import Scheme.Error (ThrowsError)


parserError :: LispError
parserError = ParserError undefined

spec :: Spec
spec = do
  describe "Scheme.Parser" $ do
    describe "bool" $ do
      it "should parse: #t" $
        readExpr "#t" `shouldBeT` Bool True

      it "should parse: #f" $
        readExpr "#f" `shouldBeT` Bool False

      it "should parse error: #true" $
        readExpr "#\\true" `shouldErrorT` parserError

      it "should parse error: #false" $
        readExpr "#\\false" `shouldErrorT` parserError

    describe "char" $ do
      it "should parse: #\\c" $ 
        readExpr "#\\c" `shouldBeT` Character 'c'

      it "should parse: #\\space" $ 
        readExpr "#\\space" `shouldBeT` Character ' '

      it "should parse: #\\newline" $ 
        readExpr "#\\newline" `shouldBeT` Character '\n'

      it "should parse error: #\\ss" $ 
        readExpr "#\\ss" `shouldErrorT` parserError

    describe "string" $ do
      it "should parse: \"hello world\"" $ 
        readExpr "\"hello world\"" `shouldBeT` String "hello world"

      it "should parse: \"\"" $ 
        readExpr "\"\"" `shouldBeT` String ""
          
    describe "number" $ do
      it "should parse: 2" $ 
        readExpr "2" `shouldBeT` Number 2

      it "should parse: #b110" $ 
        readExpr "#b110" `shouldBeT` Number 6

      it "should parse: #o37" $ 
        readExpr "#o37" `shouldBeT` Number 31

      it "should parse: #d37" $ 
        readExpr "#d37" `shouldBeT` Number 37

      it "should parse: #xA7" $ 
        pending "a bug not to accept 'a' - 'f' characters."
        -- readExpr "#xA7"  `shouldBeT` Number 167

    describe "float" $ do
      it "should parse: 3.24" $ 
        readExpr "3.24" `shouldBeT` Float 3.24

      it "should parse: 3.2e3" $
        readExpr "3.24e3" `shouldBeT` Float 3240.0

    describe "list" $ do
      it "should parse: (hoge)" $ 
        readExpr "(hoge)" `shouldBeT`
          List [Atom "hoge"]

      it "should parse: (+ 1 2 3)" $ 
        readExpr "(+ 1 2 3)" `shouldBeT`
          List [Atom "+", Number 1, Number 2, Number 3]

      it "should parse: ()" $ 
        readExpr "()" `shouldBeT` List []

    describe "pairs" $ do
      it "should parse: (a . 2)" $
        readExpr "(a . 2)" `shouldBeT`
          DottedList [Atom "a"] (Number 2)

      it "should parse: (a #\\c . 2)" $ 
        readExpr "(a #\\c . 2)" `shouldBeT`
          DottedList [Atom "a", Character 'c'] (Number 2)

      it "should parse error: (.)" $ 
        readExpr "(.)" `shouldErrorT` parserError

    describe "vector" $ do
      it "should parse: #(1 2 3)" $ 
        readExpr "#(1 2 3)" `shouldBeT`
          Vector (listArray (0, 2) [Number 1, Number 2, Number 3])

      it "should parse: #()" $ 
        readExpr "#()" `shouldBeT`
          Vector (listArray (0, -1) [])

    describe "quote" $ do
      it "should parse: '(1 2 3)" $
        readExpr "'(1 2 3)" `shouldBeT`
          List [Atom "quote", List [Number 1, Number 2, Number 3]]

      it "should parse: `(1 2 ,a 4)" $
        readExpr "`(1 2 ,a 4)" `shouldBeT`
          List [Atom "quasiquote", List [Number 1, Number 2, List [Atom "unquote", Atom "a"], Number 4]]

    describe "comment" $ do
      it "should parse 2 ; comment" $
         readExpr "2 ; comment"   `shouldBeT` Number 2
      it "should parse ; comment\n 2" $
         readExpr "; comment\n 2" `shouldBeT` Number 2


    describe "pragmatic case" $ do
      it "shoud parse: (define (map func lst)(foldr (lambda (x acc) (cons (func x) acc)) '() lst))" $
        readExpr "(define (map func lst) (foldr (lambda (x acc) (cons (func x) acc)) '() lst))" `shouldBeT`
          List [ Atom "define"
               , List [ Atom "map", Atom "func", Atom "lst"]
               , List [ Atom "foldr"
                      , List [ Atom "lambda"
                             , List [Atom "x", Atom "acc"]
                             , List [Atom "cons", List [Atom "func", Atom "x"], Atom "acc"]]
                      , List [Atom "quote", List []]
                      , Atom "lst"]]
    

main :: IO ()
main = hspec spec
