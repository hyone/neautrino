{-# LANGUAGE OverloadedStrings #-}
module Neautrino.ParserSpec (spec) where

import Test.Hspec
import Neautrino.TestUtil (shouldBeT, shouldErrorT)
import Data.Array (listArray)
import Data.Ratio ((%))
import Data.Complex (Complex(..))

import Neautrino.Parser
import Neautrino.Error (LispError(..))
import Neautrino.Type (LispVal(..))


parserError :: LispError
parserError = ParserError undefined

spec :: Spec
spec = do
  describe "Neautrino.Parser" $ do
    describe "atom" $ do
      it "should parse: hoge->fuga" $
        readExpr "hoge->fuga" `shouldBeT` Atom "hoge->fuga"

      it "should parse: .a" $
        readExpr ".a" `shouldBeT` Atom ".a"

      it "should parse: =>" $
        readExpr "=>" `shouldBeT` Atom "=>"

      it "should parse: ..." $
        readExpr "..." `shouldBeT` Atom "..."

      it "should parse: ." $
        readExpr "." `shouldErrorT` ParserError undefined

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
        readExpr "2" `shouldBeT` Integer 2

      it "should parse: #b110" $ 
        readExpr "#b110" `shouldBeT` Integer 6

      it "should parse: #o37" $ 
        readExpr "#o37" `shouldBeT` Integer 31

      it "should parse: #d37" $ 
        readExpr "#d37" `shouldBeT` Integer 37

      it "should parse: #xA7" $ 
        readExpr "#xA7"  `shouldBeT` Integer 167

    describe "float" $ do
      it "should parse: 3.24" $ 
        readExpr "3.24" `shouldBeT` Float 3.24

      it "should parse: .2" $ 
        readExpr ".2" `shouldBeT` Float 0.2

      it "should parse: 5." $ 
        readExpr "5." `shouldBeT` Float 5.0

      it "should parse: 3.2e3" $
        readExpr "3.24e3" `shouldBeT` Float 3240.0

    describe "ratio" $ do
      it "should parse: 4/2" $ 
        readExpr "4/2" `shouldBeT` Ratio (4 % 2)

      it "should parse: #x22/5" $ 
        readExpr "#x22/5" `shouldBeT` Ratio (34 % 5)

    describe "complex" $ do
      it "should parse: 5.2+3i" $ 
        readExpr "5.2+3i" `shouldBeT` Complex (5.2 :+ 3)

      it "should parse: #x52+#d53i" $
        readExpr "#x52+#d53i" `shouldBeT` Complex (82.0 :+ 53.0)

    describe "list" $ do
      it "should parse: (hoge)" $ 
        readExpr "(hoge)" `shouldBeT`
          List [Atom "hoge"]

      it "should parse: (+ 1 2 3)" $ 
        readExpr "(+ 1 2 3)" `shouldBeT`
          List [Atom "+", Integer 1, Integer 2, Integer 3]

      it "should parse: ()" $ 
        readExpr "()" `shouldBeT` List []

    describe "pairs" $ do
      it "should parse: (a . 2)" $
        readExpr "(a . 2)" `shouldBeT`
          Pair [Atom "a"] (Integer 2)

      it "should parse: (a #\\c . 2)" $ 
        readExpr "(a #\\c . 2)" `shouldBeT`
          Pair [Atom "a", Character 'c'] (Integer 2)

      it "should parse error: (.)" $ 
        readExpr "(.)" `shouldErrorT` parserError

    describe "vector" $ do
      it "should parse: #(1 2 3)" $ 
        readExpr "#(1 2 3)" `shouldBeT`
          Vector (listArray (0, 2) [Integer 1, Integer 2, Integer 3])

      it "should parse: #()" $ 
        readExpr "#()" `shouldBeT`
          Vector (listArray (0, -1) [])

    describe "quote" $ do
      it "should parse: '(1 2 3)" $
        readExpr "'(1 2 3)" `shouldBeT`
          List [Atom "quote", List [Integer 1, Integer 2, Integer 3]]

    describe "quasiquote" $ do
      it "should parse: `(1 2 ,a 4)" $
        readExpr "`(1 2 ,a 4)" `shouldBeT`
          List [Atom "quasiquote", List [Integer 1, Integer 2, List [Atom "unquote", Atom "a"], Integer 4]]

      it "should parse: `(1 2 ,@a 4)" $
        readExpr "`(1 2 ,@a 4)" `shouldBeT`
          List [Atom "quasiquote", List [ Integer 1, Integer 2
                                        , List [Atom "unquote-splicing", Atom "a"]
                                        , Integer 4 ]]

    describe "comment" $ do
      it "should parse 2 ; comment" $
         readExpr "2 ; comment"   `shouldBeT` Integer 2
      it "should parse ; comment\n 2" $
         readExprList " ; comment\n 2" `shouldBeT` [Integer 2]


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
