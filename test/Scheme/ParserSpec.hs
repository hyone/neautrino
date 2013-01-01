module Scheme.ParserSpec where

import Test.Hspec
import Test.HUnit (assertFailure)
import Data.Array (listArray)

import Scheme.Parser
import Scheme.Type (LispVal(..))
import Scheme.Error (ThrowsError)


message :: String -> String -> String
message expect got = "expected: " ++ expect ++ "\n but got: " ++ got

shouldParse :: (Eq a, Show a) => ThrowsError a -> a -> Expectation
shouldParse (Right x) expect = x `shouldBe` expect
shouldParse err       expect = assertFailure $ message (show expect) (show err)

assertParseError :: (Eq a, Show a) => ThrowsError a -> Expectation
assertParseError (Left _) = return ()
assertParseError x        = assertFailure $ message "ParseError" (show x)


specs :: Spec
specs = do
  describe "Scheme.Parser" $ do
    describe "bool" $ do
      it "should parse: #t" $
        readExpr "#t" `shouldParse` Bool True

      it "should parse: #f" $
        readExpr "#f" `shouldParse` Bool False

      it "should parse error: #true" $
        assertParseError $ readExpr "#\\true" 

      it "should parse error: #false" $
        assertParseError $ readExpr "#\\false" 

    describe "char" $ do
      it "should parse: #\\c" $ 
        readExpr "#\\c" `shouldParse` Character 'c'

      it "should parse: #\\space" $ 
        readExpr "#\\space" `shouldParse` Character ' '

      it "should parse: #\\newline" $ 
        readExpr "#\\newline" `shouldParse` Character '\n'

      it "should parse error: #\\ss" $ 
        assertParseError $ readExpr "#\\ss" 

    describe "string" $ do
      it "should parse: \"hello world\"" $ 
        readExpr "\"hello world\"" `shouldParse` String "hello world"

      it "should parse: \"\"" $ 
        readExpr "\"\"" `shouldParse` String ""
          
    describe "number" $ do
      it "should parse: 2" $ 
        readExpr "2" `shouldParse` Number 2

      it "should parse: #b110" $ 
        readExpr "#b110" `shouldParse` Number 6

      it "should parse: #o37" $ 
        readExpr "#o37" `shouldParse` Number 31

      it "should parse: #d37" $ 
        readExpr "#d37" `shouldParse` Number 37

      it "should parse: #xA7" $ 
        pending "a bug not to accept 'a' - 'f' characters."
        -- readExpr "#xA7"  `shouldParse` Number 167

    describe "float" $ do
      it "should parse: 3.24" $ 
        readExpr "3.24" `shouldParse` Float 3.24

      it "should parse: 3.2e3" $
        readExpr "3.24e3" `shouldParse` Float 3240.0

    describe "list" $ do
      it "should parse: (hoge)" $ 
        readExpr "(hoge)" `shouldParse`
          List [(Atom "hoge")]

      it "should parse: (+ 1 2 3)" $ 
        readExpr "(+ 1 2 3)" `shouldParse`
          List [(Atom "+"), (Number 1), (Number 2), (Number 3)]

      it "should parse: ()" $ 
        readExpr "()" `shouldParse` List []

    describe "pairs" $ do
      it "should parse: (a . 2)" $
        readExpr "(a . 2)" `shouldParse`
          DottedList [(Atom "a")] (Number 2)

      it "should parse: (a #\\c . 2)" $ 
        readExpr "(a #\\c . 2)" `shouldParse`
          DottedList [(Atom "a"), (Character 'c')] (Number 2)

      it "should parse error: (.)" $ 
        assertParseError $ readExpr "(.)"

    describe "vector" $ do
      it "should parse: #(1 2 3)" $ 
        readExpr "#(1 2 3)" `shouldParse`
          Vector (listArray (0, 2) [(Number 1), (Number 2), (Number 3)])

      it "should parse: #()" $ 
        readExpr "#()" `shouldParse`
          Vector (listArray (0, -1) [])

    describe "quote" $ do
      it "should parse: '(1 2 3)" $
        readExpr "'(1 2 3)" `shouldParse`
          List [Atom "quote", List [Number 1, Number 2, Number 3]]

      it "should parse: `(1 2 ,a 4)" $
        readExpr "`(1 2 ,a 4)" `shouldParse`
          List [Atom "quasiquote", List [Number 1, Number 2, List [Atom "unquote", Atom "a"], Number 4]]

    describe "comment" $ do
      it "should parse 2 ; comment" $
         readExpr "2 ; comment"   `shouldParse` Number 2
      it "should parse ; comment\n 2" $
         readExpr "; comment\n 2" `shouldParse` Number 2


    describe "pragmatic case" $ do
      it "shoud parse: (define (map func lst)(foldr (lambda (x acc) (cons (func x) acc)) '() lst))" $
        readExpr "(define (map func lst) (foldr (lambda (x acc) (cons (func x) acc)) '() lst))" `shouldParse`
          List [ Atom "define"
               , List [ Atom "map", Atom "func", Atom "lst"]
               , List [ Atom "foldr"
                      , List [ Atom "lambda"
                             , List [Atom "x", Atom "acc"]
                             , List [Atom "cons", List [Atom "func", Atom "x"], Atom "acc"]]
                      , List [Atom "quote", List []]
                      , Atom "lst"]]
    

main :: IO ()
main = hspec specs
