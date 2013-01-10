# Neautrino

[![Build Status](https://travis-ci.org/hyone/neautrino.png?branch=master)](https://travis-ci.org/hyone/neautrino)

Simple Scheme Intepreter
( develop based on [Write Yourself a Scheme in 48 hours](http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html>) )

## Build App 

    $ cd neautrino
    $ cabal configure
    $ cabal build

    # For convenience
    $ cp dist/build/neautrino/neautrino .

## Run App

REPL:

    $ ./neautrino
    # if use readline
    $ rlwrap ./neautrino

Run a script file:

    $ ./neautrino script.scm

## Usage

```scheme
;; arithmetic
neautrino> (+ 1 2)
3
neautrino> (* 3 4 5)
60

;; list operation
neautrino> (define a (cons 1 '(2 3)))
(1 2 3)
neautrino> (car a)
1
neautrino> (cdr a)
(2 3)

;; using high order function
neautrino> (define b (map (lambda (i) (+ i 5)) '(1 2 3 4 5))))
(6 7 8 9 10)
neautrino> (filter even? b)
(6 8 10)
neautrino> (foldr + 0 b)
40
neautrino> (define c (unfold (lambda (n) (+ n 2)) 0 (lambda (n) (>= n 10))))
(0 2 4 6 8 10)
neautrino> (apply sum c)
30

;; define a function
neautrino> (define (f x y) (+ x y))
#<closure>
neautrino> (f 1 2)
3
neautrino> (f 5)
Expected 2 args; found values 5

;; define recursive function
neautrino> (define (fact x) (if (= x 1) 1 (* x (fact (- x 1)))))
#<closure>
neautrino> (fact 10)
3628800

;; define closure
neautrino> (define (gen-accumulator n) (lambda (i) (set! n (+ n i))))
#<closure>
neautrino> (define acc (gen-accumulator 5))
#<closure>
neautrino> (acc 3)
8
neautrino> (acc 5)
13
neautrino> (acc 6)
19

;; quote and quasiquote
neautrino> '(1 2 3)
(1 2 3)
neautrino> (define a 5)
5
neautrino> `(1 2 ,a . 9)
(1 2 5 . 9)
neautrino> `(1 (2 ,a) 3)
(1 (2 5) 3)

;; macro
neautrino> (define-macro when (lambda (t . body) `(if ,t (begin ,@body))))
#<macro when>
neautrino> (when (= (+ 1 2) 3) (display "first\n") (display "second\n") 'end)
first
second
end
```

## Using scheme as DSL in Haskell Code
```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Neautrino (evalAST, initEnv, scheme)

main :: IO ()
main = do
  env <- initEnv
  ret <- evalAST env [scheme|
    (begin
      (define x 5)
      (set! x 9)
      x)
  |]
  print ret
  -- => Right 9
```

## Run Unit Test

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

    # to run single test
    $ runhaskell -Wall -isrc:test test/LispCodeSpec.hs
