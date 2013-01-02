# simple-scheme

[![Build Status](https://travis-ci.org/hyone/simple-scheme.png?branch=master)](https://travis-ci.org/hyone/simple-scheme)

Simple Scheme Implementation
( based on [Write Yourself a Scheme in 48 hours](http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html>) )

## Build App 

    $ cd simple-scheme
    $ cabal configure
    $ cabal build

    # For convenience
    $ cp dist/build/simple-scheme/simple-scheme .

## Run App

REPL:

    $ ./simple-scheme

Run a script file:

    $ ./simple-scheme script.scm

## Usage

```scheme
;; arithmetic
scheme> (+ 1 2)
3
scheme> (* 3 4 5)
60

;; list operation
scheme> (define a (cons 1 '(2 3)))
(1 2 3)
scheme> (car a)
1
scheme> (cdr a)
(2 3)

;; using high order function
scheme> (define b (map (lambda (i) (+ i 5)) '(1 2 3 4 5))))
(6 7 8 9 10)
scheme> (filter even? b)
(6 8 10)
scheme> (foldr + 0 b)
40
scheme> (define c (unfold (lambda (n) (+ n 2)) 0 (lambda (n) (>= n 10))))
(0 2 4 6 8 10)
scheme> (apply sum c)
30

;; define a function
scheme> (define (f x y) (+ x y))
#<closure>
scheme> (f 1 2)
3
scheme> (f 5)
Expected 2 args; found values 5

;; define recursive function
scheme> (define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
#<closure>
scheme> (factorial 10)
3628800

;; define closure
scheme> (define (gen-accumulator n) (lambda (i) (set! n (+ n i))))
#<closure>
scheme> (define acc (gen-accumulator 5))
#<closure>
scheme> (acc 3)
8
scheme> (acc 5)
13
scheme> (acc 6)
19

;; quote and quasiquote
scheme> '(1 2 3)
(1 2 3)
scheme> (define a 5)
5
scheme> `(1 2 ,a . 9)
(1 2 5 . 9)
scheme> `(1 (2 ,a) 3)
(1 (2 5) 3)
```

## Run Unit Test

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

    # to run single test
    $ runhaskell test/LispCodeSpec.hs
