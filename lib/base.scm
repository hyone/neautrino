
;; Basic Operation
;; ----------------------------------------------------------------

(define (not x)
  (if x #f #t))

(define (null? obj)
  (if (eqv? obj '()) #t #f))

(define (list . args) args)

(define (id obj) obj)

(define (flip func)
  (lambda (a b) (func b a)))

(define (curry func arg)
  (lambda (args)
    (apply func (cons arg (list args)))))

(define (compose f g)
  (lambda (arg)
    (f (apply g arg))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define (odd? n) (= (mod n 2) 1))
(define (even? n) (not (odd? n)))


;; List Operation
;; ----------------------------------------------------------------

(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caaar pair) (car (car (car pair))))
(define (caadr pair) (car (car (cdr pair))))
(define (cadar pair) (car (cdr (car pair))))
(define (caddr pair) (car (cdr (cdr pair))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (cdddr pair) (cdr (cdr (cdr pair))))

(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func acc lst)
  (if (null? lst)
      acc
      (foldl func (func acc (car lst)) (cdr lst))))

(define fold foldl)
(define reduce fold)


(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))


(define (map func lst)
  (foldr (lambda (x acc) (cons (func x) acc)) '() lst))

(define (filter pred lst)
  (foldr (lambda (x acc) (if (pred x) (cons x acc) acc)) '() lst))

(define (sum . lst)
  (fold + 0 lst))

(define (product . lst)
  (fold * 1 lst))

(define (and . lst)
  (fold && #t lst))

(define (or . lst)
  (fold || #f lst))

(define (max first . rest)
  (fold (lambda (old new)
          (if (> old new) old new))
        first rest))

(define (min first . rest)
  (fold (lambda (old new)
          (if (< old new) old new))
        first rest))

(define (length lst)
  (fold (lambda (acc v) (+ 1 acc)) 0 lst))

(define (reverse lst)
  (fold (flip cons) '() lst))


(define (mem-helper pred op)
  (lambda (acc next)
    (if (and (not acc) (pred (op next)))
        next
        acc)))
(define (memq obj lst)
  (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst)
  (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst)
  (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alst)
  (fold (mem-helper (curry eq? obj) car) #f alst))
(define (assv obj alst)
  (fold (mem-helper (curry eqv? obj) car) #f alst))
(define (assoc obj alst)
  (fold (mem-helper (curry equal? obj) car) #f alst))


;; Macros and Syntax
;; ------------------------------------------------------------------------
;;
;; ported from chibi-scheme:
;; http://code.google.com/p/chibi-scheme/source/browse/lib/init-7.scm

(define sc-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (make-syntactic-closure mac-env '() (f expr use-env)))))

(define rsc-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (f expr mac-env))))

(define er-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      ((lambda (rename compare) (f expr rename compare))
       ((lambda (renames)
          (lambda (identifier)
            ((lambda (cell)
               (if cell
                   (cdr cell)
                   ((lambda (name)
                      (set! renames (cons (cons identifier name) renames))
                      name)
                    (make-syntactic-closure mac-env '() identifier))))
             (assq identifier renames))))
        '())
       (lambda (x y) (identifier=? use-env x use-env y))))))

