#lang racket (require eopl/eopl)


(define-datatype dTree dTree?
  (leaf-t
   (datum number?))
  (node-t
   (symbol symbol?)
   (left dTree?)
   (right dTree?)))

(define (dTree-size tree)
  (cases dTree tree
    (leaf-t (datum) 1)
    (node-t (s left right) (+ (+ (dTree-size left) (dTree-size right)) 1))))

(define (dTree-height tree)
  (cases dTree tree
    (leaf-t (datum) 0)
    (node-t (s left right) (max (+ (dTree-height left) 1) (+ (dTree-height right) 1)))))

(define (dTree-paths tree)
  (cases dTree tree
    (leaf-t (datum) '())
    (node-t (s left right) (cons (list 0) (dTree-paths left)) (cons (list 1) (dTree-paths right)))))

(define (dTree-perfect? tree)
  (cases dTree tree
    (leaf-t (datum) 0)
    (node-t (s l r) (equal? (+ (dTree-height l) 1) (+ (dTree-height r) 1)))))

(define (dTree-map f g t)
  (cases dTree t
    (leaf-t (datum) (leaf-t (g datum)))
    (node-t (s l r) (node-t (f s) (dTree-map f g l) (dTree-map f g r)))))

(define (list->tree )
  (

(define symbol-upcase
  (compose string->symbol (compose string-upcase symbol->string)))

(define (succ n)
  (+ n 1))

(define tLeft (node-t 'w (node-t 'x (leaf-t 2) (leaf-t 5)) (leaf-t 8)))
(define tRight (node-t 'w (node-t 'x (leaf-t 2) (leaf-t 5)) (node-t 'y (leaf-t 7) (leaf-t 5))))