#lang racket (require eopl/eopl)

;; dTree
(define-datatype dTree dTree?
  (leaf-t
   (datum number?))
  (node-t
   (symbol symbol?)
   (left dTree?)
   (right dTree?)))

;; dTree -> num
(define (dTree-size tree)
  (cases dTree tree
    (leaf-t (datum) 1)
    (node-t (s left right) (+ (+ (dTree-size left) (dTree-size right)) 1))))

;; dTree -> num
(define (dTree-height tree)
  (cases dTree tree
    (leaf-t (datum) 0)
    (node-t (s left right) (max (+ (dTree-height left) 1) (+ (dTree-height right) 1)))))

;; dTree -> [[num]]
(define (dTree-paths tree)
  (cases dTree tree
    (leaf-t (datum) '(()))
    (node-t (s l r) (append (map (lambda (list) (cons 0 list)) (dTree-paths l))
                            (map (lambda (list) (cons 1 list)) (dTree-paths r))))))

;; dTree -> bool
(define (dTree-perfect? tree)
  (define paths (dTree-paths tree))
  (if (empty? paths)
      #t
      (dTree-Perfect? (cdr paths) (length (car paths)))))

;; {[[num]], num} -> bool
(define (dTree-Perfect? paths len)
  (if (empty? paths)
      #t
      (if (equal? len (length (car paths)))
          (dTree-Perfect? (cdr paths) len)
          #f)))

;; { (symbol -> symbol), (num -> num), dTree} -> dTree
(define (dTree-map f g t)
  (cases dTree t
    (leaf-t (datum) (leaf-t (g datum)))
    (node-t (s l r) (node-t (f s) (dTree-map f g l) (dTree-map f g r)))))

;; [symbol] -> dTree
(define (list->tree list)
  (if (empty? list)
      (leaf-t 0)
      (node-t (car list) (list->tree (cdr list)) (list->tree (cdr list)))))

;; { ([num], num), dTree } -> dTree 
(define (replaceLeafAt f t)
  (if (empty? f)
      t
      (replaceLeafAt (cdr f) (replaceLeafHelper (car f) t))))

;; { ([num], num), dTree } -> dTree
(define (replaceLeafHelper pair t)
  (cases dTree t
    (leaf-t (n) (leaf-t (cdr pair)))
    (node-t (n l r)
            (if (zero? (caar pair))
                (node-t n (replaceLeafHelper (cons (cdar pair) (cdr pair)) l) r)
                (node-t n l (replaceLeafHelper (cons (cdar pair) (cdr pair)) r))))))

;; ( [symbol], [([num], num)] ) -> dTree
(define (bf->dTree pairE)
  (replaceLeafAt (cdr pairE) (list->tree (car pairE))))

;; symbol -> symbol
(define symbol-upcase
  (compose string->symbol (compose string-upcase symbol->string)))

;; num -> num
(define (succ n)
  (+ n 1))

(define tLeft (node-t 'w (node-t 'x (leaf-t 2) (leaf-t 5)) (leaf-t 8)))
(define tRight (node-t 'w (node-t 'x (leaf-t 2) (leaf-t 5)) (node-t 'y (leaf-t 7) (leaf-t 5))))
(define func '(((0 0 0) . 0)
              ((0 0 1) . 1)
              ((0 1 0) . 1)
              ((0 1 1) . 0)
              ((1 0 0) . 1)
              ((1 0 1) . 0)
              ((1 1 0) . 0)
              ((1 1 1) . 1)))

(define binFunc
  '( (x y z) .
             (((0 0 0) . 0)
              ((0 0 1) . 1)
              ((0 1 0) . 1)
              ((0 1 1) . 0)
              ((1 0 0) . 1)
              ((1 0 1) . 0)
              ((1 1 0) . 0)
              ((1 1 1) . 1)
              )))

(define tree (list->tree '(x y z)))