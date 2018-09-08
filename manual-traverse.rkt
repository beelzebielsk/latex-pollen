#lang racket
(require txexpr 
         pollen/decode)

; tag-list is a list of tag functions, which are all of the form
; (cons symbol? procedure)
; where the symbol is the name of the tag, and the procedure is the
; function corresponding to that tag.
; tag-list txexpr? -> txexpr?
(define (apply-tag-funcs tag-list expr)
  (define (get-tag-names tag-list)
    (map car tag-list))
  (define (tag-to-apply? tag-name)
    (member tag-name (get-tag-names tag-list)))
  (define (get-tag-func tag-name)
    (let [(result (findf (λ (elem) (eq? tag-name (car elem))) tag-list))]
      (if result
        (cdr result)
        #f)))
  (define (apply-tag-func tag-name texpr)
    ((get-tag-func tag-name) texpr))
  (define (txexpr-transform t)
    (displayln (format "The expression is ~v" t))
    (displayln (format "txexpr?: ~v" (txexpr? t)))
    (cond [(not (txexpr? t)) t]
          [(null? (get-elements t))
           (if (tag-to-apply? (get-tag t))
             (apply-tag-func (get-tag t) t)
             t)]
          [else
            (let [(new-children
                    (map txexpr-transform (get-elements t)))]
              (if (tag-to-apply? (get-tag t))
                (txexpr (get-tag t) (get-attrs t) new-children)
                (apply-tag-func (get-tag t)
                                (txexpr (get-tag t) 
                                        (get-attrs t)
                                        new-children))))]))
  ;(txexpr-transform expr)
  (decode expr
          #:txexpr-proc
          (lambda (t)
            (if (and (txexpr? t) (tag-to-apply? (get-tag t)))
              (apply-tag-func (get-tag t) t)
              t))))

(define (apply-tag-funcs-to-elements tag-list elements)
  (get-elements 
    (apply-tag-funcs tag-list 
                     (txexpr (gensym "temp-tag") null elements))))

(provide (all-defined-out))
