#lang racket
(require txexpr "utility.rkt")

; - Core tags
;   - macro : For latex macros.
;   - environment : For latex environments.
;   - math : Represents stuff that thould go in between dollar-signs.
;   - symbol : Represents stuff that's actually one output character.
;     macros that translate to a single output character. Will also
;     have other connotations of being things meant for math mode.
; - Convenience tags
;   - ol (ordered list)
;       - Initially, it will have a mixture of 'i tags and lines that
;         start with some bullet.
;       - After processing, it's children will all be 'i tags.
;   - l (unordered list)
;       - Same as for ol.
;   - eql (equation list)
;       - Same as for ol.
;   - table
;       - The only children that table can have are rows.
;       - Rows can have plain text or 'cell tags as children. 
;       - After processing, all lines of plain text will become a
;         'cell tag.

(define (macro-args args)
  (add-between args '("}{")
               #:splice? #t
               #:before-first '("{")
               #:after-last '("}")))
(define (macro name . args)
  (txexpr 'macro #:start "{" #:end "}" args))
(define (environment name
                     #:args [args null] 
                     #:opt-args [optional null]
                     #:before-args [before null]
                     #:after-args [after null])
  (txexpr 'environment #:name (~a name)
          #:args args
          #:opt-args optional
          #:before-args before
          #:after-args after))

(define (group . _)
  (list-splice `("{" ,@_ "}")))
(define-tag-function 
  (math attrs elems)
  (txexpr 'math attrs elems))
(define-tag-function
  (ensure-math attrs elems)
  (txexpr 'ensure-math attrs elems))
(define-tag-function ($ _ text)
  (apply math text))
(define-tag-function ($$ _ text)
  (apply math #:display "" text))

(define-syntax-rule (define-math-tag (name attrs elems) body ...)
  (define-tag-function 
    (name attrs elems) 
    (let [(result ((lambda (attrs elems) body ...) attrs elems))]
      (ensure-math result))))

(define (newline? val)
  (and (string? val) (regexp-match val #px"^\\s*\n\\s*$")))

(define (split-newline lst list-tag)
  (split-where 
    lst 
    (λ (current . _) 
       (or (newline? current)
           (is-tag? current list-tag)))
    #:keep-where 
    (λ (split-elem . _) 
       (if (txexpr? split-elem) 'separate 'ignore))
    #:split-map
    (λ (current . _) 
       (txexpr list-tag null current))))

(define (split-bullets lst bullet-pattern list-tag)
  (split-where
    lst
    (λ (elem current-split . _)
       (or (is-tag? elem list-tag)
           (and (string? elem) 
                (regexp-match bullet-pattern elem)
                (not (null? current-split))
                ; TODO: If I allow the user to control how values are
                ; inserted into the current split, or allow them to
                ; consume more than 1 token from remaining, then I can
                ; let this function scrub out unnecessary newlines. 
                ; NOTE: Remember that, before you place the split in
                ; splits, the split is in reverse order. That's a
                ; nasty implementation detail and ought to be changed.
                ; You're losing clarity for the sake of some speed. Is
                ; that worth it?
                (and (string? (first current-split))
                     (string=? "\n" (first current-split))))))
    #:keep-where 
    (λ (current . _) 
       (if (string? current)
         'next
         'separate))
    #:split-map
    (λ (current . _)
       (if (txexpr? current)
         current
         (txexpr list-tag null current)))))

; txexpr? -> txexpr?
(define (convenience->core txpr)

