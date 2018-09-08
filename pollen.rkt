#lang racket
(require txexpr 
         "latex-commands.rkt"
         "manual-traverse.rkt"
         "utility.rkt"
         pollen/tag
         pollen/decode
         racket/stxparam)

(define-syntax ddlog
  (lambda (stx)
    (syntax-case stx ()
      [name (identifier? #'name) #'"DDLOG"]
      [(_ args ...)
       #'(ensure-math "DDLOG" args ...)])))
(define-syntax key
  (lambda (stx)
    (syntax-case stx ()
      [name (identifier? #'name) #'(ensure-math (macro 'lambda))]
      [(_ index) #'(bit key index)])))
(define-syntax input
  (lambda (stx)
    (syntax-case stx ()
      [name 
        (identifier? #'name) 
        #'(ensure-math (macro 'mathbb "I"))]
      [(_ args ...) 
       #'(ensure-math input "(" args ... ")")])))


(define-math-tag (share _ text)
  (list-splice `(,(macro 'langle) " " ,@text " " ,(macro 'rangle))))
(define-math-tag 
  (secret-share _ text)
  ;(apply share (list key "(" (cons '@ text) ")")))
  (apply share `(,key "(" ,@text ")")))
(define-math-tag 
  (set _ text)
  (list-splice 
    `( "\\{" ,@(add-between text ", ") "\\}")))
(define mult (ensure-math (macro 'otimes)))
;(define-math-tag (encryption _ text)
  ;(list-splice `(,(macro 'llbracket) " " 
                  ;,@text " " 
                  ;,(macro 'rrbracket) "_{" ,key "}")))
(define-math-tag (encryption _ text)
  (list-splice `("["
                  ,@text " " 
                  "]" "_{" ,key "}")))
(define-math-tag 
  (server _ elements)
  (let ([index (first elements)])
    (list-splice "S_{" index "}")))
(define-tag-function (congruent attr elem) "")


(define location (ensure-math "M"))
; TODO: Extract the norm-arguments routine here. The routine is about
; having space-separated arguments to a tag. This isn't the only place
; where I'd want that. And I might want to change the delimiter, too.
;(define-math-tag (bit _ text)
(define (bit . text)
  (define (norm-arguments args)
    (cond [(>= (length args) 2)
           (take args 2)]
          [(and (= 1 (length args))
                (string? (first args))
                (string-contains? (first args) " "))
           (norm-arguments (string-split (first args)))]
          [else null]))
  (let [(args (norm-arguments text))]
    (if (null? args)
      (error "Tag `bit` takes two space-separated arguments"
             text)
      (match-let [((list base index) args)]
        (ensure-math (list-splice base "^{(" index ")}"))))))


(define (split-list-at-tag-or-newline lst)
  (split-where 
    lst 
    (λ (current . _) 
       (or (and (string? current) (string=? "\n" current))
           (is-tag? current 'i)))
    #:keep-where 
    (λ (split-elem . _) 
       (if (txexpr? split-elem) 'separate 'ignore))
    #:split-map
    (λ (current . _) 
       (txexpr 'i null current))))
    
(define (eql . text)
  (displayln (format "eql contents: ~v" text))
  (displayln (format "eql split at newlines: ~v" 
                     (split-list-at-tag-or-newline text)))
  ; contextual processing: if a printable? or series of printable?
  ; is/are followed by a newline or a txexpr?, then wrap that in an
  ; 'i tag. The following is done under the assumption that the only
  ; elements in text are txexpr? and printable?. 
  (txexpr 'eql null (split-list-at-tag-or-newline text)))

(define (split-list-at-bullets-or-list-tags lst bullet-pattern list-tag)
  (split-where
    lst
    (λ (elem current-split . _)
       (or (is-tag? elem list-tag)
           (and (string? elem) 
                (regexp-match bullet-pattern elem)
                (not (null? current-split))
                ; TODO: There's more I can do here. I might want to
                ; give more control to the user on how to insert a
                ; value into the current split, consume a few extra
                ; values from the remaining, and such. Because I'd
                ; like for split-where to take care of elminating
                ; newlines around the bullets, too. So I can allow the
                ; user to control the final split, the separate split,
                ; and the remaining tokens to be scanned.
                ; NOTE: Remember that, before you place the split in
                ; splits, the split is in reverse order. That's a
                ; nasty implementation detail and ought to be changed.
                ; Besides, whatever you're doing with the consing,
                ; it's not saving you time. The Big O of append ought
                ; to be the length of the 1st list, which would be
                ; the... nope I'm wrong. I'm appending onto the end of
                ; the list, so this actually would be quadratic. No
                ; thanks.
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
         (txexpr list-tag
                 null
                 (cons
                   (regexp-replace bullet-pattern (first current) "")
                   (rest current)))))))

(define-tag-function
  (l attrs elems)
  (let* ([splits (split-list-at-bullets-or-list-tags 
                   elems 
                   #px"^\\s*-" 
                   'i)]
         [list-item-of-blanks?
           (lambda (txexpr)
             ((listof 
                (lambda (v) 
                  (and (string? v) (regexp-match #px"^\\s*$" v))))
              (get-elements txexpr)))]
         [cleaned (filter-not list-item-of-blanks? splits)])
  (txexpr 'list '((type "unordered")) cleaned)))
(define-tag-function
  (ol attrs elems)
  (txexpr 'list '((type "ordered")) 
          (split-list-at-bullets-or-list-tags elems #px"^\\s*-" 'i)))
(define-tag-function 
  (row attrs elems) 
  (txexpr 'row null (split-list-at-tag-or-newline elems)))


; TODO: Fill this in with something appropriate. Should produce a big
; sigma letter and stuff.
;(define-math-tag (sum attrs text) "")
(define-tag-function 
  (note attrs text)
  (apply environment 'note #:args (list "Note") text))
(define-tag-function 
  (example attrs text)
  (apply environment 'example text))
(define-tag-function
  (h1 attrs text)
  (apply macro 'section text))
(define-tag-function
  (h2 attrs text)
  (apply macro 'subsection text))

(define (todo . _) "")

; This function is about preventing decode-elements from acting on the
; temporarily created root tag for the elements to decode. Pollen
; introduces it as a quick-and-dirty way to implement this function
; using decode. It causes a function that acts on txexpr (eg.
; #:txexpr-proc) to not run on the temporary root tag. It depends on a
; silly implementation detail of Pollen, so this is a temporary fix.
;
; Careful when using decode-elements. It's a little weird. It
; functions by taking your list of elements and wrapping them around
; in a temporary tag, then passing that off to decode. Unfortunately,
; any function that gets called on a tag will get called for this
; temporary tag. 
; To make this concrete, something like:
;   (decode-elements (list ...))
; Becomes
;   (decode ('temp-tag ...))
; The final result of decode-elements is 
;   (get-elements (decode ('temp-tag ...)))              
; So, if you do something like I did, where I return a list of xexpr?
; from the #:txexpr-proc all the time: (a silly example follows)
;   (decode-elements #:txexpr-proc get-elements null)
; results in
;   (get-elements (decode #:txexpr-proc get-elements ('temp-tag)))
; The application of decode rightly results in null, because we do
;   (get-elements ('temp-tag))
; But then we have:
;   (get-elements null)
; Which will fail.
; And even worse, in the following example, decode-elements will have
; strange and unexpected results:
;   (decode-elements #:txexpr get-elements '((i 1) (i 2) (i 3) (i 4)))
; I intended this as a sort of flattening of the tags, but what
; happens is:
;   (decode #:txexpr get-elements ('temp-tag (i 1) (i 2) (i 3) (i 4)))
; Which results in (roughly):
;   (decode #:txexpr get-elements ('temp-tag 1 2 3 4))
; Which results in '(1 2 3 4)
; And 
;   (get-elements '(1 2 3 4))
; Results in
;   '(3 4)
; Because the initial tag and (what is thought to be the attributes)
; are removed from the list, leaving just the 1st two things.
; I also had another strange bug, where I was applying decode-elements
; to an actual txexpr, which caused me to lose only the 1st element:
;   (get-elements 
;       (decode-elements #:txexpr-proc 
;                        get-elements 
;                        '(eql (i 1) (i 2) (i 3))))
;
;   (get-elements 
;       (decode #:txexpr-proc 
;                        get-elements 
;                        '(temp-tag eql (i 1) (i 2) (i 3))))
;
;   (get-elements 
;       (decode #:txexpr-proc 
;                        get-elements 
;                        '(temp-tag eql 1 2 3)))
;
;   (get-elements '(eql 1 2 3)))
;
;   '(2 3)
;
; And thus I "mysteriously lose the 1st item".
; This is an abbreviation for make-deocde-elements-procedure.
; TODO: Change decode-elements instead to wrap the txexpr procedures
; in a procedure that makes sure to avoid the root tag. This shouldn't
; exist at all. It's depends too closely on an implementation detail
; of pollen.

(define (@-flatten txexpr) 
  (decode txexpr #:txexpr-proc (decode-flattener #:only '(@))))

(define (root . elements)
  (@-flatten
    (math-process
      (txexpr 
        'root 
        null
        (apply-tag-funcs-to-elements
          (list 
            (cons 'title
                  (lambda (tx)
                    (list-splice (apply macro 'title (get-elements tx))
                                 (macro 'maketitle))))
            (cons 'eql
                  (lambda (tx)
                    (apply environment
                           'align*
                           (decode-elements 
                             (add-between
                               (get-elements tx)
                               "\\\\\n")
                             #:txexpr-proc 
                             (make-d-el-proc get-elements)))))
            (cons 'list
                  (lambda (tx)
                    (apply environment
                           (if (string=? "unordered" (attr-ref tx 'type))
                             'itemize
                             'enumerate
                             )
                           (decode-elements 
                             (get-elements tx)
                             #:txexpr-proc
                             (lambda (tx) 
                               (if (is-tag? tx 'i) 
                                 (cons "\\item" (get-elements tx))
                                 tx))))))
            (cons 'table
                  (lambda (tx)
                    (define (get-num-cols table-tag)
                      (apply max (for/list [(row (get-elements table-tag))
                                            #:when (txexpr? row)]
                                   (length (get-elements row)))))
                    (apply environment 'tabu
                           #:before-args `("to " ,(macro 'linewidth) " ")
                           #:args (list 
                                    (string-join 
                                      (add-between 
                                        (for/list [(cols (get-num-cols tx))] "X[c]")
                                        "|")))
                           (decode 
                             tx
                             #:txexpr-proc
                             (lambda (tx)
                               (define row-end "\\\\")
                               (cond 
                                 [(is-tag? tx 'table)
                                  (list-splice
                                    (add-between 
                                      (filter 
                                        ; TODO: Change this. It's
                                        ; poorly done. You should
                                        ; perform this processing
                                        ; while the semantics of the
                                        ; different elements of the
                                        ; table are still intact,
                                        ; instead of relying on '@.
                                        (λ (tx) (is-tag? tx '@))
                                        (get-elements tx))
                                      (list-splice row-end (macro 'hline) "\n")))]
                                 [(is-tag? tx 'row)
                                  (list-splice
                                    (add-between 
                                      (filter 
                                        (λ (tx) (is-tag? tx '@))
                                        (get-elements tx))
                                      " & "))]
                                 [(is-tag? tx 'i) 
                                  (list-splice (get-elements tx))]
                                 [else tx])))))))
          elements)))))

; TODO:
; - Implement add-op and mult-op.
; - Implement the tags for pictures of boolean circuits.
; - Implement small-note.

(define-math-tag
  (log attrs elems)
  (list-splice `(,(macro 'text "log") "(" ,@elems ")")))
(define-math-tag
  (Z attrs elems)
  (list-splice `("Z/Z" ,@elems)))

(define times (ensure-math (macro 'cdot)))
(define to (ensure-math (macro 'rightarrow)))
(define identity-input (ensure-math input "_1"))

(provide (all-defined-out))
