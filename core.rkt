#lang racket/base
;; token-rich-types — minimal core
;;
;; A program is a tree of NODES. Each node has:
;;   - a stable short id        (idiom: identity is intrinsic)
;;   - a spec  = its type contract / upper bound   (idiom: spec vs. witness)
;;   - a body  = the witness term
;;
;; The only write is REFINEMENT: replace a node's body with a new one whose
;; tightest type is a SUBTYPE of the node's spec. The checker certifies it.

(require racket/match racket/list racket/string)
(provide (all-defined-out))

;; ---------------------------------------------------------------------------
;; Identity
;; ---------------------------------------------------------------------------
(define id-counter (box 0))
(define base36 "0123456789abcdefghijklmnopqrstuvwxyz")
(define (->base36 n)
  (if (zero? n)
      "0"
      (let loop ([n n] [acc '()])
        (if (zero? n)
            (list->string acc)
            (loop (quotient n 36)
                  (cons (string-ref base36 (remainder n 36)) acc))))))
(define (fresh-id)
  (define n (unbox id-counter))
  (set-box! id-counter (add1 n))
  (string-append "c" (->base36 n)))                ; short: c0 c1 … ca … c10

;; ---------------------------------------------------------------------------
;; Nodes and bodies
;;
;; body ::= (text   String)
;;        | (header Level String)        ; Level ∈ 1..6
;;        | (button String)
;;        | (stack  Node ...)            ; children are themselves nodes
;;        | (hole)                       ; a TODO; its contract is node-spec
;; ---------------------------------------------------------------------------
(struct node (id spec body) #:transparent)

(define (level? n) (and (exact-integer? n) (<= 1 n 6)))

;; Parse a "surface" datum (what a human/agent writes — no ids) into a node
;; tree, minting ids and inferring each node's spec from its kind. `spec`
;; overrides the top node's contract (used when refining a hole/target).
(define (parse datum [spec #f])
  (match datum
    [(list 'text (? string? s))
     (node (fresh-id) (or spec 'Text) (list 'text s))]
    [(list 'header (? level? n) (? string? s))
     (node (fresh-id) (or spec 'Header) (list 'header n s))]
    [(list 'button (? string? s))
     (node (fresh-id) (or spec 'Button) (list 'button s))]
    [(cons 'stack children)
     (node (fresh-id) (or spec 'Stack)
           (cons 'stack (map (λ (c) (parse c)) children)))]
    [(list 'hole T)
     (node (fresh-id) T (list 'hole))]
    [_ (error 'parse "not a valid component: ~s" datum)]))

;; Inverse of parse: node tree -> surface datum (drops ids/specs).
(define (node->surface n)
  (match (node-body n)
    [(list 'text s)     (list 'text s)]
    [(list 'header l s) (list 'header l s)]
    [(list 'button s)   (list 'button s)]
    [(list 'hole)       (list 'hole (node-spec n))]
    [(cons 'stack cs)   (cons 'stack (map node->surface cs))]))

;; ---------------------------------------------------------------------------
;; Types and subtyping
;;
;; type ::= Component | Text | Header | Button | Stack | (Stack type ...)
;;
;; Lattice:  Header <: Text ,  every kind <: Component ,
;;           (Stack …) <: Stack ,  stacks covariant in children.
;; ---------------------------------------------------------------------------
(define (stack-type? t) (and (pair? t) (eq? (car t) 'Stack)))

;; The tightest type of a node (its witness type).
(define (type-of n)
  (match (node-body n)
    [(list 'text _)     'Text]
    [(list 'header _ _) 'Header]
    [(list 'button _)   'Button]
    [(cons 'stack cs)   (cons 'Stack (map type-of cs))]
    [(list 'hole)       (node-spec n)]))   ; a hole is the top of its lattice

(define (subtype? a b)
  (cond
    [(equal? a b)                              #t]
    [(eq? b 'Component)                        #t]   ; everything is a Component
    [(and (eq? a 'Header) (eq? b 'Text))       #t]   ; a header is a text
    [(and (stack-type? a) (eq? b 'Stack))      #t]   ; concrete stack is a Stack
    [(and (stack-type? a) (stack-type? b))
     (and (= (length (cdr a)) (length (cdr b)))
          (andmap subtype? (cdr a) (cdr b)))]        ; covariant children
    [else                                      #f]))

(define (type->string t)
  (cond
    [(symbol? t) (symbol->string t)]
    [(stack-type? t)
     (string-append "(Stack " (string-join (map type->string (cdr t)) " ") ")")]
    [else (format "~s" t)]))

;; ---------------------------------------------------------------------------
;; Navigation / functional update
;; ---------------------------------------------------------------------------
(define (find-node n id)
  (if (string=? (node-id n) id)
      n
      (match (node-body n)
        [(cons 'stack cs) (for/or ([c (in-list cs)]) (find-node c id))]
        [_ #f])))

(define (replace-node n id new-node)
  (if (string=? (node-id n) id)
      new-node
      (match (node-body n)
        [(cons 'stack cs)
         (struct-copy node n
                      [body (cons 'stack
                                  (map (λ (c) (replace-node c id new-node)) cs))])]
        [_ n])))

;; ---------------------------------------------------------------------------
;; Observation: render to ASCII  (idiom: observation closes the loop)
;; ---------------------------------------------------------------------------
(define (render n [indent 0])
  (define pad (make-string indent #\space))
  (match (node-body n)
    [(list 'text s)     (string-append pad s)]
    [(list 'header l s) (string-append pad (make-string l #\#) " " s)]
    [(list 'button s)   (string-append pad "[ " s " ]")]
    [(list 'hole)       (string-append pad "‹TODO: " (type->string (node-spec n)) "›")]
    [(cons 'stack cs)   (string-join (map (λ (c) (render c indent)) cs) "\n")]))

;; A structural view that exposes ids, witness types, and contracts.
(define (body-label body)
  (match body
    [(list 'text s)     (format "text ~s" s)]
    [(list 'header l s) (format "header ~a ~s" l s)]
    [(list 'button s)   (format "button ~s" s)]
    [(cons 'stack cs)   (format "stack[~a]" (length cs))]
    [(list 'hole)       "hole"]))

(define (tree n [indent 0])
  (define pad (make-string indent #\space))
  (define line (format "~a~a  ~a  : ~a  ⊑ ~a"
                       pad (node-id n) (body-label (node-body n))
                       (type->string (type-of n)) (type->string (node-spec n))))
  (match (node-body n)
    [(cons 'stack cs)
     (string-join (cons line (map (λ (c) (tree c (+ indent 2))) cs)) "\n")]
    [_ line]))
