#lang racket/base
;; The `prompt` primitive: a component refines itself.
;;
;; Grok proposes a new body; we typecheck that its tightest type is a SUBTYPE
;; of the target's contract. Accept on success, else feed the error back and
;; retry. The checker is the guardrail — the agent cannot break the contract.

(require racket/match "core.rkt" "grok.rkt")
(provide prompt-node (struct-out attempt))

(struct attempt (ok? root note) #:transparent)

(define SYSTEM #<<EOF
You edit UI components in a tiny typed language. A component is an s-expression,
exactly one of:
  (text "STRING")
  (header LEVEL "STRING")          ; LEVEL is an integer 1-6
  (button "STRING")
  (stack COMPONENT COMPONENT ...)  ; a vertical stack of components
  (hole TYPE)                      ; a TODO placeholder

Types: Component (top), Text, Header, Button, Stack.
Subtyping: Header <: Text; every component <: Component; a stack value <: Stack;
stacks are covariant in their children.

You are given the current component, its TYPE CONTRACT, and an instruction.
Return a NEW component that (1) satisfies the instruction and (2) is a SUBTYPE
of the contract. Output ONLY the new s-expression — no prose, no code fences.
EOF
)

(define (user-prompt surface contract instruction feedback)
  (string-append
   "Current component:\n" (format "~s" surface) "\n\n"
   "Type contract (your result must be a subtype of this): "
   (type->string contract) "\n\n"
   "Instruction: " instruction "\n"
   (if (string=? feedback "") "" (string-append "\n" feedback "\n"))
   "\nReturn only the new s-expression."))

;; Pull the first balanced s-expression out of the model's reply.
(define (extract-sexp str)
  (define start (for/first ([c (in-string str)] [i (in-naturals)]
                            #:when (char=? c #\()) i))
  (and start
       (with-handlers ([exn:fail? (λ (_) #f)])
         (read (open-input-string (substring str start))))))

(define (clip s [n 200])
  (if (> (string-length s) n) (string-append (substring s 0 n) "…") s))

;; Refine the node `id` in `root` per `instruction`. Returns an `attempt`.
(define (prompt-node root id instruction #:tries [tries 3] #:log [log void])
  (define target (find-node root id))
  (unless target (error 'prompt-node "no node with id ~a" id))
  (define contract (node-spec target))
  (define surface (node->surface target))
  (let loop ([k 1] [feedback ""])
    (log (format "· attempt ~a/~a → grok…" k tries))
    (define reply (grok-complete SYSTEM (user-prompt surface contract instruction feedback)))
    (define datum (extract-sexp reply))
    (define parsed (and datum (with-handlers ([exn:fail? (λ (_) #f)])
                                (parse datum contract))))
    (cond
      [(not parsed)
       (if (< k tries)
           (loop (add1 k)
                 (format "Your previous output was not a valid component. Raw: ~a"
                         (clip reply)))
           (attempt #f root "gave up: invalid syntax"))]
      [(subtype? (type-of parsed) contract)
       ;; keep the target's id so references stay stable (idiom: identity)
       (define fixed (struct-copy node parsed [id id]))
       (attempt #t (replace-node root id fixed)
                (format "~a ⊑ ~a" (type->string (type-of parsed))
                        (type->string contract)))]
      [else
       (define why (format "~a is not a subtype of ~a"
                           (type->string (type-of parsed)) (type->string contract)))
       (if (< k tries)
           (loop (add1 k)
                 (format "Your previous answer ~s was REJECTED: ~a. Return a subtype of ~a."
                         (node->surface parsed) why (type->string contract)))
           (attempt #f root (string-append "gave up: " why)))])))
