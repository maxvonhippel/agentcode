#lang racket/base
;; Record a real session as a CASSETTE: the ordered protocol event stream, with
;; a full tree snapshot after every applied change. Replayed deterministically
;; by player.html to produce animations. Same orchestration as the live viewer.
;;
;;   racket record.rkt build "<goal>" [name]   a page builds itself
;;   racket record.rkt edit                     a targeted self-edit (the sharpie)
;;   racket record.rkt escalate                 components talk: escalate + wrap
;;
;; Writes cassettes/<name>.json.

(require racket/match racket/list racket/string racket/file json
         "core.rkt" "agent.rkt" "view.rkt")

(define rec (box '()))                 ; event hashes, newest first
(define (put! type payload)
  (set-box! rec (cons (hash-set (jsonify payload) 'type (symbol->string type))
                      (unbox rec))))

;; The emit callback for dispatch/cascade. Drop the bulky system/user prompt
;; text from requests — the player doesn't need them.
(define (emit tag payload)
  (put! tag (if (eq? tag 'request)
                (hash-remove (hash-remove payload 'system) 'user)
                payload)))

(define (tree! store)   (put! 'tree (hasheq 'tree (node->view (page-of store)))))
(define (outcome! r)    (put! 'outcome (hasheq 'kind (outcome-kind r)
                                               'at (outcome-at r)
                                               'info (outcome-info r))))

;; ---------------------------------------------------------------------------
;; Scenarios
;; ---------------------------------------------------------------------------
(define DEMO
  '(stack
    (title "Max's Burgers")
    (header (heading 1 "Max's Burgers")
            (text "The " (em "best") " burgers in Flagstaff."))
    (button "Order now")
    (footer (text "Est. 2021 - Flagstaff, Arizona"))))

(define (find-kind store tag)
  (for/first ([n (in-list (all-nodes (page-of store)))]
              #:when (eq? (car (node-body n)) tag))
    n))

(define (run-cascade store)
  (cascade store #:emit emit #:max-steps 20
           #:on-refined (λ (new-root _r) (tree! new-root))
           #:on-blocked (λ (_h rr) (outcome! rr))))

(define (scenario-build goal)
  (put! 'job-start (hasheq 'target "page" 'instruction goal 'op "build"))
  (define store0 (make-store (parse (list 'hole 'Component goal))))
  (tree! store0)
  (define-values (_final summary) (run-cascade store0))
  (put! 'summary summary))

;; A targeted prompt on an existing page: show the page, aim at one node.
(define (scenario-prompt store0 id instruction #:op [op "prompt"])
  (tree! store0)                       ; backdrop before the goal lands
  (put! 'job-start (hasheq 'target id 'instruction instruction 'op op))
  (define pre-holes (map node-id (holes-bfs store0)))
  (define r (dispatch store0 id instruction #:emit emit))
  (outcome! r)
  (when (eq? (outcome-kind r) 'refined)
    (tree! (outcome-root r))
    (define-values (_final summary)
      (cascade (outcome-root r) #:emit emit #:skip pre-holes #:max-steps 20
               #:on-refined (λ (nr _r) (tree! nr))
               #:on-blocked (λ (_h rr) (outcome! rr))))
    (put! 'summary summary)))

(define (scenario-edit)
  (define store0 (make-store (parse DEMO)))
  (define b (find-kind store0 'button))
  (scenario-prompt store0 (node-id b) "change the label to Order Online"))

(define (scenario-escalate)
  (define store0 (make-store (parse DEMO)))
  (define h (find-kind store0 'heading))
  (define pinned (pin-node store0 (node-id h) 'Heading))   ; heading can't hold a button
  (scenario-prompt pinned (node-id h)
                   "add a small ORDER button immediately to the right of the title"))

;; ---------------------------------------------------------------------------
(define (write-cassette name meta)
  (make-directory* "cassettes")
  (define events (reverse (unbox rec)))
  (define out (build-path "cassettes" (string-append name ".json")))
  (call-with-output-file out #:exists 'replace
    (λ (p) (write-json (hasheq 'name name 'meta meta 'events events) p)))
  (printf "wrote ~a  (~a events)\n" (path->string out) (length events)))

(module+ main
  (match (vector->list (current-command-line-arguments))
    [(list "build" goal name) (scenario-build goal) (write-cassette name (hasheq 'goal goal))]
    [(list "build" goal)      (scenario-build goal) (write-cassette "build" (hasheq 'goal goal))]
    [(list "edit")            (scenario-edit)       (write-cassette "edit" (hasheq))]
    [(list "escalate")        (scenario-escalate)   (write-cassette "escalate" (hasheq))]
    [args (error 'record "usage: build <goal> [name] | edit | escalate\n got: ~s" args)]))
