#lang racket/base
;; Interactive REPL for token-rich-types.
;;   racket repl.rkt

(require racket/string racket/match "core.rkt" "agent.rkt")

(define page (box #f))

(define default-page
  '(stack
    (header 1 "Max's Burgers")
    (text "The best burgers in Flagstaff.")
    (button "Order now")))

(define banner
  (string-append
   "token-rich-types — minimal POC\n"
   "every edit is a refinement; the type contract is the guardrail.\n"
   "type :help for commands."))

(define (show-help)
  (displayln (string-join
    '("commands:"
      "  :show              render the page to ASCII"
      "  :tree              show ids, witness types, and contracts (⊑)"
      "  :get <id>          inspect one node"
      "  :prompt <id> <…>   refine that node by prompting it (it rebuilds itself)"
      "  :root <…>          refine the whole page (its root node)"
      "  :new <s-expr>      replace the page, e.g. :new (stack (header 1 \"Hi\"))"
      "  :help              this message"
      "  :quit              exit"
      ""
      "components: (text \"…\") (header 1-6 \"…\") (button \"…\") (stack …) (hole T)")
    "\n")))

(define (split-first s)
  (define m (regexp-match-positions #px"\\s+" s))
  (if m (values (substring s 0 (caar m)) (substring s (cdar m)))
        (values s "")))

(define (cmd-show)
  (if (unbox page) (displayln (render (unbox page))) (displayln "(empty page)")))

(define (cmd-get id)
  (define n (and (unbox page) (find-node (unbox page) id)))
  (if n
      (begin (printf "~a : ~a  ⊑ ~a\n" id (type->string (type-of n))
                     (type->string (node-spec n)))
             (displayln (render n)))
      (printf "no node ~a (see :tree)\n" id)))

(define (do-prompt id instruction)
  (cond
    [(string=? (string-trim instruction) "")
     (displayln "give an instruction, e.g. :prompt c2 say Subscribe")]
    [else
     (define r (prompt-node (unbox page) id instruction #:log displayln))
     (cond
       [(attempt-ok? r)
        (set-box! page (attempt-root r))
        (printf "✓ ~a\n\n" (attempt-note r))
        (cmd-show)]
       [else (printf "✗ ~a\n" (attempt-note r))])]))

(define (cmd-prompt rest)
  (define-values (id instr) (split-first (string-trim rest)))
  (cond
    [(string=? id "") (displayln "usage: :prompt <id> <instruction>")]
    [(not (find-node (unbox page) id)) (printf "no node ~a (see :tree)\n" id)]
    [else (do-prompt id instr)]))

(define (cmd-new rest)
  (set-box! page (parse (read (open-input-string rest))))
  (cmd-show))

(define (handle line)
  (define t (string-trim line))
  (cond
    [(string=? t "") (void)]
    [(string-prefix? t ":")
     (define-values (cmd rest) (split-first (substring t 1)))
     (case cmd
       [("show" "s" "print" "p") (cmd-show)]
       [("tree" "t")             (displayln (tree (unbox page)))]
       [("get" "g")              (cmd-get (string-trim rest))]
       [("prompt")               (cmd-prompt rest)]
       [("root" "r")             (do-prompt (node-id (unbox page)) rest)]
       [("new")                  (cmd-new rest)]
       [("help" "h" "?")         (show-help)]
       [("quit" "q" "exit")      (exit 0)]
       [else (printf "unknown command: :~a (try :help)\n" cmd)])]
    [else (displayln "commands start with ':' — try :help")]))

(define (repl)
  (let loop ()
    (display "» ") (flush-output)
    (define line (read-line))
    (unless (eof-object? line)
      (with-handlers ([exn:fail? (λ (e) (printf "error: ~a\n" (exn-message e)))])
        (handle line))
      (loop))))

(module+ main
  (set-box! page (parse default-page))
  (displayln banner) (newline)
  (displayln (tree (unbox page))) (newline)
  (cmd-show) (newline)
  (repl))
