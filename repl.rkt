#lang racket/base
;; Interactive REPL for token-rich-types.
;;   racket repl.rkt

(require racket/string racket/match racket/list
         "core.rkt" "agent.rkt" "layout.rkt")

;; Holds the STORE: (store page journal). The journal is the program's own
;; memory — screams and outcomes live in the tree, not in this REPL.
(define page (box #f))
(define last-goal (box ""))        ; the most recent build goal, for :review

(define default-page
  '(stack
    (title "Max's Burgers")
    (header (heading 1 "Max's Burgers")
            (text "The " (em "best") " burgers in Flagstaff."))
    (button "Order now")
    (footer (text "(c) 2026 Max's Burgers - "
                  (link "https://instagram.com/maxsburgers" "Instagram")))))

(define banner
  (string-append
   "token-rich-types — minimal POC\n"
   "every edit is a refinement; the type contract is the guardrail;\n"
   "components can talk: delegate ⇘, escalate ⇖, refuse, or SCREAM."))

(define (show-help)
  (displayln (string-join
    '("commands:"
      "  :show [width]      render the page as an ASCII screenshot (default 56)"
      "  :tree              show ids, witness types, and contracts (⊑)"
      "  :get <id>          inspect one node"
      "  :prompt <id> <…>   refine that node by prompting it (it rebuilds itself)"
      "  <id>.prompt(<…>)   same, dotted form — e.g. c3.prompt(say Subscribe)"
      "  :query <id> <…>    ask a node a question; it reads only what it needs"
      "  <id>.query(<…>)    same, dotted — recursive, type-guided, never mutates"
      "  :ask <…>           query the whole page"
      "  :root <…>          refine the whole page (its root node)"
      "  :pin <id> <Type>   tighten a node's contract, e.g. :pin c1 Heading"
      "  :build <goal>      seed the page as one hole and WATCH IT BUILD ITSELF"
      "  :review [goal]     the root inspects the rendered page and fixes what it dislikes"
      "  :screams           screams from the journal (language-extension proposals)"
      "  :journal           the program's own history (screams + outcomes, in-tree)"
      "  :lexicon           the language's live vocabulary (+word = grown);"
      "                     prompt a words node to EXTEND THE LANGUAGE"
      "  :new <s-expr>      replace the page, e.g. :new (stack (heading 1 \"Hi\"))"
      "  :help              this message"
      "  :quit              exit"
      ""
      "leaves:     (text …runs…) (heading 1-6 …runs…) (button \"…\") (divider)"
      "            (embed Image|Video|Audio|Page url alt)"
      "            (input <VT> \"label\")  VT ∈ String LongString Number Bool Date"
      "              Time Color Password File Email Tel Url Search"
      "              (OneOf \"a\" \"b\") (ManyOf \"a\" \"b\") (Range 0 100)"
      "            (title \"…\") (meta Description|Author|Canonical|Keywords \"…\")"
      "            (hole Type \"brief\")   — bare strings are texts, anywhere"
      "runs:       \"plain\" (em …) (strong …) (code …) (link url …) + marks:"
      "            mark small sub sup del ins kbd time abbr q cite pre"
      "containers: stack row header footer main nav section article aside figure"
      "            blockquote list olist item deflist term defn table trow form"
      "            fieldset details    — types are the capitalized kinds;"
      "            Nav ⊏ Row, Olist ⊏ List ⊏ Stack, … ; everything ⊏ Component")
    "\n")))

(define (split-first s)
  (define m (regexp-match-positions #px"\\s+" s))
  (if m (values (substring s 0 (caar m)) (substring s (cdar m)))
        (values s "")))

(define (print-wrapped prefix text [width 64])
  (let loop ([ws (string-split text)] [cur ""])
    (cond
      [(null? ws) (unless (string=? cur "") (displayln (string-append prefix cur)))]
      [else
       (define cand (if (string=? cur "") (car ws) (string-append cur " " (car ws))))
       (if (<= (string-length cand) width)
           (loop (cdr ws) cand)
           (begin (displayln (string-append prefix cur))
                  (loop (cdr ws) (car ws))))])))

;; ---------------------------------------------------------------------------
;; Event pretty-printer — the REPL shows the internal conversation.
;; verbose? #f (used by :build) compresses the request to one line.
;; ---------------------------------------------------------------------------
(define verbose? (make-parameter #t))
(define BAR "  │ ")
(define (pp-block prefix text)
  (for ([ln (in-list (regexp-split #rx"\n" text))])
    (displayln (string-append prefix ln))))

(define (pp-event tag p)
  (case tag
    [(request)
     (printf "\n  ┌─ attempt ~a/~a · refine ~a · contract ~a\n"
             (hash-ref p 'attempt) (hash-ref p 'tries)
             (hash-ref p 'target) (hash-ref p 'contract))
     (cond
       [(verbose?)
        (printf "~a→ sent to model\n" BAR)
        (when (hash-ref p 'system)
          (displayln (string-append BAR "  system:"))
          (pp-block (string-append BAR "    ") (hash-ref p 'system)))
        (displayln (string-append BAR "  user:"))
        (pp-block (string-append BAR "    ") (hash-ref p 'user))]
       [else
        (print-wrapped (string-append BAR "→ ") (hash-ref p 'instruction))])]
    [(response)
     (printf "~a← model:\n" BAR)
     (pp-block (string-append BAR "    ") (hash-ref p 'content))]
    [(typecheck)
     (case (hash-ref p 'status)
       [(ok)       (printf "~a✓ typecheck: ~a ⊑ ~a\n  └─\n"
                           BAR (hash-ref p 'wtype) (hash-ref p 'contract))]
       [(rejected) (printf "~a✗ typechecker REJECTED: ~a\n  └─\n"
                           BAR (hash-ref p 'reason))]
       [(no-op)    (printf "~a✗ no-op — component returned itself unchanged\n  └─\n" BAR)]
       [(invalid)  (printf "~a✗ invalid component: ~a\n  └─\n"
                           BAR (hash-ref p 'reason ""))])]
    [(hop)
     (case (hash-ref p 'kind)
       [(delegate)
        (printf "\n  ⇘ ~a delegates to ~a:\n" (hash-ref p 'from) (hash-ref p 'to))
        (print-wrapped "     " (hash-ref p 'message))]
       [(escalate)
        (printf "\n  ⇖ ~a ESCALATES to ~a:\n" (hash-ref p 'from) (hash-ref p 'to))
        (print-wrapped "     " (hash-ref p 'message))])]
    [(fill-step)
     (printf "\n══ step ~a ══ ~a fills itself (depth ~a) ─ ~a\n"
             (hash-ref p 'step) (hash-ref p 'hole)
             (hash-ref p 'depth) (hash-ref p 'brief))]
    [(journal)
     (printf "~a✎ journaled: ~a at ~a\n" BAR (hash-ref p 'kind) (hash-ref p 'at))]
    [(query-request)
     (printf "  ? ~a ← ~a\n" (hash-ref p 'target) (hash-ref p 'question))]
    [(query-ask)
     (printf "  ⇙ ~a asks ~a: ~a\n" (hash-ref p 'from) (hash-ref p 'to)
             (hash-ref p 'question))]
    [(answer)
     (printf "  = ~a answers:\n" (hash-ref p 'target))
     (print-wrapped "      " (hash-ref p 'text))]
    [(review-start)
     (printf "\n══ review round ~a/~a ══ the root inspects the whole page\n"
             (hash-ref p 'round) (hash-ref p 'rounds))]
    [(review-verdict)
     (case (hash-ref p 'verdict)
       [("approve") (printf "  ✓ approved: ~a\n" (hash-ref p 'note ""))]
       [("revise")
        (printf "  ✎ revise — ~a edit(s):\n" (length (hash-ref p 'edits)))
        (for ([e (in-list (hash-ref p 'edits))])
          (print-wrapped "     " (format "~a: ~a" (hash-ref e 'id)
                                         (hash-ref e 'instruction))))])]))

;; ---------------------------------------------------------------------------
;; The journal — read from the tree; dispatch is the only writer.
;; ---------------------------------------------------------------------------
(define (scream-entries)
  (filter (λ (e) (eq? (entry-kind e) 'Screamed)) (journal-entries (unbox page))))

(define (record-scream at instruction info)
  (printf "\n  ╔═ SCREAM from ~a ═══════════════════════════════════\n" at)
  (print-wrapped "  ║ " (format "while attempting: ~a" instruction))
  (print-wrapped "  ║ " (format "diagnosis: ~a" (hash-ref info 'diagnosis)))
  (print-wrapped "  ║ " (format "proposal: ~a" (hash-ref info 'proposal)))
  (printf "  ╚═ journaled in the tree (~a total — :screams to review)\n"
          (length (scream-entries))))

(define (cmd-screams)
  (define ss (scream-entries))
  (if (null? ss)
      (displayln "no screams journaled — the language has sufficed so far.")
      (for ([e (in-list ss)] [i (in-naturals 1)])
        (printf "~a. [~a at ~a] ~a\n" i (node-id e) (entry-at e) (entry-instruction e))
        (print-wrapped "   " (format "diagnosis: ~a" (car (entry-notes e))))
        (print-wrapped "   " (format "proposal: ~a" (cadr (entry-notes e)))))))

(define (cmd-journal)
  (define es (journal-entries (unbox page)))
  (if (null? es)
      (displayln "journal is empty — the program has no history yet.")
      (for ([e (in-list es)])
        (printf "~a  ~a @~a  ~s\n" (node-id e) (entry-kind e) (entry-at e)
                (entry-instruction e))
        (for ([n (in-list (entry-notes e))]) (print-wrapped "     " n)))))

;; ---------------------------------------------------------------------------
;; Commands
;; ---------------------------------------------------------------------------
(define (cmd-show [warg ""])
  (cond
    [(not (unbox page)) (displayln "(empty page)")]
    [else
     (define t (string-trim warg))
     (define w (if (regexp-match? #px"^[0-9]+$" t) (string->number t) (page-width)))
     (displayln (render-page (unbox page) #:width w))]))

(define (cmd-get id)
  (define n (and (unbox page) (find-node (unbox page) id)))
  (if n
      (begin (printf "~a : ~a  ⊑ ~a\n" id (type->string (type-of n))
                     (type->string (node-spec n)))
             (displayln (render n)))
      (printf "no node ~a (see :tree)\n" id)))

;; The visible cascade: agent.rkt's `cascade` with REPL rendering — a frame
;; after every accepted refinement. `skip` holds hole ids that predate the
;; current prompt (deliberate TODOs are not auto-filled).
(define (fill-holes! #:skip [skip '()])
  (define-values (final summary)
    (cascade (unbox page) #:emit pp-event #:skip skip
             #:on-refined
             (λ (new-root r)
               (set-box! page new-root)
               (printf "\n✓ ~a := ~a\n\n" (outcome-at r) (outcome-info r))
               (cmd-show))
             #:on-blocked
             (λ (h r)
               (if (eq? (outcome-kind r) 'screamed)
                   (record-scream (outcome-at r) (or (hole-brief h) "(no brief)")
                                  (outcome-info r))
                   (printf "\n✗ ~a — hole ~a blocked\n" (outcome-info r) (node-id h))))))
  (set-box! page final)
  (case (hash-ref summary 'status)
    [(complete)
     (printf "\n■ all sub-components built (~a step~a).\n\n"
             (hash-ref summary 'steps) (if (= 1 (hash-ref summary 'steps)) "" "s"))
     (displayln (tree (unbox page))) (newline)
     (cmd-show)]
    [(budget)
     (printf "\n■ step budget exhausted (~a holes remain).\n"
             (hash-ref summary 'remaining))
     (cmd-show)]))

(define (do-prompt id instruction)
  (cond
    [(string=? (string-trim instruction) "")
     (displayln "give an instruction, e.g. :prompt c2 say Subscribe")]
    [else
     (define pre-holes (map node-id (holes-bfs (unbox page))))
     (define r (dispatch (unbox page) id instruction #:emit pp-event))
     ;; adopt the root unconditionally: even refusals leave a journal entry
     (set-box! page (outcome-root r))
     (case (outcome-kind r)
       [(refined)
        (printf "\n✓ ~a  (refinement landed at ~a)\n\n" (outcome-info r) (outcome-at r))
        (cmd-show)
        ;; Recursion is the default semantics of prompt: any NEW briefed holes
        ;; the refinement introduced now build themselves, visibly.
        (define new-holes
          (filter (λ (h) (not (member (node-id h) pre-holes)))
                  (holes-bfs (unbox page))))
        (unless (null? new-holes)
          (printf "\n⟳ the refinement left ~a sub-component~a to build (~a) — they now fill themselves…\n"
                  (length new-holes) (if (= 1 (length new-holes)) "" "s")
                  (string-join (map node-id new-holes) " "))
          (parameterize ([verbose? #f])
            (fill-holes! #:skip pre-holes)))]
       [(refused)
        (printf "\n✗ ~a refused: ~a\n" (outcome-at r) (outcome-info r))]
       [(screamed)
        (record-scream (outcome-at r) instruction (outcome-info r))]
       [(gave-up)
        (printf "\n✗ gave up: ~a\n" (outcome-info r))])]))

(define (promptable? id)
  (define n (find-node (unbox page) id))
  (cond
    [(not n) (printf "no node ~a (see :tree)\n" id) #f]
    [(system-node? n)
     (printf "~a is a system node (store/journal/entry) — history cannot be prompted\n" id) #f]
    [else #t]))

(define (cmd-prompt rest)
  (define-values (id instr) (split-first (string-trim rest)))
  (cond
    [(string=? id "") (displayln "usage: :prompt <id> <instruction>")]
    [(promptable? id) (do-prompt id instr)]))

(define (do-query id question)
  (cond
    [(string=? (string-trim question) "")
     (displayln "give a question, e.g. :query c0 what buttons are on this page?")]
    [else
     (define a (query-node (unbox page) id question #:emit pp-event))
     (printf "\n» ~a\n" a)]))

(define (cmd-query rest)
  (define-values (id q) (split-first (string-trim rest)))
  (cond
    [(string=? id "") (displayln "usage: :query <id> <question>")]
    [(promptable? id) (do-query id q)]))

(define (cmd-pin rest)
  (define-values (id trest) (split-first (string-trim rest)))
  (cond
    [(or (string=? id "") (string=? (string-trim trest) ""))
     (displayln "usage: :pin <id> <Type>   e.g. :pin c1 Heading")]
    [else
     (define T (read (open-input-string trest)))
     (set-box! page (pin-node (unbox page) id T))
     (printf "pinned: ~a now has contract ~a\n" id (type->string T))]))

;; Replacing the page keeps the store (journal and lexicon survive), and the
;; new page is parsed under the store's LIVE language — extended words work.
(define (cmd-new rest)
  (define new-page
    (parameterize ([current-lexicon (lexicon-hash (unbox page))])
      (parse (read (open-input-string rest)))))
  (set-box! page (store-with-page (unbox page) new-page))
  (cmd-show))

(define (cmd-lexicon)
  (for ([w (in-list (node-children (lexicon-of (unbox page))))])
    (define base (cddr (node-spec w)))
    (printf "~a ~a: ~a\n" (node-id w) (words-cat w)
            (string-join
             (for/list ([s (in-list (words-list w))])
               (if (memq s base) (symbol->string s)
                   (string-append "+" (symbol->string s))))   ; grown words
             " "))))

;; The root reviews the whole rendered page and dispatches corrective edits
;; until it approves (or the round budget is spent).
(define (run-review goal)
  (parameterize ([verbose? #f])
    (define-values (final summary)
      (review (unbox page) goal #:emit pp-event
              #:on-change (λ (nr o)
                            (set-box! page nr)
                            (printf "\n  ✓ ~a := ~a\n" (outcome-at o) (outcome-info o))
                            (cmd-show))))
    (set-box! page final)
    (printf "\n■ review ~a after ~a round~a.\n\n"
            (hash-ref summary 'status) (hash-ref summary 'rounds)
            (if (= 1 (hash-ref summary 'rounds)) "" "s"))
    (cmd-show)))

(define (cmd-review rest)
  (define g (string-trim rest))
  (run-review (cond [(non-empty-string? g) g]
                    [(non-empty-string? (unbox last-goal)) (unbox last-goal)]
                    [else "make this a coherent, complete, well-structured page"])))

;; The self-building page — just `prompt` on an empty page: seed one briefed
;; hole, let the cascade run, then the root reviews the result and corrects it.
(define (cmd-build goal)
  (cond
    [(string=? (string-trim goal) "") (displayln "usage: :build <goal>")]
    [else
     (set-box! last-goal (string-trim goal))
     (set-box! page (store-with-page (unbox page)
                                     (parse (list 'hole 'Component (string-trim goal)))))
     (printf "seeded the page as a single hole:\n\n")
     (cmd-show)
     (parameterize ([verbose? #f])
       (fill-holes!))
     (printf "\n──────── the root now reviews the rendered page ────────\n")
     (run-review (unbox last-goal))]))

;; ---------------------------------------------------------------------------
;; Dispatch loop
;; ---------------------------------------------------------------------------
(define (handle line)
  (define t (string-trim line))
  (define dot (regexp-match #px"^([A-Za-z0-9_]+)\\.(prompt|query)\\((.*)\\)\\s*$" t))
  (cond
    [(string=? t "") (void)]
    [dot
     (define id (list-ref dot 1))
     (define op (list-ref dot 2))
     (define instr (string-trim (list-ref dot 3)))
     (define unq (if (and (>= (string-length instr) 2)
                          (char=? (string-ref instr 0) #\")
                          (char=? (string-ref instr (sub1 (string-length instr))) #\"))
                     (substring instr 1 (sub1 (string-length instr)))
                     instr))
     (when (promptable? id)
       (if (string=? op "query") (do-query id unq) (do-prompt id unq)))]
    [(string-prefix? t ":")
     (define-values (cmd rest) (split-first (substring t 1)))
     (case cmd
       [("show" "s" "print" "p") (cmd-show rest)]
       [("tree" "t")             (displayln (tree (unbox page)))]
       [("get" "g")              (cmd-get (string-trim rest))]
       [("prompt")               (cmd-prompt rest)]
       [("query")                (cmd-query rest)]
       [("ask")                  (do-query (node-id (page-of (unbox page))) rest)]
       [("root" "r")             (do-prompt (node-id (page-of (unbox page))) rest)]
       [("pin")                  (cmd-pin rest)]
       [("build")                (cmd-build rest)]
       [("review")               (cmd-review rest)]
       [("screams")              (cmd-screams)]
       [("journal")              (cmd-journal)]
       [("lexicon" "lex")        (cmd-lexicon)]
       [("new")                  (cmd-new rest)]
       [("help" "h" "?")         (show-help)]
       [("quit" "q" "exit")      (exit 0)]
       [else (printf "unknown command: :~a (try :help)\n" cmd)])]
    [else (displayln "type :help, or use <id>.prompt(instruction)")]))

(define (repl)
  (let loop ()
    (display "» ") (flush-output)
    (define line (read-line))
    (unless (eof-object? line)
      (with-handlers ([exn:fail? (λ (e) (printf "error: ~a\n" (exn-message e)))])
        (handle line))
      (loop))))

(module+ main
  (set-box! page (make-store (parse default-page)))
  (displayln banner) (newline)
  (show-help) (newline)
  (displayln (tree (unbox page))) (newline)
  (cmd-show) (newline)
  (repl))
