#lang racket/base
;; The `prompt` primitive: a component refines itself — or talks.
;;
;; A prompted component answers with exactly ONE action as a JSON object:
;;   refine | delegate | escalate | refuse | scream
;;
;; Output is deliberately NOT grammar-constrained (guided decoding degraded the
;; model); the typechecker is the guardrail. JSON is only serialization: every
;; enumerated value (marks, media kinds, value types, meta keys, kinds, types)
;; is validated into the first-class keyword space at parse, and an unknown
;; word is rejected and retried like a type error.
;;
;; Locality is structural: the model sees the page as read-context but can
;; write only the target's slot; refs may point only into the target's own
;; original subtree.

(require racket/match racket/string racket/list json
         "core.rkt" "grok.rkt" "layout.rkt")
(provide dispatch prompt-once cascade query-node (struct-out outcome))

(struct outcome (kind root at info) #:transparent)

;; --------------------------------------------------------------------------
;; JSON <-> terms
;; --------------------------------------------------------------------------
(define (sym-in s allowed what)
  (unless (string? s) (error what "expected a keyword string, got ~s" s))
  (define x (string->symbol s))
  (unless (memq x allowed)
    (error what "unknown ~a ~s; allowed: ~a" what s
           (string-join (map symbol->string allowed) " ")))
  x)

(define (string->type s) (sym-in s ALL-TYPES 'type))

(define (option-list v key)
  (define opts (hash-ref v key))
  (unless (and (list? opts) (pair? opts) (andmap string? opts))
    (error 'value-type "~a needs a non-empty list of option strings" key))
  opts)

(define (json->value-type v)
  (cond
    [(string? v) (sym-in v (lexicon-words 'Values) 'value-type)]
    [(and (hash? v) (hash-has-key? v 'oneOf))  (cons 'OneOf (option-list v 'oneOf))]
    [(and (hash? v) (hash-has-key? v 'manyOf)) (cons 'ManyOf (option-list v 'manyOf))]
    [(and (hash? v) (hash-has-key? v 'range))
     (match (hash-ref v 'range)
       [(list (? real? lo) (? real? hi)) #:when (< lo hi) (list 'Range lo hi)]
       [r (error 'value-type "range needs [min, max] with min < max, got ~s" r)])]
    [else (error 'value-type
                 "expected a value-type keyword, {\"oneOf\":[…]}, {\"manyOf\":[…]}, or {\"range\":[min,max]}, got ~s" v)]))

;; A JSON run is "plain" or {"text":…, "marks":[…], "href":…}. Reconstructed
;; as nested surface forms so `parse-runs` validates the marks.
(define (json-run->surface r)
  (cond
    [(string? r) r]
    [(hash? r)
     (define marks (for/list ([m (in-list (hash-ref r 'marks '()))])
                     (sym-in m (lexicon-words 'Marks) 'mark)))
     (define base
       (for/fold ([x (hash-ref r 'text "")]) ([m (in-list (reverse marks))])
         (list m x)))
     (define href (hash-ref r 'href #f))
     (if (and (string? href) (non-empty-string? href))
         (list 'link href base)
         base)]
    [else (error 'run "expected a string or run object, got ~s" r)]))

(define (json-runs j)
  (cond
    [(hash-has-key? j 'runs) (map json-run->surface (hash-ref j 'runs))]
    [(hash-has-key? j 'text) (list (hash-ref j 'text))]
    [else (error 'text "needs \"runs\" or \"text\"")]))

;; `index` maps the target subtree's ids to live nodes; a ref resolves to the
;; node itself (identity preserved), each id used at most once.
(define (json->surface j index used)
  (cond
    [(string? j) j]                        ; bare string child = text (sugar)
    [(not (hash? j)) (error 'component "expected an object or string, got ~s" j)]
    [else
     (define kind (hash-ref j 'kind #f))
     (case kind
       [("text")    (cons 'text (json-runs j))]
       [("heading") (list* 'heading (hash-ref j 'level) (json-runs j))]
       [("button")  (list 'button (hash-ref j 'label))]
       [("embed")   (list 'embed (sym-in (hash-ref j 'media) (lexicon-words 'Media) 'media)
                          (hash-ref j 'url) (hash-ref j 'alt ""))]
       [("input")   (list 'input (json->value-type (hash-ref j 'value))
                          (hash-ref j 'label ""))]
       [("divider") (list 'divider)]
       [("title")   (list 'title (hash-ref j 'text))]
       [("meta")    (list 'meta (sym-in (hash-ref j 'key) (lexicon-words 'Metas) 'meta-key)
                          (hash-ref j 'content))]
       [("hole")
        (define b (hash-ref j 'brief ""))
        (define T (string->type (hash-ref j 'type "Component")))
        (if (non-empty-string? b) (list 'hole T b) (list 'hole T))]
       [("ref")
        (define id (hash-ref j 'id))
        (define n (hash-ref index id
                            (λ () (error 'ref "unknown ref ~a — refs may only point into your own subtree" id))))
        (when (hash-ref used id #f) (error 'ref "duplicate ref ~a" id))
        (hash-set! used id #t)
        n]
       [else
        (define tag (and (string? kind) (string->symbol kind)))
        (if (container-tag? tag)
            (cons tag (map (λ (c) (json->surface c index used))
                           (hash-ref j 'children '())))
            (error 'component "unknown kind: ~s" kind))])]))

(define (run->json r)
  (if (and (null? (run-marks r)) (not (run-href r)))
      (run-str r)
      (let* ([h (hasheq 'text (run-str r))]
             [h (if (pair? (run-marks r))
                    (hash-set h 'marks (map symbol->string (run-marks r)))
                    h)]
             [h (if (run-href r) (hash-set h 'href (run-href r)) h)])
        h)))

(define (value-type->json vt)
  (match vt
    [(cons 'OneOf opts)   (hasheq 'oneOf opts)]
    [(cons 'ManyOf opts)  (hasheq 'manyOf opts)]
    [(list 'Range lo hi)  (hasheq 'range (list lo hi))]
    [(? symbol?)          (symbol->string vt)]))

(define (node->json n #:ids? [ids? #f])
  (define (go n)
    (define base
      (match (node-body n)
        [(list 'text runs)
         (if (and (= 1 (length runs)) (string? (run->json (car runs))))
             (hasheq 'kind "text" 'text (run-str (car runs)))
             (hasheq 'kind "text" 'runs (map run->json runs)))]
        [(list 'heading l runs)
         (if (and (= 1 (length runs)) (string? (run->json (car runs))))
             (hasheq 'kind "heading" 'level l 'text (run-str (car runs)))
             (hasheq 'kind "heading" 'level l 'runs (map run->json runs)))]
        [(list 'button s)    (hasheq 'kind "button" 'label s)]
        [(list 'embed m u a) (hasheq 'kind "embed" 'media (symbol->string m)
                                     'url u 'alt a)]
        [(list 'input vt l)  (hasheq 'kind "input" 'value (value-type->json vt)
                                     'label l)]
        [(list 'divider)     (hasheq 'kind "divider")]
        [(list 'title s)     (hasheq 'kind "title" 'text s)]
        [(list 'meta k s)    (hasheq 'kind "meta" 'key (symbol->string k)
                                     'content s)]
        [(list 'hole b)      (hasheq 'kind "hole"
                                     'type (type->string (node-spec n))
                                     'brief (or b ""))]
        [(cons tag cs)       (hasheq 'kind (symbol->string tag)
                                     'children (map go cs))]))
    (if ids? (hash-set base 'id (node-id n)) base))
  (go n))

;; Pull the first balanced JSON object out of the reply. Models sometimes
;; under-close deep nesting; if the text ends outside a string with braces
;; still open, append the missing closers and try (the typechecker remains
;; the real validator of what parses).
(define (extract-json str)
  (define (try s) (with-handlers ([exn:fail? (λ (_) #f)]) (string->jsexpr s)))
  (define start (for/first ([c (in-string str)] [i (in-naturals)]
                            #:when (char=? c #\{)) i))
  (and start
       (let loop ([i start] [depth 0] [in-str? #f] [esc? #f])
         (cond
           [(= i (string-length str))
            (and (not in-str?) (> depth 0)
                 (try (string-append (substring str start)
                                     (make-string depth #\}))))]
           [else
            (define c (string-ref str i))
            (cond
              [esc? (loop (add1 i) depth in-str? #f)]
              [(and in-str? (char=? c #\\)) (loop (add1 i) depth #t #t)]
              [(char=? c #\") (loop (add1 i) depth (not in-str?) #f)]
              [in-str? (loop (add1 i) depth #t #f)]
              [(char=? c #\{) (loop (add1 i) (add1 depth) #f #f)]
              [(char=? c #\})
               (if (= depth 1)
                   (try (substring str start (add1 i)))
                   (loop (add1 i) (sub1 depth) #f #f))]
              [else (loop (add1 i) depth #f #f)])]))))

;; --------------------------------------------------------------------------
;; Prompts
;; --------------------------------------------------------------------------
(define SYSTEM-TEMPLATE #<<EOF
You are a COMPONENT of a living web page, written in a tiny typed language.
The page is a tree of components; any component can be prompted to refine
itself. You have just been prompted. Reply with exactly ONE action, as a bare
JSON object — no prose, no code fences.

LEAF COMPONENTS (types in parentheses):
  {"kind":"text","runs":[R…]}                      (Text)
      R is "plain string" or {"text":"…","marks":[…],"href":"url"}.
      marks are keywords from: ~a. href makes the run a link.
      {"kind":"text","text":"…"} is sugar for one plain run.
  {"kind":"heading","level":1-6,"runs":[R…]}       (Heading; level 1 = page title;
                                                    "text" sugar works here too)
  {"kind":"button","label":"…"}                    (Button)
  {"kind":"embed","media":M,"url":"…","alt":"…"}   ((Embed M)) — M is a keyword:
      ~a   (Page = an embedded external page)
      For Image, use a loadable placeholder URL:
      https://picsum.photos/seed/ANY-WORD/800/400 (vary the seed word and size).
  {"kind":"input","value":V,"label":"…"}           ((Input V)) — V is a keyword:
      ~a,
      or {"oneOf":["a","b",…]} (radios) | {"manyOf":["a","b",…]} (checkboxes)
      | {"range":[min,max]} (slider).
      (Bool renders as a checkbox, LongString as a textarea.)
  {"kind":"divider"}                               (Divider — horizontal rule)
  {"kind":"title","text":"…"}                      (Title — browser tab title;
                                                    place anywhere, renders in chrome)
  {"kind":"meta","key":K,"content":"…"}            (Meta — invisible; K keyword:
                                                    ~a)

CONTAINERS — {"kind":K,"children":[…]}; a bare string child is a text:
  vertical:   stack header footer main section article aside figure blockquote
              list olist item deflist term defn table form fieldset details
  horizontal: row nav trow
  list/olist mark each child with a bullet/number; table children are trows
  whose children are cells; deflist children alternate term/defn; details'
  first child is the always-visible summary. The TYPE of a container is its
  capitalized kind, and semantics lives in the type: Nav <: Row, Olist <: List
  <: Stack, Header/Footer/Section/Article/… <: Stack, Trow <: Row,
  (Embed Image) <: Embed, (Input String) <: Input; everything <: Component.

SPECIAL FORMS:
  {"kind":"hole","type":"T","brief":"…"}    a typed TODO that will be prompted
      to fill itself right after you answer; the brief says what it is for
  {"kind":"ref","id":"cX"}                  keep existing component cX here,
      unchanged (preserves identity — prefer for children you are not changing)

ACTIONS (reply with exactly one):
  {"response":{"action":"refine","component":C}}
      Rewrite yourself as C. C must be a SUBTYPE of your type contract; the
      typechecker verifies and rejects violations. For large jobs return a
      SKELETON: real structure now, briefed holes for parts that should build
      themselves later. Decompose at most ONE level per reply — a page into
      sections; if your brief already describes a single section or less,
      write its concrete content, no holes.
      Make pages look like REAL websites, using the full vocabulary where it
      fits: title for the tab; header with a nav of buttons; row for
      side-by-side layout; embed Image for pictures; list/olist/table for
      enumerable content; form with typed inputs for data entry; divider and
      footer to close. Prose-only stacks of text are dull — vary structure.
  {"response":{"action":"delegate","child":"cX","instruction":"…"}}
      The change belongs inside one of your children: route it there with a
      precise sub-instruction.
  {"response":{"action":"escalate","reason":"…","suggestion":"…"}}
      You cannot satisfy the instruction WITHIN YOUR CONTRACT, but your parent
      could by restructuring around you. Say why, and suggest how.
  {"response":{"action":"refuse","reason":"…"}}
      The instruction should not be done by ANYONE (deceptive or harmful).
      Never refuse merely because your own contract forbids it — a legitimate
      instruction that is impossible within YOUR contract must be ESCALATED so
      your parent can restructure around you. And never refuse because the
      LANGUAGE lacks a capability — that is a SCREAM.
  {"response":{"action":"scream","diagnosis":"…","proposal":"…"}}
      THE LANGUAGE ITSELF cannot express what was asked — no component kind,
      keyword, or restructuring anywhere in the tree could faithfully do it.
      Diagnose the missing capability precisely and propose a concrete language
      extension (a new kind, mark, media kind, value type, or container role).

THE WRAP PATTERN (important): to ADD something next to / around your current
content, replace yourself with a container that keeps you via a ref and adds
the new part. Example — you are heading c1 "Max's Burgers", instruction is
"add a button that says CLICK ME":
  {"response":{"action":"refine","component":
    {"kind":"row","children":[{"kind":"ref","id":"c1"},
                              {"kind":"button","label":"CLICK ME"}]}}}
Use stack to add above/below, row to add beside.

Honesty rules:
  - NEVER approximate. If you cannot faithfully do what was asked: delegate,
    escalate, refuse, or scream. A cosmetic edit that pretends is the worst
    possible answer. Do not change your kind, level, or text unless the
    instruction requires it.
  - Never return yourself unchanged, and never reply with just a bare hole/ref.
  - Do no MORE than asked; keep unrelated content via refs.
  - Total rewrites are LEGITIMATE: if asked to become something entirely
    different, replace your content wholesale (decomposing into briefed holes).
    Scale of change is never a reason to refuse — only deception, harm, or
    genuine impossibility are.
EOF
)

(define (words-line cat) (string-join (map symbol->string (lexicon-words cat)) " "))
(define (page-system)
  (format SYSTEM-TEMPLATE
          (words-line 'Marks) (words-line 'Media)
          (words-line 'Values) (words-line 'Metas)))

(define (user-prompt root target contract instruction feedback root? child-ids)
  (define path (path-to (page-of root) (node-id target)))
  (define brief (and (hole-node? target) (hole-brief target)))
  (string-append
   "The page you live in, rendered (read-only context):\n"
   (render-page root) "\n\n"
   "You are component " (node-id target)
   " (path from root: " (string-join (map node-id path) " → ") ").\n"
   "Your current form:\n" (jsexpr->string (node->json target #:ids? #t)) "\n\n"
   "Your type contract (a refinement must be a subtype of this): "
   (type->string contract) "\n"
   (if brief (string-append "Your brief (what you are for): " brief "\n") "")
   "Actions available to you: refine"
   (if (null? child-ids)
       ""
       (string-append ", delegate (children: " (string-join child-ids ", ") ")"))
   (if root? "" ", escalate")
   ", refuse, scream."
   (if root? " You are the root: there is no parent to escalate to." "")
   "\n\nInstruction: " instruction "\n"
   (if (string=? feedback "")
       ""
       (string-append "\nFeedback on your previous attempt: " feedback "\n"))))

(define (clip s [n 200])
  (if (> (string-length s) n) (string-append (substring s 0 n) "…") s))

;; --------------------------------------------------------------------------
;; Extending the language: prompting a words node. The model returns a plain
;; word list; the subtype relation ((Words C ws) <: (Words C vs) iff vs ⊆ ws)
;; makes removal of contract words a type error. Evolution can only add.
;; --------------------------------------------------------------------------
(define VOCAB-SYSTEM #<<EOF
You are one CATEGORY OF THE VOCABULARY of a tiny typed web language — a set of
keywords the language's components may use. You have been asked to change.
Reply with exactly ONE action as a bare JSON object — no prose, no code fences.

  {"response":{"action":"refine","words":["w1","w2",…]}}
      The complete new word list. It must include every word your contract
      guarantees (the typechecker rejects removals); new words extend the
      language. Keep house style: Marks are lowercase (em, strong); Media,
      Values, and Metas are Capitalized (Image, String, Author).
  {"response":{"action":"refuse","reason":"…"}}
      The requested word should not exist (deceptive or incoherent).
  {"response":{"action":"scream","diagnosis":"…","proposal":"…"}}
      The request is not satisfiable by adding words to your category (e.g. it
      needs new rendering machinery or a new component kind).

Note: a newly added word is immediately legal everywhere, but renders with the
DEFAULT for its category (marks render plain, value types as a text field)
until a renderer is taught about it. Add words anyway — meaning first.
EOF
)

(define (vocab-prompt-once root target instruction emit tries)
  (define id (node-id target))
  (define contract (node-spec target))
  (define cat (words-cat target))
  (let loop ([k 1] [feedback ""])
    (define umsg
      (string-append
       "You are " id ", the " (symbol->string cat) " category.\n"
       "Current words: " (string-join (map symbol->string (words-list target)) " ") "\n"
       "Guaranteed by contract (cannot be removed): "
       (string-join (map symbol->string (cddr contract)) " ") "\n"
       "\nInstruction: " instruction "\n"
       (if (string=? feedback "") "" (string-append "\nFeedback: " feedback "\n"))))
    (emit 'request (hasheq 'attempt k 'tries tries 'target id
                           'contract (type->string contract)
                           'instruction instruction
                           'system (if (= k 1) VOCAB-SYSTEM #f) 'user umsg))
    (define content (grok-complete VOCAB-SYSTEM umsg))
    (emit 'response (hasheq 'attempt k 'content content))
    (define j (extract-json content))
    (define r (and (hash? j) (hash-ref j 'response #f)))
    (define (retry msg note)
      (if (< k tries) (loop (add1 k) msg) (list 'gave-up note)))
    (cond
      [(not (hash? r)) (retry "Not a valid {\"response\":…} object." "invalid output")]
      [else
       (case (hash-ref r 'action #f)
         [("refine")
          (define ws (hash-ref r 'words '()))
          (cond
            [(not (and (list? ws) (pair? ws) (andmap (λ (w) (and (string? w) (non-empty-string? w))) ws)))
             (retry "words must be a non-empty list of keyword strings" "invalid words")]
            [else
             (define syms (remove-duplicates (map string->symbol ws)))
             (define new-type (list* 'Words cat syms))
             (cond
               [(equal? (sort syms symbol<?) (sort (words-list target) symbol<?))
                (emit 'typecheck (hasheq 'attempt k 'status 'no-op))
                (retry "You returned the same word set unchanged." "no-op")]
               [(subtype? new-type contract)
                (emit 'typecheck (hasheq 'attempt k 'status 'ok
                                         'wtype (type->string new-type)
                                         'contract (type->string contract)))
                (define fixed (struct-copy node target [body (list* 'words cat syms)]))
                (list 'refined (replace-node root id fixed)
                      (format "~a ⊑ ~a — the language grew" (type->string new-type)
                              (type->string contract)))]
               [else
                (emit 'typecheck (hasheq 'attempt k 'status 'rejected
                                         'reason "removed contract-guaranteed words"))
                (retry "REJECTED: you removed words the contract guarantees. Only additions are legal."
                       "gave up: tried to shrink the language")])])]
         [("refuse")  (list 'refused (hash-ref r 'reason ""))]
         [("scream")  (list 'screamed (hash-ref r 'diagnosis "") (hash-ref r 'proposal ""))]
         [else (retry "Unknown action; refine, refuse, or scream." "unknown action")])])))

;; --------------------------------------------------------------------------
;; One prompted turn of one component.
;; --------------------------------------------------------------------------
(define (prompt-once root id instruction #:tries [tries 3] #:emit [emit void]
                     #:allow-holes? [allow-holes? #t])
  (parameterize ([current-lexicon (lexicon-hash root)])
    (prompt-once* root id instruction tries emit allow-holes?)))

(define (prompt-once* root id instruction tries emit allow-holes?)
  (define target (find-node root id))
  (unless target (error 'prompt "no node with id ~a" id))
  (when (system-node? target)
    (error 'prompt "~a is a system node (store/journal/entry) — it cannot be prompted" id))
  (cond
    [(words-node? target) (vocab-prompt-once root target instruction emit tries)]
    [else (page-prompt-once root target instruction tries emit allow-holes?)]))

(define (page-prompt-once root target instruction tries emit allow-holes?)
  (define id (node-id target))
  (define contract (node-spec target))
  (define child-ids (map node-id (node-children target)))
  (define root? (string=? id (node-id (page-of root))))
  (define index (for/hash ([n (in-list (all-nodes target))]) (values (node-id n) n)))
  (define system (page-system))
  (let loop ([k 1] [feedback ""])
    (define umsg (user-prompt root target contract instruction feedback root? child-ids))
    (emit 'request (hasheq 'attempt k 'tries tries 'target id
                           'contract (type->string contract)
                           'instruction instruction
                           'system (if (= k 1) system #f) 'user umsg))
    (define content (grok-complete system umsg))
    (emit 'response (hasheq 'attempt k 'content content))
    (define j (extract-json content))
    (define r (and (hash? j) (hash-ref j 'response #f)))
    (define (retry msg note)
      (if (< k tries) (loop (add1 k) msg) (list 'gave-up note)))
    (cond
      [(not (hash? r))
       (retry (format "Your output was not a valid {\"response\":…} object. Raw: ~a"
                      (clip content))
              "invalid output")]
      [else
       (case (hash-ref r 'action #f)
         [("refine")
          (define used (make-hash))
          (define parsed          ; node on success, error string on failure
            (with-handlers ([exn:fail? (λ (e) (exn-message e))])
              (parse (json->surface (hash-ref r 'component #f) index used) contract)))
          (cond
            [(string? parsed)
             (emit 'typecheck (hasheq 'attempt k 'status 'invalid 'reason parsed))
             (retry (format "Invalid component: ~a" parsed) "invalid component")]
            [(equal? (node->surface parsed) (node->surface target))
             (emit 'typecheck (hasheq 'attempt k 'status 'no-op))
             (retry "You returned yourself unchanged. If you cannot comply, escalate, refuse, or scream."
                    "no-op: model made no change")]
            [(hole-node? parsed)
             (emit 'typecheck (hasheq 'attempt k 'status 'no-op))
             (retry "Replacing yourself with a bare hole is not progress."
                    "no-op: bare hole")]
            [(and (not allow-holes?)
                  (for/or ([n (in-list (all-nodes parsed))])
                    (and (hole-node? n) (not (hash-has-key? index (node-id n))))))
             (emit 'typecheck (hasheq 'attempt k 'status 'rejected
                                      'reason "created new holes where concrete content is required"))
             (retry "Do not create new holes: your brief is narrow enough to write the concrete final content now."
                    "gave up: kept decomposing instead of writing content")]
            [(subtype? (type-of parsed) contract)
             (emit 'typecheck (hasheq 'attempt k 'status 'ok
                                      'wtype (type->string (type-of parsed))
                                      'contract (type->string contract)))
             (define fixed (if (hash-ref used id #f)
                               parsed
                               (struct-copy node parsed [id id])))
             (list 'refined (replace-node root id fixed)
                   (format "~a ⊑ ~a" (type->string (type-of parsed))
                           (type->string contract)))]
            [else
             (define why (format "~a is not a subtype of ~a"
                                 (type->string (type-of parsed))
                                 (type->string contract)))
             (emit 'typecheck (hasheq 'attempt k 'status 'rejected 'reason why))
             (retry (format "REJECTED by the typechecker: ~a. Either return a subtype of ~a, or — if the instruction cannot be satisfied within your contract — ESCALATE so your parent can restructure around you."
                            why (type->string contract))
                    (string-append "gave up: " why))])]
         [("delegate")
          (define child (hash-ref r 'child #f))
          (if (member child child-ids)
              (list 'delegate child (hash-ref r 'instruction ""))
              (retry "delegate: child must be one of your immediate children."
                     "bad delegate target"))]
         [("escalate")
          (if root?
              (retry "You are the root; there is no parent. Refine, refuse, or scream."
                     "root tried to escalate")
              (list 'escalate (hash-ref r 'reason "") (hash-ref r 'suggestion "")))]
         [("refuse")  (list 'refused (hash-ref r 'reason ""))]
         [("scream")  (list 'screamed (hash-ref r 'diagnosis "") (hash-ref r 'proposal ""))]
         [else (retry (format "Unknown action ~s." (hash-ref r 'action #f))
                      "unknown action")])])))

;; --------------------------------------------------------------------------
;; The conversation driver: delegate hops down; escalate hops up;
;; refine/refuse/scream terminate. Every terminal outcome is journaled into
;; the store (when the root IS a store) — the program keeps its own history,
;; written here and nowhere else.
;; --------------------------------------------------------------------------
(define (dispatch root id instruction #:hops [hops 4] #:tries [tries 3] #:emit [emit void]
                  #:allow-holes? [allow-holes? #t])
  (define original instruction)              ; journal the user's ask, not hop rewrites
  (define (journaled o kind notes)
    (cond
      [(store? (outcome-root o))
       (emit 'journal (hasheq 'kind kind 'at (outcome-at o)))
       (struct-copy outcome o
                    [root (journal-append (outcome-root o) kind (outcome-at o)
                                          original notes)])]
      [else o]))
  (let loop ([id id] [instruction instruction] [budget hops])
    (match (prompt-once root id instruction #:tries tries #:emit emit
                        #:allow-holes? allow-holes?)
      [(list 'refined new-root note)
       (journaled (outcome 'refined new-root id note) 'Refined (list note))]
      [(list 'delegate child sub)
       (emit 'hop (hasheq 'kind 'delegate 'from id 'to child 'message sub))
       (if (zero? budget)
           (journaled (outcome 'gave-up root id "hop budget exhausted")
                      'GaveUp (list "hop budget exhausted"))
           (loop child sub (sub1 budget)))]
      [(list 'escalate reason suggestion)
       (define parent (find-parent root id))
       (when (or (not parent) (system-node? parent))
         (error 'dispatch "~a escalated but has no promptable parent" id))
       (emit 'hop (hasheq 'kind 'escalate 'from id 'to (node-id parent)
                          'message (format "~a — suggestion: ~a" reason suggestion)))
       (if (zero? budget)
           (journaled (outcome 'gave-up root id "hop budget exhausted")
                      'GaveUp (list "hop budget exhausted"))
           (loop (node-id parent)
                 (format "Your child ~a was asked: ~s. It escalated to you — reason: ~a Suggestion: ~a. Satisfy the original request by restructuring; keep ~a itself via a ref where appropriate."
                         id instruction reason suggestion id)
                 (sub1 budget)))]
      [(list 'refused reason)
       (journaled (outcome 'refused root id reason) 'Refused (list reason))]
      [(list 'screamed diag prop)
       (journaled (outcome 'screamed root id (hasheq 'diagnosis diag 'proposal prop))
                  'Screamed (list diag prop))]
      [(list 'gave-up note)
       (journaled (outcome 'gave-up root id note) 'GaveUp (list note))])))

;; --------------------------------------------------------------------------
;; The cascade: holes fill themselves breadth-first until none remain. Depth
;; decides whether a fill may decompose further — page (0) and section (1)
;; level may spawn briefed holes; anything deeper must write concrete content.
;; Emits 'fill-step before each fill; `on-refined` fires with each new root
;; (render your frame there); `on-blocked` with each hole that refused/
;; screamed/gave up. `skip` holds pre-existing hole ids (deliberate TODOs).
;; Returns (values final-root summary) with summary keys: status
;; ('complete|'budget), steps, remaining.
;; --------------------------------------------------------------------------
(define (fill-instruction brief allow-holes?)
  (if allow-holes?
      (format "Fill yourself according to your brief. If the brief spans several distinct sections, return structure with briefed holes so each section can build itself. Brief: ~a" brief)
      (format "Write the concrete final content for your brief now: real text/headings/buttons only, NO new holes. Brief: ~a" brief)))

;; --------------------------------------------------------------------------
;; `query` — the read dual of prompt. Recursive in the RLM sense: a queried
;; component sees only a SHALLOW view (its own content, or its children as
;; id:type index lines), answers if it can, or asks specific children; their
;; answers come back and it synthesizes. The tree is navigated, never pasted.
;; Reads never mutate and are not journaled.
;; --------------------------------------------------------------------------
(define SYSTEM-QUERY #<<EOF
You are a COMPONENT of a web page, asked a QUESTION about your contents. You
see only a shallow view: your own content if you are a leaf, or an index of
your children (id : type — summary) if you are a container. Reply with exactly
ONE action as a bare JSON object — no prose, no code fences.

  {"response":{"action":"answer","text":"…"}}
      Answer the question from what you can see. If the answer is genuinely
      not in your subtree, say so plainly — that is a correct answer.
  {"response":{"action":"ask","queries":[{"child":"cX","question":"…"},…]}}
      You cannot answer from the shallow view alone: ask up to 3 children a
      precise sub-question each. Their answers will be returned to you, then
      you answer. Ask only children whose TYPE suggests they are relevant.

Never guess content you cannot see. Types are your map: a (Section (Heading)
(Text)) child is prose; (Nav (Button)…) is navigation; (Form (Input …)…) is
data entry.
EOF
)

(define (shallow-view target)
  (if (null? (node-children target))
      (string-append "Your content:\n" (jsexpr->string (node->json target #:ids? #t)))
      (string-append
       "You are a " (symbol->string (car (node-body target)))
       " with children:\n"
       (string-join
        (for/list ([c (in-list (node-children target))])
          (format "  ~a : ~a — ~a" (node-id c) (type->string (type-of c))
                  (body-label (node-body c))))
        "\n"))))

(define (query-user target question sub-answers final?)
  (string-append
   "You are component " (node-id target)
   ", type " (type->string (type-of target)) ".\n"
   (shallow-view target) "\n"
   (if (null? sub-answers)
       ""
       (string-append "\nAnswers from children you asked:\n"
                      (string-join
                       (for/list ([a (in-list sub-answers)])
                         (format "  ~a: ~a" (car a) (cdr a)))
                       "\n")
                      "\n"))
   (if final? "\nYou must answer now; asking is no longer available.\n" "")
   "\nQuestion: " question))

;; Ask one node; recurse on ask-actions. Returns the answer string.
;; depth: remaining recursion levels; a node gets `rounds` ask-rounds.
(define (query-node root id question
                    #:emit [emit void] #:depth [depth 4] #:tries [tries 2])
  (define target (find-node root id))
  (unless target (error 'query "no node with id ~a" id))
  (when (system-node? target)
    (error 'query "~a is a system node and cannot be queried" id))
  (define children (node-children target))
  (let round ([sub-answers '()] [rounds 2] [k 1])
    (define final? (or (null? children) (zero? depth) (zero? rounds)))
    (emit 'query-request (hasheq 'target id 'question question 'depth depth))
    (define content
      (grok-complete SYSTEM-QUERY (query-user target question sub-answers final?)))
    (emit 'query-response (hasheq 'target id 'content content))
    (define j (extract-json content))
    (define r (and (hash? j) (hash-ref j 'response #f)))
    (define (retry note)
      (if (< k tries)
          (round sub-answers rounds (add1 k))
          (format "(~a: ~a)" id note)))
    (cond
      [(not (hash? r)) (retry "no valid answer")]
      [else
       (case (hash-ref r 'action #f)
         [("answer")
          (define text (hash-ref r 'text ""))
          (emit 'answer (hasheq 'target id 'text text))
          text]
         [("ask")
          (cond
            [final? (retry "asked when it had to answer")]
            [else
             (define asks
               (for/list ([q (in-list (hash-ref r 'queries '()))]
                          [_ (in-range 3)]                    ; fanout cap
                          #:when (and (hash? q)
                                      (member (hash-ref q 'child #f)
                                              (map node-id children))))
                 (cons (hash-ref q 'child) (hash-ref q 'question ""))))
             (cond
               [(null? asks) (retry "asked no valid child")]
               [else
                (define answers
                  (for/list ([a (in-list asks)])
                    (emit 'query-ask (hasheq 'from id 'to (car a)
                                             'question (cdr a)))
                    (cons (car a)
                          (query-node root (car a) (cdr a)
                                      #:emit emit #:depth (sub1 depth)
                                      #:tries tries))))
                (round (append sub-answers answers) (sub1 rounds) 1)])])]
         [else (retry "unknown action")])])))

(define (cascade root #:emit [emit void] #:max-steps [max-steps 12]
                 #:skip [skip '()]
                 #:on-refined [on-refined void] #:on-blocked [on-blocked void])
  (let loop ([root root] [step 1] [blocked skip])
    (define hs (filter (λ (h) (not (member (node-id h) blocked))) (holes-bfs root)))
    (cond
      [(null? hs)
       (values root (hasheq 'status 'complete 'steps (sub1 step) 'remaining 0))]
      [(> step max-steps)
       (values root (hasheq 'status 'budget 'steps (sub1 step)
                            'remaining (length hs)))]
      [else
       (define h (car hs))
       (define depth (sub1 (length (path-to (page-of root) (node-id h)))))
       (define allow? (< depth 2))
       (define brief (or (hole-brief h) "make something reasonable for this page"))
       (emit 'fill-step (hasheq 'step step 'hole (node-id h)
                                'depth depth 'brief brief
                                'remaining (length hs)))
       (define r (dispatch root (node-id h) (fill-instruction brief allow?)
                           #:emit emit #:allow-holes? allow?))
       (cond
         [(eq? (outcome-kind r) 'refined)
          (on-refined (outcome-root r) r)
          (loop (outcome-root r) (add1 step) blocked)]
         [else
          (on-blocked h r)
          ;; adopt the outcome root anyway: the page is unchanged but the
          ;; journal now records the refusal/scream/give-up
          (loop (outcome-root r) (add1 step) (cons (node-id h) blocked))])])))
