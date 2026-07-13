#lang racket/base
;; token-rich-types — core (v0.3: HTML-parity minimal basis)
;;
;; A program is a tree of NODES: {stable short id, spec (type contract), body}.
;; The only write is REFINEMENT: replace a node's body with one whose tightest
;; type is a SUBTYPE of the spec. Contracts default to Component; `pin-node`
;; tightens one deliberately (spec authoring).
;;
;; Design rules:
;;   - NO MAGIC STRINGS: every enumeration (marks, media kinds, value types,
;;     meta keys, container roles) is a first-class symbol validated against a
;;     table below. Strings hold only user content (labels, prose, urls).
;;   - NO FORCED PLANNING: bare strings coerce to text anywhere; semantic roles
;;     are head-words you say instead of `stack`; metadata like (title …) may
;;     appear anywhere in the tree, whenever you think of it.

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
  (string-append "c" (->base36 n)))

;; ---------------------------------------------------------------------------
;; Vocabulary — the words of the language, as keyword sets by category. These
;; are the BASE sets; a store carries its own live copy in its LEXICON, which
;; components may extend (never shrink — the subtype relation enforces it).
;; Validators read `current-lexicon`, so the active language is the store's.
;; ---------------------------------------------------------------------------
(define VOCAB-CATEGORIES '(Marks Media Values Metas))

(define BASE-LEXICON
  (hasheq 'Marks  '(em strong code mark small sub sup del ins kbd time abbr q cite pre)
          'Media  '(Image Video Audio Page)        ; Page = embedded page (iframe)
          'Values '(String LongString Number Bool Date Time Color Password File
                    Email Tel Url Search)
          'Metas  '(Description Author Canonical Keywords)))

(define current-lexicon (make-parameter BASE-LEXICON))
(define (lexicon-words cat) (hash-ref (current-lexicon) cat))

(define (valid-mark? m)     (and (memq m (lexicon-words 'Marks)) #t))
(define (valid-media? m)    (and (memq m (lexicon-words 'Media)) #t))
(define (valid-meta-key? k) (and (memq k (lexicon-words 'Metas)) #t))
(define (valid-value-type? v)
  (or (and (memq v (lexicon-words 'Values)) #t)
      (match v
        [(cons (or 'OneOf 'ManyOf) (and opts (cons _ _)))   ; options are content
         (andmap string? opts)]
        [(list 'Range (? real? lo) (? real? hi)) (< lo hi)]
        [_ #f])))

;; Containers: surface tag, type name, layout axis, supertype.
;; The role IS the head-word — (nav …) is what you say instead of (stack …)
;; when you mean navigation; the meaning lives in the TYPE (Nav <: Row).
(define CONTAINERS
  '((stack      Stack      v #f)
    (row        Row        h #f)
    (header     Header     v Stack)
    (footer     Footer     v Stack)
    (main       Main       v Stack)
    (nav        Nav        h Row)
    (section    Section    v Stack)
    (article    Article    v Stack)
    (aside      Aside      v Stack)
    (figure     Figure     v Stack)
    (blockquote Blockquote v Stack)
    (list       List       v Stack)
    (olist      Olist      v List)
    (item       Item       v Stack)
    (deflist    Deflist    v Stack)
    (term       Term       v Stack)
    (defn       Defn       v Stack)
    (table      Table      v Stack)
    (trow       Trow       h Row)
    (form       Form       v Stack)
    (fieldset   Fieldset   v Stack)
    (details    Details    v Stack)))

(define (container-entry tag) (and (symbol? tag) (assq tag CONTAINERS)))
(define (container-tag? tag) (and (container-entry tag) #t))
(define (container-type tag) (cadr (container-entry tag)))
(define (container-axis tag) (caddr (container-entry tag)))
(define CONTAINER-TYPES (map cadr CONTAINERS))

(define LEAF-TYPES '(Component Text Heading Button Embed Input Divider Title Meta))
(define ALL-TYPES (append LEAF-TYPES CONTAINER-TYPES))

;; Store-side types: the program's container for itself, its history, and its
;; language. Deliberately NOT in ALL-TYPES — the model cannot name them in page
;; content (no holes of type Entry, no fabricated journals or lexicons).
(define STORE-TYPES '(Store Journal Entry Lexicon Words))
(define ENTRY-KINDS '(Refined Refused Screamed GaveUp))
(define (valid-entry-kind? k) (and (memq k ENTRY-KINDS) #t))

;; The subtype lattice's edges: child type -> parent type.
(define TYPE-PARENT
  (for/fold ([h (hasheq 'Heading 'Text)])
            ([c (in-list CONTAINERS)])
    (if (cadddr c) (hash-set h (cadr c) (cadddr c)) h)))

;; ---------------------------------------------------------------------------
;; Nodes and bodies
;;
;; body ::= (text    (run …))            ; rich text: a sequence of runs
;;        | (heading Level (run …))      ; Level ∈ 1..6
;;        | (button  Label)
;;        | (embed   Media Url Alt)      ; Media ∈ MEDIA-KINDS
;;        | (input   ValueType Label)    ; ValueType ∈ VALUE-TYPES | (OneOf …)
;;        | (divider)
;;        | (title   String)             ; page title (renders in chrome)
;;        | (meta    Key String)         ; Key ∈ META-KEYS (invisible)
;;        | (tag     Node …)             ; tag ∈ CONTAINERS
;;        | (hole    Brief|#f)           ; typed TODO; contract = node-spec
;;
;; A run is a string with a set of MARKS and an optional link target.
;; ---------------------------------------------------------------------------
(struct node (id spec body) #:transparent)
(struct run (str marks href) #:transparent)

(define (level? n) (and (exact-integer? n) (<= 1 n 6)))

(define (valid-type? t)
  (cond
    [(symbol? t) (and (or (memq t ALL-TYPES) (memq t STORE-TYPES)) #t)]
    [(pair? t)
     (case (car t)
       [(Embed) (and (= (length t) 2) (valid-media? (cadr t)))]
       [(Input) (and (= (length t) 2) (valid-value-type? (cadr t)))]
       [(Entry) (and (= (length t) 2) (valid-entry-kind? (cadr t)))]
       [(Words) (and (>= (length t) 2) (memq (cadr t) VOCAB-CATEGORIES)
                     (andmap symbol? (cddr t)))]
       [else (and (memq (car t) (append CONTAINER-TYPES '(Store Journal)))
                  (andmap valid-type? (cdr t)))])]
    [else #f]))

;; Run forms: "plain" | (mark run-form …) | (link url run-form …).
;; Marks compose by nesting; flattened here into runs carrying mark sets.
(define (parse-runs forms)
  (define (go f marks href)
    (match f
      [(? string? s) (list (run s (remove-duplicates (reverse marks)) href))]
      [(cons 'link (cons (? string? url) rest))
       (append-map (λ (x) (go x marks url)) rest)]
      [(cons (? valid-mark? m) rest)
       (append-map (λ (x) (go x (cons m marks) href)) rest)]
      [_ (error 'parse "not a valid text run: ~s" f)]))
  (append-map (λ (f) (go f '() #f)) forms))

;; Parse a surface datum into a node tree, minting ids. `spec` pins the top
;; node's contract (used when refining: the contract persists). An embedded
;; node passes through untouched — that is how REFS keep subtrees alive.
;; A bare string anywhere is a text component (no ceremony).
(define (parse datum [spec #f])
  ;; An embedded node (a resolved REF) passes through untouched. Otherwise the
  ;; id is minted BEFORE recursing into children, so ids are parent-first: a
  ;; parsed tree's root always has the lowest id.
  (cond
    [(node? datum) datum]
    [else
     (define nid (fresh-id))
     (define (mk body) (node nid (or spec 'Component) body))
     (match datum
       [(? string? s) (mk (list 'text (list (run s '() #f))))]
       [(cons 'text rs) (mk (list 'text (parse-runs rs)))]
       [(list* 'heading (? level? l) rs) (mk (list 'heading l (parse-runs rs)))]
       [(list 'button (? string? s)) (mk (list 'button s))]
       [(list 'embed (? valid-media? m) (? string? url) (? string? alt))
        (mk (list 'embed m url alt))]
       [(list 'input (? valid-value-type? vt) (? string? label))
        (mk (list 'input vt label))]
       [(list 'divider) (mk (list 'divider))]
       [(list 'title (? string? s)) (mk (list 'title s))]
       [(list 'meta (? valid-meta-key? k) (? string? s)) (mk (list 'meta k s))]
       [(list 'hole T) #:when (valid-type? T)
        (node nid T (list 'hole #f))]
       [(list 'hole T (? string? brief)) #:when (valid-type? T)
        (node nid T (list 'hole brief))]
       [(cons (? container-tag? tag) children)
        (mk (cons tag (map (λ (c) (parse c)) children)))]
       [_ (error 'parse "not a valid component: ~s" datum)])]))

;; Inverse of parse (drops ids/specs; reconstructs run sugar).
(define (run->surface r)
  (define marked
    (for/fold ([x (run-str r)]) ([m (in-list (reverse (run-marks r)))])
      (list m x)))
  (if (run-href r) (list 'link (run-href r) marked) marked))

(define (node->surface n)
  (match (node-body n)
    [(list 'text runs)      (cons 'text (map run->surface runs))]
    [(list 'heading l runs) (list* 'heading l (map run->surface runs))]
    [(list 'button s)       (list 'button s)]
    [(list 'embed m u a)    (list 'embed m u a)]
    [(list 'input vt l)     (list 'input vt l)]
    [(list 'divider)        (list 'divider)]
    [(list 'title s)        (list 'title s)]
    [(list 'meta k s)       (list 'meta k s)]
    [(list 'hole #f)        (list 'hole (node-spec n))]
    [(list 'hole b)         (list 'hole (node-spec n) b)]
    [(cons tag cs)          (cons tag (map node->surface cs))]))

;; ---------------------------------------------------------------------------
;; Types and subtyping
;;
;; type ::= <name in ALL-TYPES> | (ContainerType type …)
;;        | (Embed Media) | (Input ValueType)
;; Lattice edges are TYPE-PARENT (e.g. Heading<:Text, Nav<:Row, Olist<:List);
;; everything <: Component; compounds are covariant and <: their bare head.
;; ---------------------------------------------------------------------------
(define (type-of n)
  (match (node-body n)
    [(list 'text _)      'Text]
    [(list 'heading _ _) 'Heading]
    [(list 'button _)    'Button]
    [(list 'embed m _ _) (list 'Embed m)]
    [(list 'input vt _)  (list 'Input vt)]
    [(list 'divider)     'Divider]
    [(list 'title _)     'Title]
    [(list 'meta _ _)    'Meta]
    [(list 'hole _)      (node-spec n)]
    [(list 'entry k _ _ _) (list 'Entry k)]
    [(list* 'words cat ws) (list* 'Words cat ws)]   ; the word set IS the type
    [(cons 'lexicon cs)  'Lexicon]
    [(cons 'journal cs)  (cons 'Journal (map type-of cs))]
    [(cons 'store cs)    (cons 'Store (map type-of cs))]
    [(cons tag cs)       (cons (container-type tag) (map type-of cs))]))

(define (head-le? a b)
  (or (eq? a b)
      (let ([p (hash-ref TYPE-PARENT a #f)])
        (and p (head-le? p b)))))

(define (subtype? a b)
  (cond
    [(equal? a b) #t]
    [(eq? b 'Component) #t]
    [(and (symbol? a) (symbol? b)) (head-le? a b)]
    [(and (pair? a) (symbol? b)) (head-le? (car a) b)]
    ;; word sets: MORE words is a subtype of FEWER (the contract lists the
    ;; words that must exist; refinement may only add). Evolution law.
    [(and (pair? a) (pair? b) (eq? (car a) 'Words) (eq? (car b) 'Words))
     (and (eq? (cadr a) (cadr b))
          (for/and ([w (in-list (cddr b))]) (and (memq w (cddr a)) #t)))]
    [(and (pair? a) (pair? b))
     (and (head-le? (car a) (car b))
          (= (length (cdr a)) (length (cdr b)))
          (andmap subtype? (cdr a) (cdr b)))]
    [else #f]))

(define (type->string t)
  (cond
    [(symbol? t) (symbol->string t)]
    [(and (pair? t) (eq? (car t) 'Words))       ; abbreviate word sets
     (format "(Words ~a ·~a)" (cadr t) (length (cddr t)))]
    [(pair? t) (format "(~a ~a)" (car t)
                       (string-join (map type->string (cdr t)) " "))]
    [else (format "~s" t)]))

;; ---------------------------------------------------------------------------
;; Navigation / functional update
;; ---------------------------------------------------------------------------
(define (node-children n)
  (match (node-body n)
    [(cons (? container-tag?) cs) cs]
    [(cons (or 'store 'journal 'lexicon) cs) cs]
    [_ '()]))       ; note: a words node's symbols are content, not children

(define (hole-node? n)
  (match (node-body n) [(list 'hole _) #t] [_ #f]))
(define (hole-brief n)
  (match (node-body n) [(list 'hole b) b] [_ #f]))
(define (title-node? n)
  (match (node-body n) [(list 'title _) #t] [_ #f]))

(define (find-node n id)
  (if (string=? (node-id n) id)
      n
      (for/or ([c (in-list (node-children n))]) (find-node c id))))

(define (find-parent n id)
  (or (for/first ([c (in-list (node-children n))]
                  #:when (string=? (node-id c) id))
        n)
      (for/or ([c (in-list (node-children n))]) (find-parent c id))))

(define (path-to n id)
  (if (string=? (node-id n) id)
      (list n)
      (for/or ([c (in-list (node-children n))])
        (define p (path-to c id))
        (and p (cons n p)))))

(define (all-nodes n) (cons n (append-map all-nodes (node-children n))))

;; Holes in breadth-first order — the fill cascade runs top-down.
(define (holes-bfs n)
  (let loop ([q (list n)] [acc '()])
    (match q
      ['() (reverse acc)]
      [(cons x rest)
       (loop (append rest (node-children x))
             (if (hole-node? x) (cons x acc) acc))])))

(define (replace-node n id new-node)
  (if (string=? (node-id n) id)
      new-node
      (match (node-body n)
        [(cons (and tag (or (? container-tag?) 'store 'journal 'lexicon)) cs)
         (struct-copy node n
                      [body (cons tag
                                  (map (λ (c) (replace-node c id new-node)) cs))])]
        [_ n])))

;; Tighten a node's contract — spec authoring. The current witness must
;; already satisfy the new contract.
(define (pin-node root id T)
  (define n (find-node root id))
  (unless n (error 'pin "no node ~a" id))
  (unless (valid-type? T) (error 'pin "not a type: ~s" T))
  (unless (subtype? (type-of n) T)
    (error 'pin "cannot pin ~a to ~a: witness type ~a is not a subtype"
           id (type->string T) (type->string (type-of n))))
  (replace-node root id (struct-copy node n [spec T])))

;; ---------------------------------------------------------------------------
;; The store: the program's container for itself. A store node holds the page
;; plus a JOURNAL — the program's memory of its own outcomes (refinements,
;; refusals, screams), written by the enforcement machinery in dispatch and by
;; nothing else. Store/journal/entry have NO surface form and no JSON form:
;; they are not words in the language the model speaks, so a refinement
;; structurally cannot fabricate or edit history.
;;
;; entry body: (entry Kind at-id instruction notes)
;;   Kind ∈ ENTRY-KINDS; notes is a list of strings —
;;   (diagnosis proposal) for Screamed, a single note otherwise.
;; ---------------------------------------------------------------------------
(define (store? n) (match (node-body n) [(cons 'store _) #t] [_ #f]))
;; Store/journal/entry record history and cannot be prompted; the lexicon
;; CONTAINER is fixed too — but its words nodes are promptable: extending the
;; language is an ordinary, typechecked refinement.
(define (system-node? n)
  (match (node-body n) [(cons (or 'store 'journal 'entry 'lexicon) _) #t] [_ #f]))

(define (words-node? n) (match (node-body n) [(cons 'words _) #t] [_ #f]))
(define (words-cat n)  (cadr (node-body n)))
(define (words-list n) (cddr (node-body n)))

(define (make-lexicon)
  (node (fresh-id) 'Lexicon
        (cons 'lexicon
              (for/list ([cat (in-list VOCAB-CATEGORIES)])
                (define ws (hash-ref BASE-LEXICON cat))
                ;; contract = the base word set: refinements must keep it
                (node (fresh-id) (list* 'Words cat ws) (list* 'words cat ws))))))

(define (make-store page)
  (define journal (node (fresh-id) 'Journal (list 'journal)))
  (node (fresh-id) 'Store (list 'store page journal (make-lexicon))))

(define (page-of n)    (if (store? n) (car (node-children n)) n))
(define (journal-of n) (and (store? n) (cadr (node-children n))))
(define (lexicon-of n) (and (store? n) (caddr (node-children n))))

;; The live language of a root: its lexicon if it has one, else the base.
(define (lexicon-hash root)
  (define lex (lexicon-of root))
  (if lex
      (for/hasheq ([w (in-list (node-children lex))])
        (values (words-cat w) (words-list w)))
      BASE-LEXICON))

(define (store-with-page st new-page)
  (struct-copy node st [body (list 'store new-page (journal-of st) (lexicon-of st))]))

(define (journal-entries n)
  (define j (journal-of n))
  (if j (node-children j) '()))

(define (make-entry kind at instruction notes)
  (unless (valid-entry-kind? kind) (error 'journal "not an entry kind: ~s" kind))
  (node (fresh-id) (list 'Entry kind) (list 'entry kind at instruction notes)))
(define (entry-kind n)        (list-ref (node-body n) 1))
(define (entry-at n)          (list-ref (node-body n) 2))
(define (entry-instruction n) (list-ref (node-body n) 3))
(define (entry-notes n)       (list-ref (node-body n) 4))

;; Append an entry to a store's journal; identity on non-store roots (the
;; benchmark drives bare pages and stays history-free).
(define (journal-append root kind at instruction notes)
  (cond
    [(store? root)
     (define j (journal-of root))
     (define j* (struct-copy node j
                             [body (append (node-body j)
                                           (list (make-entry kind at instruction notes)))]))
     (struct-copy node root
                  [body (list 'store (page-of root) j* (lexicon-of root))])]
    [else root]))

;; ---------------------------------------------------------------------------
;; Observation: plain render (the boxed page layout lives in layout.rkt)
;; ---------------------------------------------------------------------------
(define (runs->string runs) (apply string-append (map run-str runs)))

(define (render n [indent 0])
  (define pad (make-string indent #\space))
  (match (node-body n)
    [(list 'text runs)      (string-append pad (runs->string runs))]
    [(list 'heading l runs) (string-append pad (make-string l #\#) " "
                                           (runs->string runs))]
    [(list 'button s)       (string-append pad "[ " s " ]")]
    [(list 'embed m _ a)    (string-append pad (format "[~a: ~a]" m a))]
    [(list 'input vt l)     (string-append pad (format "[~a input: ~a]"
                                                       (type->string vt) l))]
    [(list 'divider)        (string-append pad "----")]
    [(list 'title s)        (string-append pad (format "(title ~s)" s))]
    [(list 'meta k s)       (string-append pad (format "(meta ~a ~s)" k s))]
    [(list 'hole b)         (string-append pad "[ TODO: "
                                           (type->string (node-spec n))
                                           (if b (string-append " — " b) "") " ]")]
    [(list 'entry k at instr notes)
     (string-append pad (format "[~a @~a] ~s — ~a" k at instr
                                (string-join notes " | ")))]
    [(list* 'words cat ws)
     (string-append pad (format "~a: ~a" cat
                                (string-join (map symbol->string ws) " ")))]
    [(cons (or 'journal 'store 'lexicon) cs)
     (if (null? cs)
         (string-append pad "(empty)")
         (string-join (map (λ (c) (render c indent)) cs) "\n"))]
    [(cons tag cs)
     (case (container-axis tag)
       [(v) (string-join (map (λ (c) (render c indent)) cs) "\n")]
       [(h) (string-append pad
                           (string-join (map (λ (c) (render c 0)) cs) "   "))])]))

;; Structural view: ids, witness types, contracts.
(define (truncate-label s [n 40])
  (if (> (string-length s) n) (string-append (substring s 0 n) "…") s))

(define (body-label body)
  (match body
    [(list 'text runs)      (format "text ~s" (truncate-label (runs->string runs)))]
    [(list 'heading l runs) (format "heading ~a ~s" l
                                    (truncate-label (runs->string runs)))]
    [(list 'button s)       (format "button ~s" s)]
    [(list 'embed m u _)    (format "embed ~a ~s" m (truncate-label u 30))]
    [(list 'input vt l)     (format "input ~a ~s" (type->string vt) l)]
    [(list 'divider)        "divider"]
    [(list 'title s)        (format "title ~s" s)]
    [(list 'meta k s)       (format "meta ~a ~s" k (truncate-label s 30))]
    [(list 'entry k at instr _) (format "entry ~a @~a ~s" k at (truncate-label instr))]
    [(list* 'words cat ws)  (format "words ~a ~s" cat
                                    (truncate-label
                                     (string-join (map symbol->string ws) " ")))]
    [(cons tag cs)          (format "~a[~a]" tag (length cs))]
    [(list 'hole #f)        "hole"]
    [(list 'hole b)         (format "hole ~s" (truncate-label b))]))

(define (tree n [indent 0])
  (define pad (make-string indent #\space))
  (define line (format "~a~a  ~a  : ~a  ⊑ ~a"
                       pad (node-id n) (body-label (node-body n))
                       (type->string (type-of n)) (type->string (node-spec n))))
  (if (null? (node-children n))
      line
      (string-join (cons line (map (λ (c) (tree c (+ indent 2)))
                                   (node-children n)))
                   "\n")))
