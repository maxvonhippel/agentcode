#lang racket/base
;; Box-model layout: lay a component tree out like HTML block flow and frame it
;; as an ASCII "screenshot" of a web page.
;;
;; A BLOCK is a list of strings, each EXACTLY the content width W. Invisible
;; components (title, meta) lay out to the empty block and vanish from flow;
;; the page title surfaces in the browser chrome instead.
;; ASCII-safety rule: only width-1-safe characters (no ambiguous-width Unicode).

(require racket/string racket/list racket/match "core.rkt")
(provide render-page page-width)

(define page-width (make-parameter 56))

(define (spaces n) (make-string (max 0 n) #\space))

(define (align-line s w how)
  (define s* (if (> (string-length s) w) (substring s 0 w) s))
  (define extra (- w (string-length s*)))
  (case how
    [(left)   (string-append s* (spaces extra))]
    [(right)  (string-append (spaces extra) s*)]
    [(center) (let ([l (quotient extra 2)])
                (string-append (spaces l) s* (spaces (- extra l))))]))

(define (wrap-words str w)
  (define words (string-split str))
  (if (null? words)
      (list "")
      (let loop ([ws (cdr words)] [cur (car words)] [acc '()])
        (cond
          [(null? ws) (reverse (cons cur acc))]
          [else
           (define cand (string-append cur " " (car ws)))
           (if (<= (string-length cand) w)
               (loop (cdr ws) cand acc)
               (loop (cdr ws) (car ws) (cons cur acc)))]))))

(define (blank W) (list (spaces W)))
(define (vstack blocks) (apply append blocks))

;; ---------------------------------------------------------------------------
;; Rich text: decorate runs into a flat string, then wrap.
;; Decorations are ASCII-only; marks without a visual (time, cite, …) render
;; plain — their meaning lives in the type-checked structure, not the glyphs.
;; ---------------------------------------------------------------------------
(define MARK-DECOR
  (hasheq 'em     '("*" . "*")
          'strong '("**" . "**")
          'code   '("`" . "`")
          'mark   '("__" . "__")
          'del    '("~~" . "~~")
          'ins    '("++" . "++")
          'kbd    '("<" . ">")
          'sub    '("_" . "")
          'sup    '("^" . "")
          'q      '("\"" . "\"")))

(define (decorate-run r)
  (define base
    (for/fold ([x (run-str r)]) ([m (in-list (reverse (run-marks r)))])
      (define d (hash-ref MARK-DECOR m #f))
      (if d (string-append (car d) x (cdr d)) x)))
  (if (run-href r) (string-append "[" base "]") base))

(define (runs->decorated runs) (apply string-append (map decorate-run runs)))

(define (text-lines runs W align)
  (map (λ (l) (align-line l W align)) (wrap-words (runs->decorated runs) W)))

;; ---------------------------------------------------------------------------
;; Leaf blocks
;; ---------------------------------------------------------------------------
(define (heading-block lvl s W)
  (define lines (wrap-words s W))
  (define maxw (apply max (map string-length lines)))
  (cond
    [(= lvl 1)
     (append (map (λ (l) (align-line l W 'center)) lines)
             (list (align-line (make-string (min W maxw) #\═) W 'center)))]
    [(= lvl 2)
     (append (map (λ (l) (align-line l W 'left)) lines)
             (list (align-line (make-string (min W maxw) #\─) W 'left)))]
    [else
     (map (λ (l) (align-line (string-append (make-string lvl #\#) " " l) W 'left))
          lines)]))

(define (button-block s W)
  (define label (string-append "  " s "  "))
  (define n (string-length label))
  (map (λ (l) (align-line l W 'center))
       (list (string-append "╭" (make-string n #\─) "╮")
             (string-append "│" label "│")
             (string-append "╰" (make-string n #\─) "╯"))))

(define (embed-block media alt W)
  (define tag (string-upcase (symbol->string media)))
  (define inner (max (+ 2 (string-length tag))
                     (min (- W 4) (max 16 (+ 2 (string-length alt))))))
  (define alt-lines (wrap-words alt (- inner 2)))
  (map (λ (l) (align-line l W 'center))
       (append
        (list (string-append "┌" (make-string inner #\─) "┐")
              (string-append "│ " (align-line tag (- inner 2) 'left) " │"))
        (for/list ([l (in-list alt-lines)])
          (string-append "│ " (align-line l (- inner 2) 'center) " │"))
        (list (string-append "└" (make-string inner #\─) "┘")))))

(define (option-line label glyph opts W)
  (define line (string-append label ": "
                              (string-join (map (λ (o) (format "~a ~a" glyph o)) opts)
                                           "  ")))
  (map (λ (l) (align-line l W 'left)) (wrap-words line W)))

(define (input-block vt label W)
  (match vt
    ['Bool (list (align-line (format "[ ] ~a" label) W 'left))]
    [(cons 'OneOf opts)  (option-line label "( )" opts W)]
    [(cons 'ManyOf opts) (option-line label "[ ]" opts W)]
    [(list 'Range lo hi)
     (list (align-line (format "~a: ~a [--------|--------] ~a" label lo hi) W 'left))]
    ['LongString
     (define bw (min (- W 2) 40))
     (append
      (list (align-line (string-append label ":") W 'left))
      (map (λ (l) (align-line l W 'left))
           (list (string-append "┌" (make-string bw #\─) "┐")
                 (string-append "│" (spaces bw) "│")
                 (string-append "│" (spaces bw) "│")
                 (string-append "└" (make-string bw #\─) "┘"))))]
    ['Date     (list (align-line (format "~a: [YYYY-MM-DD]" label) W 'left))]
    ['Time     (list (align-line (format "~a: [HH:MM]" label) W 'left))]
    ['Color    (list (align-line (format "~a: [#RRGGBB]" label) W 'left))]
    ['Number   (list (align-line (format "~a: [#####]" label) W 'left))]
    ['Password (list (align-line (format "~a: [********]" label) W 'left))]
    ['File     (list (align-line (format "~a: [ Choose file... ]" label) W 'left))]
    [_ (define field (make-string (min 24 (max 8 (- W (string-length label) 4))) #\_))
       (list (align-line (format "~a: [~a]" label field) W 'left))]))

(define (hole-block nd W)
  (define b (hole-brief nd))
  (define msg (string-append "[ TODO " (type->string (node-spec nd))
                             (if b (string-append " — " b) "") " ]"))
  (map (λ (l) (align-line l W 'center)) (wrap-words msg W)))

;; ---------------------------------------------------------------------------
;; Container blocks
;; ---------------------------------------------------------------------------
(define (v-block cs W #:gap [gap? #t])
  (define bs (filter pair? (map (λ (c) (lay c W)) cs)))
  (cond [(null? bs) '()]
        [gap? (vstack (add-between bs (blank W)))]
        [else (vstack bs)]))

(define (invisible? c)
  (match (node-body c)
    [(or (list 'title _) (list 'meta _ _)) #t]
    [_ #f]))

(define (row-block cs W)
  (define kids (filter (λ (c) (not (invisible? c))) cs))
  (cond
    [(null? kids) '()]
    [else
     (define n (length kids))
     (define gap 2)
     (define avail (max n (- W (* gap (sub1 n)))))
     (define base (quotient avail n))
     (define extra (remainder avail n))
     (define widths (for/list ([i (in-range n)]) (+ base (if (< i extra) 1 0))))
     (define blocks (for/list ([c (in-list kids)] [w (in-list widths)]) (lay c w)))
     (define h (apply max (map length blocks)))
     (define padded (for/list ([b (in-list blocks)] [w (in-list widths)])
                      (append b (make-list (- h (length b)) (spaces w)))))
     (for/list ([i (in-range h)])
       (align-line (string-join (for/list ([b (in-list padded)]) (list-ref b i))
                                (spaces gap))
                   W 'left))]))

;; Lists: each child gets a marker ("- " or "1. "); items flow with no gaps.
(define (list-block ordered? cs W)
  (define bs
    (for/list ([c (in-list cs)] [i (in-naturals 1)])
      (define marker (if ordered? (format "~a. " i) "- "))
      (define mw (string-length marker))
      (define inner (lay c (- W mw)))
      (for/list ([ln (in-list inner)] [j (in-naturals)])
        (align-line (string-append (if (zero? j) marker (spaces mw)) ln) W 'left))))
  (vstack (filter pair? bs)))

;; Definition lists: terms flush, definitions indented; no gaps inside pairs.
(define (deflist-block cs W)
  (vstack
   (filter pair?
           (for/list ([c (in-list cs)])
             (define defn? (match (node-body c) [(cons 'defn _) #t] [_ #f]))
             (if defn?
                 (map (λ (l) (align-line (string-append "    " l) W 'left))
                      (lay c (- W 4)))
                 (lay c W))))))

(define (quote-block cs W)
  (map (λ (l) (align-line (string-append "│ " l) W 'left))
       (v-block cs (- W 2))))

(define (footer-block cs W)
  (append (list (make-string W #\─))
          (v-block cs W)))

;; Disclosure: first child is the summary line; the rest indent beneath.
(define (details-block cs W)
  (match cs
    ['() '()]
    [(cons s rest)
     (append
      (for/list ([ln (in-list (lay s (- W 2)))] [j (in-naturals)])
        (align-line (string-append (if (zero? j) "v " "  ") ln) W 'left))
      (map (λ (l) (align-line (string-append "  " l) W 'left))
           (v-block rest (- W 2))))]))

;; Tables: cells flatten to one line; columns align across rows; a rule after
;; the first row.
(define (inline-string nd)
  (match (node-body nd)
    [(list 'text runs)      (runs->decorated runs)]
    [(list 'heading _ runs) (runs->decorated runs)]
    [(list 'button s)       (string-append "[" s "]")]
    [(list 'embed m _ a)    (format "[~a: ~a]" m a)]
    [(list 'input _ l)      (format "[~a]" l)]
    [(list 'divider)        "──"]
    [(list 'hole _)         "[TODO]"]
    [(or (list 'title _) (list 'meta _ _)) ""]
    [(cons _ cs)            (string-join (map inline-string cs) " ")]))

(define (table-block cs W)
  (define rows
    (for/list ([c (in-list cs)])
      (match (node-body c)
        [(cons 'trow cells) (map inline-string cells)]
        [_ (list (inline-string c))])))
  (cond
    [(null? rows) '()]
    [else
     (define ncols (apply max (map length rows)))
     (define rows* (for/list ([r (in-list rows)])
                     (append r (make-list (- ncols (length r)) ""))))
     (define naturals
       (for/list ([i (in-range ncols)])
         (apply max 1 (for/list ([r (in-list rows*)])
                        (string-length (list-ref r i))))))
     (define avail (- W (* 3 (sub1 ncols))))
     (define widths
       (if (<= (apply + naturals) avail)
           naturals
           (for/list ([_ (in-range ncols)]) (max 3 (quotient avail ncols)))))
     (define (row-line r)
       (align-line (string-join (for/list ([cell (in-list r)] [w (in-list widths)])
                                  (align-line cell w 'left))
                                " │ ")
                   W 'left))
     (define sep
       (align-line (string-join (for/list ([w (in-list widths)])
                                  (make-string w #\─))
                                "─┼─")
                   W 'left))
     (append (list (row-line (car rows*)) sep)
             (map row-line (cdr rows*)))]))

;; ---------------------------------------------------------------------------
;; Dispatch
;; ---------------------------------------------------------------------------
(define (lay nd W)
  (match (node-body nd)
    [(list 'text runs)      (text-lines runs W 'left)]
    [(list 'heading l runs) (heading-block l (runs->decorated runs) W)]
    [(list 'button s)       (button-block s W)]
    [(list 'embed m _ a)    (embed-block m a W)]
    [(list 'input vt l)     (input-block vt l W)]
    [(list 'divider)        (list (make-string W #\─))]
    [(list 'title _)        '()]
    [(list 'meta _ _)       '()]
    [(list 'entry _ _ _ _)  '()]
    [(cons (or 'journal 'lexicon 'words) _) '()]
    [(list 'hole _)         (hole-block nd W)]
    [(cons tag cs)
     (case tag
       [(list)       (list-block #f cs W)]
       [(olist)      (list-block #t cs W)]
       [(deflist)    (deflist-block cs W)]
       [(blockquote) (quote-block cs W)]
       [(footer)     (footer-block cs W)]
       [(details)    (details-block cs W)]
       [(table)      (table-block cs W)]
       [else (case (container-axis tag)
               [(v) (v-block cs W)]
               [(h) (row-block cs W)])])]))

;; ---------------------------------------------------------------------------
;; The framed page. The first (title …) in the tree names the window.
;; ---------------------------------------------------------------------------
(define (render-page nd #:width [W (page-width)])
  (define body (lay (page-of nd) W))     ; a store renders its page slot
  (define content (append (blank W) (if (null? body) (blank W) body) (blank W)))
  (define pad 2)
  (define inner (+ W (* 2 pad)))
  (define title
    (for/first ([n (in-list (all-nodes nd))] #:when (title-node? n))
      (cadr (node-body n))))
  (define chrome
    (string-append "  o o o" (if title (string-append "  ─  " title) "")))
  (define (bar l r) (string-append l (make-string inner #\─) r))
  (define (row s)   (string-append "│" (spaces pad) s (spaces pad) "│"))
  (string-join
   (append (list (bar "┌" "┐")
                 (string-append "│" (align-line chrome inner 'left) "│")
                 (bar "├" "┤"))
           (map row content)
           (list (bar "└" "┘")))
   "\n"))
