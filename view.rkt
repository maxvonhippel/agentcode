#lang racket/base
;; Serialize the tree (and journal, and emit payloads) to the JSON the browser
;; clients consume. Shared by the live viewer (ui.rkt) and the recorder
;; (record.rkt) so the two never drift.

(require racket/match "core.rkt")
(provide node->view journal->view jsonify)

(define (run->view r)
  (let* ([h (hasheq 'text (run-str r))]
         [h (if (pair? (run-marks r))
                (hash-set h 'marks (map symbol->string (run-marks r)))
                h)]
         [h (if (run-href r) (hash-set h 'href (run-href r)) h)])
    h))

(define (vt->view vt)
  (match vt
    [(cons 'OneOf opts)  (hasheq 'oneOf opts)]
    [(cons 'ManyOf opts) (hasheq 'manyOf opts)]
    [(list 'Range lo hi) (hasheq 'range (list lo hi))]
    [(? symbol?)         (symbol->string vt)]))

;; Every node carries id + witness type + contract so the client can draw the
;; devtools chip and the type-check beat.
(define (node->view n)
  (define base
    (match (node-body n)
      [(list 'text runs)      (hasheq 'kind "text" 'runs (map run->view runs))]
      [(list 'heading l runs) (hasheq 'kind "heading" 'level l
                                      'runs (map run->view runs))]
      [(list 'button s)       (hasheq 'kind "button" 'label s)]
      [(list 'embed m u a)    (hasheq 'kind "embed" 'media (symbol->string m)
                                      'url u 'alt a)]
      [(list 'input vt l)     (hasheq 'kind "input" 'value (vt->view vt) 'label l)]
      [(list 'divider)        (hasheq 'kind "divider")]
      [(list 'title s)        (hasheq 'kind "title" 'text s)]
      [(list 'meta k s)       (hasheq 'kind "meta" 'key (symbol->string k)
                                      'content s)]
      [(list 'hole b)         (hasheq 'kind "hole" 'brief (or b ""))]
      [(cons tag cs)          (hasheq 'kind (symbol->string tag)
                                      'children (map node->view cs))]))
  (hash-set* base 'id (node-id n)
             'wtype (type->string (type-of n))
             'spec (type->string (node-spec n))))

(define (journal->view store)
  (for/list ([e (in-list (journal-entries store))])
    (hasheq 'id (node-id e)
            'kind (symbol->string (entry-kind e))
            'at (entry-at e)
            'instruction (entry-instruction e)
            'notes (entry-notes e))))

;; Emit payloads carry symbols; JSON wants strings. Keys stay symbols (jsexpr).
(define (jsonify v)
  (cond [(symbol? v) (symbol->string v)]
        [(hash? v) (for/hasheq ([(k x) (in-hash v)]) (values k (jsonify x)))]
        [(list? v) (map jsonify v)]
        [(or (string? v) (boolean? v) (number? v)) v]
        [else (format "~a" v)]))
