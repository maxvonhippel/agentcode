#lang racket/base
;; The viewer: a local web app where you see the page, hover/select components
;; devtools-style, and prompt them. Serves ui.html and three endpoints:
;;
;;   GET  /tree               current component tree as JSON (+ busy flag)
;;   GET  /events?since=N     protocol events after seq N (client polls)
;;   POST /prompt {id,instruction}   run dispatch + cascade in a worker thread
;;
;;   racket ui.rkt            → http://localhost:8484  (opens your browser)

(require racket/match racket/string racket/list racket/file racket/runtime-path
         json net/url
         web-server/servlet-env web-server/http
         "core.rkt" "agent.rkt" "view.rkt")

(define PORT 8484)
(define-runtime-path UI-HTML "ui.html")

;; ---------------------------------------------------------------------------
;; State: the page, an event log with sequence numbers, one job at a time.
;; ---------------------------------------------------------------------------
(define lock (make-semaphore 1))
(define (locked thunk) (call-with-semaphore lock thunk))

;; The STORE: (store page journal). The journal is the program's own memory;
;; dispatch writes it, this server only reads it.
(define page
  (box (make-store
        (parse '(stack
                 (title "Max's Burgers")
                 (header (heading 1 "Max's Burgers")
                         (text "The " (em "best") " burgers in Flagstaff."))
                 (button "Order now")
                 (footer (text "(c) 2026 Max's Burgers - "
                               (link "https://instagram.com/maxsburgers" "Instagram"))))))))
(define events (box '()))          ; newest first
(define seq (box 0))
(define busy? (box #f))
(define last-goal (box ""))        ; most recent build goal, for /review

(define (push! type payload)
  (locked
   (λ ()
     (set-box! seq (add1 (unbox seq)))
     (set-box! events
               (cons (hash-set* (jsonify payload)
                                'type (symbol->string type)
                                'seq (unbox seq))
                     (unbox events))))))

(define (events-since n)
  (locked (λ () (reverse (takef (unbox events)
                                (λ (e) (> (hash-ref e 'seq) n)))))))

(define (push-tree!)
  (push! 'tree (hasheq 'tree (node->view (page-of (unbox page))))))

;; ---------------------------------------------------------------------------
;; A prompt job: dispatch, then the cascade for any holes it spawned.
;; ---------------------------------------------------------------------------
(define (push-outcome! r)
  (push! 'outcome (hasheq 'kind (outcome-kind r) 'at (outcome-at r)
                          'info (outcome-info r))))

(define (run-job id instruction)
  (define (emit tag p) (push! tag p))
  (with-handlers ([exn:fail? (λ (e) (push! 'error (hasheq 'message (exn-message e))))])
    (define pre-holes (map node-id (holes-bfs (unbox page))))
    (define r (dispatch (unbox page) id instruction #:emit emit))
    ;; adopt unconditionally: even a refusal leaves a journal entry in the store
    (set-box! page (outcome-root r))
    (push-outcome! r)
    (when (eq? (outcome-kind r) 'refined)
      (push-tree!)
      (define-values (final summary)
        (cascade (unbox page) #:emit emit #:skip pre-holes #:max-steps 20
                 #:on-refined (λ (new-root _r)
                                (set-box! page new-root)
                                (push-tree!))
                 #:on-blocked (λ (_h rr) (push-outcome! rr))))
      (set-box! page final)
      (push! 'summary summary)))
  (set-box! busy? #f)
  (push! 'job-done (hasheq)))

;; The root reviews the rendered page and dispatches corrective edits until it
;; approves; each change re-renders in the viewer.
(define (do-review goal)
  (define emit (λ (tag p) (push! tag p)))
  (define-values (final summary)
    (review (unbox page) goal #:emit emit
            #:on-change (λ (nr _o) (set-box! page nr) (push-tree!))))
  (set-box! page final)
  (push! 'review-summary summary))

;; Build = the page becomes one briefed hole, the cascade fills it while the
;; viewer watches, then the root reviews the result. Journal and lexicon survive.
(define (run-build goal)
  (with-handlers ([exn:fail? (λ (e) (push! 'error (hasheq 'message (exn-message e))))])
    (set-box! last-goal goal)
    (set-box! page (store-with-page (unbox page)
                                    (parse (list 'hole 'Component goal))))
    (push-tree!)
    (define-values (final summary)
      (cascade (unbox page) #:emit (λ (tag p) (push! tag p)) #:max-steps 20
               #:on-refined (λ (new-root _r)
                              (set-box! page new-root)
                              (push-tree!))
               #:on-blocked (λ (_h rr) (push-outcome! rr))))
    (set-box! page final)
    (push! 'summary summary)
    (do-review goal))
  (set-box! busy? #f)
  (push! 'job-done (hasheq)))

(define (run-review goal)
  (with-handlers ([exn:fail? (λ (e) (push! 'error (hasheq 'message (exn-message e))))])
    (do-review goal))
  (set-box! busy? #f)
  (push! 'job-done (hasheq)))

(define (run-query id question)
  (with-handlers ([exn:fail? (λ (e) (push! 'error (hasheq 'message (exn-message e))))])
    (define a (query-node (unbox page) id question #:emit (λ (tag p) (push! tag p))))
    (push! 'query-done (hasheq 'target id 'text a)))
  (set-box! busy? #f)
  (push! 'job-done (hasheq)))

;; ---------------------------------------------------------------------------
;; HTTP
;; ---------------------------------------------------------------------------
(define (json-response js [code 200])
  (response/full code (if (= code 200) #"OK" #"ERR") (current-seconds)
                 #"application/json; charset=utf-8" '()
                 (list (string->bytes/utf-8 (jsexpr->string js)))))

(define (query-ref req key default)
  (define v (assq key (url-query (request-uri req))))
  (or (and v (cdr v)) default))

(define (handle req)
  (define path (string-join (map path/param-path (url-path (request-uri req))) "/"))
  (define post? (equal? (request-method req) #"POST"))
  (cond
    [(member path '("" "/"))
     (response/full 200 #"OK" (current-seconds) TEXT/HTML-MIME-TYPE '()
                    (list (file->bytes UI-HTML)))]
    [(equal? path "tree")
     (json-response (hasheq 'tree (node->view (page-of (unbox page)))
                            'journal (journal->view (unbox page))
                            'busy (unbox busy?)))]
    [(equal? path "events")
     (define since (or (string->number (query-ref req 'since "0")) 0))
     (json-response (hasheq 'events (events-since since) 'busy (unbox busy?)))]
    [(and post? (equal? path "build"))
     (define body (with-handlers ([exn:fail? (λ (_) #f)])
                    (string->jsexpr
                     (bytes->string/utf-8 (or (request-post-data/raw req) #"")))))
     (define goal (and (hash? body) (string-trim (hash-ref body 'goal ""))))
     (cond
       [(not (non-empty-string? (or goal ""))) (json-response (hasheq 'error "empty goal") 400)]
       [(unbox busy?) (json-response (hasheq 'error "busy — one job at a time") 409)]
       [else
        (set-box! busy? #t)
        (push! 'job-start (hasheq 'target "page" 'instruction goal 'op "build"))
        (thread (λ () (run-build goal)))
        (json-response (hasheq 'ok #t))])]
    [(and post? (equal? path "review"))
     (define body (with-handlers ([exn:fail? (λ (_) #f)])
                    (string->jsexpr
                     (bytes->string/utf-8 (or (request-post-data/raw req) #"")))))
     (define g (and (hash? body) (string-trim (hash-ref body 'goal ""))))
     (define goal (cond [(non-empty-string? (or g "")) g]
                        [(non-empty-string? (unbox last-goal)) (unbox last-goal)]
                        [else "make this a coherent, complete, well-structured page"]))
     (cond
       [(unbox busy?) (json-response (hasheq 'error "busy — one job at a time") 409)]
       [else
        (set-box! busy? #t)
        (push! 'job-start (hasheq 'target "page" 'instruction goal 'op "review"))
        (thread (λ () (run-review goal)))
        (json-response (hasheq 'ok #t))])]
    [(and post? (member path '("prompt" "query")))
     (define body (with-handlers ([exn:fail? (λ (_) #f)])
                    (string->jsexpr
                     (bytes->string/utf-8 (or (request-post-data/raw req) #"")))))
     (cond
       [(not (hash? body)) (json-response (hasheq 'error "bad json") 400)]
       [(unbox busy?) (json-response (hasheq 'error "busy — one job at a time") 409)]
       [else
        (define id (hash-ref body 'id (node-id (page-of (unbox page)))))
        (define instruction (string-trim (hash-ref body 'instruction "")))
        (define target (and (string? id) (find-node (unbox page) id)))
        (cond
          [(not target)
           (json-response (hasheq 'error (format "no node ~a" id)) 404)]
          [(system-node? target)
           (json-response (hasheq 'error (format "~a is a system node" id)) 400)]
          [(string=? instruction "")
           (json-response (hasheq 'error "empty instruction") 400)]
          [else
           (set-box! busy? #t)
           (push! 'job-start (hasheq 'target id 'instruction instruction
                                     'op path))
           (thread (λ () (if (equal? path "query")
                             (run-query id instruction)
                             (run-job id instruction))))
           (json-response (hasheq 'ok #t))])])]
    [else (json-response (hasheq 'error "not found") 404)]))

(module+ main
  (printf "token-rich-types viewer → http://localhost:~a\n" PORT)
  (serve/servlet handle
                 #:port PORT
                 #:servlet-regexp #rx""
                 #:servlet-path "/"
                 #:launch-browser? (not (getenv "TRT_NO_BROWSER"))))
