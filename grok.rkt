#lang racket/base
;; xAI (Grok) transport. Reads XAI_API_KEY / XAI_MODEL from a sibling .env.

(require net/url json racket/port racket/string racket/runtime-path)
(provide grok-complete api-key)

(define-runtime-path dotenv-path ".env")

(define (load-dotenv)
  (when (file-exists? dotenv-path)
    (call-with-input-file dotenv-path
      (λ (in)
        (for ([line (in-lines in)])
          (define t (string-trim line))
          (unless (or (string=? t "") (string-prefix? t "#"))
            (define i (for/first ([c (in-string t)] [k (in-naturals)]
                                  #:when (char=? c #\=)) k))
            (when i
              (define key (string-trim (substring t 0 i)))
              (define val (string-trim (substring t (add1 i))))
              (unless (getenv key) (putenv key val)))))))))
(load-dotenv)

(define (api-key)
  (or (getenv "XAI_API_KEY")
      (error 'grok "XAI_API_KEY not set — put it in .env")))

(define (model) (or (getenv "XAI_MODEL") "grok-build-0.1"))

(define API-URL "https://api.x.ai/v1/chat/completions")

;; Send a system+user message, return the assistant's text content.
(define (grok-complete system user #:temp [temp 0])
  (define payload
    (hasheq 'model (model)
            'temperature temp
            'messages (list (hasheq 'role "system" 'content system)
                            (hasheq 'role "user"   'content user))))
  (define data (string->bytes/utf-8 (jsexpr->string payload)))
  (define headers (list "Content-Type: application/json"
                        (string-append "Authorization: Bearer " (api-key))))
  (define ip (post-impure-port (string->url API-URL) data headers))
  (purify-port ip)                 ; consume status line + headers
  (define body (port->string ip))
  (close-input-port ip)
  (define js (with-handlers ([exn:fail? (λ (_) (error 'grok "non-JSON response: ~a" body))])
               (string->jsexpr body)))
  (cond
    [(hash-has-key? js 'choices)
     (hash-ref (hash-ref (car (hash-ref js 'choices)) 'message) 'content)]
    [(hash-has-key? js 'error)
     (error 'grok "API error: ~a" (jsexpr->string (hash-ref js 'error)))]
    [else (error 'grok "unexpected response: ~a" body)]))
