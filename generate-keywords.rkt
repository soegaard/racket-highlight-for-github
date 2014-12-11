#lang racket
;; Use the following to generate lists of built-ins and keywords.
;; Run
;;   (displayln (wrap-lines KEYWORDS))
;;   (displayln (wrap-lines BUILTINS))
;; and copy the results into RacketLexer._keywords and RacketLexer._builtins.

;; (-> (listof string?) string?)
;; Appends all the strings together, quoting them as appropriate for Python,
;; with commas and spaces between them, wrapping at 80 characters, with an
;; indentation of 8 spaces.
(define (wrap-lines lst)
  (define INDENTATION '"        ")
  (define WIDTH '80)
  (define (wrap-lines* lst done-lines current-line)
    (if (null? lst)
        (string-append (foldr string-append "" done-lines) current-line)
        (let* ([str (first lst)]
               [wrapped-str (if (regexp-match-exact? '#px"[[:ascii:]]+" str)
                                (string-append "'" str "',")
                                (string-append "u'" str "',"))]
               [new-line (string-append current-line " " wrapped-str)])
          (if ((string-length new-line) . >= . WIDTH)
              (wrap-lines* (rest lst)
                           (append done-lines
                                   `(,(string-append current-line "\n")))
                           (string-append INDENTATION wrapped-str))
              (wrap-lines* (rest lst)
                           done-lines
                           new-line)))))
  (wrap-lines* lst '() INDENTATION))

;; (-> string? boolean?)
;; Returns #t if str represents a syntax identifier in the current namespace,
;; otherwise #f.
(define (syntax-identifier? str)
    (with-handlers ([exn? exn?])
      (not (eval (call-with-input-string str read)))))

(define RACKET-NAMESPACE
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket)
    (current-namespace)))

(define BOUND-IDENTIFIERS
  (parameterize ([current-namespace RACKET-NAMESPACE])
    (sort (map symbol->string (namespace-mapped-symbols))
          string<=?)))

(define-values (KEYWORDS BUILTINS)
  (parameterize ([current-namespace RACKET-NAMESPACE])
    (partition syntax-identifier? BOUND-IDENTIFIERS)))

(define (escape s)
  (define s1 (regexp-replace* #rx"%"   s  "\\\\%"))
  (define s2 (regexp-replace* #rx"-"   s1 "\\\\-"))
  (define s3 (regexp-replace* #rx"\\*" s2 "\\\\*"))
  (define s4 (regexp-replace* #rx"\\?" s3 "\\\\?"))
  (define s5 (regexp-replace* #rx"#"   s4 "\\\\#"))
  (define s6 (regexp-replace* #rx"\\." s5 "\\\\."))
  (define s7 (regexp-replace* #rx":"   s6 "\\\\:"))
  (define s8 (regexp-replace* #rx"\\+"   s7 "\\\\+"))
  s8)

; (displayln (escape (apply string-append (add-between KEYWORDS "|"))))
(displayln (escape (apply string-append (add-between BUILTINS "|"))))



  
