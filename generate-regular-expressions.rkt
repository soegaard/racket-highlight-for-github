#lang racket

(module+ test (require rackunit))

(define (wrap s)
  (define n (string-length s))
  (~a #\( s #\))
  ;;; sign the test below doesn't work - the ( and ) needs to 
  ;;; be in a pair -- that is the test below fails for ()|()
  #;(if (and (>= n 2) (and (eqv? (string-ref s 0)       #\()
                           (eqv? (string-ref s (- n 1)) #\))))
        s
        (~a #\( s #\))))

#;(module+ test
    (check-equal? (wrap  "1") "(1)")
    (check-equal? (wrap "(1)") "(1)"))

(define (emit-re re)
  (define e emit-re)
  (define (e* res) (map e res))
  (match re
    [(list '? re)          (~a (wrap (e re)) "?")]
    [(list '* re)          (~a (wrap (e re)) "*")]
    [(list '+ re)          (~a (wrap (e re)) "+")]
    [(list 'seq re* ...)   (apply ~a (e* re*))]
    [(list 'union re* ...) (wrap (apply ~a (add-between (map wrap (e* re*)) "|")))]
    [(list 'ahead re)      (~a "(?=" (wrap (e re)) ")")]
    [(list 'not-ahead re)  (~a "(?!" (wrap (e re)) ")")]
    [(list 'sub re)        (~a (wrap (e re)))]
    [(? number? n)         (~a n)]
    [(? char? c)           (~a (ch c))]
    [(? string? s)         (~a s)]    
    [_ (error 'emit-re "got ~a" re)]))

(define-syntax (define-builder stx)
  (syntax-case stx ()
    [(_ name (var ...) prefix)
     (syntax/loc stx
       (define (name var ...) (cons prefix (list var ...))))]
    [(_ name var prefix)
     (syntax/loc stx
       (define (name . var) (cons prefix var)))]))

(define-builder union res        'union)
(define-builder seq res          'seq)
(define-builder optional     (re) '?)
(define-builder one-or-more  (re) '+)
(define-builder zero-or-more (re) '*)
(define-builder sub          (re) 'sub)

; ch : char -> string
;  escapes characters
(define (ch c)
  (define escapes '(#\? #\* #\+ #\^ #\$ #\\ #\# #\. #\- #\/))
  (cond
    [(and (string? c) (= (string-length c) 1))
     (ch (string-ref c 0))]
    [(char? c)
     (if (member c escapes)
         (~a #\\ c)
         c)]
    [(symbol? c)
     (ch (symbol->string c))]
    [else
     (error 'char "got: ~a" c)]))

(define (str s)
  (apply ~a (map ch (string->list s))))


(define <sign>    (union (ch '+) (ch '-)))

(define <digit2>  "[0-1]")
(define <digit8>  "[0-7]")
(define <digit10> "[0-9]")
(define <digit16> "[0-9abcdef]")
(define (digit n) 
  (case n
    [(2)  <digit2>]
    [(8)  <digit8>]
    [(10) <digit10>]
    [(16) <digit16>]
    [else (error)]))

(define <exp-mark16> "[sl]")
(define <exp-mark10> "[sldef]")
(define <exp-mark8>  <exp-mark10>)
(define <exp-mark2>  <exp-mark10>)
(define (exp-mark n)
  (cond 
    [(extflonum?) "[tT]"]
    [else        (case n
                   [(2)  <exp-mark2>]
                   [(8)  <exp-mark8>]
                   [(10) <exp-mark10>]
                   [(16) <exp-mark16>]
                   [else (error)])]))
(define <exactness> (seq #\# "[ei]"))
(define (general-number n)
  (seq (optional <exactness>) (number n)))
(define (number n)
  (union (exact n) (inexact n)))
(define (exact n)
  (union (exact-rational n) (exact-complex n)))
(define (exact-rational n)
  (seq (optional <sign>) (unsigned-rational n)))
(define (unsigned-rational n)
  (union (unsigned-integer n)
         (seq (unsigned-integer n) #\/ (unsigned-integer n))))
(define (exact-integer n)
  (seq (optional <sign>) (unsigned-integer n)))
(define (unsigned-integer n)
  (one-or-more (digit n)))
(define (exact-complex n)
  (seq (optional (exact-rational n)) <sign> (optional (unsigned-rational n)) "i"))
(define (inexact n)
  (union (inexact-real n) (inexact-complex n)))
(define (inexact-real n)
  (union (seq (optional <sign>) (inexact-normal n))
         (seq <sign> (inexact-special n))))
(define (inexact-unsigned n)
  (union (inexact-normal n) (inexact-special n)))
(define (inexact-normal n)
  (seq (inexact-simple n) (optional (sub (seq (exp-mark n) (exact-integer n))))))
(define (inexact-simple n)
  (union (seq (digits# n) (optional #\.) (zero-or-more #\#))
         (seq (optional (unsigned-integer n)) #\. (digits# n))
         (seq (digits# n) #\/ (digits# n))))
(define (inexact-special n)
  (sub (seq (union (str "inf.") (str "nan.")) "[0ftT]")))
(define (digits# n)
  (seq (one-or-more (digit n)) (zero-or-more #\#)))
(define (inexact-complex n)
  (union (seq (optional (inexact-real n)) <sign> (optional (inexact-unsigned n)) #\i)
         (seq (inexact-real n) #\@ (inexact-real n))))
(define <delimiter-ahead> "(?=[()\\[\\]{}\",'`;\\ \\s])")

(define (extflonum)
  (parameterize ([extflonum? #t])
    (seq (sub (union (seq #\# "[bB]" (inexact-real 2))
                     (seq #\# "[oO]" (inexact-real 8))
                     (seq #\# "[xX]" (inexact-real 16))
                     (seq (optional (seq #\# "[bB]")) (inexact-real 10))))
         <delimiter-ahead>)))

(define extflonum? (make-parameter #f))

(define (binary-numbers)
  (seq (sub (seq (zero-or-more (seq #\# "[bBeEiI]"))
                 (general-number 2)))
       <delimiter-ahead>))

(define (decimal-numbers)
  (seq (sub (seq (zero-or-more (seq #\# "[dDeEiI]"))
                 (general-number 10)))
       <delimiter-ahead>))

(define (octal-numbers)
  (seq (sub (seq (zero-or-more (seq #\# "[oOeEiI]"))
                 (general-number 8)))
       <delimiter-ahead>))

(define (hexadecimal-numbers)
  (seq (sub (seq (zero-or-more (seq #\# "[xXeEiI]"))
                 (general-number 16)))
       <delimiter-ahead>))

'binary
(displayln (emit-re (binary-numbers)))
(newline)
'octal
(displayln (emit-re (octal-numbers)))
(newline)
'decimal
(displayln (emit-re (decimal-numbers)))
(newline)
'hexadecimal
(displayln (emit-re (hexadecimal-numbers)))
'extflonum
(displayln (emit-re (extflonum)))
