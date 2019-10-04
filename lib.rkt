#lang racket/base

(require
 (for-label redex/reduction-semantics)
 (for-label racket)
 scribble/manual
 scribble/example
 racket/runtime-path
 racket/function
 scribble/core
 scribble/latex-properties
 scribble/html-properties)

(provide
 (for-label
  (all-from-out
   racket
   redex/reduction-semantics))
 (all-from-out
  racket/function
  scribble/example)
 (all-defined-out))

(define boxy-evalor (make-base-eval))


(define-runtime-path custom-css-path "custom.css")
(define-runtime-path custom-tex-path "custom.tex")

(define backlink-style
  (make-style
   "footer-backlink"
   (list
    (make-css-addition custom-css-path)
    (make-tex-addition custom-tex-path))))

(define frwdlink-style
  (make-style
   "footer-frwdlink"
   (list
    (make-css-addition custom-css-path)
    (make-tex-addition custom-tex-path))))

(define (footer-nav back frwd)
  (cons
   (element backlink-style (list (seclink back "← ") (seclink back)))
   (if frwd
       (list
        (element frwdlink-style (list (seclink frwd) (seclink frwd " →"))))
       '())))

(define (rtech . x)
  (apply tech x #:doc '(lib "redex/redex.scrbl")))

(define (gtech . x)
  (apply tech x #:doc '(lib "scribblings/guide/guide.scrbl")))

(define redex racketplainfont)
