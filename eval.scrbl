#lang scribble/manual
@(require "lib.rkt" (except-in "syntax.scrbl" doc))

@title[#:tag "sec:eval"]{Reduction and Evaluation in Redex}
After specifying my language syntax, I define reduction, normalization, and
evaluation.
Once I define the reduction relation, Redex can gives me a nondeterministic
interpreter.
Redex makes this extremely easy.
I can give the small-step reductions, and Redex will compute for me the
compatible closure automatically, giving me a normalization relation.
If I need a specific reduction strategy, I can define the evaluation contexts
manually and have Redex compute the context closure of a relation.
For data like numbers, I don't need to resort to inductive encoding; I can
easily escape into Racket to compute.

After defining a reduction relation, Redex can do the following for me:
@itemize[
@item{apply one step of a reduction relation}
@item{apply a reduction relation until a normal form}
@item{apply a reduction relation until some condition}
@item{compute closures of a reduction relation}
]

@section{Reduction TLDR}
In short, I define the small-step reductions using @racket[reduction-relation].
I usually define the full reduction using @racket[compatible-closure], which in
1 line of code extends the small-step relation to apply under any context.
I use @racket[apply-reduction-relation] to apply a reduction relation a single
step, and @racket[apply-reduction-relation*] to reduce an expression to the
normal form of the given reduction relation.
When I need a specific reduction strategy, I manually define evaluation contexts
as syntax, as in the previous section, and use @racket[context-closure].
Since @racket[apply-reduction-relation*] is a Racket function, I usually define
metafunctions using @racket[define-metafunction] to easily call the reduciton
and normalization relations from Redex.

There is one common pitfall I run in to: full reduction for mutually defined
syntax.
If expressions and values are mutually defined, for example, then
@racket[compatible-closure] will not compute the correct relation.
In this case, we must manually give the mutually defined contexts for which we
@racket[context-closure] should compute the closure of a relation.
Thankfully, Redex exposes the function @rtech{compatible-closure-context} for
computing contexts common contexts.
In most cases I encounter, Redex can easily compute the various mutual contexts
with a couple of calls to @rtech{compatible-closure-context}, and then give my
the full reduction relation using @racket[context-closure].

@section{Reduction Relations for BoxyL}
In Redex, reduction relations are defined using @racket[reduction-relation].
It requires a language identifier and a sequence of rewrite rules.
The form also supports @emph{a lot} of optional and additional features, most of
which I never use.
I will commonly use @racket[#:domain] and @racket[#:codomain] to catch bugs,
including some of the pitfalls mentioned in the last section.

Below, I define the small-step reduction for @tech{BoxyL}.

@examples[
#:eval boxy-evalor
(define ->
  (reduction-relation
   BoxyL
   #:domain e
   #:codomain e
   (--> ((λ (x : A) e_1) e_2) (substitute e_1 x e_2)
       "β→")
   (--> (car (cons e_1 e_2)) e_1
       "β×₁")
   (--> (cdr (cons e_1 e_2)) e_2
       "β×₂")
   (--> (+ v_1 v_2) ,(+ (term v_1) (term v_2))
       "plus")
   (--> (let ((box x) (box e_1)) e_2) (substitute e_2 x e_1)
       "β□")))]

Unlike @racket[define-language], @racket[reduction-relation] creates a
@emph{Racket} value.
This means we need to bind it to an identifiers using Racket's @racket[define].
It also means we have to be in Racket-land when using the reduction relation.

To run the reduction, I use @racket[apply-reduction-relation] to take a single
step, and @racket[apply-reduction-relation*] to run all possible reductions.

@examples[
#:eval boxy-evalor
(apply-reduction-relation -> (term (car (cons 1 2))))
(apply-reduction-relation* -> (term (car (cons (+ 1 2) 2))))
]

Like @racket[redex-match], both of these return a set of results,
since Redex allows the reduction relation to be non-deterministic.

We can use @racket[apply-reduction-relation*] to stop when an arbitrary
condition holds of the term being reduced.
For example, we can stop when we try to reduce @redex{(+ 1 2)}.
@examples[
#:eval boxy-evalor
(apply-reduction-relation* -> (term (car (cons (+ 1 2) 2)))
  #:stop-when (lambda (term)
                (redex-match? BoxyL (+ 1 2) term)))
]

To define full reduction, we can easily get the compatible closure of the
relation with respect to a nonterminal.
Redex will lift the small-step relation to apply any reduction any where in any
subexpression of the nonterminal.
It's one line of code.

@examples[
#:eval boxy-evalor
(define ->* (compatible-closure -> BoxyL e))
(apply-reduction-relation* -> (term (λ (x : Nat) (car (cons (+ 1 2) 2)))))
(apply-reduction-relation* ->* (term (λ (x : Nat) (car (cons (+ 1 2) 2)))))
]

This lifts @racket[->] to apply under any context generated from the nonterminal
@redex{e}.
Now we can easily compute normal form of a term.

To define the call-by-value left-to-right semantics, we can reuse the evaluation
contexts defined earlier, and ask Redex to compute the closure of the relation
with respect to that context via @racket[context-closure].

@examples[
#:eval boxy-evalor
(define ->cbv (context-closure -> BoxyEvalL E))
(apply-reduction-relation ->cbv (term (λ (x : Nat) (car (cons (+ 1 2) 2)))))
(apply-reduction-relation* ->cbv (term (λ (x : Nat) (car (cons (+ 1 2) 2)))))
]

When I have deterministic semantics, I define a metafunctions that let me use a
reduction relation under @racket[term].
This help me easily use reduction relations in Redex-land rather than
Racket-land, and reduces boilerplate.

@examples[
#:eval boxy-evalor
(define-metafunction BoxyL
  eval : e -> v
  [(eval e)
   ,(car (apply-reduction-relation* ->cbv (term e)))])

(define-metafunction BoxyL
  normalize : e -> e
  [(normalize e)
   ,(car (apply-reduction-relation* ->* (term e)))])

(term (eval (λ (x : Nat) (car (cons (+ 1 2) 2)))))
(term (normalize (λ (x : Nat) (car (cons (+ 1 2) 2)))))
]
@section{A Caveat: Compatible Closure of Mutually Defined Relations}
The @racket[compatible-closure] functions doesn't work when we have expressions whose
syntax is mutually defined, since @racket[compatible-closure] requires a single
nonterminal.
Consider the call-by-push-value language definition given below.
In this language, values and expressions are mutually defined, and have explicit
injection terms between each.

@(define cbpv-eval (make-base-eval))
@examples[
#:eval cbpv-eval
(require redex/reduction-semantics)

(define-language cbpvL
  (e ::= (λ (x) e) (e v) (force v) (return v))
  (v ::= (thunk e) () x)
  (x ::= variable-not-otherwise-mentioned)

  #:binding-forms
  (λ (x) e #:refers-to x))

(define ->
  (reduction-relation
   cbpvL
   (--> (force (thunk e)) e
        "force")
   (--> ((λ (x) e) v) (substitute e x v)
        "β")))

(apply-reduction-relation* -> (term (force (thunk (return x)))))
]

Trying to use @racket[compatible-closure] here will not give us full reduction.
Worse, it won't result in an error; instead, it will compute unexpected results.
Some terms will compute properly, while others won't.

@examples[
#:eval cbpv-eval
(define wrong->* (compatible-closure -> cbpvL e))
(define wrong^->* (compatible-closure -> cbpvL v))
code:blank
(apply-reduction-relation* wrong->*
  (term (λ (x) (return (thunk (λ (x) (force (thunk (return x)))))))))
(apply-reduction-relation* wrong^->*
  (term (λ (x) (return (thunk (λ (x) (force (thunk (return x)))))))))
code:blank
(apply-reduction-relation* wrong->*
  (term (thunk (λ (x) (return (thunk (λ (x) (force (thunk (return x))))))))))
(apply-reduction-relation* wrong^->*
  (term (thunk (λ (x) (return (thunk (λ (x) (force (thunk (return x))))))))))
]

The problem is that @racket[compatible-closure] is automatically computing a
context.
However, it's default mode of computing the context does not work for mutually
defined nonterminals.

To properly define the full reduction relation for this language, we need to use
@racket[context-closure] and manually give the mutually defined contexts
All @racket[compatible-closure] does is compute the obvious context of a given
nonterminal and call @racket[context-closure].

We can define the contexts manually, but this is extremely tedious.
Thankfully, Redex exposes some of the internals of @racket[compatible-closure],
as @rtech{compatible-closure-context}, to let us compute the context we want.

@examples[
#:eval cbpv-eval
(define-extended-language cbpvCtxtL cbpvL
  (C ::= (compatible-closure-context v #:wrt e)
     (compatible-closure-context e)
     (compatible-closure-context e #:wrt v)))

(define ->* (context-closure -> cbpvCtxtL C))
(apply-reduction-relation* ->*
  (term (thunk (λ (x) (return (thunk (λ (x) (force (thunk (return x))))))))))
(apply-reduction-relation* ->*
  (term (λ (x) (return (thunk (λ (x) (force (thunk (return x)))))))))
]

@footer-nav[
"sec:syntax"
"sec:judgment"
]
