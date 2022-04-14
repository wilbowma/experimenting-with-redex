#lang scribble/manual
@(require "lib.rkt")

@title[#:tag "sec:syntax"]{Syntax in Redex}
The first part of my language model is defining the collection expressions in my
language.
This includes the expressions of my language, the types, and the data.
Sometimes I encode the meta structures---environments, machine
configurations, etc---as syntax.
I will also usually encode simple properties of interest in syntax, such as
whether a term is a value or not.
This is exactly what we do on paper in programming languages research.

After I have defined a syntax, Redex can do the following things for me:
@itemize[
@item{decide whether an s-expressions matches a BNF nonterminal}
@item{decompose a term via pattern matching}
@item{decide α-equivalence of two terms}
@item{perform substitution}
@item{generate expressions the match a nonterminal}
]

@section{Syntax TLDR}
In short, Redex makes defining and working with syntax damn simple.
I use @racket[define-language] to create BNF grammars.
With @racket[#:binding-forms] (@rtech{binding forms}), I get capture-avoiding
substitution (@racket[substitute]), and α-equivalence
(@racket[alpha-equivalent?]), for free while retaining named identifiers rather
than annoying representations such as de Bruijn.
Using @racket[define-extended-language], I can easily extend existing languages,
which can make it easy to modularize grammars; although, I usually avoid it
because then I have to remember too many language identifiers.
I use @racket[redex-match?], @racket[redex-match], and @racket[redex-let] to
query whether expressions match nonterminals and to let Redex decompose syntax
for me.
I use the test functions @racket[test-predicate], @racket[test-equal],
@racket[test-results], and the @gtech{parameter} @racket[default-equiv], for
writing test suites about syntax, and @racket[redex-check] to do random testing
of syntactic properties.

One caveat to be aware of is that grammars with @rtech{side conditions} prevent
@racket[redex-check] from working over the grammar.

There are two common pitfalls to avoid when working with syntax in Redex.

First, be careful about using untagged, arbtirary variables, such as by using
@rtech{variable} or @rtech{variable-not-otherwise-mentioned} in your syntax
definition.
This makes it really easy to create arbitrary variable names, but also easy to
make to get typos interpreted as variables.
This can cause very unexpected failures.
Apparently valid matches will not fail to match and metafunctions can have
undefined behavior.
One way to help avoid this problem is to tag variables, either in the grammar or
with a symbolic prefix using @rtech{variable-prefix}.

Second, avoid unicode subscripts, and be careful with TeX input mode.
Unicode subscripts are different than a nonterminal followed by an underscore,
@emph{i.e.}, @redex{any₁} is a symbol while @redex{any_1} is a pattern
variable matching an @rtech{any} pattern.
This is made worse since TeX input mode will transparently replace the second
expression with the first.
DrRacket has better LaTeX input that avoids this problem by default.

@section{Defining Syntax}
In Redex, we start defining the syntax of a language with the
@racket[define-language] form.
Below is the syntax for the lanugage @deftech{BoxyL}, the simply-typed λ-calculus
with the box modality.
As a convention, I usually end my language names with the letter "L".
@examples[
#:eval boxy-evalor
(require redex/reduction-semantics)
(define-language BoxyL
  (e ::= x natural (+ e e) (cons e e) (car e) (cdr e) (λ (x : A) e) (e e)
         (box e) (let ((box y) e) e))
  (A B ::= Nat (A × B) (A → B) (□ A))
  (x y ::= variable-not-otherwise-mentioned)
  (v ::= natural (box e) (λ (x : A) e) (cons v v))

  #:binding-forms
  (λ (x : A) e #:refers-to x)
  (let ((box y) e_1) e_2 #:refers-to y))
]

I usually start my models by requiring @racketmodname[redex/reduction-semantics]
instead of @racketmodname[redex].
The latter loads GUI stuff I rarely use, and sometimes don't even install.

The @racket[define-language] form takes a language identifier, which is used by
some functions to specify which grammar and binding specification to use,
and BNF-esque grammar, in s-expression notation.
After the grammar, I give a binding specification, from which Redex infers
definitions of substitution and α-equivalence.

This language defines expression using meta-variable @redex{e}, which includes
variables @redex{x}, natural numbers, cons pairs, λ, application, the box
introduction form, and a pattern matching form for box elimination.
It also defines their types, as meta-variables @redex{A} and @redex{B}.
Note that while productions need to be s-expressions, they need not be in prefix
notation.
Variables, @redex{x} and @redex{y}, are any symbol not used as a keyword or
literal elsewhere in the grammar, @rtech{variable-not-otherwise-mentioned}.

This essentially equivalent to the grammar from the Coq model in
@seclink["sec:preface"], but also fixes a representation of variables, and
defines the metafunction @racket[substitute] and the Racket
@racket[alpha-equivalent?] for @tech{BoxyL}.

@section{Pattern Matching}
Redex has a sophisticated formal pattern language, but in essense any symbols not
recognized as a BNF symbol is treated as a keyword.
When I write @redex{(car e)} in the grammar, this is understood to mean the
literal symbol @redex{car} followed by some expression @redex{e} is also an
expression.
The pattern language also has some built-in nonterminals, such as
@rtech{natural} which indicates a natural number literals such as @racket[5].

We can ask the Redex pattern matcher, via @racket[redex-match?], whether terms
match some nonterminal from the grammar.
@examples[
#:eval boxy-evalor
(define eg1 (term (box 5)))
(define eg2 (term (cdr 5)))
(redex-match? BoxyL e eg1)
(redex-match? BoxyL v eg1)
(redex-match? BoxyL v eg2)
]

This is helpful particularly with large complex languages, or low-level
languages with detailed machine configurations.
In these kinds of languages, its common to restrict syntax to achieve various
properties, and may be non-obvious whether a term is valid.

Note that keywords and literal symbols from the grammar are not variables, but
anything else is.

@examples[
#:eval boxy-evalor
(redex-match? BoxyL x (term car))
(redex-match? BoxyL x (term λ))
(redex-match? BoxyL x (term :))
(redex-match? BoxyL x (term kar))
(redex-match? BoxyL x (term lambda))
]

One of the important built-in patterns that I use frequently is @rtech{any},
which matches any s-expression.
@examples[
#:eval boxy-evalor
(redex-match? BoxyL any (term car))
(redex-match? BoxyL (any_1 any_1) (term (car car)))
(redex-match? BoxyL any (term kar))
(redex-match? BoxyL any (term :))
]

The @rtech{any} pattern can be useful for defining generic metafunctions,
grammars, and judgments, or to partially specify contracts on Redex definitions.
In large developments, I sometimes create a base language with generic
operations on syntax, using @rtech{any}.

When in Racket, we use @racket[term] to inject syntax into Redex.
@racket[term] acts like @racket[quasiquote], and even supports unquote, so we
can write use Racket to compute terms through templating.
Most quoted s-expressions are also valid Redex terms, which can be useful if we
want to move between s-expressions in Racket and terms in Redex.

@examples[
#:eval boxy-evalor
(define eg3 (term (cdr ,(+ 2 3))))
eg3
(redex-match? BoxyL e eg3)
(define eg4 '(cdr 5))
(redex-match? BoxyL e eg4)
]

Redex can also decompose a term by pattern matching.
@examples[
#:eval boxy-evalor
(redex-match BoxyL (e_1 e_2) (term ((λ (x : Nat) x) 5)))
(redex-match BoxyL (box e_1) eg1)
]

The structure returned by @racket[redex-match] is a little annoying to look at.
Redex supports non-deterministic matching, so we get back the set of all possible
ways to match.
The elements of the set are matches, which contain lists of bindings of the
pattern variable to the term.

I don't often use @racket[redex-match] directly for decomposing terms, because
@racket[redex-let] has a much better interface.
But @racket[redex-let] requires a deterministic single match.

@examples[
#:eval boxy-evalor
(redex-let BoxyL ([(e_1 e_2) (term ((λ (x : Nat) x) 5))])
  (displayln (term e_1))
  (displayln (term e_2)))
]

@section{Extending Syntax and Syntax with Holes (Contexts)}
@racket[define-extended-language] allows extending a base language with new
nonterminals, or extending existing nonterminals from the base language with new
productions.
For example, I could extend expressions with booleans literals as follows.

@examples[
#:eval boxy-evalor
(define-extended-language BoxyBoolL BoxyL
  (e ::= .... boolean))

(redex-match? BoxyL e (term #t))
(redex-match? BoxyL e (term (car #t)))
(redex-match? BoxyBoolL e (term #t))
(redex-match? BoxyBoolL e (term (car #t)))
]

This defines a new language, @deftech{BoxyBoolL}, which extends @tech{BoxyL}
with an extra production in the nonterminal @redex{e}.
To extend a nonterminal, I use @racket[....], which means "all the current
productions of this nonterminal", followed by the new productions.

We can also add new nonterminals by extending a language.

@examples[
#:eval boxy-evalor
(define-extended-language BoxyEvalL BoxyL
  (E ::= hole (E e) (v E) (cons v ... E e ...) (+ v ... E e ...) (car E) (cdr E)
         (let ((box x) E) e)))
]

This defines a new language, @deftech{BoxyEvalL}, which extends @tech{BoxyL}
with a call-by-value evaluation context.
Redex syntax also support @emph{contexts}, that is, programs with a hole.
I use this for evaluation and program contexts.
In this language, we define the context @redex{E} by listing the usual
evaluation contexts for call-by-value.
For operators with many arguments, we can easily specify left-to-right
evaluation order using non-deterministic ellipses patterns.

After specifying a context, we can easily decompose a term into its evaluation
context and its redex.

@examples[
#:eval boxy-evalor
(redex-match BoxyEvalL (in-hole E v) (term (car (cons (+ 1 2) 2))))
(redex-match BoxyEvalL (in-hole E e) (term (car (cons (+ 1 2) 2))))
(redex-let BoxyEvalL ([(in-hole E (+ 1 2)) (term (car (cons (+ 1 2) 2)))])
  (displayln (term E))
  (displayln (term (in-hole E 3))))
]

@section{Binding, Substitution, and α-equivalence}
Probably @emph{the} winning feature in Redex for me is automagic handling of
binding.
After 11 lines of code, binding is completely solved.
We have capture-avoiding substitution and α-equivalence for free @emph{using
named identifiers}.
@examples[
#:eval boxy-evalor
(default-language BoxyL)
(alpha-equivalent? (term (λ (x : Nat) e))  (term (λ (x : Nat) e)))
(alpha-equivalent? (term (λ (x : Nat) x))  (term (λ (y : Nat) y)))
(alpha-equivalent? (term (λ (x : Nat) y))  (term (λ (y : Nat) y)))
(term (substitute (λ (y : Nat) (x e2)) x (+ y 5)))
]

In this tutorial, I give only the simplest possible use of
@rtech{binding forms}: a single, lexical name.
But Redex's support for binding is extremely sophisticated.
It can support n-ary lexical binding with shadowing, pattern-matching style
binding, module-style binding.
See @rtech{binding forms} in the Redex docs to the many examples of complex binding support.

@section{Random Generation of Syntax}
Finally, we can generate terms from the grammar.
This is helpful for random testing of meta-theory, and generating examples.
@examples[
#:eval boxy-evalor
(eval:alts (define an-e1 (generate-term BoxyL e 10)) (define an-e1 '(cdr (car (λ (k : (□ Nat)) (i 0))))))
(eval:alts (define an-e2 (generate-term BoxyL e 10)) (define an-e2 '(box (f 0))))
an-e1
an-e2
]

@racket[generate-term] takes a language name, a generation specification, and a
size.
It can generate terms based on lots of Redex definitions, although I almost
always generate terms from grammars and judgments.

@section{Testing Syntax}
Once we're done querying, we can move to testing.
Redex features a unit testing library.
Tests are great for detecting breakages as you start tinkering with your model,
and can also report more information that the mostly-boolean query functions.

Redex, unfortunately, doesn't have a test function for patterns.
Instead, we have to use @racket[test-predicate] and @racket[redex-match?].
@examples[
#:eval boxy-evalor
(default-language BoxyL)
(test-predicate (redex-match? BoxyL e) (term (car (+ 1 2))))
(test-predicate (redex-match? BoxyL x) (term kar))
(test-predicate (redex-match? BoxyL x) (term car))
(test-results)
]
@margin-note{This pattern is hard to abstract over since @racket[redex-match?]
expects a language identifier.}

For testing equality of terms, we can use @racket[test-equal].
@examples[
#:eval boxy-evalor
(test-equal (term (λ (x : Nat) y)) (term (λ (y : Nat) y))
            #:equiv alpha-equivalent?)
(default-equiv alpha-equivalent?)
(test-equal (term (λ (x : Nat) x)) (term (λ (y : Nat) y)))
(test-equal (term (substitute (λ (y : Nat) (x e2)) x (+ y 5)))
            (term (λ (z : Nat) ((+ y 5) e2))))
(test-results)
]
@racket[test-equal] takes two expressions, and optionally, an equivalence
predicate.
The equivalence defaults to the value of the @gtech{parameter}
@racket[default-equiv].

We can use @racket[redex-check] to do random testing of syntactic properties.
For example, all values ought to be expressions.
@examples[
#:eval boxy-evalor
(redex-check
 BoxyL
 v
 (redex-match? BoxyL e (term v))
 #:attempts 1000)
]

When generating syntax, @racket[redex-check] takes a language identifier, a
pattern, a predicate, and (optionally), the number of attempts.

@section{Caveat: Syntax with Side Conditions}
The Redex pattern language is very expressive, and one quite nice feature that
is hard to find in the documentation is using @rtech{side-conditions} in
grammar.
I use this occassionally, particularly when modeling machine languages.
For example, below I define a syntax with 64-bit integers.
@examples[
#:eval boxy-evalor
(define-language Int64L
  (int64 ::= (side-condition integer_1
                             (<= (expt -2 31) (term integer_1) (sub1 (expt 2 31))))))

(redex-match? Int64L int64 4)
(redex-match? Int64L int64 -5)
(redex-match? Int64L int64 (expt 2 31))
]

The @rtech{side-condition} takes a pattern and a Racket predicate which can
refer to the pattern variables.
You can use a @rtech{side-condition} pattern anywhere in a pattern.

Unfortunately, using @rtech{side-conditions} in patterns does not work with
@racket[redex-check].
You can still use @racket[generate-term], though.

@section{Pitfall: All Symbols are Variables}
In a couple of the examples above, I rely on an anti-pattern.
I @emph{pun} between a real expression and an expression schema, by punning
between variables and meta-variables.
For example, consider the Redex term @redex{((λ (x : Nat) e_body)
e_arg)}.
This is only valid because I used @rtech{variable-not-otherwise-mentioned},
which makes all symbols valid variables.
In @tech{BoxyL}, this expression is valid, but in an unintuitive way.
The subterms @redex{e_body} and @redex{e_arg} are actually variables, although
we are pretending they are meta-variables.

@examples[
#:eval boxy-evalor
(redex-match? BoxyL x (term e_body))
(redex-match? BoxyL x (term e_arg))
]

This can let us treat an @emph{expression} as an expression @emph{schema}.
It is a very tempting way to create examples.
Instead of being very precise about the entire term, you can create readable
meta-examples only specifying parts of the term and pretending this applies to
all terms of this pattern.
But relying on this behavior can really cause problems if you're not careful.

For example, consider the @emph{very similar} term @redex{((λ (x : A)
e_body) e_arg)}.
Ah, surely this meta-example represents any valid application... but it isn't
even a valid expression.

@examples[
#:eval boxy-evalor
(redex-match? BoxyL e (term ((λ (x : A) e_body) e_arg)))
(redex-match? BoxyL A (term A))
]

The problem is that @redex{A}, isn't a valid type, and we did not
allow types to have variables in the grammar.

Worse, when a term doesn't match a language nonterminal, metafunctions such as
@racket[substitute] can exhibit undefined behavior.

@examples[
#:eval boxy-evalor
(term (substitute (λ (y : A) (x e2)) x (+ y 5)))
(alpha-equivalent? (term (λ (x : A) x)) (term (λ (y : A) y)))
]

One should avoid this pattern, and be aware of it because you can do it
accidently.
For example, if we use the wrong letter or wrong case, we can easily create an
invalid term, or a valid term that isn't what we expected.

@examples[
#:eval boxy-evalor
(redex-match? BoxyL e (term (subtsitute (λ (y : Nat) (x e2)) x (+ y 5))))
(redex-match? BoxyL e (term (λ (y : nat) (x e2))))
(redex-match? BoxyL e (term (kar (cons e_1 e_2))))
]

The root of the problem is that we used
@rtech{variable-not-otherwise-mentioned} and didn't tag variables, so any
symbol is a valid expression.
This is hard to avoid in Redex unless you exclusively use
@rtech{variable-prefix}.
Be careful when using the variable specifications that give you infinite
unstructured variables.
It's handy, but like in any scripting language, that usefulness comes at a cost.
It will cause you problems.
It has caused me problems.
It bit me twice while writing this tutorial.

Even when you manage to avoid this, @racket[term] simply quotes any symbol, so
typos can result in valid terms, although the term may not match a nonterminal.
If you use contracts, which I strongly encourage and will use in the rest of
this tutorial, this is less of a problem since the invalid quoted term will
hopefully not match a nonterminal.

@section{Pitfall: Subscripts and Unicode}
Redex makes it very easy to mix unicode into your formal syntax.
This is handy, since the code looks closer to the paper presentation.
Unfortunatley, it can cause problems if you're using unicode subscripts or TeX
input mode in emacs.

The underscore, @redex{_}, has a special meaning when attached to a nonterminal
in a Redex pattern: it creates a distinct pattern variable matching the
preceeding nonterminal.
You might be tempted to use a unicode subscript instead, but that doesn't work.

@examples[
#:eval boxy-evalor
(redex-match BoxyL (e₁ e₂) (term ((λ (x : Nat) x) 5)))
(redex-match BoxyL (e_1 e_2) (term ((λ (x : Nat) x) 5)))
]

This can be a particular problem if you're using TeX input mode in emacs.
When in TeX input mode, typing @redex{_} causes the input mode to being typing a
subscript unicode symbol, creating a @emph{literal symbol} rather than a pattern
variable.
This result will be a mysterious failure.

I'm not sure if unicode subscripts should be treated the same by Redex or not,
but for now, I recommend never ever using unicode subscripts in Redex.
I usually use TeX input mode, and have a hotkey for turning it on and off, so I
can type underscores.

@footer-nav["sec:preface" "sec:eval"]
