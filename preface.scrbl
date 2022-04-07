#lang scribble/manual
@(require scribble/coq "lib.rkt")

@title[#:tag "sec:preface"]{Preface---Why Redex}
I think of a language as:
@itemize[
@item{a collection of expressions,}
@item{with an evaluation function (relation),}
@item{which satisfy some properties.}
]

This is not the only definition, and might not be the best definition, but it's
a definition that works for me.

What I want out of a programming language modeling tool is the ability to model
these pieces of the language, and use the tool to check my own intuitions and
reasoning.
The tool ought to match my own intuitions about a language, and the tool ought
to communicate in the same abstractions as I do.

When modeling a language, I start with expressions.
I think of expressions (syntax) separate from judgments about expressions (e.g.,
well-typed syntax).
When working with syntax, I want a tool that can answer questions like
@itemize[
@item{Is this syntax a valid expression in the language?}
@item{Is this syntax a valid value in the language?}
@item{Does this expression decompose into a context and this other expression in
the hole?}
]
I want the ability to write syntax that matches no grammar in my language.
I want the ability to check whether some syntax matches a nonterminal in my
language grammar.
While experimenting, I do not want to be restricted to building only well formed
or well typed syntax, since my intuition about what @emph{ought} to be valid may
be at odds with what @emph{is}, formally, valid.
I want to be able to write some crap down, and ask the computer "yes or no?"; I
don't want to be forced to prove "yes", because then I'm not necessarily sure
about "no".

The second piece I build is the reduction system and evaluation function so I
can compute with expressions.
I may have some type system in mind, but I will usually build it after I have an
evaluation function, but the way I want terms to evaluate may guide changes to
the type system.
When working with reduction and evaluation, I want to ask questions like:
@itemize[
@item{Does this expression evaluate to some value?}
@item{Does some other expression evaluate to the same value?}
@item{Do all expressions evaluate?}
@item{Do all expressions evaluate to a value?}
]
These kinds of questions ought to be trivial for a computer.
Just compute!
I don't want to prove that the relation is terminating or manually build
derivations.
I don't want to build derivations by hand.
I don't want to write a (big-step) interpreter.
I just want to write some rewrite rules and say "apply them to a fixedpoint".

Lastly, I start to formalize judgments, which characterize properties of
expressions.
I usually define equivalence judgements and typing judgments, but sometimes I
define other things like translation judgments.
When working with judgments, I am interested in questions like:
@itemize[
@item{Does this expression have this property?}
@item{Do all expressions of this form have this property?}
]
Again, I don't want to prove my judgment is sensible in any way.
Most tools put far too many restrictions on the kinds of judgments I write.
I don't want to be forced to write derivations by hand.
I just want to say "can you find a derivation, computer?", and have that be
useful most of the time.

The reason I prefer Redex is that (1) Redex and I agree about what a language is
and (2) I can @emph{very quickly} get a model and start asking Redex these
questions.
Redex imposes few restrictions, and tries hard to do something useful.
One of the authors of Redex described it as "a scripting language for
metatheory".

By constrast, merely to formalize the syntax of a language in a more formal tool
such as Coq, in "the usual way", requires a large data definitions, rigid
restrcitions, and is hard to get to a point where something computes.
For example, consider the following formalization of a syntax in Coq.
@margin-note{I pick on Coq because it's another tool I know fairly well and
sometimes use.}

@(define coq-eval (make-coq-evaluator))
@coq-example[#:eval coq-eval]{
Parameter Var : Type.

Inductive BoxyType : Type :=
| boxy_ty_nat : BoxyType
| boxy_ty_box : BoxyType -> BoxyType
| boxy_ty_fun : BoxyType -> BoxyType -> BoxyType
| boxy_ty_pair : BoxyType -> BoxyType -> BoxyType.

Inductive BoxyTerm : Type :=
| boxy_tm_nat : nat -> BoxyTerm
| boxy_tm_var : Var -> BoxyTerm
| boxy_tm_cons : BoxyTerm -> BoxyTerm -> BoxyTerm
| boxy_tm_car : BoxyTerm -> BoxyTerm
| boxy_tm_cdr : BoxyTerm -> BoxyTerm
| boxy_tm_plus : BoxyTerm -> BoxyTerm -> BoxyTerm
| boxy_tm_fun : Var -> BoxyType -> BoxyTerm -> BoxyTerm
| boxy_tm_app : BoxyType -> BoxyTerm -> BoxyTerm
| boxy_tm_box : BoxyType -> BoxyTerm
| boxy_tm_unbox : Var -> BoxyType -> BoxyTerm -> BoxyTerm.

Inductive BoxyValue : BoxyTerm -> Prop :=
| boxy_v_nat : forall n, BoxyValue (boxy_tm_nat n)
| boxy_v_box : forall e, BoxyValue (boxy_tm_box e)
| boxy_v_fun : forall x t e, BoxyValue (boxy_tm_fun x t e)
| boxy_v_cons : forall v1 v2, BoxyValue v1 -> BoxyValue v2 -> BoxyValue (boxy_tm_cons v1 v2).
}

This defines the syntax for a simply-typed λ-calculus with the box modality in
Coq.

It leaves the type @code{Var} of variables abstract, for reasons anyone
familiar with modeling languages in Coq will understand.

This is 26 lines of incredibly tedious to write code, generated by
hand-compiling my mental model of the language into type theory.
It is not easy to ask simple questions such as "is this a term".
I have to manually compile the term into a well-typed type-theoretic
representation; I can't just write the symbols I'd write on paper.
By the time I ask the question I either know the answer or don't know whether
I'm too stupid to figure out the answer.
@coq-example[#:eval coq-eval]{
Example example1 : BoxyTerm := boxy_tm_nat 1.
}

Even when I write out terms, I cannot easily ask whether a term matches some
other nonterminal, @emph{e.g.}, I cannot ask whether the above term is a value.
I can only @emph{prove} that it @emph{is}, and perhaps fail, or I must write a
decision procedure.

@coq-example[#:eval coq-eval]{
(* Is example1 a value? *)
Lemma example2 : (BoxyValue example1).
Proof.
auto.
Qed.
(* Well I dunno. I guess I have to prove it by hand. *)

Fixpoint is_value (e : BoxyTerm) : bool :=
match e with
| boxy_tm_box e' => true
| boxy_tm_nat n => true
| boxy_tm_fun x A e => true
| boxy_tm_cons e1 e2 => andb (is_value e1) (is_value e2)
end.

(* Oops *)

Fixpoint is_value (e : BoxyTerm) : bool :=
match e with
| boxy_tm_box e' => true
| boxy_tm_nat n => true
| boxy_tm_fun x A e => true
| boxy_tm_cons e1 e2 => andb (is_value e1) (is_value e2)
| _ => false
end.

Eval compute in (is_value example1).
}

And that is just to deal with syntax.

I cannot quickly formalize the reduction systems, and when I do, I again have
to manually compile it into Coq's type theory (to get the small-step relational
presentation I prefer), or into a fuel monad (to get a version that actually
computes).
It would take me hours and 100s of lines of code to define the full model and
get to a point where I can ask the questions I want to ask.

If this were the state-of-the-art, I wouldn't bother to use a computer for
experimenting with languages.
It costs me much more time to come up with the model than to work on paper.
Trying to implement that model is completely unintuitive.
I end up struggling with details, like how I represent variables, that have
nothing to do with the problem I'm trying to solve.
The only time I would use that technique, if ever, is if I were writing
high-assurance software with lives on the line, or if I really needed to
convince reviewers to accept my paper.

As I will show in the rest of this tutorial, with Redex, I can go from nothing
to a model that computes in minutes and 10s of lines of code.
A Redex model is nearly identical to what I write on paper, so it's almost
completely intuitive (assuming a formal programming languages background).
Redex tries hard to do what you mean, which is what I want when experimenting.
I save a ton of time by letting Redex generate and check examples, and run the
computations.
It's not a replacement for a proof assistant, but it's irreplaceable for
experimenting.

So how do I use Redex to experiment?

@footer-nav[
"sec:index"
"sec:syntax"
]
