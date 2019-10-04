#lang scribble/manual
@(require "lib.rkt")

@title[#:tag "sec:index"]{Experimenting with Languages in Redex}
@author[(author+email "William J. Bowman" "wjb@williamjbowman.com")]

In this tutorial, I introduce Redex the way I approach Redex: as a tool to
explore and experiment with languages.
I also explain some of my useful conventions and patterns, common problems I run
into in Redex, and tricks to avoid problems as best I can.

This tutorial is aimed at programming languages researchers who understand
programming languages formalism, but want to understand how to use Redex
as an assistant in exploring formal models.
I explain Redex in context, by example.
I primarily focus on how I use it to work, and leave most details of Redex forms
to the Redex documentation.

I will use the simply-typed λ-calculus with box modality as a running example,
because I happen to be studying this calculus at the time of writing.
@margin-note{Frank Pfenning and Rowan Davies. A judgmental reconstruction of modal logic. 2001. @url{https://doi.org/10.1017/s0960129501003322}.}

@(table-of-contents)
@include-section{preface.scrbl}
@include-section{syntax.scrbl}
@include-section{eval.scrbl}
@include-section{judgment.scrbl}
