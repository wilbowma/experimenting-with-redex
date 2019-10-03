#lang scribble/manual
@(require "lib.rkt")

@title[#:tag "sec:index"]{Experimenting with Languages in Redex}
@author[(author+email "William J. Bowman" "wjb@williamjbowman.com")]

In this tutorial, I introduce Redex the way I approach Redex: as a tool to
explore and experiment with languages.
I also explain some of my useful conventions and patterns, common problems I run
into in Redex, and tricks to avoid problems as best I can.

This tutorial is aimed at programming languages researchers who understand a
programming languages formalism, but want to understand how to use Redex 
as an assistant in exploring their formalism.
I explain Redex in context, by example.
I primarily focus on how I use it to work, and leave most details of Redex forms
to the Redex documentation.

@(table-of-contents)
@include-section{preface.scrbl}
@include-section{syntax.scrbl}
@include-section{eval.scrbl}
@include-section{judgment.scrbl}
