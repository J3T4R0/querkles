#lang scribble/acmart @sigplan @anonymous @review @10pt
@require{references.rkt}
@(define neu (affiliation #:institution "Leland High School"))
@(define anon (email "niemiryan@gmail.com"))
@title{Walking the AST}
@author[#:affiliaition neu #:email anon]{Ryan Niemi}
@; optional: set the author names in the page headers
@elem[#:style "Sshortauthors"]{R. Niemi}

@require[
  (only-in scribble/core make-style)
  (only-in scribble/latex-properties make-tex-addition)]

@require[pict scriblib/figure]
@figure[
  "fig:fish"  @; figure tag, see `figure-ref`
  @elem{A Standard Fish}  @; figure caption, appears below the content
  @elem{fish = @(standard-fish 90 40)}]  @; content

@(define extra-style-files
   (list (make-tex-addition "style.tex")))

@title[#:style (make-style #f extra-style-files)]{Writing a paper with Scribble}

@; ....

@include-abstract{abstract.scrbl}
@include-section{introduction.scrbl}
@generate-bibliography[#:sec-title "References"]

