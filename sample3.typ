#set page(paper: "a4")
#set text(font: "Linux Libertine", size: 12pt)

= Hello World - Final Version

This is a comprehensive Typst document for testing - *version 3*.

== Introduction

Welcome to *Typst*! This is the final version of our test document with many features.

We can write:
- Comprehensive lists with more items
- _Italic text_ and #emph[emphasized text]
- `Code snippets` and #raw("raw text")
- #strong[Bold text] and #text(weight: "bold")[weighted text]
- #text(fill: blue)[Colored text]

=== Mathematical Expressions

$ sum_(i=1)^n i = (n(n+1))/2 $

Complex equations:
$ integral_0^infinity e^(-x^2) dif x = sqrt(pi)/2 $

$ lim_(n -> infinity) (1 + 1/n)^n = e $

=== Tables and Figures

#table(
  columns: 4,
  stroke: 0.5pt,
  [Name], [Value], [Category], [Description],
  [First], [1], [Odd], [The first number],
  [Second], [2], [Even], [The second number],
  [Third], [3], [Odd], [The third number],
  [Fourth], [4], [Even], [The fourth number],
)

== Conclusion

This demonstrates the evolution of our Typst document through multiple versions.

#pagebreak()

= Appendix

Additional content on a new page for more substantial differences.
