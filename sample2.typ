#set page(paper: "a4")
#set text(font: "Linux Libertine", size: 12pt)

= Hello World

This is a simple Typst document for testing - *version 2*.

== Introduction  

Welcome to *Typst*! This is the second version of our test document.

We can write:
- Lists with more items
- _Italic text_
- `Code snippets`
- #strong[Bold text]

$ sum_(i=1)^n i = (n(n+1))/2 $

And here's a more complex equation:
$ integral_0^infinity e^(-x^2) dif x = sqrt(pi)/2 $

#table(
  columns: 3,
  [Name], [Value], [Category],
  [First], [1], [Odd],
  [Second], [2], [Even],
  [Third], [3], [Odd],
)