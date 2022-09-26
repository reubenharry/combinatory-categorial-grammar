# Combinatory Categorial Grammar

This code uses dependent types in Haskell (via the Singletons library) to write a parser and interpreter for a combinatory categorial grammar. The types ensure, at compile time (!), that only sentences in the grammar parse, and that all parsed sentences have a meaning.

e.g.

```haskell
interpret @ExampleEntity exampleLexicon "John runs" Sentence
```

There are a few different versions, but the basic dependently typed system is in `src/CGDependent.hs`.