# labyrinth [![Build Status](https://travis-ci.org/grancalavera/labyrinth.svg?branch=master)](https://travis-ci.org/grancalavera/labyrinth)

> A clone of Ravensburger's Labyrinth

## Installation instructions

I have only tested this project using [Stack](https://docs.haskellstack.org/en/stable/README/). Currently the only way to get the project runnig is building it from source:

```bash
~ git clone git@github.com:grancalavera/labyrinth.git
~ cd labyrinth
~ stack build
~ stack exec labyrinth
```

That should do it...

## Screenshots

This is still work in progress, but so far it looks like this:

![Player selection screen](./etc/labyrinth-1.png)

![Game play](./etc/labyrinth-2.png)

## Game Rules

> Read the rules [here](./etc/rules.md).

## Extensions

- [`DuplicateRecordFields`][duplicaterecordfields]
- [`NamedFieldPuns`][namedfieldpuns]
- [`OverloadedStrings`][overloadedstrings]
- [`RankNTypes`][rankntypes]
- [`RecordWildCards`][recordwildcards]: This exension implies [DisambiguateRecordFields][disambiguaterecordfields].
- [`TemplateHaskell`][templatehaskell]

## Profiling with Stack

> [Profiling builds with Stack 1.0.0 and newer][profiling]

[disambiguaterecordfields]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DisambiguateRecordFields
[duplicaterecordfields]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#duplicate-record-fields
[namedfieldpuns]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NamedFieldPuns
[overloadedstrings]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-string-literals
[rankntypes]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RankNTypes
[recordwildcards]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RecordWildCards
[templatehaskell]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TemplateHaskell
[profiling]: https://stackoverflow.com/questions/32123475/profiling-builds-with-stack
