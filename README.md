# fastsum

This package provides `Data.Sum`, an open-union type, similar to the `Union` type that powers the implementation of [Oleg Kiselyov's extensible-effects library](http://okmij.org/ftp/Haskell/extensible/).

Unlike most open-union implementations, this type is very fast to compile, even when the type-level list of alternatives contains hundreds of entries. Membership queries are constant-time, compiling to a single type-level natural lookup in a closed type family, unlike the traditional encoding of `Union`, which relies on recursive typeclass lookups. As such, this type lends itself to representing abstract syntax trees or other rich data structures. 

GHC 8's support for custom type errors provides readable type errors should membership constraints not be satisfied.

In order to achieve speed, this package makes fewer guarantees about what can be proven given a `Member` instance. If you require a richer vocabulary to describe the implications of membership, you should use the traditional implementation of open-unions.

# Credits

This library is built on the work of Oleg Kiselyov, which was then modified by Allele Dev. It was extracted from the [effects](https://github.com/joshvera/effects/) library.
