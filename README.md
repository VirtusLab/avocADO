# avocADO - Safe compile-time parallelization of `for` comprehensions

## Example

```scala
import cats.effect.IO

import avocado.*
import avocado.catseffect3.given

val run: IO[Int] =
  ado {
    for {
      a <- doStuff1
      b <- doStuff2(a)
      c <- doStuff3
      d <- doStuff4(a)
    } yield combine(a, b, c, d)
  }
```

`avocADO` will transform the above for-comprehension to code equivalent to:
```scala
for {
  a <- doStuff1
  (b, c, d) <- doStuff2(a).zip(doStuff3).zip(doStuff4(a))
} yield combine(a, b, c, d)
```

## Description

`avocADO` is a small library that allows for automatic rewriting of `for` comprehensions to their parallel versions.

The name `avocADO` is a pun on the most important function exposed by the library -- `ado` (name taken from Haskell's language extension `ApplicativeDo`).

## References

Inspired by [Haskell's Applicative do-notation](https://gitlab.haskell.org/ghc/ghc/-/wikis/applicative-do)

Similar project: [`kitlangton/parallel-for`](https://github.com/kitlangton/parallel-for) (only for Scala 2)
