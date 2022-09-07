# avocADO - Safe compile-time parallelization of `for` comprehensions

## Example

```scala
import cats.effect.IO

import avocado.*
import avocado.catseffect3.given

val run: IO[Int] =
  ado {
    for {
      a <- heavyTask1
      b <- heavyTask2
      c <- heavyTask3(a)
      _ <- heavyTask4
    } yield c + b
  }
```

## Description

`avocADO` is a small library that allows for automatic rewriting of `for` comprehensions to their parallel versions.

The name `avocADO` is a pun on the most important function exposed by the library -- `ado` (name taken from Haskell's language extension `ApplicativeDo`).

## References

Inspired by [Haskell's Applicative do-notation](https://gitlab.haskell.org/ghc/ghc/-/wikis/applicative-do)

Similar project: [`kitlangton/parallel-for`](https://github.com/kitlangton/parallel-for) (only for Scala 2)
