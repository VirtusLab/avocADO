# avocADO - Safe compile-time parallelization of `for` comprehensions

## Example

```scala
import cats.effect.IO

import avocado.*
import avocado.instances.cats.given

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

The name `avocADO` is a pun on the most important function exposed by the library - `ado` (name taken from Haskell's language extension `ApplicativeDo`).

## Usage (with build tools)

### sbt

```scala
libraryDependencies ++= Seq(
  "org.virtuslab" %% "avocado" % "version from the badge",
  "org.virtuslab" %% "avocado-cats" % "version from the badge", // for Cats
  "org.virtuslab" %% "avocado-zio-2" % "version from the badge", // for ZIO 2.x
  "org.virtuslab" %% "avocado-zio-1" % "version from the badge", // for ZIO 1.x
)
```

### scala-cli

```scala
//> using lib "org.virtuslab::avocado:version from the badge"
//> using lib "org.virtuslab::avocado-cats:version from the badge" // for Cats
//> using lib "org.virtuslab::avocado-zio-2:version from the badge" // for ZIO 2.x
//> using lib "org.virtuslab::avocado-zio-1:version from the badge" // for ZIO 1.x
```

## Usage (in code)

All you need to do in order to use `avocADO` is to import the `ado` function and an `AvocADO` instance for your effect system. i.e.
```scala
import avocado.* // This line exposes the `ado` function - entrypoint of the library
// You should choose one of the following imports depending on your effect system of choice
import avocado.instances.cats.given
import avocado.instances.zio2.given
import avocado.instances.zio1.given
```

And that's it! All that's left is to wrap the `for`-comprehensions that you want to parallelize with a call to `ado` an watch your program run in parallel! Like so:
```scala
ado {
  for {
    ...
  } yield ...
}
```

## Usage (custom monads)

On the off chance that `avocADO` doesn't provide an instance for your favourite effect monad, you might have to write an instance of our `AvocADO` typeclass yourself. Don't worry, it's relatively straightforward.

`AvocADO` is just a `Monad` with a changed name, so that it can be easily associated with `avocADO`. So if you want to write an instance for a class called `Effect` it might look like so:
```scala
given AvocADO[Effect] = new AvocADO[Effect] {
  def pure[A](a: A): Effect[A] = Effect.pure(a)
  def map[A, B](fa: Effect[A], f: A => B): Effect[B] = fa.map(f)
  def zip[A, B](fa: Effect[A], fb: Effect[B]): Effect[(A, B)] = fa.zipPar(fb) // This is the most important method
  def flatMap[A, B](fa: Effect[A], f: A => Effect[B]): Effect[B] = fa.flatMap(f)
}
```

Every parallel part of the computation will be rewritten to a call to `zip`, so in order to achieve any speedup, you have to provide a parallel implementation for `zip`.

## References

Inspired by [Haskell's Applicative do-notation](https://gitlab.haskell.org/ghc/ghc/-/wikis/applicative-do)

Similar project: [`kitlangton/parallel-for`](https://github.com/kitlangton/parallel-for) (only for Scala 2)
