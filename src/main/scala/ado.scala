package avocado

import macros.*

// TODO eventually docs
inline def ado[F[_], A](inline comp: F[A]): F[A] =
  ${ macros.adoImpl('comp) }
