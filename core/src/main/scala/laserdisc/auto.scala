package laserdisc

import eu.timepit.refined.api.{RefType, Refined, Validate}
import eu.timepit.refined.macros.RefineMacro

object auto {
  implicit def autoUnwrap[F[_, _], T](tp: F[T, _])(implicit rt: RefType[F]): T =
    rt.unwrap(tp)

  implicit def autoRefine[T, P](t: T)(
      implicit rt: RefType[Refined],
      v: Validate[T, P]
  ): Refined[T, P] = macro RefineMacro.impl[Refined, T, P]
}
