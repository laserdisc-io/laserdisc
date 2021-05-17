package laserdisc
package fs2

import cats.effect.kernel.{Deferred, Temporal}
import cats.effect.syntax.temporal._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monadError._
import shapeless.Poly1

import scala.concurrent.TimeoutException

object PromiseMapper extends Poly1 {
  private[this] final def mapper[F[_]: Temporal, A](
      in: (Protocol.Aux[A], Env[F])
  ): F[Maybe[A]] = {
    val (protocol, (queue, duration)) = in
    Deferred[F, Maybe[A]] >>= { promise =>
      val complete: Maybe[A] => F[Unit] =
        mba => promise.complete(mba).as(())

      queue.offer(Request(protocol, complete)) >> {
        promise.get
          .timeout(duration)
          .adaptError { case _: TimeoutException => RequestTimedOut(protocol) }
      }
    }
  }

  implicit def mkOne[F[_]: Temporal, A]: Case.Aux[(Protocol.Aux[A], Env[F]), F[Maybe[A]]] =
    at[(Protocol.Aux[A], Env[F])](mapper(_))
}
