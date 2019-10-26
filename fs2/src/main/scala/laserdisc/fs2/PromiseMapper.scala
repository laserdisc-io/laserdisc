package laserdisc
package fs2

import cats.effect.concurrent.Deferred
import cats.effect.syntax.concurrent._
import cats.effect.{Concurrent, Timer}
import cats.syntax.flatMap._
import cats.syntax.monadError._
import shapeless.Poly1

import scala.concurrent.TimeoutException

object PromiseMapper extends Poly1 {
  private[this] final def mapper[F[_]: Concurrent: Timer, A](protocol: Protocol.Aux[A]): Env[F] => F[Maybe[A]] = {
    case (queue, duration) =>
      Deferred[F, Maybe[A]] >>= { promise =>
        queue.enqueue1(Request(protocol, promise.complete)) >> {
          promise.get
            .timeout(duration)
            .adaptError {
              case _: TimeoutException => RequestTimedOut(protocol)
            }
        }
      }
  }

  implicit def mkOne[F[_]: Timer: Concurrent, A]: Case.Aux[Protocol.Aux[A], Env[F] => F[Maybe[A]]] = at[Protocol.Aux[A]](mapper(_))
}
