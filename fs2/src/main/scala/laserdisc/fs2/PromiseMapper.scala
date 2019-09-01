package laserdisc
package fs2

import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, Timer}
import cats.effect.syntax.concurrent._
import cats.syntax.flatMap._
import cats.syntax.monadError._
import shapeless.Poly1

import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

object PromiseMapper extends Poly1 {
  implicit def mkOne[F[_]: Timer, A](implicit F: Concurrent[F]) =
    at[Protocol.Aux[A]] { protocol => (queueAndDuration: (Queue[F], FiniteDuration)) =>
      queueAndDuration match {
        case (queue: Queue[F], duration: FiniteDuration) =>
          Deferred[F, Maybe[A]].flatMap { promise =>
            queue.enqueue1(Request(protocol, promise.complete)) >> {
              promise.get
                .timeout(duration)
                .adaptError {
                  case _: TimeoutException => RequestTimedOut(protocol)
                }
            }
          }
      }
    }
}
