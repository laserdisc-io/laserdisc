package laserdisc
package fs2

import _root_.fs2.{Scheduler, async}
import cats.effect.Effect
import shapeless.Poly1

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

object PromiseMapper extends Poly1 {
  implicit def mkOne[F[_], A](implicit ec: ExecutionContext, s: Scheduler) = at[(Protocol.Aux[A], Effect[F])] {
    case (protocol, effect) =>
      (queueAndFiniteDuration: (Queue[F], FiniteDuration)) =>
        queueAndFiniteDuration match {
          case (queue: Queue[F], timeout: FiniteDuration) =>
            effect.flatMap(async.promise[F, Maybe[Maybe[A]]](effect, ec)) { promise =>
              effect.flatMap(queue.enqueue1(Request(protocol, promise.complete))) { _ =>
                effect.flatMap(promise.timedGet(timeout, s)) {
                  case None                  => effect.raiseError[Maybe[A]](RequestTimedOut(protocol))
                  case Some(Left(throwable)) => effect.raiseError[Maybe[A]](throwable)
                  case Some(Right(response)) => effect.pure(response)
                }
              }
            }
        }
  }
}
