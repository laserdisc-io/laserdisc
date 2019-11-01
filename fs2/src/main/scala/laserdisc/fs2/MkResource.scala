package laserdisc
package fs2

import java.util.concurrent.TimeUnit.SECONDS

import cats.effect.{Resource, Sync}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object MkResource {
  sealed trait CanShutdown[A] { def shutdown[F[_]](implicit F: Sync[F]): A => F[Unit] }

  final object CanShutdown {
    implicit val canShutdownExecutionContextExecutorService: CanShutdown[ExecutionContextExecutorService] =
      new CanShutdown[ExecutionContextExecutorService] {
        override def shutdown[F[_]](implicit F: Sync[F]): ExecutionContextExecutorService => F[Unit] =
          ec =>
            F.delay {
              ec.shutdown()
              ec.awaitTermination(3, SECONDS)
              ()
            }
      }
  }

  private[laserdisc] final def apply[F[_]: Sync, A](acquire: => F[A])(implicit A: CanShutdown[A]): Resource[F, A] =
    Resource.make(acquire)(A.shutdown)

  /**
    * Creates an execution context that will wait on shut down.
    */
  @inline final def of[F[_]: Sync](fe: F[ExecutionContextExecutorService]): Resource[F, ExecutionContext] =
    MkResource(fe).widenRight[ExecutionContext]
}
