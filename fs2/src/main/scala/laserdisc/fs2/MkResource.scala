package laserdisc
package fs2

import java.nio.channels.AsynchronousChannelGroup
import java.util.concurrent.TimeUnit.SECONDS

import cats.effect.{Resource, Sync}

import scala.concurrent.ExecutionContextExecutorService

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
    implicit val canShutdownAsynchronousChannelGroup: CanShutdown[AsynchronousChannelGroup] =
      new CanShutdown[AsynchronousChannelGroup] {
        override def shutdown[F[_]](implicit F: Sync[F]): AsynchronousChannelGroup => F[Unit] =
          acg =>
            F.delay {
              acg.shutdown()
              acg.awaitTermination(3, SECONDS)
              ()
          }
      }
  }

  final def apply[F[_]: Sync, A](acquire: => F[A])(implicit A: CanShutdown[A]): Resource[F, A] =
    Resource.make(acquire)(A.shutdown)
}
