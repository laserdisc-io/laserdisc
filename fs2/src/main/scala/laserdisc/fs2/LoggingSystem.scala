package laserdisc
package fs2

import cats.Applicative
import log.effect.LogWriter
import log.effect.fs2.SyncLogWriter

private[fs2] class LoggingSystem[F[_]](private[fs2] val ls: LogWriter[F]) extends AnyVal

private[fs2] object LoggingSystem extends LoggingSystemInstances0 {
  @inline final def apply[F[_]](implicit i: LoggingSystem[F]): LoggingSystem[F] = i
}

private[fs2] sealed trait LoggingSystemInstances0 extends LoggingSystemInstances1 {
  implicit def otherLogger[F[_]](implicit ev: LogWriter[F]): LoggingSystem[F] = new LoggingSystem(ev)
}

private[fs2] sealed trait LoggingSystemInstances1 {
  implicit def noOpLog[F[_]: Applicative]: LoggingSystem[F] = new LoggingSystem(SyncLogWriter.noOpLog[F])
}
