package laserdisc.fs2

trait Logger[F[_]] {
  def log(level: Logger.Level, msg: => String, maybeT: Logger.MaybeT): F[Unit]
}

object Logger {
  import java.util.{logging => jul}

  import cats.effect.Sync
  import fs2._
  import fs2.Stream._

  final type MaybeT = Option[Throwable]

  sealed trait Level { def value: String }
  final object Level {
    final case object Trace extends Level { override final val value = "TRACE" }
    final case object Debug extends Level { override final val value = "DEBUG" }
    final case object Info  extends Level { override final val value = "INFO"  }
    final case object Warn  extends Level { override final val value = "WARN"  }
    final case object Error extends Level { override final val value = "ERROR" }
  }

  import Level._

  implicit final class LoggerSyntax[F[_]](val self: Logger[F]) extends AnyVal {
    @inline def trace(msg: => String, maybeT: MaybeT = None): F[Unit] = self.log(Trace, msg, maybeT)
    @inline def debug(msg: => String, maybeT: MaybeT = None): F[Unit] = self.log(Debug, msg, maybeT)
    @inline def info(msg: => String, maybeT: MaybeT = None): F[Unit]  = self.log(Info, msg, maybeT)
    @inline def warn(msg: => String, maybeT: MaybeT = None): F[Unit]  = self.log(Warn, msg, maybeT)
    @inline def error(msg: => String, maybeT: MaybeT = None): F[Unit] = self.log(Error, msg, maybeT)

    @inline def traceS(msg: => String, maybeT: MaybeT = None): Stream[F, Unit] = eval(self.log(Trace, msg, maybeT))
    @inline def debugS(msg: => String, maybeT: MaybeT = None): Stream[F, Unit] = eval(self.log(Debug, msg, maybeT))
    @inline def infoS(msg: => String, maybeT: MaybeT = None): Stream[F, Unit]  = eval(self.log(Info, msg, maybeT))
    @inline def warnS(msg: => String, maybeT: MaybeT = None): Stream[F, Unit]  = eval(self.log(Warn, msg, maybeT))
    @inline def errorS(msg: => String, maybeT: MaybeT = None): Stream[F, Unit] = eval(self.log(Error, msg, maybeT))
  }

  def JULLogger[F[_]](julLogger: jul.Logger)(implicit F: Sync[F]): F[Logger[F]] = F.delay {
    new Logger[F] {
      import jul.Level._
      override final def log(level: Level, msg: => String, maybeT: MaybeT): F[Unit] = F.delay {
        val jdkLevel = level match {
          case Trace => FINEST
          case Debug => FINE
          case Info  => INFO
          case Warn  => WARNING
          case Error => SEVERE
        }
        if (julLogger.isLoggable(jdkLevel)) {
          val record = new jul.LogRecord(jdkLevel, msg)
          if (record != null) record.setThrown(maybeT.orNull)
          julLogger.log(record)
        }
      }
    }

  }

}
