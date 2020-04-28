//import cats.effect.{Concurrent, ContextShift, Timer}
//import laserdisc.fs2.RedisClient
//import log.effect.LogWriter
//import cats.syntax.flatMap._
//import laserdisc._
//import laserdisc.all._
//import laserdisc.fs2._
//import laserdisc.auto._
//import log.effect.LogWriter
//import cats.syntax.applicativeError._
//
//object test {
//
//  def redisTest[F[_]: Concurrent: Timer: ContextShift: LogWriter]: F[Unit] =
//    RedisClient.toNode[F]("localhost", 6379).use { client =>
//      client.send(
//        set("a", 23),
//        set("b", 55),
//        get[PosInt]("b"),
//        get[PosInt]("a")
//      ) >>= {
//        case (Right(OK), Right(OK), Right(Some(getOfb)), Right(Some(getOfa))) if getOfb.value == 55 && getOfa.value == 23 =>
//          LogWriter.info("yay!")
//        case other =>
//          LogWriter.error(s"something went terribly wrong $other") >>
//            new RuntimeException("boom").raiseError
//      }
//    }
//
//  trait A[-R] {
//    def aaaa[RR <: R, B](f: B => A[RR]): Unit
//
//    // def bbbb[B](f: B => A[R]): Unit BOOM!
//    // [error] /Users/fmariotti/open-source/laserdisc/fs2/src/main/scala/laserdisc/fs2/test.scala:33:17: contravariant type R occurs in covariant position in type B => test.A[R] of value f
//    // [error]     def bbbb[B](f: B => A[R]): Unit
//    // [error]                 ^
//  }
//
//}
