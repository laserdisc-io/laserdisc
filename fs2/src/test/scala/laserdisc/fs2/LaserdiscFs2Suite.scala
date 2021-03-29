package laserdisc
package fs2

import cats.effect.{ConcurrentEffect, Temporal}
import cats.effect.syntax.effect._
import laserdisc.auto._
import munit.FunSuite

abstract class LaserdiscFs2Suite[F[_]: ContextShift: Temporal: ConcurrentEffect](p: Port) extends FunSuite {

  private var cleanUp: F[Unit]               = _
  protected final var client: RedisClient[F] = _

  override final def beforeAll(): Unit = {
    val (cl, cu) = RedisClient.to("127.0.0.1", p).allocated.toIO.unsafeRunSync()
    cleanUp = cu
    client = cl
  }

  override final def afterAll(): Unit =
    cleanUp.toIO.unsafeRunSync()

  protected def assertAllEqual[A](as: List[A], a: A): Unit =
    as.foreach(assertEquals(_, a))
}
