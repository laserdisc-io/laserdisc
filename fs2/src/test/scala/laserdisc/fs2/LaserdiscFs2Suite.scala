package laserdisc
package fs2

import cats.effect.IO
import cats.effect.unsafe.IORuntime.global
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import laserdisc.auto._
import munit.FunSuite

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext.fromExecutor

abstract class LaserdiscFs2Suite(p: Port) extends FunSuite {
  protected val runtime = IORuntime(
    compute = fromExecutor(Executors.newFixedThreadPool(8)),
    blocking = fromExecutor(Executors.newCachedThreadPool()),
    scheduler = global.scheduler,
    shutdown = global.shutdown,
    config = IORuntimeConfig()
  )

  private var cleanUp: IO[Unit]               = _
  protected final var client: RedisClient[IO] = _

  override final def beforeAll(): Unit = {
    val (cl, cu) = RedisClient[IO].to("127.0.0.1", p).allocated.unsafeRunSync()(runtime)
    cleanUp = cu
    client = cl
  }

  override final def afterAll(): Unit =
    cleanUp.unsafeRunSync()(runtime)

  protected def assertAllEqual[A](as: List[A], a: A): Unit =
    as.foreach(assertEquals(_, a))
}
