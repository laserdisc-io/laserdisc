/*
 * Copyright (c) 2018-2025 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
    compute = fromExecutor(Executors.newFixedThreadPool(16)),
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
