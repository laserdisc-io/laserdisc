/*
 * Copyright (c) 2018-2026 LaserDisc
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
import laserdisc.auto.*
import munit.FunSuite

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext.fromExecutor
import scala.reflect.ClassTag

/** The servers these tests run against: each version's import is tested against its own server by setting LASERDISC_REDIS_IMPORT or
  * LASERDISC_VALKEY_IMPORT, and LASERDISC_PORT_OFFSET moves every port, so that they can run next to other servers
  */
object LiveServers {
  private val offset: Int = sys.env.get("LASERDISC_PORT_OFFSET").fold(0)(_.toInt)

  def port(p: Port): Port = Port.unsafeFrom(p.value + offset)

  private val imports: Map[String, Base6_2] = Map(
    "redis_6_2"  -> redis_6_2,
    "redis_7_0"  -> redis_7_0,
    "redis_7_2"  -> redis_7_2,
    "redis_7_4"  -> redis_7_4,
    "redis_8_0"  -> redis_8_0,
    "redis_8_2"  -> redis_8_2,
    "redis_8_4"  -> redis_8_4,
    "redis_8_6"  -> redis_8_6,
    "redis_8_8"  -> redis_8_8,
    "redis_8_10" -> redis_8_10,
    "valkey_7_2" -> valkey_7_2,
    "valkey_8_0" -> valkey_8_0,
    "valkey_8_1" -> valkey_8_1,
    "valkey_9_0" -> valkey_9_0,
    "valkey_9_1" -> valkey_9_1
  )

  private def chosen(variable: String, latest: String): Base6_2 = {
    val name = sys.env.getOrElse(variable, latest)
    imports.getOrElse(name, sys.error(s"$variable is $name, which isn't one of ${imports.keys.toList.sorted.mkString(", ")}"))
  }

  val redis: Base6_2  = chosen("LASERDISC_REDIS_IMPORT", "redis_8_10")
  val valkey: Base6_2 = chosen("LASERDISC_VALKEY_IMPORT", "valkey_9_1")
}

abstract class LaserdiscFs2Suite(p: Port) extends FunSuite {
  // the import of the server this suite runs against, from the start of its name, as the CI matrix picks suites by it
  protected final val imported: Base6_2 = getClass.getSimpleName match {
    case name if name.startsWith("Valkey") => LiveServers.valkey
    case name if name.startsWith("KeyDb")  => redis_6_2
    case _                                 => LiveServers.redis
  }

  /** A test that runs only when this run's import is `V` or a later version, with that import */
  protected final def since[V <: Base6_2](name: String)(body: V => Unit)(implicit V: ClassTag[V]): Unit =
    imported match {
      case V(v) => test(name)(body(v))
      case _    => ()
    }

  protected val runtime: IORuntime = IORuntime(
    compute = fromExecutor(Executors.newFixedThreadPool(16)),
    blocking = fromExecutor(Executors.newCachedThreadPool()),
    scheduler = global.scheduler,
    shutdown = global.shutdown,
    config = IORuntimeConfig()
  )

  private var cleanUp: IO[Unit]               = null
  protected final var client: RedisClient[IO] = null

  override final def beforeAll(): Unit = {
    val (cl, cu) = RedisClient[IO].to("127.0.0.1", p).allocated.unsafeRunSync()(using runtime)
    cleanUp = cu
    client = cl
  }

  override final def afterAll(): Unit =
    cleanUp.unsafeRunSync()(using runtime)

  protected def assertAllEqual[A](as: List[A], a: A): Unit =
    as.foreach(assertEquals(_, a))
}
