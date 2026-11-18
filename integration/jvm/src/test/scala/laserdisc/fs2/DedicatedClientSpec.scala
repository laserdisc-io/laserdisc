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
import laserdisc.auto.*
import laserdisc.redis_6_2.*

import scala.concurrent.duration.*

final class RedisDedicatedClientSpec     extends DedicatedClientSpec(LiveServers.port(6379), "redis")
final class KeyDbDedicatedClientSpec     extends DedicatedClientSpec(LiveServers.port(6380), "keyDb")
final class ValkeyDedicatedClientSpec    extends DedicatedClientSpec(LiveServers.port(6381), "valkey")
final class DragonflyDedicatedClientSpec extends DedicatedClientSpec(LiveServers.port(6382), "dragonfly")

abstract class DedicatedClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  private def run[A](io: IO[A]): A = io.unsafeRunSync()(using runtime)

  private def send[A](protocol: Protocol[A]): A =
    run(client.send(protocol)).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  private val list = OneOrMoreKeys.unsafeFrom(List(Key("dedicated:l")))

  test(s"a dedicated $dest client waits for as long as it takes for an element") {
    send(del("dedicated:l"))
    val popped = run(RedisClient[IO].dedicated("127.0.0.1", p).use { dedicated =>
      for {
        waiting <- dedicated.send(blocking.forever.blpop[String](list)).start
        _       <- IO.sleep(500.millis)
        _       <- client.send(rpush("dedicated:l", OneOrMore.unsafeFrom(List("x"))))
        popped  <- waiting.joinWithNever
      } yield popped
    })
    assertEquals(popped, Right(Some(KV(Key("dedicated:l"), "x"))))
  }

  test(s"cancelling a dedicated $dest client's wait stops it on the server too, and the next command connects again") {
    send(del("dedicated:l"))
    val next = run(RedisClient[IO].dedicated("127.0.0.1", p).use { dedicated =>
      for {
        waiting <- dedicated.send(blocking.forever.blpop[String](list)).start
        _       <- IO.sleep(500.millis)
        _       <- waiting.cancel
        next    <- dedicated.send(get[String]("dedicated:l"))
      } yield next
    })
    assertEquals(next, Right(None))
    // a BLPOP still waiting on the server would take the element
    send(rpush("dedicated:l", OneOrMore.unsafeFrom(List("x"))))
    run(IO.sleep(200.millis))
    assertEquals(send(llen("dedicated:l")).value, 1)
  }
}
