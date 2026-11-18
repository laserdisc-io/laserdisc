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

final class RedisSetupClientSpec     extends SetupClientSpec(LiveServers.port(6379), "redis")
final class KeyDbSetupClientSpec     extends SetupClientSpec(LiveServers.port(6380), "keyDb")
final class ValkeySetupClientSpec    extends SetupClientSpec(LiveServers.port(6381), "valkey")
final class DragonflySetupClientSpec extends SetupClientSpec(LiveServers.port(6382), "dragonfly")

abstract class SetupClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  private def run[A](io: IO[A]): A = io.unsafeRunSync()(using runtime)

  private def send[A](on: RedisClient[IO], protocol: Protocol[A]): A =
    run(on.send(protocol)).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  // a command sent before the client sees the server closed the connection fails, so it's sent again until it's answered
  private def untilAnswered[A](on: RedisClient[IO], protocol: Protocol[A], attempts: Int = 20): A =
    run(on.send(protocol).attempt).flatMap(identity) match {
      case Right(a)                => a
      case Left(_) if attempts > 1 => run(IO.sleep(100.millis)); untilAnswered(on, protocol, attempts - 1)
      case Left(e)                 => fail(s"$dest replied with an error: ${e.getMessage}", e)
    }

  private val killById = (id: NonNegLong) => Protocol("CLIENT", "KILL" *: "ID" *: id *: EmptyTuple).as[Num, NonNegLong]

  test(s"a $dest client stays in its database when its connection is closed and it connects again") {
    run(RedisClient[IO].to("127.0.0.1", p, database = Some(DbIndex(3))).use { inDb3 =>
      IO {
        send(client, del("ld:setup"))
        send(inDb3, set("ld:setup", "in 3"))
        assertEquals(send(client, get[String]("ld:setup")), None)

        assertEquals(send(client, killById(send(inDb3, laserdisc.redis_6_2.client.id))).value, 1L)
        assertEquals(untilAnswered(inDb3, get[String]("ld:setup")), Some("in 3"))
        send(inDb3, del("ld:setup"))
      }
    })
  }

  test(s"a $dest client logs in as the user it's given, and stops when the password is wrong") {
    val user = Key("ld-setup")
    assertEquals(send(client, Protocol("ACL", "SETUSER" *: user *: "on" *: ">secret" *: "~*" *: "+@all" *: EmptyTuple).as[Str, OK]), OK)
    try {
      run(RedisClient[IO].to("127.0.0.1", p, credentials = Some(Credentials(user, Key("secret")))).use { loggedIn =>
        // Dragonfly answers "User is ld-setup"
        IO(assert(send(loggedIn, acl.whoami).endsWith("ld-setup")))
      })
      run(RedisClient[IO].to("127.0.0.1", p, credentials = Some(Credentials(user, Key("wrong")))).use { refused =>
        refused.send(acl.whoami).attempt.map(_.flatMap(identity)).map {
          case Left(ConnectionSetupRefused(_, _)) => ()
          case other                              => fail(s"expected the login to be refused, got $other")
        }
      })
    } finally assertEquals(send(client, acl.deluser(OneOrMoreKeys.unsafeFrom(List(user)))).value, 1L)
  }
}
